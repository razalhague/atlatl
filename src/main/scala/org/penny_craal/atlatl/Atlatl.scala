/* Copyright © 2016 Ville Jokela
 *
 * This file is part of atlatl.
 *
 * atlatl is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * atlatl is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with atlatl.  If not, see <http://www.gnu.org/licenses/>.
 *
 * contact me <ville.jokela@penny-craal.org>
 */

package org.penny_craal.atlatl

import java.time.{LocalDateTime, LocalTime}
import javax.sound.sampled.{AudioSystem, Clip}

import akka.actor.{Actor, ActorLogging, ActorSystem, Props, ReceiveTimeout}
import akka.pattern.ask
import akka.util.Timeout
import better.files.File
import org.jutils.jprocesses.JProcesses
import org.jutils.jprocesses.model.ProcessInfo

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * @author Ville Jokela
  */
object Atlatl extends App {
  private val system = ActorSystem("atlatl-system")
  system.actorOf(Props[Atlatl], "atlatl")
}

case object Tick

case class GroupRuntimeInformation(name: String, spentMinutes: Double, continuousUseMinutes: Double)

class Atlatl extends Actor with ActorLogging {
  val configFileName = "config.json"

  private val conf = Config.parse(File(configFileName).contentAsString)
  /** Maps filename to Clip */
  private val sounds = setupAudioSystem(List(conf.continuousUseAlarmSoundFilename, conf.killAlarmSoundFilename, conf.killSoundFilename))
  private val trayActor =
    if (TrayActor.isSupported)
      Some(context.actorOf(Props(classOf[TrayActor], conf), "trayActor"))
    else
      None
  private val persistenceActor = context.actorOf(Props(classOf[PersistenceActor], conf), "persistenceActor")

  private val appGroups = (conf.appGroups map (appGroup => (appGroup.name, appGroup))).toMap

  private var terminating = false
  private var prevGroupTimes = loadGroupTimes()
  private var prevRefreshTime = LocalTime.now()
  private var prevApps = Seq[ProcessInfo]()
  private var suspendedTimeRanges = Seq[TimeRange]()

  import context.dispatcher
  private val tick = context.system.scheduler.schedule(1.milli, (conf.refreshMinutes * 60 * 1000).toLong.millis, self, Tick)

  override def receive: Receive = {
    case Suspend =>
      log.info("suspension engaged")
      val currentTime = LocalTime.now()
      suspendedTimeRanges = suspendedTimeRanges :+ new TimeRange(
        currentTime.plusMinutes(conf.suspensionDelayMinutes),
        currentTime.plusMinutes(conf.suspensionDurationMinutes + conf.suspensionDelayMinutes)
      )
    case Exit =>
      log.info("exiting...")
      terminating = true
      context.system.terminate()
    case Tick =>
      log.info("tick")
      val apps = for {
        appGroup <- conf.appGroups
        pi <- fetchRunningProcesses() if appGroup.processNames contains pi.getName
      } yield pi
      val refreshDateTime = LocalDateTime.now()
      val refreshTime = refreshDateTime.toLocalTime
      val refreshTimeRange = new TimeRange(prevRefreshTime, refreshTime) // assumes that the refresh takes less than a day
      suspendedTimeRanges = suspendedTimeRanges filter (suspension =>
        suspension.contains(refreshTime) || suspension.contains(refreshTime.plusMinutes(conf.suspensionDelayMinutes))
      )
      def anyAppsRunningFromGroup(groupName: String) =
        apps exists (appGroups(groupName).processNames contains _.getName)
      def updatedContinuousUse(groupName: String, continuousUseMinutes: Double) = {
        if ((prevApps map (_.getName) intersect (apps map (_.getName)) intersect appGroups(groupName).processNames).nonEmpty)
          if (continuousUseMinutes == 0.0)
            // this is the first time after a reset that this process is found in both apps and prevApps
            // it must have been running for two refresh intervals for this to happen
            refreshTimeRange.lengthMinutes * 2
          else
            if (continuousUseMinutes < conf.continuousUseAlarmMinutes)
              continuousUseMinutes + refreshTimeRange.lengthMinutes
            else
              // we'll have warned of continuous use last refresh, so reduce the amount of time by the alarm interval
              continuousUseMinutes - conf.continuousUseAlarmMinutes + refreshTimeRange.lengthMinutes
        else
          0.0
      }
      val groupTimes =
        if (refreshTimeRange.contains(conf.dailyResetTime))
          prevGroupTimes map (gri => GroupRuntimeInformation(gri.name, 0.0, gri.continuousUseMinutes))
        else
          for (GroupRuntimeInformation(groupName, spentMinutes, continuousUseMinutes) <- prevGroupTimes)
            yield GroupRuntimeInformation(
              groupName,
              if (anyAppsRunningFromGroup(groupName)) spentMinutes + refreshTimeRange.lengthMinutes else spentMinutes,
              updatedContinuousUse(groupName, continuousUseMinutes)
            )
      val alarmTime = refreshTime.plusMinutes(conf.alarmThresholdMinutes)
      val shouldKillAlarm =
        groupTimes exists { case GroupRuntimeInformation(groupName, spentMinutes, _) =>
          anyAppsRunningFromGroup(groupName) &&
            shouldKillGroupAt(groupName, alarmTime, spentMinutes + conf.alarmThresholdMinutes) && // should be killed in $alarmThresholdMinutes
            !shouldKillGroupAt(groupName, refreshTime, spentMinutes) // but should not be killed right now
        }
      val shouldContinuousUseAlarm =
        groupTimes exists (gri => appGroups(gri.name).trackContinuousUse && gri.continuousUseMinutes >= conf.continuousUseAlarmMinutes)
      val toBeKilled = for {
        GroupRuntimeInformation(groupName, spentMinutes, _) <- groupTimes if shouldKillGroupAt(groupName, refreshTime, spentMinutes)
        pi <- apps if appGroups(groupName).processNames contains pi.getName
      } yield pi.getPid.toInt
      trayActor foreach (_ ! UpdateToolTip(trayTooltip(groupTimes, refreshTime)))
      if (shouldKillAlarm) {
        playSound(conf.killAlarmSoundFilename)
      }
      if (shouldContinuousUseAlarm) {
        playSound(conf.continuousUseAlarmSoundFilename)
      }
      if (toBeKilled.nonEmpty) {
        playSound(conf.killSoundFilename)
      }
      toBeKilled foreach JProcesses.killProcess
      prevRefreshTime = refreshTime
      prevGroupTimes = groupTimes
      prevApps = apps
      persistenceActor ! SaveGroupTimes(groupTimes, refreshDateTime)
  }

  private def trayTooltip(groupTimes: Seq[GroupRuntimeInformation], now: LocalTime) = {
    val suspensions = suspendedTimeRanges map (suspensionRange =>
      "suspension: " + (
        if (!suspensionRange.contains(now))
          minutesToTimeString(suspensionRange.lengthMinutes) + " starts in " + minutesToTimeString(now until suspensionRange.start)
        else
          minutesToTimeString(now until suspensionRange.end) + " left"
      )
    )
    val groupDescriptions = groupTimes map { case GroupRuntimeInformation(groupName, spentMinutes, _) =>
      val dailyAllowance = appGroups(groupName).dailyMinutes match {
        case Some(allowedMinutes) => minutesToTimeString(allowedMinutes - spentMinutes) + " left"
        case None => ""
      }
      val forbiddenTimes =
        if (appGroups(groupName).forbiddenTimes.nonEmpty)
          "forbidden at [" + separateNonEmpties(", ", appGroups(groupName).forbiddenTimes map (_.toString): _*) + "]"
        else
          ""
      groupName + ": " + separateNonEmpties(", ", dailyAllowance, forbiddenTimes)
    }
    separateNonEmpties("\n", suspensions ++ groupDescriptions: _*)
  }

  private def separateNonEmpties(separator: String, strings: String*) =
    strings filter (_.nonEmpty) reduce (_ + separator + _)

  private def shouldKillGroupAt(groupName: String, time: LocalTime, spentMinutes: Double): Boolean =
    !suspensionInEffectAt(time) &&
      appGroups(groupName).shouldBeKilled(spentMinutes, time)

  private def suspensionInEffectAt(time: LocalTime): Boolean =
    suspendedTimeRanges exists (_.contains(time))

  private def minutesToTimeString(minutes: Double): String = {
    if (Math.abs(minutes) > 120)
      f"${(minutes / 60).floor}%1.0f h"
    else if (Math.abs(minutes) > 2)
      f"${minutes.floor}%1.0f m"
    else // less than two minutes left
      f"${(minutes * 60).floor}%1.0f s"
  }

  private def fetchRunningProcesses(): Seq[ProcessInfo] = {
    JProcesses.getProcessList.asScala
  }

  private def setupAudioSystem(soundFileNames: Seq[String]): Map[String, Clip] = {
    (soundFileNames map { sfn =>
      val clip = AudioSystem.getClip()
      clip.open(AudioSystem.getAudioInputStream(File(sfn).uri.toURL))
      (sfn, clip)
    }).toMap
  }

  private def playSound(soundFileName: String): Unit = {
    if (sounds(soundFileName).isRunning) {
      sounds(soundFileName).stop()
    }
    sounds(soundFileName).setFramePosition(0)
    sounds(soundFileName).start()
  }

  private def loadGroupTimes(): Seq[GroupRuntimeInformation] = {
    implicit val timeout: Timeout = Timeout(1.minute)
    lazy val emptyGroupTimes = conf.appGroups map (appGroup => GroupRuntimeInformation(appGroup.name, 0.0, 0.0))
    trayActor foreach (_ ! UpdateToolTip("Loading data..."))
    val future = persistenceActor ? LoadGroupTimes
    Await.result(future, timeout.duration) match {
      case Some((saveTime: LocalDateTime, groupTimes: Seq[GroupRuntimeInformation])) =>
        if (dataValidAt(saveTime, LocalDateTime.now()))
          // construct the new groupTimes list from the configuration file in case it has been changed since the last
          // save, and the loaded groupTimes is either missing appGroups or has old appGroups that are no longer in
          // the configuration
          conf.appGroups map (appGroup => GroupRuntimeInformation(
            appGroup.name,
            groupTimes find (_.name == appGroup.name) map (_.spentMinutes) getOrElse 0.0,
            0.0
          ))
        else // data is outdated
          emptyGroupTimes
      case None => emptyGroupTimes
    }
  }

  private def dataValidAt(saveTime: LocalDateTime, referenceTime: LocalDateTime): Boolean = {
    val lastResetTime =
      if (conf.dailyResetTime.isBefore(referenceTime.toLocalTime)) // check that we haven't already reset today
        conf.dailyResetTime.atDate(referenceTime.toLocalDate) // use today's reset time
      else
        conf.dailyResetTime.atDate(referenceTime.toLocalDate.minusDays(1)) // use yesterday's reset time
    saveTime.isAfter(lastResetTime)
  }

  override def postStop(): Unit = {
    tick.cancel()
    sounds.values foreach { _.close() }
    // the sound system leaves a thread alive that I cannot figure out how to terminate, which will leave the actor-
    // system hanging in certain situations, so let's terminate the whole program here
    if (terminating) {
      sys.exit()
    }
  }

  // doing these manually every time got too error-prone and verbose
  implicit class LocalTimeHelper(x: LocalTime) {
    def plusMinutes(minutes: Double): LocalTime =
      x.plus(java.time.Duration.ofMillis((minutes * 60 * 1000).toLong))

    def until(lt: LocalTime): Double =
      new TimeRange(x, lt).lengthMinutes
  }
}
