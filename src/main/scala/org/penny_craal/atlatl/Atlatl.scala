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

import java.io.File
import java.time.LocalTime
import javax.sound.sampled.{AudioSystem, Clip}

import akka.actor.{Actor, ActorLogging, ActorSystem, Props, ReceiveTimeout}
import org.jutils.jprocesses.JProcesses
import org.jutils.jprocesses.model.ProcessInfo

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.io.Source

/**
  * @author Ville Jokela
  */
object Atlatl extends App {
  private val system = ActorSystem("atlatl-system")
  system.actorOf(Props[Atlatl], "atlatl")
}

class Atlatl extends Actor with ActorLogging {
  val configFileName = "config.json"

  private val conf = Config.parse(readConfig())
  /** Maps filename to Clip */
  private val sounds = setupAudioSystem(List(conf.alarmSoundFilename, conf.killSoundFilename))
  private val trayActor =
    if (TrayActor.isSupported)
      Some(context.actorOf(Props(classOf[TrayActor], conf), "trayActor"))
    else
      None

  private val appGroups = (conf.appGroups map (appGroup => (appGroup.name, appGroup))).toMap

  private var prevRefreshTime = LocalTime.now()
  private var prevTimeoutMinutes = 0.0
  private var prevGroupTimes = (conf.appGroups map (appGroup => (appGroup.name, 0.0))).toMap
  private var suspendedTimeRanges = Seq[TimeRange]()
  context.setReceiveTimeout(1.milli)

  override def receive: Receive = {
    case Suspend =>
      log.info("suspension engaged")
      val currentTime = LocalTime.now()
      suspendedTimeRanges = suspendedTimeRanges :+ new TimeRange(
        currentTime.plusMinutes(conf.suspensionDelayMinutes),
        currentTime.plusMinutes(conf.suspensionDurationMinutes + conf.suspensionDelayMinutes)
      )
      context.setReceiveTimeout(1.milli) // to update the tooltip and everything else ASAP
    case Exit =>
      log.info("exiting...")
      context.system.terminate()
    case ReceiveTimeout =>
      val apps = for {
        appGroup <- conf.appGroups
        pi <- fetchRunningProcesses() if appGroup.processNames contains pi.getName
      } yield pi
      val refreshTime = LocalTime.now()
      val refreshTimeRange = new TimeRange(prevRefreshTime, refreshTime) // assumes that the refresh takes less than a day
      val timeoutMinutes = Math.max(conf.refreshMinutes - (refreshTimeRange.lengthMinutes - prevTimeoutMinutes), 1.0 / 60.0 / 1000.0) // timeout must be at least one millisecond
      suspendedTimeRanges = suspendedTimeRanges filter (suspension =>
        suspension.contains(refreshTime) || suspension.contains(refreshTime.plusMinutes(conf.suspensionDelayMinutes))
      )
      def anyAppsRunningFromGroup(groupName: String) =
        apps exists (appGroups(groupName).processNames contains _.getName)
      val groupTimes =
        if (refreshTimeRange.contains(conf.dailyResetTime))
          prevGroupTimes mapValues (_ => 0.0)
        else
          for ((groupName, spentMinutes) <- prevGroupTimes)
            yield (groupName, if (anyAppsRunningFromGroup(groupName)) spentMinutes + refreshTimeRange.lengthMinutes else spentMinutes)
      val alarmTime = refreshTime.plusMinutes(conf.alarmThresholdMinutes)
      val shouldAlarm =
        groupTimes exists { case (groupName, spentMinutes) =>
          anyAppsRunningFromGroup(groupName) &&
            shouldKillGroupAt(groupName, alarmTime, spentMinutes + conf.alarmThresholdMinutes) && // should be killed in $alarmThresholdMinutes
            !shouldKillGroupAt(groupName, refreshTime, spentMinutes) // but should not be killed right now
        }
      val toBeKilled = for {
        (groupName, spentMinutes) <- groupTimes if shouldKillGroupAt(groupName, refreshTime, spentMinutes)
        pi <- apps if appGroups(groupName).processNames contains pi.getName
      } yield pi.getPid.toInt
      trayActor foreach (_ ! UpdateToolTip(trayTooltip(groupTimes, refreshTime)))
      if (shouldAlarm) {
        playSound(conf.alarmSoundFilename)
      }
      if (toBeKilled.nonEmpty) {
        playSound(conf.killSoundFilename)
      }
      toBeKilled foreach JProcesses.killProcess
      context.setReceiveTimeout((timeoutMinutes * 60 * 1000).toLong.millis)
      prevRefreshTime = refreshTime
      prevTimeoutMinutes = timeoutMinutes
      prevGroupTimes = groupTimes
  }

  private def trayTooltip(groupTimes: Map[String, Double], now: LocalTime) = {
    val suspensions = suspendedTimeRanges map (suspensionRange =>
      "suspension: " + (
        if (!suspensionRange.contains(now))
          minutesToTimeString(suspensionRange.lengthMinutes) + " starts in " + minutesToTimeString(now until suspensionRange.start)
        else
          minutesToTimeString(now until suspensionRange.end) + " left"
      )
    )
    val groupDescriptions = groupTimes map { case (groupName, spentMinutes) =>
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

  private def readConfig(): String = {
    val configFile = Source.fromFile(configFileName)
    try {
      configFile.mkString
    } finally {
      configFile.close()
    }
  }

  private def fetchRunningProcesses(): Seq[ProcessInfo] = {
    JProcesses.getProcessList.asScala
  }

  private def setupAudioSystem(soundFileNames: Seq[String]): Map[String, Clip] = {
    (soundFileNames map { sfn =>
      val clip = AudioSystem.getClip()
      clip.open(AudioSystem.getAudioInputStream(new File(sfn)))
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

  override def postStop(): Unit = {
    sounds.values foreach { _.close() }
    sys.exit()
  }

  // doing these manually every time got too error-prone and verbose
  implicit class LocalTimeHelper(x: LocalTime) {
    def plusMinutes(minutes: Double): LocalTime =
      x.plus(java.time.Duration.ofMillis((minutes * 60 * 1000).toLong))

    def until(lt: LocalTime): Double =
      new TimeRange(x, lt).lengthMinutes
  }
}
