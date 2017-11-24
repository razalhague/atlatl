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

import akka.actor.{Actor, ActorLogging, ActorSystem, Cancellable, Props}
import akka.pattern.ask
import akka.util.Timeout
import better.files.File
import org.jutils.jprocesses.JProcesses
import org.jutils.jprocesses.model.ProcessInfo
import org.penny_craal.atlatl.Helpers._

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.{Failure, Success}


/**
  * @author Ville Jokela
  */
object Atlatl extends App {
  private val system = ActorSystem("atlatl-system")
  system.actorOf(Props[Atlatl], "atlatl")
}

case object Refresh

case class GroupRuntimeInformation(name: String, spentMinutes: Double, continuousUseMinutes: Double) {
  def descriptionString(appGroup: AppGroup): String = {
    val dailyAllowance = appGroup.dailyMinutes map (m => (m - spentMinutes).minutesToTimeString + " left")
    val forbiddenTimes =
      if (appGroup.forbiddenTimes.nonEmpty) Some(appGroup.forbiddenTimes.mkString("forbidden at [", ", ", "]"))
      else None
    (dailyAllowance ++ forbiddenTimes).mkString(appGroup.name + ": ", ", ", "")
  }
}

class Atlatl extends Actor with ActorLogging {
  val configFileName = "config.json"

  private val conf = Config.parse(File(configFileName).contentAsString)
  /** Maps filename to Clip */
  private val sounds = setupAudioSystem(conf.soundFiles)
  private val trayActor =
    if (TrayActor.isSupported) Some(context.actorOf(Props(classOf[TrayActor], conf), "trayActor"))
    else None
  private val persistenceActor = context.actorOf(Props(classOf[PersistenceActor], conf), "persistenceActor")

  private val appGroups = conf.appGroups.map(ag => ag.name -> ag).toMap

  private var terminating = false
  private var suspendedTimeRanges = Seq[TimeRange]()
  private var prevSnapshot: Snapshot = _  // this will never be referenced before being defined
  private var refreshTick: Option[Cancellable] = None

  implicit val timeout: Timeout = Timeout(1.minute)
  import context.dispatcher
  trayActor foreach (_ ! UpdateToolTip("Loading data..."))
  persistenceActor ? LoadGroupTimes onComplete {
    case Success(savedGroupTimes) =>
      val now = LocalDateTime.now()
      val groupTimes = verifyGroupTimes(savedGroupTimes, now)
      prevSnapshot = StrippedSnapshot(appGroups, conf, groupTimes, now.plusMinutes(-conf.refreshMinutes))
      val refreshDelayMillis = (conf.refreshMinutes * 60 * 1000).toLong.millis
      refreshTick = Some(context.system.scheduler.schedule(1.milli, refreshDelayMillis, self, Refresh))
    case Failure(throwable) =>
      log.error(throwable, "error while loading state from disk")
      exit()
  }

  override def receive: Receive = {
    case Refresh => refresh()
    case Suspend => suspend()
    case Exit => exit()
  }

  private def refresh(): Unit = {
    log.info("refresh")
    val processes = fetchRunningProcesses()
    // a map from process name to list of process IDs
    val pIdsByName = processes groupBy (_.getName) mapValues (_ map (_.getPid.toInt))
    val snapshot = FullSnapshot(prevSnapshot, processes map (_.getName), LocalDateTime.now())
    // filters out suspensions that are not pertinent
    suspendedTimeRanges = suspendedTimeRanges filter (suspension =>
      suspension.contains(snapshot.refreshTime) ||
        suspension.contains(snapshot.refreshTime.plusMinutes(conf.suspensionDelayMinutes))
    )
    val alarmTime = snapshot.refreshTime.plusMinutes(conf.alarmThresholdMinutes)
    // checks if any group exists that should be killed in $alarmThresholdMinutes, but should not be killed right now
    val shouldPlayKillAlarm = snapshot.groupTimes exists (gri =>
      snapshot.anyAppsRunningFromGroup(gri.name) &&
        shouldKillGroupAt(appGroups(gri.name), alarmTime, gri.spentMinutes + conf.alarmThresholdMinutes) &&
        !shouldKillGroupAt(appGroups(gri.name), snapshot.refreshTime, gri.spentMinutes)
      )
    val pIdsToBeKilled = for {
      GroupRuntimeInformation(groupName, spentMinutes, _) <- snapshot.groupTimes
      if shouldKillGroupAt(appGroups(groupName), snapshot.refreshTime, spentMinutes)
      pName <- snapshot.runningTrackedApps if snapshot.appGroups(groupName).processNames contains pName
      pId <- pIdsByName(pName)
    } yield pId

    trayActor foreach (_ ! UpdateToolTip(trayTooltip(snapshot)))
    if (shouldPlayKillAlarm) {
      playSound(conf.killAlarmSoundFilename)
    }
    if (snapshot.anyAppsReachedContinuousUseThreshold) {
      playSound(conf.continuousUseAlarmSoundFilename)
    }
    if (pIdsToBeKilled.nonEmpty) {
      playSound(conf.killSoundFilename)
    }
    pIdsToBeKilled foreach JProcesses.killProcess
    prevSnapshot = snapshot.strippedOfHistory
    persistenceActor ! SaveGroupTimes(snapshot.groupTimes, snapshot.refreshDateTime)
  }

  private def suspend(): Unit = {
    val currentTime = LocalTime.now()
    log.info(s"suspension engaged at $currentTime")
    suspendedTimeRanges = suspendedTimeRanges :+ TimeRange(
      currentTime.plusMinutes(conf.suspensionDelayMinutes),
      currentTime.plusMinutes(conf.suspensionDurationMinutes + conf.suspensionDelayMinutes)
    )
  }

  private def exit(): Unit = {
    log.info("exiting...")
    terminating = true
    context.system.terminate()
  }

  private def trayTooltip(snapshot: Snapshot) = {
    def suspensionDescriptionString(timeRange: TimeRange) = {
      val desc =
        if (!timeRange.contains(snapshot.refreshTime)) {
          val susLength = timeRange.lengthMinutes.minutesToTimeString
          val susDelay = (snapshot.refreshTime until timeRange.start).minutesToTimeString
          s"$susLength starts in $susDelay"
        } else (snapshot.refreshTime until timeRange.end).minutesToTimeString + " left"
      s"suspension: $desc"
    }
    val suspensions = suspendedTimeRanges map suspensionDescriptionString
    val groupDescriptions = snapshot.groupTimes map (gri => gri.descriptionString(snapshot.appGroups(gri.name)))
    suspensions ++ groupDescriptions filter (_.nonEmpty) mkString "\n"
  }

  private def shouldKillGroupAt(appGroup: AppGroup, time: LocalTime, spentMinutes: Double): Boolean =
    !suspensionInEffectAt(time) && appGroup.shouldBeKilled(spentMinutes, time)

  private def suspensionInEffectAt(time: LocalTime): Boolean =
    suspendedTimeRanges exists (_.contains(time))

  private def fetchRunningProcesses(): Seq[ProcessInfo] = {
    JProcesses.getProcessList().asScala
  }

  private def setupAudioSystem(soundFileNames: Seq[String]): Map[String, Clip] = {
    val sounds = soundFileNames.map(_ -> AudioSystem.getClip()).toMap
    sounds foreach { case (sfn, clip) => clip.open(AudioSystem.getAudioInputStream(File(sfn).uri.toURL)) }
    sounds
  }

  private def playSound(soundFileName: String): Unit = {
    if (sounds(soundFileName).isRunning) {
      sounds(soundFileName).stop()
    }
    sounds(soundFileName).setFramePosition(0)
    sounds(soundFileName).start()
  }


  private def verifyGroupTimes(savedGroupTimes: Any, now: LocalDateTime) = savedGroupTimes match {
    case Some((saveTime: LocalDateTime, groupTimes: Seq[GroupRuntimeInformation]))
    if dataValidAt(saveTime, now) =>
      conf.appGroups map (appGroup => GroupRuntimeInformation(
        appGroup.name,
        groupTimes find (_.name == appGroup.name) map (_.spentMinutes) getOrElse 0.0,
        0.0
      ))
    case _ => conf.appGroups map (appGroup => GroupRuntimeInformation(appGroup.name, 0.0, 0.0))
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
    refreshTick foreach { _.cancel() }
    sounds.values foreach { _.close() }
    // the sound system leaves a thread alive that I cannot figure out how to terminate, which will leave the actor-
    // system hanging in certain situations, so let's terminate the whole program here
    if (terminating) {
      sys.exit()
    }
  }
}
