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
  private val atlatl = system.actorOf(Props[Atlatl], "atlatl")
}

class Atlatl extends Actor with ActorLogging {
  val configFileName = "config.json"

  private val conf = Config.parse(readConfig())
  /** Maps filename to Clip */
  private val sounds = setupAudioSystem(List(conf.alarmSoundFilename, conf.killSoundFilename))
  private val trayActor = context.actorOf(Props[TrayActor], "trayActor")

  private val appGroups = (conf.appGroups map (appGroup => (appGroup.name, appGroup))).toMap

  private var prevRefreshTime = LocalTime.now()
  private var prevTimeoutMinutes = 0.0
  private var prevGroupTimes = (conf.appGroups map (appGroup => (appGroup.name, 0.0))).toMap
  context.setReceiveTimeout(1.milli)

  override def receive: Receive = {
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
      val timeoutMinutes = Math.min(conf.refreshMinutes - (refreshTimeRange.lengthMinutes - prevTimeoutMinutes), 1.0 / 60.0 / 1000.0) // timeout must be at least one millisecond
      def anyAppsRunningFromGroup(groupName: String) =
        apps exists (appGroups(groupName).processNames contains _.getName)
      val groupTimes =
        if (refreshTimeRange.contains(conf.dailyResetTime))
          prevGroupTimes mapValues (_ => 0.0)
        else
          for ((groupName, spentMinutes) <- prevGroupTimes)
            yield (groupName, if (anyAppsRunningFromGroup(groupName)) spentMinutes + refreshTimeRange.lengthMinutes else spentMinutes)
      val alarmTime = refreshTime.plusSeconds((conf.alarmThresholdMinutes * 60).toLong)
      val shouldAlarm =
        groupTimes exists { case (groupName, spentMinutes) =>
          anyAppsRunningFromGroup(groupName) &&
            appGroups(groupName).shouldBeKilled(spentMinutes + conf.alarmThresholdMinutes, alarmTime) && // should be killed in $alarmThresholdMinutes
            !appGroups(groupName).shouldBeKilled(spentMinutes, refreshTime) // but should not be killed right now
        }
      val toBeKilled = for {
        (groupName, spentMinutes) <- groupTimes if appGroups(groupName).shouldBeKilled(spentMinutes, refreshTime)
        pi <- apps if appGroups(groupName).processNames contains pi.getName
      } yield pi.getPid.toInt
      trayActor ! UpdateToolTip(trayTooltip(groupTimes, appGroups))
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

  private def trayTooltip(updatedGroupTimes: Map[String, Double], appGroups: Map[String, AppGroup]) = {
    updatedGroupTimes map { case (groupName, spentMinutes) =>
      groupName + ": " +
        (Seq(
          appGroups(groupName).dailyMinutes match {
            case Some(allowedMinutes) => Some(minutesToTimeString(spentMinutes) + "/" + minutesToTimeString(allowedMinutes))
            case None => None
          },
          if (appGroups(groupName).forbiddenTimes.nonEmpty)
            Some("forbidden during [" + (appGroups(groupName).forbiddenTimes map (_.toString) reduce (_ + ", " + _)) + "]")
          else
            None
        ) collect { case Some(s) => s } reduce (_ + ", " + _)) // if the group has both a daily limit and forbidden times, separate them with a comma
    } reduce (_ + "\n" + _) // separate groups with newlines
  }

  private def minutesToTimeString(minutes: Double): String =
    f"${(minutes / 60).floor}%1.0f:${(minutes % 60).floor}%02.0f:${(minutes % 1 * 60).floor}%02.0f"

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
}
