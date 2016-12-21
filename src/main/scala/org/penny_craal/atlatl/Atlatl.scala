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

import java.awt.{SystemTray, TrayIcon}
import java.io.File
import java.time.{LocalDateTime, LocalTime}
import javax.imageio.ImageIO
import javax.sound.sampled.{AudioSystem, Clip}

import org.jutils.jprocesses.JProcesses
import org.jutils.jprocesses.model.ProcessInfo

import scala.collection.JavaConverters._
import scala.io.Source

/**
  * @author Ville Jokela
  */
object Atlatl extends App {
  val configFileName = "config.json"
  val trayIconFileName = "atlatl.png"

  private val conf = Config.parse(readConfig())
  /** Maps filename to Clip */
  private val sounds = setupAudioSystem(List(conf.alarmSoundFilename, conf.killSoundFilename))
  private val trayIcon: TrayIcon = setupTrayIcon()

  val appGroups = (conf.appGroups map (appGroup => (appGroup.name, appGroup))).toMap

  loop((conf.appGroups map (appGroup => (appGroup.name, 0.0))).toMap, LocalDateTime.now(), 0.0)

  private def loop(groupTimes: Map[String, Double], lastRefresh: LocalDateTime, oldSleepMinutes: Double): Unit = {
    val apps = for {
      appGroup <- conf.appGroups
      pi <- fetchRunningProcesses() if appGroup.processNames contains pi.getName
    } yield pi
    val now = LocalDateTime.now()
    val refreshTimeRange = new TimeRange(lastRefresh.toLocalTime, now.toLocalTime) // assumes that the refresh takes less than a day
    val sleepMinutes = conf.refreshMinutes - (refreshTimeRange.lengthMinutes - oldSleepMinutes)
    def anyAppsRunningFromGroup(groupName: String) =
      apps exists (appGroups(groupName).processNames contains _.getName)
    val updatedGroupTimes =
      if (refreshTimeRange.contains(conf.dailyResetTime))
        groupTimes mapValues (_ => 0.0)
      else
        for ((groupName, spentMinutes) <- groupTimes)
          yield (groupName, if (anyAppsRunningFromGroup(groupName)) spentMinutes + refreshTimeRange.lengthMinutes else spentMinutes)
    val currentTime = LocalTime.now()
    val alarmTime = currentTime.plusSeconds((conf.alarmThresholdMinutes * 60).toLong)
    val shouldAlarm =
      updatedGroupTimes exists { case (groupName, spentMinutes) =>
        anyAppsRunningFromGroup(groupName) &&
          appGroups(groupName).shouldBeKilled(spentMinutes + conf.alarmThresholdMinutes, alarmTime) && // should be killed in $alarmThresholdMinutes
          !appGroups(groupName).shouldBeKilled(spentMinutes, currentTime) // but should not be killed right now
      }
    val toBeKilled = for {
      (groupName, spentMinutes) <- updatedGroupTimes if appGroups(groupName).shouldBeKilled(spentMinutes, currentTime)
      pi <- apps if appGroups(groupName).processNames contains pi.getName
    } yield pi.getPid.toInt
    updateTrayIconTooltip(trayTooltip(updatedGroupTimes, appGroups))
    if (shouldAlarm) {
      playSound(conf.alarmSoundFilename)
    }
    if (toBeKilled.nonEmpty) {
      playSound(conf.killSoundFilename)
    }
    toBeKilled foreach JProcesses.killProcess
    if (sleepMinutes > 0) {
      Thread.sleep((sleepMinutes * 60 * 1000).toLong)
    }
    loop(updatedGroupTimes, now, sleepMinutes)
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

  private def setupTrayIcon(): TrayIcon = {
    if (SystemTray.isSupported) {
      val ti = new TrayIcon(ImageIO.read(getClass.getResource("/" + trayIconFileName)))
      SystemTray.getSystemTray.add(ti)
      ti
    } else {
      _
    }
  }

  private def updateTrayIconTooltip(text: String): Unit = {
    if (trayIcon != null) {
      trayIcon.setToolTip(text)
    }
  }
}
