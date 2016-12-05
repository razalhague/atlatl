package org.penny_craal.atlatl

import java.awt.{SystemTray, TrayIcon}
import java.io.{File, IOException}
import java.time.LocalTime
import javax.imageio.ImageIO
import javax.sound.sampled.{AudioSystem, Clip}

import org.jutils.jprocesses.JProcesses
import org.jutils.jprocesses.model.ProcessInfo

import scala.collection.JavaConverters._
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * @author Ville Jokela
  */
object Atlatl {
  val configFileName = "config.json"
  val trayIconFileName = "atlatl.png"

  /** Maps filename to Clip */
  private val sounds = scala.collection.mutable.Map[String, Clip]()

  private var trayIcon: TrayIcon = _

  def main(args: Array[String]): Unit = {
    readConfig() match {
      case Success(conf) => run(conf)
      case Failure(e) => println("Failed to read configuration file: " + e.getLocalizedMessage)
    }
  }

  private def run(confText: String): Unit = {
    val conf = Config.parse(confText)
    val appGroups = Map(conf.appGroups map (appGroup => (appGroup.name, appGroup)): _*)
    for (appGroup <- conf.appGroups; ft <- appGroup.forbiddenTimes if ft.lengthMinutes < conf.alarmThresholdMinutes) {
      throw new RuntimeException("Forbidden time ranges must be longer than the alarm threshold")
    }
    if (conf.appGroups.isEmpty) {
      throw new RuntimeException("Cannot run with no defined app groups")
    }
    if (conf.refreshMinutes <= 0) {
      throw new RuntimeException("refresh period must be positive")
    }
    setupAudioSystem(List(conf.alarmSoundFilename, conf.killSoundFilename))
    setupTrayIcon()

    def loop(groupTimes: Map[String, Double]): Unit = {
      val apps = for {
        appGroup <- conf.appGroups
        pi <- fetchRunningProcesses() if appGroup.processNames contains pi.getName
      } yield pi
      def anyAppsRunningFromGroup(groupName: String) =
        apps exists (appGroups(groupName).processNames contains _.getName)
      val updatedGroupTimes =
        for ((groupName, spentTime) <- groupTimes)
          yield (groupName, if (anyAppsRunningFromGroup(groupName)) spentTime + conf.refreshMinutes else spentTime)
      val currentTime = LocalTime.now()
      val alarmTime = currentTime.plusSeconds((conf.alarmThresholdMinutes * 60).toLong)
      val shouldAlarm =
        updatedGroupTimes exists { case (groupName, spentTime) =>
          anyAppsRunningFromGroup(groupName) &&
            appGroups(groupName).shouldBeKilled(spentTime + conf.alarmThresholdMinutes, alarmTime) && // should be killed in $alarmThresholdMinutes
            !appGroups(groupName).shouldBeKilled(spentTime, currentTime) // but should not be killed right now
        }
      updateTrayIconTooltip(trayTooltip(updatedGroupTimes, appGroups))
      for ((groupName, spentTime) <- updatedGroupTimes if appGroups(groupName).shouldBeKilled(spentTime, currentTime)) {
        for (pi <- apps if appGroups(groupName).processNames contains pi.getName) {
          playSound(conf.killSoundFilename)
          JProcesses.killProcess(pi.getPid.toInt)
        }
      }
      if (shouldAlarm) {
        playSound(conf.alarmSoundFilename)
      }
      Thread.sleep((conf.refreshMinutes * 60 * 1000).toLong)
      loop(updatedGroupTimes)
    }

    loop(Map(conf.appGroups map (appGroup => (appGroup.name, 0.0)): _*))
  }

  private def trayTooltip(updatedGroupTimes: Map[String, Double], appGroups: Map[String, AppGroup]) = {
    updatedGroupTimes map { case (groupName, spentTime) =>
      groupName + ": " +
        (Seq(
          appGroups(groupName).dailyMinutes match {
            case Some(allowedTime) => Some(s"$spentTime/$allowedTime")
            case None => None
          },
          if (appGroups(groupName).forbiddenTimes.nonEmpty)
            Some("forbidden during [" + (appGroups(groupName).forbiddenTimes map (_.toString) reduce (_ + ", " + _)) + "]")
          else
            None
        ) collect { case Some(s) => s } reduce (_ + ", " + _))
    } reduce (_ + "\n" + _)
  }

  private def readConfig(): Try[String] = {
    val configFile = Source.fromFile(configFileName)
    try {
      Success(configFile.mkString)
    } catch {
      case e: IOException => Failure(e)
    } finally {
      configFile.close()
    }
  }

  private def fetchRunningProcesses(): Seq[ProcessInfo] = {
    JProcesses.getProcessList.asScala
  }

  private def setupAudioSystem(soundFileNames: Seq[String]): Unit = {
    for (sfn <- soundFileNames) {
      sounds(sfn) = AudioSystem.getClip()
      sounds(sfn).open(AudioSystem.getAudioInputStream(new File(sfn)))
    }
  }

  private def playSound(soundFileName: String): Unit = {
    if (sounds(soundFileName).isRunning) {
      sounds(soundFileName).stop()
    }
    sounds(soundFileName).setFramePosition(0)
    sounds(soundFileName).start()
  }

  private def setupTrayIcon(): Unit = {
    if (SystemTray.isSupported) {
      trayIcon = new TrayIcon(ImageIO.read(getClass.getResource("/" + trayIconFileName)))
      SystemTray.getSystemTray.add(trayIcon)
    }
  }

  private def updateTrayIconTooltip(text: String): Unit = {
    if (trayIcon != null) {
      trayIcon.setToolTip(text)
    }
  }
}
