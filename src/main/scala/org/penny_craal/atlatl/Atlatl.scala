package org.penny_craal.atlatl

import java.awt.{SystemTray, TrayIcon}
import java.io.{File, IOException}
import javax.imageio.ImageIO
import javax.sound.sampled.{Clip, AudioSystem}

import org.json.simple.{JSONArray, JSONValue, JSONObject}
import org.jutils.jprocesses.JProcesses
import org.jutils.jprocesses.model.ProcessInfo

import scala.collection.JavaConverters
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * @author Ville Jokela
  */
object Atlatl {
  val configFileName = "config.json"
  val trayIconFileName = "atlatl.png"

  var sounds = scala.collection.mutable.Map[String,Clip]()
  var trayIcon: TrayIcon = null

  def main(args: Array[String]): Unit = {
    readConfig() match {
      case Failure(e) => println("Failed to read configuration file: " + e.getLocalizedMessage)
      case Success(conf) =>
        val (appGroups, killSound, alarmSound, alarmThresholdMinutes, refreshMinutes) = parseConfig(conf)
        setupAudioSystem(List(alarmSound, killSound))
        setupTrayIcon()
        def loop(groupTimes: Map[String, Double], appsFromLastRefresh: Seq[ProcessInfo]): Unit = {
          val apps = fetchRunningProcesses() filter (pi => appGroups map {case (_,appGroup) => appGroup.processNames contains pi.getName} reduce (_||_))
          val appsInCommon =
            if (apps.nonEmpty) apps filter (piNew =>
              appsFromLastRefresh.nonEmpty && (appsFromLastRefresh map (piOld => piNew.getName == piOld.getName) reduce (_||_)))
            else Seq()
          val updatedGroupTimes =
            if (appsInCommon.nonEmpty) groupTimes map {case (groupName,spentTime) =>
              (groupName, if (appsInCommon map (pi => appGroups(groupName).processNames contains pi.getName) reduce (_||_)) spentTime+refreshMinutes else spentTime)}
            else groupTimes
          val areAnyAppsRunningFromAGroupInWarningZone = updatedGroupTimes map {case (groupName,spentTime) =>
            val timeLeft = appGroups(groupName).dailyMinutes - spentTime
            val appsFromThisGroupRunning = if (apps.nonEmpty) apps map (pi => appGroups(groupName).processNames contains pi.getName) reduce (_||_) else false
            0 < timeLeft && timeLeft <= alarmThresholdMinutes && appsFromThisGroupRunning
          } reduce (_||_)
          updateTrayIconTooltip(updatedGroupTimes map {case (groupName,spentTime) => groupName + ": " + spentTime + "/" + appGroups(groupName).dailyMinutes + "\n"} reduce (_+_))
          updatedGroupTimes foreach {case (groupName,spentTime) =>
            if (spentTime >= appGroups(groupName).dailyMinutes) apps filter (pi => appGroups(groupName).processNames contains pi.getName) foreach (pi => {
              playSound(killSound)
              JProcesses.killProcess(pi.getPid.toInt)
            })
          }
          if (areAnyAppsRunningFromAGroupInWarningZone) playSound(alarmSound)
          Thread sleep (refreshMinutes * 60 * 1000).toLong
          loop(updatedGroupTimes, apps)
        }
        loop(appGroups map {case (_,appGroup) => (appGroup.name, 0.0)}, Seq())
    }
  }

  def readConfig(): Try[String] = {
    val configFile = Source fromFile configFileName
    try Success(configFile.mkString)
    catch { case e: IOException => Failure(e) }
    finally configFile.close
  }

  def parseConfig(jsonText: String): (Map[String, AppGroup], String, String, Double, Double) = {
    // TODO: extract field names?
    val configJson = JSONValue.parse(jsonText).asInstanceOf[JSONObject]
    val appGroups = JavaConverters.asScalaIterator(configJson.get("groups").asInstanceOf[JSONArray].iterator).map(o => {
      val jsonAppGroup = o.asInstanceOf[JSONObject]
      new AppGroup(
        jsonAppGroup.get("name").asInstanceOf[String],
        jsonAppGroup.get("dailyMinutes").asInstanceOf[Double],
        JavaConverters.asScalaIterator(jsonAppGroup.get("processNames").asInstanceOf[JSONArray].iterator).map(_.asInstanceOf[String]).toSeq
      )
    }).toSeq
    (
      Map(appGroups map (appGroup => (appGroup.name, appGroup)): _*),
      configJson.get("killSound").asInstanceOf[String],
      configJson.get("alarmSound").asInstanceOf[String],
      configJson.get("alarmThresholdMinutes").asInstanceOf[Double],
      configJson.get("refreshMinutes").asInstanceOf[Double]
    )
  }

  def fetchRunningProcesses(): Seq[ProcessInfo] = {
    JavaConverters.asScalaIterator(JProcesses.getProcessList.iterator).toSeq
  }

  def setupAudioSystem(soundFileNames: Seq[String]) = {
    soundFileNames foreach {sfn =>
      sounds(sfn) = AudioSystem.getClip
      sounds(sfn).open(AudioSystem.getAudioInputStream(new File(sfn)))
    }
  }

  def playSound(soundFileName: String) = {
    if (sounds(soundFileName).isRunning) sounds(soundFileName).stop()
    sounds(soundFileName).setFramePosition(0)
    sounds(soundFileName).start()
  }

  def setupTrayIcon() = {
    if (SystemTray.isSupported) {
      trayIcon = new TrayIcon(ImageIO.read(getClass.getResource("/" + trayIconFileName)))
      SystemTray.getSystemTray.add(trayIcon)
    }
  }

  def updateTrayIconTooltip(text: String) = {
    if (trayIcon != null) trayIcon.setToolTip(text)
  }
}

class AppGroup(val name: String, val dailyMinutes: Double, val processNames: Seq[String])

