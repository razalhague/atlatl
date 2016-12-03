package org.penny_craal.atlatl

import java.awt.{SystemTray, TrayIcon}
import java.io.{File, IOException}
import javax.imageio.ImageIO
import javax.sound.sampled.{AudioSystem, Clip}

import org.json.simple.{JSONArray, JSONObject, JSONValue}
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

  private def run(conf: String): Unit = {
    val (appGroupList, killSound, alarmSound, alarmThresholdMinutes, refreshMinutes) = parseConfig(conf)
    val appGroups = Map(appGroupList map (appGroup => (appGroup.name, appGroup)): _*)
    setupAudioSystem(List(alarmSound, killSound))
    setupTrayIcon()

    def loop(groupTimes: Map[String, Double], appsFromLastRefresh: Iterable[ProcessInfo]): Unit = {
      val apps = for {
        appGroup <- appGroupList
        pi <- fetchRunningProcesses() if appGroup.processNames contains pi.getName
      } yield pi
      val appsInCommon = for {
        piOld <- appsFromLastRefresh
        piNew <- apps if piNew.getName == piOld.getName
      } yield piNew
      def anyAppsRunningFromGroup(groupName: String) =
        appsInCommon exists (appGroups(groupName).processNames contains _.getName)
      val updatedGroupTimes =
        for ((groupName, spentTime) <- groupTimes)
          yield (groupName, if (anyAppsRunningFromGroup(groupName)) spentTime + refreshMinutes else spentTime)
      val areAnyAppsRunningFromAGroupInWarningZone =
        updatedGroupTimes exists { case (groupName, spentTime) =>
          val timeLeft = appGroups(groupName).dailyMinutes - spentTime
          0 < timeLeft && timeLeft <= alarmThresholdMinutes && anyAppsRunningFromGroup(groupName)
        }
      val trayTooltip = updatedGroupTimes map { case (groupName, spentTime) =>
        s"$groupName: $spentTime/${appGroups(groupName).dailyMinutes}\n"
      } reduce (_ + _)

      updateTrayIconTooltip(trayTooltip)
      for ((groupName, spentTime) <- updatedGroupTimes if spentTime >= appGroups(groupName).dailyMinutes) {
        for (pi <- apps if appGroups(groupName).processNames contains pi.getName) {
          playSound(killSound)
          JProcesses.killProcess(pi.getPid.toInt)
        }
      }
      if (areAnyAppsRunningFromAGroupInWarningZone) {
        playSound(alarmSound)
      }
      Thread.sleep((refreshMinutes * 60 * 1000).toLong)
      loop(updatedGroupTimes, apps)
    }

    loop(Map(appGroupList map (appGroup => (appGroup.name, 0.0)): _*), Seq())
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

  /**
    * Parses the JSON text of the configuration file into separate values.
    * @param jsonText
    * @return A tuple containing 5 values:
    *         a list of app groups
    *         filename to the kill sound
    *         filename to the alarm sound
    *         how long before an app group's time runs out should the program start emitting warning sounds
    *         how often to poll the running processes (and to emit warning sounds)
    */
  private def parseConfig(jsonText: String): (Seq[AppGroup], String, String, Double, Double) = {
    // TODO: extract json field names into constants
    // TODO: find native scala json library?
    val configJson = JSONValue.parse(jsonText).asInstanceOf[JSONObject]
    val appGroups = asScalaIterator(configJson.get("groups").asInstanceOf[JSONArray].iterator()) map (_.asInstanceOf[JSONObject]) map (jsonAppGroup =>
      new AppGroup(
        jsonAppGroup.get("name").asInstanceOf[String],
        jsonAppGroup.get("dailyMinutes").asInstanceOf[Double],
        (asScalaIterator(jsonAppGroup.get("processNames").asInstanceOf[JSONArray].iterator()) map (_.asInstanceOf[String])).toSeq
      )
    )
    (
      appGroups.toSeq,
      configJson.get("killSound").asInstanceOf[String],
      configJson.get("alarmSound").asInstanceOf[String],
      configJson.get("alarmThresholdMinutes").asInstanceOf[Double],
      configJson.get("refreshMinutes").asInstanceOf[Double]
    )
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

class AppGroup(val name: String, val dailyMinutes: Double, val processNames: Seq[String])
