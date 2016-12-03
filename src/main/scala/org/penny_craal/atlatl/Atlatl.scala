package org.penny_craal.atlatl

import java.awt.{SystemTray, TrayIcon}
import java.io.{File, IOException}
import java.time.LocalTime
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
    for (appGroup <- appGroupList; ft <- appGroup.forbiddenTimes if ft.lengthMinutes < alarmThresholdMinutes) {
      throw new RuntimeException("Forbidden time ranges must be longer than the alarm threshold")
    }
    if (appGroupList.isEmpty) {
      throw new RuntimeException("Cannot run with no defined app groups")
    }
    if (refreshMinutes <= 0) {
      throw new RuntimeException("refresh period must be positive")
    }
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
      val currentTime = LocalTime.now()
      val alarmTime = currentTime.plusSeconds((alarmThresholdMinutes * 60).toLong)
      val shouldAlarm =
        updatedGroupTimes exists { case (groupName, spentTime) =>
          anyAppsRunningFromGroup(groupName) &&
            appGroups(groupName).shouldBeKilled(spentTime + alarmThresholdMinutes, alarmTime) && // should be killed in $alarmThresholdMinutes
            !appGroups(groupName).shouldBeKilled(spentTime, currentTime) // but should not be killed right now
        }
      updateTrayIconTooltip(trayTooltip(updatedGroupTimes, appGroups))
      for ((groupName, spentTime) <- updatedGroupTimes if appGroups(groupName).shouldBeKilled(spentTime, currentTime)) {
        for (pi <- apps if appGroups(groupName).processNames contains pi.getName) {
          playSound(killSound)
          JProcesses.killProcess(pi.getPid.toInt)
        }
      }
      if (shouldAlarm) {
        playSound(alarmSound)
      }
      Thread.sleep((refreshMinutes * 60 * 1000).toLong)
      loop(updatedGroupTimes, apps)
    }

    loop(Map(appGroupList map (appGroup => (appGroup.name, 0.0)): _*), Seq())
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
        if (jsonAppGroup.containsKey("dailyMinutes"))
          Some(jsonAppGroup.get("dailyMinutes").asInstanceOf[Double])
        else
          None,
        parseForbiddenTimes(jsonAppGroup),
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

  private def parseForbiddenTimes(jsonAppGroup: JSONObject): Seq[TimeRange] = {
    if (jsonAppGroup.containsKey("forbiddenTimes"))
      (asScalaIterator(jsonAppGroup.get("forbiddenTimes").asInstanceOf[JSONArray].iterator()) map (_.asInstanceOf[JSONObject]) map (forbiddenTime =>
        new TimeRange(
          LocalTime.parse(forbiddenTime.get("start").asInstanceOf[String]),
          LocalTime.parse(forbiddenTime.get("end").asInstanceOf[String])
        )
      )).toSeq
    else
      Seq()
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
