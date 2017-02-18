package org.penny_craal.atlatl

import java.time.LocalDateTime

import akka.actor.{Actor, ActorLogging}
import better.files._
import org.json.simple.{JSONArray, JSONObject, JSONValue}

import scala.collection.JavaConverters._

case class LoadGroupTimes(sender: Actor)
case class SaveGroupTimes(groupTimes: Seq[GroupRuntimeInformation], time: LocalDateTime)

/**
  * @author Ville Jokela
  */
class PersistenceActor(private val conf: Config) extends Actor with ActorLogging {
  private val timestampKey = "timestamp"
  private val groupTimesKey = "groupTimes"
  private val groupNameKey = "groupName"
  private val spentMinutesKey = "spentMinutes"

  private val dataFolderName = "atlatl"
  private val dataFileName = "data.json"

  private val osName = System.getProperty("os.name").toLowerCase
  log.info("OS: " + osName)
  private lazy val systemDefault: File =
    if (osName.contains("win"))
      System.getenv("AppData") / dataFolderName / dataFileName
    else if (osName.contains("os x"))
      System.getProperty("user.home") / "Library" / "Application Support" / dataFolderName / dataFileName
    else if (osName.contains("linux"))
      System.getProperty("user.home") / ("." + dataFolderName) / dataFileName
    else
      System.getProperty("user.home") / ("." + dataFileName) // should take care of most unix-like systems
  private val file = conf.dataFile match {
    case Some(fileName) => File(fileName)
    case None => systemDefault
  }
  log.info("persistence file: " + file.toString)

  override def receive: Receive = {
    case LoadGroupTimes => sender ! loadData()
    case SaveGroupTimes(groupTimes, timestamp) => saveData(groupTimes, timestamp)
  }

  private def loadData(): Option[(LocalDateTime, Seq[GroupRuntimeInformation])] = {
    try {
      if (file.isReadable) {
        val json = JSONValue.parse(file.contentAsString).asInstanceOf[JSONObject]
        if (json != null) {
          val saveTime = LocalDateTime.parse(json.get(timestampKey).asInstanceOf[String])
          val groupTimes =
            (asScalaIterator(json.get(groupTimesKey).asInstanceOf[JSONArray].iterator()) map (_.asInstanceOf[JSONObject]) map jsonToGri).toSeq
          Some((saveTime, groupTimes))
        } else {
          log.warning("data file corrupted")
          None
        }
      } else {
        log.warning("data file not readable")
        None
      }
    } catch {
      case e: Exception =>
        log.error(e, "failed to read data file")
        None
    }
  }

  private def saveData(groupTimes: Seq[GroupRuntimeInformation], timestamp: LocalDateTime): Unit = {
    val data = Map(
      timestampKey -> timestamp.toString,
      groupTimesKey -> (groupTimes map griToJson).asJava
    )
    log.info("writing groupTimes to file")
    if (file.isWriteable || file.notExists) {
      file.createIfNotExists(createParents = true)
      file.write(JSONObject.toJSONString(mapAsJavaMap[String, Object](data)))
    } else {
      log.warning("cannot write data to file")
    }
  }

  private def griToJson(gri: GroupRuntimeInformation): JSONObject = {
    new JSONObject(Map(
      groupNameKey -> gri.name,
      spentMinutesKey -> gri.spentMinutes
    ).asJava)
  }

  private def jsonToGri(jo: JSONObject) = {
    GroupRuntimeInformation(
      jo.get(groupNameKey).asInstanceOf[String],
      jo.get(spentMinutesKey).asInstanceOf[Double],
      0.0
    )
  }
}
