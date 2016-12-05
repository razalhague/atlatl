package org.penny_craal.atlatl

import java.time.LocalTime

import org.json.simple.{JSONArray, JSONObject, JSONValue}

import scala.collection.JavaConverters.asScalaIterator

class Config private (
    val appGroups: Seq[AppGroup],
    val killSoundFilename: String,
    val alarmSoundFilename: String,
    val alarmThresholdMinutes: Double,
    val refreshMinutes: Double
  ) {}

object Config {
  // constants for JSON field names
  private val groups = "groups"
  private val name = "name"
  private val dailyMinutes = "dailyMinutes"
  private val processNames = "processNames"
  private val killSound = "killSound"
  private val alarmSound = "alarmSound"
  private val alarmThresholdMinutes = "alarmThresholdMinutes"
  private val refreshMinutes = "refreshMinutes"
  private val forbiddenTimes = "forbiddenTimes"
  private val forbiddenTimeStart = "start"
  private val forbiddenTimeEnd = "end"

  def parse(jsonText: String): Config = {
    // TODO: find native scala json library?
    val configJson = JSONValue.parse(jsonText).asInstanceOf[JSONObject]
    val appGroups = asScalaIterator(configJson.get(groups).asInstanceOf[JSONArray].iterator()) map (_.asInstanceOf[JSONObject]) map (jsonAppGroup =>
      new AppGroup(
        jsonAppGroup.get(name).asInstanceOf[String],
        if (jsonAppGroup.containsKey(dailyMinutes))
          Some(jsonAppGroup.get(dailyMinutes).asInstanceOf[Double])
        else
          None,
        parseForbiddenTimes(jsonAppGroup),
        (asScalaIterator(jsonAppGroup.get(processNames).asInstanceOf[JSONArray].iterator()) map (_.asInstanceOf[String])).toSeq
      )
    )
    new Config(
      appGroups.toSeq,
      configJson.get(killSound).asInstanceOf[String],
      configJson.get(alarmSound).asInstanceOf[String],
      configJson.get(alarmThresholdMinutes).asInstanceOf[Double],
      configJson.get(refreshMinutes).asInstanceOf[Double]
    )
  }

  private def parseForbiddenTimes(jsonAppGroup: JSONObject): Seq[TimeRange] = {
    if (jsonAppGroup.containsKey(forbiddenTimes))
      (asScalaIterator(jsonAppGroup.get(forbiddenTimes).asInstanceOf[JSONArray].iterator()) map (_.asInstanceOf[JSONObject]) map (forbiddenTime =>
        new TimeRange(
          LocalTime.parse(forbiddenTime.get(forbiddenTimeStart).asInstanceOf[String]),
          LocalTime.parse(forbiddenTime.get(forbiddenTimeEnd).asInstanceOf[String])
        )
      )).toSeq
    else
      Seq()
  }
}
