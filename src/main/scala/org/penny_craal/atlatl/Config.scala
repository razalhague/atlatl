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

import java.time.LocalTime

import org.json.simple.{JSONArray, JSONObject, JSONValue}

import scala.collection.JavaConverters.asScalaIterator

class Config private (
    val appGroups: Seq[AppGroup],
    val hideExitMenuItem: Boolean,
    val dataFile: Option[String],
    val killSoundFilename: String,
    val killAlarmSoundFilename: String,
    val continuousUseAlarmSoundFilename: String,
    val alarmThresholdMinutes: Double,
    val suspensionDelayMinutes: Double,
    val suspensionDurationMinutes: Double,
    val refreshMinutes: Double,
    val continuousUseAlarmMinutes: Double,
    val dailyResetTime: LocalTime
  ) {
  for (appGroup <- appGroups; ft <- appGroup.forbiddenTimes if ft.lengthMinutes < alarmThresholdMinutes) {
    throw new IllegalArgumentException("Forbidden time ranges must be longer than the alarm threshold")
  }
  if (appGroups.isEmpty) {
    throw new IllegalArgumentException("At least one app group must be defined")
  }
  if (refreshMinutes <= 0) {
    throw new IllegalArgumentException("Refresh period must be positive")
  }
}

object Config {
  // constants for JSON field names
  private val hideExitMenuItem = "hideExitMenuItem"
  private val dataFile = "dataFile"
  private val groups = "groups"
  private val name = "name"
  private val dailyMinutes = "dailyMinutes"
  private val trackContinuousUse = "trackContinuousUse"
  private val processNames = "processNames"
  private val killSound = "killSound"
  private val killAlarmSound = "killAlarmSound"
  private val continuousUseAlarmSound = "continuousUseAlarmSound"
  private val alarmThresholdMinutes = "alarmThresholdMinutes"
  private val suspensionDelayMinutes = "suspensionDelayMinutes"
  private val suspensionDurationMinutes = "suspensionDurationMinutes"
  private val refreshMinutes = "refreshMinutes"
  private val continuousUseAlarmMinutes = "continuousUseAlarmMinutes"
  private val forbiddenTimes = "forbiddenTimes"
  private val forbiddenTimeStart = "start"
  private val forbiddenTimeEnd = "end"
  private val dailyResetTime = "dailyResetTime"

  def parse(jsonText: String): Config = {
    // TODO: find native scala json library?
    val configJson = JSONValue.parse(jsonText).asInstanceOf[JSONObject]
    val appGroups = asScalaIterator(configJson.getValue[JSONArray](groups).iterator()) map (_.asInstanceOf[JSONObject]) map (jsonAppGroup =>
      new AppGroup(
        jsonAppGroup.getValue[String](name),
        jsonAppGroup.getOptional[Any](dailyMinutes) map (_.asDoubleMinutes),
        jsonAppGroup.getOptional[Boolean](trackContinuousUse) getOrElse false,
        parseForbiddenTimes(jsonAppGroup),
        (asScalaIterator(jsonAppGroup.getValue[JSONArray](processNames).iterator()) map (_.asInstanceOf[String])).toSeq
      )
    )
    new Config(
      appGroups.toSeq,
      configJson.getValue[Boolean](hideExitMenuItem),
      configJson.getOptional[String](dataFile),
      configJson.getValue[String](killSound),
      configJson.getValue[String](killAlarmSound),
      configJson.getValue[String](continuousUseAlarmSound),
      configJson.get(alarmThresholdMinutes).asDoubleMinutes,
      configJson.get(suspensionDelayMinutes).asDoubleMinutes,
      configJson.get(suspensionDurationMinutes).asDoubleMinutes,
      configJson.get(refreshMinutes).asDoubleMinutes,
      configJson.get(continuousUseAlarmMinutes).asDoubleMinutes,
      LocalTime.parse(configJson.getValue[String](dailyResetTime))
    )
  }

  private def parseForbiddenTimes(jsonAppGroup: JSONObject): Seq[TimeRange] = {
    if (jsonAppGroup.containsKey(forbiddenTimes))
      (asScalaIterator(jsonAppGroup.getValue[JSONArray](forbiddenTimes).iterator()) map (_.asInstanceOf[JSONObject]) map (forbiddenTime =>
        new TimeRange(
          LocalTime.parse(forbiddenTime.getValue[String](forbiddenTimeStart)),
          LocalTime.parse(forbiddenTime.getValue[String](forbiddenTimeEnd))
        )
      )).toSeq
    else
      Seq()
  }

  implicit class DoubleHelper(x: Any) {
    def asDoubleMinutes: Double = x match {
      case l: Long => l.toDouble
      case d: Double => d
      case other => throw new IllegalArgumentException("expected a number, got a " + other.getClass.getSimpleName)
    }
  }

  implicit class JsonObjectHelper(jsonObject: JSONObject) {
    def getValue[V](key: String): V =
      jsonObject.get(key).asInstanceOf[V]

    def getOptional[V](key: String): Option[V] =
      if (jsonObject.containsKey(key))
        Some(jsonObject.getValue[V](key))
      else
        None
  }
}
