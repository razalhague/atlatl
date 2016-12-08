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
    val killSoundFilename: String,
    val alarmSoundFilename: String,
    val alarmThresholdMinutes: Double,
    val refreshMinutes: Double
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
