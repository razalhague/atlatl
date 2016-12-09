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
import java.time.temporal.ChronoUnit

class TimeRange(val start: LocalTime, val end: LocalTime) {
  def inRange(time: LocalTime): Boolean = {
    if (start.isAfter(end))
      time.isAfter(start) || time.isBefore(end)
    else
      time.isAfter(start) && time.isBefore(end)
  }

  val lengthMinutes: Double =
    if (start.isAfter(end))
      24 * 60 - (end.until(start, ChronoUnit.MILLIS).toDouble / 60000)
    else
      start.until(end, ChronoUnit.MILLIS).toDouble / 60000

  override def toString: String =
    start.toString + "-" + end.toString
}