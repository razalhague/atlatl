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

class AppGroup(val name: String, val dailyMinutes: Option[Double], val trackContinuousUse: Boolean, val forbiddenTimes: Seq[TimeRange], val processNames: Seq[String]) {
  require(dailyMinutes.nonEmpty || forbiddenTimes.nonEmpty, "An app group must define either a daily time limit or a forbidden time range")
  require(processNames.nonEmpty, "An app group must define process names that belong to it")
  require(dailyMinutes forall (_ > 0), "Allowed time for an app group must be positive")

  def isOverTime(spentMinutes: Double): Boolean = dailyMinutes match {
    case None => false
    case Some(allowedMinutes) => spentMinutes >= allowedMinutes
  }

  def isDuringForbiddenTime(time: LocalTime): Boolean =
    forbiddenTimes exists (_.contains(time))

  def shouldBeKilled(spentMinutes: Double, time: LocalTime): Boolean =
    isOverTime(spentMinutes) || isDuringForbiddenTime(time)
}

