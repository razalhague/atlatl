package org.penny_craal.atlatl

import java.time.LocalTime

class AppGroup(val name: String, val dailyMinutes: Option[Double], val forbiddenTimes: Seq[TimeRange], val processNames: Seq[String]) {
  if (dailyMinutes.isEmpty && forbiddenTimes.isEmpty) {
    throw new IllegalArgumentException("An app group must define either a daily time limit or a forbidden time range")
  }
  if (processNames.isEmpty) {
    throw new IllegalArgumentException("An app group must define process names that belong to it")
  }
  dailyMinutes match { case Some(time) =>
    if (time <= 0) {
      throw new IllegalArgumentException("Allowed time for an app group must be positive")
    }
  }

  def isOverTime(spentMinutes: Double): Boolean = dailyMinutes match {
    case None => false
    case Some(allowedMinutes) => spentMinutes >= allowedMinutes
  }

  def isDuringForbiddenTime(time: LocalTime): Boolean =
    forbiddenTimes exists (_.inRange(time))

  def shouldBeKilled(spentMinutes: Double, time: LocalTime): Boolean =
    isOverTime(spentMinutes) || isDuringForbiddenTime(time)
}

