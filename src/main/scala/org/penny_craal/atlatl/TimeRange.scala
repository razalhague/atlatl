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
      start.until(end, ChronoUnit.SECONDS).toDouble / 60

  override def toString: String =
    start.toString + "-" + end.toString
}