package org.penny_craal.atlatl

import java.time.{Duration, LocalDateTime, LocalTime}

/**
  * @author Ville Jokela
  */
object Helpers {
  implicit class DoubleHelper(minutes: Double) {
    def minutesToTimeString: String = {
      if (Math.abs(minutes) > 120) f"${(minutes / 60).floor}%1.0f h"
      else if (Math.abs(minutes) > 2) f"${minutes.floor}%1.0f m"
      else f"${(minutes * 60).floor}%1.0f s"
    }
  }

  implicit class LocalTimeHelper(lt: LocalTime) {
    def plusMinutes(minutes: Double): LocalTime = lt.plus(Duration.ofMillis((minutes * 60 * 1000).toLong))
    def until(that: LocalTime): Double = TimeRange(lt, that).lengthMinutes
  }

  implicit class LocalDateTimeHelper(ldt: LocalDateTime) {
    def plusMinutes(minutes: Double): LocalDateTime = ldt.plus(Duration.ofMillis((minutes * 60 * 1000).toLong))
  }
}
