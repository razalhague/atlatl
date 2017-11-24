package org.penny_craal.atlatl

import java.time.{LocalDateTime, LocalTime}

import Helpers._

/**
  * @author Ville Jokela
  */

abstract class Snapshot {
  val conf: Config
  val appGroups: Map[String, AppGroup]
  val trackedApps: Seq[String]
  val groupTimes: Seq[GroupRuntimeInformation]
  val runningTrackedApps: Seq[String]
  val refreshDateTime: LocalDateTime
  val refreshTime: LocalTime
  val refreshTimeRange: TimeRange
  def anyAppsRunningFromGroup(groupName: String): Boolean =
    runningTrackedApps exists (appGroups(groupName).processNames contains _)
  def strippedOfHistory: Snapshot
  def strippedOfData: Snapshot
}

case class StrippedSnapshot(
  override val appGroups: Map[String, AppGroup],
  override val conf: Config,
  override val groupTimes: Seq[GroupRuntimeInformation],
  override val refreshDateTime: LocalDateTime
) extends Snapshot {
  override val trackedApps: Seq[String] = appGroups.values.flatMap(_.processNames).toSeq.distinct
  override val runningTrackedApps = Seq()
  override val refreshTime: LocalTime = refreshDateTime.toLocalTime
  override val refreshTimeRange = TimeRange(refreshTime.plusMinutes(-conf.refreshMinutes), refreshTime)
  override def strippedOfHistory: Snapshot = this
  override def strippedOfData: Snapshot = this
}

case class FullSnapshot(
  prev: Snapshot,
  runningProcesses: Seq[String],
  override val refreshDateTime: LocalDateTime
) extends Snapshot {
  override val conf: Config = prev.conf
  override val appGroups: Map[String, AppGroup] = prev.appGroups
  override val trackedApps: Seq[String] = prev.trackedApps
  override val runningTrackedApps: Seq[String] = runningProcesses filter (trackedApps contains _)
  override val refreshTime: LocalTime = refreshDateTime.toLocalTime
  override val refreshTimeRange = TimeRange(prev.refreshTime, refreshTime)
  override val groupTimes: Seq[GroupRuntimeInformation] =
    prev.groupTimes map updatedContinuousUse map updatedSpentMinutes
  override def strippedOfHistory: Snapshot = copy(prev = prev.strippedOfData)
  override def strippedOfData: Snapshot = StrippedSnapshot(appGroups, conf, groupTimes, refreshDateTime)
  val anyAppsReachedContinuousUseThreshold: Boolean = groupTimes exists (gri =>
    appGroups(gri.name).trackContinuousUse && gri.continuousUseMinutes >= conf.continuousUseAlarmMinutes
  )

  private def updatedContinuousUse(gri: GroupRuntimeInformation): GroupRuntimeInformation = {
    val newMinutes = if (anyAppsRunningFromGroup(gri.name) && prev.anyAppsRunningFromGroup(gri.name)) {
      gri.continuousUseMinutes match {
        // this is the first time after a reset that this process is found in both apps and prevRunningTrackedApps
        // it must have been running for two refresh intervals for this to happen
        case 0.0 => refreshTimeRange.lengthMinutes * 2
        case preAlarm if preAlarm < conf.continuousUseAlarmMinutes => preAlarm + refreshTimeRange.lengthMinutes
        case postAlarm => postAlarm - conf.continuousUseAlarmMinutes + refreshTimeRange.lengthMinutes
      }
    } else 0.0
    gri.copy(continuousUseMinutes = newMinutes)
  }

  private def updatedSpentMinutes(gri: GroupRuntimeInformation): GroupRuntimeInformation = {
    val newSpentMinutes =
      if (refreshTimeRange contains conf.dailyResetTime) 0.0
      else if (anyAppsRunningFromGroup(gri.name)) gri.spentMinutes + refreshTimeRange.lengthMinutes
      else gri.spentMinutes
    gri.copy(spentMinutes = newSpentMinutes)
  }
}
