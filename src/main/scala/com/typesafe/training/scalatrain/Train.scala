/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

import scala.collection.immutable.Seq

case class Train(info: TrainInfo, schedule: Seq[(Time, Station)]) {
  require(schedule.size >= 2, "schedule must contain at least two elements")
  // TODO Verify that `schedule` is strictly increasing in time
  require(isIncreasing(schedule.map(_._1)), "schedule must be strictly increasing in time")

  val stations: Seq[Station] =
    // Could also be expressed in short notation: schedule map (_._2)
    schedule.map(trainAndStation => trainAndStation._2)

  def timeAt(station: Station): Option[Time] =
    // Could also be expressed in notation: schedule find (_._2 == station) map (_._1)
    schedule.find(timeAndStation => timeAndStation._2 == station).map(found => found._1)

  val backToBackStations = stations zip stations.tail

  val departureTimes = schedule.map(_.swap)(collection.breakOut).toMap
  // better than: schedule.map(_.swap).toMap

}

object Station {
  implicit def stringToStation(string: String) = Station(string)
//  implicit val s: String => Station = Station.apply
}

case class Station(name: String) extends AnyVal

sealed abstract class TrainInfo {
  def number: Int
}
case class InterCityExpress(number: Int, hasWifi: Boolean = false) extends TrainInfo
case class RegionalExpress(number: Int) extends TrainInfo
case class BavarianRegional(number: Int) extends TrainInfo
