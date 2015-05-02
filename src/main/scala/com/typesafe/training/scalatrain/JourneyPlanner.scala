/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

class JourneyPlanner(trains: Set[Train]) {

  val stations: Set[Station] =
  // Could also be expressed in short notation: trains flatMap (_.stations)
    trains.flatMap(train => train.stations)

  val hops: Map[Station, Set[Hop]] = {
    val allHops = for {
      train <- trains
      (from, to) <- train.backToBackStations
    } yield Hop(from, to, train)

    allHops groupBy { _.from }
  }

  def trainsAt(station: Station): Set[Train] =
    // Could also be expressed in short notation: trains filter (_.stations contains station)
    trains.filter(train => train.stations.contains(station))

  def stopsAt(station: Station): Set[(Time, Train)] =
    for {
      train <- trains
      time <- train.timeAt(station)
    } yield (time, train)

  def isShortTrip(from: Station, to: Station): Boolean =
    trains.exists(train =>
      train.stations.dropWhile(station => station != from) match {
        case `from` +: `to` +: _      => true
        case `from` +: _ +: `to` +: _ => true
        case _                        => false
      }
    )

  def connections(from: Station, to: Station, departureTime: Time): Set[Seq[Hop]] = {
    require(from != to, s"from and to stations must not be equal, was $from -> $to")
    def connections(soFar: Seq[Hop]): Set[Seq[Hop]] =
      if (soFar.last.to == to) Set(soFar)
      else {
        val soFarStations: Seq[Station] = soFar.head.from +: soFar.map(_.to)
        val possibleHops = hops.getOrElse(soFar.last.to, Set())
        possibleHops.filter { hop =>
          hop.departureTime >= soFar.last.arrivalTime && !(soFarStations contains hop.to)
        } flatMap { hop =>
          connections(soFar :+ hop)
        }
      }

    val firstHops = hops.getOrElse(from, Set())
    firstHops.filter(hop => hop.departureTime >= departureTime) flatMap { hop =>
      connections(Seq(hop))
    }

  }
}
