package com.typesafe.training.scalatrain

/**
 * Created by jhoffmann on 29/04/15.
 */
case class Hop(from: Station, to: Station, train: Train) {
  require(from != to, s"from and to stations must not be equal, was $from -> $to")
  require(train.backToBackStations contains (from -> to), s"$from and $to must be back-to-back stations of ${train.info}")

  val departureTime = train.departureTimes(from)
  val arrivalTime = train.departureTimes(to)
}
