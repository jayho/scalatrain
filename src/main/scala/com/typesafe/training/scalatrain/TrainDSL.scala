package com.typesafe.training.scalatrain

import scala.collection.immutable.Seq

/**
 * Created by jhoffmann on 30/04/15.
 */
object TrainDSL {

  // Initial cycle ensures that a train with schedule of size 1 won't be created

  implicit class TrainInfoOps(val info: TrainInfo) extends AnyVal {
    def at(time: Time) = new TrainInfoFromOps(info, time)
  }

  private[TrainDSL] class TrainInfoFromOps(info: TrainInfo, time: Time) {
    def from(station: Station) = new TrainInfoAtOps(info, Vector(time -> station))
  }

  private[TrainDSL] class TrainInfoAtOps(info: TrainInfo, schedule: Seq[(Time, Station)]) {
    def at(time: Time) = new TrainFromOps(info, schedule, time)
  }

  // Entry point when we have an existing train (e.g. after finishing the initial cycle)

  implicit class TrainOps(val train: Train) extends AnyVal {
    def at(time: Time) = new TrainFromOps(train.info, train.schedule, time)
  }

  // Exit point that creates a train

  private[TrainDSL] class TrainFromOps(info: TrainInfo, schedule: Seq[(Time, Station)], time: Time) {
    def from(station: Station) = new Train(info, schedule :+ time -> station)
  }

}
