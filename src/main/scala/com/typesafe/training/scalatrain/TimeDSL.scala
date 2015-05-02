package com.typesafe.training.scalatrain

/**
 * Created by jhoffmann on 30/04/15.
 */
object TimeDSL {

  def pm(t: Time) = {
    require(t.asMinutes > 0 && t.asMinutes <= 12 * 60, "can only call am/pm on time between 0 and 12")
    if (t.hours == 12) t.copy(hours = 0) else t.copy(hours = t.hours + 12)
  }

  def am(t: Time) = {
    require(t.asMinutes > 0 && t.asMinutes < 12 * 60, "can only call am/pm on time between 0 and 12")
    t
  }

  implicit class IntTimeOps(val i: Int) extends AnyVal {
    def am = TimeDSL.am(Time(i))
    def pm = TimeDSL.pm(Time(i))
    def ::(hours: Int) = Time(hours, i)
  }

  implicit class TimeOps(val t: Time) extends AnyVal {
    def am = TimeDSL.am(t)
    def pm = TimeDSL.pm(t)
  }

}
