package com.typesafe.training

import scala.annotation.tailrec

/**
 * Created by jhoffmann on 29/04/15.
 */
package object scalatrain {

  // Solution via Context Bounds and type
  type OrderedView[A] = A => Ordered[A]

  @tailrec
  def isIncreasing[A: OrderedView](as: Seq[A]): Boolean = {
    as match {
      // +: works for all sequence types, :: works only for lists
      case t1 +: t2 +: _ => t1 < t2 && isIncreasing(as.tail)
      case _ => true
    }
  }

  // Solution via using implicitly
  //  @tailrec
  //  def isIncreasing[A: Ordering](as: Seq[A]): Boolean = {
  //    as match {
  //      // +: works for all sequence types, :: works only for lists
  //      case t1 +: t2 +: _ => implicitly[Ordering[A]].lt(t1, t2) && isIncreasing(as.tail)
  //      case _ => true
  //    }
  //  }

  // Solution via View Bound
  //  @tailrec
  //  def isIncreasing[A <% Ordered[A]](as: Seq[A]): Boolean = {
  //    as match {
  //      // +: works for all sequence types, :: works only for lists
  //      case t1 +: t2 +: _ => (t1 < t2) && isIncreasing(as.tail)
  //      case _ => true
  //    }
  //  }

  def isIncreasingSliding[A <: Ordered[A]](as: Seq[A]) = {
    as sliding 2 forall {
      case Seq(t1, t2) => t1 < t2
      case _ => true
    }
  }

}
