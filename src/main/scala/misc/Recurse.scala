package misc

/**
 * Created by jhoffmann on 29/04/15.
 */
object Recurse {

  def map[A, B](as: Seq[A])(f: A => B): Seq[B] = as match {
    case head +: tail => f(head) +: map(tail)(f)
    case _ => Seq.empty
  }

  def flatMap[A, B](as: Seq[A])(f: A => Seq[B]): Seq[B] = as match {
    case head +: tail => f(head) ++ flatMap(tail)(f)
    case _ => Seq.empty
  }

  def filter[A](as: Seq[A])(pred: A => Boolean): Seq[A] = as match {
    case head +: tail if pred(head) => head +: filter(tail)(pred)
    case head +: tail => filter(tail)(pred)
    case _ => Seq.empty
  }

}
