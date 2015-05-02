package misc

/**
 * Created by jhoffmann on 29/04/15.
 */
object Fold {

  def map[A, B](as: Seq[A])(f: A => B): Seq[B] =
    as.foldRight(Seq.empty[B]){ case (a, b) => f(a) +: b }

  def flatMap[A, B](as: Seq[A])(f: A => Seq[B]): Seq[B] =
    as.foldRight(Seq.empty[B]){ case (a, b) => f(a) ++ b }

  def filter[A](as: Seq[A])(pred: A => Boolean): Seq[A] =
    as.foldRight(Seq.empty[A]){ case (a, b) => if (pred(a)) a +: b else b }

}
