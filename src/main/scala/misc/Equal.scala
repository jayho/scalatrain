package misc

/**
 * Created by jhoffmann on 30/04/15.
 */
// Type class
trait Equal[-A] {
  def equal(a1: A, a2: A): Boolean
}

object Equal {

  // Type class glue
  implicit class EqualOps[A](val a1: A) extends AnyVal {
    def ===(a2: A)(implicit instance: Equal[A] = anyEqual): Boolean = instance.equal(a1, a2)
  }

  // Type class instances
//  implicit val intEqual = new Equal[Int] {
//    def equal(a1: Int, a2: Int): Boolean = a1 == a2
//  }
  implicit val stringEqual = new Equal[String] {
    def equal(a1: String, a2: String): Boolean = a1 != a2
  }
  // don't make default implicit otherwise it will be always chosen
  val anyEqual = new Equal[Any] {
    def equal(a1: Any, a2: Any): Boolean = a1 == a2
  }

}

