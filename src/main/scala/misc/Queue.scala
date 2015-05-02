package misc

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ListBuffer
import scala.collection.{mutable, SeqLike}

/**
 * Created by jhoffmann on 29/04/15.
 */
object Queue {

  def apply[A](elems: A*): Queue[A] = new Queue(elems)

  private def newBuilder[A]: mutable.Builder[A, Queue[A]] = ListBuffer[A]() mapResult(new Queue(_))
//  private def newBuilder2[A] = ListBuffer[A]() mapResult(Vector(_:_*))

  // Our instance of a type class CanBuildFrom will make map return the correct type
  implicit def canBuildFrom[A, B] = new CanBuildFrom[A, B, Queue[B]] {
    override def apply(): mutable.Builder[B, Queue[B]] = newBuilder
    override def apply(from: A): mutable.Builder[B, Queue[B]] = newBuilder
  }
}

class Queue[+A] private (val elements: Seq[A]) extends Seq[A] with SeqLike[A, Queue[A]] {

  def enqueue[T >: A](elem: T): Queue[T] = new Queue(elements :+ elem)
//  def enqueue[T >: A](elem: T): Queue[T] = Queue((elements :+ elem): _*)

  def dequeue: (A, Queue[A]) = elements match {
    case Nil => throw new UnsupportedOperationException("Can't dequeue on empty Queue")
    case head +: tail => head -> new Queue(tail)
//    case head +: tail => head -> Queue(tail: _*)
  }

  override def hashCode(): Int = elements.hashCode()

  override def equals(that: scala.Any): Boolean = that match {
    case q: Queue[A] => (this eq q) || elements == q.elements
    case _ => false
  }

  override def toString: String = s"Queue(${elements.mkString(", ")})"

  // Custom Scala Collections:

  override def length: Int = elements.length

  override def iterator: Iterator[A] = elements.iterator

  override def apply(idx: Int): A = elements(idx)

  override protected[this] def newBuilder: mutable.Builder[A, Queue[A]] = Queue.newBuilder

}
