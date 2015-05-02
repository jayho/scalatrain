package misc

import scala.annotation.tailrec

/**
 * Created by jhoffmann on 30/04/15.
 */
object Loop {

  @tailrec
  def repeatWhile(cond: => Boolean)(block: => Unit): Unit = {
    if (cond) {
      block
      repeatWhile(cond)(block)
    }
  }

  def repeat(block: => Unit) = new {
    def until(condition: => Boolean) = repeatWhile(!condition)(block)
  }

}
