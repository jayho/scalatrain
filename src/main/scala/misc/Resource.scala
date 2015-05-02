package misc

/**
 * Created by jhoffmann on 30/04/15.
 */
object Resource {

  type Closeable = {
    def close(): Unit
  }

  // Loan pattern
  def withResource[A, B <: Closeable](resource: B)(block: B => A) = {
    try {
      block(resource)
    } finally {
      resource.close()
    }
  }

  // Alternative
//  def withResource[A, R <: { def close() : Unit}](resource: R)(block: R => A) = {
//    try {
//      block(resource)
//    } finally {
//      resource.close()
//    }
//  }

}
