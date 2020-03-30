package gr.dmst.aueb.cynthia

import scala.util.Random


object RUtils {
  val r = new Random

  def chooseFrom[T](s: Seq[T]): T =
    s(r.nextInt(s.size))
}
