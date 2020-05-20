package gr.dmst.aueb.cynthia

import scala.util.Random


object RUtils {
  val r = new Random

  def chooseFrom[T](s: Seq[T]): T =
    // TODO: Check the size of n
    s(r.nextInt(s.size))

  def sample[T](s: Seq[T]): Seq[T] = {
    // TODO: Check the size of n
    val n = r.nextInt(s.size)
    r.shuffle(s).take(n)
  }
}
