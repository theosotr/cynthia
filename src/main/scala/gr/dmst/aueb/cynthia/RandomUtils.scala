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

  def string(n: Int = 10): String =
    r.alphanumeric.take(r.nextInt(n)) mkString ""

  def word(n: Int = 9): String =
    r.alphanumeric.filter { _.isLetter }.take(r.nextInt(n) + 1) mkString ""

  def integer(n: Int = 10): Int =
    r.nextInt(n)

  def bool() =
    r.nextBoolean
}
