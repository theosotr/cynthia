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

  def word(special: Boolean = true): String = {
    val w = Words.value(r.nextInt(Words.value.size))
    if (!special || bool) w
    else
      w.patch(
        0, chooseFrom(Seq("%", "_", "\"", "'", "/", "\\")), w.size - 1
      ).patch(
        0, chooseFrom(Seq("%", "_", "\"", "'", "/", "\\")), w.size - 1
      )
  }

  def subword(): String = {
    val w = word()
    val from = integer(n = w.size)
    val to = integer(from = from, n = w.size - from)
    w.slice(from, to)
  }

  def integer(from: Int = 0, n: Int = 10): Int =
    r.nextInt(n) + from

  def bool() =
    r.nextBoolean
}
