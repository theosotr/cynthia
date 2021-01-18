/*
 * Copyright (c) 2020-2021 Thodoris Sotiropoulos, Stefanos Chaliasos
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cynthia.utils

import scala.util.Random

object RUtils {
  val tlRandom = ThreadLocal.withInitial[Random](() => new Random);

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
    val specialChars = Seq("%", "_", "'", "/", "\\")
    if (!special || bool()) w
    else
      new StringBuilder(w)
        .insert(w.size - 1, chooseFrom(specialChars))
        .insert(w.size - 1, chooseFrom(specialChars))
        .toString
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
    r.nextBoolean()

  def seed(s: Long) =
    r.setSeed(s)

  def r =
    tlRandom.get()
}
