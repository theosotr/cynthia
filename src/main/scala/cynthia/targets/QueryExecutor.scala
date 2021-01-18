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

package cynthia.targets

import scala.util.{Try, Success, Failure}
import scala.sys.process._

import cynthia.lang.{Query, InvalidQuery, State}
import cynthia.translators.{ORMTranslator, UnsupportedException}
import cynthia.utils.Utils

sealed trait QueryRes
case class Ok(res: String) extends QueryRes {
  override def toString() =
    "Ok[" + res + "]"
}

case class Unsupported(res: String) extends QueryRes {
  override def toString() =
    "Unsupported[" + res + "]"
}

case class Fail(msg: String) extends QueryRes {
  override def toString() =
    "Fail[" + msg + "]"
}

case class Invalid(msg: String) extends QueryRes {
  override def toString() =
    "Invalid[" + msg + "]"
}

object QueryExecutor {

  def apply(q: Query, s: State, target: Target) = {
    Try(ORMTranslator(q, s, target)) match {
      case Success(ormQ) =>
        Utils.writeToFile(target.orm.getDriverPath(target.db), ormQ)
        val (stdout, stderr) = (new StringBuilder, new StringBuilder)
        target.getTargetCommand() !
          ProcessLogger(stdout append _ + "\n", stderr append _ + "\n") match {
          case 0 =>
            if (stderr.length != 0 && stdout.length == 0)
              Fail(stderr.toString)
            else if (q.queryset.ordered()) Ok(stdout.toString)
            else {
              // If the queryset is not ordered we sort the results
              // so that we can perform safe comparisons about the output.
              val l = stdout.toString.split('\n').sortWith(_ < _)
              Ok(l mkString "\n")
            }
          case _ => Fail(stderr.toString)
        }
      case Failure(UnsupportedException(msg)) => Unsupported(msg)
      case Failure(InvalidQuery(msg))         => Invalid(msg)
      case Failure(e)                         => throw e
    }
  }
}
