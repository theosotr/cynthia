package gr.dmst.aueb.cynthia

import scala.util.{Try, Success, Failure}
import scala.sys.process._

import gr.dmst.aueb.cynthia.translators.{ORMTranslator, UnsupportedException}

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

object QueryExecutor {
  
  def apply(q: Query, target: Target) = {
    Try(ORMTranslator(q, target)) match {
      case Success(q) =>
        Utils.writeToFile(target.orm.getDriverPath(target.db), q)
        val (stdout, stderr) = (new StringBuilder, new StringBuilder)
        target.getTargetCommand() !
        ProcessLogger(stdout append _ + "\n", stderr append _ + "\n") match {
          case 0 => Ok(stdout.toString)
          case _ => Fail(stderr.toString)
        }
      case Failure(UnsupportedException(msg)) => Unsupported(msg)
      case Failure(e) => throw e
    }
  }
}
