package gr.dmst.aueb.cynthia

import scala.sys.process._

sealed trait QueryRes
case class Ok(res: String) extends QueryRes {
  override def toString() =
    "Ok[" + res + "]"
}

case class Fail(msg: String) extends QueryRes {
  override def toString() =
    "Fail"
}

object QueryExecutor {
  
  def apply(q: String, target: Target) = {
    Utils.writeToFile(target.orm.getDriverPath(target.db), q)
    val (stdout, stderr) = (new StringBuilder, new StringBuilder)
    target.getTargetCommand() ! ProcessLogger(stdout append _, stderr append _) match {
      case 0 => Ok(stdout.toString)
      case _ => Fail(stderr.toString)
    }
  }
}
