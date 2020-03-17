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

  def emitPreamble(target: Target) = target.orm match {
    case Django (name, _, setDir) =>
      new StringBuilder("import os, django\n")
        .append("os.environ.setdefault('DJANGO_SETTINGS_MODULE', '")
        .append(setDir)
        .append(".settings')\n")
        .append("django.setup()\n")
        .append("from ")
        .append(name)
        .append(".models import *\n")
        .toString()
    case SQLAlchemy(_, _) =>
      new StringBuilder("from sqlalchemy import create_engine\n")
        .append("from sqlalchemy.orm import sessionmaker\n")
        .append("from models import *\n")
        .append("engine = create_engine('")
        .append(target.db.getURI())
        .append("')\n")
        .append("Session = sessionmaker(bind=engine)\n")
        .append("session = Session()\n")
        .toString()
  }
  
  def apply(q: String, target: Target) = {
    val query = emitPreamble(target) + q
    Utils.writeToFile(target.orm.getDriverPath(target.db), query)
    val (stdout, stderr) = (new StringBuilder, new StringBuilder)
    target.getTargetCommand() ! ProcessLogger(stdout append _, stderr append _) match {
      case 0 => Ok(stdout.toString)
      case _ => Fail(stderr.toString)
    }
  }
}
