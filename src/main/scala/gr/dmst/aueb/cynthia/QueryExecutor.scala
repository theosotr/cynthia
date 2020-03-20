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
  val pythonDump =
    """import numbers, decimal
    |def dump(x):
    |    if isinstance(x, numbers.Number):
    |       print(round(decimal.Decimal(x), 2))
    |    else:
    |       print(x)""".stripMargin

  def emitPreamble(target: Target) = target.orm match {
    case Django (name, _, setDir) =>
      new StringBuilder("import os, django\n")
        .append("from django.db.models import *\n")
        .append("os.environ.setdefault('DJANGO_SETTINGS_MODULE', '")
        .append(setDir)
        .append(".settings')\n")
        .append("django.setup()\n")
        .append("from ")
        .append(name)
        .append(".models import *\n")
        .append(pythonDump + "\n")
        .toString()
    case SQLAlchemy(_, _) =>
      new StringBuilder("from sqlalchemy import create_engine, or_, and_, not_, func\n")
        .append("from sqlalchemy.orm import sessionmaker\n")
        .append("from models import *\n")
        .append("engine = create_engine('")
        .append(target.db.getURI())
        .append("')\n")
        .append("Session = sessionmaker(bind=engine)\n")
        .append("session = Session()\n")
        .append(pythonDump + "\n")
        .toString()
    case Sequelize(_, _) => {
      val dbsettings = target.db match {
        case Postgres(user, password, dbname) =>
          new StringBuilder("new Sequelize(")
            .append(Utils.quoteStr(dbname) + ", ")
            .append(Utils.quoteStr(user) + ", ")
            .append(Utils.quoteStr(password) + ", {\n")
            .append("  dialect: " + Utils.quoteStr(target.db.getName()) + ",\n")
        case MySQL(user, password, dbname) =>
          new StringBuilder("new Sequelize(")
            .append(Utils.quoteStr(dbname) + ", ")
            .append(Utils.quoteStr(user) + ", ")
            .append(Utils.quoteStr(password) + ", {\n")
            .append("  dialect: " + Utils.quoteStr(target.db.getName()) + ",\n")
        case SQLite(dbname) =>
          new StringBuilder("new Sequelize(")
            .append(Utils.quoteStr(dbname) + ",")
            .append("'user', 'password', {\n")
            .append("  dialect: " + Utils.quoteStr(target.db.getName()) + ",\n")
            .append("  storage: '")
            .append(dbname + "',\n")
      }
      val setstr = dbsettings.append("  logging: false,\n")
        .append("  define: { timestamps: false }\n});\n")
        .toString
      new StringBuilder("const {Op, Sequelize} = require('sequelize');\n")
        .append("const sequelize = " + setstr)
        .toString
    }
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
