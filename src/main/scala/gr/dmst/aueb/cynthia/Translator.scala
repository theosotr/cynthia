package gr.dmst.aueb.cynthia

case class Target (orm: ORM, db: DB)

object PythonPrinter {
  def emitPrint(q: Query, ret: String) = q match {
    case SetRes (_)     => "for r in res:\n\tprint (r.id)"
    case AggrRes (_, _) => "print(res)"
  }
}

sealed trait Translator {
  def emitPreamble(q: Query, db: DB): String
  def emitPrint(q: Query): String
  def translatePred(pred: Predicate): String
  def translateQuerySet(qs: QuerySet, db: DB): String
  def translateQuery(q: Query, db: DB): String
}


// TODO: Flatten query for efficiency
case class DjangoTranslator (orm: Django) extends Translator {
  override def emitPreamble(q: Query, db: DB) = 
    new StringBuilder("import os, django\n")
      .append("os.environ.setdefault('DJANGO_SETTINGS_MODULE', '")
      .append(orm.setDir)
      .append(".settings')\n")
      .append("django.setup()\n")
      .append("from ")
      .append(orm.name)
      .append(".models import *\n")
      .toString()

  override def emitPrint(q: Query) =
    PythonPrinter.emitPrint(q, "res")

  override def translatePred(pred: Predicate) =
    ""

  override def translateQuerySet(qs: QuerySet, db: DB) = qs match {
    case New(m, f) => {
      val dbname = db match {
        case Postgres (_, _, _) => "postgres"
        case MySQL (_, _, _)    => "mysql"
        case SQLite (_)         => "default"
      }
      m + ".objects.using('" + dbname + "')"
    }
    case Apply(Filter(pred), qs) => {
      val predStr: String = translatePred(pred)
      val qsStr = translateQuerySet(qs, db)
      qsStr + ".filter(" + predStr + ")"
    }
    case Union (qs1, qs2) =>
      new StringBuilder(translateQuerySet(qs1, db))
        .append(".union(")
        .append(translateQuerySet(qs2, db))
        .append(")")
        .toString()
    case Intersect (qs1, qs2) =>
      new StringBuilder(translateQuerySet(qs1, db))
        .append(".intersect(")
        .append(translateQuerySet(qs2, db))
        .append(")")
        .toString()
  }

  override def translateQuery(q: Query, db: DB) = q match {
    case SetRes (qs)         => "res = " + translateQuerySet(qs, db) + "\n"
    case AggrRes (Count, qs) => "res = " + translateQuerySet(qs, db) + ".count()\n"
    case AggrRes (Sum, qs)   => "res = " + translateQuerySet(qs, db) + ".sum()\n"
  }
}


case class SQLAlchemyTranslator (orm: SQLAlchemy) extends Translator {
  override def emitPreamble(q: Query, db: DB) =
    new StringBuilder("from sqlalchemy import create_engine\n")
      .append("from sqlalchemy.orm import sessionmaker\n")
      .append("from models import *\n")
      .append("engine = create_engine('")
      .append(db.getURI())
      .append("')\n")
      .append("Session = sessionmaker(bind=engine)\n")
      .append("session = Session()\n")
      .toString()

  override def emitPrint(q: Query) =
    PythonPrinter.emitPrint(q, "res")

  override def translatePred(pred: Predicate) =
    ""

  override def translateQuerySet(qs: QuerySet, db: DB) = qs match {
    case New (m, f)              => "session.query(" + m + ")"
    case Apply (Filter (pred), qs) =>
      translateQuerySet(qs, db) + ".filter(" + translatePred(pred) + ")"
    case Union (qs1, qs2) =>
      new StringBuilder(translateQuerySet(qs1, db))
        .append(".union(")
        .append(translateQuerySet(qs2, db))
        .append(")")
        .toString()
    case Intersect (qs1, qs2) =>
      new StringBuilder(translateQuerySet(qs1, db))
        .append(".intersect(")
        .append(translateQuerySet(qs2, db))
        .append(")")
        .toString()
  }

  override def translateQuery(q: Query, db: DB) = q match {
    case SetRes (qs)         => "res = " + translateQuerySet(qs, db) + "\n"
    case AggrRes (Count, qs) => "res = " + translateQuerySet(qs, db) + ".count()\n"
    case AggrRes (Sum, qs)   => "res = " + translateQuerySet(qs, db) + ".sum()\n"
  }
}


object ORMTranslator {
  def translate(q: Query, target: Target): Unit = {
    val t = target.orm match {
      case Django(_, _, _)   => DjangoTranslator (target.orm.asInstanceOf[Django])
      case SQLAlchemy (_, _) => SQLAlchemyTranslator (target.orm.asInstanceOf[SQLAlchemy])
    }
    val str = t.emitPreamble(q, target.db) + t.translateQuery(q, target.db) + t.emitPrint(q)
    Utils.writeToFile(target.orm.getDriverPath(), str)
  }
}
