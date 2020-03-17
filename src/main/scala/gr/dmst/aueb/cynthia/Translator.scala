package gr.dmst.aueb.cynthia


object PythonPrinter {
  def emitPrint(q: Query, ret: String) = q match {
    case SetRes (_)     => "for r in res:\n\tprint (r.id)"
    case AggrRes (_, _) => "print(res)"
  }
}

sealed trait Translator {
  def emitPrint(q: Query): String
  def translatePred(pred: Predicate): String
  def translateQuerySet(qs: QuerySet, db: DB): String
  def translateQuery(q: Query, db: DB): String
}


object DjangoTranslator extends Translator {
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


object SQLAlchemyTranslator extends Translator {
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

  def apply(q: Query, target: Target): String = {
    val t = target.orm match {
      case Django(_, _, _)   => DjangoTranslator
      case SQLAlchemy (_, _) => SQLAlchemyTranslator
    }
    t.translateQuery(q, target.db) + t.emitPrint(q)
  }
}
