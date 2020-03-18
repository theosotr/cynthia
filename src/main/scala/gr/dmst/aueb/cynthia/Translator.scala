package gr.dmst.aueb.cynthia


object PythonPrinter {
  def emitPrint(q: Query, ret: String) = q match {
    case SetRes (_)     => "for r in res:\n\tprint (int(r.id))"
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
  def getDjangoFieldName(field: String) =
    field.split('.').toList match {
      case Nil | _ :: Nil => field
      case _ :: t         => t.mkString("__")
    }

  override def emitPrint(q: Query) =
    PythonPrinter.emitPrint(q, "res")

  override def translatePred(pred: Predicate) = pred match {
    case Eq(k, Value(v, Quoted))    => getDjangoFieldName(k) + "=" + Utils.quoteStr(v)
    case Eq(k, Value(v, UnQuoted))  => getDjangoFieldName(k) + "=" + v
    case Gt(k, Value(v, Quoted))    => getDjangoFieldName(k) + "__gt=" + Utils.quoteStr(v)
    case Gt(k, Value(v, UnQuoted))  => getDjangoFieldName(k) + "__gt=" + v
    case Gte(k, Value(v, Quoted))   => getDjangoFieldName(k) + "__gte=" + Utils.quoteStr(v)
    case Gte(k, Value(v, UnQuoted)) => getDjangoFieldName(k) + "__gte=" + v
    case Lt(k, Value(v, Quoted))    => getDjangoFieldName(k) + "__lt=" + Utils.quoteStr(v)
    case Lt(k, Value(v, UnQuoted))  => getDjangoFieldName(k) + "__le=" + v
    case Lte(k, Value(v, Quoted))   => getDjangoFieldName(k) + "__lte=" + Utils.quoteStr(v)
    case Lte(k, Value(v, UnQuoted)) => getDjangoFieldName(k) + "__lte=" + v
    case Contains(k, v)             => getDjangoFieldName(k) + "__contains=" + Utils.quoteStr(v)
    case Not(pred)                  => "~Q(" + translatePred(pred) + ")"
    case Or(p1, p2)                 => "Q(" + translatePred(p1) + ") | Q(" + translatePred(p2) + ")" 
    case And(p1, p2)                => "Q(" + translatePred(p1) + "), Q(" + translatePred(p2) + ")"
  }

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
    case Apply(Sort(spec), qs) => {
      val orderSpec = spec.foldLeft (Seq[String]()) { (acc, x) =>
        x match {
          case (k, Desc) => acc :+ Utils.quoteStr("-" + getDjangoFieldName(k))
          case (k, Asc)  => acc :+ Utils.quoteStr(getDjangoFieldName(k))
        }
      } mkString(",")
      translateQuerySet(qs, db) + ".order_by(" + orderSpec + ")"
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

  override def translatePred(pred: Predicate) = pred match {
    case Eq(k, Value(v, Quoted))    => k + "==" + Utils.quoteStr(v)
    case Eq(k, Value(v, UnQuoted))  => k + "==" + v
    case Gt(k, Value(v, Quoted))    => k + " > " + Utils.quoteStr(v)
    case Gt(k, Value(v, UnQuoted))  => k + " > " + v
    case Gte(k, Value(v, Quoted))   => k + " >= " + Utils.quoteStr(v)
    case Gte(k, Value(v, UnQuoted)) => k + " >= " + v
    case Lt(k, Value(v, Quoted))    => k + " < " + Utils.quoteStr(v)
    case Lt(k, Value(v, UnQuoted))  => k + " < " + v
    case Lte(k, Value(v, Quoted))   => k + " <= " + Utils.quoteStr(v)
    case Lte(k, Value(v, UnQuoted)) => k + " <= " + v
    case Contains(k, v)             => k + ".contains(" + Utils.quoteStr(v) + ")"
    case Not(pred)                  => "not_(" + translatePred(pred) + ")"
    case Or(p1, p2)                 => "or_(" + translatePred(p1) + ", " + translatePred(p2) + ")" 
    case And(p1, p2)                => "and_(" + translatePred(p1) + ", " + translatePred(p2) + ")"
  }

  override def translateQuerySet(qs: QuerySet, db: DB) = qs match {
    case New (m, f)              => "session.query(" + m + ")"
    case Apply (Filter (pred), qs) =>
      translateQuerySet(qs, db) + ".filter(" + translatePred(pred) + ")"
    case Apply (Sort (spec), qs) => {
      val orderSpec = spec.foldLeft (Seq[String]()) { (acc, x) =>
        x match {
          case (k, Desc) => acc :+ (k + ".desc()")
          case (k, Asc)  => acc :+ (k + ".asc()")
        }
      } mkString(",")
      translateQuerySet(qs, db) + ".order_by(" + orderSpec + ")"
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


object SequelizeTranslator extends Translator {
  override def emitPrint(q: Query) =
    ""

  override def translatePred(pred: Predicate) = ""

  override def translateQuerySet(qs: QuerySet, db: DB) = ""

  override def translateQuery(q: Query, db: DB) = ""
}


object ORMTranslator {

  def apply(q: Query, target: Target): String = {
    val t = target.orm match {
      case Django(_, _, _)   => DjangoTranslator
      case SQLAlchemy (_, _) => SQLAlchemyTranslator
      case Sequelize(_, _)   => SequelizeTranslator 
    }
    t.translateQuery(q, target.db) + t.emitPrint(q)
  }
}
