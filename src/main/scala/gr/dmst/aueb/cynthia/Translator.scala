package gr.dmst.aueb.cynthia


case class QueryStr(
  ret: String,
  q: Option[String],
  builtQ: Seq[String] = Seq()) {

  def >>(qstr: QueryStr) =
    QueryStr(qstr.ret, qstr.q, toBuiltQ())

  def <<(qstr: QueryStr) =
    QueryStr(ret, None, toBuiltQ() ++ qstr.toBuiltQ())

  def toBuiltQ() = q match {
    case None    => builtQ
    case Some(q) => builtQ :+ (ret + " = " + q)
  }

  override def toString() =
    toBuiltQ mkString ("\n")
} 

case class State(
  db: DB,
  sources: Set[String] = Set(),
  preds: Set[Predicate] = Set(),
  orders: Seq[(String, Order)] = Seq(),
  aggrs: Seq[Aggregate] = Seq(),
  query: Option[QueryStr] = None,
  numGen: Iterator[Int] = Stream.from(1).iterator
  ) {

  def +(source: String) =
    State(db, sources + source, preds, orders, aggrs, query, numGen)

  def ++(pred: Predicate): State =
    State(db, sources, preds + pred, orders, aggrs, query, numGen)

  def :+(o: (String, Order)): State =
    State(db, sources, preds, orders :+ o, aggrs, query, numGen)

  def :-(a: Seq[Aggregate]): State =
    State(db, sources, preds, orders, aggrs ++ a, query, numGen)

  def >>(qstr: QueryStr): State = query match {
    case None        =>
      State(db, sources, preds, orders, aggrs, Some(qstr), numGen)
    case Some(query) =>
      State(db, sources, preds, orders, aggrs, Some(query >> qstr), numGen)
  }
}


sealed abstract class Translator {
  def evalQuerySet(s: State)(qs: QuerySet): State = qs match {
    case New(m, f)               => s + m // Add source to state
    case Apply(Filter(pred), qs) => evalQuerySet(s)(qs) ++ pred // Add predicate to state
    case Apply(Sort(spec), qs)   =>
      spec.foldLeft(evalQuerySet(s)(qs)) { (s, x) => s :+ x } // Add order spec to state
    case Union (qs1, qs2) =>
      unionQueries(evalQuerySet(s)(qs1), evalQuerySet(s)(qs2)) // Merge queries
    case Intersect (qs1, qs2) =>
      intersectQueries(evalQuerySet(s)(qs1), evalQuerySet(s)(qs2)) // Intersect queries
  }

  def evalAggrQuery(s: State)(q: Query): State = q match {
    case AggrRes(aggrs, qs) => evalQuerySet(s)(qs) :- aggrs
    case _ => ???
  }

  def apply(q: Query, db: DB): String = {
    q match {
      case SetRes (qs) => {
        val f = evalQuerySet(State(db)) _ andThen constructQuery _
        val qStr = f(qs)
        qStr.toString + "\n" + emitPrint(q, qStr.ret)
      }
      case _ => {
        val f = evalAggrQuery(State(db)) _ andThen constructQuery _
        val qStr = f(q)
        qStr.toString + "\n" + emitPrint(q, qStr.ret)
      }
    }
  }

  def emitPrint(q: Query, ret: String): String
  def constructQuery(state: State): QueryStr
  def unionQueries(s1: State, s2: State): State
  def intersectQueries(s1: State, s2: State): State
}


object DjangoTranslator extends Translator {

  def getDjangoFieldName(field: String) =
    field.split('.').toList match {
      case Nil | _ :: Nil => field
      case _ :: t         => t.mkString("__")
    }

  override def emitPrint(q: Query, ret: String) = q match {
    case SetRes (_)         => s"for r in $ret:\n  dump(r.id)"
    case AggrRes (aggrs, _) => aggrs map { x => {
        val label = x.label match {
          case None => throw new Exception(
            "You must provide a label for root aggregates")
          case Some(l) => l
        }
        s"dump($ret['$label'])"
      }
    } mkString ("\n")
  }

  def constructFilter(preds: Set[Predicate]) =
    preds map { x =>
      (Str("filter(") << translatePred(x) << ")").!
    } mkString(".")

  def constructOrderBy(spec: Seq[(String, Order)]) = spec match {
    case Seq() => ""
    case _     =>
      (
        Str("order_by(") << (
          spec map { x =>
            x match {
              case (k, Desc) => Utils.quoteStr("-" + getDjangoFieldName(k))
              case (k, Asc)  => Utils.quoteStr(getDjangoFieldName(k))
            }
          } mkString(",")
        ) << ")"
      ).!
  }

  def constructQueryPrefix(s: State) =  s.query match {
    case None =>
      s.sources.toList match {
        case Nil | _ :: _ :: _ => ???
        case h :: Nil =>
          val dbname = s.db match {
            case Postgres (_, _, _) => "postgres"
            case MySQL (_, _, _)    => "mysql"
            case SQLite (_)         => "default"
          }
          QueryStr("ret" + s.numGen.next().toString,
                   Some(h + ".objects.using('" + dbname + "')"))
      }
    case Some(q) => q
  }

  def constructPrimAggr(aggr: Aggregate) = {
    val (field, op, label) = aggr match {
      case Count(l)      => ("*", "Count", l)
      case Sum(field, l) => (field, "Sum", l)
      case Avg(field, l) => (field, "Avg", l)
      case Min(field, l) => (field, "Min", l)
      case Max(field, l) => (field, "Max", l)
      case _             => ??? // Unreachable case
    }
    val k = Utils.quoteStr(getDjangoFieldName(field))
    label match {
      case None    => op + "(" + k + ", output_field=FloatField())"
      case Some(l) => l + "=" + op + "(" + k + ", output_field=FloatField())"
    }
  }

  def constructAggr(aggr: Aggregate): String = aggr match {
    case Add(a1, a2, None)    => "(" + constructAggr(a1) + " + " + constructAggr(a2) + ")"
    case Add(a1, a2, Some(l)) => l + "=" + constructAggr(a1) + " + " + constructAggr(a2)
    case Sub(a1, a2, None)    => "(" + constructAggr(a1) + " - " + constructAggr(a2) + ")"
    case Sub(a1, a2, Some(l)) => l + "=" + constructAggr(a1) + " - " + constructAggr(a2)
    case Mul(a1, a2, None)    => "(" + constructAggr(a1) + " * " + constructAggr(a2) + ")"
    case Mul(a1, a2, Some(l)) => l + "=" + constructAggr(a1) + " * " + constructAggr(a2)
    case Div(a1, a2, None)    => "(" + constructAggr(a1) + " / " + constructAggr(a2) + ")"
    case Div(a1, a2, Some(l)) => l + "=" + constructAggr(a1) + " / " + constructAggr(a2)
    case _                    => constructPrimAggr(aggr)
  }

  def constructAggrs(aggrs: Seq[Aggregate]) = aggrs match {
    case Seq() => ""
    case _     => "aggregate(" + (aggrs map { constructAggr } mkString (",")) + ")"
  }

  override def constructQuery(s: State) = {
    val qStr = constructQueryPrefix(s)
    qStr >> QueryStr("ret" + s.numGen.next().toString,
      Some(Seq(
        qStr.ret,
        constructFilter(s.preds),
        constructOrderBy(s.orders),
        constructAggrs(s.aggrs)
      ) filter {
        case "" => false
        case _  => true
      }  mkString("."))
    )
  }

  override def unionQueries(s1: State, s2: State) = {
    val (q1, q2) = (constructQuery(s1), constructQuery(s2))
    s1 >> (q1 << q2 >> QueryStr("ret" + s1.numGen.next().toString,
                                Some(q1.ret + ".union(" + q2.ret + ")")))
  }

  override def intersectQueries(s1: State, s2: State) = {
    val (q1, q2) = (constructQuery(s1), constructQuery(s2))
    s1 >> (q1 << q2 >> QueryStr("ret" + s1.numGen.next().toString,
                                Some(q1.ret + ".intersect(" + q2.ret + ")")))
  }

  def translatePred(pred: Predicate): String = pred match {
    case Eq(k, Value(v, Quoted))    =>
      (Str(getDjangoFieldName(k)) << "=" << Utils.quoteStr(v)).!
    case Eq(k, Value(v, UnQuoted))  =>
      (Str(getDjangoFieldName(k)) << "=" << v).!
    case Gt(k, Value(v, Quoted))    =>
      (Str(getDjangoFieldName(k)) << "__gt=" << Utils.quoteStr(v)).!
    case Gt(k, Value(v, UnQuoted))  =>
      (Str(getDjangoFieldName(k)) << "__gt=" << v).!
    case Gte(k, Value(v, Quoted))   =>
      (Str(getDjangoFieldName(k)) << "__gte=" << Utils.quoteStr(v)).!
    case Gte(k, Value(v, UnQuoted)) =>
      (Str(getDjangoFieldName(k)) << "__gte=" << v).!
    case Lt(k, Value(v, Quoted))    =>
      (Str(getDjangoFieldName(k)) << "__lt=" << Utils.quoteStr(v)).!
    case Lt(k, Value(v, UnQuoted))  =>
      (Str(getDjangoFieldName(k)) << "__le=" << v).!
    case Lte(k, Value(v, Quoted))   =>
      (Str(getDjangoFieldName(k)) << "__lte=" << Utils.quoteStr(v)).!
    case Lte(k, Value(v, UnQuoted)) =>
      (Str(getDjangoFieldName(k)) << "__lte=" << v).!
    case Contains(k, v)             =>
      (Str(getDjangoFieldName(k)) << "__contains=" << Utils.quoteStr(v)).!
    case Not(pred)                  =>
      (Str("~Q(") << translatePred(pred) << ")").!
    case Or(p1, p2)                 =>
      (Str("Q(") << translatePred(p1) << ") | Q(" << translatePred(p2) << ")").!
    case And(p1, p2)                =>
      (Str("Q(") << translatePred(p1) << "), Q(" << translatePred(p2) << ")").!
  }
}


object SQLAlchemyTranslator extends Translator {
  override def emitPrint(q: Query, ret: String) = q match {
    case SetRes(_)         => s"for r in $ret:\n  dump(r.id)"
    case AggrRes(aggrs, _) => aggrs match {
      case Seq(Count(_)) => s"dump($ret)"
      case _ => aggrs map { x => {
        val label = x.label match {
          case None => throw new Exception(
            "You must provide a label for root aggregates")
          case Some(l) => l
        }
        s"dump($ret.$label)"
        }} mkString ("\n")
      }
    }

  def constructFilter(preds: Set[Predicate]) =
    preds map { x =>
      (Str("filter(") << translatePred(x) << ")").!
    } mkString(".")

  def constructOrderBy(spec: Seq[(String, Order)]) = spec match {
    case Seq() => ""
    case _     =>
      (
        Str("order_by(") << (
          spec map { x =>
            x match {
              case (k, Desc) => k + ".desc()"
              case (k, Asc)  => k + ".asc()"
            }
          } mkString(",")
        ) << ")"
      ).!
  }

  def constructPrimAggr(aggr: Aggregate) = {
    val (field, op, label) = aggr match {
      case Count(l)      => ("", "func.count", l)
      case Sum(field, l) => (field, "func.sum", l)
      case Avg(field, l) => (field, "func.avg", l)
      case Min(field, l) => (field, "func.min", l)
      case Max(field, l) => (field, "func.max", l)
      case _             => ??? // Unreachable case
    }
    label match {
      case None    => op + "(" + field + ")"
      case Some(l) => op + "(" + field + ").label(" + Utils.quoteStr(l) + ")" 
    }
  }

  def constructAggr(aggr: Aggregate): String = aggr match {
    case Add(a1, a2, None)    => "(" + constructAggr(a1) + " + " + constructAggr(a2) + ")"
    case Add(a1, a2, Some(l)) => l + "=" + constructAggr(a1) + " + " + constructAggr(a2)
    case Sub(a1, a2, None)    => "(" + constructAggr(a1) + " - " + constructAggr(a2) + ")"
    case Sub(a1, a2, Some(l)) => l + "=" + constructAggr(a1) + " - " + constructAggr(a2)
    case Mul(a1, a2, None)    => "(" + constructAggr(a1) + " * " + constructAggr(a2) + ")"
    case Mul(a1, a2, Some(l)) => l + "=" + constructAggr(a1) + " * " + constructAggr(a2)
    case Div(a1, a2, None)    => "(" + constructAggr(a1) + " / " + constructAggr(a2) + ")"
    case Div(a1, a2, Some(l)) => l + "=" + constructAggr(a1) + " / " + constructAggr(a2)
    case _                    => constructPrimAggr(aggr)
  }

  def constructQueryPrefix(s: State) =  s.query match {
    case None =>
      s.sources.toList match {
        case Nil => ??? // Unreachable case
        case _   => s.aggrs match {
          case Seq() | Seq(Count(_)) => QueryStr(
            "ret" + s.numGen.next().toString,
            Some("session.query(" + s.sources.mkString(",") + ")"))
          case _ => QueryStr(
            "ret" + s.numGen.next().toString,
            Some("session.query(" + (s.aggrs map { constructAggr } mkString ",") + ")"))
        }
      }
    case Some(q) => q
  }

  override def constructQuery(s: State) = {
    val qStr = constructQueryPrefix(s)
    qStr >> QueryStr("ret" + s.numGen.next().toString,
      Some(Seq(
        qStr.ret,
        constructFilter(s.preds),
        constructOrderBy(s.orders),
        s.aggrs match {
          case Seq()         => ""
          case Seq(Count(_)) => "count()"
          case _             => "first()"
        }
      ) filter {
        case "" => false
        case _  => true
      }  mkString("."))
    )
  }

  override def unionQueries(s1: State, s2: State) = {
    val (q1, q2) = (constructQuery(s1), constructQuery(s2))
    s1 >> (q1 << q2 >> QueryStr("ret" + s1.numGen.next().toString,
                                Some(q1.ret + ".union(" + q2.ret + ")")))
  }

  override def intersectQueries(s1: State, s2: State) = {
    val (q1, q2) = (constructQuery(s1), constructQuery(s2))
    s1 >> (q1 << q2 >> QueryStr("ret" + s1.numGen.next().toString,
                                Some(q1.ret + ".intersect(" + q2.ret + ")")))
  }

  def translatePred(pred: Predicate): String = pred match {
    case Eq(k, Value(v, Quoted))    => (Str(k) << "==" << Utils.quoteStr(v)).!
    case Eq(k, Value(v, UnQuoted))  => (Str(k) << "==" << v).!
    case Gt(k, Value(v, Quoted))    => (Str(k) << " > " << Utils.quoteStr(v)).!
    case Gt(k, Value(v, UnQuoted))  => (Str(k) << " > " << v).!
    case Gte(k, Value(v, Quoted))   => (Str(k) << " >= " << Utils.quoteStr(v)).!
    case Gte(k, Value(v, UnQuoted)) => (Str(k) << " >= " << v).!
    case Lt(k, Value(v, Quoted))    => (Str(k) << " < " << Utils.quoteStr(v)).!
    case Lt(k, Value(v, UnQuoted))  => (Str(k) << " < " << v).!
    case Lte(k, Value(v, Quoted))   => (Str(k) << " <= " << Utils.quoteStr(v)).!
    case Lte(k, Value(v, UnQuoted)) => (Str(k) << " <= " << v).!
    case Contains(k, v)             =>
      (Str(k) << ".contains(" << Utils.quoteStr(v) << ")").!
    case Not(pred)                  =>
      (Str("not_(") << translatePred(pred) << ")").!
    case Or(p1, p2)                 =>
      (Str("or_(") << translatePred(p1) << ", " << translatePred(p2) << ")").! 
    case And(p1, p2)                =>
      (Str("and_(") << translatePred(p1) << ", " << translatePred(p2) << ")").!
  }
}


object SequelizeTranslator extends Translator {
  override def emitPrint(q: Query, ret: String) = q match {
    case SetRes (_)     => s"$ret.then((x) => { x.forEach((x) => console.log(x.id)) })"
    case AggrRes (_, _) => s"$ret.then((x) => console.log(x))"
  }

  def getSeqFieldName(field: String) =
    field.split('.').toList match {
      case Nil | _ :: Nil => field
      case _ :: t         => t.mkString(".")
    }

  def translatePred(pred: Predicate): String = pred match {
    case Eq(k, Value(v, Quoted))    =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.eq]: " << Utils.quoteStr(v) << "}").!
    case Eq(k, Value(v, UnQuoted))  =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.eq]: " << v << "}").!
    case Gt(k, Value(v, Quoted))    =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.gt]: " << Utils.quoteStr(v) << "}").!
    case Gt(k, Value(v, UnQuoted))  =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.gt]: " << v << "}").!
    case Gte(k, Value(v, Quoted))   =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.gte]: " << Utils.quoteStr(v) << "}").!
    case Gte(k, Value(v, UnQuoted)) =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.gte]: " << v << "}").!
    case Lt(k, Value(v, Quoted))    =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.lt]: " << Utils.quoteStr(v) << "}").!
    case Lt(k, Value(v, UnQuoted))  =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.lt]: " << v << "}").!
    case Lte(k, Value(v, Quoted))   =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.lte]: " << Utils.quoteStr(v) << "}").!
    case Lte(k, Value(v, UnQuoted)) =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.lte]: " << v << "}").!
    case Contains(k, v)             =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.substring]: " << Utils.quoteStr(v) << "}").!
    case Not(pred)                  =>
      (Str("[Op.not]: {") << translatePred(pred) << "}").!
    case Or(p1, p2)                 =>
      (Str("[Op.or]: [") << "{" << translatePred(p1) << "}," <<
          "{" << translatePred(p2) << "}" << "]").!
    case And(p1, p2)                =>
      (Str("[Op.and]: [") << "{" << translatePred(p1) << "}," <<
          "{" << translatePred(p2) << "}" << "]").!

  }

  def constructWhere(preds: Set[Predicate]) =
    (
      Str("where: {\n  [Op.and]: [\n") << (
        preds map { x => "    {" + translatePred(x) + "}" } mkString(",")
      ) << "  ]\n}"
    ).!

  def constructOrderBy(spec: Seq[(String, Order)]) = spec match {
    case Seq() => ""
    case _     =>
      (
        Str("order: [\n") << (
          spec map { x => x match {
            case (k, Asc)  => "  [" + Utils.quoteStr(getSeqFieldName(k)) + ", 'ASC']"
            case (k, Desc) => "  [" + Utils.quoteStr(getSeqFieldName(k)) + ", 'DESC']"
            }
          } mkString(",")
        ) << "]"
      ).!
  }


  override def constructQuery(state: State): QueryStr = state.sources.toList match {
    case h :: Nil => {
      val qStr = QueryStr(h, Some("sequelize.import(" +
        Utils.quoteStr(h.toLowerCase + ".js") + ")"))
      val method = state.aggrs match {
        case Seq()       => "findAll"
        case _           => "count"
      }
      val q = (Str(h) << "." << method << "({\n" <<
        (
          Seq(constructWhere(state.preds), constructOrderBy(state.orders))
          filter (x => x match {
            case "" => false
            case _  => true
          }) mkString(",\n")
        ) << "\n})").!
      qStr >> QueryStr("ret" + state.numGen.next(), Some(q))
    }
    case _ => ???
  }
  override def unionQueries(s1: State, s2: State): State = s1
  override def intersectQueries(s1: State, s2: State): State = s1
}


object ORMTranslator {

  def apply(q: Query, target: Target): String = {
    val t = target.orm match {
      case Django(_, _, _)   => DjangoTranslator
      case SQLAlchemy (_, _) => SQLAlchemyTranslator
      case Sequelize(_, _)   => SequelizeTranslator 
    }
    t(q, target.db)
  }
}
