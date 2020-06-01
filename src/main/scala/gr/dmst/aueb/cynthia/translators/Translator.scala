package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._


final case class UnsupportedException(private val message: String)
extends Exception(message)

final case class InvalidQuery(private val message: String)
extends Exception(message)

case class QueryStr(
  ret: Option[String] = None,
  q: Option[String] = None,
  builtQ: Seq[String] = Seq()) {

  def >>(qstr: QueryStr) =
    QueryStr(qstr.ret, None, toBuiltQ ++ qstr.toBuiltQ)

  def <<(qstr: QueryStr) =
    QueryStr(ret, None, toBuiltQ ++ qstr.toBuiltQ)

  def toBuiltQ() = (ret, q) match {
    case (None, None) | (Some(_), None) => builtQ
    case (Some(r), Some(q)) => builtQ :+ (r + " = " + q)
    case (None, Some(q)) => builtQ :+ q
  }

  override def toString() =
    toBuiltQ mkString ("\n")
}

case class State(
  db: DB,
  source: String = "",                    // model
  fields: Map[String, FieldDecl] = Map(), // FieldDecl.as FieldDecl
  preds: Set[Predicate] = Set(),
  orders: Seq[(String, Order)] = Seq(),   // FieldDecl.as asc or desc
  groupBy: Set[String] = Set(),
  aggrs: Seq[FieldDecl] = Seq(),
  joins: Set[(String, String)] = Set(),
  query: Option[QueryStr] = None,
  numGen: Iterator[Int] = Stream.from(1).iterator
  ) {

  def source(s: String) =
    State(db, s, fields, preds, orders, groupBy, aggrs, joins, query,
          numGen)

  def f(fd: FieldDecl) = fd match {
    case FieldDecl(_, as, _, _) =>
      State(db, source, fields + (as -> fd), preds, orders, groupBy, aggrs,
            joins, query, numGen)
  }

  def pred(p: Predicate): State =
    State(db, source, fields, preds + p, orders, groupBy, aggrs, joins, query,
          numGen)

  def order(o: (String, Order)): State =
    State(db, source, fields, preds, orders :+ o, groupBy, aggrs, joins, query,
          numGen)

  def group(f: Set[String]): State =
    State(db, source, fields, preds, orders, f, aggrs, joins, query,
          numGen)

  def aggr(a: Seq[FieldDecl]): State =
    State(db, source, fields, preds, orders, groupBy, aggrs ++ a, joins, query,
          numGen)

  def join(j: (String, String)): State =
    State(db, source, fields, preds, orders, groupBy, aggrs, joins + j, query,
          numGen)

  def >>(qstr: QueryStr): State = query match {
    case None =>
      State(db, source, fields, preds, orders, groupBy, aggrs, joins,
            Some(qstr), numGen)
    case Some(query) =>
      State(db, source, fields, preds, orders, groupBy, aggrs, joins,
            Some(query >> qstr), numGen)
  }
}


abstract class Translator(val target: Target) {
  val preamble: String

  def updateJoins(field: String, s: State) = {
    def _updateJoins(segs: List[String], s: State): State = segs match {
      case Nil | _ :: Nil | _ :: (_ :: Nil) => s
      case h1 :: (h2 :: t) => {
        val cap = h2.capitalize
        _updateJoins(cap :: t, s join (h1, cap))
      }
    }
    _updateJoins(field.split('.').toList, s)
  }

  def hasAggregate(fields: Set[FieldDecl], store: Map[String, FieldExpr]) = {
    def _hasAggregate(e: FieldExpr): Boolean = e match {
      case Constant(_, _) => false
      case Count(_) | Sum(_) | Avg(_) | Max(_) | Min(_) => true
      case F(f) => store get f match {
        case None    => false
        case Some(e) => _hasAggregate(e)
      }
      case Add(e1, e2) => _hasAggregate(e1) || _hasAggregate(e2)
      case Sub(e1, e2) => _hasAggregate(e1) || _hasAggregate(e2)
      case Mul(e1, e2) => _hasAggregate(e1) || _hasAggregate(e2)
      case Div(e1, e2) => _hasAggregate(e1) || _hasAggregate(e2)
    }
    (fields filter { !FieldDecl.hidden(_) } map FieldDecl.expr) exists _hasAggregate
  }

  def computeGroupBy(source: String, fields: Set[FieldDecl]): Set[String] = {
    val init = (
      Set[String](),
      Set[Constant](),
      Map[String, (FieldExpr, Boolean)]()
    )
    //  Compute a map of field names to their corresponding expression
    //  and an the initial sets of group bys.
    val (groupBy, cons, store) = fields.foldLeft(init) { (acc, x) => {
      val (g, c, s) = acc
      x match {
        case FieldDecl(e, as, _, false) =>
          (if (!e.isAggregate) g + as else g, c, s + (as -> (e, false)))
        case FieldDecl(e, as, _, true) => (g, c, s + (as -> (e, true)))
      }
    }}
    def _handleComplexExpr(e: FieldExpr, as: String, g: Set[String]): Set[String] = {
      val (e1, e2) = e match {
        case Add(e1, e2) => (e1, e2)
        case Sub(e1, e2) => (e1, e2)
        case Mul(e1, e2) => (e1, e2)
        case Div(e1, e2) => (e1, e2)
        case _           => ???
      }
      if (e1.isNaiveAggregate) _computeGroupBy(e2, as, g)
      else if(e2.isNaiveAggregate) _computeGroupBy(e1, as, g)
      else _computeGroupBy(e2, as, _computeGroupBy(e1, as, g))
    }
    def _computeGroupBy(e: FieldExpr, as: String, g: Set[String]): Set[String] = e match {
      case F(f) =>
        store get f match {
          case None         => g + f // the field is native
          case Some((e, h)) =>
            if (e.isAggregate) (g - as)
            else _computeGroupBy(e, as, if (h) g else g + f)
        }
      case Constant(_, _) |
        Count(_) |
        Sum(_) |
        Avg(_) |
        Max(_) |
        Min(_) => g
      case _   => _handleComplexExpr(e, as, g)
    }
    // Compute all fields that must be included in the GROUP BY clause.
    // Examine recursively the fields that are not hidden.
    val groupedF = (fields filter { !FieldDecl.hidden(_) }).foldLeft(groupBy) {
      (s, f) => { f match { case FieldDecl(e, as, _, h) => _computeGroupBy(e, as, s) }
    }}
    // Check if all grouped fields are constants.
    val isConstant = groupedF forall { x => store get x match {
      case Some((x, _)) => x.isConstant
      case None         => false
    }}
    // Check if fields contain aggregate functions.
    val hasAggr = hasAggregate(fields, store map { case (k, v) => (k, v._1) })
    if (!hasAggr) Set()
    // If the list of grouped fields is empty or the list contains only
    // constant values, group by id of table.
    else if (groupedF.isEmpty || isConstant) Set(source + ".id")
    else groupedF
  }

  def traversePredicate(s: State, pred: Predicate): State = pred match {
    case Eq(k, _)       => updateJoins(k, s)
    case Gt(k, _)       => updateJoins(k, s)
    case Gte(k, _)      => updateJoins(k, s)
    case Lt(k, _)       => updateJoins(k, s)
    case Lte(k, _)      => updateJoins(k, s)
    case Contains(k, _) => updateJoins(k, s)
    case Not(pred)      => traversePredicate(s, pred)
    case Or(p1, p2)     => traversePredicate(traversePredicate(s, p1), p2)
    case And(p1, p2)    => traversePredicate(traversePredicate(s, p1), p2)
  }

  def traverseFields(s: State, fields: Seq[String]): State =
    fields.foldLeft (s) { (acc, x) => updateJoins(x, acc) }

  def evalQuerySet(s: State)(qs: QuerySet): State = qs match {
    case New(m, f) => { // Add source and fields to state
      val s1 = s source m
      val s2 = s1 group (computeGroupBy(m, f))
      f.foldLeft(s2) { (acc, x) => acc f (x) }
    }
    case Apply(Filter(pred), qs) => {
      val s1 = evalQuerySet(s)(qs) pred pred // Add predicate to state
      traversePredicate(s1, pred) // update joins
    }
    case Apply(Sort(spec), qs) => {
      val s1 = traverseFields(s, spec map { _._1 })
      spec.foldLeft(evalQuerySet(s1)(qs)) { (s, x) => s order x } // Add order spec to state
    }
    case Union (qs1, qs2) =>
      unionQueries(evalQuerySet(s)(qs1), evalQuerySet(s)(qs2)) // Merge queries
    case Intersect (qs1, qs2) =>
      intersectQueries(evalQuerySet(s)(qs1), evalQuerySet(s)(qs2)) // Intersect queries
  }

  def evalAggrQuery(s: State)(q: Query): State = q match {
    case AggrRes(aggrs, qs) => evalQuerySet(s)(qs) aggr aggrs
    case _ => ??? // Unreachable case
  }

  def apply(q: Query): String = {
    val s1 = State(target.db)
    val (s, qStr) = q match {
      case FirstRes(qs) => {
        val s2 = evalQuerySet(s1)(qs)
        if (s2.orders.isEmpty)
          throw new InvalidQuery(
            "You have to make queryset ordered in order to perform safe comparisons")
        (s2, constructQuery(first = true)(s2))
      }
      case SetRes(qs) => {
        val s2 = evalQuerySet(s1)(qs)
        (s2, constructQuery()(s2))
      }
      case AggrRes(_, _) => {
        val s2 = evalAggrQuery(s1)(q)
        (s2, constructQuery()(s2))
      }
      case SubsetRes(offset, limit, qs) => {
        val s2 = evalQuerySet(s1)(qs)
        if (s2.orders.isEmpty)
          throw new InvalidQuery(
            "You have to make queryset ordered in order to perform safe comparisons")
        (s2, constructQuery(offset = offset, limit = limit)(s2))
      }
    }
    val dfields = s.fields.values.filter { !FieldDecl.hidden(_) }.toSeq match {
      case Seq() => Seq("id")
      case f     => f.map  { FieldDecl.as }.toSeq
    }
    preamble + "\n" + qStr.toString + "\n" + emitPrint(
      q, dfields, qStr.ret.get)
  }

  def emitPrint(q: Query, dFields: Seq[String], ret: String): String
  def constructQuery(first: Boolean = false, offset: Int = 0,
    limit: Option[Int] = None)(state: State): QueryStr
  def unionQueries(s1: State, s2: State): State
  def intersectQueries(s1: State, s2: State): State
}


object TUtils {
  def filterMapAs(f: FieldDecl => Boolean)(fields: Iterable[FieldDecl]) =
    fields filter f map FieldDecl.as

  def mapNonHiddenFields[T](fields: Iterable[FieldDecl], f: FieldDecl => T): Iterable[T] =
    fields filter { !FieldDecl.hidden(_) } map { f }

  def mapHiddenFields[T](fields: Iterable[FieldDecl], f: FieldDecl => T): Iterable[T] =
    fields filter FieldDecl.hidden map { f }

  def filterHidden(fields: Iterable[FieldDecl]) =
    filterMapAs(FieldDecl.hidden)(fields)

  def filterNonAggrHidden(fields: Iterable[FieldDecl]) =
    filterMapAs({x => !(FieldDecl.hidden(x) || FieldDecl.isAggregate(x))})(fields)

  def getAggrAndNonAggr(fields: Iterable[FieldDecl]) =
    fields.foldLeft((Seq[String](), Seq[String]())) { (acc, x) => {
      val (a, b) = acc
      val as = FieldDecl.as(x)
      if (FieldDecl.isAggregate(x) && !FieldDecl.hidden(x)) (a :+ as, b)
      else
        if (!FieldDecl.isAggregate(x) && !FieldDecl.hidden(x)) (a, b :+ as)
        else (a, b)
      }
    }
}


object ORMTranslator {

  def apply(q: Query, target: Target): String = target.orm match {
    case Django(name, _, setDir)      => DjangoTranslator(target)(q)
    case SQLAlchemy (_, _)            => SQLAlchemyTranslator(target)(q)
    case Sequelize(_, _)              => SequelizeTranslator(target)(q)
    case Peewee(_, _)                 => PeeweeTranslator(target)(q)
    case ActiveRecord(_, _)           => ActiveRecordTranslator(target)(q)
  }
}
