package gr.dmst.aueb.cynthia.translators

import scala.collection.immutable.ListMap

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
  source: String = "",                          // model
  fields: Map[String, FieldDecl] = ListMap(),   // FieldDecl.as FieldDecl
  preds: Set[Predicate] = Set(),
  orders: Seq[(String, Order)] = Seq(),         // FieldDecl.as asc or desc
  nonAggrF: Set[String] = Set(),                // Fields for group by (Translator)
  aggrF: Set[String] = Set(),                   // FieldDecl connected with aggregate functions
  constantF: Set[String] = Set(),
  aggrs: Seq[FieldDecl] = Seq(),                // Aggregate functions to apply
  joins: Seq[Seq[String]] = Seq(),              // Models to join with source
  query: Option[QueryStr] = None,               // Query string (target), e.g. Unions
  combined: Boolean = false,
  numGen: Iterator[Int] = LazyList.from(1).iterator
  ) {

  def source(s: String) =
    State(db, s, fields, preds, orders, nonAggrF, aggrF, constantF, aggrs,
          joins, query, combined, numGen)

  def f(fd: FieldDecl) = fd match {
    case FieldDecl(_, as, _, _) =>
      State(db, source, fields + (as -> fd), preds, orders, nonAggrF, aggrF,
            constantF, aggrs, joins, query, combined, numGen)
  }

  def pred(p: Predicate): State =
    State(db, source, fields, preds + p, orders, nonAggrF, aggrF, constantF,
          aggrs, joins, query, combined, numGen)

  def order(o: (String, Order)): State =
    State(db, source, fields, preds, orders :+ o, nonAggrF, aggrF, constantF,
          aggrs, joins, query, combined, numGen)

  def nonAggrF(f: Set[String]): State =
    State(db, source, fields, preds, orders, nonAggrF ++ f, aggrF, constantF,
          aggrs, joins, query, combined, numGen)

  def addGroupF(f: String): State =
    State(db, source, fields, preds, orders, nonAggrF + f, aggrF, constantF,
          aggrs, joins, query, combined, numGen)

  def aggrF(f: Set[String]): State =
    State(db, source, fields, preds, orders, nonAggrF, f, constantF, aggrs,
          joins, query, combined, numGen)

  def constantFields(c: Set[String]): State =
    State(db, source, fields, preds, orders, nonAggrF, aggrF, c, aggrs,
          joins, query, combined, numGen)

  def aggr(a: Seq[FieldDecl]): State =
    State(db, source, fields, preds, orders, nonAggrF, aggrF, constantF,
          aggrs ++ a, joins, query, combined, numGen)

  def join(p: Seq[String]): State =
    State(db, source, fields, preds, orders, nonAggrF, aggrF, constantF, aggrs,
          joins :+ p, query, combined, numGen)

  def getJoinPairs(): Set[(String, String)] =
    (joins map { x => {
      val Seq(a, b) = x takeRight 2
      (a, b)
    } }).toSet

  def getNonConstantGroupingFields(): Set[String] =
    if (nonAggrF.isEmpty) nonAggrF
    else {
      val groupingF = nonAggrF filter { x => !constantF.contains(x) }
      if (groupingF.isEmpty) Set(source + ".id")
      else groupingF
    }

  def >>(qstr: QueryStr): State = query match {
    case None =>
      State(db, source, fields, preds, orders, nonAggrF, aggrF, constantF,
            aggrs, joins, Some(qstr), combined, numGen)
    case Some(query) =>
      State(db, source, fields, preds, orders, nonAggrF, aggrF, constantF,
        aggrs, joins, Some(query >> qstr), combined, numGen)
  }

  def combinedQ(): State =
    // revisit
    State(db, source, fields, Set(), Seq(), nonAggrF, aggrF, constantF,
          aggrs, joins, query, true, numGen)
}


abstract class Translator(val target: Target) {
  val preamble: String

  def updateJoins(field: String, s: State) = {
    field split '.' match {
      case Array(_) | Array(_, _) => s
      case arr => {
        val path = (arr dropRight 1).toSeq map { _.capitalize }
        val tail = path drop 1
        (List.range(0, path.size - 1) map { x => path(0) +: tail dropRight x })
          .foldLeft(s) { (acc, x) => acc join x }
      }
    }
  }

  def getConstants(fields: Seq[FieldDecl], store: Map[String, FieldDecl]) = {
    def _getConstants(acc: Set[String], e: FieldExpr, as: String): Set[String] = e match {
      case Constant(_, _) => acc + as
      case F(f) => store get f match {
        case None => acc
        case Some(FieldDecl(e, _, _, _)) => _getConstants(acc, e, as)
      }
      case _ => acc
    }
    (fields filter { !FieldDecl.hidden(_) }).foldLeft(Set[String]()) { (acc, x) =>
      x match { case FieldDecl(e, as, _, _) => _getConstants(acc, e, as) }
    }
  }

  def getAggregate(fields: Seq[FieldDecl], store: Map[String, FieldExpr]) = {
    def _getAggregate(acc: Set[String], e: FieldExpr, as: String): Set[String] = e match {
      case Constant(_, _) => acc
      case Count(_) | Sum(_) | Avg(_) | Max(_) | Min(_) => acc + as
      case F(f) => store get f match {
        case None    => acc
        case Some(e) => _getAggregate(acc, e, as)
      }
      case Add(e1, e2) => _getAggregate(acc, e1, as) ++ _getAggregate(acc, e2, as)
      case Sub(e1, e2) => _getAggregate(acc, e1, as) ++ _getAggregate(acc, e2, as)
      case Mul(e1, e2) => _getAggregate(acc, e1, as) ++ _getAggregate(acc, e2, as)
      case Div(e1, e2) => _getAggregate(acc, e1, as) ++ _getAggregate(acc, e2, as)
    }
    (fields filter { !FieldDecl.hidden(_) }).foldLeft(Set[String]()) { (acc, x) =>
      x match { case FieldDecl(e, as, _, _) => _getAggregate(acc, e, as) }
    }
  }

  def groupFields(s: State, fields: Seq[FieldDecl]): (Set[String], Set[String]) = {
    val init = (
      Set[String](),
      Map[String, (FieldExpr, Boolean)]()
    )
    //  Compute a map of field names to their corresponding expression
    //  and an the initial sets of grouping fields.
    val (groupBy, store) = fields.foldLeft(init) { (acc, x) => {
      val (g, s) = acc
      x match {
        case FieldDecl(e, as, _, false) =>
          (if (!e.isAggregate) g + as else g, s + (as -> (e, false)))
        case FieldDecl(e, as, _, true) => (g, s + (as -> (e, true)))
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
          case Some((e, h)) => {
            val g2 =
              if (e.isAggregate)
                if (h) g - as else (g - as) + f
              else
                if (h) g else g + f
            _computeGroupBy(e, as, g2)
          }
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
    // Check if fields contain aggregate functions.
    val aggrF = getAggregate(fields, store map { case (k, v) => (k, v._1) })
    if (aggrF.isEmpty) (Set(), aggrF)
    // If the list of grouped fields is empty, group by the id of table.
    else if (groupedF.isEmpty) (Set(s.source + ".id"), aggrF)
    else (groupedF, aggrF)
  }

  def setJoinAndGroup(f: String, s: State) =
    s.fields get f match {
      case None =>
        if (!s.aggrF.isEmpty) updateJoins(f, s) addGroupF f
        else updateJoins(f, s)
      case _ => s
    }

  def traverseFieldExpr(s: State, e: FieldExpr,
      updateGroup: Boolean = true): State = e match {
    case Constant(_, _) => s
    case F(f) => s.fields get f match {
      case None =>
        if (updateGroup) setJoinAndGroup(f, s)
        else updateJoins(f, s)
      case Some(FieldDecl(e2, _, _, _)) => traverseFieldExpr(s, e2, false)
    }
    case Count(None) => s
    case Count(Some(e2)) => traverseFieldExpr(s, e2, false)
    case Sum(e2) => traverseFieldExpr(s, e2, false)
    case Avg(e2) => traverseFieldExpr(s, e2, false)
    case Max(e2) => traverseFieldExpr(s, e2, false)
    case Min(e2) => traverseFieldExpr(s, e2, false)
    case Add(e1, e2) => traverseFieldExpr(traverseFieldExpr(s, e1, updateGroup), e2, updateGroup)
    case Sub(e1, e2) => traverseFieldExpr(traverseFieldExpr(s, e1, updateGroup), e2, updateGroup)
    case Mul(e1, e2) => traverseFieldExpr(traverseFieldExpr(s, e1, updateGroup), e2, updateGroup)
    case Div(e1, e2) => traverseFieldExpr(traverseFieldExpr(s, e1, updateGroup), e2, updateGroup)
  }

  def traverseDeclaredFields(s: State, fields: Seq[FieldDecl]): State =
    (fields filter { !FieldDecl.hidden(_) } map FieldDecl.expr).foldLeft (s) {
      (acc, e) => traverseFieldExpr(acc, e)
    }

  def traversePredicate(s: State, pred: Predicate): State = pred match {
    case Eq(k, e)       => traverseFieldExpr(setJoinAndGroup(k, s), e)
    case Gt(k, e)       => traverseFieldExpr(setJoinAndGroup(k, s), e)
    case Gte(k, e)      => traverseFieldExpr(setJoinAndGroup(k, s), e)
    case Lt(k, e)       => traverseFieldExpr(setJoinAndGroup(k, s), e)
    case Lte(k, e)      => traverseFieldExpr(setJoinAndGroup(k, s), e)
    case Contains(k, e) => traverseFieldExpr(setJoinAndGroup(k, s), e)
    case Not(pred)      => traversePredicate(s, pred)
    case Or(p1, p2)     => traversePredicate(traversePredicate(s, p1), p2)
    case And(p1, p2)    => traversePredicate(traversePredicate(s, p1), p2)
  }

  def traverseSortedFields(s: State, fields: Seq[String]): State =
    fields.foldLeft (s) { (acc, x) => setJoinAndGroup(x, acc) }

  def evalQuerySet(s: State)(qs: QuerySet): State = qs match {
    case New(m, f) => { // Add source and fields to state
      val s1 = f.foldLeft(s source m) { (acc, x) => acc f x }
      val s2 = s1.constantFields(getConstants(f, s1.fields))
      val (groupF, aggrF) = groupFields(s2, f)
      val s3 = s2 nonAggrF groupF
      traverseDeclaredFields(s3 aggrF aggrF, f)
    }
    case Apply(Filter(pred), qs) => {
      val s1 = evalQuerySet(s)(qs) pred pred // Add predicate to state
      traversePredicate(s1, pred) // update joins
    }
    case Apply(Sort(spec), qs) => {
      val s1 = traverseSortedFields(evalQuerySet(s)(qs), spec map { _._1 })
      // Finally, always sort by id to eliminate non-deterministic results.
      // Some backends (e.g., MySQL, Postgres) fetch results in an unpredictive
      // manner when the ordering is unspecified.
      val spec2 = if (s1.combined) spec else spec :+ (s1.source + ".id", Desc)
      val s2 = if (!s1.nonAggrF.isEmpty) s1 addGroupF (s1.source + ".id") else s1
      spec2.foldLeft(s2) { (s, x) => {
        // If this field is a constant, we do not add to the set of the sorted
        // fields.
        val s3 = if (s.constantF.contains(x._1)) s else s order x
        val (f, _) = x
        s3.fields get f match {
          // the field is native so add it to grouping fields
          case None => if (!s3.aggrF.isEmpty) s3 addGroupF f else s3
          // we have already examined this field because is declared.
          case _    => s3
        }
      }}
    }
    case Union (qs1, qs2) =>
      unionQueries(evalQuerySet(s)(qs1), evalQuerySet(s)(qs2)).combinedQ // Merge queries
    case Intersect (qs1, qs2) =>
      intersectQueries(evalQuerySet(s)(qs1), evalQuerySet(s)(qs2)).combinedQ // Intersect queries
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
        (s2, constructQuery(s2, first = true))
      }
      case SetRes(qs) => {
        val s2 = evalQuerySet(s1)(qs)
        (s2, constructQuery(s2))
      }
      case AggrRes(f, _) => {
        val s2 = evalAggrQuery(s1)(q)
        (s2, constructQuery(traverseDeclaredFields(s2, f)))
      }
      case SubsetRes(offset, limit, qs) => {
        val s2 = evalQuerySet(s1)(qs)
        if (s2.orders.isEmpty)
          throw new InvalidQuery(
            "You have to make queryset ordered in order to perform safe comparisons")
        (s2, constructQuery(s2, offset = offset, limit = limit))
      }
    }
    val dfields = s.fields.values.filter { !FieldDecl.hidden(_) }.toSeq match {
      case Seq() => Seq("id")
      case f     => f.map  { FieldDecl.as }.toSeq
    }
    preamble + "\n" + qStr.toString + "\n" + emitPrint(
      q, dfields, qStr.ret.get)
  }

  def constructQuery(s: State, first: Boolean = false, offset: Int = 0,
      limit: Option[Int] = None): QueryStr =
    if (s.combined) constructCombinedQuery(s)
    else constructNaiveQuery(s, first, offset, limit)

  def constructNaiveQuery(s: State, first: Boolean , offset: Int,
                          limit: Option[Int]): QueryStr
  def constructCombinedQuery(s: State): QueryStr
  def emitPrint(q: Query, dFields: Seq[String], ret: String): String
  def unionQueries(s1: State, s2: State): State
  def intersectQueries(s1: State, s2: State): State
}


object TUtils {
  def toFieldVar(f: String) =
    f + "_cynthia"

  def toLabel(v: String) =
    v.replace("_cynthia", "")

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
    case Django(_, _, _)      => DjangoTranslator(target)(q)
    case SQLAlchemy (_, _)    => SQLAlchemyTranslator(target)(q)
    case Sequelize(_, _)      => SequelizeTranslator(target)(q)
    case Peewee(_, _)         => PeeweeTranslator(target)(q)
    case ActiveRecord(_, _)   => ActiveRecordTranslator(target)(q)
  }
}
