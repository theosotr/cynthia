/*
 * Copyright (c) 2020-2021 Thodoris Sotiropoulos, Stefanos Chaliasos
 *
 * This program is free software: you can redistribute it and/or modify  
 * it under the terms of the GNU General Public License as published by  
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License 
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cynthia.translators

import scala.collection.immutable.ListMap

import cynthia.Target
import cynthia.lang._
import cynthia.orms.{Django, Peewee, SQLAlchemy, ActiveRecord, Sequelize, Pony}
import cynthia.utils.Utils


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
  distinct: Option[String] = None,
  combined: Boolean = false,
  from: Option[CombinedState] = None,
  numGen: Iterator[Int] = LazyList.from(1).iterator
  ) {

  def source(s: String) =
    State(s, fields, preds, orders, nonAggrF, aggrF, constantF, aggrs,
          joins, query, distinct, combined, from, numGen)

  def f(fd: FieldDecl) = fd match {
    case FieldDecl(_, as, _, _) =>
      State(source, fields + (as -> fd), preds, orders, nonAggrF, aggrF,
            constantF, aggrs, joins, query, distinct, combined, from, numGen)
  }

  def pred(p: Predicate): State =
    State(source, fields, preds + p, orders, nonAggrF, aggrF, constantF,
          aggrs, joins, query, distinct, combined, from, numGen)

  def order(o: (String, Order)): State =
    State(source, fields, preds, orders :+ o, nonAggrF, aggrF, constantF,
          aggrs, joins, query, distinct, combined, from, numGen)

  def nonAggrF(f: Set[String]): State =
    State(source, fields, preds, orders, nonAggrF ++ f, aggrF, constantF,
          aggrs, joins, query, distinct, combined, from, numGen)

  def addGroupF(f: String): State =
    State(source, fields, preds, orders, nonAggrF + f, aggrF, constantF,
          aggrs, joins, query, distinct, combined, from, numGen)

  def aggrF(f: Set[String]): State =
    State(source, fields, preds, orders, nonAggrF, f, constantF, aggrs,
          joins, query, distinct, combined, from, numGen)

  def constantFields(c: Set[String]): State =
    State(source, fields, preds, orders, nonAggrF, aggrF, c, aggrs,
          joins, query, distinct, combined, from, numGen)

  def aggr(a: Seq[FieldDecl]): State =
    State(source, fields, preds, orders, nonAggrF, aggrF, constantF,
          aggrs ++ a, joins, query, distinct, combined, from, numGen)

  def join(p: Seq[String]): State =
    State(source, fields, preds, orders, nonAggrF, aggrF, constantF, aggrs,
          joins :+ p, query, distinct, combined, from, numGen)

  def distinct(d: Option[String]): State = {
    val distinct = d match {
      case Some(x) => Some(x)
      case _       => Some("")
    }
    State(source, fields, preds, orders, nonAggrF, aggrF, constantF, aggrs,
          joins, query, distinct, combined, from, numGen)
  }

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
      State(source, fields, preds, orders, nonAggrF, aggrF, constantF,
            aggrs, joins, Some(qstr), distinct, combined, from, numGen)
    case Some(query) =>
      State(source, fields, preds, orders, nonAggrF, aggrF, constantF,
        aggrs, joins, Some(query >> qstr), distinct, combined, from, numGen)
  }
}


sealed trait CombinedState
case class UnionState(s1: State, s2: State) extends CombinedState
case object UnionState {

  def combine(s1: State, s2: State): State =
    State(s1.source, s1.fields, Set(), Seq(), s1.nonAggrF, s1.aggrF,
          s1.constantF,
          s1.aggrs, s1.joins, s1.query, s1.distinct, true,
          Some(UnionState(s1, s2)),
          s1.numGen)
}
case class IntersectState(s1: State, s2: State) extends CombinedState
case object IntersectState {

  def combine(s1: State, s2: State): State =
    State(s1.source, s1.fields, Set(), Seq(), s1.nonAggrF, s1.aggrF,
          s1.constantF,
          s1.aggrs, s1.joins, s1.query, s1.distinct, true,
          Some(IntersectState(s1, s2)),
          s1.numGen)
}


case object QueryInterpreter {

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
                if (h) g - as else (g - as) - f
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
    case Eq(k, e)         => traverseFieldExpr(setJoinAndGroup(k, s), e)
    case Gt(k, e)         => traverseFieldExpr(setJoinAndGroup(k, s), e)
    case Gte(k, e)        => traverseFieldExpr(setJoinAndGroup(k, s), e)
    case Lt(k, e)         => traverseFieldExpr(setJoinAndGroup(k, s), e)
    case Lte(k, e)        => traverseFieldExpr(setJoinAndGroup(k, s), e)
    case Contains(k, e)   => setJoinAndGroup(k, s)
    case StartsWith(k, e) => setJoinAndGroup(k, s)
    case EndsWith(k, e)   => setJoinAndGroup(k, s)
    case Not(pred)        => traversePredicate(s, pred)
    case Or(p1, p2)       => traversePredicate(traversePredicate(s, p1), p2)
    case And(p1, p2)      => traversePredicate(traversePredicate(s, p1), p2)
  }

  def traverseSortedFields(s: State, fields: Seq[String]): State =
    fields.foldLeft (s) { (acc, x) => setJoinAndGroup(x, acc) }

  def interpretQuerySet(s: State, qs: QuerySet): State = qs match {
    case New(m, f) => { // Add source and fields to state
      val s1 = f.foldLeft(s source m) { (acc, x) => acc f x }
      val s2 = s1.constantFields(getConstants(f, s1.fields))
      val (groupF, aggrF) = groupFields(s2, f)
      val s3 = s2 nonAggrF groupF
      traverseDeclaredFields(s3 aggrF aggrF, f)
    }
    case Apply(Distinct(field), qs) => {
      val s1 = interpretQuerySet(s, qs)
      field match {
        case None    => s1 distinct field
        case Some(f) => updateJoins(f, s1) distinct field
      }
    }
    case Apply(Filter(pred), qs) => {
      val s1 = interpretQuerySet(s, qs) pred pred // Add predicate to state
      traversePredicate(s1, pred) // update joins
    }
    case Apply(Sort(spec), qs) => {
      val s1 = traverseSortedFields(interpretQuerySet(s, qs), spec map { _._1 })
      // Finally, always sort by id to eliminate non-deterministic results.
      // Some backends (e.g., MySQL, Postgres) fetch results in an unpredictive
      // manner when the ordering is unspecified.
      val idField = s1.source + ".id"
      val spec2 =
        if (s1.combined) spec
        else
          if (spec exists { x =>
            x._1.equals(idField) || x._1.equals("_default") }) spec
          else spec :+ (idField, Desc)
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
      UnionState.combine(interpretQuerySet(s, qs1), interpretQuerySet(s, qs2))
    case Intersect (qs1, qs2) =>
      IntersectState.combine(interpretQuerySet(s, qs1), interpretQuerySet(s, qs2))
  }

  def interpretAggrQuery(s: State, q: Query): State = q match {
    case AggrRes(aggrs, qs) => interpretQuerySet(s, qs) aggr aggrs
    case _ => ??? // Unreachable case
  }

  def apply(q: Query): State = q match {
    case FirstRes(qs) => {
      val s = interpretQuerySet(State(), qs)
      if (s.orders.isEmpty)
        throw new InvalidQuery(
          "You have to make queryset ordered in order to perform safe comparisons")
      else s
    }
    case SubsetRes(_, _, qs) => {
      val s = interpretQuerySet(State(), qs)
      if (s.orders.isEmpty)
        throw new InvalidQuery(
          "You have to make queryset ordered in order to perform safe comparisons")
      else s
    }
    case SetRes(qs) => interpretQuerySet(State(), qs)
    case AggrRes(f, _) => traverseDeclaredFields(
      interpretAggrQuery(State(), q), f)
  }
}


abstract class Translator {
  val preamble: String

  def apply(q: Query, s: State): String = {
    val qStr = q match {
      case FirstRes(_)   => constructQuery(s, first = true)
      case SetRes(_)     => constructQuery(s)
      case AggrRes(f, _) => constructQuery(s)
      case SubsetRes(offset, limit, _) =>
        constructQuery(s, offset = offset, limit = limit)
    }
    val dfields = s.fields.values.filter { !FieldDecl.hidden(_) }.toSeq match {
      case Seq() => Seq("_default") // Here is the default field
      case f     => (f map FieldDecl.as).toSeq
    }
    val str = preamble + "\n" + qStr.toString
    qStr.ret match {
      case None => str
      case Some(ret) => str + "\n" + emitPrint(q, dfields, ret)
    }
  }

  def constructCombinedQueries(s: State): QueryStr = s.from match {
    case None => ??? // Unreachable case
    case Some(UnionState(s1, s2)) => constructCombinedQuery(unionQueries(s1, s2))
    case Some(IntersectState(s1, s2)) =>
      constructCombinedQuery(intersectQueries(s1, s2))
  }

  def constructQuery(s: State, first: Boolean = false, offset: Int = 0,
      limit: Option[Int] = None): QueryStr =
    if (s.combined) constructCombinedQueries(s)
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

  def apply(q: Query, s: State, target: Target): String = target.orm match {
    case Django(_, _, _)      => DjangoTranslator(target)(q, s)
    case SQLAlchemy (_, _)    => SQLAlchemyTranslator(target)(q, s)
    case Sequelize(_, _)      => SequelizeTranslator(target)(q, s)
    case Peewee(_, _)         => PeeweeTranslator(target)(q, s)
    case ActiveRecord(_, _)   => ActiveRecordTranslator(target)(q, s)
    case Pony(_, _)           => PonyTranslator(target)(q, s)
  }
}
