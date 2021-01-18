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

package cynthia.lang

import scala.collection.immutable.ListMap

case class QueryStr(
    ret: Option[String] = None,
    q: Option[String] = None,
    builtQ: Seq[String] = Seq()
) {

  def >>(qstr: QueryStr) =
    QueryStr(qstr.ret, None, toBuiltQ() ++ qstr.toBuiltQ())

  def <<(qstr: QueryStr) =
    QueryStr(ret, None, toBuiltQ() ++ qstr.toBuiltQ())

  def toBuiltQ() = (ret, q) match {
    case (None, None) | (Some(_), None) => builtQ
    case (Some(r), Some(q))             => builtQ :+ (r + " = " + q)
    case (None, Some(q))                => builtQ :+ q
  }

  override def toString() =
    toBuiltQ() mkString ("\n")
}

case class State(
    source: String = "", // model
    fields: Map[String, FieldDecl] = ListMap(), // FieldDecl.as FieldDecl
    preds: Set[Predicate] = Set(),
    orders: Seq[(String, Order)] = Seq(), // FieldDecl.as asc or desc
    nonAggrF: Set[String] = Set(), // Fields for group by (Translator)
    aggrF: Set[String] = Set(), // FieldDecl connected with aggregate functions
    constantF: Set[String] = Set(),
    aggrs: Seq[FieldDecl] = Seq(), // Aggregate functions to apply
    joins: Seq[Seq[String]] = Seq(), // Models to join with source
    query: Option[QueryStr] = None, // Query string (target), e.g. Unions
    distinct: Option[String] = None,
    combined: Boolean = false,
    from: Option[CombinedState] = None,
    numGen: Iterator[Int] = LazyList.from(1).iterator
) {

  def source(s: String) =
    State(
      s,
      fields,
      preds,
      orders,
      nonAggrF,
      aggrF,
      constantF,
      aggrs,
      joins,
      query,
      distinct,
      combined,
      from,
      numGen
    )

  def f(fd: FieldDecl) = fd match {
    case FieldDecl(_, as, _, _) =>
      State(
        source,
        fields + (as -> fd),
        preds,
        orders,
        nonAggrF,
        aggrF,
        constantF,
        aggrs,
        joins,
        query,
        distinct,
        combined,
        from,
        numGen
      )
  }

  def pred(p: Predicate): State =
    State(
      source,
      fields,
      preds + p,
      orders,
      nonAggrF,
      aggrF,
      constantF,
      aggrs,
      joins,
      query,
      distinct,
      combined,
      from,
      numGen
    )

  def order(o: (String, Order)): State =
    State(
      source,
      fields,
      preds,
      orders :+ o,
      nonAggrF,
      aggrF,
      constantF,
      aggrs,
      joins,
      query,
      distinct,
      combined,
      from,
      numGen
    )

  def nonAggrF(f: Set[String]): State =
    State(
      source,
      fields,
      preds,
      orders,
      nonAggrF ++ f,
      aggrF,
      constantF,
      aggrs,
      joins,
      query,
      distinct,
      combined,
      from,
      numGen
    )

  def addGroupF(f: String): State =
    State(
      source,
      fields,
      preds,
      orders,
      nonAggrF + f,
      aggrF,
      constantF,
      aggrs,
      joins,
      query,
      distinct,
      combined,
      from,
      numGen
    )

  def aggrF(f: Set[String]): State =
    State(
      source,
      fields,
      preds,
      orders,
      nonAggrF,
      f,
      constantF,
      aggrs,
      joins,
      query,
      distinct,
      combined,
      from,
      numGen
    )

  def constantFields(c: Set[String]): State =
    State(
      source,
      fields,
      preds,
      orders,
      nonAggrF,
      aggrF,
      c,
      aggrs,
      joins,
      query,
      distinct,
      combined,
      from,
      numGen
    )

  def aggr(a: Seq[FieldDecl]): State =
    State(
      source,
      fields,
      preds,
      orders,
      nonAggrF,
      aggrF,
      constantF,
      aggrs ++ a,
      joins,
      query,
      distinct,
      combined,
      from,
      numGen
    )

  def join(p: Seq[String]): State =
    State(
      source,
      fields,
      preds,
      orders,
      nonAggrF,
      aggrF,
      constantF,
      aggrs,
      joins :+ p,
      query,
      distinct,
      combined,
      from,
      numGen
    )

  def distinct(d: Option[String]): State = {
    val distinct = d match {
      case Some(x) => Some(x)
      case _       => Some("")
    }
    State(
      source,
      fields,
      preds,
      orders,
      nonAggrF,
      aggrF,
      constantF,
      aggrs,
      joins,
      query,
      distinct,
      combined,
      from,
      numGen
    )
  }

  def getJoinPairs(): Set[(String, String)] =
    (joins map { x =>
      {
        val Seq(a, b) = x takeRight 2
        (a, b)
      }
    }).toSet

  def getNonConstantGroupingFields(): Set[String] =
    if (nonAggrF.isEmpty) nonAggrF
    else {
      val groupingF = nonAggrF filter { x => !constantF.contains(x) }
      if (groupingF.isEmpty) Set(source + ".id")
      else groupingF
    }

  def >>(qstr: QueryStr): State = query match {
    case None =>
      State(
        source,
        fields,
        preds,
        orders,
        nonAggrF,
        aggrF,
        constantF,
        aggrs,
        joins,
        Some(qstr),
        distinct,
        combined,
        from,
        numGen
      )
    case Some(query) =>
      State(
        source,
        fields,
        preds,
        orders,
        nonAggrF,
        aggrF,
        constantF,
        aggrs,
        joins,
        Some(query >> qstr),
        distinct,
        combined,
        from,
        numGen
      )
  }
}

sealed abstract class CombinedState(val s1: State, val s2: State)
case class UnionState(rs: State, ls: State) extends CombinedState(rs, ls)
case object UnionState {

  def combine(s1: State, s2: State): State =
    State(
      s1.source,
      s1.fields,
      Set(),
      Seq(),
      Set(),
      Set(),
      Set(),
      Seq(),
      Seq(),
      s1.query,
      None,
      true,
      Some(UnionState(s1, s2)),
      s1.numGen
    )
}
case class IntersectState(rs: State, ls: State) extends CombinedState(rs, ls)
case object IntersectState {

  def combine(s1: State, s2: State): State =
    State(
      s1.source,
      s1.fields,
      Set(),
      Seq(),
      Set(),
      Set(),
      Set(),
      Seq(),
      Seq(),
      s1.query,
      None,
      true,
      Some(IntersectState(s1, s2)),
      s1.numGen
    )
}
