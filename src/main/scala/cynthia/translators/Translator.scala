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


import cynthia.targets.Target
import cynthia.lang._
import cynthia.targets.{Django, Peewee, SQLAlchemy, ActiveRecord, Sequelize, Pony}
import cynthia.utils.Utils


final case class UnsupportedException(private val message: String)
extends Exception(message)


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
      case Seq() => Seq("id") // Here is the default field
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
    case Some(UnionState(s1, s2)) =>
      constructCombinedQuery(unionQueries(s1, s2).copy(
        aggrs = s.aggrs, orders = s.orders))
    case Some(IntersectState(s1, s2)) =>
      constructCombinedQuery(intersectQueries(s1, s2).copy(
        aggrs = s.aggrs, orders = s.orders))
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
    fields filter { !FieldDecl.hidden(_) } map f

  def mapHiddenFields[T](fields: Iterable[FieldDecl], f: FieldDecl => T): Iterable[T] =
    fields filter FieldDecl.hidden map f

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
