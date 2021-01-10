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

import cynthia.targets.{DB, SQLite, MySQL, Postgres, MSSQL, Cockroachdb}


sealed trait ConstantType
case object Quoted extends ConstantType
case object UnQuoted extends ConstantType

sealed abstract class FieldExpr(val compound: Boolean) {
  def isAggregate(): Boolean
  def isNaiveAggregate(): Boolean
  def isConstant(): Boolean
}
case class Constant(v: String, vt: ConstantType) extends FieldExpr(false) {
  def isAggregate() = false
  def isNaiveAggregate() = false
  def isConstant() = true
}
case class F(f: String) extends FieldExpr(false) {
  def isAggregate() = false
  def isNaiveAggregate() = false
  def isConstant() = false
}
case class Count(f: Option[FieldExpr]) extends FieldExpr(false) {
  def isAggregate() = true
  def isNaiveAggregate() = true
  def isConstant() = false
}
case class Sum(f: FieldExpr) extends FieldExpr(false) {
  def isAggregate() = true
  def isNaiveAggregate() = true
  def isConstant() = false
}
case class Avg(f: FieldExpr) extends FieldExpr(false) {
  def isAggregate() = true
  def isNaiveAggregate() = true
  def isConstant() = false
}
case class Max(f: FieldExpr) extends FieldExpr(false) {
  def isAggregate() = true
  def isNaiveAggregate() = true
  def isConstant() = false
}
case class Min(f: FieldExpr) extends FieldExpr(false) {
  def isAggregate() = true
  def isNaiveAggregate() = true
  def isConstant() = false
}
case class Add(f1: FieldExpr, f2: FieldExpr) extends FieldExpr(true) {
  def isAggregate() =
    f1.isAggregate() || f2.isAggregate()
  def isNaiveAggregate() = false
  def isConstant() =
    f1.isConstant() && f2.isConstant()
}
case class Sub(f1: FieldExpr, f2: FieldExpr) extends FieldExpr(true) {
  def isAggregate() =
    f1.isAggregate() || f2.isAggregate()
  def isNaiveAggregate() = false
  def isConstant() =
    f1.isConstant() && f2.isConstant()
}
case class Mul(f1: FieldExpr, f2: FieldExpr) extends FieldExpr(true) {
  def isAggregate() =
    f1.isAggregate() || f2.isAggregate()
  def isNaiveAggregate() = false
  def isConstant() =
    f1.isConstant() && f2.isConstant()
}
case class Div(f1: FieldExpr, f2: FieldExpr) extends FieldExpr(true) {
  def isAggregate() =
    f1.isAggregate() || f2.isAggregate()
  def isNaiveAggregate() = false
  def isConstant() =
    f1.isConstant() && f2.isConstant()
}

sealed abstract class Predicate {
  def isAggregateField(field: String, fields: Map[String, FieldDecl]) =
    fields.get(field) match {
      case None                        => false
      case Some(FieldDecl(f, _, _, _)) => f.isAggregate()
    }

  def hasAggregate(fields: Map[String, FieldDecl]): Boolean
}
case class Eq(key: String, value: FieldExpr) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    value.isAggregate() || isAggregateField(key, fields)
}
case class Gt(key: String, value: FieldExpr) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    value.isAggregate() || isAggregateField(key, fields)
}
case class Lt(key: String, value: FieldExpr) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    value.isAggregate() || isAggregateField(key, fields)
}
case class Gte(key: String, value: FieldExpr) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    value.isAggregate() || isAggregateField(key, fields)
}
case class Lte(key: String, value: FieldExpr) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    value.isAggregate() || isAggregateField(key, fields)
}
case class Contains(key: String, value: String) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    isAggregateField(key, fields)
}
case class StartsWith(key: String, value: String) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    false
}
case class EndsWith(key: String, value: String) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    false
}
case class And(p1: Predicate, p2: Predicate) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    p1.hasAggregate(fields) || p2.hasAggregate(fields)
}
case class Or(p1: Predicate, p2: Predicate) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    p1.hasAggregate(fields) || p2.hasAggregate(fields)
}
case class Not(p: Predicate) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    p.hasAggregate(fields)
}

sealed trait Order
case object Asc extends Order
case object Desc extends Order

sealed trait Operation
case class Filter (pred: Predicate) extends Operation
case class Sort(spec: Seq[(String, Order)]) extends Operation
case class Distinct(field: Option[String]) extends Operation

sealed trait FieldType {
  def convertType(db: DB): String
  def isNumeric(): Boolean
  def isStr(): Boolean
}
case object FieldType {
  def dataTypeToFieldType(t: DataType) = t match {
    case Int8 | Int16 | Int32 | Int64 | Serial | Foreign(_) => IntF
    case VarChar(_) => StringF
    case Numeric => DoubleF
    case Bool => BooleanF
  }
}
case object StringF extends FieldType {
  override def convertType(db: DB) = db match {
    case MySQL(_, _, _) => "char"
    case _              => "varchar(100)"
  }

  override def isNumeric() = false
  override def isStr() = true
}
case object IntF extends FieldType {
  override def convertType(db: DB) = db match {
    case Postgres(_, _, _) | Cockroachdb(_, _, _) | MSSQL(_, _, _)=> "integer"
    case _ => "signed"
  }

  override def isNumeric() = true
  override def isStr() = false
}
case object DoubleF extends FieldType {
  override def convertType(db: DB) = db match {
    case SQLite(_) => "float"
    case _         => "decimal(10, 2)"
  }

  override def isNumeric() = true
  override def isStr() = false
}
case object BooleanF extends FieldType {
  override def convertType(db: DB) = "boolean"

  override def isNumeric() = true
  override def isStr() = false
}
case object DateTimeF extends FieldType {
  override def convertType(db: DB) = "datetime"

  override def isNumeric() = true
  override def isStr() = false
}

case class FieldDecl(f: FieldExpr, as: String, ftype: FieldType, hidden: Boolean = false)
object FieldDecl {

  def expr(fieldDecl: FieldDecl) = fieldDecl match {
    case FieldDecl(e, _, _, _) => e
  }

  def as(fieldDecl: FieldDecl) = fieldDecl match {
    case FieldDecl(_, as, _, _) => as
  }

  def ftype(fieldDecl: FieldDecl) = fieldDecl match {
    case FieldDecl(_, _, t, _) => t
  }

  def hidden(fieldDecl: FieldDecl) = fieldDecl match {
    case FieldDecl(_, _, _, h) => h
  }

  def isAggregate(fieldDecl: FieldDecl) = fieldDecl match {
    case FieldDecl(f, _, _, _) => f.isAggregate()
  }
}

sealed trait QuerySet {
  def ordered(): Boolean
  def combined(): Boolean
  def filtered(): Boolean
}
case class New(model: String, fields: Seq[FieldDecl]) extends QuerySet {
  def ordered() = false
  def combined() = false
  def filtered() = false
}
case class Apply(op: Operation, q: QuerySet) extends QuerySet {
  def ordered() = op match {
    case Sort(_) => true
    case _       => q.ordered()
  }

  def filtered() = op match {
    case Filter(_) => true
    case _         => q.filtered()
  }

  def combined() =
    q.combined()
}
case class Union(q1: QuerySet, q2: QuerySet) extends QuerySet {
  def ordered() = q1.ordered() && q2.ordered()
  def filtered() = q1.filtered() || q2.filtered()
  def combined() = true
}
case class Intersect(q1: QuerySet, q2: QuerySet) extends QuerySet {
  def ordered() = q1.ordered() && q2.ordered()
  def filtered() = q1.filtered() || q2.filtered()
  def combined() = true
}

sealed abstract class Query(val queryset: QuerySet)
case class FirstRes(qs: QuerySet) extends Query(qs)
case class SubsetRes(offset: Int = 0, limit: Option[Int], qs: QuerySet) extends Query(qs)
case class SetRes(qs: QuerySet) extends Query(qs)
case class AggrRes(aggrs: Seq[FieldDecl], qs: QuerySet) extends Query(qs)
