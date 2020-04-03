package gr.dmst.aueb.cynthia

sealed trait ValueType
case object Quoted extends ValueType
case object UnQuoted extends ValueType

case class Value(v: String, vt: ValueType)

sealed trait Predicate
case class Eq(key: String, value: Value) extends Predicate
case class Gt(key: String, value: Value) extends Predicate
case class Lt(key: String, value: Value) extends Predicate
case class Gte(key: String, value: Value) extends Predicate
case class Lte(key: String, value: Value) extends Predicate
case class Contains(key: String, value: String) extends Predicate
case class And(p1: Predicate, p2: Predicate) extends Predicate
case class Or(p1: Predicate, p2: Predicate) extends Predicate
case class Not(p: Predicate) extends Predicate

sealed trait Order
case object Asc extends Order
case object Desc extends Order

sealed trait Operation
case class Filter (pred: Predicate) extends Operation
case class Sort(spec: Seq[(String, Order)]) extends Operation

sealed abstract class FieldExpr (val compound: Boolean) {  }
case class F(f: String) extends FieldExpr(false)
case class Count(f: Option[FieldExpr]) extends FieldExpr(false)
case class Sum(f: FieldExpr) extends FieldExpr(false)
case class Avg(f: FieldExpr) extends FieldExpr(false)
case class Max(f: FieldExpr) extends FieldExpr(false)
case class Min(f: FieldExpr) extends FieldExpr(false)
case class Add(f1: FieldExpr, f2: FieldExpr) extends FieldExpr(true)
case class Sub(f1: FieldExpr, f2: FieldExpr) extends FieldExpr(true)
case class Mul(f1: FieldExpr, f2: FieldExpr) extends FieldExpr(true)
case class Div(f1: FieldExpr, f2: FieldExpr) extends FieldExpr(true)

case class FieldDecl(f: FieldExpr, as: String)

sealed trait QuerySet
case class New(model: String, fields: Set[FieldDecl]) extends QuerySet
case class Apply(op: Operation, q: QuerySet) extends QuerySet
case class Union(q1: QuerySet, q2: QuerySet) extends QuerySet
case class Intersect(q1: QuerySet, q2: QuerySet) extends QuerySet

sealed trait Query
case class FirstRes(qs: QuerySet) extends Query
case class SubsetRes(offset: Int = 0, limit: Option[Int], qs: QuerySet) extends Query
case class SetRes (qs: QuerySet) extends Query
case class AggrRes (aggrs: Seq[FieldDecl], qs: QuerySet) extends Query
