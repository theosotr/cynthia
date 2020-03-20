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

sealed trait QuerySet
case class New(model: String, fields: Option[Array[String]]) extends QuerySet
case class Apply(op: Operation, q: QuerySet) extends QuerySet
case class Union(q1: QuerySet, q2: QuerySet) extends QuerySet
case class Intersect(q1: QuerySet, q2: QuerySet) extends QuerySet

sealed trait Aggregate
case class Count(label: Option[String]) extends Aggregate
case class Sum(field: String, label: Option[String] = None) extends Aggregate
case class Avg(field: String, label: Option[String] = None) extends Aggregate
case class Max(field: String, label: Option[String] = None) extends Aggregate
case class Min(field: String, label: Option[String] = None) extends Aggregate
case class Add(ag1: Aggregate, ag2: Aggregate, label: Option[String] = None) extends Aggregate
case class Sub(ag1: Aggregate, ag2: Aggregate, label: Option[String] = None) extends Aggregate
case class Mul(ag1: Aggregate, ag2: Aggregate, label: Option[String] = None) extends Aggregate
case class Div(ag1: Aggregate, ag2: Aggregate, label: Option[String] = None) extends Aggregate

sealed trait Query
case class SetRes (qs: QuerySet) extends Query
case class AggrRes (aggrs: Seq[Aggregate], qs: QuerySet) extends Query
