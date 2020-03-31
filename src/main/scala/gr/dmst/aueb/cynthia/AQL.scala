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

sealed abstract class Aggregate (val compound: Boolean) {  }
case class Count(field: Option[String]) extends Aggregate(false)
case class Sum(field: String) extends Aggregate(false)
case class Avg(field: String) extends Aggregate(false)
case class Max(field: String) extends Aggregate(false)
case class Min(field: String) extends Aggregate(false)
case class Add(ag1: Aggregate, ag2: Aggregate) extends Aggregate(true)
case class Sub(ag1: Aggregate, ag2: Aggregate) extends Aggregate(true)
case class Mul(ag1: Aggregate, ag2: Aggregate) extends Aggregate(true)
case class Div(ag1: Aggregate, ag2: Aggregate) extends Aggregate(true)

sealed abstract class QueryField(val label: String) {  }
case class NativeField(name: String, as: String) extends QueryField(as)
case class CompoundField(aggr: Aggregate, as: String) extends QueryField(as)

sealed trait QuerySet
case class New(model: String, fields: Option[Array[String]]) extends QuerySet
case class Apply(op: Operation, q: QuerySet) extends QuerySet
case class Union(q1: QuerySet, q2: QuerySet) extends QuerySet
case class Intersect(q1: QuerySet, q2: QuerySet) extends QuerySet

sealed trait Query
case class FirstRes(qs: QuerySet) extends Query
case class SubsetRes(offset: Int = 0, limit: Option[Int], qs: QuerySet) extends Query
case class SetRes (qs: QuerySet) extends Query
case class AggrRes (aggrs: Seq[CompoundField], qs: QuerySet) extends Query
