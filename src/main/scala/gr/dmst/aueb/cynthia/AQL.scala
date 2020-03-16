package gr.dmst.aueb.cynthia


sealed trait Predicate
case class Eq(key: String, value: String) extends Predicate

sealed trait Operation
case class Filter (pred: Predicate) extends Operation

sealed trait QuerySet
case class New(model: String, fields: Option[Array[String]]) extends QuerySet
case class Apply(op: Operation, q: QuerySet) extends QuerySet
case class Union(q1: QuerySet, q2: QuerySet) extends QuerySet
case class Intersect(q1: QuerySet, q2: QuerySet) extends QuerySet

sealed trait Aggr
case object Count extends Aggr
case object Sum extends Aggr

sealed trait Query
case class SetRes (qs: QuerySet) extends Query
case class AggrRes (aggr: Aggr, qs: QuerySet) extends Query
