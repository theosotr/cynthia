package gr.dmst.aueb.cynthia


sealed trait Predicate
case class Eq(key: String, value: String) extends Predicate

sealed trait Operation
case class Filter (pred: Predicate) extends Operation

sealed trait Query
case class Getall(model: String, fields: Option[Array[String]]) extends Query
case class Apply(op: Operation, q: Query) extends Query
case class Union(q1: Query, q2: Query) extends Query
case class Intersect(q1: Query, q2: Query) extends Query
