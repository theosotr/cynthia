package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._


final case class UnsupportedException(private val message: String)
extends Exception(message)


case class QueryStr(
  ret: String,
  q: Option[String],
  builtQ: Seq[String] = Seq()) {

  def >>(qstr: QueryStr) =
    QueryStr(qstr.ret, qstr.q, toBuiltQ())

  def <<(qstr: QueryStr) =
    QueryStr(ret, None, toBuiltQ() ++ qstr.toBuiltQ())

  def toBuiltQ() = q match {
    case None    => builtQ
    case Some(q) => builtQ :+ (ret + " = " + q)
  }

  override def toString() =
    toBuiltQ mkString ("\n")
} 

case class State(
  db: DB,
  sources: Set[String] = Set(),
  preds: Set[Predicate] = Set(),
  orders: Seq[(String, Order)] = Seq(),
  aggrs: Seq[Aggregate] = Seq(),
  query: Option[QueryStr] = None,
  numGen: Iterator[Int] = Stream.from(1).iterator
  ) {

  def +(source: String) =
    State(db, sources + source, preds, orders, aggrs, query, numGen)

  def ++(pred: Predicate): State =
    State(db, sources, preds + pred, orders, aggrs, query, numGen)

  def :+(o: (String, Order)): State =
    State(db, sources, preds, orders :+ o, aggrs, query, numGen)

  def :-(a: Seq[Aggregate]): State =
    State(db, sources, preds, orders, aggrs ++ a, query, numGen)

  def >>(qstr: QueryStr): State = query match {
    case None        =>
      State(db, sources, preds, orders, aggrs, Some(qstr), numGen)
    case Some(query) =>
      State(db, sources, preds, orders, aggrs, Some(query >> qstr), numGen)
  }
}


abstract class Translator(val target: Target) {
  val preamble: String

  def evalQuerySet(s: State)(qs: QuerySet): State = qs match {
    case New(m, f)               => s + m // Add source to state
    case Apply(Filter(pred), qs) => evalQuerySet(s)(qs) ++ pred // Add predicate to state
    case Apply(Sort(spec), qs)   =>
      spec.foldLeft(evalQuerySet(s)(qs)) { (s, x) => s :+ x } // Add order spec to state
    case Union (qs1, qs2) =>
      unionQueries(evalQuerySet(s)(qs1), evalQuerySet(s)(qs2)) // Merge queries
    case Intersect (qs1, qs2) =>
      intersectQueries(evalQuerySet(s)(qs1), evalQuerySet(s)(qs2)) // Intersect queries
  }

  def evalAggrQuery(s: State)(q: Query): State = q match {
    case AggrRes(aggrs, qs) => evalQuerySet(s)(qs) :- aggrs
    case _ => ???
  }

  def apply(q: Query): String = {
    q match {
      case SetRes (qs) => {
        val f = evalQuerySet(State(target.db)) _ andThen constructQuery _
        val qStr = f(qs)
        preamble + "\n" + qStr.toString + "\n" + emitPrint(q, qStr.ret)
      }
      case _ => {
        val f = evalAggrQuery(State(target.db)) _ andThen constructQuery _
        val qStr = f(q)
        preamble + "\n" + qStr.toString + "\n" + emitPrint(q, qStr.ret)
      }
    }
  }

  def emitPrint(q: Query, ret: String): String
  def constructQuery(state: State): QueryStr
  def unionQueries(s1: State, s2: State): State
  def intersectQueries(s1: State, s2: State): State
}


object ORMTranslator {

  def apply(q: Query, target: Target): String = target.orm match {
    case Django(name, _, setDir)   => DjangoTranslator(target)(q)
    case SQLAlchemy (_, _)         => SQLAlchemyTranslator(target)(q)
    case Sequelize(_, _)           => SequelizeTranslator(target)(q)
  }
}
