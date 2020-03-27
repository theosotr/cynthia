package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._


final case class UnsupportedException(private val message: String)
extends Exception(message)


case class QueryStr(
  ret: Option[String] = None,
  q: Option[String] = None,
  builtQ: Seq[String] = Seq()) {

  def >>(qstr: QueryStr) =
    QueryStr(qstr.ret, qstr.q, toBuiltQ())

  def <<(qstr: QueryStr) =
    QueryStr(ret, None, toBuiltQ() ++ qstr.toBuiltQ())

  def toBuiltQ() = (ret, q) match {
    case (None, None) | (Some(_), None) => builtQ
    case (Some(r), Some(q)) => builtQ :+ (r + " = " + q)
    case (None, Some(q)) => builtQ :+ q
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
  joins: Set[(String, String)] = Set(),
  query: Option[QueryStr] = None,
  numGen: Iterator[Int] = Stream.from(1).iterator
  ) {

  def +(source: String) =
    State(db, sources + source, preds, orders, aggrs, joins, query, numGen)

  def ++(pred: Predicate): State =
    State(db, sources, preds + pred, orders, aggrs, joins, query, numGen)

  def :+(o: (String, Order)): State =
    State(db, sources, preds, orders :+ o, aggrs, joins, query, numGen)

  def :-(a: Seq[Aggregate]): State =
    State(db, sources, preds, orders, aggrs ++ a, joins, query, numGen)

  def :>(j: (String, String)): State =
    State(db, sources, preds, orders, aggrs, joins + j, query, numGen)

  def >>(qstr: QueryStr): State = query match {
    case None        =>
      State(db, sources, preds, orders, aggrs, joins, Some(qstr), numGen)
    case Some(query) =>
      State(db, sources, preds, orders, aggrs, joins, Some(query >> qstr),
            numGen)
  }
}


abstract class Translator(val target: Target) {
  val preamble: String

  def updateJoins(field: String, s: State) = {
    def _updateJoins(segs: List[String], s: State): State = segs match {
      case Nil | _ :: Nil | _ :: (_ :: Nil) => s
      case h1 :: (h2 :: t) => {
        val cap = h2.capitalize
        _updateJoins(cap :: t, s :> (h1, cap))
      }
    }
    _updateJoins(field.split('.').toList, s)
  }

  def traversePredicate(s: State, pred: Predicate): State = pred match {
    case Eq(k, _)       => updateJoins(k, s)
    case Gt(k, _)       => updateJoins(k, s)
    case Gte(k, _)      => updateJoins(k, s)
    case Lt(k, _)       => updateJoins(k, s)
    case Lte(k, _)      => updateJoins(k, s)
    case Contains(k, _) => updateJoins(k, s)
    case Not(pred)      => traversePredicate(s, pred)
    case Or(p1, p2)     => traversePredicate(traversePredicate(s, p1), p2)
    case And(p1, p2)    => traversePredicate(traversePredicate(s, p1), p2)
  }

  def evalQuerySet(s: State)(qs: QuerySet): State = qs match {
    case New(m, f)               => s + m // Add source to state
    case Apply(Filter(pred), qs) => {
      val s1 = evalQuerySet(s)(qs) ++ pred // Add predicate to state
      traversePredicate(s1, pred) // update joins
    }
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
        preamble + "\n" + qStr.toString + "\n" + emitPrint(q, qStr.ret.get)
      }
      case _ => {
        val f = evalAggrQuery(State(target.db)) _ andThen constructQuery _
        val qStr = f(q)
        preamble + "\n" + qStr.toString + "\n" + emitPrint(q, qStr.ret.get)
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
