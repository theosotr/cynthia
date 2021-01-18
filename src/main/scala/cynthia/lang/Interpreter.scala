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

final case class InvalidQuery(private val message: String)
    extends Exception(message)

case object QueryInterpreter {

  def updateJoins(field: String, s: State) = {
    field split '.' match {
      case Array(_) | Array(_, _) => s
      case arr => {
        val path = (arr dropRight 1).toSeq map { _.capitalize }
        val tail = path drop 1
        (List.range(0, path.size - 1) map { x => path(0) +: tail dropRight x })
          .foldLeft(s) { (acc, x) => acc join x }
      }
    }
  }

  def getConstants(fields: Seq[FieldDecl], store: Map[String, FieldDecl]) = {
    def _getConstants(acc: Set[String], e: FieldExpr, as: String): Set[String] =
      e match {
        case Constant(_, _) => acc + as
        case F(f) =>
          store get f match {
            case None                        => acc
            case Some(FieldDecl(e, _, _, _)) => _getConstants(acc, e, as)
          }
        case _ => acc
      }
    (fields filter { !FieldDecl.hidden(_) }).foldLeft(Set[String]()) {
      (acc, x) =>
        x match { case FieldDecl(e, as, _, _) => _getConstants(acc, e, as) }
    }
  }

  def getAggregate(fields: Seq[FieldDecl], store: Map[String, FieldExpr]) = {
    def _getAggregate(acc: Set[String], e: FieldExpr, as: String): Set[String] =
      e match {
        case Constant(_, _)                               => acc
        case Count(_) | Sum(_) | Avg(_) | Max(_) | Min(_) => acc + as
        case F(f) =>
          store get f match {
            case None    => acc
            case Some(e) => _getAggregate(acc, e, as)
          }
        case Add(e1, e2) =>
          _getAggregate(acc, e1, as) ++ _getAggregate(acc, e2, as)
        case Sub(e1, e2) =>
          _getAggregate(acc, e1, as) ++ _getAggregate(acc, e2, as)
        case Mul(e1, e2) =>
          _getAggregate(acc, e1, as) ++ _getAggregate(acc, e2, as)
        case Div(e1, e2) =>
          _getAggregate(acc, e1, as) ++ _getAggregate(acc, e2, as)
      }
    (fields filter { !FieldDecl.hidden(_) }).foldLeft(Set[String]()) {
      (acc, x) =>
        x match { case FieldDecl(e, as, _, _) => _getAggregate(acc, e, as) }
    }
  }

  def groupFields(
      s: State,
      fields: Seq[FieldDecl]
  ): (Set[String], Set[String]) = {
    val init = (
      Set[String](),
      Map[String, (FieldExpr, Boolean)]()
    )
    //  Compute a map of field names to their corresponding expression
    //  and an the initial sets of grouping fields.
    val (groupBy, store) = fields.foldLeft(init) { (acc, x) =>
      {
        val (g, s) = acc
        x match {
          case FieldDecl(e, as, _, false) =>
            (if (!e.isAggregate()) g + as else g, s + (as -> (e, false)))
          case FieldDecl(e, as, _, true) => (g, s + (as -> (e, true)))
        }
      }
    }
    def _handleComplexExpr(
        e: FieldExpr,
        as: String,
        g: Set[String]
    ): Set[String] = {
      val (e1, e2) = e match {
        case Add(e1, e2) => (e1, e2)
        case Sub(e1, e2) => (e1, e2)
        case Mul(e1, e2) => (e1, e2)
        case Div(e1, e2) => (e1, e2)
        case _           => ???
      }
      if (e1.isNaiveAggregate()) _computeGroupBy(e2, as, g)
      else if (e2.isNaiveAggregate()) _computeGroupBy(e1, as, g)
      else _computeGroupBy(e2, as, _computeGroupBy(e1, as, g))
    }
    def _computeGroupBy(e: FieldExpr, as: String, g: Set[String]): Set[String] =
      e match {
        case F(f) =>
          store get f match {
            case None => g + f // the field is native
            case Some((e, h)) => {
              val g2 =
                if (e.isAggregate())
                  if (h) g - as else (g - as) - f
                else if (h) g
                else g + f
              _computeGroupBy(e, as, g2)
            }
          }
        case Constant(_, _) | Count(_) | Sum(_) | Avg(_) | Max(_) | Min(_) => g
        case _                                                             => _handleComplexExpr(e, as, g)
      }
    // Compute all fields that must be included in the GROUP BY clause.
    // Examine recursively the fields that are not hidden.
    val groupedF = (fields filter { !FieldDecl.hidden(_) }).foldLeft(groupBy) {
      (s, f) =>
        { f match { case FieldDecl(e, as, _, h) => _computeGroupBy(e, as, s) } }
    }
    // Check if fields contain aggregate functions.
    val aggrF = getAggregate(fields, store map { case (k, v) => (k, v._1) })
    if (aggrF.isEmpty) (Set(), aggrF)
    // If the list of grouped fields is empty, group by the id of table.
    else if (groupedF.isEmpty) (Set(s.source + ".id"), aggrF)
    else (groupedF, aggrF)
  }

  def setJoinAndGroup(f: String, s: State) =
    s.fields get f match {
      case None =>
        if (!s.aggrF.isEmpty) updateJoins(f, s) addGroupF f
        else updateJoins(f, s)
      case _ => s
    }

  def traverseFieldExpr(
      s: State,
      e: FieldExpr,
      updateGroup: Boolean = true
  ): State = e match {
    case Constant(_, _) => s
    case F(f) =>
      s.fields get f match {
        case None =>
          if (updateGroup) setJoinAndGroup(f, s)
          else updateJoins(f, s)
        case Some(FieldDecl(e2, _, _, _)) => traverseFieldExpr(s, e2, false)
      }
    case Count(None)     => s
    case Count(Some(e2)) => traverseFieldExpr(s, e2, false)
    case Sum(e2)         => traverseFieldExpr(s, e2, false)
    case Avg(e2)         => traverseFieldExpr(s, e2, false)
    case Max(e2)         => traverseFieldExpr(s, e2, false)
    case Min(e2)         => traverseFieldExpr(s, e2, false)
    case Add(e1, e2) =>
      traverseFieldExpr(traverseFieldExpr(s, e1, updateGroup), e2, updateGroup)
    case Sub(e1, e2) =>
      traverseFieldExpr(traverseFieldExpr(s, e1, updateGroup), e2, updateGroup)
    case Mul(e1, e2) =>
      traverseFieldExpr(traverseFieldExpr(s, e1, updateGroup), e2, updateGroup)
    case Div(e1, e2) =>
      traverseFieldExpr(traverseFieldExpr(s, e1, updateGroup), e2, updateGroup)
  }

  def traverseDeclaredFields(s: State, fields: Seq[FieldDecl]): State =
    (fields filter { !FieldDecl.hidden(_) } map FieldDecl.expr).foldLeft(s) {
      (acc, e) => traverseFieldExpr(acc, e)
    }

  def traversePredicate(s: State, pred: Predicate): State = pred match {
    case Eq(k, e)         => traverseFieldExpr(setJoinAndGroup(k, s), e)
    case Gt(k, e)         => traverseFieldExpr(setJoinAndGroup(k, s), e)
    case Gte(k, e)        => traverseFieldExpr(setJoinAndGroup(k, s), e)
    case Lt(k, e)         => traverseFieldExpr(setJoinAndGroup(k, s), e)
    case Lte(k, e)        => traverseFieldExpr(setJoinAndGroup(k, s), e)
    case Contains(k, e)   => setJoinAndGroup(k, s)
    case StartsWith(k, e) => setJoinAndGroup(k, s)
    case EndsWith(k, e)   => setJoinAndGroup(k, s)
    case Not(pred)        => traversePredicate(s, pred)
    case Or(p1, p2)       => traversePredicate(traversePredicate(s, p1), p2)
    case And(p1, p2)      => traversePredicate(traversePredicate(s, p1), p2)
  }

  def traverseSortedFields(s: State, fields: Seq[String]): State =
    fields.foldLeft(s) { (acc, x) => setJoinAndGroup(x, acc) }

  def interpretQuerySet(s: State, qs: QuerySet): State = qs match {
    case New(m, f) => { // Add source and fields to state
      val s1 = f.foldLeft(s source m) { (acc, x) => acc f x }
      val s2 = s1.constantFields(getConstants(f, s1.fields))
      val (groupF, aggrF) = groupFields(s2, f)
      val s3 = s2 nonAggrF groupF
      traverseDeclaredFields(s3 aggrF aggrF, f)
    }
    case Apply(Distinct(field), qs) => {
      val s1 = interpretQuerySet(s, qs)
      field match {
        case None    => s1 distinct field
        case Some(f) => updateJoins(f, s1) distinct field
      }
    }
    case Apply(Filter(pred), qs) => {
      val s1 = interpretQuerySet(s, qs) pred pred // Add predicate to state
      traversePredicate(s1, pred) // update joins
    }
    case Apply(Sort(spec), qs) => {
      val s1 = traverseSortedFields(interpretQuerySet(s, qs), spec map { _._1 })
      // Finally, always sort by id to eliminate non-deterministic results.
      // Some backends (e.g., MySQL, Postgres) fetch results in an unpredictive
      // manner when the ordering is unspecified.
      val idField = s1.source + ".id"
      val spec2 =
        if (s1.combined) spec
        else if (
          spec exists { x =>
            x._1.equals(idField) || x._1.equals("_default")
          }
        ) spec
        else spec :+ (idField, Desc)
      val s2 =
        if (!s1.nonAggrF.isEmpty) s1 addGroupF (s1.source + ".id") else s1
      spec2.foldLeft(s2) { (s, x) =>
        {
          // If this field is a constant, we do not add to the set of the sorted
          // fields.
          val s3 = if (s.constantF.contains(x._1)) s else s order x
          val (f, _) = x
          s3.fields get f match {
            // the field is native so add it to grouping fields
            case None => if (!s3.aggrF.isEmpty) s3 addGroupF f else s3
            // we have already examined this field because is declared.
            case _ => s3
          }
        }
      }
    }
    case Union(qs1, qs2) =>
      UnionState.combine(interpretQuerySet(s, qs1), interpretQuerySet(s, qs2))
    case Intersect(qs1, qs2) =>
      IntersectState.combine(
        interpretQuerySet(s, qs1),
        interpretQuerySet(s, qs2)
      )
  }

  def interpretAggrQuery(s: State, q: Query): State = q match {
    case AggrRes(aggrs, qs) => interpretQuerySet(s, qs) aggr aggrs
    case _                  => ??? // Unreachable case
  }

  def apply(q: Query): State = q match {
    case FirstRes(qs) => {
      val s = interpretQuerySet(State(), qs)
      if (s.orders.isEmpty)
        throw new InvalidQuery(
          "You have to make queryset ordered in order to perform safe comparisons"
        )
      else s
    }
    case SubsetRes(_, _, qs) => {
      val s = interpretQuerySet(State(), qs)
      if (s.orders.isEmpty)
        throw new InvalidQuery(
          "You have to make queryset ordered in order to perform safe comparisons"
        )
      else s
    }
    case SetRes(qs) => interpretQuerySet(State(), qs)
    case AggrRes(f, _) =>
      traverseDeclaredFields(interpretAggrQuery(State(), q), f)
  }
}
