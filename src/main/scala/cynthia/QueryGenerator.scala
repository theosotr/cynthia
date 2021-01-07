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

package cynthia


sealed trait QuerySetNode
case object NewQS extends QuerySetNode
case object ApplySort extends QuerySetNode
case object ApplyFilter extends QuerySetNode
case object ApplyDistinct extends QuerySetNode
case object UnionQS extends QuerySetNode
case object IntersectQS extends QuerySetNode


sealed trait PredNode
case object EqPred extends PredNode
case object GtPred extends PredNode
case object GtePred extends PredNode
case object LtPred extends PredNode
case object LtePred extends PredNode
case object ContainsPred extends PredNode
case object StartsWithPred extends PredNode
case object EndsWithPred extends PredNode
case object AndPred extends PredNode
case object OrPred extends PredNode
case object NotPred extends PredNode

sealed trait ExprNode
case object ConstantExpr extends ExprNode
case object FExpr extends ExprNode
case object CountExpr extends ExprNode
case object SumExpr extends ExprNode
case object MaxExpr extends ExprNode
case object MinExpr extends ExprNode
case object AvgExpr extends ExprNode
case object AddExpr extends ExprNode
case object SubExpr extends ExprNode
case object MulExpr extends ExprNode
case object DivExpr extends ExprNode

sealed trait FType
case object NumberF extends FType
case object StrF extends FType


case object FieldFilterer {
  def filterDeclaredField(fType: Option[FType])(f: FieldDecl) = fType match {
    case None => true
    case Some(NumberF) => f.ftype.isNumeric
    case Some(StrF)    => f.ftype.isStr
  }

  def filterModelField(fType: Option[FType], filterForeign: Boolean = false)(f: Field) = fType match {
    case None => true
    case Some(NumberF) => f.ftype match {
      case Foreign(_) => true
      case _          => f.ftype.isNumeric
    }
    case Some(StrF) => f.ftype match {
      case Foreign(_) => !filterForeign
      case _          => f.ftype.isStr
    }
  }
}


case class GenState(
  schema: Schema,
  model: Option[Model] = None,
  depth: Int = 0,
  cands: Seq[QuerySetNode] = Seq(NewQS, UnionQS, IntersectQS),
  exprCands: Seq[ExprNode] = Seq(),
  qs: Option[QuerySet] = None,
  dfields: Seq[FieldDecl] = Seq(),
  aggrF: Seq[FieldDecl] = Seq()) {

  val aggregateNodes = Seq(
    CountExpr,
    SumExpr,
    AvgExpr,
    MinExpr,
    MaxExpr
  )

  val exprNodes = Seq(
    ConstantExpr,
    FExpr,
    AddExpr,
    SubExpr,
    MulExpr,
    //DivExpr
  )

  def ++() =
    GenState(schema, model, depth + 1, cands, exprCands, qs, dfields, aggrF)

  def queryset(qs: QuerySet) =
    GenState(schema, model, depth, cands, exprCands, Some(qs), dfields, aggrF)

  def model(m: Model) =
    GenState(schema, Some(m), depth, cands, exprCands, qs, dfields, aggrF)

  def candidates(c: Seq[QuerySetNode]) =
    GenState(schema, model, depth, c, exprCands, qs, dfields, aggrF)

  def exprCandidates(c: Seq[ExprNode]) =
    GenState(schema, model, depth, cands, c, qs, dfields, aggrF)

  def strExprCandidates() =
    GenState(schema, model, depth, cands,
             exprCands filter {
               case CountExpr | SumExpr | AvgExpr | AddExpr |
                 SubExpr | MulExpr | DivExpr => false
               case _ => true
             },
             qs, dfields, aggrF)

  def dfield(f: FieldDecl) =
    GenState(schema, model, depth, cands, exprCands, qs, dfields :+ f, aggrF)

  def afield(f: FieldDecl) =
    GenState(schema, model, depth, cands, exprCands, qs, dfields, aggrF :+ f)

  def disgardAggregates() = {
    GenState(schema, model, depth, cands, exprNodes, qs, dfields, aggrF)
  }

  def disgardExprs() = {
    GenState(schema, model, depth, cands, aggregateNodes, qs, dfields, aggrF)
  }

  def getFields(declaredOnly: Boolean, fType: Option[FType]) = {
    val declFields = dfields filter FieldFilterer.filterDeclaredField(fType) filter {
      !FieldDecl.hidden(_) } map FieldDecl.as
    if (declaredOnly) declFields
    else declFields ++
      (model.get.fields filter FieldFilterer.filterModelField(fType, filterForeign = true) map { x =>
        x match {
          case Field(n, Foreign(_)) => model.get.name + "." + n + ".id"
          case Field(n, _)          => model.get.name + "." + n
      }})
  }
}



case class QueryGenerator(
  minDepth: Int,
  maxDepth: Int,
  noCombined: Boolean,
  wellTyped: Boolean
) {

  val predNodes = Seq(
    EqPred,
    GtPred,
    GtePred,
    LtPred,
    LtePred,
    ContainsPred,
    StartsWithPred,
    EndsWithPred,
    AndPred,
    OrPred,
    NotPred
  )

  val exprNodes = Seq(
    ConstantExpr,
    FExpr,
    CountExpr,
    SumExpr,
    MaxExpr,
    MinExpr,
    AvgExpr,
    AddExpr,
    SubExpr,
    MulExpr,
    DivExpr
  )

  def getColumn(schema: Schema, model: Model,
                fType: Option[FType],
                columnName: String = ""): (String, FieldType) = {
    val field = RUtils.chooseFrom(model.fields filter FieldFilterer.filterModelField(fType))
    val fieldName = columnName match {
      case "" => s"${model.name.capitalize}.${field.name}"
      case _  => s"$columnName.${field.name}"
    }
    field match {
      case Field(_, Serial)
      | Field(_, Int8)
      | Field(_, Int16)
      | Field(_, Int32)
      | Field(_, Int64)         => (fieldName, IntF)
      case Field(_, Bool)       => (fieldName, BooleanF)
      case Field(_, Numeric)    => (fieldName, DoubleF)
      case Field(_, VarChar(_)) => (fieldName, StringF)
      case Field(_, Foreign(t)) => {
        assert(schema.models.contains(t))
        val targetModel = schema.models(t)
        getColumn(schema, targetModel, fType, fieldName)
      }
    }
  }

  def chooseField(s: GenState, model: Model,  declaredOnly: Boolean,
    nonAggrField: Boolean, fType: Option[FType]) = {
    if (declaredOnly || (!s.dfields.isEmpty && RUtils.bool())) {
      val typedDFields = s.dfields filter FieldFilterer.filterDeclaredField(fType)
      val dfields =
        if (nonAggrField) typedDFields filter { !FieldDecl.isAggregate(_) }
        else typedDFields
      dfields match {
        case Seq() => {
          val (columnName, columnType) = getColumn(s.schema, model, fType)
          (F(columnName), columnType)
        }
        case dfields => {
          val dfield = RUtils.chooseFrom(dfields)
          (F(dfield.as), dfield.ftype)
        }
      }
    } else {
      val (columnName, columnType) = getColumn(s.schema, model, fType)
      (F(columnName), columnType)
    }
  }

  def getGenStateExpr(s: GenState, fType: Option[FType]) = fType match {
    case None | Some(NumberF) => s
    case Some(StrF)           => s.strExprCandidates
  }

  def genConstant(fType: Option[FType]) = fType match {
    case None =>
      if(RUtils.bool) (Constant(RUtils.word(), Quoted), StringF)
      else (Constant(RUtils.integer().toString, UnQuoted), IntF)
    case Some(NumberF) => (Constant(RUtils.integer().toString, UnQuoted), IntF)
    case _ => (Constant(RUtils.word(), Quoted), StringF)
  }

  def generateFieldExpr(s: GenState, model: Model,
      fType: Option[FType],
      forAggr: Boolean = false,
      declaredOnly: Boolean = false,
      nonAggrField: Boolean = false): (FieldExpr, FieldType) = {
    val s2 = getGenStateExpr(s, fType)
    val exprNode =
      if (!forAggr && (s2.depth >= maxDepth || RUtils.bool()))
        // If we reached the maximum depth, generate a leaf node.
        RUtils.chooseFrom(Seq(FExpr, ConstantExpr))
      else
        if (!forAggr && (s2.depth < minDepth)) {
          val fExprs = s2.exprCands filter(e => (fType, e) match {
            case (None, FExpr) | (None, ConstantExpr)
              | (Some(NumberF), FExpr) | (Some(NumberF), ConstantExpr) => false
            case (Some(StrF), FExpr) | (Some(StrF), ConstantExpr)=> true
            case (Some(StrF), _) => false
            case _ => true
          })
          if (fExprs.size > 1) RUtils.chooseFrom(fExprs)
          else RUtils.chooseFrom(s2.exprCands)
        }
        else RUtils.chooseFrom(s2.exprCands)
    exprNode match {
      case FExpr => chooseField(s2, model, declaredOnly, nonAggrField, fType)
      case ConstantExpr => genConstant(fType)
      case CountExpr => {
        // It's not meaningful to count a compound expression. revisit
        val s3 = s2 exprCandidates (List(FExpr))
        (Count(Some(generateFieldExpr(
          s3.++, model, fType, declaredOnly = declaredOnly, nonAggrField = true)._1)), IntF)
      }
      case SumExpr => {
        // You cannot apply an aggregate function to an aggregate function.
        val s3 = s2.disgardAggregates
        (Sum(generateFieldExpr(
          s3.++, model, fType, declaredOnly = declaredOnly, nonAggrField = true)._1), DoubleF)
      }
      case AvgExpr => {
        val s3 = s2.disgardAggregates
        (Avg(generateFieldExpr(
          s3.++, model, fType, declaredOnly = declaredOnly, nonAggrField = true)._1), DoubleF)
      }
      case MinExpr => {
        val s3 = s2.disgardAggregates
        val (e, eType) = generateFieldExpr(
          s3.++, model, fType, declaredOnly = declaredOnly, nonAggrField = true)
        (Min(e), eType)
      }
      case MaxExpr => {
        val s3 = s2.disgardAggregates
        val (e, eType) = generateFieldExpr(
          s3.++, model, fType, declaredOnly = declaredOnly, nonAggrField = true)
        (Max(e), eType)
      }
      case AddExpr => {
        val e1 = generateFieldExpr(s2.++, model, fType, declaredOnly = declaredOnly, nonAggrField = nonAggrField)._1
        val e2 = generateFieldExpr(s2.++, model, fType, declaredOnly = declaredOnly, nonAggrField = nonAggrField)._1
        (Add(e1, e2), DoubleF)
      }
      case SubExpr => {
        val e1 = generateFieldExpr(s2.++, model, fType, declaredOnly = declaredOnly, nonAggrField = nonAggrField)._1
        val e2 = generateFieldExpr(s2.++, model, fType, declaredOnly = declaredOnly, nonAggrField = nonAggrField)._1
        (Sub(e1, e2), DoubleF)
      }
      case MulExpr => {
        val e1 = generateFieldExpr(s2.++, model, fType, declaredOnly = declaredOnly, nonAggrField = nonAggrField)._1
        val e2 = generateFieldExpr(s2.++, model, fType, declaredOnly = declaredOnly)._1
        (Mul(e1, e2), DoubleF)
      }
      case DivExpr => {
        val e1 = generateFieldExpr(s2.++, model, fType, declaredOnly = declaredOnly, nonAggrField = nonAggrField)._1
        val e2 = generateFieldExpr(s2.++, model, fType, declaredOnly = declaredOnly, nonAggrField = nonAggrField)._1
        (Div(e1, e2), DoubleF)
      }
    }
  }

  def generatePredicate(s: GenState, model: Model, fType: Option[FType]): Predicate = {
    assert(s.model.isDefined)
    val fields = s.getFields(false, fType)
    val predExprs = fType match {
      case None | Some(StrF) => predNodes filter {
        case GtPred | GtePred | LtPred | LtePred => false // FIXME
        case _ => true
      }
      case Some(NumberF) => predNodes filter {
        case ContainsPred | StartsWithPred | EndsWithPred => false
        case _ => true }
    }
    val predNode = RUtils.chooseFrom(predExprs)
    val s2 = s.disgardAggregates
    predNode match {
      case EqPred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s2.++, model, fType)._1
        Eq(field, e)
      }
      case GtPred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s2.++, model, fType)._1
        Gt(field, e)
      }
      case GtePred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s2.++, model, fType)._1
        Gte(field, e)
      }
      case LtPred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s2.++, model, fType)._1
        Lt(field, e)
      }
      case LtePred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s2.++, model, fType)._1
        Lte(field, e)
      }
      case ContainsPred => {
        val field = RUtils.chooseFrom(fields)
        Contains(field, RUtils.word())
      }
      case StartsWithPred => StartsWith(RUtils.chooseFrom(fields), RUtils.word())
      case EndsWithPred => EndsWith(RUtils.chooseFrom(fields), RUtils.word())
      case AndPred => And(generatePredicate(s2.++, model, fType), generatePredicate(s2.++, model, genQSType))
      case OrPred  => Or(generatePredicate(s2.++, model, fType), generatePredicate(s2.++, model, genQSType))
      case NotPred => Not(generatePredicate(s2.++, model, fType))
    }
  }

  // This method generates a default field that corresponds to the id
  // of the source table. The name of this field is '_default'.
  def genDefaultField(s: GenState, model: Model) = s.dfields match {
    case Seq() =>
      s dfield FieldDecl(F(model.name + ".id"), "_default", IntF, hidden = false)
    case _ => s
  }

  def generateDeclFields(s: GenState, model: Model,
                         nonHidden: Boolean = true,
                         declF: Option[List[FType]] = None,
                         forAggr: Boolean = false,
                         declaredOnly: Boolean = false) = {
    def _generateFieldDecl(s: GenState, declF: Option[List[FType]]): GenState = {
      val (stopCond, fields) =
        if(forAggr) (!s.aggrF.isEmpty && RUtils.bool(), None)
        else declF match {
          case None      => (RUtils.bool(), None) // We randomly choose whether to stop or not.
          case Some(Nil) => (true, None) // We do not have more fields to generate.
          case _         => (false, declF)
        }
      if (stopCond) s
      else {
        val (ftype, tail) = fields match {
          case None         => (genQSType, None)
          case Some(h :: t) => (Some(h), Some(t))
          case _            => ???
        }
        val (e, eType) = generateFieldExpr(s.++, model, ftype, forAggr = forAggr,
                                           declaredOnly = declaredOnly)
        val f = FieldDecl(e, RUtils.word(special = false), eType,
                          if (nonHidden) RUtils.bool() else false)
        _generateFieldDecl(if (forAggr) s afield f else s dfield f, tail)
      }
    }
    _generateFieldDecl(s, declF)
  }

  def generateQuerySetforCombined(s: GenState) = s.qs match {
    // First option: there is not any generated query. So
    // generate two random query sets and combine the two.
    case None => {
      // Determine the number of fields in each sub-query
      val nuf = RUtils.integer() + 1
      val dFields = List.range(0, nuf) map { _ => RUtils.chooseFrom(Seq(NumberF, StrF)) }
      (
        // first sub-query
        generateQuerySet(
          GenState(s.schema, cands = Seq(NewQS), exprCands = exprNodes),
          declF = Some(dFields), hiddenF = false),
        // second sub-query
        generateQuerySet(
          GenState(s.schema, cands = Seq(NewQS), exprCands = exprNodes),
          declF = Some(dFields), hiddenF = false)
      )
    }
    // Second option: generate a single query, and combine it
    // with the already generated one.
    case Some(qs) => {
      (
        s,
        generateQuerySet(
          GenState(s.schema, cands = Seq(NewQS), exprCands = exprNodes),
          declF = Some((s.dfields map FieldDecl.ftype map {
            case StringF => StrF
            case _       => NumberF
          }).toList), hiddenF = false)
      )
    }
  }

  def genQSType() =
    if (wellTyped) Some(RUtils.chooseFrom(Seq(NumberF, StrF)))
    else None

  def generateQuerySet(s: GenState, declF: Option[List[FType]] = None,
      hiddenF: Boolean = false, declaredOnly: Boolean = false): GenState =
    // We randomly decide if we should stop query generation or not.
    if (s.qs.isDefined && RUtils.bool() || s.cands.isEmpty) s
    else {
      assert(!s.cands.isEmpty)
      val next = RUtils.chooseFrom(s.cands)
      next match {
        case NewQS => {
          // Choose a random model to query.
          val model = RUtils.chooseFrom(s.schema.models.values.toSeq)
          val s2 = genDefaultField(generateDeclFields(
            s.++, model, nonHidden = hiddenF, declF = declF), model)
          val qs = New(model.name, s2.dfields)
          val s3 = s2 queryset qs
          val s4 = s3 candidates List(ApplySort, ApplyFilter, ApplyDistinct)
          generateQuerySet(s4 model model)
        }
        case UnionQS => {
          val (s1, s2) = generateQuerySetforCombined(s)
          val combined = (s1 queryset (Union(s1.qs.get, s2.qs.get))).++
          generateQuerySet(combined candidates List(ApplySort, UnionQS, IntersectQS),
                           declaredOnly = true)
        }
        case IntersectQS => {
          val (s1, s2) = generateQuerySetforCombined(s)
          val combined = (s1 queryset (Intersect(s1.qs.get, s2.qs.get))).++
          generateQuerySet(combined candidates List(ApplySort, UnionQS, IntersectQS),
                           declaredOnly = true)
        }
        case ApplySort => {
          assert(s.model.isDefined)
          val sortedFields = RUtils.sample(s.getFields(declaredOnly, None))
          val s2 = s candidates (s.cands filter { x => x != ApplySort })
          if (sortedFields.isEmpty) generateQuerySet(s2)
          else {
            val orderSpec = sortedFields map { x =>
              (x, RUtils.chooseFrom(List(Asc, Desc)))
            }
            val qs = Apply(Sort(orderSpec), s2.qs.get)
            generateQuerySet(s2 queryset qs)
          }
        }
        case ApplyFilter => {
          assert(s.model.isDefined)
          val pred = generatePredicate(s.++, s.model.get, genQSType)
          val qs = Apply(Filter(pred), s.qs.get)
          val s2 = s queryset qs
          val s3 = s2 candidates (s2.cands filter { x => x != ApplyFilter })
          generateQuerySet(s3)
        }
        case ApplyDistinct => {
          assert(s.model.isDefined)
          // With field
          val qs =
            if (RUtils.bool) {
              val field = RUtils.chooseFrom(s.getFields(false, None))
              Apply(Distinct(Some(field)), s.qs.get)
            } else { // Without Field
              Apply(Distinct(Option.empty[String]), s.qs.get)
            }
          val s2 = s queryset qs
          val s3 = s2 candidates (s2.cands filter { x => x != ApplyDistinct })
          generateQuerySet(s3)
        }
      }
    }

  def apply(schema: Schema): Query = {
    val qType = RUtils.chooseFrom(List(
      "set",
      "aggr",
      "union",
      "first",
      "subset"
    ))
    val cands = if (noCombined) Seq(NewQS) else Seq(NewQS, UnionQS, IntersectQS)
    qType match {
      case "set" =>
        SetRes(generateQuerySet(GenState(
          schema, cands = cands, exprCands = exprNodes)).qs.get)
      case "first" => {
        val qs = generateQuerySet(GenState(
          schema, cands = cands, exprCands = exprNodes)).qs.get
        if (qs.ordered) FirstRes(qs)
        else SetRes(qs)
      }
      case "subset" => {
        val qs = generateQuerySet(GenState(
          schema, cands = cands, exprCands = exprNodes)).qs.get
        val offset = RUtils.integer()
        if (qs.ordered)
          SubsetRes(offset, limit = Some(offset + RUtils.integer()), qs)
        else SetRes(qs)
      }
      case "aggr" => {
        val exprCands = exprNodes filter { x => x match {
          case CountExpr | SumExpr | AvgExpr | MaxExpr | MinExpr => false
          case _ => true
        } }
        val s1 = generateQuerySet(GenState(
          schema, cands = cands, exprCands = exprCands))
        val s2 = s1.disgardExprs
        // reference only declared fields if the generated query set is
        // combined.
        val f = generateDeclFields(s2.++, s2.model.get, nonHidden = false,
                                   forAggr = true,
                                   declaredOnly = s2.qs.get.combined)
        AggrRes(f.aggrF, s2.qs.get)
      }
      case "union" =>
        SetRes(generateQuerySet(GenState(
          schema, cands = cands, exprCands = exprNodes)).qs.get)
    }
  }
}
