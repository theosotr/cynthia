package gr.dmst.aueb.cynthia


sealed trait QuerySetNode
case object NewQS extends QuerySetNode
case object ApplySort extends QuerySetNode
case object ApplyGroup extends QuerySetNode
case object ApplyFilter extends QuerySetNode
case object UnionQS extends QuerySetNode
case object IntersectQS extends QuerySetNode


sealed trait PredNode
case object EqPred extends PredNode
case object GtPred extends PredNode
case object GtePred extends PredNode
case object LtPred extends PredNode
case object LtePred extends PredNode
case object ContainsPred extends PredNode
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


case class GenState(
  schema: Schema,
  model: Option[Model] = None,
  depth: Int = 0,
  cands: Seq[QuerySetNode] = Seq(NewQS),
  exprCands: Seq[ExprNode] = Seq(),
  qs: Option[QuerySet] = None,
  dfields: Seq[FieldDecl] = Seq()) {

  val aggregateNodes = Seq(
    CountExpr,
    SumExpr,
    AvgExpr,
    MinExpr,
    MaxExpr
  )

  def ++() =
    GenState(schema, model, depth + 1, cands, exprCands, qs, dfields)

  def queryset(qs: QuerySet) =
    GenState(schema, model, depth, cands, exprCands, Some(qs), dfields)

  def model(m: Model) =
    GenState(schema, Some(m), depth, cands, exprCands, qs, dfields)

  def candidates(c: Seq[QuerySetNode]) =
    GenState(schema, model, depth, c, exprCands, qs, dfields)

  def exprCandidates(c: Seq[ExprNode]) =
    GenState(schema, model, depth, cands, c, qs, dfields)

  def dfield(f: FieldDecl) =
    GenState(schema, model, depth, cands, exprCands, qs, dfields :+ f)

  def disgardAggregates() = {
    val c = exprCands filter { !aggregateNodes.contains(_) }
    GenState(schema, model, depth, cands, c, qs, dfields)
  }
}


case object QueryGenerator {

  val maxDepth = 25

  val predNodes = Seq(
    EqPred,
    GtPred,
    GtePred,
    LtPred,
    LtePred,
    ContainsPred,
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
                columnName: String = ""): (String, FieldType) = {
    val field = RUtils.chooseFrom(model.fields)
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
        getColumn(schema, targetModel, fieldName)
      }
    }
  }

  def generateFieldExpr(s: GenState, model: Model): (FieldExpr, FieldType) = {
    val exprNode =
      if (s.depth >= maxDepth || RUtils.bool())
        // If we reached the maximum depth, generate a leaf node.
        RUtils.chooseFrom(Seq(FExpr, ConstantExpr))
      else RUtils.chooseFrom(s.exprCands)
    exprNode match {
      case FExpr => {
        if (!s.dfields.isEmpty && RUtils.bool()) {
          val dField = RUtils.chooseFrom(s.dfields)
          (F(dField.as), dField.ftype)
        } else {
          val (columnName, columnType) = getColumn(s.schema, model)
          (F(columnName), columnType)
        }
      }
      case ConstantExpr =>
        if (RUtils.bool) (Constant(RUtils.string(), Quoted), StringF)
        else (Constant(s"${RUtils.integer()}", UnQuoted), IntF)
      case CountExpr => {
        // It's not meaningful to count a compound expression. revisit
        val s2 = s exprCandidates (List(FExpr))
        (Count(Some(generateFieldExpr(s2.++, model)._1)), IntF)
      }
      case SumExpr => {
        // You cannot apply an aggregate function to an aggregate function.
        val s2 = s.disgardAggregates
        (Sum(generateFieldExpr(s2.++, model)._1), DoubleF)
      }
      case AvgExpr => {
        val s2 = s.disgardAggregates
        (Avg(generateFieldExpr(s2.++, model)._1), DoubleF)
      }
      case MinExpr => {
        val s2 = s.disgardAggregates
        val (e, eType) = generateFieldExpr(s2.++, model)
        (Min(e), eType)
      }
      case MaxExpr => {
        val s2 = s.disgardAggregates
        val (e, eType) = generateFieldExpr(s2.++, model)
        (Max(e), eType)
      }
      case AddExpr => {
        val e1 = generateFieldExpr(s.++, model)._1
        val e2 = generateFieldExpr(s.++, model)._1
        (Add(e1, e2), DoubleF)
      }
      case SubExpr => {
        val e1 = generateFieldExpr(s.++, model)._1
        val e2 = generateFieldExpr(s.++, model)._1
        (Sub(e1, e2), DoubleF)
      }
      case MulExpr => {
        val e1 = generateFieldExpr(s.++, model)._1
        val e2 = generateFieldExpr(s.++, model)._1
        (Mul(e1, e2), DoubleF)
      }
      case DivExpr => {
        val e1 = generateFieldExpr(s.++, model)._1
        val e2 = generateFieldExpr(s.++, model)._1
        (Div(e1, e2), DoubleF)
      }
    }
  }

  def generatePredicate(s: GenState, model: Model): Predicate = {
    assert(s.model.isDefined)
    val fields = s.dfields map { FieldDecl.as }
    val predNode = RUtils.chooseFrom(predNodes)
    predNode match {
      case EqPred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s.++, model)._1
        Eq(field, e)
      }
      case GtPred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s.++, model)._1
        Gt(field, e)
      }
      case GtePred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s.++, model)._1
        Gte(field, e)
      }
      case LtPred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s.++, model)._1
        Lt(field, e)
      }
      case LtePred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s.++, model)._1
        Lte(field, e)
      }
      case ContainsPred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s.++, model)._1
        Contains(field, e)
      }
      case AndPred => And(generatePredicate(s.++, model), generatePredicate(s.++, model))
      case OrPred  => Or(generatePredicate(s.++, model), generatePredicate(s.++, model))
      case NotPred => Not(generatePredicate(s.++, model))
    }
  }

  def generateDeclFields(s: GenState, model: Model): GenState =
      if (RUtils.bool()) s
      else {
        val (e, eType) = generateFieldExpr(s.++, model)
        val f = FieldDecl(e, RUtils.word(), eType, RUtils.bool())
        generateDeclFields(s dfield f, model)
      }


  def generateQuerySet(s: GenState): QuerySet =
    // We randomly decide if we should stop query generation or not.
    if (s.qs.isDefined && RUtils.bool() || s.cands.isEmpty) s.qs.get
    else {
      assert(!s.cands.isEmpty)
      val next = RUtils.chooseFrom(s.cands)
      next match {
        case NewQS => {
          // Choose a random model to query.
          val model = RUtils.chooseFrom(s.schema.models.values.toSeq)
          val s2 = generateDeclFields(s.++, model)
          val qs = New(model.name, s2.dfields)
          val s3 = s2 queryset qs
          val s4 = s3 candidates List(ApplySort)
          generateQuerySet(s4 model model)
        }
        case ApplySort => {
          assert(s.model.isDefined)
          val sortedFields = RUtils.sample(
            (s.dfields filter { !FieldDecl.hidden(_) } map FieldDecl.as) ++
            (s.model.get.fields map { x => x match {
              case Field(n, Foreign(_)) => s.model.get.name + "." + n + ".id"
              case Field(n, _)          => s.model.get.name + "." + n
            }})
          )
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
          val pred = generatePredicate(s.++, s.model.get)
          val qs = Apply(Filter(pred), s.qs.get)
          val s2 = s queryset qs
          val s3 = s2 candidates (s2.cands filter { x => x != ApplyFilter })
          generateQuerySet(s3)
        }
      }
    }

  def apply(schema: Schema): Query = {
    val qs = generateQuerySet(GenState(schema, exprCands = exprNodes))
    val qType = RUtils.chooseFrom(List(
      "set"
    ))
    qType match {
      case "set" => SetRes(qs)
    }
  }
}
