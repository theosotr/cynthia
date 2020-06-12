package gr.dmst.aueb.cynthia


sealed trait QuerySetNode
case object NewQS extends QuerySetNode
case object ApplySort extends QuerySetNode
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
    DivExpr
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

  def getFields(declaredOnly: Boolean) = {
    val declFields = dfields filter { !FieldDecl.hidden(_) } map FieldDecl.as
    if (declaredOnly) declFields
    else declFields ++
      (model.get.fields map { x => x match {
        case Field(n, Foreign(_)) => model.get.name + "." + n + ".id"
        case Field(n, _)          => model.get.name + "." + n
      }})
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

  def generateFieldExpr(s: GenState, model: Model,
      forAggr: Boolean = false): (FieldExpr, FieldType) = {
    val exprNode =
      if (!forAggr && (s.depth >= maxDepth || RUtils.bool()))
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
        if (RUtils.bool) (Constant(RUtils.word, Quoted), StringF)
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
    val fields = s.getFields(false)
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

  def generateDeclFields(s: GenState, model: Model, nonHidden: Boolean = true,
      number: Option[Int] = None, forAggr: Boolean = false) = {
    def _generateFieldDecl(s: GenState, i: Int): GenState = {
      val stopCond =
        if(forAggr) !s.aggrF.isEmpty && RUtils.bool()
        else number match {
          case None    => RUtils.bool() // We randomly choose whether to stop or not.
          case Some(n) => i >= n
        }
      if (stopCond) s
      else {
        val (e, eType) = generateFieldExpr(s.++, model, forAggr = forAggr)
        val f = FieldDecl(e, RUtils.word(), eType,
                          if (nonHidden) RUtils.bool() else false)
        _generateFieldDecl(if (forAggr) s afield f else s dfield f, i + 1)
      }
    }
    _generateFieldDecl(s, 0)
  }

  def generateQuerySetforCombined(s: GenState) = s.qs match {
    // First option: there is not any generated query. So
    // generate two random query sets and combine the two.
    case None => {
      // Determine the number of fields in each sub-query
      val nuf = RUtils.integer() + 1
      (
        // first sub-query
        generateQuerySet(
          GenState(s.schema, cands = Seq(NewQS), exprCands = exprNodes),
          declF = Some(nuf), hiddenF = false),
        // second sub-query
        generateQuerySet(
          GenState(s.schema, cands = Seq(NewQS), exprCands = exprNodes),
          declF = Some(nuf), hiddenF = false)
      )
    }
    // Second option: generate a single query, and combine it
    // with the already generated one.
    case Some(qs) => {
      (
        s,
        generateQuerySet(
          GenState(s.schema, cands = Seq(NewQS), exprCands = exprNodes),
          declF = Some(s.dfields.size), hiddenF = false)
      )
    }
  }

  def generateQuerySet(s: GenState, declF: Option[Int] = None,
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
          val s2 = generateDeclFields(s.++, model, nonHidden = hiddenF,
                                      number = declF)
          val qs = New(model.name, s2.dfields)
          val s3 = s2 queryset qs
          val s4 = s3 candidates List(ApplySort)
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
          val sortedFields = RUtils.sample(s.getFields(declaredOnly))
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
    val qType = RUtils.chooseFrom(List(
      "set",
      "aggr",
      "union"
    ))
    qType match {
      case "set" =>
        SetRes(generateQuerySet(GenState(schema, exprCands = exprNodes)).qs.get)
      case "aggr" => {
        val exprCands = exprNodes filter { x => x match {
          case CountExpr | SumExpr | AvgExpr | MaxExpr | MinExpr => false
          case _ => true
        } }
        val s1 = generateQuerySet(GenState(schema, exprCands = exprCands))
        val s2 = s1.disgardExprs
        val f = generateDeclFields(s2.++, s2.model.get, nonHidden = false,
                                   forAggr = true)
        AggrRes(f.aggrF, s2.qs.get)
      }
      case "union" =>
        SetRes(generateQuerySet(GenState(schema, exprCands = exprNodes)).qs.get)
    }
  }
}
