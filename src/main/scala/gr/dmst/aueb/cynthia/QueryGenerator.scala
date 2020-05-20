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


case class GenState(
  schema: Schema,
  model: Option[Model] = None,
  depth: Int = 0,
  cands: List[QuerySetNode] = List(NewQS, UnionQS, IntersectQS),
  qs: Option[QuerySet] = None,
  dfields: Set[FieldDecl] = Set()) {

  def ++() =
    GenState(schema, model, depth + 1, cands, qs, dfields)

  def queryset(qs: QuerySet) =
    GenState(schema, model, depth, cands, Some(qs), dfields)

  def model(m: Model) =
    GenState(schema, Some(m), depth, cands, qs, dfields)

  def candidates(c: List[QuerySetNode]) =
    GenState(schema, model, depth, c, qs, dfields)
}


trait QueryGenerator {

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

  def generateFieldExpr(s: GenState, model: Model): FieldExpr

  def generatePredicate(s: GenState, model: Model): Predicate = {
    assert(s.model.isDefined)
    val mfields = s.model.get.fields.map { case Field(n, _) => n }
    val fields = mfields ++ (s.dfields map { FieldDecl.as })
    val predNode = RUtils.chooseFrom(predNodes)
    predNode match {
      case EqPred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s.++, model)
        Eq(field, e)
      }
      case GtPred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s.++, model)
        Gt(field, e)
      }
      case GtePred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s.++, model)
        Gte(field, e)
      }
      case LtPred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s.++, model)
        Lt(field, e)
      }
      case LtePred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s.++, model)
        Lte(field, e)
      }
      case ContainsPred => {
        val field = RUtils.chooseFrom(fields)
        val e = generateFieldExpr(s.++, model)
        Contains(field, e)
      }
      case AndPred => And(generatePredicate(s.++, model), generatePredicate(s.++, model))
      case OrPred  => Or(generatePredicate(s.++, model), generatePredicate(s.++, model))
      case NotPred => Not(generatePredicate(s.++, model))
    }
  }

  def generateDeclFields(s: GenState, model: Model): Set[FieldDecl]

  def generateQuerySet(s: GenState): QuerySet =
    if (s.qs.isDefined && RUtils.chooseFrom(Seq(false, true))) s.qs.get
    else {
      assert(!s.cands.isEmpty)
      val next = RUtils.chooseFrom(s.cands)
      next match {
        case NewQS => {
          val model = RUtils.chooseFrom(s.schema.models)
          val dFields = generateDeclFields(s.++, model)
          val qs = New(model.name, dFields)
          val s2 = s queryset qs
          val s3 = s2 candidates List(ApplySort, ApplyFilter)
          generateQuerySet(s3 model model)
        }
        case ApplySort => {
          assert(s.model.isDefined)
          val fields = s.model.get.fields.map { case Field(n, _) => n }
          val sortedFields = RUtils.sample(fields) 
          val orderSpec = sortedFields map { x =>
            (x, RUtils.chooseFrom(List(Asc, Desc)))
          }
          val qs = Apply(Sort(orderSpec), s.qs.get)
          val s2 = s queryset qs
          val s3 = s2 candidates (s2.cands filter { x => x != ApplySort })
          generateQuerySet(s3)
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

  def apply(schema: Schema): Query
}
