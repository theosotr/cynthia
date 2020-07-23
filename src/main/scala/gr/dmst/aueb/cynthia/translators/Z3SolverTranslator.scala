package gr.dmst.aueb.cynthia.translators


import gr.dmst.aueb.cynthia._


case class Z3SolverTranslator(schema: Schema) extends Translator {

  val preamble = "from z3 import *"

  def getVarType(t: DataType) = t match {
    case Int8 | Int16 | Int32 | Int64 => "Int"
    case Numeric => "Real"
    case _ => "Int" // FIXME
  }

  def declareModelVariables(m: Model) =
    m.fields.foldLeft(QueryStr()) { case (acc, Field(n, t)) =>
      List.range(1, 6).foldLeft(acc) { case (acc, i) => {
        val varName = s"${m.name}_${n}_${i.toString}"
        acc >> QueryStr(
          Some(varName),
          Some(s"${getVarType(t)}(${Utils.quoteStr(varName)})")
        )}
      }
    }

  def declareModelsVariables(s: State) = schema.models get s.source match {
    // TODO Support Joined tables.
    case None => throw new Exception(
      s"Model '${s.source}' not found in schema '${schema.name}'")
    case Some(m) => declareModelVariables(m)
  }

  def declareVariables(s: State) =
    s.fields.values.foldLeft(QueryStr()) { case (acc, FieldDecl(e, as, t, _)) =>
      List.range(1, 6).foldLeft(acc) { case (acc, i) =>
        acc >> QueryStr(Some(as + "_" + i.toString), Some(constructExpr(e, i)))
      }
    }

  def getVariable(field: String, i: Int) =
    field split '.' match {
      case Array(field) => field + "_" + i.toString
      case Array(m, f)  => m + "_" +  f + "_" + i.toString
      case array => ??? // TODO
    }

  def constructExpr(e: FieldExpr, i: Int): String = e match {
    case Constant(c, UnQuoted) => c
    case Constant(c, Quoted)   => c
    case F(f)                  => getVariable(f, i)
    case Count(_)              => "1" // It's the number of records included in each group.
    case Sum(e)                => constructExpr(e, i)
    case Avg(e)                => constructExpr(e, i)
    case Max(e)                => constructExpr(e, i)
    case Min(e)                => constructExpr(e, i)
    case Add(e1, e2)           => "(" + constructExpr(e1, i) + "+" + constructExpr(e2, i) + ")"
    case Sub(e1, e2)           => "(" + constructExpr(e1, i) + "-" + constructExpr(e2, i) + ")"
    case Mul(e1, e2)           => "(" + constructExpr(e1, i) + "*" + constructExpr(e2, i) + ")"
    case Div(e1, e2)           => "(" + constructExpr(e1, i) + "/" + constructExpr(e2, i) + ")"
  }

  def constructConstraint(k: String, e: FieldExpr, operator: String) =
    "Or(" + (
      List.range(1, 6) map { i =>
        getVariable(k, i) + " " + operator + " " + constructExpr(e, i)
      } mkString ","
    ) + ")"

  def predToFormula(pred: Predicate): String = pred match {
    case Eq(k, e)    => constructConstraint(k, e, "==")
    case Lt(k, e)    => constructConstraint(k, e, "<")
    case Lte(k, e)   => constructConstraint(k, e, "<=")
    case Gt(k, e)    => constructConstraint(k, e, ">")
    case Gte(k, e)   => constructConstraint(k, e, ">=")
    case Not(p)      => "Not(" + predToFormula(p) + ")"
    case Or(p1, p2)  => "Or(" + predToFormula(p1) + ", " + predToFormula(p2) + ")"
    case And(p1, p2) => "And(" + predToFormula(p1) + ", " + predToFormula(p2) + ")"
    case Contains(_, _) | StartsWith(_, _) | EndsWith(_, _) => "" // FIXME
  }

  def getPredConstraints(preds: Set[Predicate]) =
    preds filter { !_.equals("") } map predToFormula

  def getFieldConstraints(s: State) = schema.models get s.source match {
    case None => throw new Exception(
      s"Model '${s.source}' not found in schema '${schema.name}'")
    case Some(m) => m.fields map {
      case Field("id", _) =>
        "And(" + (
          List.range(1, 6) map { i => getVariable(s.source + ".id", i) + "==" + i.toString } mkString ","
        ) + ")"
      case Field(f, _) => {
        val constraints = List.range(1, 6).foldLeft(List[String]()) { case (acc, i) =>
          List.range(i, 6).foldLeft(acc) { case (acc, j) =>
            if (i != j) {
              val con = 
                getVariable(s.source + "." + f, i) + "!=" + getVariable(s.source + "." + f, j)
              con :: acc
            } else acc
          }
        }
        "And(" + (constraints mkString ",") + ")"
      } 
    }
  }

  override def constructNaiveQuery(s: State, first: Boolean , offset: Int,
      limit: Option[Int]) = {
    val qStr = declareModelsVariables(s) >> declareVariables(s)
    val constraints = getPredConstraints(s.preds) ++ getFieldConstraints(s)
    qStr >> QueryStr(None, Some("solve(" + (constraints mkString ",") + ")"))
  }

  override def constructCombinedQuery(s: State): QueryStr =
    QueryStr(Some(""), Some(""))

  override def emitPrint(q: Query, dFields: Seq[String], ret: String): String =
    ""

  override def unionQueries(s1: State, s2: State): State =
    s1

  override def intersectQueries(s1: State, s2: State): State =
    s2
}
