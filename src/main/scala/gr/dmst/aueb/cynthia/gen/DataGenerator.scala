package gr.dmst.aueb.cynthia.gen

import scala.sys.process._
import scala.collection.immutable.ListMap

import com.microsoft.z3.{Solver,Context,Status,Expr,ArithExpr,IntExpr,SeqExpr,BoolExpr}

import gr.dmst.aueb.cynthia._
import gr.dmst.aueb.cynthia.translators.State


object NaiveDataGenerator {
  def generateRow(fields: Seq[Field], i: Int, foreignKeyCands: Int): Seq[Constant] =
    fields map { case Field(n, t) =>  t match {
      case Serial => Constant(i.toString, UnQuoted)
      case Int8 | Int16 | Int32 | Int64 | Numeric =>
        Constant(
          (RUtils.integer() * (RUtils.chooseFrom(List(1, -1)))).toString,
          UnQuoted
        )
      case VarChar(n) => Constant(Utils.escapeSQLStr(RUtils.word()), Quoted)
      case Bool => Constant(RUtils.bool().toString, UnQuoted)
      case Foreign(_) => Constant(
        (RUtils.integer(n = foreignKeyCands) + 1).toString, UnQuoted)
    }}

  def apply(model: Model, foreignKeyCands: Int, limit: Int = 1000) = {
    def _generateData(data: LazyList[Seq[Constant]], i: Int): LazyList[Seq[Constant]] =
      if (i <= 0) data
      else _generateData(generateRow(model.fields, i, foreignKeyCands) #:: data, i - 1)

    _generateData(LazyList.empty, limit)
  }
}


object SolverDataGenerator {

  def getVariable(field: String, i: Int,
      vars: ListMap[String, (Seq[Expr], FieldType)]) =
    vars get field match {
      case None => throw new Exception(s"Variable of field $field not found.")
      case Some((expr, _)) => expr(i)
    }

  def constructExpr(e: FieldExpr, i: Int, c: Context,
      vars: ListMap[String, (Seq[Expr], FieldType)]): Expr = e match {
    case Constant(v, UnQuoted) => c.mkInt(v)
    case Constant(v, Quoted)   => c.mkString(v)
    case F(f)                  => getVariable(f, i, vars)
    case Count(_)              => c.mkInt("1") // It's the number of records included in each group.
    case Sum(e)                => constructExpr(e, i, c, vars)
    case Avg(e)                => constructExpr(e, i, c, vars)
    case Max(e)                => constructExpr(e, i, c, vars)
    case Min(e)                => constructExpr(e, i, c, vars)
    case Add(e1, e2)           =>
      c.mkAdd(
        constructExpr(e1, i, c, vars).asInstanceOf[ArithExpr],
        constructExpr(e2, i, c, vars).asInstanceOf[ArithExpr]
      )
    case Sub(e1, e2)           =>
      c.mkSub(
        constructExpr(e1, i, c, vars).asInstanceOf[ArithExpr],
        constructExpr(e2, i, c, vars).asInstanceOf[ArithExpr]
      )
    case Mul(e1, e2)           =>
      c.mkMul(
        constructExpr(e1, i, c, vars).asInstanceOf[ArithExpr],
        constructExpr(e2, i, c, vars).asInstanceOf[ArithExpr]
      )
    case Div(e1, e2)           =>
      c.mkDiv(
        constructExpr(e1, i, c, vars).asInstanceOf[ArithExpr],
        constructExpr(e2, i, c, vars).asInstanceOf[ArithExpr]
      )
  }

  def constructConstraint(k: String, e: FieldExpr, op: (Expr, Expr) => BoolExpr,
      c: Context, vars: ListMap[String, (Seq[Expr], FieldType)]) = {
    val cons = List.range(0, 5) map { i =>
      op(getVariable(k, i, vars), constructExpr(e, i, c, vars))
    }
    c.mkOr(cons:_*)
  }

  def predToFormula(pred: Predicate, c: Context,
      vars: ListMap[String, (Seq[Expr], FieldType)]): BoolExpr = pred match {
    case Eq(k, e)       => constructConstraint(k, e, c.mkEq, c, vars)
    case Lt(k, e)       => constructConstraint(k, e, { (e1, e2) => c.mkLt(e1.asInstanceOf[ArithExpr], e2.asInstanceOf[ArithExpr]) }, c, vars)
    case Lte(k, e)      => constructConstraint(k, e, { (e1, e2) => c.mkLe(e1.asInstanceOf[ArithExpr], e2.asInstanceOf[ArithExpr]) }, c, vars)
    case Gt(k, e)       => constructConstraint(k, e, { (e1, e2) => c.mkGt(e1.asInstanceOf[ArithExpr], e2.asInstanceOf[ArithExpr]) }, c, vars)
    case Gte(k, e)      => constructConstraint(k, e, { (e1, e2) => c.mkGe(e1.asInstanceOf[ArithExpr], e2.asInstanceOf[ArithExpr]) }, c, vars)
    case Contains(k, Constant(v, _)) =>
      constructConstraint(
        k,
        Constant(v, Quoted),
        { (e1, e2) => c.mkContains(e1.asInstanceOf[SeqExpr], e2.asInstanceOf[SeqExpr]) },
        c, vars
      )
    case StartsWith(k, v) =>
      constructConstraint(
        k,
        Constant(v, Quoted),
        { (e1, e2) => c.mkPrefixOf(e2.asInstanceOf[SeqExpr], e1.asInstanceOf[SeqExpr]) },
        c, vars
      )
    case EndsWith(k, v) =>
      constructConstraint(
        k,
        Constant(v, Quoted),
        { (e1, e2) => c.mkSuffixOf(e2.asInstanceOf[SeqExpr], e1.asInstanceOf[SeqExpr]) },
        c, vars
      )
    case Not(p) => c.mkNot(predToFormula(p, c, vars))
    case Or(p1, p2) =>
      c.mkOr(predToFormula(p1, c, vars), predToFormula(p2, c, vars))
    case And(p1, p2) => 
      c.mkAnd(predToFormula(p1, c, vars), predToFormula(p2, c, vars))
  }

  def getPredConstraints(preds: Set[Predicate], c: Context,
      vars: ListMap[String, (Seq[Expr], FieldType)]) =
    preds map { p => predToFormula(p, c, vars) }

  def getFieldConstraints(m: Model, ctx: Context,
      vars: ListMap[String, (Seq[Expr], FieldType)]) = m.fields map {
    case Field("id", _) => {
      val cons = List.range(0, 5) map { i =>
        ctx.mkEq(
          getVariable(m.name + ".id", i, vars).asInstanceOf[IntExpr],
          ctx.mkInt((i + 1).toString)
        )
      }
      ctx.mkAnd(cons:_*)
    }
    case Field(f, _) => {
      val varName = m.name + "." + f
      val cons = List.range(0, 5).foldLeft(List[BoolExpr]()) { case (acc, i) =>
        List.range(i, 5).foldLeft(acc) { case (acc, j) =>
          if (i != j) {
            val con = ctx.mkNot(
              ctx.mkEq(getVariable(varName, i, vars), getVariable(varName, j, vars)))
            con :: acc
          } else acc
        }
      }
      ctx.mkAnd(cons:_*)
    }
  }

  def getVarSort(t: DataType, ctx: Context) = t match {
    case Int8 | Int16 | Int32 | Int64 | Foreign(_) | Serial => ctx.getIntSort
    case Numeric => ctx.getRealSort
    case VarChar(_) => ctx.mkStringSort
    case Bool => ctx.mkBoolSort
  }

  def constructModelVariables(m: Model, c: Context) =
    m.fields.foldLeft(ListMap[String, (Seq[Expr], FieldType)]()) { case (acc, Field(n, t)) => {
      val varName = m.name + "." + n
      val vars = List.range(0, 5) map { i =>
        c.mkConst(varName + "_" + i.toString, getVarSort(t, c))
      }
      acc + (varName -> (vars, FieldType.dataTypeToFieldType(t))) }
    }

  def constructCompoundVariables(s: State, c: Context,
      vars: ListMap[String, (Seq[Expr], FieldType)]) =
    s.fields.values.foldLeft(vars) { case (acc, FieldDecl(e, as, t, _)) =>
      val exprs = List.range(0, 5) map { i =>
        constructExpr(e, i, c, acc)
      }
      acc + (as -> (exprs, t))
    }

  def generateData(m: Model, s: Solver,
      vars: ListMap[String, (Seq[Expr], FieldType)]) = s.check match {
    case Status.SATISFIABLE => {
      val m = s.getModel
      Some(List.range(0, 5) map { i =>
        (vars map {
          case (_, (exprs, StringF)) =>
            Constant(m.eval(exprs(i), false).toString.replace("\"", ""), Quoted)
          case (_, (exprs, _)) => Constant(m.eval(exprs(i), false).toString, UnQuoted)
        }).toSeq
      })
    }
    case _ => None 
  }

  def apply(q: Query, state: State, s: Schema) = s.models get state.source match {
    case None => throw new Exception(
      s"Model '${state.source}' not found in schema '${s.name}'")
    case Some(m) => {
      val ctx = new Context
      val modelVars = constructModelVariables(m, ctx)
      val vars = constructCompoundVariables(state, ctx, modelVars) 
      val cons = getFieldConstraints(m, ctx, vars) ++ getPredConstraints(
        state.preds, ctx, vars)
      val s = ctx.mkSolver
      s.add(cons:_*)
      generateData(m, s, modelVars)
    }
  }
}
