package gr.dmst.aueb.cynthia.gen

import scala.sys.process._
import scala.collection.immutable.ListMap

import com.microsoft.z3.{Solver,Context,Status,Expr,IntExpr,SeqExpr,BoolExpr}

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
      case Some((expr, _)) => expr(i)
      case Some((expr, _)) => expr(i)
    }

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

  def generateData(m: Model, s: Solver,
      vars: ListMap[String, (Seq[Expr], FieldType)]): Seq[Seq[Constant]] = s.check match {
    case Status.SATISFIABLE => {
      val m = s.getModel
      List.range(0, 5) map { i =>
        (vars map {
          case (_, (exprs, StringF)) => Constant(m.eval(exprs(i), false).toString, Quoted)
          case (_, (exprs, _)) => Constant(m.eval(exprs(i), false).toString, UnQuoted)
        }).toSeq
      }
    }
  }

  def apply(q: Query, state: State, s: Schema) = s.models get state.source match {
    case None => throw new Exception(
      s"Model '${state.source}' not found in schema '${s.name}'")
    case Some(m) => {
      val ctx = new Context
      val vars = constructModelVariables(m, ctx)
      val cons = getFieldConstraints(m, ctx, vars)
      val s = ctx.mkSolver
      s.add(cons:_*)
      generateData(m, s, vars)
    }
  }
}
