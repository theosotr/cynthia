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


case class SolverDataGenerator(schema: Schema) {
  private val declModels: scala.collection.mutable.Set[String] =
    scala.collection.mutable.Set()

  private val vars: scala.collection.mutable.ListMap[String, (Seq[Expr], FieldType)] =
    scala.collection.mutable.ListMap()

  private var solver: Solver = null

  def getVariable(field: String, i: Int) =
    vars get field match {
      case None => throw new Exception(s"Variable of field $field not found.")
      case Some((expr, _)) => expr(i)
    }

  def constructJoinVars(models: Seq[String], c: Context) = {
    def addPrev(x: String, prev: Seq[String]) =
      if (prev.isEmpty) prev :+ x
      else prev :+ x.toLowerCase

    (models zip models.tail).foldLeft(Seq[String]()) { case (prev, (s, t)) =>
      if (declModels.contains(t)) addPrev(s, prev)
      else {
        schema.models get t match {
          case None    => throw new Exception(
            s"Model $t not found in schema '{$schema.name}'")
          case Some(m) => {
            val prev2 = addPrev(s, prev)
            declModels.add(m.name)
            vars ++= constructModelVariables(m, c, prefix = prev2 mkString ".")
            List.range(0, 5) foreach { i => {
              val source = prev2 mkString "."
              val con = c.mkEq(
                getVariable(source + "." + t.toLowerCase + "_id", i),
                getVariable(source + "." + t.toLowerCase + ".id", i)
              )
              solver.add(con)
            }}
            solver.add(
              getFieldConstraints(m, c, prefix = prev2 mkString "."):_*)
            prev2
          }
        }
      }
    }
  }

  def constructOrderJoins(state: State, c: Context) =
    state.orders map { case (x, _) => x } foreach { x =>
      x split '.' match {
        case Array() | Array(_) | Array(_, _) => ()
        case arr =>
          constructJoinVars(arr.dropRight(1).toSeq map { _.capitalize }, c)
      }
    }

  def getVariableJ(field:String, i: Int, c: Context) = field split '.' match {
    case Array(_) | Array(_, _) => getVariable(field, i)
    case arr => {
      val models = arr.dropRight(1).toSeq map { _.capitalize }
      constructJoinVars(models, c)
      getVariable(field, i)
    }
  }

  def constructExpr(e: FieldExpr, i: Int, c: Context): Expr = e match {
    case Constant(v, UnQuoted) => c.mkInt(v)
    case Constant(v, Quoted)   => c.mkString(v)
    case F(f)                  => getVariableJ(f, i, c)
    case Count(_)              => c.mkInt("1") // It's the number of records included in each group.
    case Sum(e)                => constructExpr(e, i, c)
    case Avg(e)                => constructExpr(e, i, c)
    case Max(e)                => constructExpr(e, i, c)
    case Min(e)                => constructExpr(e, i, c)
    case Add(e1, e2)           =>
      c.mkAdd(
        constructExpr(e1, i, c).asInstanceOf[ArithExpr],
        constructExpr(e2, i, c).asInstanceOf[ArithExpr]
      )
    case Sub(e1, e2)           =>
      c.mkSub(
        constructExpr(e1, i, c).asInstanceOf[ArithExpr],
        constructExpr(e2, i, c).asInstanceOf[ArithExpr]
      )
    case Mul(e1, e2)           =>
      c.mkMul(
        constructExpr(e1, i, c).asInstanceOf[ArithExpr],
        constructExpr(e2, i, c).asInstanceOf[ArithExpr]
      )
    case Div(e1, e2)           =>
      c.mkDiv(
        constructExpr(e1, i, c).asInstanceOf[ArithExpr],
        constructExpr(e2, i, c).asInstanceOf[ArithExpr]
      )
  }

  def constructConstraint(k: String, e: FieldExpr, op: (Expr, Expr) => BoolExpr,
      c: Context) = {
    val cons = List.range(0, 5) map { i =>
      op(getVariableJ(k, i, c), constructExpr(e, i, c))
    }
    c.mkOr(cons:_*)
  }

  def predToFormula(pred: Predicate, c: Context): BoolExpr = pred match {
    case Eq(k, e)       => constructConstraint(k, e, c.mkEq, c)
    case Lt(k, e)       => constructConstraint(k, e, { (e1, e2) => c.mkLt(e1.asInstanceOf[ArithExpr], e2.asInstanceOf[ArithExpr]) }, c)
    case Lte(k, e)      => constructConstraint(k, e, { (e1, e2) => c.mkLe(e1.asInstanceOf[ArithExpr], e2.asInstanceOf[ArithExpr]) }, c)
    case Gt(k, e)       => constructConstraint(k, e, { (e1, e2) => c.mkGt(e1.asInstanceOf[ArithExpr], e2.asInstanceOf[ArithExpr]) }, c)
    case Gte(k, e)      => constructConstraint(k, e, { (e1, e2) => c.mkGe(e1.asInstanceOf[ArithExpr], e2.asInstanceOf[ArithExpr]) }, c)
    case Contains(k, Constant(v, _)) =>
      constructConstraint(
        k,
        Constant(v, Quoted),
        { (e1, e2) => c.mkContains(e1.asInstanceOf[SeqExpr], e2.asInstanceOf[SeqExpr]) },
        c
      )
    case Contains(_, _) => ??? // Unreachable case
    case StartsWith(k, v) =>
      constructConstraint(
        k,
        Constant(v, Quoted),
        { (e1, e2) => c.mkPrefixOf(e2.asInstanceOf[SeqExpr], e1.asInstanceOf[SeqExpr]) },
        c
      )
    case EndsWith(k, v) =>
      constructConstraint(
        k,
        Constant(v, Quoted),
        { (e1, e2) => c.mkSuffixOf(e2.asInstanceOf[SeqExpr], e1.asInstanceOf[SeqExpr]) },
        c
      )
    case Not(p) => c.mkNot(predToFormula(p, c))
    case Or(p1, p2) =>
      c.mkOr(predToFormula(p1, c), predToFormula(p2, c))
    case And(p1, p2) => 
      c.mkAnd(predToFormula(p1, c), predToFormula(p2, c))
  }

  def getPredConstraints(preds: Set[Predicate], c: Context) =
    preds map { p => predToFormula(p, c) }

  def getFieldConstraints(m: Model, ctx: Context, prefix: String = "") = m.fields map {
    case Field("id", _) => {
      val modelName =
        if(prefix.equals("")) m.name
        else prefix + "." + m.name.toLowerCase
      val cons = List.range(0, 5) map { i =>
        ctx.mkEq(
          getVariable(modelName + ".id", i).asInstanceOf[IntExpr],
          ctx.mkInt((i + 1).toString)
        )
      }
      ctx.mkAnd(cons:_*)
    }
    case Field(f, t) => {
      val modelName =
        if(prefix.equals("")) m.name
        else prefix + "." + m.name.toLowerCase
      val varName = t match {
        case Foreign(_) => modelName + "." + f + "_id"
        case _          => modelName + "." + f
      }
      val cons = List.range(0, 5).foldLeft(List[BoolExpr]()) { case (acc, i) =>
        List.range(i, 5).foldLeft(acc) { case (acc, j) =>
          if (i != j) {
            val con = ctx.mkNot(
              ctx.mkEq(getVariable(varName, i), getVariable(varName, j)))
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

  def constructModelVariables(m: Model, c: Context, prefix: String = "") = {
    declModels.add(m.name)
    m.fields.foldLeft(ListMap[String, (Seq[Expr], FieldType)]()) { case (acc, Field(n, t)) => {
      val fName = t match {
        case Foreign(_) => n + "_id"
        case _          => n
      }
      val varName =
        if (!prefix.equals("")) prefix + "." + m.name.toLowerCase + "." + fName
        else m.name + "." + fName
      val vars = List.range(0, 5) map { i =>
        c.mkConst(varName + "_" + i.toString, getVarSort(t, c))
      }
      acc + (varName -> (vars, FieldType.dataTypeToFieldType(t))) }
    }
  }

  def constructCompoundVariables(s: State, c: Context) =
    s.fields.values foreach { case FieldDecl(e, as, t, _) =>
      val exprs = List.range(0, 5) map { i =>
        constructExpr(e, i, c)
      }
      vars += (as -> (exprs, t))
    }

  def getModelVariables(m: Model, f: String) = (vars.view.filterKeys { x =>
    x.contains(m.name + "." + f) || x.contains(m.name.toLowerCase + "." + f)
  }).head

  def generateData(s: Solver) = s.check match {
    case Status.SATISFIABLE => {
      val m = s.getModel
      val modelMap = schema.models.foldLeft(Map[String, Set[String]]()) { case (acc, (k, v)) => {
        val acc2 = if (acc.contains(k)) acc else acc + (k -> Set[String]())
        (v.fields filter Field.isForeign).foldLeft(acc2) { case (acc, Field(_, Foreign(n))) => {
          acc get k match {
            case None    => acc + (k -> Set(n))
            case Some(e) => acc + (k -> (e + n))
          }
        }}
      }}
      val models = Utils.topologicalSort(modelMap) filter { x =>
        declModels.contains(x) }
      Some(models.foldLeft(Map[Model, Seq[Seq[Constant]]]()) { case (acc, x) => {
        val model = schema.models(x)
        val records = List.range(0, 5) map { i =>
          (model.fields map {
            case Field(n, Foreign(_)) => n + "_id"
            case Field(n, _)          => n
           } map { f => getModelVariables(model, f) } map {
            case (_, (exprs, StringF)) =>
              Constant(m.eval(exprs(i), false).toString.replace("\"", ""), Quoted)
            case (_, (exprs, _)) => Constant(m.eval(exprs(i), false).toString, UnQuoted)
          }).toSeq
        }
        acc + (model -> records)
      }})
    }
    case _ => None
  }

  def apply(q: Query, state: State) = schema.models get state.source match {
    case None => throw new Exception(
      s"Model '${state.source}' not found in schema '${schema.name}'")
    case Some(m) => {
      val ctx = new Context
      solver = ctx.mkSolver
      val params = ctx.mkParams
      params.add("timeout", 2000)
      solver.setParameters(params)
      vars ++= constructModelVariables(m, ctx)
      constructCompoundVariables(state, ctx) 
      constructOrderJoins(state, ctx)
      val cons = getFieldConstraints(m, ctx) ++ getPredConstraints(
        state.preds, ctx)
      solver.add(cons:_*)
      generateData(solver)
    }
  }
}
