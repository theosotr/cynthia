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

package cynthia.gen

import java.io.File
import scala.sys.process._
import scala.collection.immutable.ListMap

import com.microsoft.z3.{Solver, Context, Status, Expr, ArithExpr, IntExpr,
  SeqExpr, BoolExpr}

import cynthia.lang._
import cynthia.targets.{DB, DBSetup}
import cynthia.translators.{State, SchemaTranslator}
import cynthia.utils.{Utils, RUtils, Str}


sealed trait DataGenerator {
  def apply(): Map[Model, Seq[Seq[Constant]]]
}

case class NaiveDataGenerator(schema: Schema, nuRecords: Int) extends DataGenerator {

  private val foreignKeyCands = nuRecords

  def generateRow(fields: Seq[Field], i: Int): Seq[Constant] =
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

  def generateModelData(model: Model) = {
    def _generateData(data: LazyList[Seq[Constant]], i: Int): LazyList[Seq[Constant]] =
      if (i <= 0) data
      else _generateData(generateRow(model.fields, i) #:: data, i - 1)

    _generateData(LazyList.empty, nuRecords)
  }

  def apply() = {
    (schema.getModelsInTopSort map (m => {
      val model = schema.models(m)
      (model, generateModelData(model))
    })).toMap
  }
}


case class SolverDataGenerator(
  schema: Schema,
  nuRecords: Int,
  q: Query,
  queryState: State,
  timeout: Int) extends DataGenerator {

  // This set holds the name of models related to the given query. This
  // also holds the name of models joined with the initial one.
  private val declModels: scala.collection.mutable.Set[String] =
    scala.collection.mutable.Set()

  // This map is used for holding all Z3 variables that model the fields of
  // the given query. Recall that each query field is modeled through a sequence
  // of Z3 variables (i.e., one for every database record).
  private var vars: ListMap[String, (Seq[Expr], FieldType)] = ListMap()

  // This is the Z3 context object.
  private val ctx: Context = new Context

  // This is the Z3 Solver object taken from context.
  private val solver: Solver = ctx.mkSolver

  def getVariable(field: String, i: Int) =
    vars get field match {
      case None => throw new Exception(s"Variable of field $field not found.")
      case Some((expr, _)) => expr(i)
    }

  def constructJoinVars(models: Seq[String]) = {
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
            vars ++= constructModelVariables(m, prefix = prev2 mkString ".")
            List.range(0, nuRecords) foreach { i => {
              val source = prev2 mkString "."
              val con = ctx.mkEq(
                getVariable(source + "." + t.toLowerCase + "_id", i),
                getVariable(source + "." + t.toLowerCase + ".id", i)
              )
              solver.add(con)
            }}
            solver.add(
              getFieldConstraints(m, prefix = prev2 mkString "."):_*)
            prev2
          }
        }
      }
    }
  }

  def constructOrderJoins() =
    queryState.orders map { case (x, _) => x } foreach { x =>
      x split '.' match {
        case Array() | Array(_) | Array(_, _) => ()
        case arr =>
          constructJoinVars(arr.dropRight(1).toSeq map { _.capitalize })
      }
    }

  def getVariableJ(field:String, i: Int) = field split '.' match {
    case Array(_) | Array(_, _) => getVariable(field, i)
    case arr => {
      val models = arr.dropRight(1).toSeq map { _.capitalize }
      constructJoinVars(models)
      getVariable(field, i)
    }
  }

  def constructExpr(e: FieldExpr, i: Int): Expr = e match {
    case Constant(v, UnQuoted) => ctx.mkInt(v)
    case Constant(v, Quoted)   => ctx.mkString(v)
    case F(f)                  => getVariableJ(f, i)
    case Count(None)           => ctx.mkInt("1") // It's the number of records included in each group.
    case Count(Some(e))        => { constructExpr(e, i); ctx.mkInt("1") }
    case Sum(e)                => constructExpr(e, i)
    case Avg(e)                => constructExpr(e, i)
    case Max(e)                => constructExpr(e, i)
    case Min(e)                => constructExpr(e, i)
    case Add(e1, e2)           =>
      ctx.mkAdd(
        constructExpr(e1, i).asInstanceOf[ArithExpr],
        constructExpr(e2, i).asInstanceOf[ArithExpr]
      )
    case Sub(e1, e2)           =>
      ctx.mkSub(
        constructExpr(e1, i).asInstanceOf[ArithExpr],
        constructExpr(e2, i).asInstanceOf[ArithExpr]
      )
    case Mul(e1, e2)           =>
      ctx.mkMul(
        constructExpr(e1, i).asInstanceOf[ArithExpr],
        constructExpr(e2, i).asInstanceOf[ArithExpr]
      )
    case Div(e1, e2)           =>
      ctx.mkDiv(
        constructExpr(e1, i).asInstanceOf[ArithExpr],
        constructExpr(e2, i).asInstanceOf[ArithExpr]
      )
  }

  def constructConstraint(k: String, e: FieldExpr, op: (Expr, Expr) => BoolExpr) = {
    val cons = List.range(0, nuRecords) map { i =>
      op(getVariableJ(k, i), constructExpr(e, i))
    }
    ctx.mkOr(cons:_*)
  }

  def predToFormula(pred: Predicate): BoolExpr = pred match {
    case Eq(k, e)       => constructConstraint(k, e, ctx.mkEq)
    case Lt(k, e)       => constructConstraint(k, e, { (e1, e2) => ctx.mkLt(e1.asInstanceOf[ArithExpr], e2.asInstanceOf[ArithExpr]) })
    case Lte(k, e)      => constructConstraint(k, e, { (e1, e2) => ctx.mkLe(e1.asInstanceOf[ArithExpr], e2.asInstanceOf[ArithExpr]) })
    case Gt(k, e)       => constructConstraint(k, e, { (e1, e2) => ctx.mkGt(e1.asInstanceOf[ArithExpr], e2.asInstanceOf[ArithExpr]) })
    case Gte(k, e)      => constructConstraint(k, e, { (e1, e2) => ctx.mkGe(e1.asInstanceOf[ArithExpr], e2.asInstanceOf[ArithExpr]) })
    case Contains(k, v) =>
      constructConstraint(
        k,
        Constant(v, Quoted),
        { (e1, e2) => ctx.mkContains(e1.asInstanceOf[SeqExpr], e2.asInstanceOf[SeqExpr]) }
      )
    case StartsWith(k, v) =>
      constructConstraint(
        k,
        Constant(v, Quoted),
        { (e1, e2) => ctx.mkPrefixOf(e2.asInstanceOf[SeqExpr], e1.asInstanceOf[SeqExpr]) }
      )
    case EndsWith(k, v) =>
      constructConstraint(
        k,
        Constant(v, Quoted),
        { (e1, e2) => ctx.mkSuffixOf(e2.asInstanceOf[SeqExpr], e1.asInstanceOf[SeqExpr]) }
      )
    case Not(p) => ctx.mkNot(predToFormula(p))
    case Or(p1, p2) =>
      ctx.mkOr(predToFormula(p1), predToFormula(p2))
    case And(p1, p2) => 
      ctx.mkAnd(predToFormula(p1), predToFormula(p2))
  }

  def getPredConstraints(preds: Set[Predicate]) =
    preds map { p => predToFormula(p) }

  def getFieldConstraints(m: Model, prefix: String = "") = m.fields.foldLeft(Seq[BoolExpr]()) {
    case (acc, Field(f, t)) => {
      val modelName =
        if(prefix.equals("")) m.name
        else prefix + "." + m.name.toLowerCase
      val varName = t match {
        case Foreign(_) => modelName + "." + f + "_id"
        case _          => modelName + "." + f
      }
      // Checks whether the variable name corresponds to a grouping field
      // or the primary key of the table.
      if (!queryState.getNonConstantGroupingFields.contains(varName) || f.equals("id")) {
		val cons = List.range(0, nuRecords).foldLeft(List[BoolExpr]()) { case (acc, i) =>
		  List.range(i, nuRecords).foldLeft(acc) { case (acc, j) =>
			if (i != j) {
			  val con = ctx.mkNot(
				ctx.mkEq(getVariable(varName, i), getVariable(varName, j)))
			  con :: acc
			} else acc
		  }
		}
		acc :+ ctx.mkAnd(cons:_*)
      } else acc
    }
  }

  def getVarSort(t: DataType) = t match {
    case Int8 | Int16 | Int32 | Int64 | Foreign(_) | Serial => ctx.getIntSort
    case Numeric => ctx.getRealSort
    case VarChar(_) => ctx.mkStringSort
    case Bool => ctx.mkBoolSort
  }

  def constructModelVariables(m: Model, prefix: String = "") = {
    declModels.add(m.name)
    m.fields.foldLeft(ListMap[String, (Seq[Expr], FieldType)]()) { case (acc, Field(n, t)) => {
      val fName = t match {
        case Foreign(_) => n + "_id"
        case _          => n
      }
      val varName =
        if (!prefix.equals("")) prefix + "." + m.name.toLowerCase + "." + fName
        else m.name + "." + fName
      val vars = List.range(0, nuRecords) map { i =>
        ctx.mkConst(varName + "_" + i.toString, getVarSort(t))
      }
      acc + (varName -> (vars, FieldType.dataTypeToFieldType(t))) }
    }
  }

  def constructCompoundVariables() =
    queryState.fields.values foreach { case FieldDecl(e, as, t, _) =>
      val exprs = List.range(0, nuRecords) map { i => constructExpr(e, i) }
      vars += (as -> (exprs, t))
    }

  def getModelVariables(m: Model, f: String) = (vars.view.filterKeys { x =>
    x.contains(m.name + "." + f) || x.contains(m.name.toLowerCase + "." + f)
  }).head

  def generateData() = solver.check match {
    case Status.SATISFIABLE => {
      val m = solver.getModel

      val models = schema.getModelsInTopSort filter { declModels.contains(_) }
      models.foldLeft(Map[Model, Seq[Seq[Constant]]]()) { case (acc, x) => {
        val model = schema.models(x)
        val records = List.range(0, nuRecords) map { i =>
          (model.fields map {
            case Field(n, Foreign(_)) => n + "_id"
            case Field(n, _)          => n
           } map { f => getModelVariables(model, f) } map {
            case (_, (exprs, StringF)) =>
              Constant(m.eval(exprs(i), true).toString.replace("\"", ""), Quoted)
            case (_, (exprs, _)) => Constant(m.eval(exprs(i), true).toString, UnQuoted)
          }).toSeq
        }
        acc + (model -> records)
      }}
    }
    // The solver was unable to generate any data.
    case _ => Map[Model, Seq[Seq[Constant]]]()
  }

  def inspectAggregates() = {
    def inspectExpr(e: FieldExpr): Unit = e match {
      case F(f) => f split '.' match {
        case Array() | Array(_) | Array(_, _) => ()
        case arr =>
          constructJoinVars(arr.dropRight(1).toSeq map { _.capitalize })
      }
      case Count(Some(e)) => inspectExpr(e)
      case Sum(e) => inspectExpr(e)
      case Avg(e) => inspectExpr(e)
      case Max(e) => inspectExpr(e)
      case Min(e) => inspectExpr(e)
      case Add(e1, e2) => {inspectExpr(e1); inspectExpr(e2)}
      case Sub(e1, e2) => {inspectExpr(e1); inspectExpr(e2)}
      case Mul(e1, e2) => {inspectExpr(e1); inspectExpr(e2)}
      case Div(e1, e2) => {inspectExpr(e1); inspectExpr(e2)}
      case _ => ()
    }

    (queryState.aggrs map FieldDecl.expr) foreach inspectExpr
  }

  def apply() = schema.models get queryState.source match {
    case None => throw new Exception(
      s"Model '${queryState.source}' not found in schema '${schema.name}'")
    case Some(m) => {
      val params = ctx.mkParams
      params.add("timeout", timeout)
      solver.setParameters(params)
      vars ++= constructModelVariables(m)
      constructCompoundVariables
      constructOrderJoins
      inspectAggregates
      val cons = getFieldConstraints(m) ++ getPredConstraints(queryState.preds)
      solver.add(cons:_*)
      generateData
    }
  }
}


sealed trait DataGenRes
case object DataExists extends DataGenRes
case object DataGenFailed extends DataGenRes
case class  DataGenSucc(data: () => Unit) extends DataGenRes


case class DataGeneratorController(
  schema: Schema,
  dataFile: String,
  genStrategy: Option[DataGenerator]
) {

  def populateDBs(dbs: Set[DB]) =
    if (Utils.exists(dataFile)) {
      dbs.foreach { db => DBSetup.setupSchema(db, dataFile) }
      DataExists
    } else {
      genStrategy match {
        case None => DataExists
        case Some(genStrategy) => {
          val data = genStrategy()
          if (data.isEmpty) DataGenFailed
          else {
            val insStms =
              data.foldLeft(Str("")) { case (acc, (model, data)) =>
                acc << SchemaTranslator.dataToInsertStmts(model, data)
              }.toString
            dbs foreach { db =>
              val dataPath = Utils.joinPaths(
                List(Utils.getProjectDir, schema.name, s"data_${db.getName}.sql"))
              Utils.writeToFile(dataPath, insStms)
              DBSetup.setupSchema(db, dataPath)
            }
            DataGenSucc(() => {
              new File(dataFile).getParentFile.mkdirs
              Utils.writeToFile(dataFile, insStms)
            })
          }
        }
      }
    }
}