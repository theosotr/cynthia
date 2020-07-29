package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._

import scala.collection.immutable.ListMap


case class PonyTranslator(target: Target) extends Translator {
  target.db match {
    case MSSQL(_, _, _) =>
      throw UnsupportedException("Pony does not support MSSQL queries")
    case _ => ()
  }
  var fieldsMap = ListMap[String, String]()
  var nonHiddenFieldsMap = Map[String, String]()

  override val preamble =
    s"""import numbers
    |from decimal import Decimal
    |from pony.orm import *
    |from models_${target.db.getName} import *
    |
    |# set_sql_debug(True)
    |db.generate_mapping()
    |
    |def dump(x, label):
    |    if isinstance(x, numbers.Number):
    |        print(label, round(Decimal(float(x) + 0.00), 2))
    |    else:
    |        try:
    |            print(label, round(Decimal(float(x) + 0.00), 2))
    |        except:
    |            if type(x) is bytes:
    |                print(label, str(x.decode('utf-8')))
    |            else:
    |                print(label, x if x is not None else '0.00')
    |
    |with db_session:
    |""".stripMargin


  def getPonyFieldName(field: String) =
    field.split('.').toList match {
      case Nil | _ :: Nil => field
      case t              =>
        t takeRight(2) map { x => x.toLowerCase } mkString "."
    }

  override def emitPrint(q: Query, dFields: Seq[String], ret: String) = {
    def _dumpField(v: String, fields: Iterable[String], ident: String = "") =
      fields map { as => {
        if (nonHiddenFieldsMap.contains(as)) {
          val index = fieldsMap.keysIterator.toList.indexOf(as)
          s"${ident}dump($v[$index], '$as')"
        } else {
          s"${ident}dump(getattr($v, '$as', getattr($v[0], '$as', None) if isinstance($v, tuple) else None), '$as')"
        }
      }
      } mkString "\n"

    q match {
      case SetRes(_)  => {
        val trimmedRet = ret.trim
        s"    for r in $trimmedRet:\n${_dumpField("r", dFields, ident = " " * 8)}"
      }
      case SubsetRes(offset, limit, _) => {
        val trimmedRet = ret.trim
        val resolvedLimit = limit match {
          case Some(x) => x + 1
          case _       => ""
        }
        val subset = s"[$offset:$resolvedLimit]"
        s"    for r in $trimmedRet$subset:\n${_dumpField("r", dFields, ident = " " * 8)}"
      }
      case FirstRes(_) => _dumpField(ret, dFields, ident = " " * 4)
      case _ => ???
    }
  }

  def constructPrimAggr(fexpr: FieldExpr) = {
    val (field, op) = fexpr match {
      case Count(None)        => ("", "fn.count")
      case Count(Some(field)) => (constructFieldExpr(field), "fn.count")
      case Sum(field)         => (constructFieldExpr(field), "fn.sum")
      case Avg(field)         => (constructFieldExpr(field), "fn.avg")
      case Min(field)         => (constructFieldExpr(field), "fn.min")
      case Max(field)         => (constructFieldExpr(field), "fn.max")
      case _                  => ??? // Unreachable case
    }
    op + "(" + field + ")"
  }

  def constructCompoundAggr(fexpr: FieldExpr) = {
    val (a1, a2, op) = fexpr match {
      case Add(a1, a2) => (a1, a2, " + ")
      case Sub(a1, a2) => (a1, a2, " - ")
      case Mul(a1, a2) => (a1, a2, " * ")
      case Div(a1, a2) => (a1, a2, " / ")
      case _           => ??? // Unreachable case
    }
    val str = Str("(") << constructFieldExpr(a1) << op << constructFieldExpr(a2) << ")"
    str.!
  }

  def constructFieldExpr(fexpr: FieldExpr): String = fexpr match {
    case F(f)                  => getPonyFieldName(f)
    case Constant(v, UnQuoted) => v
    case Constant(v, Quoted)   => s""""${v}""""
    case _    => ???
      // if (!fexpr.compound) constructPrimAggr(fexpr)
      // else constructCompoundAggr(fexpr)
  }

  def translatePred(pred: Predicate): String = pred match {
    case Eq(k, e) =>
      (Str(getPonyFieldName(k)) << "==" << constructFieldExpr(e)).!
    case Gt(k, e) =>
      (Str(getPonyFieldName(k)) << " > " << constructFieldExpr(e)).!
    case Gte(k, e) =>
      (Str(getPonyFieldName(k)) << " >= " << constructFieldExpr(e)).!
    case Lt(k, e) =>
      (Str(getPonyFieldName(k)) << " < " << constructFieldExpr(e)).!
    case Lte(k, e) =>
      (Str(getPonyFieldName(k)) << " <= " << constructFieldExpr(e)).!
    case Contains(k, e)             =>
      (Str(constructFieldExpr(e)) << " in " << getPonyFieldName(k)).!
    case StartsWith(k, v) => ???
    case EndsWith(k, v) => ???
    case Not(pred)                  =>
      (Str("not (") << translatePred(pred) << ")").!
    case Or(p1, p2)                 =>
      (Str("(") << translatePred(p1) << ") or (" << translatePred(p2) << ")").!
    case And(p1, p2)                =>
      (Str("(") << translatePred(p1) << ") and (" << translatePred(p2) << ")").!
  }

  // The following function until extractFields are used to create a map for
  // field declarations to their expressions.
  def constructPrimField(acc: ListMap[String, String], fexpr: FieldExpr) = {
    val (field, op) = fexpr match {
      case Count(None)        => ("", "count")
      case Count(Some(field)) => (constructField(acc, field), "count")
      case Sum(field)         => (constructField(acc, field), "sum")
      case Avg(field)         => (constructField(acc, field), "avg")
      case Min(field)         => (constructField(acc, field), "min")
      case Max(field)         => (constructField(acc, field), "max")
      case _                  => ??? // Unreachable case
    }
    op + "(" + field + ")"
  }

  def constructField(acc: ListMap[String, String], fexpr: FieldExpr): String = fexpr match {
    case F(f) => acc.get(f) match {
      case None     => getPonyFieldName(f)
      case Some(s)  => s
    }
    case Constant(v, UnQuoted) => v
    case Constant(v, Quoted)   => Utils.quoteStr(v)
    case Add(e1, e2) => "(" + constructField(acc, e1) + "+" + constructField(acc, e2) + ")"
    case Sub(e1, e2) => "(" + constructField(acc, e1) + "-" + constructField(acc, e2) + ")"
    case Mul(e1, e2) => "(" + constructField(acc, e1) + "*" + constructField(acc, e2) + ")"
    case Div(e1, e2) => "(" + constructField(acc, e1) + "/" + constructField(acc, e2) + ")"
    case _    => constructPrimField(acc, fexpr)
  }

  def extractFields(fields: Map[String, FieldDecl]): ListMap[String, String] = {
    val counter: Iterator[Int] = LazyList.from(0).iterator
    fields.foldLeft(ListMap[String, String]()) ({ (acc, x) =>
        x._2 match {
          case FieldDecl(e, as, _, _) =>
            acc + (as -> (constructField(acc, e)))
        }
      })
  }

  def constructSelectedItems(source: String, joins: Seq[Seq[String]],
    fields: ListMap[String, String]) =
    if (fields.isEmpty)
      joins.foldLeft(Seq[String](source.toLowerCase)) { (acc, x) => {
        val (_, Seq(_, t)) = x splitAt (x.size - 2)
        val tLow = t.toLowerCase
        acc :+ tLow
      }} mkString ","
    else
      fields.foldLeft(Seq[String]()) { (acc, x) => {
        acc :+ x._2
      }} mkString ","


  def constructFilter(preds: Set[Predicate]) =
    if (preds.isEmpty)
      ""
    else {
      val conditions = preds map { x =>
        (Str("(") << translatePred(x) << ")").!
      } mkString " and "
      "if " + conditions
    }

  def constructJoins(joins: Seq[Seq[String]]) = {
    joins.foldLeft(Seq[String]()) { (acc, x) => {
      val (_, Seq(s, t)) = x splitAt (x.size - 2)
      val tLow = t.toLowerCase
      val sLow = s.toLowerCase
      acc :+ s"for ${tLow} in ${sLow}.${tLow}_id"
    }} mkString " "
  }

  // select, join, filter
  def constructQueryPrefix(s: State, selectedItems: String, joins: String,
    filter: String) =  s.query match {
    case None    => {
      val qstr = s"select((${selectedItems}) for ${s.source.toLowerCase} in ${s.source} ${joins} ${filter})"
      QueryStr(
        Some("    ret" + s.numGen.next().toString),
        Some(qstr)
      )
    }
    case Some(q) => q
  }

  def constructOrderBy(s: State, selectedItems: String) = s.orders match {
    case Seq() => ""
    case spec  => {
      (
        Str(s"order_by(lambda ${selectedItems}: (") << (
          spec map { x =>
            x match {
              case (k, Desc) => "desc(" + getPonyFieldName(k) + ")"
              case (k, Asc)  => getPonyFieldName(k)
            }
          } mkString ","
        ) << "))"
      ).!
    }
  }

  def constructFirst(first: Boolean) =
    if (first) "first()"
    else ""

  override def constructCombinedQuery(s: State) = QueryStr(Some("combined"))

  override def constructNaiveQuery(s: State, first: Boolean, offset: Int,
      limit: Option[Int]) = {
    def _filterNonHiddenFieldDecls(i: String) =
      !FieldDecl.hidden(s.fields.get(i).get)
    fieldsMap = extractFields(s.fields)
    nonHiddenFieldsMap = (fieldsMap.view filterKeys _filterNonHiddenFieldDecls).toMap
    val orderedJoins = s.joins.toSet.toSeq.sortWith { (x, y) => x.size < y.size }
    val selectedItems = constructSelectedItems(s.source, orderedJoins, fieldsMap)
    val joins = constructJoins(orderedJoins)
    val (aggrP, nonAggrP) = s.preds partition { _.hasAggregate(s.fields) }
    val filter = constructFilter(nonAggrP)
    val qStr = constructQueryPrefix(s, selectedItems, joins, filter)
    qStr >> QueryStr(Some("    ret" + s.numGen.next().toString),
      Some(
        Seq(
          qStr.ret.get.trim,
          constructOrderBy(s, selectedItems),
          s.distinct match {
            case Some(x) => ""
            case _       => "without_distinct()"
          },
          constructFirst(first),
        ) filter {
          case "" => false
          case _  => true
        }  mkString(".")
      )
    )
  }

  override def unionQueries(s1: State, s2: State) =
    throw new UnsupportedException("unions are not implemented")

  override def intersectQueries(s1: State, s2: State) =
    throw new UnsupportedException("unions are not implemented")
}
