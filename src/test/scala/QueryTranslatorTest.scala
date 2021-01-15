import java.nio.file.Paths
import scala.io.Source

import org.scalatest.funsuite.AnyFunSuite

import cynthia.lang._
import cynthia.targets.{Peewee, Django, SQLAlchemy, Sequelize,
  ActiveRecord, SQLite, Target}
import cynthia.translators.{QueryInterpreter, PeeweeTranslator,
  DjangoTranslator, SQLAlchemyTranslator, SequelizeTranslator,
  ActiveRecordTranslator, UnsupportedException}
import cynthia.utils.{Utils, RUtils}


class TranslatorTest extends AnyFunSuite {
  // Add a seed to make tests deterministic.
  RUtils.seed(10342)

  def getQueryParams(q: Query) = q match {
    case SetRes(_) => (false, 0, None)
    case SubsetRes(offset, limit, _) => (false, offset, limit)
    case FirstRes(_) => (true, 0, None)
    case AggrRes(_, _) => (false, 0, None)
  }

  def getTranslator(name: String) = name match {
    case "peewee" => PeeweeTranslator(
      Target(Peewee("foo", "dir"), SQLite("db")))
    case "django" => DjangoTranslator(
      Target(Django("foo", "bar", "baz"), SQLite("db")))
    case "sqlalchemy" => SQLAlchemyTranslator(
      Target(SQLAlchemy("foo", "bar"), SQLite("db")))
    case "sequelize" => SequelizeTranslator(
      Target(Sequelize("foo", "bar"), SQLite("db")))
    case "activerecord" => ActiveRecordTranslator(
      Target(ActiveRecord("foo", "bar"), SQLite("db")))
    case _ => ???
  }

  def translateQuery(name: String, qid: Int, q: Query) = {
    val s = QueryInterpreter(q)
    val (first, offset, limit) = getQueryParams(q)
    val qStr = getTranslator(name).constructQuery(
      s, first, offset, limit).toString
    val expectedQStr = Source.fromFile(
      getClass.getResource(
        name + "/query_" + (qid + 1) + ".out").getFile()).mkString
    (qStr, expectedQStr)
  }

  TestQueries.queries.zipWithIndex.foreach {case (q, i) =>
    test("testing PeeweeTranslator in query " + (i + 1)) {
      val (qStr, expectedQStr) = translateQuery("peewee", i, q)
      assert(qStr == expectedQStr)
    }

    test("testing DjangoTranslator in query " + (i + 1)) {
      val (qStr, expectedQStr) = translateQuery("django", i, q)
      assert(qStr == expectedQStr)

    }

    test("testing SQLAlchemyTranslator in query " + (i + 1)) {
      val (qStr, expectedQStr) = translateQuery("sqlalchemy", i, q)
      assert(qStr == expectedQStr)
    }

    test("testing SequelizeTranslator in query " + (i + 1)) {
      val invalidQueries = List(
        1, 2, 4, 6, 10, 16, 20, 23, 24, 28, 29, 32, 34, 37,
        38, 42, 44, 45)

      if (invalidQueries.contains(i + 1)) {
        assertThrows[UnsupportedException] {
          translateQuery("sequelize", i, q)
        }
      } else {
        val (qStr, expectedQStr) = translateQuery("sequelize", i, q)
        assert(qStr == expectedQStr)
      }
    }

    test("testing ActiveRecordTranslator in query " + (i + 1)) {
      val invalidQueries = List(1, 2, 4, 6, 18, 19)
      if (invalidQueries.contains(i + 1)) {
        assertThrows[UnsupportedException] {
          translateQuery("activerecord", i, q)
        }
      } else {
        val (qStr, expectedQStr) = translateQuery("activerecord", i, q)
        assert(qStr == expectedQStr)
      }
    }
  }
}
