import java.io.File
import java.nio.file.Paths

import org.scalatest.funsuite.AnyFunSuite

import cynthia.utils.Utils


class UtilsTest extends AnyFunSuite {
  val cwd = Paths.get(".").normalize().toAbsolutePath.toString
  val sep = File.separator

  test("check normalize") {
    val paths = List("foo", "bar/bar", "/baz")
    val expected = cwd + sep + "foo" + sep + "bar" + sep + "bar" + sep + "baz"
    assert(Utils.joinPaths(paths) == expected)
  }

  test("check .cynthia workdir") {
    assert(Utils.getWorkdir() == cwd + sep + Utils.cwdDir)
  }

  test("check DB dir") {
    assert(Utils.getDBDir() == cwd + sep + Utils.cwdDir + sep + Utils.dbDir)
  }
  
  test("check project dir") {
    assert(Utils.getProjectDir() == cwd + sep + Utils.cwdDir
      + sep + Utils.projectDir)
  }

  test("check schema dir") {
    assert(Utils.getSchemaDir() == cwd + sep + Utils.cwdDir
      + sep + Utils.schemaDir)
  }

  test("check session dir") {
    assert(Utils.getSessionDir() == cwd + sep + Utils.cwdDir 
      + sep + Utils.sessionDir)
  }

  test("merging two maps with different keys") {
    val m1 = Map(
      "k1" -> Set("1", "2"),
      "k2" -> Set("1") 
    )
    val m2 = Map(
      "k3" -> Set[String](),
      "k4" -> Set("f")
    )
    val expected = Map(
      "k1" -> Set("1", "2"),
      "k2" -> Set("1"), 
      "k3" -> Set[String](),
      "k4" -> Set("f")
    )
    val m3 = Utils.mergeMap(m1, m2)
    assert(m3 == expected)
  }

  test("merging a map with an empty one") {
    val m1 = Map[String, Set[String]]()
    val m2 = Map(
      "k1" -> Set("1", "2"),
      "k2" -> Set("1") 
    )
    val expected = Map(
      "k1" -> Set("1", "2"),
      "k2" -> Set("1"), 
    )
    val m3 = Utils.mergeMap(m1, m2)
    assert(m3 == expected)
  }

  test("merging two maps whose keys overlap") {
    val m1 = Map(
      "k1" -> Set("1", "2"),
      "k2" -> Set("1") 
    )
    val m2 = Map(
      "k2" -> Set[String]("2"),
    )
    val expected = Map(
      "k1" -> Set("1", "2"),
      "k2" -> Set("1", "2"), 
    )
    val m3 = Utils.mergeMap(m1, m2)
    assert(m3 == expected)
  }

  test("test topological sort implementation") {
    var m = Map[String, Set[String]]()
    assert(Utils.topologicalSort(m) == Seq())

    m = Map(
      "k1" -> Set("k2"),
      "k2" -> Set("k3"),
      "k3" -> Set()
    )
    assert(Utils.topologicalSort(m) == Seq("k3", "k2", "k1"))

    m = Map(
      "k1" -> Set("k3", "k2"),
      "k2" -> Set("k3"),
      "k3" -> Set("k4"),
      "k4" -> Set()
    )
    assert(Utils.topologicalSort(m) == Seq("k4", "k3", "k2", "k1"))
  }

  test("strip the extension of a file name") {
    assert(Utils.basenameWithoutExtension("foo" + sep + "bar.txt") == "bar")
    assert(Utils.basenameWithoutExtension("foo" + sep + "bar") == "bar")
    assert(Utils.basenameWithoutExtension("bar.txt") == "bar")
    assert(Utils.basenameWithoutExtension("bar") == "bar")
    assert(Utils.basenameWithoutExtension("bar.txt.d") == "bar.txt")
  }

  test("removing duplicates from a sequence") {
    assert(Utils.removeDups(Seq()) == Seq())
    assert(Utils.removeDups(Seq(1, 2, 3)) == Seq(1, 2, 3))
    assert(Utils.removeDups(Seq(1, 2, 1, 3)) == Seq(1, 2, 3))
    assert(Utils.removeDups(Seq(1, 2, 1, 3, 3, 1)) == Seq(1, 2, 3))
  }

  test("escaping an SQL string") {
    assert(Utils.escapeSQLStr("foo") == "foo")
    assert(Utils.escapeSQLStr("'foo'") == "''foo''")
    assert(Utils.escapeSQLStr("'fo\\o'") == "''fo\\\\o''")
  }

  test("escaping a string") {
    assert(Utils.escapeStr("foo") == "foo")
    assert(Utils.escapeStr("'foo'") == "\\'foo\\'")
    assert(Utils.escapeStr("'fo\\o'") == "\\'fo\\\\o\\'")
  }
}
