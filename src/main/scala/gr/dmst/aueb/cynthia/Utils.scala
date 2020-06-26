package gr.dmst.aueb.cynthia

import java.io.{File, FileWriter, FileInputStream, FileOutputStream}
import java.nio.file.{Files, Paths}
import java.io.{BufferedWriter, FileWriter}
import scala.reflect.io.Directory
import scala.io.Source
import scala.sys.process._

import spray.json._

import gr.dmst.aueb.cynthia.serializers.AQLJsonProtocol._

case class Str(str: String) {
  val buff: StringBuilder = new StringBuilder(str)

  def <<(obj: Object) = {
    buff.append(obj.toString)
    this
  }

  def !() =
    buff.toString

  override def toString() =
    buff.toString
}


object Utils {
  val cwdDir = ".cynthia"
  val dbDir = "dbs"
  val projectDir = "projects"
  val reportDir = "report"
  val schemaDir = "schemas"

  def exists(file: String) =
    new File(file).exists

  def listFiles(dir: String, ext: String = ".sql"): Option[Array[String]] = {
    val file = new File(dir)
    if (file.isDirectory)
      Some (
        file.listFiles
          .filter(x => x.getName().endsWith(ext))
          .map (x => x.getName())
      )
    else None
  }

  def emptyFile(dir: String) = {
    val file = new File(dir)
    if (file.isDirectory)
      file.listFiles
        .foreach { file =>
          if (file.isDirectory) new Directory(file).deleteRecursively()
          else file.delete()
        }
    else if (file.exists) file.delete()
  }

  def joinPaths(paths: List[String]) =
    paths match {
      case Nil    => ""
      case h :: t => t.foldLeft(h) { (acc, x) =>
        Paths.get(acc, x).normalize().toAbsolutePath().toString()
      }
    }

  def appendToFile(path: String, str: String) = {
    val fw = new FileWriter(path, true)
    try { fw.write(str) }
    finally { fw.close() }
  }

  def writeToFile(path: String, str: String) = {
    val fw = new FileWriter(path, false)
    try { fw.write(str) }
    finally { fw.close() }
  }

  def readFromFile(path: String) =
    new String(Files.readAllBytes(Paths.get(path)))

  def writeJson(path: String, q: Query) = {
    val json = q.toJson
    val w = new BufferedWriter(new FileWriter(path))
    w.write(json.prettyPrint)
    w.close
  }

  def getListOfFiles(dir: String): List[String] = {
    new File(dir).listFiles.filter(_.isFile)
      .map(_.getPath).toList
  }

  def getListOfDirs(dir: String): List[String] = {
    if (Files.exists(Paths.get(dir))) {
      new File(dir).listFiles.filter(_.isDirectory)
        .map(_.getPath).toList
    } else {
      List[String]()
    }
  }

  def copyFile(src: String, dest: String) = {
    val inputChannel = new FileInputStream(src).getChannel();
    val outputChannel = new FileOutputStream(dest).getChannel();
    outputChannel.transferFrom(inputChannel, 0, inputChannel.size());
    inputChannel.close();
    outputChannel.close();
  }

  def getWorkdir() =
    joinPaths(List(".", cwdDir))

  def getDBDir() =
    joinPaths(List(".", cwdDir, dbDir))

  def getProjectDir() =
    joinPaths(List(".", cwdDir, projectDir))

  def getReportDir() =
    joinPaths(List(".", cwdDir, reportDir))

  def getSchemaDir() =
    joinPaths(List(".", cwdDir, schemaDir))

  def createDir(dir: String) = {
    val file = new File(dir)
    if (!(file.exists && file.isDirectory)) {
      file.mkdirs()
    }
  }

  def setWorkDir() = {
     val workdir = getWorkdir()
     List(dbDir, projectDir, reportDir, schemaDir)
       .foreach { dir => {
           val fdir = Utils.joinPaths(List(workdir, dir))
           createDir(fdir)
         }
       }
  }

  def deleteRecursively(file: File): Unit = {
    if (file.isDirectory) {
      file.listFiles.foreach(deleteRecursively)
    }
    if (file.exists && !file.delete) {
      throw new Exception(s"Unable to delete ${file.getAbsolutePath}")
    }
  }

  def runCmd(cmd: String, dir: Option[String]): String = dir match {
    case None      => cmd.!!
    case Some(dir) => Process(cmd, new File(dir)).!!
  }

  def quoteStr(str: String, quotes: String = "'") =
    quotes + str + quotes

  def mergeMap[T1, T2](m1: Map[T1, Set[T2]], m2: Map[T1, Set[T2]]): Map[T1, Set[T2]] = {
    val grouped = (m1.toSeq ++ m2.toSeq).groupBy(_._1)
    grouped.view.mapValues(_.foldLeft(Set[T2]()) { (acc, x) => acc ++ x._2 }).toMap
  }

  def topologicalSort(g: Map[String, Set[String]]): Seq[String] = {
    def dfs(acc: (Set[String], Seq[String]), n: String): (Set[String], Seq[String]) = {
      val (v, o) = acc
      g get n match {
        case None    => (v + n, o :+ n)
        case Some(e) =>
          if (e.isEmpty)
            (v + n, o :+ n)
          else {
            val (v2, o2) = e.foldLeft((v + n, o)) { (acc, v) =>
              if (acc._1.contains(v)) acc
              else dfs(acc, v)
            }
            // revisit: non tail recursive
            (v2, o2 :+ n)
          }
      }
    }

    g.keys.foldLeft((Set[String](), Seq[String]())) { (acc, n) =>
      // Node is already visited
      if (acc._1.contains(n)) acc
      else dfs(acc, n)
    }._2
  }
}
