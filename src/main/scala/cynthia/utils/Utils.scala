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

package cynthia.utils

import java.io.{File, FileWriter, FileInputStream, FileOutputStream,
                BufferedWriter}
import java.nio.file.{Files, Paths}

import scala.reflect.io.Directory
import scala.io.Source
import scala.sys.process._

import me.tongfei.progressbar.{ProgressBarStyle, ProgressBarBuilder};
import spray.json._

import cynthia.lang.Query
import cynthia.lang.AQLJsonProtocol._


case class Str(str: String) {
  val buff: StringBuilder = new StringBuilder(str)

  def <<(obj: Object) = {
    buff.append(obj.toString)
    this
  }

  def ! =
    buff.toString

  override def toString() =
    buff.toString
}


object Utils {
  val cwdDir = ".cynthia"
  val dbDir = "dbs"
  val projectDir = "projects"
  val sessionDir = "sessions"
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
    val inputChannel = new FileInputStream(src).getChannel()
    val outputChannel = new FileOutputStream(dest).getChannel()
    outputChannel.transferFrom(inputChannel, 0, inputChannel.size())
    inputChannel.close()
    outputChannel.close()
  }

  def getWorkdir() =
    joinPaths(List(".", cwdDir))

  def getDBDir() =
    joinPaths(List(".", cwdDir, dbDir))

  def getProjectDir() =
    joinPaths(List(".", cwdDir, projectDir))

  def getSessionDir() =
    joinPaths(List(".", cwdDir, sessionDir))

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
     List(dbDir, projectDir, sessionDir, schemaDir)
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

  def loadQuery(path: String): Query =
    readFromFile(path).parseJson.convertTo[Query]

  def basenameWithoutExtension(x: String) =
      x.split("/").last.split("\\.(?=[^\\.]+$)").head

  def removeDups[T](l: Seq[T]): Seq[T] =
    l.foldLeft((Seq[T](), Set[T]())) { case ((acc, v), x) =>
      if (v.contains(x)) (acc, v)
      else (acc :+ x, v + x)
    }._1

  def escapeSQLStr(str: String): String =
    str.replace("\\", "\\\\").replace("'", "''")

  def escapeStr(str: String): String =
    str.replace("\\", "\\\\").replace("'", "\\'")

  def buildProgressBar(name: String, max: Option[Int]) = {
    val pbarBuilder = new ProgressBarBuilder()
      .setTaskName(name)
      .setUpdateIntervalMillis(50)
      .setStyle(ProgressBarStyle.ASCII)
    max match {
      case None => pbarBuilder.build()
      case Some(max) => pbarBuilder.setInitialMax(max).build()
    }
  }

  def encodeStr2Int(str: String) =
    str.toList.map(_.toInt).foldLeft(0)(_ + _)
}
