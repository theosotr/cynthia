package gr.dmst.aueb.cynthia

import java.io.{File, FileWriter}
import java.nio.file.{Files, Paths}
import scala.reflect.io.Directory
import scala.sys.process._


object Utils {
  val cwdDir = ".cynthia"
  val dbDir = "dbs"
  val projectDir = "projects"

  def exists(file: String) =
    new File(file).exists

  def listFiles(dir: String): Option[Array[String]] = {
    val file = new File(dir)
    if (file.isDirectory)
      Some (
        file.listFiles
          .filter(x => x.getName().endsWith(".sql"))
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

  def getWorkdir() =
    joinPaths(List(".", cwdDir))

  def getDBDir() =
    joinPaths(List(".", cwdDir, dbDir))

  def getProjectDir() =
    joinPaths(List(".", cwdDir, projectDir))

  def createDir(dir: String) = {
    val file = new File(dir)
    if (!(file.exists && file.isDirectory)) {
      file.mkdirs()
    }
  }

  def setWorkDir() = {
     val workdir = getWorkdir()
     List(dbDir, projectDir)
       .foreach { dir => createDir(Utils.joinPaths(List(workdir, dir))) }
  }

  def runCmd(cmd: String, dir: Option[String]): String = dir match {
    case None      => cmd.!!
    case Some(dir) => Process(cmd, new File(dir)).!!
  }

  def quoteStr(str: String, quotes: String = "'") =
    quotes + str + quotes
}
