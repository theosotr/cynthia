package gr.dmst.aueb.cynthia

import java.io.File
import java.nio.file.{Files, Paths}


object Utils {
  val cwdDir = ".cynthia"

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

  def joinPaths(paths: List[String]) =
    paths match {
      case Nil    => ""
      case h :: t => t.foldLeft(h) { (acc, x) =>
        Paths.get(acc, x).normalize().toString()
      }
    }

  def getWorkdir() =
    joinPaths(List(".", cwdDir))

  def setWorkDir(): Unit = {
     val workdir = getWorkdir()
     val file = new File(workdir)
     if (!(file.exists && file.isDirectory)) {
       file.mkdirs()
     }
  }
}
