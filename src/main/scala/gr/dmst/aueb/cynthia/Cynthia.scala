package gr.dmst.aueb.cynthia

import scopt.OParser


case class Options (
  schemas: String = ""
)


object Cynthia {
  def main(args: Array[String]): Unit = {
    val builder = OParser.builder[Options]
    val cliParser = {
      import builder._
      OParser.sequence(
        programName("cynthia"),
        head("cynthia", "0.1"),
        opt[String]('s', "schemas")
          .required()
          .action((x, o) => o.copy(schemas = x))
          .text("Path to database schemas")
      )
    }

    OParser.parse(cliParser, args, Options()) match {
      case Some(options) => println(options.schemas)
      case             _ => println("Wrong arguments")
    }
  }
}
