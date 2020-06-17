package gr.dmst.aueb.cynthia

import scala.util.{Success, Failure}
import scopt.OParser
import gr.dmst.aueb.cynthia.translators.SchemaTranslator


case class Options (
  schemas: String = "",
  orms: Seq[String] = Seq(),
  dbs: Seq[String] = Seq("sqlite")
)


object Cynthia {

  val listingModel = Model("Listing", Seq(
    Field("id", Serial),
    Field("yearly_rent", Numeric),
    Field("sale_price", Numeric)
  ))

  var authorModel = Model("Author", Seq(
    Field("id", Serial),
    Field("first_name", VarChar(50)),
    Field("surname", VarChar(50))
  ))

  val bookModel = Model("Book", Seq(
    Field("id", Serial),
    Field("title", VarChar(100)),
    Field("isbn", VarChar(100)),
    Field("author", Foreign("Author"))
  ))

  val reviewModel = Model("Review", Seq(
    Field("id", Serial),
    Field("reviewer_name", VarChar(255)),
    Field("content", VarChar(255)),
    Field("rating", Int16),
    Field("book", Foreign("Book"))
  ))

  val listingSchema = Schema("listing", Map("Listing" -> listingModel))
  val bookSchema = Schema("book", Map(
    "Author" -> authorModel,
    "Book" -> bookModel,
    "Review" -> reviewModel
  ))

  def genSchemas() =
    List(bookSchema, listingSchema)

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
          .text("Path to database schemas"),
        opt[Seq[String]]('o', "orms")
          .required()
          .action((x, o) => o.copy(orms = x))
          .validate(_.foldLeft (success) { (acc, x) => x match {
              case "django"  | "sqlalchemy" | "sequelize"
              | "peewee" | "activerecord" => acc
              case _  => failure("ORM '" + x + "' is not supported")
            }
          })
          .text("ORMs to differentially test"),
        opt[Seq[String]]('d', "backends")
          .action((x, o) => o.copy(dbs = o.dbs ++ x))
          .validate(_.foldLeft (success) { (acc, x) => x match {
              case "mysql" | "postgres" => acc
              case "sqlite"             => failure ("SQLite is used by default")
              case _                    => failure ("Database backend '" + x + "' is not supported")
            }
          })
          .text("Database backends to store data"),
        checkConfig(x =>
          if (x.dbs.length + x.orms.length < 2)
            failure(
              "Number of database backends + number of ORMs must be greather than 1")
          else success
        )
      )
    }

    OParser.parse(cliParser, args, Options()) match {
      case Some(options) => {
        Utils.setWorkDir()
        genSchemas map { s => {
          Utils.writeToFile(s.getSchemaPath, SchemaTranslator(s))
          TestRunnerCreator(options, s) match {
            case Success(testRunner) => testRunner.start()
            case Failure(e)          => println(e.getMessage)
          }
        }}
      }
      case _ => println("Wrong arguments")
    }
  }
}
