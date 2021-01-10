# Cynthia: Data-Oriented Differential Testing of Object-Relational Mapping Systems

`Cynthia` is the first approach for systematically testing Object-Relational
Mapping (ORM) systems.
It leverages differential testing to establish
a test oracle for ORM-specific bugs.
Specifically, `Cynthia` first generates random
relational database schemas, sets up
the respective databases,
and then, it queries
these databases using the APIs of the ORM systems under test.
To tackle the challenge that ORMs lack a common input language,
`Cynthia` generates queries  written in an abstract query language
(AQL).
These abstract queries are translated into
concrete, executable ORM queries,
which are ultimately used to differentially test 
the correctness of target implementations.

The effectiveness of `Cynthia` heavily relies on
the data inserted to the underlying  databases.
Therefore,  `Cynthia` adopts a
solver-based  approach for producing targeted
database records with respect to the
constraints of the generated queries.

`Cynthia` currently supports the following popular ORM systems.

* [Django](https://www.djangoproject.com/)
* [SQLAlchemy](https://www.sqlalchemy.org/)
* [peewee](http://docs.peewee-orm.com/en/latest/)
* [Sequelize](https://sequelize.org/)
* [ActiveRecord](https://guides.rubyonrails.org/active_record_basics.html)
* [Pony](https://ponyorm.org/) (unstable)


`Cynthia` can run the ORMs above on the following Database Systems.

* [SQLite](https://www.sqlite.org/index.html)
* [MySQL](https://www.mysql.com/)
* [PostgreSQL](https://www.postgresql.org/)
* [SQL Server](https://www.microsoft.com/en-us/sql-server/sql-server-downloads)
* [CockroachDB](https://www.cockroachlabs.com/)


## Building

To build `Cynthia`, you must have installed the Scala programming language.

```bash
sudo apt install scala
```

Install [sbt](https://www.scala-sbt.org/) which is the
build system for compiling the project.

```bash
echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
sudo apt update && sudo apt install sbt
```

Finally, to build `Cynthia` run

```bash
make
sudo make install
```

**NOTE**: To successfully run `Cynthia`, you need to specify
the environment variable `CYNTHIA_JAR_FILE` that points to the
`JAR` file created by `sbt`.

```bash
export CYNTHIA_JAR_FILE=$(pwd)/target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar
```

## Docker Image

`Cynthia` comes with a Docker Image that contains the required environment
(e.g., installations of ORMs and database servers) for testing ORMs.
To build the docker image from source, run

```bash
sudo docker build -t cynthia .
```

Otherwise, you can pull our "pre-baked" image from the Docker registry

```bash
sudo docker pull <add docker image>
```


## Usage

The CLI of `Cynthia` provides six sub-commands; `test`, 
`generate`, `replay`, `run`, `inspect`, and `clean`.

```
❯ cynthia
Error: A sub-command is required.
cynthia 0.1
Usage: cynthia [test|generate|replay|run|inspect|clean]



Command: test [options]

  -n, --queries <value>    Number of queries to generate for each schema (Default value: 200)
  -s, --schemas <value>    Number of schemas to generate (Default value: 1)
  --timeout <value>        Timeout for testing in seconds
  -o, --orms <value>       ORMs to differentially test
  -d, --backends <value>   Database backends to store data (Default Value: sqlite)
  -S, --store-matches      Save matches
  --no-combined            Don't generate combined queries
  -r, --records <value>    Number of records to generate for each table
  --min-depth <value>      Minimum depth of generated AQL queries
  --max-depth <value>      Maximum depth of generated AQL queries
  --well-typed             Generate well-typed queries
  --solver                 Generate database records through a solver-based approach
  --solver-timeout <value>
                           Solver timeout for each query
Command: generate [options]

  -n, --queries <value>    Number of queries to generate for each schema (Default value: 200)
  -s, --schemas <value>    Number of schemas to generate (Default value: 1)
  --no-combined            Don't generate combined queries
  -r, --records <value>    Number of records to generate for each table
  --min-depth <value>      Minimum depth of generated AQL queries
  --max-depth <value>      Maximum depth of generated AQL queries
  --well-typed             Generate well-typed queries
  --solver                 Generate database records through a solver-based approach
  --solver-timeout <value>
                           Solver timeout for each query
Command: replay [options]

  -c, --cynthia <value>    cynthia directory for replaying missmatches (default .cynthia)
  -s, --schema <value>     schema to replay
  -a, --all                Replay all queries. Always use it with --store-matches to not remove matches queries
  -m, --mismatches <value>
                           Mismatches to replay
  -o, --orms <value>       ORMs to differentially test
  -d, --backends <value>   Database backends to store data (Default Value: sqlite)
Command: run [options]

  -s, --sql <value>        File with the sql script to generate and feed the Database
  -a, --aql <value>        A file with an AQL query or a directory with many AQL queries
  -o, --orms <value>       ORMs to differentially test
  -d, --backends <value>   Database backends to store data (Default Value: sqlite)
  -S, --store-matches      Save matches
Command: inspect [options]

  -c, --cynthia <value>    cynthia directory for inspecting missmatches (default .cynthia)
  -s, --schema <value>     schema to inspect
  -m, --mismatches <value>
                           mismatches to inspect
Command: clean [options]

  --only-workdir           Clean only the working directory .cynthia

```

## Run Cynthia
