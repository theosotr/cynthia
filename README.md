# Cynthia: Data-Oriented Differential Testing of Object-Relational Mapping Systems

`Cynthia` is the first approach for systematically testing Object-Relational
Mapping (ORM) systems.
It leverages differential testing to establish
a test oracle for ORM-specific bugs.
Specifically, `Cynthia` first generates random
relational database schemas, sets up
the respective databases,
and then, queries
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

`Cynthia` currently supports the following popular ORM systems

* [Django](https://www.djangoproject.com/)
* [SQLAlchemy](https://www.sqlalchemy.org/)
* [peewee](http://docs.peewee-orm.com/en/latest/)
* [Sequelize](https://sequelize.org/)
* [ActiveRecord](https://guides.rubyonrails.org/active_record_basics.html)
* [Pony](https://ponyorm.org/) (unstable)


`Cynthia` is able to run ORM queries on top of the following Database
Management Systems

* [SQLite](https://www.sqlite.org/index.html)
* [MySQL](https://www.mysql.com/)
* [PostgreSQL](https://www.postgresql.org/)
* [SQL Server](https://www.microsoft.com/en-us/sql-server/sql-server-downloads)
* [CockroachDB](https://www.cockroachlabs.com/) (unstable)

You can cite `Cynthia` as follows.
Thodoris Sotiropoulos, Stefanos Chaliasos, Vaggelis Atlidakis, Dimitris Mitropoulos and Diomidis Spinellis.
Data-Oriented Differential Testing of Object-Relational Mapping Systems.
In _43rd International Conference on Software Engineering, ICSE '21_,
25–28 May 2021.

## Building

To build `Cynthia`, you must install
[sbt](https://www.scala-sbt.org/) which is the
build system for compiling the project.

```bash
echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
sudo apt update && sudo apt install sbt
```

`Cynthia` employs the [Z3 theorem prover](https://github.com/Z3Prover/z3)
for generating targeted data by solving the constraints of
the individual AQL queries.
To install Z3, follow the instructions below

```bash
git clone https://github.com/Z3Prover/z3
cd z3
python scripts/mk_make.py --java
cd build
make -j 8
sudo make install
cd ../..
rm -rf z3
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
export CYNTHIA_JAR_FILE=$(pwd)/target/scala-2.13/cynthia.jar
```

## Docker Image

`Cynthia` comes with a Docker Image that contains the required environment
(e.g., installations of ORMs and database servers) for testing ORMs.
To build the docker image from source, run

```bash
docker build -t cynthia .
```

Otherwise, you can pull our "pre-baked" image from the Docker registry

```bash
docker pull theosotr/cynthia
docker tag theosotr/cynthia cynthia
```


## Getting Started

To get started with `Cynthia`,
we will use the previously created Docker image
(namely, `cynthia`).
Recall that this image contains all the
required environment for testing ORMs
(i.e., it contains installations of the corresponding
ORM systems,
as well as installations
of the underlying database management systems).

You can enter a new container by using the following command

```bash
docker run -ti --rm cynthia
```

### Usage

The CLI of `Cynthia` provides six sub-commands; `test`, 
`generate`, `replay`, `run`, `inspect`, and `clean`.
Below, we explain each sub-command by providing a set
of examples and use cases.

```
cynthia@0fbedf262c3d:~$ cynthia --help
Cynthia version: 0.1
Usage: cynthia [test|generate|replay|run|inspect|clean] [options]

Cynthia: Data-Oriented Differential Testing of Object-Relational Mapping Systems

  --help                   Prints this usage text
  --version                Prints the version of Cynthia
Command: test [options]

  -n, --queries <value>    Number of queries to generate for each schema (default value: 200)
  -s, --schemas <value>    Number of schemas to generate (default value: 1)
  --timeout <value>        Timeout for testing in seconds
  -o, --orms <value>       ORMs to differentially test
                           (Available options: 'django', 'sqlalchemy', 'sequelize', 'peewee', 'activerecord', or 'pony')
  -d, --backends <value>   Database backends to store data
                           (Available options: 'sqlite', 'postgres', 'mysql',  'mssql', 'cockroachdb', default value: sqlite)
  -u, --db-user <value>    The username to log in the database
  -p, --db-pass <value>    The password used to log in the database
  -S, --store-matches      Save matches into the 'sessions' directory
  --combined               Generate AQL queries consting of other simpler queries
  -r, --records <value>    Number of records to generate for each table
  --min-depth <value>      Minimum depth of the generated AQL queries
  --max-depth <value>      Maximum depth of the generated AQL queries
  --no-well-typed          Generate AQL queries that are type incorrect
  --solver                 Generate database records through a solver-based approach
  --solver-timeout <value>
                           Solver timeout for each query
  --random-seed <value>    Make the testing procedure deterministic by giving a random seed
  --only-constrained-queries
                           Generate only constrained queries
Command: generate [options]

  -n, --queries <value>    Number of queries to generate for each schema (default value: 200)
  -s, --schemas <value>    Number of schemas to generate (Default value: 1)
  --combined               Generate AQL queries consting of other simpler queries
  -r, --records <value>    Number of records to generate for each table
  --min-depth <value>      Minimum depth of the generated AQL queries
  --max-depth <value>      Maximum depth of the generated AQL queries
  --no-well-typed          Generate AQL queries that are type incorrect
  --solver                 Generate database records through a solver-based approach
  --solver-timeout <value>
                           Solver timeout for each query
  --random-seed <value>    Make the testing procedure deterministic by giving a random seed
  --only-constrained-queries
                           Generate only constrained queries
Command: replay [options]

  -c, --cynthia <value>    The cynthia directory for replaying missmatches (default value: .cynthia)
  -s, --schema <value>     schema to replay
  -a, --all                Replay all queries.
  -m, --mismatches <value>
                           Replay queries for which ORM previously produced different results
  --generate-data          Re-generate data while replaying testing sessions
  -o, --orms <value>       ORMs to differentially test
                           (Available options: 'django', 'sqlalchemy', 'sequelize', 'peewee', 'activerecord', or 'pony')
  -d, --backends <value>   Database backends to store data
                           (Available options: 'sqlite', 'postgres', 'mysql',  'mssql', 'cockroachdb', default value: sqlite)
  -u, --db-user <value>    The username to log in the database
  -p, --db-pass <value>    The password used to log in the database
  -r, --records <value>    Number of records to generate for each table
  --solver                 Generate database records through a solver-based approach
  --solver-timeout <value>
                           Solver timeout for each query
  --random-seed <value>    Make the testing procedure deterministic by giving a random seed
Command: run [options]

  -s, --sql <value>        File with the sql script to generate and feed the database
  -a, --aql <value>        A file with an AQL query or a directory with many AQL queries
  -o, --orms <value>       ORMs to differentially test
                           (Available options: 'django', 'sqlalchemy', 'sequelize', 'peewee', 'activerecord', or 'pony')
  -d, --backends <value>   Database backends to store data
                           (Available options: 'sqlite', 'postgres', 'mysql',  'mssql', 'cockroachdb', default value: sqlite)
  -u, --db-user <value>    The username to log in the database
  -p, --db-pass <value>    The password used to log in the database
  -S, --store-matches      Save matches into the 'sessions' directory
Command: inspect [options]

  -c, --cynthia <value>    The cynthia directory for inspecting missmatches (default value: .cynthia)
  -s, --schema <value>     schema to inspect
  -m, --mismatches <value>
                           mismatches to inspect
Command: clean [options]

  --only-workdir           Clean only the working directory '.cynthia'
  -u, --db-user <value>    The username to log in the database
  -p, --db-pass <value>    The password used to log in the database

```

### cynthia test

This is the main sub-command for testing ORMs.
`cynthia test` expects at least two ORMs to test
(through the `--orms` option),
and some database systems (i.e., backends) specified
by the `--backends` options.
Note that if `--backends` is not given,
the `SQLite` database system is used by default.

`cynthia test` first generates a number of relational database
schemas
The number of the generated schemas is
specified by the `--schemas` option.
Every generated schema corresponds to a *testing session*.
In every testing session, `cynthia test` generates a number of
random AQL queries (given by the `--queries` option),
translates every AQL query into the corresponding executable ORM query,
and finally runs every ORM query on the given backends.
Note that for a given AQL query,
`Cynthia` generates multiple ORM queries,
one for every backend.

#### Example

In the following scenario,
we differentially test the `peewee` and `Django` ORMs.
The ORM queries are run
on top of the `SQLite` and `PostgreSQL` databases,
and we spawn 5 testing sessions (`--schemas 5`).
In every testing session, we generate 100 AQL queries
(`--queries 100`).
To populate the underlying databases with data,
we use the Z3 solver (`--solver`) to generate five
records (`--records 5`) by solving the constraints
of every generated AQL query.
Finally, the option `--store-matches` is used to store the information
coming from all AQL query runs inside the `.cynthia/sessions/`
directory (see below).
If this option is not provided, `Cynthia`
stores only the AQL queries for which the ORMs under test produced
different results.

```bash
cynthia@0fbedf262c3d:~$ cynthia test \
  --schemas 5 \
  --queries 100 \
  --orms django,peewee \
  --backends postgres \
  --solver \
  --records 5 \
  --random-seed 1 \
  --store-matches
```

The above command will produce an output similar to the following

```
Testing Serially 100% [========================= Passed ✔: 95, Failed ✘: 0, Unsp: 5, Timeouts: 1
Testing Cucumbers 100% [======================== Passed ✔: 98, Failed ✘: 0, Unsp: 2, Timeouts: 3
Testing Mumbles 100% [========================== Passed ✔: 97, Failed ✘: 0, Unsp: 3, Timeouts: 3
Testing Subhead 100% [========================== Passed ✔: 98, Failed ✘: 0, Unsp: 2, Timeouts: 2
Testing Wild 100% [============================= Passed ✔: 96, Failed ✘: 0, Unsp: 4, Timeouts: 0
```

Note that `Cynthia` processes testing sessions in parallel by using the Scala
futures. `Cynthia` also dumps some statistics for every testing session.

```
Testing Cucumbers 100% [======================== Passed ✔: 98, Failed ✘: 0, Unsp: 2, Timeouts: 3
```

For example, the above message
means that in the testing session named `Cucumbers`,
`Cynthia` generated 100 AQL queries of which

* 98 / 100 queries passed (i.e., the ORMs under test produced exact results).
* 0 / 100 queries failed (i.e., the ORMs under test produced different results).
  Note that failed queries indicate a potential bug
  in at least one of the ORMs under test.
* 2 / 100 queries were unsupported meaning that the ORMs were unable to execute
  these queries, because these queries contained features
  that are not currently supported by the ORMs under test.
* 3 / 100 queries timed out, i.e., the SMT solver timed out and failed to
  generate records for populating the underlying databases.

**NOTE**: When solver times out, `Cynthia` still tests the ORM implementations
under test against the generated AQL query. However this time, the underlying
database contains the data stemming from the previous AQL query,
as the solver did not manage to generate records that satisfy the constraints
of the current AQL query in a reasonable time limit. This is why the number of
`Passed` + `Failed` + `Unsp` + `Timeouts` > 100.


#### The .cynthia working directory

`Cynthia` also produces a directory named `.cynthia`
(inside the current working directory)
where it stores important information about each run.
The `.cynthia` directory has the following structure.

* `.cynthia/cynthia.log`: A file containing logs associated with the current
  `cynthia` run.
* `.cynthia/dbs/`: This is the directory where the `SQLite` database files
  of each testing session are stored.
* `.cynthia/schemas/`: This directory contains SQL scripts corresponding to
the database schema of each testing session. Every SQL script contains
all the necessary `CREATE TABLE` statements for creating the relational
tables defined in each schema.
* `.cynthia/projects/`: A directory where `Cynthia` creates and executes
the corresponding ORM queries.
* `.cynthia/sessions/`: This directory contains complete information about
the runs taken place at every testing session. 

In particular, by inspecting the structure of the `.cynthia/sessions/`
directory, we have the following

* `.cynthia/sessions/<Session Name>/<Query ID>/data.sql`:
  This is the SQL script that populates the underlying database
  with data generated by the SMT solver. Note that these data are targeted,
  meaning that satisfy the constraints of the query identified by `<Query ID>`. 
* `.cynthia/sessions/<Session Name>/<Query ID>/diff_test.out`:
  The output of differential testing. Either "MATCH", "MISMATCH", or "UNSUPPORTED".
* `.cynthia/sessions/<Session Name>/<Query ID>/<orm>_<backend>.out`:
  The output produced by the query written by using the API of ORM named `<orm>`,
  and when this query is run on the backend `<backend>`.
* `.cynthia/sessions/<Session Name>/<Query ID>/query.aql`:
  The AQL query in human readable format.
* `.cynthia/sessions/<Session Name>/<Query ID>/query.aql.json`:
  The AQL query in JSON format. This is the format that `Cynthia` expects
  as input when replaying an AQL query.
* `.cynthia/sessions/<Session Name>/<Query ID>/<orm>/`:
  This directory contains all ORM-specific files for executing
  the ORM queries written in ORM named `<orm>`.
  For example, by executing

  ```bash
  python .cynthia/sessions/Cucumbers/22/django/driver_postgres.py
  ```

  You re-execute the Django query stemming from the AQL query
  with id `22`. This query is run on the PostgresSQL database.


### cynthia replay

This sub-command replays the execution of a particular testing
session based on information extracted from the `.cynthia`
directory. This command is particularly useful when we want
to run the same queries with different settings
(i.e., running the same AQL queries on different database systems).

#### Examples

Replay all testing sessions previously created by `cynthia test`

```bash
cynthia@0fbedf262c3d:~$ cynthia replay \
  --orms django,peewee \
  --backends postgres \
  --all
```

This produces the exact results as `cynthia test`

```
Replaying Serially 100% [======================= Passed ✔: 95, Failed ✘: 0, Unsp: 5, Timeouts: 0
Replaying Cucumbers 100% [====================== Passed ✔: 98, Failed ✘: 0, Unsp: 2, Timeouts: 0
Replaying Subhead 100% [======================== Passed ✔: 98, Failed ✘: 0, Unsp: 2, Timeouts: 0
Replaying Mumbles 100% [======================== Passed ✔: 97, Failed ✘: 0, Unsp: 3, Timeouts: 0
Replaying Wild 100% [=========================== Passed ✔: 96, Failed ✘: 0, Unsp: 4, Timeouts: 0
Command replay finished successfully.

```

Replay the execution of a specific testing session

```bash
cynthia@0fbedf262c3d:~$ cynthia replay \
  --schema Cucumbers \
  --orms django,peewee \
  --backends postgres \
  --all
```

This produces

```
Replaying Cucumbers 100% [====================== Passed ✔: 98, Failed ✘: 0, Unsp: 2, Timeouts: 0
Command replay finished successfully.
```

Replay the execution of a specific testing session, and run ORM queries
on MySQL instead of Postgres.

```bash
cynthia@0fbedf262c3d:~$ cynthia replay \
  --schema Cucumbers \
  --orms django,peewee \
  --backends mysql \
  --all
```

Replay the execution of a specific testing session, and differentially
test `SQLAlchemy` and `Sequelize` instead of `Django` and `peeewee`.


```bash
cynthia@0fbedf262c3d:~$ cynthia replay \
  --schema Cucumbers \
  --orms sqlalchemy,sequelize \
  --all
```

### cynthia run

The sub-command `cynthia run` tests the given ORMs against
certain AQL queries (provided by the user)
based on a given database schema,
which is also provided by the user (not generated by `Cynthia`).


#### Example

The command below, tests `Django` and `peewee` against the AQL query
located in the directory `cynthia_src/examples/books/10.aql.json`
and the database schema defined
by the script `cynthia_src/examples/book.sql`.
The underlying database system is `SQLite`.

```bash
cynthia@0fbedf262c3d:~$ cynthia run \
  --sql cynthia_src/examples/books.sql \
  --aql cynthia_src/examples/books/10.aql.json \
  --orms django,peewee \
  --store-matches
```

This produces

```
Running books 100% [========================== Passed ✔: 1, Failed ✘: 0, Unsp: 0, Timeouts: 0
Command run finished successfully.
```


### cynthia generate

`cynthia generate` generates a number of relational database schema,
and for each database schema, it produces a number of AQL queries
and data.
This command does *not* test ORMs, i.e., it does not translate the
generated AQL queries into concrete ORM queries.
Every generated query is stored
inside the `.cynthia/sessions/` directory.

In order to test ORMs, the queries generated by `cynthia generate`
can be later executed using the `cynthia replay`
command as documented above.


#### Example

Generate 5 random database schema. For every schema, generate 100 AQL queries.
The generated data are generated by a solver-based approach,
and each table contains 5 records.

```bash
cynthia@0fbedf262c3d:~$ cynthia generate \
 --schemas 5 \
 --queries 100 \
 --records 5 \
 --solver
```

### cynthia inspect

This helper sub-command inspects the results, and reports
the queries for which the ORMs under test produced different results.
To do so, `cynthia inspect` extracts information
from the `.cynthia` directory.

### Example

Inspect the testing session named `Cucumbers`.

```bash
cynthia@0fbedf262c3d:~$ cynthia inspect --schema Cucumbers
```

This produces

```
Session: Cucumbers
  Crashes:
  Mismatches:
   * 71[sqlite]:
     - django,sqlalchemy,peewee
     - sequelize
   * 17[sqlite]:
     - sequelize
     - django,sqlalchemy,peewee
   * 73[sqlite]:
     - django,sqlalchemy,peewee
     - sequelize
==================================
Command inspect finished successfully.
```

The output above indicates that in three AQL queries
(namely, 17, 71, and 73), the ORMs under test produced
different results. Specifically,
in all queries, the `Sequelize` ORM produced different results
from those produced by the `Django`, `peewee`, and `SQLAlchemy`
ORMs.

You can verify this by inspecting the corresponding ORM outputs
from the `.cynthia` directory

```bash
cynthia@0fbedf262c3d:~$ diff .cynthia/sessions/Cucumbers/73/sequelize_sqlite.out \
  .cynthia/sessions/Cucumbers/73/django_sqlite.out
```

This gives

```diff
0a1,2
> _default -1.00
> _default 2.00
\ No newline at end of file
```

### cynthia clean

`cynthia clean` simply cleans the `.cynthia` directory
and all the tables and databases created
by `Cynthia` in the underlying servers.
This command is safe, as each database created by `Cynthia`
has the prefix `'cynthia_'`, so `cynthia clean` does not remove
any user-defined table or database.

```bash
cynthia@8461308e0af8:~$ cynthia clean
Cleaning working directory .cynthia...
Cleaning backend mysql...
Cleaning backend mssql...
Cleaning backend postgres...
Cleaned database mssql successfully
Cleaned database mysql successfully
Cleaned database postgres successfully
Command clean finished successfully.
```

## Extending Cynthia

You can extend `Cynthia` by implementing
support for a new ORM system.
To do so, you have to follow the next steps.

### 1. Adding ORM case class

As first step, you need to create a case class for
the new ORM inside the `cynthia.targets.ORMs.scala` file.
This case class needs to inherit from the
abstract class `ORM` and implement the
following abstract methods.
You can consult the existing ORM case classes
for more details.

```scala

sealed abstract class ORM(
    /** The name of ORM. */
    val ormName: String,

    /**
     * The name of the project that relies on this ORM.
     * Typically, the name of the project corresponds to
     * the name of the current testing session.
     */
    val projectName: String,

    /** The directory where the ORM project is located. */
    val projectDir: String
) {

  /**
   * Get the path where the file containing the models of
   * the project is located.
   */
  def getModelsPath(): String

  /**
   * Get the path where the file containing the settings of the
   * project is located.
   */
  def getSettingsPath(): String

  /**
   * Get the path where the executable ORM query, which runs on top of the
   * given database, is located.
   */
  def getDriverPath(db: DB): String
}
```

### 2. Extending ProjectCreator to support the new ORM

The next step is to extend the `cynthia.targets.ProjectCreator.scala`
class.
In particular, you need to extend the following two methods:

```scala
setupProject(orm: ORM, dbs: Seq[DB]): Unit
createModels(orm: ORM, dbs: Seq[DB]): Unit
```

The first method (`setupProject()`) is used to create all
the necessary ORM-specific directories
and files in order to set up a new project
that relies on the ORM.

The second method (`createModels()`) creates a file containing
the model classes using the API of the given ORM.
As you notice, the existing implementation use external tools
for automatically deriving the models classes from an existing
database. For example, to generate the `SQLAlchemy` models,
we use a tool called `sqlacodegen`. 
Therefore, you have to use an external tool
(if such a tool does not exist for your ORM, you have to implement your own)
that expects a database
connection and generates the ORM models automatically.


### 3. Implementing a translator for the new ORM

As a final step, you have to implement a translator
for your ORM inside the `cynthia.translators` package.
This translator is responsible for translating an AQL query into
a concrete, executable query,
and needs to inherit from the
`cynthia.translators.Translator` base class.
Your translator has to provide implementations
for the following

#### 1

```scala
val preamble: String
```

This string contains all the boilerplate code needed for executing
an ORM query (e.g., necessary imports, creating connection with the database,
etc.).

#### 2
```scala
  def constructNaiveQuery(
      s: State,
      first: Boolean,
      offset: Int,
      limit: Option[Int]
  ): QueryStr
```

This translates the state of an AQL query into a `QueryStr`.

#### 3

```scala
def constructCombinedQuery(s: State): QueryStr
```

The above translates the state coming a combined AQL query
(which consists of other simpler AQL queries)
into a `QueryStr`.

#### 4

```scala
def unionQueries(s1: State, s2: State): State
```

The above handles the union of two states. Every state stems from a specific
AQL query.

#### 5

```scala
def intersectQueries(s1: State, s2: State): State
```
This handles the intersection of two states. Every state stems from a specific
AQL query.


#### 6

```scala
def emitPrint(q: Query, dFields: Seq[String], ret: String): String
```

The method above is responsible for dumping
the results of the given query to standard output, by using the API
of the ORM.
The variable `dFields` gives the names of the fields that you need to print
to the standard output, while the variable `ret` stands for the variable
holding the results of the query.
