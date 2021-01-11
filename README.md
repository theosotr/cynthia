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


## Getting Started

To get started with `Cynthia`,
we will use the previously created Docker image
(i.e., `Cynthia`) as this image contains all the
required environment for testing ORMs
(i.e., it contains installations of the corresponding
ORMs and the underlying database management systems).

You can enter a new container by using the following command

```bash
docker run -ti cynthia
```

### Usage

The CLI of `Cynthia` provides six sub-commands; `test`, 
`generate`, `replay`, `run`, `inspect`, and `clean`.

```
cynthia@0fbedf262c3d:~$ cynthia --help
cynthia 0.1
Usage: cynthia [test|generate|replay|run|inspect|clean] [options]

Cynthia: Data-Oriented Differential Testing of Object-Relational Mapping Systems

  --help                   Prints this usage text
  --version                Prints the current tool's version
Command: test [options]

  -n, --queries <value>    Number of queries to generate for each schema (Default value: 200)
  -s, --schemas <value>    Number of schemas to generate (Default value: 1)
  --timeout <value>        Timeout for testing in seconds
  -o, --orms <value>       ORMs to differentially test
  -d, --backends <value>   Database backends to store data (Default Value: sqlite)
  -S, --store-matches      Save matches
  --combined               Generate AQL queries consting of other simpler queries
  -r, --records <value>    Number of records to generate for each table
  --min-depth <value>      Minimum depth of generated AQL queries
  --max-depth <value>      Maximum depth of generated AQL queries
  --no-well-typed          Generate AQL queries that are type incorrect
  --solver                 Generate database records through a solver-based approach
  --solver-timeout <value>
                           Solver timeout for each query
  --random-seed <value>    Make the testing procedure deterministic by giving a random seed
Command: generate [options]

  -n, --queries <value>    Number of queries to generate for each schema (Default value: 200)
  -s, --schemas <value>    Number of schemas to generate (Default value: 1)
  --combined               Generate AQL queries consting of other simpler queries
  -r, --records <value>    Number of records to generate for each table
  --min-depth <value>      Minimum depth of generated AQL queries
  --max-depth <value>      Maximum depth of generated AQL queries
  --no-well-typed          Generate AQL queries that are type incorrect
  --solver                 Generate database records through a solver-based approach
  --solver-timeout <value>
                           Solver timeout for each query
  --random-seed <value>    Make the testing procedure deterministic by giving a random seed
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

### cynthia test

This is the main sub-command for testing ORMs.
`cynthia test` expects at least two ORMs to test
(through the `--orms` option),
and some database systems (i.e., backends) specified
by the `--backends` options.
Note that if `--backends` is not given,
the `SQLite` database system is used by default.

`cynthia test` first generates a number of relational database
schemas specified by the `--schemas` option.
Every generated schema corresponds to a *testing session*.
In every testing session, `cynthia test` generates a number of
random AQL queries (given by the `--queries` option),
translates every AQL query into the corresponding executable ORM query,
and finally runs every ORM query on the given backends.

#### Example

In the following scenario,
we differentially test the `peewee` and `Django` ORMs
agaist the `SQLite` and `PostgreSQL` databases,
by using 5 testing sessions (`--schemas 5`).
In every testing session, we generate 100 AQL queries
(`--queries 100`).
Finally, to populate the underlying databases with data,
we use an SMT solver (`--solver`) to generate five
records (`--records 5`) by solving the contraints
of every generated AQL query.

```bash
cynthia@0fbedf262c3d:~$ cynthia test \
  --schemas 5 \
  --queries 100 \
  --orms django,peewee \
  --backends postgres \
  --solver \
  --records 5 \
  --random-seed 1
```

The above command will produce an output similar to the following

```
Testing Serially 100% [===================== Passed ✔: 93, Failed ✘: 0, Unsp: 5, Timeouts: 2
Testing Cucumbers 100% [==================== Passed ✔: 95, Failed ✘: 0, Unsp: 2, Timeouts: 3
Testing Mumbles 100% [====================== Passed ✔: 94, Failed ✘: 0, Unsp: 3, Timeouts: 3
Testing Subhead 100% [====================== Passed ✔: 96, Failed ✘: 0, Unsp: 2, Timeouts: 2
Testing Wild 100% [========================= Passed ✔: 96, Failed ✘: 0, Unsp: 4, Timeouts: 0
Command test finished successfully.
```

Note that `Cynthia` processes testing sessions in parallel by using Scala
futures. `Cynthia` also dumps some statistics for every testing session.

```
Testing Cucumbers 100% [==================== Passed ✔: 95, Failed ✘: 0, Unsp: 2, Timeouts: 3
```

For example, the above message
means that in the testing session named `Cucumbers`,
`Cynthia` generated 100 AQL queries of which

* 95 / 100 queries passed (i.e., the ORMs under test produced exact results).
* 0 / 100 queries failed (i.e., the ORMs under test produced different results).
  Note that failed queries indicate a bug
  in at least one of the ORMs under test.
* 2 / 100 queries were unsupported meaning that the ORMs were unable to execute
  these queries, because these queries contained features
  that are not currently supported by the ORMs under test.
* 3 / 100 queries timed out, i.e., the SMT solver timed out and failed to
  generate records for populating databases.

#### The .cynthia working directory

`Cynthia` also produces a directory named `.cynthia`
(inside the current working directory)
where it stores the complete output of each run.
The `.cynthia` directory has the following structure.

* `.cynthia/dbs/`: This is the directory where the `SQLite` database files
  of each testing session are stored.
* `.cynthia/schemas/`: This directory contains SQL scripts corresponding to
the database schema of each testing session. Every SQL script contains
all the necessary `CREATE TABLE` statements for creating the relational
tables defined in each schema.
* `.cynthia/projects/`: A directory where `Cynthia` creates and runs
the corresponding ORM queries.
* `.cynthia/sessions/`: This directory contains complete information about
the runs taken place at every testing session. 

In particular, by inspecting the structure of the `.cynthia/sessions/`
directory, we have the following

* `.cynthia/sessions/<Session Name>/<Query ID>/data.sql`:
  This is the SQL script that populates the underlying database
  with data generated by the SMT solver. Note that these data are targeted,
  meaning that satisfy the constraints of the query specified by `<Query ID>`. 
* `.cynthia/sessions/<Session Name>/<Query ID>/diff_test.out`:
  The output of differential testing. Either "MATCH", "MISMATCH", "UNSUPPORTED",
  or "TIMEOUT".
* `.cynthia/sessions/<Session Name>/<Query ID>/<orm>_<backend>.out`:
  The output produced by the query written in ORM named `<orm>`,
  and when this query is run on the backend `<backend>`.
* `.cynthia/sessions/<Session Name>/<Query ID>/query.aql`:
  The AQL query in human readable format.
* `.cynthia/sessions/<Session Name>/<Query ID>/query.aql.json`:
  The AQL query in JSON format. This is what `Cynthia` understands.
* `.cynthia/sessions/<Session Name>/<Query ID>/<orm>/`:
  This directory contains all ORM-specific files for executing
  the ORM queries written in ORM named `<orm>`.
  For example, by executing

  ```bash
  python .cynthia/sessions/Cucumbers/22/django/driver_postgres.py
  ```

  You re-execute the Django query stemming from the AQL query
  with id `22` on the PostgresSQL database.


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
Replaying Serially  ? % [     =              Passed ✔: 95, Failed ✘: 0, Unsp: 5, Timeouts: 0
Replaying Cucumbers  ? % [          =        Passed ✔: 98, Failed ✘: 0, Unsp: 2, Timeouts: 0
Replaying Subhead  ? % [=                    Passed ✔: 98, Failed ✘: 0, Unsp: 2, Timeouts: 0
Replaying Mumbles  ? % [=                    Passed ✔: 97, Failed ✘: 0, Unsp: 3, Timeouts: 0
Replaying Wild  ? % [        =               Passed ✔: 96, Failed ✘: 0, Unsp: 4, Timeouts: 0
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
Replaying Cucumbers  ? % [          =        Passed ✔: 98, Failed ✘: 0, Unsp: 2, Timeouts: 0
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
cynthia@0fbedf262c3d:~$ replay \
  --schema Cucumbers \
  --orms sqlalchemy,sequelize \
  --all
```

### cynthia run

The sub-command `cynthia run` tests the given ORMs against
certain AQL queries (provided by the user)
based on a given database schema.


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
  --orms django,peewee -S
```

This produces

```bash
Running books  ? % [ =                         Passed ✔: 1, Failed ✘: 0, Unsp: 0, Timeouts: 0
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

In order to test ORMs, the generated queries can be later executed
using the `cynthia replay` command as documented above.


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

This sub-command inspects the results, and reports
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
> _default 0.00
\ No newline at end of file
```

### cynthia clean

`cynthia` simply cleans the `.cynthia` directory
and the database servers from the generated databases.
