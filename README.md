# Testing ORM Systems

## Setup

Install Scala compiler, Sqlite and other necessary libraries

```
sudo apt install scala sqlite3 libpq-dev libmysqlclient-dev
```

Install [sbt](https://www.scala-sbt.org/) which is the
build system for compiling project.

```
echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
sudo apt update && sudo apt install sbt
```

Install Python `virtualenv`
```
pip3 install virtualenv
```

Setup a Python virtual environment and install requirements
(note that this installs (among other things) `django` and `sqlalchemy`).

```
virtualenv -p python3 .env
source .env/bin/activate
pip install -r requirements.txt
```

## ORMs dependencies

* ActiveRecord (Ruby)

The first time you should run Cynthia with postgres backend to initialize
the Database

```bash
sudo apt install ruby-full ruby-activerecord rudy-dev
sudo gem install sqlite3 mysql2 postgres rmre pg
```

__Redirect stderr to hide ruby warnings__

* sequelize (nodejs)

```bash
npm install --save sequelize
npm install --save sequelize-auto
npm install --save sqlite3
```

## Compile Cynthia

Simply run

```
sbt assembly
```

This gonna take a while. Upon completion, an executable `jar` of
`cynthia` will be generated at ` target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar`.


## Run Cynthia

Cynthia CLI provides three sub-commands; auto, replay, and select.

```
$ java -jar target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar --help
cynthia 0.1
Usage: cynthia [auto|replay|select] [options]

  -o, --orms <value>       ORMs to differentially test
  -d, --backends <value>   Database backends to store data (Default Value: sqlite)


  --help                   prints this usage text
Command: auto [options]

  -n, --queries <value>    Number of queries to generate for each schema (Default value: 200)
  -s, --schemas <value>    Number of schemas to generate (Default value: 1)
Command: replay [options]

  -c, --cynthia <value>    .cynthia directory for replaying missmatches
  -m, --mismatches <value>
                           Mismatches to replay
Command: select [options]

  -s, --sql <value>        File with the sql script to generate and feed the Database
  -a, --aql <value>        A file with an AQL query or a directory with many AQL queries
```

### auto

It generates new schemas and queries for the targeted orms.
For example the following command will generate 2 new schemas, and will
run 100 queries for each one of those schemas on activerecord and sqlalchemy orms
using sqlite and mysql as backends.

```
java -jar target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar auto -n 100 -s 2 -o activerecord,sqlalchemy -d mysql
```

### select

It runs cynthia using the queries provided by `-a` option on the schema provided
by `-s` option. If `-a` is a directory then it must contains only `.aql.json`files
that cynthia will use to test the orms. `-s` option must provide a file
with an sql script to generate and fill a database.

```
java -jar target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar select -s examples/listing.sql -a examples/listing/ -o sqlalchemy,activerecord -d mysql
```

```
java -jar target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar select -s .cynthia/schemas/listing -a .cynthia/report/listing/mismatches/1/query.aql.json -o sqlalchemy,activerecord -d mysql
```

### replay

### clean

Remove `.cynthia` directory

## Reproduce Django Bug

Install the faulty Django version inside the previously-created
virtualenv

```
pip uninstall django
pip install django==2.2
```

Run cynthia

```
java -jar target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar -s examples -o django,sqlalchemy
```

This will generate (ok I know it's awful).

```
Mismatches

Ok[1.002.003.00] Target Group: django[sqlite]
Ok[3.001.002.00] Target Group: sqlalchemy[sqlite]
```

The above means that (for the same query written in AQL), Django
produced
```bash
1.00
2.00
3.00
```

while SQLAlchemy produced

```bash
3.00
1.00
2.00
```

For more details, see the reported bug at
https://code.djangoproject.com/ticket/30628
