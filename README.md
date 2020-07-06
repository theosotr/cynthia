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
sudo apt install ruby-full rudy-dev
sudo gem install sqlite3 mysql2 postgres activerecord pg
```

For installing `rmre`, which is the tool for creating models, run

```bash
git clone https://github.com/theosotr/rmre
cd rmre
gem build rmre.gemspec
sudo gem install rmre-0.0.9.gem
```

__Redirect stderr to hide ruby warnings__

* sequelize (nodejs)

```bash
sudo npm uninstall -g sqlite3 --unsafe-perm
sudo npm uninstall -g sequelize --unsafe-perm
npm install --save sequelize
npm install --save sqlite3
```

* peewee (Python)

```bash
pip uninstall peewee
git clone https://github.com/coleifer/peewee.git
cd peewee
patch < ../patches/pwiz.patch
patch < ../patches/peewee.patch
python setup.py install
```

## Compile Cynthia

Simply run

```
sbt assembly
```

This gonna take a while. Upon completion, an executable `jar` of
`cynthia` will be generated at ` target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar`.


## Run Cynthia

Cynthia CLI provides three sub-commands; test, replay, and run.

```
Usage: cynthia [test|generate|replay|run|clean] [options]

  -o, --orms <value>       ORMs to differentially test
  -d, --backends <value>   Database backends to store data (Default Value: sqlite)
  -S, --store-matches      Save matches
  --no-combined            Don't generate combined queries
  -r, --records <value>    Number of records to generate for each table


  --help                   prints this usage text
Command: test [options]

  -n, --queries <value>    Number of queries to generate for each schema (Default value: 200)
  -s, --schemas <value>    Number of schemas to generate (Default value: 1)
  --no-combined            Don't generate combined queries
  -r, --records <value>    Number of records to generate for each table
Command: generate [options]

  -n, --queries <value>    Number of queries to generate for each schema (Default value: 200)
  -s, --schemas <value>    Number of schemas to generate (Default value: 1)
  --no-combined            Don't generate combined queries
  -r, --records <value>    Number of records to generate for each table
Command: replay [options]

  -c, --cynthia <value>    cynthia directory for replaying missmatches (Default .cynthia)
  -s, --schema <value>     Schema to replay
  -a, --all                Replay all queries. Always use it with --store-matches to not remove matches queries
  -m, --mismatches <value>
                           Mismatches to replay
Command: run [options]

  -s, --sql <value>        File with the sql script to generate and feed the Database
  -a, --aql <value>        A file with an AQL query or a directory with many AQL queries
Command: clean
```

### test

Generate AQL queries and test orms.

* Example

```
java -jar target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar test -n 100 -s 2 -o activerecord,sqlalchemy -d mysql
```

### run

It runs cynthia using the queries provided by `-a` option on the schema provided
by `-s` option. If `-a` is a directory then it must contains only `.aql.json`files
that cynthia will use to test the orms. `-s` option must provide a file
with an sql script to generate and fill a database.

* Examples

```
java -jar target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar run -s examples/listing.sql -a examples/listing/ -o sqlalchemy,activerecord -d mysql
```

```
java -jar target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar run -s .cynthia/schemas/listing -a .cynthia/report/listing/mismatches/1/query.aql.json -o sqlalchemy,activerecord -d mysql
```

### replay

Run the given AQL queries on the given schemas. If no options are specified, this command inspects the .cynthia directory.

* Examples

```
java -jar target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar replay --orms activerecord,sqlalchemy -d mysql
```

```
java -jar target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar replay -s Hubbubs --all --orms activerecord,sqlalchemy -d mysql --store-matches
```

```
java -jar target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar replay -s Hubbubs --mismatches 1 --all --orms activerecord,sqlalchemy -d mysql --store-matches
```

### generate

Generate random AQL queries and schemas. It does not translate queries into ORM queries.

* Example

```
java -jar target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar generate -s 2 -n 100
```

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
