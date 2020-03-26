# Testing ORM Systems

## Setup

Install Scala compiler and Sqlite

```
sudo apt install scala sqlite3
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


## Compile Cynthia

Simply run

```
sbt assembly
```

This gonna take a while. Upon completion, an executable `jar` of
`cynthia` will be generated at ` target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar`.


## Run Cynthia

```
java -jar target/scala-2.13/cynthia-assembly-0.1.0-SNAPSHOT.jar -s examples -o django,sqlalchemy
```

* The option `-s` is the directory of the database schemas.
* The option `-o` is a set of ORMs (comma separated) under test.


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

while `sqlalchemy` produced

```bash
3.00
1.00
2.00
```

For more details, see the reported bug at
https://code.djangoproject.com/ticket/30628
