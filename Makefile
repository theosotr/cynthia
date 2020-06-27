SCALA_FILES=$(shell find src -name '*.scala')
RESOURCES=src/main/resources/words
JAR_FILE=cynthia-assembly-0.1.0-SNAPSHOT.jar

scripts/cynthia-assembly-0.1.0-SNAPSHOT.jar: $(SCALA_FILES) $(RESOURCES)
	sbt assembly
	cp target/scala-2.13/${JAR_FILE} scripts

clean:
	rm -f scripts/${JAR_FILE}
