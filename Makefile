INSTALL_PREFIX?=/usr/local/bin
SCALA_FILES=$(shell find src -name '*.scala')
RESOURCES=src/main/resources/words

target/scala-2.13/cynthia.jar: $(SCALA_FILES) $(RESOURCES)
	sbt assembly

install:
	install -D -o root scripts/cynthia "${INSTALL_PREFIX}/cynthia"

uninstall:
	rm -f "${INSTALL_PREFIX}/cynthia"

clean:
	sbt clean
