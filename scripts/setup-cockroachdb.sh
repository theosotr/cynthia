#! /bin/sh
wget -qO- https://binaries.cockroachdb.com/cockroach-v20.1.3.linux-amd64.tgz | tar  xvz && \
sudo cp -i cockroach-v20.1.3.linux-amd64/cockroach /usr/local/bin/ && \
rm -rf cockroach-v20.1.3.linux-amd64/ && \
cockroach start \
--insecure \
--store=node1 \
--listen-addr=localhost:26257 \
--http-addr=localhost:8080 \
--join=localhost:26257,localhost:26258,localhost:26259 \
--background && \
cockroach init --insecure --host=localhost:26257 && \
cockroach sql --insecure --host=localhost:26257 --execute "GRANT ALL ON DATABASE defaultdb TO admin;"
