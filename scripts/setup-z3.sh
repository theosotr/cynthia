#! /bin/bash

cd ${HOME}
git clone https://github.com/Z3Prover/z3
cd z3
python3 scripts/mk_make.py --java
cd build
make -j 8
sudo make install
