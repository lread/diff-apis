#!/usr/bin/env bash

set -eou pipefail

(cd ../rewrite-cljs-playground;mvn install)

./g1.sh
./g1f.sh
./g2.sh
./g3.sh
./g4.sh
./g4-nodoc.sh
