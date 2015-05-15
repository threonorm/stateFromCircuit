#!/bin/bash
./Sat $1 > $1.eqn
./minisat+ $1.eqn -1 > $1.sol
./Reconstruction $1 $1.sol > $1.after.dot #| dot -Tsvg > $1.after.svg
./Visualization $1 > $1.before.dot #| dot -Tsvg > $1.before.svg
dot -Tsvg $1.after.dot > graphTest.svg
gnome-open graphTest.svg
