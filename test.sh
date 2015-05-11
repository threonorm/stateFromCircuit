#!/bin/bash
./Logic $1 > $1.eqn
./minisat+ $1.eqn > $1.sol
./Reconstruction $1 $1.sol | dot -Tsvg > $1.svg
