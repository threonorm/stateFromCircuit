#!/bin/bash
ghc -O2 --make -static -optc-static -optl-static Sat.hs -fvia-C -optl-pthread
ghc -O2 --make -static -optc-static -optl-static Reconstruction.hs -fvia-C -optl-pthread
ghc -O2 --make -static -optc-static -optl-static Visualization.hs -fvia-C -optl-pthread

