#!/bin/sh

rm -rf main-tests.tix .hpc && \
cabal clean && \
cabal configure -fhpc && \
cabal build && \
cabal test && \
hpc markup --destdir=dist/hpc main-tests.tix