#!/bin/bash

ghc main.hs -o run

if [ -f run ]; then
    ./run
else
    exit 1
fi

rm -f *.o *.hi run

