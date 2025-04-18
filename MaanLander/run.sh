#!/bin/bash

set -e

if [ "$#" -ne 6 ]; then
  echo "Usage: $0 <strategie #> <hoogte> <brandstof> <motorkracht> <maxSnelheid> <valversnelling>"
  exit 1
fi

HS_SRC="Maanlander.hs"
HS_BIN="Maanlander"
PY_FILE="maanlander_plot.py"
OUTPUT_FILE="maanlander_output.txt"

ghc "$HS_SRC" -o "$HS_BIN"
./"$HS_BIN" "$@"

if [ ! -f "$OUTPUT_FILE" ]; then
  echo "Error: Output file '$OUTPUT_FILE' not found."
  exit 1
fi

python3 "$PY_FILE"
rm -f "$HS_BIN" *.hi *.o
