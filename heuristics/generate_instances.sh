#!/bin/bash

OUTPUT_DIR='instances'
FILENAME='sample.dat'
CMD='cabal run heuristics -- generator'

STEP=50
MIN_SIZE=150
MAX_SIZE=250

for ((size = $MIN_SIZE ; size <= $MAX_SIZE ; size+=STEP)); do
    eval $"$CMD -n $size -f '$OUTPUT_DIR/$FILENAME'"
done
