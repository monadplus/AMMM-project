#!/bin/bash

for ((size = 10 ; size <= 100 ; size+=10)); do
    eval $"cabal run heuristics -- generator -n $size -f 'examples/example.dat'"
done
