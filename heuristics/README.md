# Heuristics

The project includes:

- A collection of heuristics algorithms to solve the statement problem.
- An instance generator for both the CPLEX and the heuristic approach.

## Structure

- `app` contains the sources of the executable.
- `src` contains the sources of the library that the exectuable depends on.
- `test` contains the sources of the library's test.
- `examples` contains some instances to run.

## Installation

This project is build using [GHC](https://www.haskell.org/ghc/)(compiler) and [cabal](https://cabal.readthedocs.io/en/latest/index.html)(build tool).

The easiest way to install both is using [ghcup](https://gitlab.haskell.org/haskell/ghcup-hs)

``` sh
# Install ghcup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Install GHC using ghcup
ghcup install ghc 8.8.4

# Install cabal using ghcup
ghcup install cabal
```

Finally, we need to compile the project. This may takes some minutes and requires internet connection. This project does not depend on any `.so` so it should be possible to compile it in any architecture that supports `ghc`. 

```sh
# It may takes some minutes
$ cabal build
...
Preprocessing executable 'heuristics' for heuristics-0.1.0.0..
Building executable 'heuristics' for heuristics-0.1.0.0..
HOME/heuristics/dist-newstyle/build/x86_64-linux/ghc-8.8.4/heuristics-0.1.0.0/x/heuristics/opt/build/heuristics/heuristics ...
```

## HOW TO

Let's start by showing the available options:

```sh
# Use the --help flag to display the options
$ cabal run heuristics -- --help

Usage: heuristics COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  generator                Problem instance generator
  solver                   Heuristic solver
```

From this point, you should be able to run the different algorithms. See examples below:

```sh
# Greedy (the most basic one)
cabal run heuristics -- solver greedy -f 'examples/sample_500.dat'

# Greedy with local search first improvement
cabal run heuristics -- solver greedy --localSearch -f 'examples/sample_500.dat'

# Greedy with local search best improvement
cabal run heuristics -- solver greedy --localSearch -b -f 'examples/sample_500.dat'

# GRASP with time limit and alpha threshold.
cabal run heuristics -- solver grasp --limit 300 --threshold 0.5 -f 'examples/sample_500.dat'
```

### Instance Generator

If you are interested in creating new instances, a random instance generator is included with customizable size:

```sh
$ cabal run heuristics -- generator -n 10000 -f 'example.dat'
./example_10000.dat
```

There is the `generate_instances.sh` script that makes it easier to generate test samples.

## TODO

- [ ] Performance can be improved A LOT.
- [ ] The code ergonomics can be improved.
- [ ] Test, test, test.
