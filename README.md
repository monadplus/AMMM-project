# Integer Linear Programming and Heuristics

The statement of the problem can be found [here](./statement.pdf).

The mathematical model can be found [here](model-draft.pdf).

## Integer Linear Programming

The project contains an [IBM ILOG CPLEX](https://www.ibm.com/support/knowledgecenter/SSSA5P_12.9.0/ilog.odms.ide.help/OPL_Studio/maps/groupings/opl_Language.html) program written in OPL that solves the stated problem.

- The OPL program can be found [here](./program.mod)
- The runner script can be found [here](./script.mod)

### Instructions

You only need import the whole project in OPLIDE and run the script.

The input instances are fixed in the [script](./script.mod). Open the script with your favourite text editor and change the route. Have a look at the `./input` folders for **examples**.

## Heuristic Algorithms

Two main algorithm implemented:

1. Greedy constructive algorithm with optional local search procedure.
2. GRASP algorithm with tunable parameters.

#### Instructions

The instructions heuristics algorithms can be found at [heuristics](./heuristics/README.md).
