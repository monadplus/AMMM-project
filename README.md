# Integer Linear Programming and Heuritic Algorithms

The statement of the problem can be found [here](./statement.pdf).

The mathematical model can be found [here](model-draft.pdf).

## CPLEX

The project contains an [IBM ILOG CPLEX](https://www.ibm.com/support/knowledgecenter/SSSA5P_12.9.0/ilog.odms.ide.help/OPL_Studio/maps/groupings/opl_Language.html) program written in OPL.

- The OPL program can be found [here](./program.mod)
- The runner script can be found [here](./script.mod)

You only need import the whole project in OPLIDE and run the script.

## Heuristic Algorithms

There are three different heuristics approaches:

- a greedy constructive algorithm
- a greedy constructive + a local search procedure
- GRASP as a meta-heuristic algorithm.

The heuristics algorithms can be found at [heuristics](./heuristics/README.md).
