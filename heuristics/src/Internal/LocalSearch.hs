module Internal.LocalSearch where

{-

Apply H1 and on the solution:
  Sort cities by population.
  For each city c
    For each city c'
      Try swapping primary and primary'
      and check if it feasible and the cost
      If the cost is better, swap them
      Do the same for secondary
  Repeat until no change is possible.

-}
