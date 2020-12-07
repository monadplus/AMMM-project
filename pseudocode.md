# Pseudocode

## Constructive

```
For each city c
    For each location l
        cost <- q(c, l, Primary)
    l_i <- Pick the location with smallest cost
    Assign c -> l_i
    Do the same for secondary location

Return the assignments c -> l
```

Cost Function

```
q(c, l, Primary/Secondary):
    If the location contains a facility
      Then
        Check if the assignment is feasible with the current facility (range, capacity)
        If feasible
          Then cost = 0
        Otherwise
          Try to upgrade the facility type
          If there is a feasible one
            Then cost = cost facility type - cost previous facility type
          Otherwise
            Infeasible
    Otherwhise
      If you can place a facility in the location (d_center)
        Then Pick the facility with smallest cost that is feasible (range, capacity)
      Otherwise 
        Infeasible
```

## Local Search

Local Search

```
While improvement
  For each facility f
    For each city c assigned the facility f (as primary or secondary)
      If the facility can be downgraded (improving its cost) by removing c
        Then 
          If c can be assigned to another facility f_2
            If FirstImprovement
              Then update the solution with this reassignment and facility downgrade
            If BestImprovement
              Then return candidate (cost, upgrade)
  If BestImprovement
    Then pick the candidate with smallest cost and upgrade the solution.
```

The algorithm:

```
Greedy
Local Search
```


## GRASP

RCL candidates selection

```
For each city c
  RCL <-  For each location l
          cost <- q(c, l, Primary)
  q_min <- min RCL
  q_max <- max RCL
  ls <- { l \in RCL | q_min <= q <= q_min + alpha * (q_max - q_min)}
  Assign c -> u.a.r ls
```

The algorithm

```
While !time_limit && !improvement_in_last_n_iterations
    Greedy + RCL
    Local Search
```
