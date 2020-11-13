int nLocations = ...;
int nCities = ...;
int nTypes = ...;

range L = 1..nLocations;
range C = 1..nCities;
range T = 1..nTypes;

int posCities[C][1..2] = ...;
int posLocations[L][1..2] = ...;
float D_ll[L][L]; // Distance between localtion l1 and location l2
float D_cl[C][L]; // Distance between city c and location l

int p[C] = ...; // population of city c

float d_city[T] = ...;
int cap[T] = ...;
int cost[T] = ...;

float d_center = ...;

dvar boolean P[C][L]; // When city c primary logistic center is location l
dvar boolean S[C][L]; // When city c secondary logistic center is location l
dvar boolean X[L][T]; // Location l contains a logistic center of type t

execute{
   for(var l1 = 1; l1 <= nLocations; l1++)
        for(var l2 = 1; l2 <= nLocations; l2++)
            D_ll[l1][l2] = Math.sqrt( Math.pow(posLocations[l2][1] - posLocations[l1][1], 2)
                                    + Math.pow(posLocations[l2][2] - posLocations[l1][2], 2)
                                    );

   for(var c = 1; c <= nCities; c++)
        for(var l = 1; l <= nLocations; l++)
            D_cl[c][l] = Math.sqrt( Math.pow(posCities[c][1] - posLocations[l][1], 2)
                                  + Math.pow(posCities[c][2] - posLocations[l][2], 2)
                                  );
}

minimize sum(l in L) sum(t in T) X[l][t]*cost[t];

subject to {

  // (1.1)
  // You may think this is not needed because the problem is minimizing X[l][t]
  // but in reality it may happen that we need 3 X[l][t] to fullful the constraints
  // and if we do not limit to one type per location, multiple types be assigned to a single location.
  forall(l in L)
    sum(t in T) X[l][t] <= 1;

  // (1.2)
  forall(c in C)
    sum(l in L) P[c][l] == 1;

  // (1.3)
  forall(c in C)
    sum(l in L) S[c][l] == 1;

  // (1.4)
  forall(c in C)
    forall(l in L)
        P[c][l] + S[c][l] <= 1;

  // (2)
  forall(l1, l2 in L:l2<l1)
    forall(t1,t2 in T)
      D_ll[l1][l2] >= X[l1][t1]*X[l2][t2]*d_center;

  // (3.1)
  forall(c in C)
    forall(l in L)
      P[c][l]*D_cl[c][l] <= (sum(t in T) X[l][t]*d_city[t]);

  // (3.2)
  forall(c in C)
    forall(l in L)
      S[c][l]*D_cl[c][l] <= 3*(sum(t in T) X[l][t]*d_city[t]);

  // (4)
  forall(l in L)
    (sum(c in C) p[c]*P[c][l]) + 0.1*(sum(c in C) p[c]*S[c][l]) <= (sum(t in T) X[l][t]*cap[t]);
}
