main {

  var out = new IloOplOutputFile("output.txt");

  var files = new Array();
  files[0] = "input/input.dat";
  files[1] = "input/input2.dat";
  files[2] = "input/input3.dat";

  var src = new IloOplModelSource("project.mod");
  var def = new IloOplModelDefinition(src);
  var cplex = new IloCplex();

  cplex.tilim=120; // time limit in seconds
  cplex.epgap=0.001; // Branch and bound acceptance.

  var start = new Date();
  var startTime = start.getTime();
 
  for (var i = 0; i < files.length; ++i) {

    out.writeln("============ " + files[i] + " ================");

    var model = new IloOplModel(def, cplex);
    var data = new IloOplDataSource(files[i]);
    model.addDataSource(data);
    model.generate();

    if (cplex.solve()) {

      out.writeln("Objective value " + cplex.getObjValue());

      for(var l=1; l<=model.nLocations; l++) {
        for(var t=1; t<=model.nTypes; t++) {
          if (model.X[l][t] == 1) {
            out.writeln("Location " + l + " contains a logistic center of type " + t); 
          } 
        }
      }

      for(var c=1; c<=model.nCities; c++) {
        out.writeln("City " + c + " is server by");
        for(var l=1; l<=model.nLocations; l++) {
          if(model.P[c][l] == 1) {
            out.writeln("* primary center in location " + l + " (distance " + model.D_cl[c][l] + ")");
          }
        }
        for(var l=1; l<=model.nLocations; l++) {
          if(model.S[c][l] == 1) {
            out.writeln("* secondary center at location " + l + " (distance " + model.D_cl[c][l] + ")");
          }
        }
      }

      var end = new Date();
      var endTime = end.getTime();
      out.writeln("Execution time: " + (endTime - startTime) + "ms");
    }
    else {
      out.writeln("No solution found");
    }
    out.writeln("============================================");
    data.end();
    model.end();
  }

  cplex.end();
  def.end();
  src.end();
};
