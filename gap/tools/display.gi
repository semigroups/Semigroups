#############################################################################
##
##  tools/display.gi
##  Copyright (C) 2013-2024                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#############################################################################
# Tex strings
#############################################################################

InstallMethod(TexString, "for a transformation and a pos int",
[IsTransformation, IsPosInt],
function(f, deg)
  local str;

  if deg < DegreeOfTransformation(f) then
    ErrorNoReturn("the 2nd argument (a pos. int.) is less than ",
                  "the degree of the 1st argument (a ",
                  "transformation)");
  fi;
  str := "\\begin{pmatrix}\n  ";
  Append(str, JoinStringsWithSeparator(List([1 .. deg], String), " & "));
  Append(str, " \\\\\n  ");
  Append(str, JoinStringsWithSeparator(List([1 .. deg],
                                            i -> String(i ^ f)), " & "));
  Append(str, "\n\\end{pmatrix}");
  return str;
end);

InstallMethod(TexString, "for a transformation",
[IsTransformation],
f -> TexString(f, DegreeOfTransformation(f)));

InstallMethod(TexString, "for a transformation collection",
[IsTransformationCollection],
function(coll)
  local deg;
  deg := DegreeOfTransformationCollection(coll);
  return JoinStringsWithSeparator(List(coll, x -> TexString(x, deg)), "\n");
end);

#############################################################################
# Tikz internal stuff
#############################################################################

SEMIGROUPS.GetTikzInit := function(opts)
  local extra, strings;

  if IsBound(opts.extraInit) then
    extra := opts.extraInit;
  else
    extra := "";
  fi;

  strings := ["%latex", "\\documentclass{minimal}", "\\usepackage{tikz}",
              extra, "\\begin{document}"];
  return JoinStringsWithSeparator(strings, "\n");
end;

SEMIGROUPS.TikzEnd := "\\end{document}";

SEMIGROUPS.TikzColors := ["red",
                          "green",
                          "blue",
                          "cyan",
                          "magenta",
                          "yellow",
                          "black",
                          "gray",
                          "darkgray",
                          "lightgray",
                          "brown",
                          "lime",
                          "olive",
                          "orange",
                          "pink",
                          "purple",
                          "teal",
                          "violet",
                          "white"];

SEMIGROUPS.TikzPBRInit := Concatenation("\\usetikzlibrary{arrows}\n",
                                        "\\usetikzlibrary{arrows.meta}\n",
                                        "\\newcommand{\\arc}{\\draw[semithick,",
                                        " -{>[width = 1.5mm, length = ",
                                        "2.5mm]}]}\n");

SEMIGROUPS.TikzPBROpts := rec(beginDocument := true,
                              endDocument := true,
                              extraInit := SEMIGROUPS.TikzPBRInit,
                              labels := false);

SEMIGROUPS.TikzBipartitionOpts := rec(colors        := false,
                                      beginDocument := true,
                                      endDocument   := true,
                                      labels        := true);
SEMIGROUPS.TikzBlocksOpts      := rec(labels := "above",
                                      edges  := "below",
                                      colors := false);

###############################################################################
# Tikz collections
###############################################################################

InstallMethod(TikzString, "for a collection and record",
[IsCollection, IsRecord],
function(coll, opts)
  local outOpts, str, x;

  str := ShallowCopy(SEMIGROUPS.GetTikzInit(opts));
  Append(str, "\\begin{center}\n");
  outOpts := ShallowCopy(opts);
  outOpts.beginDocument := false;
  outOpts.endDocument   := false;

  for x in coll do
    Append(str, TikzString(x, outOpts));
    Append(str, "\n\\bigskip\\bigskip\n\n");
  od;
  Append(str, "\\end{center}");

  Append(str, ShallowCopy(SEMIGROUPS.TikzEnd));
  return str;
end);

#############################################################################
# Tikz pbrs
#############################################################################

InstallMethod(TikzString, "for a pbr collection",
[IsPBRCollection],
coll -> TikzString(coll, SEMIGROUPS.TikzPBROpts));

InstallMethod(TikzString, "for a pbr",
[IsPBR],
x -> TikzString(x, SEMIGROUPS.TikzPBROpts));

InstallMethod(TikzString, "for a pbr and record",
[IsPBR, IsRecord],
function(x, opts)
  local ext, n, str, coeff1, coeff2, jj, i, j;

  ext := ExtRepOfObj(x);
  n   := DegreeOfPBR(x);

  SEMIGROUPS.ProcessOptionsRec(SEMIGROUPS.TikzPBROpts, opts);

  if opts.beginDocument = true then
    str := ShallowCopy(SEMIGROUPS.GetTikzInit(opts));
  else
    str := "";
  fi;
  Append(str, "\\begin{tikzpicture}[\n");
  Append(str, "  vertex/.style={circle, draw, fill=black, inner sep =");
  Append(str, "0.04cm},\n");
  Append(str, "  ghost/.style={circle, draw = none, inner sep = 0.14cm},\n");
  Append(str, "  botloop/.style={min distance = 8mm, out = -70, in = -110},\n");
  Append(str, "  toploop/.style={min distance = 8mm, out = 70, in = 110}]\n\n");

  # draw the vertices and their labels
  Append(str, "  % vertices and labels\n");
  Append(str, "  \\foreach \\i in {1,...,");
  Append(str, String(n));
  Append(str, "} {\n");

  if opts.labels then
    Append(str, "    \\node [vertex, label={[yshift=9mm]\\i}] ");
    Append(str, "at (\\i/1.5, 3) ");
    Append(str, "{};\n");
  else
    Append(str, "    \\node [vertex] at (\\i/1.5, 3) {};\n");
  fi;

  Append(str, "    \\node [ghost] (\\i) at (\\i/1.5, 3) {};\n");
  Append(str, "  }\n\n");

  Append(str, "  \\foreach \\i in {1,...,");
  Append(str, String(n));
  Append(str, "} {\n");

  if opts.labels then
    Append(str, "    \\node [vertex, label={[yshift=-15mm,");
    Append(str, "xshift=-0.5mm]-\\i}] at (\\i/1.5, 0) {};\n");
  else
    Append(str, "    \\node [vertex] at (\\i/1.5, 0) {};\n");
  fi;

  Append(str, "    \\node [ghost] (-\\i) at (\\i/1.5, 0) {};\n");
  Append(str, "  }\n\n");

  # draw the arcs
  for i in [1 .. n] do
    Append(str, "  % arcs from vertex ");
    Append(str, String(i));
    Append(str, "\n");

    for j in ext[1][i] do
      if j = i then
        Append(str, "  \\arc (");
        Append(str, String(i));
        Append(str, ") edge [toploop] (");
        Append(str, String(i));
        Append(str, ");\n");
      elif j > 0 then
        if i < j then
          coeff1 := 1;
          coeff2 := -1;
        else
          coeff1 := -1;
          coeff2 := 1;
        fi;

        Append(str, "  \\arc (");
        Append(str, String(i));
        Append(str, ") .. controls (");
        Append(str, String(i / 1.5 + (coeff1 * 0.4 * AbsInt(i - j))));
        Append(str, ", ");
        Append(str, String(Float(2.75 - (5 / (4 * n)) * AbsInt(i - j))));
        Append(str, ") and (");
        Append(str, String(j / 1.5 + (coeff2 * 0.4 * AbsInt(i - j))));
        Append(str, ", ");
        Append(str, String(Float(2.75 - (5 / (4 * n)) * AbsInt(i - j))));
        Append(str, ") .. (");
        Append(str, String(j));
        Append(str, ");\n");
      else
        Append(str, "  \\arc (");
        Append(str, String(i));
        Append(str, ") to (");
        Append(str, String(j));
        Append(str, ");\n");
      fi;
    od;
    Append(str, "\n");

    Append(str, "  % arcs from vertex -");
    Append(str, String(i));
    Append(str, "\n");
    for j in ext[2][i] do
      if j = -i then
        Append(str, "  \\arc (");
        Append(str, String(-i));
        Append(str, ") edge [botloop] (");
        Append(str, String(-i));
        Append(str, ");\n");
      elif j < 0 then
        jj := -j;
        if i < jj then
          coeff1 := 1;
          coeff2 := -1;
        else
          coeff1 := -1;
          coeff2 := 1;
        fi;

        Append(str, "  \\arc (");
        Append(str, String(-i));
        Append(str, ") .. controls (");
        Append(str, String(i / 1.5 + (coeff1 * 0.4 * AbsInt(i + j))));
        Append(str, ", ");
        Append(str, String(Float(0.25 + (5 / (4 * n)) * AbsInt(i + j))));
        Append(str, ") and (");
        Append(str, String(jj / 1.5 + (coeff2 * 0.4 * AbsInt(i + j))));
        Append(str, ", ");
        Append(str, String(Float(0.25 + (5 / (4 * n)) * AbsInt(i + j))));
        Append(str, ") .. (");
        Append(str, String(j));
        Append(str, ");\n");
      else
        Append(str, "  \\arc (");
        Append(str, String(-i));
        Append(str, ") to (");
        Append(str, String(j));
        Append(str, ");\n");
      fi;
    od;
    Append(str, "\n");
  od;

  Append(str, "\\end{tikzpicture}\n");
  if opts.endDocument = true then
    Append(str, ShallowCopy(SEMIGROUPS.TikzEnd));
  fi;
  return str;
end);

#############################################################################
# Tikz bipartitions
#############################################################################

InstallMethod(TikzString, "for a bipartition collection",
[IsBipartitionCollection],
{coll} -> TikzString(coll, SEMIGROUPS.TikzBipartitionOpts));

InstallMethod(TikzString, "for a bipartition",
[IsBipartition],
x -> TikzString(x, SEMIGROUPS.TikzBipartitionOpts));

InstallMethod(TikzString, "for a bipartition and record",
[IsBipartition, IsRecord],
function(x, opts)
  local fill, draw, labels, ext, n, str, block, up, down, min, j, i, k;

  # Process the options
  SEMIGROUPS.ProcessOptionsRec(SEMIGROUPS.TikzBipartitionOpts, opts);

  if opts.colors and NrBlocks(x) < 20 then
    fill := i -> Concatenation("  \\fill[", SEMIGROUPS.TikzColors[i], "](");
    draw := i -> Concatenation("  \\draw[", SEMIGROUPS.TikzColors[i], "](");
  else
    fill := i -> "  \\fill(";
    draw := i -> "  \\draw(";
  fi;

  labels := opts.labels;

  ext := ExtRepOfObj(x);
  n   := DegreeOfBipartition(x);

  if opts.beginDocument = true then
    str := ShallowCopy(SEMIGROUPS.GetTikzInit(opts));
  else
    str := "";
  fi;

  Append(str, "\\begin{tikzpicture}\n");

  # draw the lines
  for j in [1 .. Length(ext)] do
    block := ext[j];
    Append(str, "\n");
    Append(str, "  %block number ");
    Append(str, String(j));
    Append(str, "\n");
    Append(str, "  %vertices and labels\n");
    # vertices and their labels
    for i in block do
      if i > 0 then
        Append(str, fill(j));
        Append(str, String(i));
        Append(str, ", 2)circle(.125);\n");
        if labels then
          Append(str, draw(j));
          Append(str, String(i - 0.05));
          Append(str, ", 2.2) node [above] {$");
          Append(str, String(i));
          Append(str, "$};");
          Append(str, "\n");
        fi;
      else
        Append(str, fill(j));
        Append(str, String(-i));
        Append(str, ", 0)circle(.125);\n");
        if labels then
          Append(str, draw(j));
          Append(str, String(-i));
          Append(str, ", -0.2) node [below] {$-");
          Append(str, String(-i));
          Append(str, "$};");
          Append(str, "\n");
        fi;
      fi;
    od;

    Append(str, "\n  %lines\n");
    # lines
    up := [];
    down := [];
    for i in [2 .. Length(block)] do
      if block[i - 1] > 0 and block[i] > 0 then
        AddSet(up, block[i - 1]);
        AddSet(up, block[i]);
        Append(str, draw(j));
        Append(str, String(block[i - 1]));
        Append(str, ", 1.875) .. controls (");
        Append(str, String(block[i - 1]));
        Append(str, ", ");
        Append(str, String(Float(1.5 - (1 / (2 * n))
                               * (block[i] - block[i - 1]))));
        Append(str, ") and (");
        Append(str, String(block[i]));
        Append(str, ", ");
        Append(str, String(Float(1.5 - (1 / (2 * n))
                               * (block[i] - block[i - 1]))));
        Append(str, ") .. (");
        Append(str, String(block[i]));
        Append(str, ", 1.875);\n");
      elif block[i - 1] < 0 and block[i] < 0 then
        AddSet(down, block[i - 1]);
        AddSet(down, block[i]);
        Append(str, draw(j));
        Append(str, String(- block[i - 1]));
        Append(str, ", 0.125) .. controls (");
        Append(str, String(- block[i - 1]));
        Append(str, ", ");
        Append(str, String(Float(0.5 + (- 1 / (2 * n))
                               * (block[i] - block[i - 1]))));
        Append(str, ") and (");
        Append(str, String(- block[i]));
        Append(str, ", ");
        Append(str, String(Float(0.5 + (- 1 / (2 * n))
                               * (block[i] - block[i - 1]))));
        Append(str, ") .. (");
        Append(str, String(- block[i]));
        Append(str, ", 0.125);\n");
      elif block[i - 1] > 0 and block[i] < 0 then
        AddSet(down, block[i]);
        AddSet(up, block[i - 1]);
      elif block[i - 1] < 0 and block[i] > 0 then
        AddSet(down, block[i - 1]);
        AddSet(up, block[i]);
      fi;
    od;
    if Length(up) <> 0 and Length(down) <> 0 then
      min := [n + 1];
      down := down * -1;
      for i in up do
        for k in down do
          if AbsInt(i - k) < min[1] then
            min[1] := AbsInt(i - k);
            min[2] := i;
            min[3] := k;
          fi;
        od;
      od;
      Append(str, draw(j));
      Append(str, String(min[2]));
      Append(str, ", 2)--(");
      Append(str, String(min[3]));
      Append(str, ", 0);\n");
    fi;
  od;
  Append(str, "\\end{tikzpicture}\n\n");
  if opts.endDocument = true then
    Append(str, ShallowCopy(SEMIGROUPS.TikzEnd));
  fi;
  return str;
end);

#############################################################################
# Tikz Cayley graphs
#############################################################################

InstallMethod(TikzLeftCayleyDigraph, "for a semigroup", [IsSemigroup],
S -> TikzString(LeftCayleyDigraph(S)));

InstallMethod(TikzRightCayleyDigraph, "for a semigroup", [IsSemigroup],
S -> TikzString(RightCayleyDigraph(S)));

InstallMethod(TikzString, "for a Cayley digraph", [IsCayleyDigraph],
function(digraph)
  local S, vertex, edge, str, nbs, x, from, gen;

  S := SemigroupOfCayleyDigraph(digraph);
  if not CanUseFroidurePin(S) or Size(S) > 26 then
    TryNextMethod();
  fi;

  vertex := function(x)
    local word, name, label;
    word  := MinimalFactorization(S, x);
    name  := SEMIGROUPS.WordToString(word);
    label := SEMIGROUPS.ExtRepObjToString(SEMIGROUPS.WordToExtRepObj(word));
    return Concatenation("  \\node [vertex] (", name, ") at (0, 0) {};\n",
                         "  \\node at (0, 0) {$", label, "$};\n\n");
  end;

  edge := function(from, to, gen)
    local word;
    word := MinimalFactorization(S, AsListCanonical(S)[from]);
    from := SEMIGROUPS.WordToString(word);
    word := MinimalFactorization(S, AsListCanonical(S)[to]);
    to   := SEMIGROUPS.WordToString(word);
    gen  := SEMIGROUPS.WordToString([gen]);
    if from <> to then
      return Concatenation("  \\path[->] (", from,
                           ") edge [edge] node {$", gen, "$} (",
                           to, ");\n");
    else
      return Concatenation("  \\path[->] (", from,
                           ") edge [loop]\n",
                           "           node {$", gen, "$} (", to, ");\n");
    fi;
  end;

  str := "";

  Append(str, "\\begin{tikzpicture}[scale=1, auto, \n");
  Append(str, "  vertex/.style={circle, draw, thick, fill=white, minimum");
  Append(str, " size=0.65cm},\n");
  Append(str, "  edge/.style={arrows={-angle 90}, thick},\n");
  Append(str, "  loop/.style={min distance=5mm,looseness=5,");
  Append(str, "arrows={-angle 90},thick}]\n\n");

  Append(str, "  % Vertices . . .\n");
  for x in AsListCanonical(S) do
    Append(str, vertex(x));
  od;

  Append(str, "  % Edges . . .\n");
  nbs := OutNeighbours(digraph);
  for from in [1 .. Size(S)] do
    for gen in [1 .. Size(nbs[from])] do
      Append(str, edge(from, nbs[from][gen], gen));
    od;
  od;

  Append(str, "\\end{tikzpicture}");
  return str;
end);

#############################################################################
# Dot for semigroups/monoids
#############################################################################

InstallMethod(DotString, "for a semigroup", [IsSemigroup],
S -> AsString(GraphvizDClasses(S)));

InstallMethod(DotString, "for a semigroup", [IsSemigroup, IsRecord],
{S, opts} -> AsString(GraphvizDClasses(S, opts)));

InstallMethod(GraphvizDClasses, "for a semigroup", [IsSemigroup],
S -> GraphvizDClasses(S, rec()));

SEMIGROUPS.HTMLTableOfDClass := function(D, n, R, struct, number)
  local mat, str, next, item, bgcolor, color, c, r;

  Assert(0, IsGreensDClass(D));

  mat := Matrix(R);

  str := "<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" ";
  Append(str, "CELLSPACING=\"0\" PORT=\"{}\">\n");

  str := StringFormatted(str, n);

  if number then
    next := "<TR BORDER=\"0\"><TD COLSPAN=\"{}\" BORDER = \"0\">{}</TD></TR>";
    Append(str, StringFormatted(next, NrRClasses(D), n));
  fi;

  for c in Columns(R) do
    Append(str, "<TR>");
    for r in Rows(R) do
      next := "<TD BGCOLOR=\"{}\"><font color=\"{}\">{}</font></TD>";
      item := "*";
      if mat[c][r] <> 0 then
        bgcolor := "gray";
        item := struct;
        color := "black";
      else
        bgcolor := "white";
        color := "white";
      fi;
      Append(str, StringFormatted(next, bgcolor, color, item));
    od;
    Append(str, "</TR>\n");
  od;
  Append(str, "</TABLE>>");
  return str;
end;

InstallMethod(GraphvizDClasses, "for a semigroup and record",
[IsSemigroup, IsRecord],
function(S, opts)
  local HTML, gv, i, ZeroMat, injection, RMS, sd, n, label, D, rel, j;

  # Process the options
  if not IsBound(opts.maximal) or not opts.maximal in [true, false] then
    opts.maximal := false;
  fi;
  if not IsBound(opts.number) or not opts.number in [true, false] then
    opts.number := true;
  fi;
  if not IsBound(opts.normal) or not opts.number in [true, false] then
    opts.normal := true;
  fi;

  HTML := SEMIGROUPS.HTMLTableOfDClass;

  gv := GraphvizDigraph("DClasses");
  GraphvizSetAttr(gv, "node [shape=plaintext]");
  GraphvizSetAttr(gv, "edge [color=black, arrowhead=none]");

  i := 0;

  ZeroMat := {rows, cols} -> List([1 .. rows], r -> [1 .. cols] * 0);

  if opts.normal then
    injection := InjectionNormalizedPrincipalFactor;
  else
    injection := InjectionPrincipalFactor;
  fi;
  RMS := function(D)
    if IsRegularDClass(D) then
      return Range(injection(D));
    else
      return ReesZeroMatrixSemigroup(Group(()),
               ZeroMat(NrLClasses(D), NrRClasses(D)));
    fi;
  end;

  for D in DClasses(S) do
    i := i + 1;
    if opts.maximal and IsRegularDClass(D) then
      sd := StructureDescription(GroupHClass(D));
    else
      sd := "*";
    fi;
    n := GraphvizAddNode(gv, i);
    GraphvizSetAttr(n, "shape", "box");
    GraphvizSetAttr(n, "style", "invisible");
    label := HTML(D, i, RMS(D), sd, opts.number);
    GraphvizSetAttr(n, "label", label);
  od;

  D := PartialOrderOfDClasses(S);
  rel := OutNeighbours(DigraphReflexiveTransitiveReduction(D));
  for i in [1 .. Length(rel)] do
    for j in rel[i] do
      GraphvizAddEdge(gv, i, j);
    od;
  od;

  return gv;
end);

InstallMethod(GraphvizSemilatticeOfIdempotents,
"for inverse semigroup with inverse op",
[IsInverseSemigroup and IsGeneratorsOfInverseSemigroup],
function(S)
  local E, U, gv, nr, V, cluster, D, v, u;

  E := IdempotentGeneratedSubsemigroup(S);
  U := Elements(E);

  gv := GraphvizGraph("semilattice");

  GraphvizSetAttr(gv, "node[shape=point]");
  GraphvizSetAttr(gv, "ranksep", "2");

  nr := 0;
  for D in GreensDClasses(S) do
    nr := nr + 1;
    V := List(Idempotents(D), x -> Position(U, x));
    cluster := GraphvizAddSubgraph(gv, StringFormatted("cluster_{}", nr));
    for v in V do
      GraphvizAddNode(cluster, v);
    od;
  od;

  D := DigraphNC(IsMutable, NaturalPartialOrder(E));
  DigraphReflexiveTransitiveReduction(D);

  for u in DigraphVertices(D) do
    for v in OutNeighboursOfVertexNC(D, u) do
      GraphvizAddEdge(gv, u, v);
    od;
  od;

  return gv;
end);

InstallMethod(DotSemilatticeOfIdempotents,
"for inverse semigroup with inverse op",
[IsInverseSemigroup and IsGeneratorsOfInverseSemigroup],
S -> AsString(GraphvizSemilatticeOfIdempotents(S)));

########################################################################
# DotLeft/RightCayleyDigraph
########################################################################

InstallMethod(DotLeftCayleyDigraph, "for a semigroup", [IsSemigroup],
S -> AsString(GraphvizLeftCayleyDigraph(S)));

InstallMethod(DotRightCayleyDigraph, "for a semigroup", [IsSemigroup],
S -> AsString(GraphvizRightCayleyDigraph(S)));

InstallMethod(DotLeftCayleyDigraph,
"for a semigroup and list of generator names",
[IsSemigroup, IsHomogeneousList],
{S, names} -> AsString(GraphvizLeftCayleyDigraph(S, names)));

InstallMethod(DotRightCayleyDigraph,
"for a semigroup and list of generator names",
[IsSemigroup, IsHomogeneousList],
{S, names} -> AsString(GraphvizRightCayleyDigraph(S, names)));

########################################################################
# GraphvizLeftCayleyDigraph
########################################################################

InstallMethod(GraphvizLeftCayleyDigraph,
"for a semigroup and list of generator names",
[IsSemigroup and CanUseFroidurePin, IsHomogeneousList],
function(S, names)
  local pos, ToExtRepObj, ToString, label;

  if Length(GeneratorsOfSemigroup(S)) <> Length(names) then
    ErrorNoReturn("the 2nd argument (list of names) must have ",
                  "length equal to Length(GeneratorsOfSemigroup(S)) = ",
                  Length(GeneratorsOfSemigroup(S)),
                  "where S is the 1st argument (a semigroup)",
                  ", but found ", Length(names));
  elif not IsString(names[1]) then
    ErrorNoReturn("the 2nd argument (list of names) must consist of ",
                  "strings, but found ", TNAM_OBJ(names[1]));
  fi;

  if IsMonoidAsSemigroup(S) then
    S := AsMonoid(S);
    pos := Position(GeneratorsOfSemigroup(S), One(S));
    if pos <> fail then
      names := ShallowCopy(names);
      Remove(names, pos);
    fi;
    return GraphvizLeftCayleyDigraph(S, names);
  fi;
  ToExtRepObj := SEMIGROUPS.WordToExtRepObj;
  ToString    := x -> SEMIGROUPS.ExtRepObjToString(x, names);
  label       := x -> ToString(ToExtRepObj(MinimalFactorization(S, x)));
  return GraphvizWordGraph(LeftCayleyDigraph(S),
                           List(S, label),
                           List(GeneratorsOfSemigroup(S), label));
end);

InstallMethod(GraphvizLeftCayleyDigraph, "for a monoid",
[IsMonoid and CanUseFroidurePin, IsHomogeneousList],
function(S, names)
  local label, pos, D, x;

  if Length(GeneratorsOfMonoid(S)) <> Length(names) then
    ErrorNoReturn("the 2nd argument (list of names) must have ",
                  "length equal to Length(GeneratorsOfMonoid(S)) = ",
                  Length(GeneratorsOfMonoid(S)),
                  "where S is the 1st argument (a monoid)",
                  ", but found ", Length(names));
  elif not IsString(names[1]) then
    ErrorNoReturn("the 2nd argument (list of names) must consist of ",
                  "strings, but found ", TNAM_OBJ(names[1]));
  fi;

  Info(InfoWarning,
       1,
       "GraphvizLeftCayleyDigraph: using GeneratorsOfMonoid ",
       "instead of GeneratorsOfSemigroup");

  label := SEMIGROUPS.HTMLSemigroupElement(S, names);

  # TODO replace with LeftCayleyDigraph(S, SomeElements) method instead
  pos := Position(GeneratorsOfSemigroup(S), One(S));
  D := LeftCayleyDigraph(S);
  if pos <> fail then
    D := DigraphMutableCopy(D);
    for x in OutNeighbours(D) do
      Remove(x, pos);
    od;
    MakeImmutable(D);
  fi;
  return GraphvizWordGraph(D,
                           List(S, label),
                           List(GeneratorsOfMonoid(S), label));
end);

InstallMethod(GraphvizLeftCayleyDigraph, "for a semigroup", [IsSemigroup],
function(S)
  local names;

  names := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  names := List([1 .. Size(GeneratorsOfSemigroup(S))], x -> [names[x]]);

  Info(InfoWarning, 1, "GraphvizLeftCayleyDigraph: no generator labels ",
       "specified, using ", names, " for GeneratorsOfSemigroup");

  return GraphvizLeftCayleyDigraph(S, names);
end);

# The next method doesn't really need to exist but the info warnings make more
# sense with it.
InstallMethod(GraphvizLeftCayleyDigraph, "for a monoid", [IsMonoid],
function(S)
  local names;

  names := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  names := List([1 .. Size(GeneratorsOfMonoid(S))], x -> [names[x]]);

  Info(InfoWarning, 1, "GraphvizLeftCayleyDigraph: no generator labels ",
       "specified, using ", names, " for GeneratorsOfMonoid");

  return GraphvizLeftCayleyDigraph(S, names);
end);

# This method exists so that we use the same letters as S in the output.
InstallMethod(GraphvizLeftCayleyDigraph, "for an fp semigroup", [IsFpSemigroup],
function(S)
  local label;

  label := x -> ReplacedString(String(x), "*", "");
  return GraphvizWordGraph(LeftCayleyDigraph(S),
                           List(S, label),
                           List(GeneratorsOfSemigroup(S), label));
end);

SEMIGROUPS.HTMLSemigroupElement := function(S, names)
  if IsFpMonoid(S) or IsFpSemigroup(S) then
    return function(x)
      if IsOne(x) then
        return "&#949;";
      fi;
      return ReplacedString(String(x), "*", "");
    end;
  else
    return function(x)
      local ToExtRepObj, ToString;
      ToExtRepObj := SEMIGROUPS.WordToExtRepObj;
      ToString    := x -> SEMIGROUPS.ExtRepObjToString(x, names);
      if IsOne(x) then
        return "&#949;";
      fi;
      return ToString(ToExtRepObj(MinimalMonoidFactorization(S, x)));
    end;
  fi;
end;

# This method exists so that we use the same letters as S in the output.
InstallMethod(GraphvizLeftCayleyDigraph, "for an fp monoid", [IsFpMonoid],
function(S)
  local label, D, pos, x;

  Info(InfoWarning,
       1,
       "GraphvizLeftCayleyDigraph: using GeneratorsOfMonoid ",
       "instead of GeneratorsOfSemigroup");

  label := SEMIGROUPS.HTMLSemigroupElement(S, fail);

  # TODO replace with LeftCayleyDigraph(S, SomeElements) method instead
  D   := DigraphMutableCopy(LeftCayleyDigraph(S));
  pos := Position(GeneratorsOfSemigroup(S), One(S));
  for x in OutNeighbours(D) do
    Remove(x, pos);
  od;
  MakeImmutable(D);

  return GraphvizWordGraph(D,
                           List(S, label),
                           List(GeneratorsOfMonoid(S), label));
end);

########################################################################
# GraphvizRightCayleyDigraph
########################################################################

InstallMethod(GraphvizRightCayleyDigraph,
"for a semigroup and list of generator names",
[IsSemigroup and CanUseFroidurePin, IsHomogeneousList],
function(S, names)
  local pos, ToExtRepObj, ToString, label;

  if Length(GeneratorsOfSemigroup(S)) <> Length(names) then
    ErrorNoReturn("the 2nd argument (list of names) must have ",
                  "length equal to Length(GeneratorsOfSemigroup(S)) = ",
                  Length(GeneratorsOfSemigroup(S)),
                  "where S is the 1st argument (a semigroup)",
                  ", but found ", Length(names));
  elif not IsString(names[1]) then
    ErrorNoReturn("the 2nd argument (list of names) must consist of ",
                  "strings, but found ", TNAM_OBJ(names[1]));
  fi;

  if IsMonoidAsSemigroup(S) then
    S := AsMonoid(S);
    pos := Position(GeneratorsOfSemigroup(S), One(S));
    if pos <> fail then
      names := ShallowCopy(names);
      Remove(names, pos);
    fi;
    return GraphvizRightCayleyDigraph(S, names);
  fi;
  ToExtRepObj := SEMIGROUPS.WordToExtRepObj;
  ToString    := x -> SEMIGROUPS.ExtRepObjToString(x, names);
  label       := x -> ToString(ToExtRepObj(MinimalFactorization(S, x)));
  return GraphvizWordGraph(RightCayleyDigraph(S),
                           List(S, label),
                           List(GeneratorsOfSemigroup(S), label));
end);

InstallMethod(GraphvizRightCayleyDigraph, "for a monoid",
[IsMonoid and CanUseFroidurePin, IsHomogeneousList],
function(S, names)
  local label, pos, D, x;

  if Length(GeneratorsOfMonoid(S)) <> Length(names) then
    ErrorNoReturn("the 2nd argument (list of names) must have ",
                  "length equal to Length(GeneratorsOfMonoid(S)) = ",
                  Length(GeneratorsOfMonoid(S)),
                  "where S is the 1st argument (a monoid)",
                  ", but found ", Length(names));
  elif not IsString(names[1]) then
    ErrorNoReturn("the 2nd argument (list of names) must consist of ",
                  "strings, but found ", TNAM_OBJ(names[1]));
  fi;

  Info(InfoWarning,
       1,
       "GraphvizRightCayleyDigraph: using GeneratorsOfMonoid ",
       "instead of GeneratorsOfSemigroup");

  label := SEMIGROUPS.HTMLSemigroupElement(S, names);
  # TODO replace with RightCayleyDigraph(S, SomeElements) method instead
  pos := Position(GeneratorsOfSemigroup(S), One(S));
  D := RightCayleyDigraph(S);
  if pos <> fail then
    D := DigraphMutableCopy(D);
    for x in OutNeighbours(D) do
      Remove(x, pos);
    od;
    MakeImmutable(D);
  fi;
  return GraphvizWordGraph(D,
                           List(S, label),
                           List(GeneratorsOfMonoid(S), label));
end);

InstallMethod(GraphvizRightCayleyDigraph, "for a semigroup", [IsSemigroup],
function(S)
  local names;

  names := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  names := List([1 .. Size(GeneratorsOfSemigroup(S))], x -> [names[x]]);

  Info(InfoWarning, 1, "GraphvizRightCayleyDigraph: no generator labels ",
       "specified, using ", names, " for GeneratorsOfSemigroup");

  return GraphvizRightCayleyDigraph(S, names);
end);

# The next method doesn't really need to exist but the info warnings make more
# sense with it.
InstallMethod(GraphvizRightCayleyDigraph, "for a monoid", [IsMonoid],
function(S)
  local names;

  names := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  names := List([1 .. Size(GeneratorsOfMonoid(S))], x -> [names[x]]);

  Info(InfoWarning, 1, "GraphvizRightCayleyDigraph: no generator labels ",
       "specified, using ", names, " for GeneratorsOfMonoid");

  return GraphvizRightCayleyDigraph(S, names);
end);

# This method exists so that we use the same letters as S in the output.
InstallMethod(GraphvizRightCayleyDigraph, "for an fp semigroup", [IsFpSemigroup],
function(S)
  local label;

  label := x -> ReplacedString(String(x), "*", "");
  return GraphvizWordGraph(RightCayleyDigraph(S),
                           List(S, label),
                           List(GeneratorsOfSemigroup(S), label));
end);

# This method exists so that we use the same letters as S in the output.
InstallMethod(GraphvizRightCayleyDigraph, "for an fp monoid", [IsFpMonoid],
function(S)
  local label, D, pos, x;

  Info(InfoWarning,
       1,
       "GraphvizRightCayleyDigraph: using GeneratorsOfMonoid ",
       "instead of GeneratorsOfSemigroup");

  label := SEMIGROUPS.HTMLSemigroupElement(S, fail);

  # TODO replace with RightCayleyDigraph(S, SomeElements) method instead
  D   := DigraphMutableCopy(RightCayleyDigraph(S));
  pos := Position(GeneratorsOfSemigroup(S), One(S));
  for x in OutNeighbours(D) do
    Remove(x, pos);
  od;
  MakeImmutable(D);

  return GraphvizWordGraph(D,
                           List(S, label),
                           List(GeneratorsOfMonoid(S), label));
end);

########################################################################
# GraphvizWordGraph
########################################################################

InstallMethod(GraphvizWordGraph,
"for a word graph, list of node labels, and list of edge labels",
[IsDigraph, IsHomogeneousList, IsHomogeneousList],
function(D, node_labels, edge_labels)
  local M, N, edge_colors, gv, pos, legend, start_table, end_table, row, label,
  e, m, i;

  if not DigraphHasAVertex(D) then
    # Word graphs must have at least one node . . .
    ErrorNoReturn("the 1st argument (a digraph) must have at least 1 vertex");
  elif not IsOutRegularDigraph(D) then
    ErrorNoReturn("the 1st argument (a digraph) must be out-regular");
  fi;

  M := DigraphNrVertices(D);
  N := Length(OutNeighboursOfVertex(D, 1));
  if Length(node_labels) <> M then
    ErrorFormatted("expected the 2nd argument (list of node labels) ",
                   "to have length {}, but found {}",
                   M,
                   Length(node_labels));
  elif not IsString(node_labels[1]) then
    ErrorFormatted("expected the 2nd argument (list of node labels) ",
                   "to consist of strings, found {}",
                   TNAM_OBJ(node_labels[1]));
  elif Length(edge_labels) <> N then
    ErrorFormatted("expected the 3rd argument (list of edge labels) ",
                   "to have length {}, but found {}",
                   N,
                   Length(edge_labels));
  elif not IsString(edge_labels[1]) then
    ErrorFormatted("expected the 3rd argument (list of edge labels) ",
                   "to consist of strings, found {}",
                   TNAM_OBJ(node_labels[1]));
  fi;

  # TODO longer list here
  edge_colors := ["#00ff00", "#ff00ff", "#007fff", "#ff7f00",
                  "#7fbf7f", "#4604ac", "#de0328", "#19801d",
                  "#d881f5", "#00ffff", "#ffff00", "#00ff7f",
                  "#ad5867", "#85f610", "#84e9f5", "#f5c778",
                  "#207090", "#764ef3", "#7b4c00", "#0000ff",
                  "#b80c9a", "#601045", "#29b7c0", "#839f12"];

  if N > Length(edge_colors) then
    ErrorFormatted("the 1st argument (an out-regular digraph) must have ",
                   "out-degree at most {}, found {}",
                   Length(edge_colors),
                   N);
  fi;

  edge_colors := edge_colors{[1 .. N]};
  # TODO some of the checks above are probably duplicated in SetNodeLabels, and
  # in GraphvizEdgeColoredDigraph
  gv := GraphvizEdgeColoredDigraph(D,
                                   ListWithIdenticalEntries(DigraphNrVertices(D),
                                                            edge_colors));
  GraphvizSetNodeLabels(gv, node_labels);
  GraphvizSetAttr(gv, "node [shape=\"box\"]");
  for m in [1 .. M] do
    pos := Position(edge_labels, node_labels[m]);
    if pos <> fail then
      GraphvizSetAttr(GraphvizNode(gv, m), "color", edge_colors[pos]);
      GraphvizSetAttr(GraphvizNode(gv, m), "style", "filled");
    fi;
  od;

  legend := GraphvizAddContext(gv, "legend");
  GraphvizSetAttr(legend, "node [shape=plaintext]");
  legend := GraphvizAddSubgraph(legend, "legend");

  start_table := Concatenation("<<table border=\"0\" cellpadding=\"2\"",
                               " cellspacing=\"0\"",
                               " cellborder=\"0\">\n");
  end_table := "</table>>\n";

  row := {index, content} -> StringFormatted(
      "<tr><td align=\"right\" port=\"port{}\">{}&nbsp;</td></tr>\n",
      index,
      content);

  # HTML table for the head of the arrows in the legend
  label := Concatenation(ShallowCopy(start_table),
                         Concatenation(List([1 .. N],
                                            i -> row(i, edge_labels[i]))),
                         end_table);
  GraphvizSetAttr(GraphvizAddNode(legend, "head"), "label", label);

  # HTML table for the tail of the arrows in the legend
  label := Concatenation(start_table,
                         Concatenation(List([1 .. N], i -> row(i, ""))),
                         end_table);
  GraphvizSetAttr(GraphvizAddNode(legend, "tail"), "label", label);

  # The actual arrows from the head to the tail
  for i in [1 .. N] do
    e := GraphvizAddEdge(legend,
                         StringFormatted("head:port{}:e", i),
                         StringFormatted("tail:port{}:w", i));
    GraphvizSetAttr(e, "color", edge_colors[i]);
    GraphvizSetAttr(e, "constraint", false);
  od;

  return gv;
end);
