#############################################################################
##
##  tools/display.gi
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#############################################################################

SEMIGROUPS.GetTikzInit := function(opts)
  local extra;
  if IsBound(opts.extraInit) then
    extra := opts.extraInit;
  else
    extra := "";
  fi;

  return StringFormatted("1\n2\n3\n4\n5\n",
                         "%latex",
                         "\\documentclass{minimal}",
                         "\\usepackage{tikz}",
                         extra,
                         "\\begin{document}");
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

InstallMethod(DotString, "for a semigroup", [IsSemigroup],
S -> DotString(S, rec()));

InstallMethod(DotString, "for a semigroup and record",
[IsSemigroup, IsRecord],
function(S, opts)
  local es, elts, str, i, color, pos, gp, iso, inv, RMS, mat, G, x, D, rel, ii,
  di, j, dk, k, d, l, col, row;

  # process the options
  if not IsBound(opts.maximal) then
    opts.maximal := false;
  fi;
  if not IsBound(opts.number) then
    opts.number := true;
  fi;
  if not IsBound(opts.normal) then
    opts.normal := true;
  fi;
  if not IsBound(opts.highlight) then
    opts.highlight := false;  # JDM means highlight H-classes
  else
    for x in opts.highlight do
      if not IsBound(x.HighlightGroupHClassColor) then
        x.HighlightGroupHClassColor := "#880000";
      fi;

      if not IsBound(x.HighlightNonGroupHClassColor) then
        x.HighlightNonGroupHClassColor := "#FF0000";
      fi;
    od;
  fi;

  if not IsBound(opts.idempotentsemilattice) then
    opts.idempotentsemilattice := false;
  elif opts.idempotentsemilattice then
    es := IdempotentGeneratedSubsemigroup(S);
    elts := Elements(es);  # JDM could be enumerator sorted
  fi;

  str := "//dot\n";
  Append(str, "digraph  DClasses {\n");
  Append(str, "node [shape=plaintext]\n");
  Append(str, "edge [color=black,arrowhead=none]\n");
  i := 0;

  for d in DClasses(S) do

    i := i + 1;
    Append(str, String(i));
    Append(str, " [shape=box style=invisible ");
    Append(str, "label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\"");
    Append(str, " CELLPADDING=\"10\" CELLSPACING=\"0\"");
    Append(str, Concatenation(" PORT=\"", String(i), "\">\n"));

    if opts.number then
      Append(str, "<TR BORDER=\"0\"><TD COLSPAN=\"");
      Append(str, String(NrRClasses(d)));
      Append(str, "\" BORDER = \"0\" > ");
      Append(str, String(i));
      Append(str, "</TD></TR>");
    fi;

    if not IsRegularDClass(d) then
      for l in LClasses(d) do
        Append(str, "<TR>");
        for x in HClasses(l) do
          color := "white";
          if opts.highlight <> false then
            pos := PositionProperty(opts.highlight,
                                    record -> x in record.HClasses);
            if pos <> fail then
              color := opts.highlight[pos].HighlightNonGroupHClassColor;
            fi;
          fi;
          Append(str, Concatenation("<TD CELLPADDING=\"10\" BGCOLOR=\"",
                                    color,
                                    "\"><font color=\"white\">*</font></TD>"));
        od;
        Append(str, "</TR>\n");
      od;
      Append(str, "</TABLE>>];\n");
      continue;
    fi;

    if opts.maximal then
      gp := StructureDescription(GroupHClass(d));
    fi;

    if opts.normal then
      iso := InjectionNormalizedPrincipalFactor(d);
    else
      iso := InjectionPrincipalFactor(d);
    fi;
    inv := InverseGeneralMapping(iso);
    RMS := Range(iso);
    mat := Matrix(RMS);
    G := UnderlyingSemigroup(RMS);

    for col in Columns(RMS) do  # Columns of RMS = RClasses
      Append(str, "<TR>");
      for row in Rows(RMS) do  # Rows of RMS = LClasses
        Append(str, "<TD BGCOLOR=\"");
        if opts.highlight <> false then
          x := HClass(d, RMSElement(RMS, row, Identity(G), col) ^ inv);
          pos := PositionProperty(opts.highlight, rcd -> x in rcd.HClasses);
        fi;
        if mat[col][row] <> 0 then
          # group H-class
          if opts.highlight <> false and pos <> fail then
            color := opts.highlight[pos].HighlightGroupHClassColor;
          else
            color := "gray";
          fi;
          Append(str, color);
          Append(str, "\"");
          if opts.maximal then
            Append(str, ">");
            Append(str, gp);
          else
            if opts.idempotentsemilattice then
              x := HClass(d, RMSElement(RMS, row, Identity(G), col) ^ inv);
              Append(str, " PORT=\"e");
              Append(str, String(Position(elts, Idempotents(x)[1])));
              Append(str, "\"");
            fi;
            Append(str, ">*");
          fi;
        else
          # non-group H-class
          if opts.highlight <> false and pos <> fail then
            color := opts.highlight[pos].HighlightNonGroupHClassColor;
          else
            color := "white";
          fi;
          Append(str, color);
          Append(str, "\">");
        fi;
        Append(str, "</TD>");
      od;
      Append(str, "</TR>\n");
    od;
    Append(str, "</TABLE>>];\n");
  od;
  D := PartialOrderOfDClasses(S);
  rel := OutNeighbours(DigraphReflexiveTransitiveReduction(D));
  for i in [1 .. Length(rel)] do
    ii := String(i);
    for k in rel[i] do
      Append(str, Concatenation(ii, " -> ", String(k), "\n"));
    od;
  od;

  # add semilattice of idempotents
  if opts.idempotentsemilattice then
    Append(str, "edge [color=blue,arrowhead=none,style=dashed]\n");
    rel := NaturalPartialOrder(es);

    for i in [1 .. Length(rel)] do
      di := String(Position(DClasses(S), DClass(S, elts[i])));
      j := Difference(rel[i], Union(rel{rel[i]}));
      i := String(i);
      for k in j do
        dk := String(Position(DClasses(S), DClass(S, elts[k])));
        k := String(k);
        Append(str, Concatenation(di, ":e", i, " -> ", dk, ":e", k, "\n"));
      od;
    od;
  fi;
  Append(str, " }");

  return str;
end);

InstallMethod(DotSemilatticeOfIdempotents,
"for inverse semigroup with inverse op",
[IsInverseSemigroup and IsGeneratorsOfInverseSemigroup],
function(S)
  local U, rel, elts, str, nr, V, j, i, k, D, v;

  U := IdempotentGeneratedSubsemigroup(S);
  rel := NaturalPartialOrder(U);
  elts := Elements(U);

  str := "//dot\n";

  Append(str, "graph graphname {\n  node [shape=point]\n");
  Append(str, "ranksep=2;\n");

  nr := 0;
  for D in GreensDClasses(S) do
    nr := nr + 1;
    V := List(Idempotents(D), x -> String(Position(elts, x)));
    Append(str, Concatenation("subgraph cluster_", String(nr), "{\n"));
    for v in V do
      Append(str, v);
      Append(str, " ");
    od;
    Append(str, "\n");
    Append(str, "}\n");
  od;

  for i in [1 .. Length(rel)] do
    j := Difference(rel[i], Union(rel{rel[i]}));
    i := String(i);
    for k in j do
      k := String(k);
      Append(str, Concatenation(i, " -- ", k, "\n"));
    od;
  od;

  Append(str, " }");

  return str;
end);

InstallMethod(TexString, "for a transformation and a pos int",
[IsTransformation, IsPosInt],
function(f, deg)
  local str, i;

  if deg < DegreeOfTransformation(f) then
    ErrorNoReturn("the 2nd argument (a pos. int.) is less than ",
                  "the degree of the 1st argument (a ",
                  "transformation)");
  fi;
  str := "\\begin{pmatrix}\n  ";
  for i in [1 .. deg] do
    Append(str, String(i));
    if i <> deg then
      Append(str, " & ");
    fi;
  od;
  Append(str, " \\\\\n  ");
  for i in [1 .. deg] do
    Append(str, String(i ^ f));
    if i <> deg then
      Append(str, " & ");
    fi;
  od;
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

InstallMethod(DotString, "for a Cayley digraph", [IsCayleyDigraph],
function(D)
  local S, edge_colors, N, msg, legend, node_labels, offset, en, label, next,
  edge_func, node_func, result, i;

  # TODO replace with an appropriate call to DotWordGraph

  S := SemigroupOfCayleyDigraph(D);
  edge_colors := ["\"#00ff00\"", "\"#ff00ff\"", "\"#007fff\"", "\"#ff7f00\"",
                  "\"#7fbf7f\"", "\"#4604ac\"", "\"#de0328\"", "\"#19801d\"",
                  "\"#d881f5\"", "\"#00ffff\"", "\"#ffff00\"", "\"#00ff7f\"",
                  "\"#ad5867\"", "\"#85f610\"", "\"#84e9f5\"", "\"#f5c778\"",
                  "\"#207090\"", "\"#764ef3\"", "\"#7b4c00\"", "\"#0000ff\"",
                  "\"#b80c9a\"", "\"#601045\"", "\"#29b7c0\"", "\"#839f12"];
  N := Length(GeneratorsOfSemigroup(S));
  if not CanUseFroidurePin(S) then
    TryNextMethod();
  fi;

  if IsMonoidAsSemigroup(S) and One(GeneratorsOfSemigroup(S)) in S then
    offset := -1;
  else
    offset := 1;
  fi;

  node_labels := [];
  en := EnumeratorCanonical(S);
  for i in [1 .. Size(S)] do
    if IsOne(en[i]) then
      label := "&#949;";
    else
      # TODO maybe reuse some of this it seems to give better factorisations,
      # than the DotRightCayleyDigraph method below
      label := SEMIGROUPS.WordToExtRepObj(MinimalFactorization(S, i) + offset);
      label := SEMIGROUPS.ExtRepObjToString(label);
    fi;
    node_labels[i] := label;
  od;

  legend := "node [shape=plaintext]\nsubgraph cluster_01 {\nlabel=\"Legend\"\n";
  Append(legend, "key2 [label=<<table border=\"0\" cellpadding=\"2\"");
  Append(legend, " cellspacing=\"0\" cellborder=\"0\">\n");

  for i in [1 .. N] do
    Append(legend,
           StringFormatted("  <tr><td port=\"i{}\">&nbsp;</td></tr>\n",
                           i));
  od;
  Append(legend, "</table>>]\n");
  Append(legend, "key [label=<<table border=\"0\" cellpadding=\"2\"");
  Append(legend, " cellspacing=\"0\" cellborder=\"0\">\n");

  for i in [1 .. N] do
    label := node_labels[i];
    next := "<tr><td align=\"right\" ";
    Append(next, StringFormatted("port=\"i{}\">{}&nbsp;</td></tr>\n", i, label));
    Append(legend, next);
  od;

  Append(legend, "</table>>]\n\n");

  for i in [1 .. N] do
    next := StringFormatted("key:i{1}:e -> key2:i{1}:w [color={2},",
                            i,
                            edge_colors[i]);
    Append(next, "constraint=false]\n");
    Append(legend, next);
  od;
  Append(legend, "}\n");

  node_func := i -> StringFormatted(" [label=\"{}\"]", node_labels[i]);
  edge_func := {i, j} -> StringFormatted("[color={}]", edge_colors[j]);

  result := DIGRAPHS_DotDigraph(D, [node_func], [edge_func]);
  result := SplitString(result, "\n");
  result[3] := "subgraph 00 {";
  Add(result, "node [shape=box]", 3);
  Add(result, legend);
  Add(result, "}");
  return JoinStringsWithSeparator(result, "\n");
end);

InstallMethod(DotLeftCayleyDigraph, "for a semigroup", [IsSemigroup],
function(S)
  local label;
  label := x -> SEMIGROUPS.WordToString(MinimalFactorization(S, x));
  return DotWordGraph(LeftCayleyDigraph(S),
                      List(S, label),
                      List(GeneratorsOfSemigroup(S), label));
end);

InstallMethod(DotRightCayleyDigraph, "for a semigroup", [IsSemigroup],
function(S)
  local label;

  # TODO handle monoids
  # ToExtRepObj := SEMIGROUPS.WordToExtRepObj;
  # ToString    := SEMIGROUPS.ExtRepObjToString;
  # label := x -> ToString(ToExtRepObj(MinimalFactorization(S, x)));
  # Current the above approach doesn't work becase labels aren't put into
  # quotes " by default and so an error is given for this at present.
  # TODO delete the next line when graphviz is fixed.
  label := x -> SEMIGROUPS.WordToString(MinimalFactorization(S, x));
  return DotWordGraph(RightCayleyDigraph(S),
                      List(S, label),
                      List(GeneratorsOfSemigroup(S), label));
end);

InstallMethod(DotWordGraph,
"for a word graph, list of node labels, and list of edge labels",
[IsDigraph, IsHomogeneousList, IsHomogeneousList],
function(D, node_labels, edge_labels)
  local M, N, msg, edge_colors, dot, mm, pos, targets, e, legend, subgraph,
  key, key2, label, next, str, m, n, i;

  # TODO have some options, like include the legend, maybe fancy labels

  if not DigraphHasAVertex(D) then
    # Word graphs must have at least one node . . .
    ErrorNoReturn("The 1st argument (a digraph) must have at least one vertex");
  elif not IsOutRegularDigraph(D) then
    ErrorNoReturn("The 1st argument (a digraph) must be out-regular");
  fi;

  M := DigraphNrVertices(D);
  N := Length(OutNeighboursOfVertex(D, 1));
  if Length(node_labels) <> M then
    msg := "Expected the 2nd argument (a list) to have length ";
    Append(msg, "{}, but found {}");
    ErrorNoReturn(StringFormatted(msg), M, Length(node_labels));
  elif not IsString(node_labels[1]) then
    ErrorNoReturn("TODO");
  elif Length(edge_labels) <> N then
    msg := "Expected the 3rd argument (a list) to have length ";
    Append(msg, "{}, but found {}");
    ErrorNoReturn(msg, M, Length(edge_labels));
  elif not IsString(edge_labels[1]) then
    ErrorNoReturn("TODO");
  fi;

  # TODO longer list here
  edge_colors := ["#00ff00", "#ff00ff", "#007fff", "#ff7f00",
                  "#7fbf7f", "#4604ac", "#de0328", "#19801d",
                  "#d881f5", "#00ffff", "#ffff00", "#00ff7f",
                  "#ad5867", "#85f610", "#84e9f5", "#f5c778",
                  "#207090", "#764ef3", "#7b4c00", "#0000ff",
                  "#b80c9a", "#601045", "#29b7c0", "#839f12"];

  if N > Length(edge_colors) then
    msg := Concatenation("the out-degree of every vertex in the 1st argument ",
                         "(a digraph) must be at most {}, found {}");
    ErrorNoReturn(StringFormatted(msg, Length(edge_colors), N));
  fi;

  dot := GV_Digraph("WordGraph");
  GV_SetAttr(dot, "node [shape=\"box\"]");
  for m in [1 .. M] do
    mm := GV_AddNode(dot, m);
    GV_SetAttr(mm, "label", node_labels[m]);
    pos := Position(edge_labels, node_labels[m]);
    if pos <> fail then
      GV_SetAttr(mm, "color", edge_colors[pos]);
      GV_SetAttr(mm, "style", "filled");
    fi;
  od;

  for m in [1 .. M] do
    targets := OutNeighboursOfVertex(D, m);
    for n in [1 .. N] do
      e := GV_AddEdge(dot, m, targets[n]);
      GV_SetAttr(e, "color", edge_colors[n]);
    od;
  od;

  legend := GV_AddContext(dot, "legend");
  GV_SetAttr(legend, "node [shape=plaintext]");
  subgraph := GV_AddSubgraph(legend, "legend");
  key := GV_AddNode(subgraph, "key");
  key2 := GV_AddNode(subgraph, "key2");

  label := Concatenation("<<table border=\"0\" cellpadding=\"2\"",
                         " cellspacing=\"0\"",
                         " cellborder=\"0\">\n");
  for i in [1 .. N] do
    Append(label,
           StringFormatted("  <tr><td port=\"i{}\">&nbsp;</td></tr>\n",
                           i));
  od;
  Append(label, "</table>>\n");
  GV_SetAttr(key2, "label", label);

  # TODO remove code dupl
  label := Concatenation("<<table border=\"0\" cellpadding=\"2\"",
                         " cellspacing=\"0\"",
                         " cellborder=\"0\">\n");
  for i in [1 .. N] do
    next := "    <tr><td align=\"right\" ";
    Append(next, StringFormatted("port=\"i{}\">{}&nbsp;</td></tr>\n",
                                 i,
                                 edge_labels[i]));
    Append(label, next);
  od;
  Append(label, "</table>>\n");

  GV_SetAttr(key, "label", label);

  for i in [1 .. N] do
    e := GV_AddEdge(subgraph,
                    StringFormatted("key:i{}:e", i),
                    StringFormatted("key2:i{}:w", i));
    GV_SetAttr(e, "color", edge_colors[i]);
    GV_SetAttr(e, "constraint", false);
  od;

  str := GV_String(dot);
  # TODO remove the next two lines, these work around some issues in graphviz
  str := ReplacedString(str, "--", "->");
  return Concatenation("//dot\n", str);
end);
