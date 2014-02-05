#############################################################################
##
#W  display.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(DotDClasses, "for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup], 
function(S)
  if IsActingSemigroup(S) then 
    TryNextMethod();
  fi;
  return DotDClasses(Semigroup(GeneratorsOfSemigroup(S)));
end);

#

InstallMethod(DotDClasses, "for an acting semigroup",
[IsActingSemigroup], 
function(s)
  return DotDClasses(s, rec());
end);

InstallMethod(DotDClasses, "for an acting semigroup and record",
[IsActingSemigroup, IsRecord],
function(s, opts)
  local es, elts, str, i, gp, h, rel, j, k, di, dk, d, l, x;

  # process the options
  if not IsBound(opts.maximal) then 
    opts.maximal:=false;
  fi;
  if not IsBound(opts.number) then 
    opts.number:=true;
  fi;
  if not IsBound(opts.idempotentsemilattice) then 
    opts.idempotentsemilattice:=false;
  elif opts.idempotentsemilattice then 
    es:=IdempotentGeneratedSubsemigroup(s);
    elts:=Elements(es); #JDM could be enumerator sorted
  fi;

  str:="";
  Append(str, "digraph  DClasses {\n");
  Append(str, "node [shape=plaintext]\n");
  Append(str, "edge [color=red,arrowhead=none]\n");
  i:=0;

  for d in DClasses(s) do
    i:=i+1;
    Append(str, String(i));
    Append(str, " [shape=box style=dotted label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\"");
    Append(str, " CELLPADDING=\"10\" CELLSPACING=\"0\"");
    Append(str, Concatenation(" PORT=\"", String(i), "\">\n"));

    if opts.number then
      Append(str, "<TR BORDER=\"0\"><TD COLSPAN=\"");
      Append(str, String(NrRClasses(d)));
      Append(str, "\" BORDER=\"0\" >");
      Append(str, String(i));
      Append(str, "</TD></TR>");
    fi;

    if opts.maximal and IsRegularDClass(d) then
       gp:=StructureDescription(GroupHClass(d));
    fi;
    
    for l in LClasses(d) do
      Append(str, "<TR>");
      if not IsRegularClass(l) then
        for j in [1..NrRClasses(d)] do
          Append(str, "<TD CELLPADDING=\"10\"> </TD>");
        od;
      else
        h:=HClasses(l);
        for x in h do
          if IsGroupHClass(x) then
            if opts.maximal then
              Append(str, Concatenation("<TD BGCOLOR=\"grey\">", gp, "</TD>"));
            else
              Append(str, "<TD BGCOLOR=\"grey\"");
              if opts.idempotentsemilattice then 
                Append(str, Concatenation(" PORT=\"e", String(Position(elts,
                 Idempotents(x)[1])), "\""));
              fi; 
              Append(str, ">*</TD>");
            fi;
          else
            Append(str, "<TD></TD>");
          fi;
        od;
      fi;
      Append(str, "</TR>\n");
    od;
    Append(str, "</TABLE>>];\n");
  od;

  rel:=PartialOrderOfDClasses(s);
  rel:=List([1..Length(rel)], x-> Filtered(rel[x], y-> not x=y));

  for i in [1..Length(rel)] do
    j:=Difference(rel[i], Union(rel{rel[i]})); i:=String(i);
    for k in j do
      k:=String(k);
      Append(str, Concatenation(i, " -> ", k, "\n"));
    od;
  od;

  # add semilattice of idempotents
  if opts.idempotentsemilattice then 
    Append(str, "edge [color=blue,arrowhead=none,style=dashed]\n");
    rel:=NaturalPartialOrder(es);
    
    for i in [1..Length(rel)] do 
      di:=String(Position(DClasses(s), DClass(s, elts[i]))); 
      j:=Difference(rel[i], Union(rel{rel[i]})); i:=String(i);
      for k in j do 
        dk:=String(Position(DClasses(s), DClass(s, elts[k]))); 
        k:=String(k);
        Append(str, Concatenation(di, ":e", i, " -> ", dk, ":e", k, "\n"));
      od;
    od;
  fi;
  Append(str, " }");

  return str;
end);

InstallMethod(DotSemilatticeOfIdempotents,
"for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(S)
  local U, rel, elts, str, nr, V, j, i, k, D, v;

  U:=IdempotentGeneratedSubsemigroup(S);
  rel:=NaturalPartialOrder(U);
  elts:=Elements(U); 

  str:="";

  #if Length(rel)<20 then
  #  Append(str,  "graph graphname {\n  node [shape=circle]\n");
  #else
    Append(str,  "graph graphname {\n  node [shape=point]\n");
  #fi;
  Append(str, "ranksep=2;\n");

  nr:=0;
  for D in GreensDClasses(S) do 
    nr:=nr+1;
    V:=List(Idempotents(D), x-> String(Position(elts, x))); 
    Append(str, Concatenation("subgraph cluster_", String(nr), "{\n"));
    for v in V do 
      Append(str, v);
      Append(str, " ");
    od;
    Append(str, "\n");
    Append(str, "}\n");
  od;

  for i in [1..Length(rel)] do
    j:=Difference(rel[i], Union(rel{rel[i]})); i:=String(i);
    for k in j do
      k:=String(k);
      Append(str, Concatenation(i, " -- ", k, "\n"));
    od;
  od;

  Append(str, " }");

  return str;
end); 

#EOF
