

#

Find@:=function(B, x)
  local r;
  r:= x;
  while B[r]<>r do 
    r := B[r];  #remonte l'arbre de x
  od;

  while B[x]<>x do 
    B[x] := r;  #x devient fils de r 
    x := B[x];
  od;
  return r;
end;

#

Union@:=function(B, S, x, y)
  local r, s;
  r:=Find@(B, x);
  s:=Find@(B, y);
  if S[r] > S[s] then 
    B[s]:=r;
    S[r]:=S[r] + S[s];
  else
    B[r]:=s;
    S[s]:=S[r]+S[s];
  fi;
  return;     
end;

# The calculation consists of finding the connected components of the bipartite
# graph with vertices which are the R-classes and L-classes of S and an edge
# from <r> to <l> if the intersection of <r> and <l> contains an idempotent. 

#

GrahamBlocks:=function(s)
  local len, B, S, data, nr, e, r, l, i;

  len:=NrRClasses(s)+NrLClasses(s);
  B:=[1..len];	
  S:=[1..len]*0+1;
  data:=SemigroupData(s);
  nr:=NrIdempotents(s);
  e:=Idempotents(s);
  for i in [1..nr] do 
    r:=Find@(B, Position(data, e[i])-1);
    l:=Find@(B, NrRClasses(s)+Position(LClasses(s), LClass(s, e[i])));
    Union@(B, S, r, l);
  od;
  return B;
end;

#

InstallMethod(DotDClasses, "for an acting semigroup",
[IsActingSemigroup], 
function(s)
  return DotDClasses(s, rec(maximal:=false, number:=true));
end);

InstallMethod(DotDClasses, "for an acting semigroup and record",
[IsActingSemigroup, IsRecord],
function(s, opts)
  local str, i, gp, h, rel, j, k, d, l, x;

  # process the options
  if not IsBound(opts.maximal) then 
    opts.maximal:=false;
  fi;
  if not IsBound(opts.number) then 
    opts.number:=true;
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
              Append(str, "<TD BGCOLOR=\"grey\">*</TD>");
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

  Append(str, " }");

  return str;
end);

#InstallGlobalFunction(DotDClass, 
#  function(d)
#    local str, h, l, j, x;
#
#    if not IsGreensClassOfTransSemigp(d) or not IsGreensDClass(d) then
#      Error("the argument should be a D-class of a trans. semigroup.");
#    fi;
#
#    str:="";
#    Append(str, "digraph  DClasses {\n");
#    Append(str, "node [shape=plaintext]\n");
#
#    Append(str, "1 [label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\"");
#    Append(str, " CELLPADDING=\"10\" CELLSPACING=\"0\">\n");
#
#    for l in LClasses(d) do
#      Append(str, "<TR>");
#      if not IsRegularClass(l) then
#        for j in [1..NrRClasses(d)] do
#          Append(str, "<TD></TD>");
#        od;
#      else
#        h:=HClasses(l);
#        for x in h do
#          if IsGroupHClass(x) then
#            Append(str, "<TD>*</TD>");
#          else
#            Append(str, "<TD></TD>");
#          fi;
#        od;
#      fi;
#      Append(str, "</TR>\n");
#    od;
#    Append(str, "</TABLE>>];\n}");
#
#    return str;
#  end);
