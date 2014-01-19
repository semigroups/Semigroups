#############################################################################
##
#W  pictures.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#

BindGlobal("TikzInit",
  Concatenation("%tikz\n", 
   "\\documentclass{minimal}\n",
   "\\usepackage{tikz}\n",
   "\\begin{document}\n")
);

#

InstallGlobalFunction(TikzEnd, function(str)
  Append(str, "\\end{document}");
  return str;
end);

#

InstallGlobalFunction(TikzBipartition, 
function(f)
  return Concatenation(TikzInit, TikzEnd(TikzStringForBipartition(f)));
end);

# for bipartition

InstallGlobalFunction(TikzRightBlocks,
function(f)
  return Concatenation(TikzInit,
   TikzEnd(TikzStringForBlocks(RightBlocks(f), "right", "right")));
end);

# for bipartition

InstallGlobalFunction(TikzLeftBlocks,
function(f)
  return Concatenation(TikzInit,
   TikzEnd(TikzStringForBlocks(LeftBlocks(f), "left", "left")));
end);

# for blocks, JDM have a right/left version of this

InstallGlobalFunction(TikzBlocks, 
function(blocks)
  return Concatenation(TikzInit, 
   TikzEnd(TikzStringForBlocks(blocks, "left", "left")));
end);

#

InstallGlobalFunction(TikzBipartitionRight, 
function(f)
  return Concatenation(TikzInit, TikzStringForBipartition(f),
    TikzEnd(TikzStringForBlocks(RightBlocks(f), "none", "right")));
end);

#

InstallGlobalFunction(TikzBipartitionLeft, function(f)
  return Concatenation(TikzInit, 
  TikzStringForBlocks(LeftBlocks(f), "none", "left"), 
  TikzEnd(TikzStringForBipartition(f)));
end);

#

InstallGlobalFunction(TikzBipartitionLeftRight, 
function(f)
  return Concatenation(TikzInit, 
  TikzStringForBlocks(LeftBlocks(f), "none", "left"), 
  TikzStringForBipartition(f), 
  TikzEnd(TikzStringForBlocks(RightBlocks(f), "none", "right")));
end);

#


InstallGlobalFunction(TikzStringForBlocks,
function(blocks, labels, edges)
  local str, n, x, ext, block, y, i;
  
  str:="\\begin{tikzpicture}\n"; 
  n:=DegreeOfBlocks(blocks);

  # draw the nodes
  for i in [1..n] do
    if blocks[n+blocks[i]]=0 then #non-transverse block
      #node
      Append(str, "\\draw[ultra thick](2,");
      Append(str, ViewString(n-i+1));
      Append(str, ")circle(.115);\n");
    else #transverse block
      #node
      Append(str, "\\fill(2,");
      Append(str, ViewString(n-i+1));
      Append(str, ")circle(.125);\n");
    fi;  
    
    if labels<>"none" then  
      if labels="left" then 
        #node label
        x:="1.8";
      elif labels="right" then 
        x:="2.2";
      else
        Error("usage: <labels> should be \"right\", \"left\", or \"none\",");
        return;
      fi;
      Append(str, "\\draw(");
      Append(str, x);
      Append(str, ","); 
      Append(str, ViewString(n-i+1));
      Append(str, ") node [");
      Append(str, labels);
      Append(str, "] {{$"); 
      Append(str, ViewString(i));
      Append(str, "$}};"); 
      Append(str, "\n");
    fi;
  od;
    
  # draw the edges
  ext:=ExtRepOfBlocks(blocks);
  for block in ext do
    block:=ShallowCopy(block);
    Apply(block, AbsInt);
    
    if edges="left" then 
      x:="2.125";
      y:=i-> ViewString(Float(2.5+(1/(2*n))*(block[i]-block[i-1])));
    elif edges="right" then  
      x:="1.875";
      y:=i-> ViewString(Float(1.5-(1/(2*n))*(block[i]-block[i-1])));
    else 
      Error("usage: <edges> should be \"left\" or \"right\",");
      return;
    fi;
    
    for i in [2..Length(block)] do
      Append(str, "\\draw (");
      Append(str, x);
      Append(str, ",");
      Append(str, ViewString(n-block[i-1]+1));
      Append(str, ") .. controls (");
      Append(str, y(i));
      Append(str, ",");
      Append(str, ViewString(n-block[i-1]+1));
      Append(str, ") and (");
      Append(str, y(i));
      Append(str, ",");
      Append(str, ViewString(n-block[i]+1));
      Append(str, ") .. (");
      Append(str, x);
      Append(str, ",");
      Append(str, ViewString(n-block[i]+1));
      Append(str, ");\n");
    od;
  od;

  Append(str, "\\end{tikzpicture}\n\n");
  return str;
end);

# JDM - requires reworking.

InstallGlobalFunction(TikzStringForBipartition,
function(f)
  local str, ext, n, up, down, min, i, block, j;
  
  str:="\\begin{tikzpicture}\n"; 
  ext:=ExtRepOfBipartition(f);
  n:=DegreeOfBipartition(f);
 
  # vertices and their labels
  for i in [1..n] do 
    Append(str, "\\fill (");
    Append(str, ViewString(i-1)); Append(str, ",2)circle(.125);\n");
    
    Append(str, "\\draw("); Append(str, ViewString(i-1.05));
    Append(str, ", 2.2) node [above] {{ $"); Append(str, ViewString(i));
    Append(str, "$}};"); Append(str, "\n");
    
    Append(str, "\\fill (" ); 
    Append(str, ViewString(i-1)); Append(str, ",0)circle(.125);\n");
  
    Append(str, "\\draw("); Append(str, ViewString(i-1));
    Append(str, ", -0.2) node [below] {{ $"); Append(str, ViewString(i));
    Append(str, "'$}};"); Append(str, "\n");
  od;

  # draw the lines
  for block in ext do
    up:=[]; down:=[];
    for i in [2..Length(block)] do
      if block[i-1]<=n and block[i]<=n then
        AddSet(up, block[i-1]);
        AddSet(up, block[i]);
        Append(str, "\\draw (");
        Append(str, ViewString(block[i-1]-1));
        Append(str, ",1.875) .. controls (");
        Append(str, ViewString(block[i-1]-1));
        Append(str, ",");
        Append(str, ViewString(Float(1.5-(1/(2*n))*(block[i]-block[i-1]))));
        Append(str, ") and (");
        Append(str, ViewString(block[i]-1));
        Append(str, ",");
        Append(str, ViewString(Float(1.5-(1/(2*n))*(block[i]-block[i-1]))));
        Append(str, ") .. (");
        Append(str, ViewString(block[i]-1));
        Append(str, ",1.875);\n");
      elif block[i-1]>n and block[i]>n then 
        AddSet(down, block[i-1]-n);
        AddSet(down, block[i]-n);
        Append(str, "\\draw (");
        Append(str, ViewString(block[i-1]-1-n));
        Append(str, ",0.125) .. controls (");
        Append(str, ViewString(block[i-1]-1-n));
        Append(str, ",");
        Append(str, ViewString(Float(0.5+(1/(2*n))*(block[i]-block[i-1]))));
        Append(str, ") and (");
        Append(str, ViewString(block[i]-1-n));
        Append(str, ",");
        Append(str, ViewString(Float(0.5+(1/(2*n))*(block[i]-block[i-1]))));
        Append(str, ") .. (");
        Append(str, ViewString(block[i]-1-n));
        Append(str, ",0.125);\n");
      elif block[i-1]<=n and block[i]>n then
        AddSet(down, block[i]-n); AddSet(up, block[i-1]);
      elif block[i-1]>n and block[i]<=n then
        AddSet(down, block[i-1]-n); AddSet(up, block[i]);
      fi;
    od;
    if Length(up)<>0 and Length(down)<>0 then 
      min:=[n];
      for i in up do 
        for j in down do 
          if AbsInt(i-j)<min[1] then 
            min[1]:=AbsInt(i-j); min[2]:=i; min[3]:=j;
          fi;
        od;
      od;
      Append(str, "\\draw ("); Append(str, ViewString(min[2]-1));
      Append(str, ",2)--(");
      Append(str, ViewString(min[3]-1)); Append(str, ",0);\n");
    fi;
  od;
  Append(str, "\\end{tikzpicture}\n\n");
  return str;
end);


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
