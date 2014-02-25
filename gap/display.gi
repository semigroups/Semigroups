#############################################################################
##
#W  display.gi
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

BindGlobal("TikzEnd", "\\end{document}");

#

InstallGlobalFunction(TikzBipartition, 
function(arg)
  return Concatenation(TikzInit, CallFuncList(TikzStringForBipartition, arg),
  TikzEnd);
end);

# for bipartition

InstallGlobalFunction(TikzRightBlocks,
function(f)
  return Concatenation(TikzInit, 
    TikzStringForBlocks(RightBlocks(f), "bottom", "bottom"), TikzEnd);
end);

# for bipartition

InstallGlobalFunction(TikzLeftBlocks,
function(f)
  return Concatenation(TikzInit, TikzStringForBlocks(LeftBlocks(f), "top", "top"), 
    TikzEnd);
end);

# for blocks, JDM have a right/left version of this

InstallGlobalFunction(TikzBlocks, 
function(blocks)
  return Concatenation(TikzInit, TikzStringForBlocks(blocks, "top", "top"), TikzEnd);
end);

#

InstallGlobalFunction(TikzBipartitionRight, 
function(f)
  return Concatenation(TikzInit, "\\begin{center}\n",
   TikzStringForBipartition(f), "\\bigskip\n",
   TikzStringForBlocks(RightBlocks(f), "none", "bottom"),
    "\\end{center}\n", TikzEnd);
end);

#

InstallGlobalFunction(TikzBipartitionLeft, function(f)
  return Concatenation(TikzInit, "\\begin{center}\n",
   TikzStringForBipartition(f), "\\bigskip\n",
   TikzStringForBlocks(LeftBlocks(f), "none", "top"),
    "\\end{center}\n", TikzEnd);
end);

#

InstallGlobalFunction(TikzBipartitionLeftRight, 
function(f)
  return Concatenation(TikzInit, "\\begin{center}\n", 
  TikzStringForBlocks(LeftBlocks(f), "none", "top"), "\\bigskip\n",
  TikzStringForBipartition(f), "\\bigskip\n",
  TikzStringForBlocks(RightBlocks(f), "none", "bottom"),
  "\\end{center}\n", TikzEnd);
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
      Append(str, "  \\draw[ultra thick](");
      Append(str, ViewString(n-i+1));
      Append(str, ",2)circle(.115);\n");
    else #transverse block
      #node
      Append(str, "  \\fill(");
      Append(str, ViewString(n-i+1));
      Append(str, ",2)circle(.125);\n");
    fi;  
    
    if labels<>"none" then  
      if labels="top" then 
        #node label
        x:="1.8";
      elif labels="bottom" then 
        x:="2.2";
      else
        Error("usage: <labels> should be \"bottom\", \"top\", or \"none\",");
        return;
      fi;
      Append(str, "  \\draw(");
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
  Append(str, "\n");
  ext:=ExtRepOfBlocks(blocks);
  for block in ext do
    block:=ShallowCopy(block);
    Apply(block, AbsInt);
    
    if edges="top" then 
      x:="2.125";
      y:=i-> ViewString(Float(2.5+(1/(2*n))*(block[i]-block[i-1])));
    elif edges="bottom" then  
      x:="1.875";
      y:=i-> ViewString(Float(1.5-(1/(2*n))*(block[i]-block[i-1])));
    else 
      Error("usage: <edges> should be \"top\" or \"bottom\",");
      return;
    fi;
    
    for i in [2..Length(block)] do
      Append(str, "  \\draw (");
      Append(str, ViewString(n-block[i-1]+1));
      Append(str, ",");
      Append(str, x);
      Append(str, ") .. controls (");
      Append(str, ViewString(n-block[i-1]+1));
      Append(str, ",");
      Append(str, y(i));
      Append(str, ") and (");
      Append(str, ViewString(n-block[i]+1));
      Append(str, ",");
      Append(str, y(i));
      Append(str, ") .. (");
      Append(str, ViewString(n-block[i]+1));
      Append(str, ",");
      Append(str, x);
      Append(str, ");\n");
    od;
  od;

  Append(str, "\\end{tikzpicture}\n\n");
  return str;
end);

#

InstallGlobalFunction(TikzStringForBipartition,
function(arg)
  local fill, draw, f, opts, colors, str, ext, n, block, up, down, min, j, i, k;
  
  fill:=i-> "  \\fill(";
  draw:=i-> "  \\draw(";
  
  f:=arg[1]; 
  if IsBound(arg[2]) then 
    opts:=arg[2]; 
    if IsBound(opts.colors) and opts.colors=true and NrBlocks(f)<20 then 
      colors:=["red", "green", "blue", "cyan", "magenta", "yellow", "black",
      "gray", "darkgray", "lightgray", "brown", "lime", "olive", "orange", "pink",
      "purple", "teal", "violet", "white"];
      fill:=i-> Concatenation("  \\fill[", colors[i], "](");
      draw:=i-> Concatenation("  \\draw[", colors[i], "](");
    fi; 
  fi;

  str:="\\begin{tikzpicture}\n"; 
  ext:=ExtRepOfBipartition(f);
  n:=DegreeOfBipartition(f);
 
  # draw the lines
  for j in [1..Length(ext)] do
    block:=ext[j];
    Append(str, "\n");
    Append(str, "  %block #");
    Append(str, String(j));
    Append(str, "\n");
    Append(str, "  %vertices and labels\n"); 
    # vertices and their labels
    for i in block do 
      if i>0 then 
        Append(str, fill(j));
        Append(str, ViewString(i)); Append(str, ",2)circle(.125);\n");
        
        Append(str, draw(j)); Append(str, ViewString(i-0.05));
        Append(str, ", 2.2) node [above] {{ $"); Append(str, ViewString(i));
        Append(str, "$}};"); Append(str, "\n");
      else
        Append(str, fill(j)); 
        Append(str, ViewString(-i)); Append(str, ",0)circle(.125);\n");
      
        Append(str, draw(j)); Append(str, ViewString(-i));
        Append(str, ", -0.2) node [below] {{ $-"); Append(str, ViewString(-i));
        Append(str, "$}};"); Append(str, "\n");
      fi;
    od;

    Append(str, "\n  %lines\n"); 
    # lines
    up:=[]; down:=[];
    for i in [2..Length(block)] do
      if block[i-1]>0 and block[i]>0 then
        AddSet(up, block[i-1]);
        AddSet(up, block[i]);
        Append(str, draw(j));
        Append(str, ViewString(block[i-1]));
        Append(str, ",1.875) .. controls (");
        Append(str, ViewString(block[i-1]));
        Append(str, ",");
        Append(str, ViewString(Float(1.5-(1/(2*n))*(block[i]-block[i-1]))));
        Append(str, ") and (");
        Append(str, ViewString(block[i]));
        Append(str, ",");
        Append(str, ViewString(Float(1.5-(1/(2*n))*(block[i]-block[i-1]))));
        Append(str, ") .. (");
        Append(str, ViewString(block[i]));
        Append(str, ",1.875);\n");
      elif block[i-1]<0 and block[i]<0 then 
        AddSet(down, block[i-1]);
        AddSet(down, block[i]);
        Append(str,  draw(j));
        Append(str, ViewString(-block[i-1]));
        Append(str, ",0.125) .. controls (");
        Append(str, ViewString(-block[i-1]));
        Append(str, ",");
        Append(str, ViewString(Float(0.5+(-1/(2*n))*(block[i]-block[i-1]))));
        Append(str, ") and (");
        Append(str, ViewString(-block[i]));
        Append(str, ",");
        Append(str, ViewString(Float(0.5+(-1/(2*n))*(block[i]-block[i-1]))));
        Append(str, ") .. (");
        Append(str, ViewString(-block[i]));
        Append(str, ",0.125);\n");
      elif block[i-1]>0 and block[i]<0 then
        AddSet(down, block[i]); AddSet(up, block[i-1]);
      elif block[i-1]<0 and block[i]>0 then
        AddSet(down, block[i-1]); AddSet(up, block[i]);
      fi;
    od;
    if Length(up)<>0 and Length(down)<>0 then 
      min:=[n+1]; down:=down*-1;
      for i in up do 
        for k in down do 
          if AbsInt(i-k)<min[1] then 
            min[1]:=AbsInt(i-k); min[2]:=i; min[3]:=k;
          fi;
        od;
      od;
      Append(str, draw(j)); Append(str, ViewString(min[2]));
      Append(str, ",2)--(");
      Append(str, ViewString(min[3])); Append(str, ",0);\n");
    fi;
  od;
  Append(str, "\\end{tikzpicture}\n\n");
  return str;
end);
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
