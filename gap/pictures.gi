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
    Append(str, ", -0.2) node [below] {{ $-"); Append(str, ViewString(i));
    Append(str, "$}};"); Append(str, "\n");
  od;

  # draw the lines
  for block in ext do
    up:=[]; down:=[];
    for i in [2..Length(block)] do
      if block[i-1]>0 and block[i]>0 then
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
      elif block[i-1]<0 and block[i]<0 then 
        AddSet(down, block[i-1]);
        AddSet(down, block[i]);
        Append(str, "\\draw (");
        Append(str, ViewString(-1*block[i-1]-1));
        Append(str, ",0.125) .. controls (");
        Append(str, ViewString(-1*block[i-1]-1));
        Append(str, ",");
        Append(str, ViewString(Float(0.5+(-1/(2*n))*(block[i]-block[i-1]))));
        Append(str, ") and (");
        Append(str, ViewString(-1*block[i]-1));
        Append(str, ",");
        Append(str, ViewString(Float(0.5+(-1/(2*n))*(block[i]-block[i-1]))));
        Append(str, ") .. (");
        Append(str, ViewString(-1*block[i]-1));
        Append(str, ",0.125);\n");
      elif block[i-1]>0 and block[i]<0 then
        AddSet(down, block[i]); AddSet(up, block[i-1]);
      elif block[i-1]<0 and block[i]>0 then
        AddSet(down, block[i-1]); AddSet(up, block[i]);
      fi;
    od;
    #Error();
    if Length(up)<>0 and Length(down)<>0 then 
      min:=[n+1]; down:=down*-1;
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

