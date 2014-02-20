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

