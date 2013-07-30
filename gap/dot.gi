
#

TikzInit:=function()
  local str;

  str:="\\documentclass{minimal}\n";
  Append(str, "\\usepackage{tikz}\n");
  Append(str, "\\begin{document}\n");
  return str;
end;

#

TikzPicLeftSignedPartition:=function(f)
  local str, ext, n, up, down, min, i, block, j;
  
  str:="\\begin{tikzpicture}\n"; 
  ext:=ExtRepOfBipartition(f);
  n:=DegreeOfBipartition(f);
 
  # draw the lines
  for block in ext do
    
    if ForAny(block, i-> i<=n) then 
      if not ForAny(block, i-> i>n) then #non-transverse block
        for i in block do
          if i<=n then 
            #node
            Append(str, "\\draw[ultra thick](");
            Append(str, ViewString(i-1));
            Append(str, ",2)circle(.115);\n");

            #node label
            Append(str, "\\draw("); Append(str, ViewString(i-1.05));
            Append(str, ", 2.2) node [above] {{ $"); 
            Append(str, ViewString(i));
            Append(str, "$}};"); Append(str, "\n");
          fi;
        od;  
      else #transverse block
        for i in block do 
          if i<=n then 
            #node
            Append(str, "\\fill(");
            Append(str, ViewString(i-1));
            Append(str, ",2)circle(.125);\n");

            #node label
            Append(str, "\\draw("); Append(str, ViewString(i-1.05));
            Append(str, ", 2.2) node [above] {{ $"); 
            Append(str, ViewString(i));
            Append(str, "$}};"); Append(str, "\n");
          fi;  
        od;
      fi;
    fi;
    
    # edges
    for i in [2..Length(block)] do
      if block[i-1]<=n and block[i]<=n then
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
      fi;
    od;
  od;

  Append(str, "\\end{tikzpicture}\n\n");
  return str;
end;

#

TikzPicRightSignedPartition:=function(f)
  local str, ext, n, up, down, min, i, block, j;
  
  str:="\\begin{tikzpicture}\n"; 
  ext:=ExtRepOfBipartition(f);
  n:=DegreeOfBipartition(f)/2;
 
  # draw the lines
  for block in ext do
    
    if ForAny(block, i-> i>n) then 
      if not ForAny(block, i-> i<=n) then #non-transverse block
        for i in block do
          if i>n then 
            #node
            Append(str, "\\draw[ultra thick](");
            Append(str, ViewString(i-n-1));
            Append(str, ",2)circle(.115);\n");

            #node label
            Append(str, "\\draw("); Append(str, ViewString(i-n-1.05));
            Append(str, ", 1.8) node [below] {{ $"); 
            Append(str, ViewString(i-n));
            Append(str, "$}};"); Append(str, "\n");
          fi;
        od;  
      else #transverse block
        for i in block do 
          if i>n then 
            #node
            Append(str, "\\fill(");
            Append(str, ViewString(i-n-1));
            Append(str, ",2)circle(.125);\n");

            #node label
            Append(str, "\\draw("); Append(str, ViewString(i-n-1.05));
            Append(str, ", 1.8) node [below] {{ $"); 
            Append(str, ViewString(i-n));
            Append(str, "$}};"); Append(str, "\n");
          fi;  
        od;
      fi;
    fi;
    
    # edges
    for i in [2..Length(block)] do
      if block[i-1]>n and block[i]>n then 
        Append(str, "\\draw (");
        Append(str, ViewString(block[i-1]-1-n));
        Append(str, ",2.125) .. controls (");
        Append(str, ViewString(block[i-1]-1-n));
        Append(str, ",");
        Append(str, ViewString(Float(2.5+(1/(2*n))*(block[i]-block[i-1]))));
        Append(str, ") and (");
        Append(str, ViewString(block[i]-1-n));
        Append(str, ",");
        Append(str, ViewString(Float(2.5+(1/(2*n))*(block[i]-block[i-1]))));
        Append(str, ") .. (");
        Append(str, ViewString(block[i]-1-n));
        Append(str, ",2.125);\n");
      fi;
    od;
  od;

  Append(str, "\\end{tikzpicture}\n\n");
  return str;
end;

#

TikzPicBipartition:=function(f)
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
end;

TikzEnd:=function(str)

  Append(str, "\\end{document}");
  return str;
end;

TikzBipartition:=function(f)
  return Concatenation(TikzInit(), TikzEnd(TikzPicBipartition(f)));
end;

TikzRightSignedPartition:=function(f)
  return Concatenation(TikzInit(), TikzEnd(TikzPicRightSignedPartition(f)));
end;

TikzLeftSignedPartition:=function(f)
  return Concatenation(TikzInit(), TikzEnd(TikzPicLeftSignedPartition(f)));
end;

TikzBipartitionRight:=function(f)
  return Concatenation(TikzInit(), TikzPicBipartition(f),
    TikzEnd(TikzPicRightSignedPartition(f)));
end;

TikzBipartitionLeft:=function(f)
  return Concatenation(TikzInit(), TikzPicLeftSignedPartition(f), 
  TikzEnd(TikzPicBipartition(f)));
end;

TikzBipartitionLeftRight:=function(f)
  return Concatenation(TikzInit(), TikzPicLeftSignedPartition(f), 
  TikzPicBipartition(f), TikzEnd(TikzPicRightSignedPartition(f)));
end;

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
