#############################################################################
###
##W  partition.gi
##Y  Copyright (C) 2011-13                                James D. Mitchell
###
###  Licensing information can be found in the README file of this package.
###
##############################################################################
###

BindGlobal("BipartitionFamily", NewFamily("BipartitionFamily",
 IsBipartition, CanEasilySortElements, CanEasilySortElements));

BindGlobal("BipartitionType", NewType(BipartitionFamily,
 IsBipartition and IsDataObjectRep and IsAssociativeElementWithAction));

#

InternalRepOfBipartition:=f-> List([1..f[1]+2], i-> f[i]);

NrClassesSignedPartition:=x-> x[1];
NrClassesBipartition:=x-> x[2];

DegreeOfSignedPartition:=x-> Length(x)-NrClassesSignedPartition(x)-1;

#

TikzInit:=function()
  local str;

  str:="\\documentclass{minimal}\n";
  Append(str, "\\usepackage{tikz}\n");
  Append(str, "\\begin{document}\n");
  return str;
end;

#


#

TikzPicLeftSignedPartition:=function(f)
  local str, ext, n, up, down, min, i, block, j;
  
  str:="\\begin{tikzpicture}\n"; 
  ext:=ExtRepBipartition(f);
  n:=DegreeOfBipartition(f)/2;
 
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
  ext:=ExtRepBipartition(f);
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
  ext:=ExtRepBipartition(f);
  n:=DegreeOfBipartition(f)/2;
 
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

InstallGlobalFunction(INV_SIGNED_PART_BIPART, 
function(signed, f)
  local n, p1, p2, fuse, fuseit, x, y, x1, lookup, j, out, next, seen, i;

  n := DegreeOfBipartition(f)/2;
  Assert(1,n = DegreeOfSignedPartition(signed));
  p1 := NrClassesSignedPartition(signed);
  p2 := NrClassesBipartition(f);
  fuse := [1..p1+p2]; 
  
  fuseit := function(i) 
    while fuse[i] < i do 
      i := fuse[i]; 
    od; 
    return i; 
  end;

  for i in [1..n] do
    x := fuseit(signed[i+1]);
    y := fuseit(f[i+2]+p1);
    if x <> y then
      if x < y then
        fuse[y] := x;
      else
        fuse[x] := y;
      fi;
    fi;
  od;

  x1:=OnRightSignedPartition(signed, f);
  lookup:=[1..p1+p2]*0;

  for i in [1..n] do 
    j:=fuseit(f[2+n+i]+p1);
    j:=First([1..p1], i-> signed[n+1+i]=1 and fuseit(i)=j);
    if x1[n+1+x1[i+1]]=1 and lookup[j]=0 then 
      lookup[j]:=x1[i+1];
    fi;
  od;
  out:=x1{[2..n+1]};
  next:=NrClassesSignedPartition(x1)+1;
  seen:=[1..p1]*0;
  for i in [1..n] do 
    if signed[n+1+signed[i+1]]=0 then 
      if seen[signed[i+1]]<>0 then 
        Add(out, seen[signed[i+1]]);
      else
        seen[signed[i+1]]:=next;
        next:=next+1;
        Add(out, seen[signed[i+1]]);
      fi;
    else 
      Add(out, lookup[signed[i+1]]); 
    fi;
    if 0 in out then Error(); fi;
  od;  

  return BipartitionByIntRepNC(out);
end);

#

InstallGlobalFunction(BipartitionNC, 
function(arg)
  if IsList(arg[1][1]) then 
    return BipartitionByPartitionNC(arg[1]);
  elif ForAll(arg[1], IsPosInt) then 
    return BipartitionByIntRepNC(arg[1]);
  fi;
  Error("usage: the argument should be a partition or the ",
  "internal rep of a partition");
  return;
end);
    
#

InstallMethod(\^, "for a pos int and bipartition",
[IsPosInt, IsBipartition], OnPointsBP);

#

InstallOtherMethod(POW, "for a set of pos ints and bipartition",
[IsListOrCollection, IsBipartition], 
function(set, f) 
  return Set(Concatenation(List(set, x-> OnPointsBP(x,f))));
end);

#

InstallMethod(\^, "for a bipartition and neg int",
[IsBipartition, IsNegInt],
function(f, r)
  local foo;

  foo:=function(i) 
    if i=f[1] then 
      return i;
    fi;

    return i mod f[1];
  end;

  if r=-1 then
    return BipartitionNC(Set(List(ExtRepBipartition(f), x-> Set(List(x, y->
    foo(y+f[1]/2))))));
  fi;
  return (f^-1)^-r;
end);

#

InstallMethod(\<, "for a bipartition and bipartition", 
[IsBipartition, IsBipartition],
function(f,g)
  return ExtRepBipartition(f)<ExtRepBipartition(g);
end);

#

InstallMethod(\=, "for a bipartition and bipartition", 
[IsBipartition, IsBipartition],
function(f,g)
  return ForAll([1..f[1]+2], x-> f[x]=g[x]);
end);

#

InstallMethod(AsBipartition, "for a permutation and pos int",
[IsPerm, IsPosInt],
function(f, n)
  return BipartitionByIntRepNC(Concatenation([1..n], OnTuples([1..n], f^-1)));
end);

#

InstallOtherMethod(AsBipartition, "for a partial perm and pos int",
[IsPartialPerm, IsPosInt],
function(f, n)
  local out, g, r, i;

  g:=f^-1;
  r:=n;
  out:=EmptyPlist(2*n);

  for i in [1..n] do 
    out[i]:=i;
    if i^g<>0 then 
      out[n+i]:=i^g;
    else 
      r:=r+1;
      out[n+i]:=r;
    fi;
  od;
  return BipartitionByIntRepNC(out); 
end);

#

InstallOtherMethod(AsBipartition, "for a transformation",
[IsTransformation],
function(f)
  local n, r, ker, out, g, i;

  n:=DegreeOfTransformation(f);
  r:=RankOfTransformation(f);;
  ker:=FlatKernelOfTransformation(f); 
  out:=ShallowCopy(ker);
  g:=List([1..n], x-> 0);

  #inverse of f
  for i in [1..n] do 
    g[i^f]:=i;
  od;

  for i in [1..n] do 
    if g[i]<>0 then 
      out[n+i]:=ker[g[i]];
    else 
      r:=r+1;
      out[n+i]:=r;
    fi;
  od;
  return BipartitionByIntRepNC(out);
end);

#

InstallMethod(DegreeOfBipartition, "for a bipartition",
[IsBipartition], x-> x[1]);

#

InstallMethod(DegreeOfBipartitionCollection, "for a bipartition collection",
[IsBipartitionCollection], 
function(coll)

  if IsBipartitionSemigroup(coll) then 
    return DegreeOfBipartitionSemigroup(coll);
  elif not ForAll(coll, x-> x[1]=coll[1][1]) then 
    Error("usage: collection of bipartitions of equal degree,");
    return;
  fi;
  return coll[1][1];
end);

#

InstallMethod(DegreeOfBipartitionSemigroup, "for a bipartition semigroup",
[IsBipartitionSemigroup], 
 s-> Representative(s)[1]);

InstallMethod(Display, "for a bipartition",
[IsBipartition], function(f)
  Print("BipartitionNC( ", ExtRepBipartition(f), " )");
  return;
end);

InstallMethod(Display, "for a bipartition collection",
[IsBipartitionCollection],
function(coll) 
  local i;

  Print("[ ");
  for i in [1..Length(coll)] do 
    if not i=1 then Print(" "); fi;
    Display(coll[i]);
    if not i=Length(coll) then 
      Print(",\n");
    else
      Print(" ]\n");
    fi;
  od;
  return;
end);

#

InstallMethod(RankOfBipartition, "for a bipartition",
[IsBipartition], 
function(x)
  local n, m, seen, rank, i;

  n:=x[1]/2;

  m:=MaximumList(x{[3..n+2]}); # max on the left
  seen:=BlistList([1..m], []); 
  rank:=0;

  for i in [1..n] do 
    if x[n+2+i]<=m and not seen[x[n+2+i]] then 
      seen[x[n+2+i]]:=true;
      rank:=rank+1;
    fi;
  od;
  return rank;
end);

#

InstallOtherMethod(ELM_LIST, "for a bipartition and a pos int",
[IsBipartition, IsPosInt], ELM_LIST_BP);

#

InstallOtherMethod(ELMS_LIST, "for a bipartition and a pos int",
[IsBipartition, IsDenseList and IsSmallList], ELMS_LIST_BP);


#

#JDM this does not currently work!

# InstallOtherMethod(IsomorphismTransformationSemigroup,
# "for a bipartiton semigroup",
# [IsBipartitionSemigroup],
# function(s)
#   local n, pts, o, t, pos, i;
# 
#   n:=DegreeOfBipartition(Generators(s)[1]);
#   pts:=EmptyPlist(2^(n/2));
# 
#   for i in [1..n/2] do
#     o:=Orb(s, [i], OnPoints); #JDM multiseed orb
#     Enumerate(o);
#     pts:=Union(pts, AsList(o));
#   od;
#   ShrinkAllocationPlist(pts);
#   t:=Semigroup(List(GeneratorsOfSemigroup(s), 
#    x-> TransformationOp(x, pts, OnPoints)));
#   pos:=List([1..n], x-> Position(pts, [x]));
# 
#   return MappingByFunction(s, t, x-> TransformationOp(x, pts, OnPoints));
# end);

#

InstallMethod(PrintObj, "for a bipartition",
[IsBipartition],
function(f)
  local ext, n, i, j;
  
  Print("<bipartition: ");
  ext:=ExtRepBipartition(f);
  n:=DegreeOfBipartition(f)/2;
  for i in [1..f[2]] do 

    Print("[ ");
    for j in [1..Length(ext[i])-1] do 
      if ext[i][j]>n then 
        Print(ext[i][j]-n, "', ");
      else
        Print(ext[i][j], ", ");
      fi;
    od;
    if ext[i][Length(ext[i])]>n then 
      Print(ext[i][Length(ext[i])]-n, "' ");
    else
      Print(ext[i][Length(ext[i])], " ");
    fi;
    if i=f[2] then 
      Print("] ");
    else
      Print("], ");
    fi;
  od;
  
  Print(">");
  return;
end);

#

InstallGlobalFunction(ExtRepBipartition,
function(q)
  local i,n2,p;
  n2 := q[1];
  p := List([1..q[2]],i->[]);
  for i in [1..n2] do
    Add(p[q[i+2]],i);
  od;
  return p;
end);

#

InstallMethod(\*, "for a bipartition and a perm",
[IsBipartition, IsPerm],
function(f,g)
  return f*AsBipartition(g, DegreeOfBipartition(f)/2);
end);

#

InstallMethod(\*, "for a perm and a bipartition",
[IsPerm, IsBipartition],
function(f,g)
  return AsBipartition(f, DegreeOfBipartition(g)/2)*g;
end);

#

InstallMethod(\*, "for a bipartition and bipartition",
[IsBipartition, IsBipartition], 
function(a,b)
  # This composes two partitions of [1..n] in internal rep
  local c,fuse,fuseit,i,n,next,p1,p2,tab3,x,y;
  n := a[1]/2;
  Assert(1,n = b[1]/2);
  p1 := a[2];
  p2 := b[2];
  fuse := [1..p1+p2]; 
  # From now on i in partition a is in part number a[i]
  #         and j in partition b is in part number b[j]+p2
  # The fusion tab always maintains fuse[i] <= i and the fuse function
  # is defined to be 
  #     # We can now easily fuse parts by changing one number in a table.
  fuseit := function(i) 
              while fuse[i] < i do i := fuse[i]; od; 
              return i; 
            end;
  for i in [1..n] do
      # we want to fuse the parts of i+n in a and i in b:
      x := fuseit(a[i+n+2]);
      y := fuseit(b[i+2]+p1);
      if x <> y then
          #Print("Fusing parts ",x," and ",y,"\n");
          if x < y then
              fuse[y] := x;
          else
              fuse[x] := y;
          fi;
      fi;
  od;
  # We can now put together the resulting partition, we take 1..n from a
  # and n+1..2*n and look at the fusion, in which part they are.
  tab3 := 0*[1..p1+p2];    # A table for the old part numbers
  c := EmptyPlist(2*n);
  next := 1;
  for i in [1..n] do
      x := fuseit(a[i+2]);
      if tab3[x] = 0 then
          tab3[x] := next;
          next := next + 1;
      fi;
      Add(c,tab3[x]);
  od;
  for i in [n+1..2*n] do
      x := fuseit(b[i+2]+p1);
      if tab3[x] = 0 then
          tab3[x] := next;
          next := next + 1;
      fi;
      Add(c,tab3[x]);
  od;
  #Add(c,next-1);
  return BipartitionByIntRepNC(c);
# return Concatenation([2*n, next-1], c);
end);

# part should be of the form [4,1,2,3,3,4,4,1,0,1,0]
# [# classes,partition in internal rep, signing]
# Length of partition must =f[1]!!!

InstallMethod(RightSignedPartition, 
[IsBipartition],
function(a)
  local n, mark, next, tab, out, j, x, i;
 
  n:=a[1]/2;
  mark:=[1..a[2]]*0;

  for i in [3..n+2] do 
    mark[a[i]]:=1;
  od;

  next:=1;
  tab:=[1..a[2]]*0;
  out:=[];
  j:=1;
  for i in [n+3..2*n+2] do 
    x:=a[i];
    if tab[x]=0 then 
      tab[x]:=next;
      next:=next+1;
    fi;
    j:=j+1;
    out[j]:=tab[x];
    out[1+n+tab[x]]:=mark[a[i]];
  od;
  out[1]:=next-1;
  return out;
end);

InstallMethod(LeftSignedPartition, "for a bipartition",
[IsBipartition],
function(a)
  local n, out, max, i;
 
  n:=a[1]/2;
  out:=[];
  max:=1;
  for i in [3..n+2] do 
    out[i-1]:=a[i];
    if a[i]>max then 
      max:=a[i];
    fi;
  od;
  for i in [1..max] do 
    out[n+1+i]:=0;
  od;
  for i in [n+3..2*n+2] do 
    if a[i]<=max then 
      out[1+n+a[i]]:=1;
    fi;
  od;
  out[1]:=max;
  return out;
end);

InstallMethod(OnRightSignedPartition, 
[IsList, IsBipartition],
function(a, b)
  local n, p1, p2, fuse, mark, fuseit, x, y, tab3, c, j, next, i;

  n:=b[1]/2; # length of partition!!
  p1:=a[1]; #number of classes in a
  if p1>n then 
    return RightSignedPartition(b);
  fi;
  p2:=b[2]; #number of classes in b

  fuse:=[1..p1+p2];
  mark:=[1..p1+p2]*0;

  for i in [1..p1] do 
    mark[i]:=a[i+n+1];
  od;

  fuseit := function(i) 
              while fuse[i] < i do i := fuse[i]; od; 
              return i; 
            end;
  for i in [1..n] do
    x := fuseit(a[i+1]);
    y := fuseit(b[i+2]+p1);
    if x <> y then
      if x < y then
        fuse[y] := x;
        if mark[y]=1 then mark[x]:=1; fi;
      else
        fuse[x] := y;
        if mark[x]=1 then mark[y]:=1; fi;
      fi;
    fi;
  od;
  tab3:=0*[1..p1+p2];
  c:=[];
  j:=1;
  next:=1;
  for i in [n+1..2*n] do
    x := fuseit(b[i+2]+p1);
    if tab3[x] = 0 then
      tab3[x] := next;
      next := next + 1;
    fi;
    j:=j+1;
    c[j]:=tab3[x];
    c[n+1+tab3[x]]:=mark[x];
  od;
  c[1]:=next-1;
  return c;
end);

InstallMethod(OnLeftSignedPartition, 
[IsList, IsBipartition],
function(a, b)
  local n, p1, p2, fuse, mark, fuseit, x, y, tab3, c, j, next, i;

  n:=b[1]/2; # length of partition!!
  p1:=a[1]; #number of classes in a
  if p1>n then 
    return LeftSignedPartition(b);
  fi;
  p2:=b[2]; #number of classes in b

  fuse:=[1..p1+p2];
  mark:=[1..p1+p2]*0;
  fuseit := function(i) 
              while fuse[i] < i do i := fuse[i]; od; 
              return i; 
            end;
  for i in [1..n] do
    x := fuseit(b[n+i+2]);
    y := fuseit(a[i+1]+p2);
    if x <> y then
      if x < y then
        fuse[y] := x;
        if a[n+a[i+1]+1]=1 then mark[x]:=1; fi;
      else
        fuse[x] := y;
        if a[n+a[i+1]+1]=1 then mark[y]:=1; fi;
      fi;
    fi;
  od;
  tab3:=0*[1..p1+p2];
  c:=[];
  j:=1;
  next:=1;
  for i in [1..n] do
    x := fuseit(b[i+2]);
    if tab3[x] = 0 then
      tab3[x] := next;
      next := next + 1;
    fi;
    j:=j+1;
    c[j]:=tab3[x];
    c[n+1+tab3[x]]:=mark[x];
  od;
  c[1]:=next-1;
  return c;
end);

InstallMethod(OneMutable, "for a bipartition",
[IsBipartition],
function(a)
  local n;
  n:=DegreeOfBipartition(a)/2;
  return BipartitionNC(List([1..n], x-> [x,x+n]));
end);

InstallOtherMethod(OneMutable, "for a bipartition coll",
[IsBipartitionCollection],
function(coll)
  local n;
  n:=DegreeOfBipartitionCollection(coll)/2;
  return BipartitionNC(List([1..n], x-> [x, x+ n]));
end);

InstallMethod(RandomBipartition, "for a pos int",
[IsPosInt],
function(n)
  local out, m, vals, j, i;

  out:=EmptyPlist(2*n);
  m:=1;
  vals:=[1];

  for i in [1..2*n] do 
    j:=Random(vals);
    if j=m then 
      m:=m+1;
      Add(vals, m);
    fi;
    out[i]:=j;
  od;
  return BipartitionByIntRepNC(out);
end);


# Results:

# n=1 partitions=2 idempots=2
# n=2 partitions=15 idempots=12
# n=3 partitions=203 idempots=114
# n=4 partitions=4140 idempots=1512
# n=5 partitions=115975 idempots=25826
# n=6 partitions=4213597 idempots=541254, 33 seconds
# n=7 partitions=190899322 idempots=13479500, 1591 seconds

