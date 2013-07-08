############################################################################
##
#W  bipartition.gi
#Y  Copyright (C) 2011-13                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

BindGlobal("BipartitionFamily", NewFamily("BipartitionFamily",
 IsBipartition, CanEasilySortElements, CanEasilySortElements));

BindGlobal("BipartitionType", NewType(BipartitionFamily,
 IsBipartition and IsComponentObjectRep and IsAttributeStoringRep and
 IsAssociativeElementWithAction));

#

InverseRightBlocks:=function(blocks, f)
  local n, nrblocks, nrfblocks, fblocks, fuse, sign, fuseit, x, y, out, junk, next, tab1, nrleft, tab2, i;

  # the start of this is very similar to OnRightBlocks

  n:=DegreeOfBlocks(blocks); # length of partition!!
  nrblocks:=blocks[1];
  nrfblocks:=NrBlocks(f); 
  fblocks:=f!.blocks;
  
  fuse:=[1..nrblocks+nrfblocks];
  sign:=EmptyPlist(nrfblocks+nrblocks);

  for i in [1..nrblocks] do
    sign[i]:=blocks[n+1+i];
  od;
  for i in [nrblocks+1..nrfblocks+nrblocks] do
    sign[i]:=0;
  od;
  
  fuseit := function(i) 
    while fuse[i] < i do 
      i := fuse[i]; 
    od; 
    return i; 
  end;
  
  for i in [1..n] do
    x := fuseit(blocks[i+1]);
    y := fuseit(fblocks[i]+nrblocks);
    if x <> y then
      if x < y then
        fuse[y] := x;
        if sign[y]=1 then
          sign[x]:=1;
        fi;
      else
        fuse[x] := y;
        if sign[x]=1 then
          sign[y]:=1;
        fi;
      fi;
    fi;
  od;
  
  out:=[]; junk:=0; next:=0;
  
  tab1:=[];
  for i in [1..n] do 
    x:=fuseit(fblocks[i+n]+nrblocks);
    if x>NrLeftBlocks(f) or sign[x]=0 then 
      if junk=0 then 
        next:=next+1;
        junk:=next;
      fi;
      out[i]:=junk;
    else 
      if not IsBound(tab1[x]) then 
        next:=next+1;
        tab1[x]:=next;
      fi;
      out[i]:=tab1[x];
    fi;
  od;
  nrleft:=next;
  
  tab2:=[];
  for i in [n+1..2*n] do 
    x:=blocks[i-n+1];
    if blocks[n+1+x]=1 then 
      x:=fuseit(x);
      out[i]:=tab1[x];
    else
      if not IsBound(tab2[x]) then 
        next:=next+1;
        tab2[x]:=next;
      fi;
      out[i]:=tab2[x];
    fi;
  od;

  out:=Objectify(BipartitionType, rec(blocks:=out));

  SetDegreeOfBipartition(out, n);
  SetNrLeftBlocks(out, nrleft);
  SetNrBlocks(out, next);
  return out;
end;

#

InstallMethod(IdentityBipartition, "for a positive integer",
[IsPosInt],
function(n)
  local blocks, out, i;
  
  blocks:=EmptyPlist(2*n);
  for i in [1..n] do 
    blocks[i]:=i;
    blocks[i+n]:=i;
  od;
  
  out:=Objectify(BipartitionType, rec(blocks:=blocks));

  SetDegreeOfBipartition(out, n);
  SetNrLeftBlocks(out, n);
  SetNrBlocks(out, n);

  return out;
end);

#

InstallMethod(RankOfBipartition, "for a bipartition",
[IsBipartition],
function(f)
  return Number(TransverseBlocksLookup(f), x-> x=true);
end);

# c function

InstallGlobalFunction(BipartitionNC, 
function(classes)
  local list, n, nrker, out, i, j;

  list:=[];
  n:=Sum(List(classes, Length))/2;

  for i in [1..Length(classes)] do
    for j in classes[i] do 
      if j<0 then 
        list[AbsInt(j)+n]:=i;
      else 
        nrker:=i;
        list[j]:=i;
      fi;
    od;
  od;
  out:=Objectify(BipartitionType, rec(blocks:=list)); 
  
  SetDegreeOfBipartition(out, n);
  SetNrLeftBlocks(out, nrker);
  SetExtRepBipartition(out, classes);
  SetNrBlocks(out, Length(classes));

  return out;
end);

# linear - attribute

# returns a blist <out> for the Left blocks so that <out[i]> is <true> if
# and only the <i>th block of <f> is a transverse block.

InstallGlobalFunction(TransverseBlocksLookup,
function(f)
  local n, k, blocks, out, i;
  
  if IsBound(f!.lookup) then 
    return f!.lookup;
  fi;

  n:=DegreeOfBipartition(f);
  k:=NrLeftBlocks(f);
  blocks:=f!.blocks;
  out:=BlistList([1..k], []);

  for i in [1..n] do 
    if blocks[i+n]<=k then 
      out[blocks[i+n]]:=true;
    fi;
  od;

  f!.lookup:=out;
  return out;
end);

# return the classes of <f> as a list of lists

InstallMethod(ExtRepBipartition, "for a bipartition",
[IsBipartition],
function(f)
  local n, blocks, ext, i;

  n:=DegreeOfBipartition(f);
  blocks:=f!.blocks;
  ext:=[];
  
  for i in [1..2*n] do 
    if not IsBound(ext[blocks[i]]) then 
      ext[blocks[i]]:=[];
    fi;
    if i<=n then 
      Add(ext[blocks[i]], i);
    else
      Add(ext[blocks[i]], -(i-n));
    fi;
  od;

  return ext;
end);

#JDM proper string method!

InstallMethod(PrintObj, "for a bipartition",
[IsBipartition],
function(f)
  local ext, i;

  Print("<bipartition: ");
  ext:=ExtRepBipartition(f);
  Print(ext[1]);
  for i in [2..Length(ext)] do 
    Print(", ", ext[i]);
  od;
  Print(">");
  return;
end);

# xx^* - linear - 2*degree - attribute

InstallMethod(LeftProjection, "for a bipartition", 
[IsBipartition],
function(f)
  local n, k, blocks, lookup, table, out, i;

  n:=DegreeOfBipartition(f);
  k:=NrLeftBlocks(f);
  blocks:=f!.blocks;
  lookup:=TransverseBlocksLookup(f);
  table:=[];
  out:=[];

  for i in [1..n] do 
    out[i]:=blocks[i];
    if lookup[blocks[i]] then 
      out[i+n]:=blocks[i];
    elif IsBound(table[blocks[i]]) then 
      out[i+n]:=table[blocks[i]];
    else 
      k:=k+1;
      table[blocks[i]]:=k;
      out[i+n]:=k;
    fi;
  od;
  out:=Objectify(BipartitionType, rec(blocks:=out));
  
  SetDegreeOfBipartition(out, n);
  SetNrLeftBlocks(out, NrLeftBlocks(f));
  SetNrBlocks(out, k);
  #SetRankOfBipartition(out, RankOfBipartition(f));
  return out;
end);

# linear - 2*degree

InstallMethod(InverseOp, "for a bipartition", [IsBipartition],
function(f)
  local n, blocks, table, out, k, nrker, i;

  n:=DegreeOfBipartition(f);
  blocks:=f!.blocks;
  table:=[];
  out:=[];
  k:=0;

  for i in [1..n] do 
    if IsBound(table[blocks[i+n]]) then 
      out[i]:=table[blocks[i+n]];
    else
      k:=k+1;
      table[blocks[i+n]]:=k;
      out[i]:=k;
    fi;
  od;

  nrker:=k;

  for i in [1..n] do 
    if IsBound(table[blocks[i]]) then 
      out[i+n]:=table[blocks[i]];
    else
      k:=k+1;
      table[blocks[i]]:=k;
      out[i+n]:=k;
    fi;
  od;

  out:=Objectify(BipartitionType, rec(blocks:=out));
  
  SetDegreeOfBipartition(out, Length(blocks)/2);
  SetNrLeftBlocks(out, nrker);
  SetNrBlocks(out, k);
  #SetRankOfBipartition(out, RankOfBipartition(f));
  return out;
end);  

# linear - 2*degree 

InstallMethod(RightProjection, "for a bipartition",
[IsBipartition],
function(f)
  local n, blocks, table, out, k, nrker, lookup, i;

  n:=DegreeOfBipartition(f);
  blocks:=f!.blocks;
  table:=[];
  out:=[];
  k:=0;

  for i in [1..n] do 
    if IsBound(table[blocks[i+n]]) then 
      out[i]:=table[blocks[i+n]];
    else
      k:=k+1;
      table[blocks[i+n]]:=k;
      out[i]:=k;
    fi;
  od;

  nrker:=k;
  table:=[];
  lookup:=TransverseBlocksLookup(f);

  for i in [1..n] do 
    if blocks[i+n]<=NrLeftBlocks(f) and lookup[blocks[i+n]] then 
      out[i+n]:=out[i];
    elif IsBound(table[blocks[i+n]]) then 
      out[i+n]:=table[blocks[i+n]];
    else
      k:=k+1;
      table[blocks[i+n]]:=k;
      out[i+n]:=k;
    fi;
  od;

  out:=Objectify(BipartitionType, rec(blocks:=out));
  
  SetDegreeOfBipartition(out, n);
  SetNrLeftBlocks(out, nrker);
  SetNrBlocks(out, k);
  return out;
end);

#

InstallMethod(RandomBipartition, "for a pos int",
[IsPosInt],
function(n)
  local out, nrblocks, vals, j, nrkerblocks, i;

  out:=EmptyPlist(2*n);
  nrblocks:=0;
  vals:=[1];

  for i in [1..n] do 
    j:=Random(vals);
    if j=nrblocks+1 then 
      nrblocks:=nrblocks+1;
      Add(vals, nrblocks+1);
    fi;
    out[i]:=j;
  od;

  nrkerblocks:=nrblocks;

  for i in [1..n] do 
    j:=Random(vals);
    if j=nrblocks+1 then 
      nrblocks:=nrblocks+1;
      Add(vals, nrblocks+1);
    fi;
    out[i+n]:=j;
  od;

  out:=Objectify(BipartitionType, rec(blocks:=out));
  
  SetDegreeOfBipartition(out, n);
  SetNrLeftBlocks(out, nrkerblocks);
  SetNrBlocks(out, nrblocks);

  return out;
end);

#

InstallMethod(RightBlocks, "for a bipartition", [IsBipartition],
function(f)
  local n, blocks, tab, out, nrblocks, i;
  
  n:=DegreeOfBipartition(f);
  blocks:=f!.blocks;
  tab:=EmptyPlist(2*n);
  out:=[];
  nrblocks:=0;

  for i in [n+1..2*n] do 
    if not IsBound(tab[blocks[i]]) then 
      nrblocks:=nrblocks+1;
      tab[blocks[i]]:=nrblocks;
      if blocks[i]<=NrLeftBlocks(f) then 
        out[n+1+nrblocks]:=1; #signed
      else 
        out[n+1+nrblocks]:=0; #unsigned
      fi;
    fi;
    out[i-n+1]:=tab[blocks[i]];
  od;
  out[1]:=nrblocks;
  return out;
end);

# could use TransverseBlocksLookup if known here JDM

InstallMethod(LeftBlocks, "for a bipartition", [IsBipartition],
function(f)
  local n, blocks, tab, out, nrblocks, i;
  
  n:=DegreeOfBipartition(f);
  blocks:=f!.blocks;
  tab:=List([1..n], x-> false);
  out:=EmptyPlist(n+2);
  out[1]:=0;
  out[n+2]:=[];
  nrblocks:=0;

  for i in [1..n] do 
    out[i+1]:=blocks[i];
    if not tab[blocks[i]] then 
      out[1]:=out[1]+1;
      out[n+1+blocks[i]]:=0;
      tab[blocks[i]]:=true;
    fi;
  od;
  
  for i in [n+1..2*n] do 
    if blocks[i]<=out[1] then #transverse block!
      out[n+1+blocks[i]]:=1;
    fi;
  od;

  return out;
end);

#

InstallGlobalFunction(OnRightBlocks, 
function(blocks, f)
  local n, nrblocks, nrfblocks, fblocks, fuse, sign, fuseit, x, y, tab, out,
   next, i;

  n:=DegreeOfBlocks(blocks); # length of partition!!
  nrblocks:=blocks[1];
  
  if nrblocks=0 then   # special case for dummy/seed 
    return RightBlocks(f);
  fi;

  nrfblocks:=NrBlocks(f); 
  fblocks:=f!.blocks;
  
  fuse:=[1..nrblocks+nrfblocks];
  sign:=EmptyPlist(nrfblocks+nrblocks);

  for i in [1..nrblocks] do 
    sign[i]:=blocks[n+1+i];
  od;
  for i in [nrblocks+1..nrfblocks+nrblocks] do 
    sign[i]:=0;
  od;

  fuseit := function(i) 
    while fuse[i] < i do 
      i := fuse[i]; 
    od; 
    return i; 
  end;
  
  for i in [1..n] do
    x := fuseit(blocks[i+1]);
    y := fuseit(fblocks[i]+nrblocks);
    if x <> y then
      if x < y then
        fuse[y] := x;
        if sign[y]=1 then 
          sign[x]:=1;
        fi;
      else
        fuse[x] := y;
        if sign[x]=1 then 
          sign[y]:=1;
        fi;
      fi;
    fi;
  od;

  tab:=0*fuse;
  out:=[];
  next:=0;
  
  for i in [n+1..2*n] do
    x := fuseit(fblocks[i]+nrblocks);
    if tab[x]=0 then
      next:=next+1;
      tab[x]:=next;
    fi;
    out[i-n+1]:=tab[x];
    out[n+1+tab[x]]:=sign[x];
  od;
  out[1]:=next;
  return out;
end);

#

InstallGlobalFunction(OnLeftBlocks, 
function(blocks, f)
  local n, nrblocks, nrfblocks, fblocks, fuse, sign, fuseit, x, y, tab, out, next, i;

  n:=DegreeOfBlocks(blocks);  # length of <blocks>
  nrblocks:=blocks[1];
  
  if nrblocks=0 then 
    return LeftBlocks(f);
  fi;

  nrfblocks:=NrBlocks(f);
  fblocks:=f!.blocks;
  
  fuse:=[1..nrfblocks+nrblocks];
  sign:=EmptyPlist(nrfblocks+nrblocks);

  for i in [1..nrfblocks] do 
    sign[i]:=0;
  od;
  for i in [1..nrblocks] do 
    sign[i+nrfblocks]:=blocks[n+1+i];
  od;

  fuseit := function(i) 
    while fuse[i] < i do 
      i := fuse[i]; 
    od; 
    return i; 
  end;
  
  for i in [1..n] do
    x := fuseit(fblocks[n+i]);
    y := fuseit(blocks[i+1]+nrfblocks);
    if x <> y then
      if x < y then
        fuse[y] := x;
        if sign[y]=1 then 
          sign[x]:=1; 
        fi;
      else
        fuse[x] := y;
        if sign[x]=1 then 
          sign[y]:=1;
        fi;
      fi;
    fi;
  od;

  tab:=0*fuse;
  out:=[];
  next:=0;
  
  for i in [1..n] do
    x := fuseit(fblocks[i]);
    if tab[x]=0 then
      next := next + 1;
      tab[x] := next;
    fi;
    out[i+1]:=tab[x];
    out[n+1+tab[x]]:=sign[x];
  od;
  out[1]:=next;
  return out;
end);

#

InstallGlobalFunction(ExtRepOfBlocks,
function(blocks)
  local n, sign, out, i;
  
  n:=DegreeOfBlocks(blocks);
  out:=EmptyPlist(n);
  for i in [1..n] do 
    out[i]:=blocks[i+1];
    if blocks[n+1+blocks[i+1]]=0 then 
      out[i]:=out[i]*-1;
    fi;
  od;
    
  return out;
end);

#

InstallGlobalFunction(BlocksByExtRep,
function(ext)
  local n, tab, out, nr, i;
  
  n:=Length(ext);
  tab:=EmptyPlist(n);
  out:=EmptyPlist(n+2);
  out[n+2]:=[];
  nr:=0;
  
  for i in [1..n] do
    if ext[i]<0 then 
      out[i+1]:=-1*ext[i];
      out[n+1+out[i+1]]:=0;
    else
      out[i+1]:=ext[i];
      out[n+1+ext[i]]:=1;
    fi;
    if not IsBound(tab[out[i+1]]) then 
      tab[out[i+1]]:=true;
      nr:=nr+1;
    fi;
  od;

  out[1]:=nr;
  return out;
end);

#

InstallGlobalFunction(RankOfBlocks, 
function(blocks)
  local n, rank, i;
  
  n:=DegreeOfBlocks(blocks);
  rank:=0;
  for i in [1..blocks[1]] do 
    if blocks[n+1+i]=1 then 
      rank:=rank+1;
    fi;
  od;
  return rank;
end);

#

InstallGlobalFunction(DegreeOfBlocks,
function(blocks)
  return Length(blocks)-blocks[1]-1;
end);

#


InstallMethod(\*, "for a bipartition and bipartition",
[IsBipartition, IsBipartition], 
function(a,b)
  local n, anr, fuse, fuseit, ablocks, bblocks, x, y, tab, cblocks, next, nrleft, c, i;
  
  n := DegreeOfBipartition(a);
  Assert(1,n = DegreeOfBipartition(b));
  anr := NrBlocks(a);
  
  fuse := [1..anr+NrBlocks(b)]; 
  
  fuseit := function(i) 
    while fuse[i] < i do 
      i := fuse[i]; 
    od; 
    return i; 
  end;
 
  ablocks:=a!.blocks;
  bblocks:=b!.blocks;

  for i in [1..n] do
    x := fuseit(ablocks[i+n]);
    y := fuseit(bblocks[i]+anr);
    if x <> y then
      if x < y then
        fuse[y] := x;
      else
        fuse[x] := y;
      fi;
    fi;
  od;
  
  tab:=0*fuse;    # A table for the old part numbers
  cblocks:=EmptyPlist(2*n);
  next:=0;
  
  for i in [1..n] do
    x := fuseit(ablocks[i]);
    if tab[x]=0 then
      next:=next+1;
      tab[x]:=next;
    fi;
    cblocks[i]:=tab[x];
  od;
  
  nrleft:=next;

  for i in [n+1..2*n] do
    x:=fuseit(bblocks[i]+anr);
    if tab[x]=0 then
      next:=next+1;
      tab[x]:=next;
    fi;
    cblocks[i]:=tab[x];
  od;
  
  c:=Objectify(BipartitionType, rec(blocks:=cblocks)); 
  SetDegreeOfBipartition(c, n);
  SetNrLeftBlocks(c, nrleft);
  SetNrBlocks(c, next);
  return c;
end);

#InternalRepOfBipartition:=f-> List([1..f[1]+2], i-> f[i]);
#
#NrClassesSignedPartition:=x-> x[1];
#NrClassesBipartition:=x-> x[2];
#
#DegreeOfSignedPartition:=x-> Length(x)-NrClassesSignedPartition(x)-1;

#

#InstallGlobalFunction(INV_SIGNED_PART_BIPART, 
#function(signed, f)
#  local n, p1, p2, fuse, fuseit, x, y, x1, lookup, j, out, next, seen, i;
#
#  n := DegreeOfBipartition(f)/2;
#  Assert(1,n = DegreeOfSignedPartition(signed));
#  p1 := NrClassesSignedPartition(signed);
#  p2 := NrClassesBipartition(f);
#  fuse := [1..p1+p2]; 
#  
#  fuseit := function(i) 
#    while fuse[i] < i do 
#      i := fuse[i]; 
#    od; 
#    return i; 
#  end;
#
#  for i in [1..n] do
#    x := fuseit(signed[i+1]);
#    y := fuseit(f[i+2]+p1);
#    if x <> y then
#      if x < y then
#        fuse[y] := x;
#      else
#        fuse[x] := y;
#      fi;
#    fi;
#  od;
#
#  x1:=OnRightSignedPartition(signed, f);
#  lookup:=[1..p1+p2]*0;
#
#  for i in [1..n] do 
#    j:=fuseit(f[2+n+i]+p1);
#    j:=First([1..p1], i-> signed[n+1+i]=1 and fuseit(i)=j);
#    if x1[n+1+x1[i+1]]=1 and lookup[j]=0 then 
#      lookup[j]:=x1[i+1];
#    fi;
#  od;
#  out:=x1{[2..n+1]};
#  next:=NrClassesSignedPartition(x1)+1;
#  seen:=[1..p1]*0;
#  for i in [1..n] do 
#    if signed[n+1+signed[i+1]]=0 then 
#      if seen[signed[i+1]]<>0 then 
#        Add(out, seen[signed[i+1]]);
#      else
#        seen[signed[i+1]]:=next;
#        next:=next+1;
#        Add(out, seen[signed[i+1]]);
#      fi;
#    else 
#      Add(out, lookup[signed[i+1]]); 
#    fi;
#    if 0 in out then Error(); fi;
#  od;  
#
#  return BipartitionByIntRepNC(out);
#end);
#
#    
##
#
#InstallMethod(\^, "for a pos int and bipartition",
#[IsPosInt, IsBipartition], OnPointsBP);
#
##
#
#InstallOtherMethod(POW, "for a set of pos ints and bipartition",
#[IsListOrCollection, IsBipartition], 
#function(set, f) 
#  return Set(Concatenation(List(set, x-> OnPointsBP(x,f))));
#end);
#
##
#
#InstallMethod(\^, "for a bipartition and neg int",
#[IsBipartition, IsNegInt],
#function(f, r)
#  local foo;
#
#  foo:=function(i) 
#    if i=f[1] then 
#      return i;
#    fi;
#
#    return i mod f[1];
#  end;
#
#  if r=-1 then
#    return BipartitionNC(Set(List(ExtRepBipartition(f), x-> Set(List(x, y->
#    foo(y+f[1]/2))))));
#  fi;
#  return (f^-1)^-r;
#end);
#
##
#
#InstallMethod(\<, "for a bipartition and bipartition", 
#[IsBipartition, IsBipartition],
#function(f,g)
#  return ExtRepBipartition(f)<ExtRepBipartition(g);
#end);
#
##
#
#InstallMethod(\=, "for a bipartition and bipartition", 
#[IsBipartition, IsBipartition],
#function(f,g)
#  return ForAll([1..f[1]+2], x-> f[x]=g[x]);
#end);
#
##
#
#InstallMethod(AsBipartition, "for a permutation and pos int",
#[IsPerm, IsPosInt],
#function(f, n)
#  return BipartitionByIntRepNC(Concatenation([1..n], OnTuples([1..n], f^-1)));
#end);
#
##
#
#InstallOtherMethod(AsBipartition, "for a partial perm and pos int",
#[IsPartialPerm, IsPosInt],
#function(f, n)
#  local out, g, r, i;
#
#  g:=f^-1;
#  r:=n;
#  out:=EmptyPlist(2*n);
#
#  for i in [1..n] do 
#    out[i]:=i;
#    if i^g<>0 then 
#      out[n+i]:=i^g;
#    else 
#      r:=r+1;
#      out[n+i]:=r;
#    fi;
#  od;
#  return BipartitionByIntRepNC(out); 
#end);
#
##
#
#InstallOtherMethod(AsBipartition, "for a transformation",
#[IsTransformation],
#function(f)
#  local n, r, ker, out, g, i;
#
#  n:=DegreeOfTransformation(f);
#  r:=RankOfTransformation(f);;
#  ker:=FlatKernelOfTransformation(f); 
#  out:=ShallowCopy(ker);
#  g:=List([1..n], x-> 0);
#
#  #inverse of f
#  for i in [1..n] do 
#    g[i^f]:=i;
#  od;
#
#  for i in [1..n] do 
#    if g[i]<>0 then 
#      out[n+i]:=ker[g[i]];
#    else 
#      r:=r+1;
#      out[n+i]:=r;
#    fi;
#  od;
#  return BipartitionByIntRepNC(out);
#end);
#
##
#
#InstallMethod(DegreeOfBipartition, "for a bipartition",
#[IsBipartition], x-> x[1]);
#
##
#
#InstallMethod(DegreeOfBipartitionCollection, "for a bipartition collection",
#[IsBipartitionCollection], 
#function(coll)
#
#  if IsBipartitionSemigroup(coll) then 
#    return DegreeOfBipartitionSemigroup(coll);
#  elif not ForAll(coll, x-> x[1]=coll[1][1]) then 
#    Error("usage: collection of bipartitions of equal degree,");
#    return;
#  fi;
#  return coll[1][1];
#end);
#
##
#
#InstallMethod(DegreeOfBipartitionSemigroup, "for a bipartition semigroup",
#[IsBipartitionSemigroup], 
# s-> Representative(s)[1]);
#
#InstallMethod(Display, "for a bipartition",
#[IsBipartition], function(f)
#  Print("BipartitionNC( ", ExtRepBipartition(f), " )");
#  return;
#end);
#
#InstallMethod(Display, "for a bipartition collection",
#[IsBipartitionCollection],
#function(coll) 
#  local i;
#
#  Print("[ ");
#  for i in [1..Length(coll)] do 
#    if not i=1 then Print(" "); fi;
#    Display(coll[i]);
#    if not i=Length(coll) then 
#      Print(",\n");
#    else
#      Print(" ]\n");
#    fi;
#  od;
#  return;
#end);
#
##
#
#InstallMethod(RankOfBipartition, "for a bipartition",
#[IsBipartition], 
#function(x)
#  local n, m, seen, rank, i;
#
#  n:=x[1]/2;
#
#  m:=MaximumList(x{[3..n+2]}); # max on the left
#  seen:=BlistList([1..m], []); 
#  rank:=0;
#
#  for i in [1..n] do 
#    if x[n+2+i]<=m and not seen[x[n+2+i]] then 
#      seen[x[n+2+i]]:=true;
#      rank:=rank+1;
#    fi;
#  od;
#  return rank;
#end);
#
##
#
#InstallOtherMethod(ELM_LIST, "for a bipartition and a pos int",
#[IsBipartition, IsPosInt], ELM_LIST_BP);
#
##
#
#InstallOtherMethod(ELMS_LIST, "for a bipartition and a pos int",
#[IsBipartition, IsDenseList and IsSmallList], ELMS_LIST_BP);
#
#
##
#
##JDM this does not currently work!
#
## InstallOtherMethod(IsomorphismTransformationSemigroup,
## "for a bipartiton semigroup",
## [IsBipartitionSemigroup],
## function(s)
##   local n, pts, o, t, pos, i;
## 
##   n:=DegreeOfBipartition(Generators(s)[1]);
##   pts:=EmptyPlist(2^(n/2));
## 
##   for i in [1..n/2] do
##     o:=Orb(s, [i], OnPoints); #JDM multiseed orb
##     Enumerate(o);
##     pts:=Union(pts, AsList(o));
##   od;
##   ShrinkAllocationPlist(pts);
##   t:=Semigroup(List(GeneratorsOfSemigroup(s), 
##    x-> TransformationOp(x, pts, OnPoints)));
##   pos:=List([1..n], x-> Position(pts, [x]));
## 
##   return MappingByFunction(s, t, x-> TransformationOp(x, pts, OnPoints));
## end);
#
##
#
#
##
#
#InstallGlobalFunction(ExtRepBipartition,
#function(q)
#  local i,n2,p;
#  n2 := q[1];
#  p := List([1..q[2]],i->[]);
#  for i in [1..n2] do
#    Add(p[q[i+2]],i);
#  od;
#  return p;
#end);
#
##
#
#InstallMethod(\*, "for a bipartition and a perm",
#[IsBipartition, IsPerm],
#function(f,g)
#  return f*AsBipartition(g, DegreeOfBipartition(f)/2);
#end);
#
##
#
#InstallMethod(\*, "for a perm and a bipartition",
#[IsPerm, IsBipartition],
#function(f,g)
#  return AsBipartition(f, DegreeOfBipartition(g)/2)*g;
#end);
#
##
#
#
#


# Results:

# n=1 partitions=2 idempots=2
# n=2 partitions=15 idempots=12
# n=3 partitions=203 idempots=114
# n=4 partitions=4140 idempots=1512
# n=5 partitions=115975 idempots=25826
# n=6 partitions=4213597 idempots=541254, 33 seconds
# n=7 partitions=190899322 idempots=13479500, 1591 seconds

