
BindGlobal("BipartitionFamily", NewFamily("BipartitionFamily",
 IsBipartition, CanEasilySortElements, CanEasilySortElements));

BindGlobal("BipartitionType", NewType(BipartitionFamily,
 IsBipartition and IsDataObjectRep and IsActingElt));


# new for 1.0! - \^ - "for a pos int and bipartition" 
############################################################################# 

InstallMethod(\^, "for a pos int and bipartition",
[IsPosInt, IsBipartition], OnPointsBP);

# new for 1.0! - \^ - "for a pos int and bipartition" 
############################################################################# 

InstallOtherMethod(POW, "for a set of pos ints and bipartition",
[IsListOrCollection, IsBipartition], 
function(set, f) 
  return Set(Concatenation(List(set, x-> OnPointsBP(x,f))));
end);

# new for 1.0! - \^ - "for a bipartition and neg int"
#############################################################################

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

# new for 1.0! - \< - "for a bipartition and bipartition"
############################################################################

InstallMethod(\<, "for a bipartition and bipartition", 
[IsBipartition, IsBipartition],
function(f,g)
  return ExtRepBipartition(f)<ExtRepBipartition(g);
end);

# new for 1.0! - \= - "for a bipartition and bipartition"
############################################################################

InstallMethod(\=, "for a bipartition and bipartition", 
[IsBipartition, IsBipartition],
function(f,g)
  return f[1]=g[1] and ForAll([1..f[1]+2], x-> f[x]=g[x]);
end);

# new for 1.0! - AsBipartition - "for a permutation and pos int"
############################################################################

InstallMethod(AsBipartition, "for a permutation and pos int",
[IsPerm, IsPosInt],
function(f, n)
  return BipartitionByIntRep(Concatenation([2*n, n], [1..n], 
   OnTuples([1..n], f^-1)));
end);

# new for 1.0! - AsBipartition - "for a partial perm and pos int"
############################################################################

InstallOtherMethod(AsBipartition, "for a partial perm and pos int",
[IsPartialPerm, IsPosInt],
function(f, n)
  local out, g, r, i;

  out:=[2*n];
  g:=f^-1;
  r:=n;

  for i in [1..n] do 
    out[i+2]:=i;
    if i^g<>fail then 
      out[n+i+2]:=i^g;
    else 
      r:=r+1;
      out[n+i+2]:=r;
    fi;
  od;
  out[2]:=r;
  return BipartitionByIntRep(out); 
end);

# new for 1.0! - AsBipartition - "for a transformation"
############################################################################

InstallOtherMethod(AsBipartition, "for a transformation",
[IsTransformation],
function(f)
  local n, r, ker, out, g, i;

  n:=f[1];
  r:=f[2];
  ker:=KerT(f); 
  out:=Concatenation([2*n, n], ker);
  g:=List([1..f[1]], x-> 0);

  for i in RanSetT(f) do 
    g[f[i+4]]:=i;
  od;

  for i in [1..n] do 
    if g[i]<>0 then 
      out[n+i+2]:=ker[g[i]];
    else 
      r:=r+1;
      out[n+i+2]:=r;
    fi;
  od;
  return BipartitionByIntRep(out);
end);

# new for 1.0! - DegreeOfBipartition - "for a bipartition"
############################################################################

InstallMethod(DegreeOfBipartition, "for a bipartition",
[IsBipartition], x-> x[1]);

# new for 1.0! - DegreeOfBipartition - "for a bipartition"
############################################################################

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

# new for 1.0! - DegreeOfBipartition - "for a bipartition"
############################################################################

InstallMethod(DegreeOfBipartitionSemigroup, "for a bipartition semigroup",
[IsBipartitionSemigroup], 
 s-> Representative(s)[1]);

# new for 1.0! - RankOfBipartition - "for a bipartition"
############################################################################

InstallMethod(RankOfBipartition, "for a bipartition",
[IsBipartition], x-> x[2]);

# new for 1.0! - ELM_LIST - "for a bipartition and pos int"
############################################################################

InstallOtherMethod(ELM_LIST, "for a bipartition and a pos int",
[IsBipartition, IsPosInt], ELM_LIST_BP);

# new for 1.0! - InternalRepOfBipartition - "for a bipartition"
#############################################################################

InternalRepOfBipartition:=f-> List([1..f[1]+2], i-> f[i]);

# new for 1.0! - IsomorphismTransformationSemigroup - "for a bipartition semi"
###########################################################################

InstallOtherMethod(IsomorphismTransformationSemigroup,
"for a bipartiton semigroup",
[IsBipartitionSemigroup],
function(s)
  local n, pts, o, t, pos, i;

  n:=DegreeOfBipartition(Generators(s)[1]);
  pts:=EmptyPlist(2^(n/2));

  for i in [1..n/2] do
    o:=Orb(s, [i], OnPoints); #JDM multiseed orb
    Enumerate(o);
    pts:=Union(pts, AsList(o));
  od;
  ShrinkAllocationPlist(pts);
  t:=Semigroup(TransformationActionNC(s, pts, OnPoints));
  pos:=List([1..n], x-> Position(pts, [x]));

  return MappingByFunction(s, t, x-> TransformationActionNC(x, pts, OnPoints));
end);

# new for 1.0! - PrintObj - "for a bipartition"
#############################################################################

InstallMethod(PrintObj, "for a bipartition",
[IsBipartition],
function(f)
  local ext, n, i, j;
  
  Print("<bipartition: [ ");
  ext:=ExtRepBipartition(f);
  n:=DegreeOfBipartition(f)/2;
  for i in [1..RankOfBipartition(f)] do 

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
    if i=RankOfBipartition(f) then 
      Print("] ");
    else
      Print("], ");
    fi;
  od;
  
  Print(">");
  return;
end);

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

# new for 0.7! - PrintObj - "for a bipartition semigroup"
################################################################################

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
  c := EmptyPlist(2*n+1);
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
  return BipartitionByIntRep(Concatenation([2*n, next-1], c));
# return Concatenation([2*n, next-1], c);
end);

# part should be of the form [4,1,2,3,3,4,4,1,0,1,0]
# [rank,partition in internal rep, signing]
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

InstallMethod(OnRightSignedPartitionWithBipartition, 
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

Do := function(n)
    local eee,ppp,qqq;
    ppp := PartitionsSet([1..2*n]);
    #qqq := List(ppp,PartitionExtRep);
    #eee := Filtered(qqq,x->x=PartitionComposition(x,x));
    #Print("n=",n," partitions=",Length(ppp)," idempots=",Length(eee),"\n");
    #return [n,Length(ppp),Length(eee)];
end;

MakePartitions := function(n,f)
  # This makes all partitions of 2*n in internal format and
  # calls f on them.
  local Recurse,i,v;

  Recurse := function(n,depth,v,nrparts,f)
    local i;
    if depth > 2*n then
        v[2*n+1] := nrparts;
        f(v);
        return;
    fi;
    for i in [1..nrparts] do
        v[depth] := i;
        Recurse(n,depth+1,v,nrparts,f);
    od;
    v[depth] := nrparts+1;
    Recurse(n,depth+1,v,nrparts+1,f);
  end;

  v := EmptyPlist(2*n+1);
  Recurse(n,1,v,0,f);
end;


# Results:

# n=1 partitions=2 idempots=2
# n=2 partitions=15 idempots=12
# n=3 partitions=203 idempots=114
# n=4 partitions=4140 idempots=1512
# n=5 partitions=115975 idempots=25826
# n=6 partitions=4213597 idempots=541254, 33 seconds
# n=7 partitions=190899322 idempots=13479500, 1591 seconds

DoCount := function(n)
    local counteri,counterp,Tester;
    counterp := 0;
    counteri := 0;
    Tester := function(x) 
      counterp := counterp + 1;
      if x = x*x then
        counteri := counteri + 1; 
      fi; 
    end;
    MakePartitions(n,Tester);
    return [n,counterp,counteri];
end;

