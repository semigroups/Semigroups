############################################################################# 
## 
#W  examples.gi
#Y  Copyright (C) 2013-14                                 James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

#for testing purposes

# BlocksOfPartition:=function(partition)
#   local blocks, lookup, n, i, j;
#   
#   blocks:=[]; lookup:=[]; n:=0;
#   for i in [1..Length(partition)] do 
#     blocks[i]:=[n+1..partition[i]+n];
#     for j in blocks[i] do 
#       lookup[j]:=i;
#     od;
#     n:=n+partition[i]; 
#   od;  
#   return [blocks, lookup];  
# end;
# 
# IsEndomorphismOfPartition:=function(bl, f)
#   local imblock, x;
# 
#   for x in bl[1] do #blocks
#     imblock:=bl[1][bl[2][x[1]^f]];
#     if not ForAll(x, y-> y^f in imblock) then 
#       return false;
#     fi;
#   od;
#   return true;
# end;
# 
# NrEndomorphismsPartition:=function(partition)
#   local bl;
#   bl:=BlocksOfPartition(partition);
#   return Number(FullTransformationSemigroup(Sum(partition)), x-> 
#     IsEndomorphismOfPartition(bl, x));
# end;

# from the `The rank of the semigroup of transformations stabilising a partition
# of a finite set', by Araujo, Bentz, Mitchell, and Schneider (2014). 

InstallMethod(EndomorphismsPartition, "for a list of positive integers",
[IsCyclotomicCollection],
function(partition)
  local s, r, distinct, equal, prev, n, blocks, unique, didprevrepeat, gens, x, m, y, w, i, j, k, block;
  
  if not ForAll(partition, IsPosInt) then 
    Error("usage: the argument <partition> must be a list of positive integers,");
  elif ForAll(partition, x-> x=1) then 
    return FullTransformationMonoid(Length(partition));
  elif Length(partition)=1 then 
    return FullTransformationMonoid(partition[1]);
  fi;
  
  if not IsSortedList(partition) then  
    Sort(partition); #JDM does this copy? or should we?
  fi;

  # preprocessing...
  
  s:=0;         # nr of distinct block sizes
  r:=0;         # nr of block sizes with at least one other block of equal size
  distinct:=[]; # indices of blocks with distinct block sizes
  equal:=[];    # indices of blocks with at least one other block of equal size,
                # partitioned according to the sizes of the blocks 
  prev:=0;      # size of the previous block
  n:=0;         # the degree of the transformations
  blocks:=[];   # the actual blocks of the partition
  unique:=[];   # blocks of a unique size
  
  for i in [1..Length(partition)] do 
    blocks[i]:=[n+1..partition[i]+n];
    n:=n+partition[i]; 
    if partition[i]>prev then 
      s:=s+1;
      distinct[s]:=i;
      prev:=partition[i];
      didprevrepeat:=false;
      AddSet(unique, i);
    elif not didprevrepeat then
      # repeat block size
      r:=r+1;
      equal[r]:=[i-1, i];
      didprevrepeat:=true;
      RemoveSet(unique, i-1);
    else 
      Add(equal[r], i);
    fi;
  od;  
  
  # get the generators of T(X,P) over Sigma(X,P)... 
  # from the proof of Theorem 3.3
  gens:=[];
  for i in [1..Length(distinct)-1] do 
    for j in [i+1..Length(distinct)] do 
      x:=[1..n]; 
      for k in [1..Length(blocks[distinct[i]])] do 
        x[blocks[distinct[i]][k]]:=blocks[distinct[j]][k];
      od;
      Add(gens, Transformation(x));
    od;
  od;

  for block in equal do 
    x:=[1..n];
    x{blocks[block[1]]}:=blocks[block[2]];
    Add(gens, Transformation(x));
  od;

  # get the generators of Sigma(X,P) over S(X,P)...

  # the generators from B (swap blocks of adjacent distinct sizes)...
  for i in [1..s-1] do
    x:=[1..n]; 
    # map up
    for j in [1..Length(blocks[distinct[i]])] do 
      x[blocks[distinct[i]][j]]:=blocks[distinct[i+1]][j];
      x[blocks[distinct[i+1]][j]]:=blocks[distinct[i]][j];
    od;
    # map down
    for j in [Length(blocks[distinct[i]])+1..Length(blocks[distinct[i+1]])] do 
      x[blocks[distinct[i+1]][j]]:=blocks[distinct[i]][1];
    od;
    Add(gens, Transformation(x));
  od;

  # the generators from C...
  if Length(blocks[distinct[1]])<>1 then 
    x:=[1..n];
    x[1]:=2;
    Add(gens, Transformation(x));
  fi;

  for i in [2..s] do 
    if Length(blocks[distinct[i]])-Length(blocks[distinct[i-1]])>1 then 
      x:=[1..n]; 
      x[blocks[distinct[i]][1]]:=x[blocks[distinct[i]][2]];
      Add(gens, Transformation(x));
    fi;
  od;
  
  # get the generators of S(X,P)...
  if s=r or s-r>=2 then 
    # 2 generators for the r wreath products of symmetric groups 
    for i in [1..r] do 
      m:=Length(equal[i]);       #WreathProduct(S_n, S_m) m blocks of size n 
      n:=partition[equal[i][1]];
      x:=blocks{equal[i]};

      if n>1 then 
        x[2]:=Permuted(x[2], (1,2));
      fi;
      
      if IsOddInt(m) or IsOddInt(n) then 
        x:=Permuted(x, PermList(Concatenation([2..m], [1])));
      else
        x:=Permuted(x, PermList(Concatenation([1], [3..m], [2])));
      fi;

      x:=MappingPermListList(Concatenation(blocks{equal[i]}), Concatenation(x));
      Add(gens, AsTransformation(x));
      
      y:=blocks{equal[i]};
      y[1]:=Permuted(y[1],  PermList(Concatenation([2..n], [1])));
      y:=Permuted(y, (1,2));
      y:=MappingPermListList(Concatenation(blocks{equal[i]}), Concatenation(y));
      Add(gens, AsTransformation(y));
    od;
  elif s=1 and r=0 then 
    Append(gens, List(GeneratorsOfGroup(SymmetricGroup(blocks[1])),
     AsTransformation));
  elif s-r=1 and r>=1 then#JDM this case should be changed as in the previous case 
    # 2 generators for the r-1 wreath products of symmetric groups 
    for i in [1..r-1] do 
      m:=Length(equal[i]);       #WreathProduct(S_n, S_m) m blocks of size n 
      n:=partition[equal[i][1]];
      x:=blocks{equal[i]};

      if n>1 then 
        x[2]:=Permuted(x[2], (1,2));
      fi;
      
      if IsOddInt(m) or IsOddInt(n) then 
        x:=Permuted(x, PermList(Concatenation([2..m], [1])));
      else
        x:=Permuted(x, PermList(Concatenation([1], [3..m], [2])));
      fi;

      x:=MappingPermListList(Concatenation(blocks{equal[i]}), Concatenation(x));
      Add(gens, AsTransformation(x));
      
      y:=blocks{equal[i]};
      y[1]:=Permuted(y[1],  PermList(Concatenation([2..n], [1])));
      y:=Permuted(y, (1,2));
      y:=MappingPermListList(Concatenation(blocks{equal[i]}), Concatenation(y));
      Add(gens, AsTransformation(y));
    od;
    
    # 3 generators for (S_{n_k}wrS_{m_k})\times S_{l_1}
    m:=Length(equal[r]);      
    n:=partition[equal[r][1]];
    if IsOddInt(m) or IsOddInt(n) then 
      x:=Permuted(blocks{equal[r]}, PermList(Concatenation([2..m], [1])));
    else
      x:=Permuted(blocks{equal[r]}, PermList(Concatenation([1], [3..m], [2])));
    fi;

    if n>1 then 
      x[2]:=Permuted(x[2], (1,2));
    fi;
    x:=MappingPermListList(Concatenation(blocks{equal[r]}), Concatenation(x));
    Add(gens, AsTransformation(x)); # (x, id)=u in the paper

    y:=Permuted(blocks{equal[r]}, (1,2));
    y[1]:=Permuted(y[1],  PermList(Concatenation([2..n], [1])));
    y:=MappingPermListList(Concatenation(blocks{equal[r]}), Concatenation(y));
    y:=y*MappingPermListList(blocks[unique[1]],
     Concatenation(blocks[unique[1]]{[2..Length(blocks[unique[1]])]},
     [blocks[unique[1]][1]])); # (y, (1,2,\ldots, l_1))=v in the paper
    Add(gens, AsTransformation(y));
    
    if Length(blocks[unique[1]])>1 then  
      w:=MappingPermListList(blocks[unique[1]], Permuted(blocks[unique[1]], (1,2)));
      Add(gens, AsTransformation(w)); # (id, (1,2))=w in the paper
    fi;
  fi;
  if s-r>=2 then # the (s-r) generators of W_2 in the proof
    for i in [1..s-r-1] do 
      if Length(blocks[unique[i]])<>1 then 
        x:=Permuted(blocks[unique[i]], (1,2));
      else
        x:=ShallowCopy(blocks[unique[i]]);
      fi;
      if IsOddInt(Length(blocks[unique[i+1]])) then 
        Append(x, Permuted(blocks[unique[i+1]], 
         PermList(Concatenation([2..Length(blocks[unique[i+1]])], [1]))));
      else
        Append(x, Permuted(blocks[unique[i+1]], 
         PermList(Concatenation([1], [3..Length(blocks[unique[i+1]])], [2]))));
      fi;
      x:=MappingPermListList(Union(blocks[unique[i]], blocks[unique[i+1]]), 
       x);
      if x<>() then 
        Add(gens, AsTransformation(x));
      fi;
    od;
    
    x:=[];
    if partition[unique[1]]<>1 then  
      if IsOddInt(partition[unique[1]]) then 
        Append(x, Permuted(blocks[unique[1]], 
         PermList(Concatenation([2..Length(blocks[unique[1]])], [1]))));
      else
        Append(x, Permuted(blocks[unique[1]], 
         PermList(Concatenation([1], [3..Length(blocks[unique[1]])], [2]))));
      fi;
    else
      Append(x, blocks[unique[1]]);
    fi;
    Append(x, Permuted(blocks[unique[s-r]], (1,2)));
    x:=MappingPermListList(Concatenation(blocks[unique[1]], blocks[unique[s-r]]), x);
    Add(gens, AsTransformation(x));
  fi;
  
  return Semigroup(gens);
end);

#

InstallMethod(RegularBinaryRelationSemigroup, "for a positive integer",
[IsPosInt],
function(n) 
  local gens, s;

  gens:=[ Concatenation(List([2..n], x-> [x]),[[1]]), 
          Concatenation([[2],[1]], List([3..n], x-> [x])),
          Concatenation(List([1..n-1], x-> [x]), [[1,n]]),
          Concatenation(List([1..n-1], x-> [x]), [[]]) ] ;
  
  s:=Semigroup(List(gens, BinaryRelationByListOfImagesNC));
  #SetIsBinaryRelationCollection(s, true);
  return s;
end);

#

InstallMethod(FullMatrixSemigroup, "for pos int and pos int",  
[IsPosInt, IsPosInt],
function(d,q)
  local g, S;

  g:=List([1..d], x-> List([1..d], function(y) if y=x and not y=d then
   return Z(q)^0; else return 0*Z(q); fi; end));
  g:=OneMutable(GeneratorsOfGroup(GL(d,q))[1]);
  g[d][d]:=Z(q)*0;

  S:=Monoid(Concatenation(GeneratorsOfGroup(GL(d,q)), [g]));
  SetIsFullMatrixSemigroup(S, true);
  SetIsGeneralLinearSemigroup(S, true);
  SetIsRegularSemigroup(S, true);
  
  return S;
end);

#

InstallMethod(ViewObj, "for a full matrix semigroup",
[IsFullMatrixSemigroup and HasGeneratorsOfSemigroup], 4,
function(s) 
  local n;

  Print("<full matrix semigroup ");
  n:=Length(GeneratorsOfSemigroup(s)[1][1]);
  Print(n, "x", n, " over ", BaseDomain(GeneratorsOfSemigroup(s)[1][1]));
  Print(">");
end);

#

InstallMethod(GeneralLinearSemigroup, "for 2 pos ints", 
[IsPosInt, IsPosInt], FullMatrixSemigroup);

#

InstallMethod(IsFullMatrixSemigroup, "for a semigroup", 
[IsSemigroup], ReturnFalse);

#JDM method for IsFullMatrixSemigroup for a matrix semigroup

# undocumented, from the semigroupe manual... JDM is this right?

InstallMethod(MonoidOfMultiplicationByN, "for a positive integer",
[IsPosInt],
function(n)
  local out, i;

  out:=EmptyPlist(n);
  for i in [1..n] do 
    out[2*i-1]:=i;
    out[2*i]:=i;
  od;

  return Monoid(Transformation(out{[1..n]}),Transformation(out{[n+1..2*n]}));
end);

#

if not (IsGrapeLoaded and IsGrapeCompiled) then
  InstallMethod(MunnSemigroup, "for a semilattice", [IsSemigroup], 
  function(S)
    Info(InfoWarning, 1, GrapeIsNotCompiledString);
    return fail;
  end);
else
  # JDM use ClosureInverseSemigroup to improve things here!
  InstallMethod(MunnSemigroup, "for a semilattice", [IsSemigroup],
  function(s)
  local sl, GraphFromIdeal, IdealOfSemilattice, AutGpIdeal, IdentityTrans, 
  d, max, ideals, out, min, n, f, j, g, not_iso, k, g_j, g_k, p, i;

    if not IsSemilatticeAsSemigroup(s) then 
      Info(InfoWarning, 1, "usage: argument should be a semilattice,");
      return fail;
    fi;

    sl:=PartialOrderOfDClasses(s);

    ############

    GraphFromIdeal:=function(sl, ideal)
      local adj, i;
      adj:=[];
      for i in [1..Size(sl)] do
        if i in ideal then
          adj[i]:=sl[i];
        fi;
      od;
      return Graph(Group(()), ideal, OnPoints, function(i,j) return j in adj[i];
      end, true);
    end;

    ############

    IdealOfSemilattice:=function(sl, i)
      local out;
      out:=Difference(Union(sl{sl[i]}), sl[i]);
      return Union(sl[i], Union(List(out, x-> IdealOfSemilattice(sl, x))));
    end;

    ############

    AutGpIdeal:=function(sl, ideal)
      local g;
      g:=GraphFromIdeal(sl, ideal);
      return AutGroupGraph(g)^(MappingPermListList(ideal,
       Vertices(g))^-1);
    end;

   ############
      
    d:=List([1..Size(sl)], i-> IdealOfSemilattice(sl, i));
    max:=Maximum(List(d, Length));
    ideals:=List([1..max], x-> []);

    for i in [1..Length(d)] do
      Add(ideals[Length(d[i])],d[i]);
    od;

    out:=[];

    min:=ideals[1][1][1]; n:=Size(sl);
    Add(out, PartialPermNC([min], [min]));

    for i in ideals[2] do
      for j in ideals[2] do
        f:=ListWithIdenticalEntries(n, 0);
        f[min]:=min;
        f[Difference(i, [min])[1]]:=Difference(j, [min])[1];
        Add(out, PartialPermNC(f));
      od;
    od;

    for i in [3..Length(ideals)] do
      while not ideals[i]=[] do
        j:=ideals[i][1];
        ideals[i]:=Difference(ideals[i], [j]);
        f:=PartialPermNC(j, j); g:=AutGpIdeal(sl, j);
        if not IsTrivial(g) then
          Append(out, List(GeneratorsOfGroup(g), x-> f*x));
        else
          Add(out, f);
        fi;
        not_iso:=[];
        for k in ideals[i] do
          g_j:=GraphFromIdeal(sl, j); g_k:=GraphFromIdeal(sl, k);
          p:=GraphIsomorphism(g_j, g_k);
          if not p=fail then
            p:=MappingPermListList(j,
             Vertices(g_j))*p*MappingPermListList(Vertices(g_k), k);
            Add(out, f*p);
            Add(out, PartialPermNC(k, k)*p^-1);
          else
            Add(not_iso, k);
          fi;
        od;
        ideals[i]:=not_iso;
      od;
    od;

    return InverseSemigroup(out); 
  end);
fi;

#

InstallMethod(OrderEndomorphisms, "for a positive integer",
[IsPosInt],
function(n)
  local gens, s, i;

  gens:=EmptyPlist(n);
  gens[1]:=Transformation(Concatenation([1], [1..n-1]));

  for i in [1..n-1] do
    gens[i+1]:=[1..n];
    gens[i+1][i]:=i+1;
    gens[i+1]:=TransformationNC(gens[i+1]);
  od; 

  s:=Monoid(gens);
  SetIsRegularSemigroup(s, true);
  return s;
end);

#

InstallMethod(PartialTransformationSemigroup, "for a positive integer", 
[IsPosInt],
function(n)
  local a, b, c, d, s;

  a:= [1..n+1];  a[1]:= 2;  a[2]:= 1;
  b:= [0..n];  b[1]:= n;  b[n+1]:= n+1;
  c:= [1..n+1];  c[1]:= n+1;
  d:= [1..n+1];  d[1]:= 2;

  s:=Monoid(List([a, b, c, d], TransformationNC));
  SetIsRegularSemigroup(s, true);
  return s;
end);

#

InstallMethod(PartitionMonoid, "for a positive integer",
[IsPosInt], 
function(n)
  local gens;

  if n=1 then 
    return Monoid(BipartitionNC([[1],[-1]]));
  fi;

  gens:=List(GeneratorsOfGroup(SymmetricGroup(n)), x-> AsBipartition(x, n));
  Add(gens, AsBipartition(PartialPermNC([2..n], [2..n]), n));
  Add(gens, BipartitionNC(Concatenation([[1,2,-1, -2]], 
   List([3..n], x-> [x, -x]))));

  return Monoid(gens, rec(regular:=true));
end);

#

InstallMethod(DualSymmetricInverseSemigroup, "for a positive integer",
[IsPosInt], 
function(n)
  local gens, s;
  
  if n=1 then 
    return Semigroup(BipartitionNC([[1,-1]]));
  fi;
  
  gens:=List(GeneratorsOfGroup(SymmetricGroup(n)), x-> AsBipartition(x, n));
  
  if n=2 then 
    Add(gens, BipartitionNC([[1,2,-1,-2]]));
  else 
    Add(gens, BipartitionNC(Concatenation([[1,2,-3], [3,-1,-2]],
     List([4..n], x-> [x, -x]))));
  fi;
  s:=InverseMonoid(gens);
  return s;
end);

#

InstallMethod(FactorisableDualSymmetricInverseSemigroup, 
"for a positive integer", [IsPosInt], 
function(n)
  local gens;
  gens:=List(GeneratorsOfGroup(SymmetricGroup(n)), x-> AsBipartition(x, n));
  Add(gens, BipartitionNC(Concatenation([[1,2,-1,-2]],
   List([3..n], x-> [x, -x]))));
  return InverseMonoid(gens);
end);

#

InstallMethod(BrauerMonoid, "for a positive integer", [IsPosInt],
function(n)
  local gens;

  if n=1 then 
    return Semigroup(BipartitionNC([[1,-1]]));
  fi;
  gens:=List(GeneratorsOfGroup(SymmetricGroup(n)), x-> AsBipartition(x, n));
  Add(gens, BipartitionNC(Concatenation([[1,2]], 
   List([3..n], x-> [x, -x]),[[-1,-2]])));
  return Monoid(gens, rec(regular:=true));
end);

#

InstallMethod(PartialBrauerMonoid, "for a positive integer", [IsPosInt],
function(n)
  local gens;

  if n=1 then 
    return Semigroup(BipartitionNC([[1,-1]]));
  fi;

  gens:=List(GeneratorsOfGroup(SymmetricGroup(n)), x-> AsBipartition(x, n));
  Add(gens, BipartitionNC(Concatenation([[1,2]], 
   List([3..n], x-> [x, -x]),[[-1,-2]])));
  Add(gens, AsBipartition(PartialPermNC([2..n], [2..n]), n));
  return Monoid(gens, rec(regular:=true));
end);

#

InstallMethod(JonesMonoid, "for a positive integer", [IsPosInt],
function(n)
  local gens, next, s, i, j;
 
  if n=1 then 
    return Monoid(BipartitionNC([[1,-1]]));
  fi;

  gens:=[];
  for i in [1..n-1] do 
    next:=[];
    for j in [1..i-1] do 
      next[j]:=j;
      next[n+j]:=j;
    od;
    next[i]:=i; next[i+1]:=i;
    next[i+n]:=n; next[i+n+1]:=n;
    for j in [i+2..n] do 
      next[j]:=j-1;
      next[n+j]:=j-1;
    od;
    gens[i]:=BipartitionByIntRep(next);
  od;
  return Monoid(gens, rec(regular:=true));
end);

#

InstallMethod(TriapsisMonoid, "for a positive integer", [IsPosInt],
function(n)
  local gens, next, i, j;
  
  gens:=[];
  for i in [1..n-2] do 
    next:=[];
    for j in [1..i-1] do 
      next[j]:=j;
      next[n+j]:=j;
    od;
    next[i]:=i; next[i+1]:=i;  next[i+2]:=i;
    next[i+n]:=n-1; next[i+n+1]:=n-1; next[i+n+2]:=n-1;
    for j in [i+3..n] do 
      next[j]:=j-2;
      next[n+j]:=j-2;
    od;
    gens[i]:=BipartitionByIntRep(next);
  od;
  return Semigroup(gens);
end);

#

InstallMethod(POI, "for a positive integer",
[IsPosInt],
function(n)
  local out, i;

  out:=EmptyPlist(n);
  out[1]:=PartialPermNC([0..n-1]);
  for i in [0..n-2] do 
    out[i+2]:=[1..n];
    out[i+2][(n-i)-1]:=n-i; out[i+2][n-i]:=0;
    out[i+2]:=PartialPermNC(out[i+2]);
  od;
  return InverseMonoid(out); 
end);

#

InstallMethod(POPI, "for a positive integer",
[IsPosInt],
function(n)
  return InverseMonoid(PartialPermNC(Concatenation([2..n],[1])), 
     PartialPermNC(Concatenation([1..n-2],[n])));
end);

#

InstallMethod(PowerSemigroup, "for a group",
[IsGroup],
function(g)
  local act, dom, gens, s, i, f;

  act:=function(A, B) return Union(List(A, x-> x*B)); end;
  dom:=Combinations(Elements(g));
  Sort(dom, function(x,y) return Length(x)<Length(y); end);
  gens:=[TransformationOp(dom[1], dom, act)];
  s:=Semigroup(gens);
  i:=2;

  while Size(s)<2^Size(g) do  
    i:=i+1;
    f:=TransformationOp(dom[i], dom, act);
    s:=ClosureSemigroup(s, f);
  od;
  return s;
end);

#

InstallMethod(SingularPartitionMonoid, "for a positive integer",
[IsPosInt],
function(n)
  local blocks, i;
  
  blocks:=[[1,2,-1,-2]];
  for i in [3..n] do 
    blocks[i-1]:=[i, -i];
  od;
  return SemigroupIdeal(PartitionMonoid(n), Bipartition(blocks));
end);

#

InstallMethod(SingularTransformationSemigroup, "for a positive integer",
[IsPosInt],
function(n)
  local x, S;
  
  x:=TransformationNC(Concatenation([1..n-1], [n-1]));
  S:=FullTransformationSemigroup(n);
  return SemigroupIdeal(S, x);
end);

#

InstallMethod(SingularOrderEndomorphisms, "for a positive integer",
[IsPosInt],
function(n)
  local x, S;
  
  x:=TransformationNC(Concatenation([1..n-1], [n-1]));
  S:=OrderEndomorphisms(n);
  return SemigroupIdeal(S, x);
end);

#

InstallMethod(SingularBrauerMonoid, "for a positive integer",
[IsPosInt],
function(n)
  local blocks, x, S, i;
  
  blocks:=[[1,2], [-1,-2]];
  for i in [3..n] do 
    blocks[i]:=[i, -i];
  od;
  x:=Bipartition(blocks);
  S:=BrauerMonoid(n);
  return SemigroupIdeal(S, x);
end);

#

InstallMethod(SingularJonesMonoid, "for a positive integer",
[IsPosInt],
function(n)
  local blocks, x, S, i;
  
  blocks:=[[1,2], [-1,-2]];
  for i in [3..n] do 
    blocks[i]:=[i, -i];
  od;
  x:=Bipartition(blocks);
  S:=JonesMonoid(n);
  return SemigroupIdeal(S, x);
end);

#

InstallMethod(SingularDualSymmetricInverseSemigroup, "for a positive integer",
[IsPosInt],
function(n)
  local blocks, x, S, i;
  
  blocks:=[[1,2,-1,-2]];
  for i in [3..n] do 
    blocks[i-1]:=[i, -i];
  od;
  x:=Bipartition(blocks);
  S:=DualSymmetricInverseMonoid(n);
  return SemigroupIdeal(S, x);
end);

#

InstallMethod(SingularFactorisableDualSymmetricInverseSemigroup, 
"for a positive integer", [IsPosInt],
function(n)
  local blocks, x, S, i;
  
  blocks:=[[1,2,-1,-2]];
  for i in [3..n] do 
    blocks[i-1]:=[i, -i];
  od;
  x:=Bipartition(blocks);
  S:=FactorisableDualSymmetricInverseSemigroup(n);
  return SemigroupIdeal(S, x);
end);

#EOF
