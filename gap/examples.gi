############################################################################# 
## 
#W  examples.gi
#Y  Copyright (C) 2013                                    James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

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

if Filename(DirectoriesPackagePrograms("grape"),"dreadnautB") = fail then
  InstallMethod(MunnSemigroup, "for a semilattice", [IsSemigroup], 
  function(s)
    Info(InfoWarning, 1, "the nauty/dreadnaut binaries in GRAPE are not", 
    " installed and so this ");
    Info(InfoWarning, 1, "function does not work.");
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

#
#
#
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
    if not f in s then 
      Add(gens, f);
      s:=Semigroup(gens);
    fi;
  od;
  return s;
end);

#

InstallMethod(SingularTransformationSemigroup, "for a positive integer",
[IsPosInt],
function(n)
  local x, S, T;
  
  x:=TransformationNC(Concatenation([1..n-1], [n-1]));
  S:=FullTransformationSemigroup(n);
  T:=SubsemigroupNC(S, Idempotents(GreensDClassOfElementNC(S, x)));
  SetIsRegularSemigroup(T, true);
  return T;
end);

#EOF
