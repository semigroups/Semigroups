############################################################################# 
## 
#W  semibipart.gi 
#Y  Copyright (C) 2013                                    James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

# this is just a composition of IsomorphismTransformationSemigroup and the
# method below for IsomorphismBipartitionSemigroup...

InstallMethod(IsomorphismBipartitionSemigroup, "for a semigroup", [IsSemigroup],
function(S)
  local en, act, gens;

  en:=EnumeratorSorted(S);
  
  act:=function(i, x)
    if i<=Length(en) then 
      return Position(en, en[i]*x);
    fi;
    return Position(en, x);
  end;
  
  gens:=List(en, x-> AsBipartition(TransformationOp(x, [1..Length(en)+1],
   act), Length(en)+1));

  return MagmaIsomorphismByFunctionsNC(S, Semigroup(gens), 
   x-> AsBipartition(TransformationOp(x, [1..Length(en)+1], act), Length(en)+1),  
   x-> en[(Length(en)+1)^AsTransformation(x)]);
end);

#

InstallMethod(IsomorphismBipartitionSemigroup, 
"for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local n, source, range, i;
 
  n:=DegreeOfTransformationSemigroup(S);
  source:=GeneratorsOfSemigroup(S); 
  range:=EmptyPlist(Length(source));

  for i in [1..Length(source)] do 
    range[i]:=AsBipartition(source[i], n);
  od;

  return MagmaIsomorphismByFunctionsNC(S, Semigroup(range), 
   x-> AsBipartition(x, n), AsTransformation);
end);

# the converse of the previous method

InstallMethod(IsomorphismTransformationSemigroup, 
"for a bipartition semigroup with generators",
[IsBipartitionSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local n, source, range, i;
  
  if not ForAll(GeneratorsOfSemigroup(S), IsTransBipartition) then 
    TryNextMethod();
  fi;
  
  n:=DegreeOfBipartitionSemigroup(S);
  source:=GeneratorsOfSemigroup(S); 
  range:=EmptyPlist(Length(source));

  for i in [1..Length(source)] do 
    range[i]:=AsTransformation(source[i]);
  od;

  return MagmaIsomorphismByFunctionsNC(S, Semigroup(range), 
   AsTransformation, x-> AsBipartition(x, n));
end);

#

InstallMethod(IsomorphismBipartitionSemigroup, 
"for a partial perm semigroup with generators",
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local n, source, range, i;
 
  n:=Maximum(DegreeOfPartialPermSemigroup(S), CodegreeOfPartialPermSemigroup(S));
  source:=GeneratorsOfSemigroup(S); 
  range:=EmptyPlist(Length(source));

  for i in [1..Length(source)] do 
    range[i]:=AsBipartition(source[i], n);
  od;

  return MagmaIsomorphismByFunctionsNC(S, Semigroup(range), 
   x-> AsBipartition(x, n), AsPartialPerm);
end);

# the converse of the previous method

InstallMethod(IsomorphismPartialPermSemigroup, 
"for a bipartition semigroup with generators",
[IsBipartitionSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local n, source, range, i;
 
  if not ForAll(GeneratorsOfSemigroup(S), IsPartialPermBipartition) then 
    return fail;
  fi;

  n:=DegreeOfBipartitionSemigroup(S);
  source:=GeneratorsOfSemigroup(S); 
  range:=EmptyPlist(Length(source));

  for i in [1..Length(source)] do 
    range[i]:=AsPartialPerm(source[i]);
  od;

  return MagmaIsomorphismByFunctionsNC(S, Semigroup(range), 
   AsPartialPerm, x-> AsBipartition(x, n));
end);

#

InstallMethod(IsomorphismBipartitionSemigroup, 
"for a partial perm inverse semigroup with generators",
[IsPartialPermSemigroup and IsInverseSemigroup and HasGeneratorsOfInverseSemigroup],
function(S)
  local n, source, range, i;
 
  n:=Maximum(DegreeOfPartialPermSemigroup(S), CodegreeOfPartialPermSemigroup(S));
  source:=GeneratorsOfInverseSemigroup(S); 
  range:=EmptyPlist(Length(source));

  for i in [1..Length(source)] do 
    range[i]:=AsBipartition(source[i], n);
  od;

  return MagmaIsomorphismByFunctionsNC(S, InverseSemigroup(range), 
   x-> AsBipartition(x, n), AsPartialPerm);
end);

# the converse of the last method

InstallMethod(IsomorphismPartialPermSemigroup, 
"for a bipartition inverse semigroup with generators",
[IsBipartitionSemigroup and IsInverseSemigroup and HasGeneratorsOfInverseSemigroup],
function(S)
  local n, source, range, i;
 
  if not ForAll(GeneratorsOfInverseSemigroup(S), IsPartialPermBipartition) then 
    TryNextMethod();
  fi;

  n:=DegreeOfBipartitionSemigroup(S);
  source:=GeneratorsOfSemigroup(S); 
  range:=EmptyPlist(Length(source));

  for i in [1..Length(source)] do 
    range[i]:=AsPartialPerm(source[i]);
  od;

  return MagmaIsomorphismByFunctionsNC(S, InverseSemigroup(range), 
   AsPartialPerm, x-> AsBipartition(x, n));
end);

#

InstallMethod(IsomorphismBipartitionSemigroup, 
"for a perm group with generators",
[IsPermGroup and HasGeneratorsOfGroup],
function(S)
  local n, source, range, i;
 
  n:=LargestMovedPoint(S);
  source:=GeneratorsOfGroup(S); 
  range:=EmptyPlist(Length(source));

  for i in [1..Length(source)] do 
    range[i]:=AsBipartition(source[i], n);
  od;

  return MagmaIsomorphismByFunctionsNC(S, Semigroup(range), 
   x-> AsBipartition(x, n), AsPermutation);
end);

# the converse of the previous method

InstallMethod(IsomorphismPermGroup, 
"for a bipartition semigroup with generators",
[IsBipartitionSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local n, source, range, i;

  if not ForAll(GeneratorsOfSemigroup(S), IsPermBipartition) then 
    TryNextMethod();
  fi;

  n:=LargestMovedPoint(S);
  source:=GeneratorsOfSemigroup(S); 
  range:=EmptyPlist(Length(source));

  for i in [1..Length(source)] do 
    range[i]:=AsPermutation(source[i]);
  od;

  return MagmaIsomorphismByFunctionsNC(S, Semigroup(range), 
   AsPermutation, x-> AsBipartition(x, n));
end);

# this is one way, i.e. no converse method

InstallMethod(IsomorphismBlockBijectionSemigroup, 
"for an inverse partial perm semigroup with generators",
[IsPartialPermSemigroup and IsInverseSemigroup and HasGeneratorsOfInverseSemigroup],
function(S)
  local n, source, range, i, inv;
 
  n:=DegreeOfPartialPermSemigroup(S)+1;
  source:=GeneratorsOfInverseSemigroup(S); 
  range:=EmptyPlist(Length(source));

  for i in [1..Length(source)] do 
    range[i]:=AsBlockBijection(source[i], n);
  od;

  # AsPartialPerm for a block bijection created using AsBlockBijection with
  # argument a partial perm
  inv:=function(x) 
    local blocks, n, seen, i, lookup, out, bigblock; 
    
    blocks:=x!.blocks;
    n:=DegreeOfBipartition(x);
    bigblock:=blocks[n];

    # find the images of [1..n]
    lookup:=EmptyPlist(n-1);
    for i in [1..n-1] do 
      lookup[blocks[i+n]]:=i;
    od;

    # put it together
    out:=[1..n-1]*0;
    for i in [1..n-1] do 
      if blocks[i]<>bigblock then 
        out[i]:=lookup[blocks[i]];
      fi;
    od;

    return PartialPerm(out);
  end;

  return MagmaIsomorphismByFunctionsNC(S, InverseSemigroup(range), 
   x-> AsBlockBijection(x, n), inv);
end);

# this is one way, i.e. no converse method

InstallMethod(IsomorphismBlockBijectionSemigroup, 
"for a partial perm semigroup with generators",
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local n, source, range, i, inv;
 
  n:=Maximum(DegreeOfPartialPermSemigroup(S), CodegreeOfPartialPermSemigroup(S))+1;
  source:=GeneratorsOfSemigroup(S); 
  range:=EmptyPlist(Length(source));

  for i in [1..Length(source)] do 
    range[i]:=AsBlockBijection(source[i], n);
  od;

  # AsPartialPerm for a block bijection created using AsBlockBijection with
  # argument a partial perm
  inv:=function(x) 
    local blocks, n, seen, i, lookup, out, bigblock; 
    
    blocks:=x!.blocks;
    n:=DegreeOfBipartition(x);
    bigblock:=blocks[n];

    # find the images of [1..n]
    lookup:=EmptyPlist(n-1);
    for i in [1..n-1] do 
      lookup[blocks[i+n]]:=i;
    od;

    # put it together
    out:=[1..n-1]*0;
    for i in [1..n-1] do 
      if blocks[i]<>bigblock then 
        out[i]:=lookup[blocks[i]];
      fi;
    od;

    return PartialPerm(out);
  end;

  return MagmaIsomorphismByFunctionsNC(S, Semigroup(range), 
   x-> AsBlockBijection(x, n), inv);
end);

# JDM could have a method for
# IsomorphismBlockBijectionSemigroup for IsPartialPermBipartitions too..

#

InstallMethod(IsGeneratorsOfInverseSemigroup, "for a bipartition collection", 
[IsBipartitionCollection], 
function(coll)
  return ForAll(coll, IsBlockBijection) or ForAll(coll, IsPartialPermBipartition);
end);

#

InstallMethod(GeneratorsOfInverseSemigroup,
"for an inverse bipartition semigroup with generators",
[IsBipartitionSemigroup and IsInverseSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, pos, f;

  gens:=ShallowCopy(GeneratorsOfSemigroup(s));
  for f in gens do
    pos:=Position(gens, f^-1);
    if pos<>fail and f<>f^-1 then 
      Remove(gens, pos);
    fi;
  od;
  MakeImmutable(gens);
  return gens;
end);

#

InstallMethod(GeneratorsOfInverseMonoid,
"for an inverse bipartition monoid with generators",
[IsBipartitionSemigroup and IsInverseMonoid and HasGeneratorsOfMonoid],
function(s)
  local gens, one, pos, f;

  gens:=ShallowCopy(GeneratorsOfMonoid(s));
  one:=One(s);
  for f in gens do
    pos:=Position(gens, f^-1);
    if pos<>fail and (f<>f^-1 or f=one) then 
      Remove(gens, pos);
    fi;
  od;
  MakeImmutable(gens);
  return gens;
end);

#

InstallImmediateMethod(GeneratorsOfSemigroup, 
IsBipartitionSemigroup and HasGeneratorsOfInverseSemigroup, 0, 
function(s)
  local gens, f;

  gens:=ShallowCopy(GeneratorsOfInverseSemigroup(s));
  for f in gens do 
    if not IsPermBipartition(f) then 
      f:=f^-1;
      if not f in gens then 
        Add(gens, f);
      fi;
    fi;
  od;
  MakeImmutable(gens);
  return gens;
end);

#

InstallImmediateMethod(GeneratorsOfMonoid,
IsBipartitionMonoid and HasGeneratorsOfInverseMonoid, 0, 
function(s)
  local gens, pos, f;

  gens:=ShallowCopy(GeneratorsOfInverseMonoid(s));
  for f in gens do
    if not IsPermBipartition(f) then  
      f:=f^-1;
      if not f in gens then 
        Add(gens, f);
      fi;
    fi;
  od;
  MakeImmutable(gens);
  return gens;
end);

#

InstallMethod(IsBipartitionSemigroupGreensClass, "for a Green's class",
[IsGreensClass], x-> IsBipartitionSemigroup(Parent(x)));

#

InstallMethod(ViewString, "for a bipartition semigroup with generators",
[IsBipartitionSemigroup and HasGeneratorsOfSemigroup], 8, # to trump IsGroup
function(s)
  local str, nrgens;
  
  str:="\><";

  if HasIsTrivial(s) and IsTrivial(s) then 
    Append(str, "\>trivial\< ");
  else 
    if HasIsCommutative(s) and IsCommutative(s) then 
      Append(str, "\>commutative\< ");
    fi;
  fi;
  if not IsGroup(s) then 
    if (HasIsTrivial(s) and IsTrivial(s)) or IsGroup(s) then 
    elif HasIsZeroSimpleSemigroup(s) and IsZeroSimpleSemigroup(s) then 
      Append(str, "\>0-simple\< ");
    elif HasIsSimpleSemigroup(s) and IsSimpleSemigroup(s) then 
      Append(str, "\>simple\< ");
    fi;

    if HasIsInverseSemigroup(s) and IsInverseSemigroup(s) then 
      Append(str, "\>inverse\< ");
    elif HasIsRegularSemigroup(s) 
     and not (HasIsSimpleSemigroup(s) and IsSimpleSemigroup(s)) then 
      if IsRegularSemigroup(s) then 
        Append(str, "\>regular\< ");
      else
        Append(str, "\>non-regular\< ");
      fi;
    fi;
  fi;
  
  Append(str, "\>bipartition\< ");
  
  if HasIsMonoid(s) and IsMonoid(s) then 
    Append(str, "monoid ");
    if HasIsInverseSemigroup(s) and IsInverseSemigroup(s) then 
      nrgens:=Length(GeneratorsOfInverseMonoid(s));
    else 
      nrgens:=Length(GeneratorsOfMonoid(s));
    fi;
  else 
    Append(str, "semigroup ");
    if HasIsInverseSemigroup(s) and IsInverseSemigroup(s) then 
      nrgens:=Length(GeneratorsOfInverseSemigroup(s));
    else
      nrgens:=Length(GeneratorsOfSemigroup(s));
    fi;
  fi;
  
  if HasIsTrivial(s) and not IsTrivial(s) and HasSize(s) and Size(s)<2^64 then 
    Append(str, "\>of size\> ");
    Append(str, String(Size(s)));
    Append(str, ",\<\< ");
  fi;
 
  Append(str, "\>on \>");
  Append(str, ViewString(DegreeOfBipartitionSemigroup(s)));
  Append(str, "\< pts with\> ");
  Append(str, ViewString(nrgens));
  Append(str, "\< generator");

  if nrgens>1 or nrgens=0 then 
    Append(str, "s\<");
  else 
    Append(str, "\<");
  fi;
  Append(str, ">\<");

  return str;
end);

#

InstallMethod(DegreeOfBipartitionSemigroup, "for a bipartition semigroup",
[IsBipartitionSemigroup], s-> DegreeOfBipartition(Representative(s)));

#

InstallMethod(PrintObj, "for a bipartition semigroup",
[IsBipartitionSemigroup],
function(s)
  Print(Generators(s));
end);

#
