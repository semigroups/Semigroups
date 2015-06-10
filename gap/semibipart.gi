#############################################################################
##
#W  semibipart.gi
#Y  Copyright (C) 2013-15                                 James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(AsBlockBijectionSemigroup, "for a semigroup", [IsSemigroup],
function(S)
  return Range(IsomorphismBlockBijectionSemigroup(S));
end);

InstallMethod(AsBipartitionSemigroup, "for a semigroup", [IsSemigroup],
function(S)
  return Range(IsomorphismBipartitionSemigroup(S));
end);

#

InstallMethod(ViewString,
"for a bipartition semigroup ideal with ideal generators",
[IsBipartitionSemigroup and IsSemigroupIdeal and
 HasGeneratorsOfSemigroupIdeal],
function(I)
  local str, nrgens;

  str := "\><";

  if HasIsTrivial(I) and IsTrivial(I) then
    Append(str, "\>trivial\< ");
  else
    if HasIsCommutative(I) and IsCommutative(I) then
      Append(str, "\>commutative\< ");
    fi;
  fi;
  if not IsGroup(I) then
    if (HasIsTrivial(I) and IsTrivial(I)) or IsGroup(I) then
    elif HasIsZeroSimpleSemigroup(I) and IsZeroSimpleSemigroup(I) then
      Append(str, "\>0-simple\< ");
    elif HasIsSimpleSemigroup(I) and IsSimpleSemigroup(I) then
      Append(str, "\>simple\< ");
    fi;

    if HasIsInverseSemigroup(I) and IsInverseSemigroup(I) then
      Append(str, "\>inverse\< ");
    elif HasIsRegularSemigroup(I)
        and not (HasIsSimpleSemigroup(I) and IsSimpleSemigroup(I)) then
      if IsRegularSemigroup(I) then
        Append(str, "\>regular\< ");
      else
        Append(str, "\>non-regular\< ");
      fi;
    fi;
  fi;

  Append(str, "\>bipartition\< \>semigroup\< \>ideal\< ");
  Append(str, "\>on \>");
  Append(str, ViewString(DegreeOfBipartitionSemigroup(I)));
  Append(str, "\< pts with\> ");

  nrgens := Length(GeneratorsOfSemigroupIdeal(I));
  Append(str, ViewString(nrgens));
  Append(str, "\< generator");

  if nrgens > 1 or nrgens = 0 then
    Append(str, "s\<");
  else
    Append(str, "\<");
  fi;
  Append(str, ">\<");

  return str;
end);

#

InstallImmediateMethod(IsBlockBijectionSemigroup, IsSemigroup and
HasGeneratorsOfSemigroup, 0,
function(S)
  return IsBipartitionSemigroup(S)
         and ForAll(GeneratorsOfSemigroup(S), IsBlockBijection);
end);

#

InstallImmediateMethod(IsPartialPermBipartitionSemigroup, IsSemigroup and
HasGeneratorsOfSemigroup, 0,
function(S)
  return IsBipartitionSemigroup(S)
         and ForAll(GeneratorsOfSemigroup(S), IsPartialPermBipartition);
end);

#

InstallImmediateMethod(IsPermBipartitionGroup, IsSemigroup and
HasGeneratorsOfSemigroup, 0,
function(S)
  return IsBipartitionSemigroup(S)
         and ForAll(GeneratorsOfSemigroup(S), IsPermBipartition);
end);

#

InstallMethod(IsBlockBijectionSemigroup, "for a semigroup ideal",
[IsSemigroupIdeal],
function(S)
  if IsBlockBijectionSemigroup(SupersemigroupOfIdeal(S)) then
    return true;
  fi;
  return IsBipartitionSemigroup(S)
         and ForAll(GeneratorsOfSemigroup(S), IsBlockBijection);
end);

#

InstallMethod(IsPartialPermBipartitionSemigroup, "for a semigroup ideal",
[IsSemigroupIdeal],
function(S)
  if IsPartialPermBipartitionSemigroup(SupersemigroupOfIdeal(S)) then
    return true;
  fi;
  return IsBipartitionSemigroup(S)
         and ForAll(GeneratorsOfSemigroup(S), IsPartialPermBipartition);
end);

#

InstallMethod(IsPermBipartitionGroup, "for a semigroup ideal",
[IsSemigroupIdeal],
function(S)
  if IsPermBipartitionGroup(SupersemigroupOfIdeal(S)) then
    return true;
  fi;
  return IsBipartitionSemigroup(S)
         and ForAll(GeneratorsOfSemigroup(S), IsPermBipartition);
end);

#

InstallMethod(NaturalPartialOrder,
"for an inverse block bijection semigroup",
[IsBlockBijectionSemigroup and IsInverseSemigroup],
function(S)
  local elts, n, out, i, j;

  elts := Elements(S);
  n := Length(elts);
  out := List([1 .. n], x -> []);

  for i in [n, n - 1 .. 2] do
    for j in [i - 1, i - 2 .. 1] do
      if NaturalLeqBlockBijection(elts[j], elts[i]) then
        AddSet(out[i], j);
      fi;
    od;
  od;

  Perform(out, ShrinkAllocationPlist);
  return out;
end);

#

InstallMethod(NaturalPartialOrder,
"for an inverse partial perm bipartition semigroup",
[IsPartialPermBipartitionSemigroup and IsInverseSemigroup],
function(S)
  local elts, p, n, out, i, j;

  elts := ShallowCopy(Elements(S));
  n := Length(elts);
  out := List([1 .. n], x -> []);
  p := Sortex(elts, PartialPermLeqBipartition) ^ -1;

  for i in [n, n - 1 .. 2] do
    for j in [i - 1, i - 2 .. 1] do
      if NaturalLeqPartialPermBipartition(elts[j], elts[i]) then
        AddSet(out[i ^ p], j ^ p);
      fi;
    od;
  od;

  Perform(out, ShrinkAllocationPlist);
  return out;
end);

# this is just a composition of IsomorphismTransformationSemigroup and the
# method below for IsomorphismBipartitionSemigroup...

InstallMethod(IsomorphismBipartitionSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  local en, act, gens;

  en := EnumeratorSorted(S);

  act := function(i, x)
    if i <= Length(en) then
      return Position(en, en[i] * x);
    fi;
    return Position(en, x);
  end;

  gens := List(en,
               x -> AsBipartition(TransformationOp(x,
                                                   [1 .. Length(en) + 1],
                                                   act),
                                  Length(en) + 1));
  # gaplint: ignore 4
  return MagmaIsomorphismByFunctionsNC(S, Semigroup(gens),
   x -> AsBipartition(TransformationOp(x, [1 .. Length(en) + 1], act),
                      Length(en) + 1),
   x -> en[(Length(en) + 1) ^ AsTransformation(x)]);
end);

#

InstallMethod(IsomorphismBipartitionSemigroup,
"for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local n, source, range, i;

  n := DegreeOfTransformationSemigroup(S);
  source := GeneratorsOfSemigroup(S);
  range := EmptyPlist(Length(source));

  for i in [1 .. Length(source)] do
    range[i] := AsBipartition(source[i], n);
  od;
  # gaplint: ignore 2
  return MagmaIsomorphismByFunctionsNC(S, Semigroup(range),
           x -> AsBipartition(x, n), AsTransformation);
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

  n := DegreeOfBipartitionSemigroup(S);
  source := GeneratorsOfSemigroup(S);
  range := EmptyPlist(Length(source));

  for i in [1 .. Length(source)] do
    range[i] := AsTransformation(source[i]);
  od;

  # gaplint: ignore 2
  return MagmaIsomorphismByFunctionsNC(S, Semigroup(range),
           AsTransformation, x -> AsBipartition(x, n));
end);

#

InstallMethod(IsomorphismBipartitionSemigroup,
"for a partial perm semigroup with generators",
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local n, source, range, i;

  n := Maximum(DegreeOfPartialPermSemigroup(S),
               CodegreeOfPartialPermSemigroup(S));
  source := GeneratorsOfSemigroup(S);
  range := EmptyPlist(Length(source));

  for i in [1 .. Length(source)] do
    range[i] := AsBipartition(source[i], n);
  od;

  # gaplint: ignore 2
  return MagmaIsomorphismByFunctionsNC(S, Semigroup(range),
   x -> AsBipartition(x, n), AsPartialPerm);
end);

# the converse of the previous method

InstallMethod(IsomorphismPartialPermSemigroup,
"for a bipartition semigroup with generators",
[IsBipartitionSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local n, source, range, i;

  if not ForAll(GeneratorsOfSemigroup(S), IsPartialPermBipartition) then
    TryNextMethod();
  fi;

  n := DegreeOfBipartitionSemigroup(S);
  source := GeneratorsOfSemigroup(S);
  range := EmptyPlist(Length(source));

  for i in [1 .. Length(source)] do
    range[i] := AsPartialPerm(source[i]);
  od;

  # gaplint: ignore 2
  return MagmaIsomorphismByFunctionsNC(S, Semigroup(range),
           AsPartialPerm, x -> AsBipartition(x, n));
end);

#

InstallMethod(IsomorphismBipartitionSemigroup,
"for a partial perm inverse semigroup with generators",
[IsPartialPermSemigroup and IsInverseSemigroup and
 HasGeneratorsOfInverseSemigroup],
function(S)
  local n, source, range, i;

  n := Maximum(DegreeOfPartialPermSemigroup(S),
               CodegreeOfPartialPermSemigroup(S));
  source := GeneratorsOfInverseSemigroup(S);
  range := EmptyPlist(Length(source));

  for i in [1 .. Length(source)] do
    range[i] := AsBipartition(source[i], n);
  od;

  return MagmaIsomorphismByFunctionsNC(S,
                                       InverseSemigroup(range),
                                       x -> AsBipartition(x, n),
                                       AsPartialPerm);
end);

# the converse of the last method

InstallMethod(IsomorphismPartialPermSemigroup,
"for a bipartition inverse semigroup with generators",
[IsBipartitionSemigroup and IsInverseSemigroup and
 HasGeneratorsOfInverseSemigroup],
function(S)
  local n, source, range, i;

  if not ForAll(GeneratorsOfInverseSemigroup(S), IsPartialPermBipartition) then
    TryNextMethod();
  fi;

  n := DegreeOfBipartitionSemigroup(S);
  source := GeneratorsOfSemigroup(S);
  range := EmptyPlist(Length(source));

  for i in [1 .. Length(source)] do
    range[i] := AsPartialPerm(source[i]);
  od;

  return MagmaIsomorphismByFunctionsNC(S,
                                       InverseSemigroup(range),
                                       AsPartialPerm,
                                       x -> AsBipartition(x, n));
end);

#

InstallMethod(IsomorphismBipartitionSemigroup,
"for a perm group with generators",
[IsPermGroup and HasGeneratorsOfGroup],
function(S)
  local n, source, range, i;

  n := LargestMovedPoint(S);
  source := GeneratorsOfGroup(S);
  range := EmptyPlist(Length(source));

  for i in [1 .. Length(source)] do
    range[i] := AsBipartition(source[i], n);
  od;

  return MagmaIsomorphismByFunctionsNC(S,
                                       Semigroup(range),
                                       x -> AsBipartition(x, n),
                                       AsPermutation);
end);

# the converse of the previous method

InstallMethod(IsomorphismPermGroup,
"for a perm bipartition group with generators",
[IsPermBipartitionGroup and HasGeneratorsOfSemigroup],
1, # to beat the method for IsBlockBijectionSemigroup
function(S)
  local n, source, range, i;

  n := DegreeOfBipartitionSemigroup(S);
  source := GeneratorsOfSemigroup(S);
  range := EmptyPlist(Length(source));

  for i in [1 .. Length(source)] do
    range[i] := AsPermutation(source[i]);
  od;

  return MagmaIsomorphismByFunctionsNC(S,
                                       Semigroup(range),
                                       AsPermutation,
                                       x -> AsBipartition(x, n));
end);

InstallMethod(IsomorphismPermGroup,
"for a block bijection semigroup with generators",
[IsBlockBijectionSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local iso, inv;

  if not IsGroupAsSemigroup(S) then
    return fail;
  fi;

  iso := IsomorphismPermGroup(GroupHClass(DClass(S, Representative(S))));
  inv := InverseGeneralMapping(iso);

  return MagmaIsomorphismByFunctionsNC(S,
                                       Range(iso),
                                       x -> x ^ iso,
                                       x -> x ^ inv);
end);

# this is one way, i.e. no converse method

InstallMethod(IsomorphismBlockBijectionSemigroup,
"for an inverse partial perm semigroup with generators",
[IsPartialPermSemigroup and IsInverseSemigroup and
 HasGeneratorsOfInverseSemigroup],
function(S)
  local n, source, range, i, inv;

  n := DegreeOfPartialPermSemigroup(S) + 1;
  source := GeneratorsOfInverseSemigroup(S);
  range := EmptyPlist(Length(source));

  for i in [1 .. Length(source)] do
    range[i] := AsBlockBijection(source[i], n);
  od;

  # AsPartialPerm for a block bijection created using AsBlockBijection with
  # argument a partial perm
  inv := function(x)
    local blocks, n, bigblock, lookup, out, i;

    blocks := x!.blocks;
    n := DegreeOfBipartition(x);
    bigblock := blocks[n];

    # find the images of [1..n]
    lookup := EmptyPlist(n - 1);
    for i in [1 .. n - 1] do
      lookup[blocks[i + n]] := i;
    od;

    # put it together
    out := [1 .. n - 1] * 0;
    for i in [1 .. n - 1] do
      if blocks[i] <> bigblock then
        out[i] := lookup[blocks[i]];
      fi;
    od;

    return PartialPerm(out);
  end;

  return MagmaIsomorphismByFunctionsNC(S,
                                       InverseSemigroup(range), x ->
                                       AsBlockBijection(x, n),
                                       inv);
end);

# this is one way, i.e. no converse method

InstallMethod(IsomorphismBlockBijectionSemigroup,
"for a partial perm semigroup with generators",
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local n, source, range, i, inv;

  n := Maximum(DegreeOfPartialPermSemigroup(S),
               CodegreeOfPartialPermSemigroup(S)) + 1;
  source := GeneratorsOfSemigroup(S);
  range := EmptyPlist(Length(source));

  for i in [1 .. Length(source)] do
    range[i] := AsBlockBijection(source[i], n);
  od;

  # AsPartialPerm for a block bijection created using AsBlockBijection with
  # argument a partial perm
  inv := function(x)
    local blocks, n, bigblock, lookup, out, i;

    blocks := x!.blocks;
    n := DegreeOfBipartition(x);
    bigblock := blocks[n];

    # find the images of [1..n]
    lookup := EmptyPlist(n - 1);
    for i in [1 .. n - 1] do
      lookup[blocks[i + n]] := i;
    od;

    # put it together
    out := [1 .. n - 1] * 0;
    for i in [1 .. n - 1] do
      if blocks[i] <> bigblock then
        out[i] := lookup[blocks[i]];
      fi;
    od;

    return PartialPerm(out);
  end;

  return MagmaIsomorphismByFunctionsNC(S,
                                       Semigroup(range),
                                       x -> AsBlockBijection(x, n),
                                       inv);
end);

# JDM could have a method for
# IsomorphismBlockBijectionSemigroup for IsPartialPermBipartitions too..
# or just for general inverse semigroups, via composing
# IsomorphismPartialPermSemigroup and IsomorphismBlockBijection

InstallMethod(IsGeneratorsOfInverseSemigroup, "for a bipartition collection",
[IsBipartitionCollection],
function(coll)
  return ForAll(coll, IsBlockBijection)
   or ForAll(coll, IsPartialPermBipartition);
end);

#

InstallMethod(GeneratorsOfInverseSemigroup,
"for an inverse bipartition semigroup with generators",
[IsBipartitionSemigroup and IsInverseSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, pos, f;

  gens := ShallowCopy(GeneratorsOfSemigroup(s));
  for f in gens do
    pos := Position(gens, f ^ -1);
    if pos <> fail and f <> f ^ -1 then
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

  gens := ShallowCopy(GeneratorsOfMonoid(s));
  one := One(s);
  for f in gens do
    pos := Position(gens, f ^ -1);
    if pos <> fail and (f <> f ^ -1 or f = one) then
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

  gens := ShallowCopy(GeneratorsOfInverseSemigroup(s));
  for f in gens do
    if not IsPermBipartition(f) then
      f := f ^ -1;
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
  local gens, f;

  gens := ShallowCopy(GeneratorsOfInverseMonoid(s));
  for f in gens do
    if not IsPermBipartition(f) then
      f := f ^ -1;
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
[IsGreensClass], x -> IsBipartitionSemigroup(Parent(x)));

#

InstallMethod(ViewString, "for a bipartition semigroup with generators",
[IsBipartitionSemigroup and HasGeneratorsOfSemigroup], 8, # to trump IsGroup
function(s)
  local str, nrgens;

  str := "\><";

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
      nrgens := Length(GeneratorsOfInverseMonoid(s));
    else
      nrgens := Length(GeneratorsOfMonoid(s));
    fi;
  else
    Append(str, "semigroup ");
    if HasIsInverseSemigroup(s) and IsInverseSemigroup(s) then
      nrgens := Length(GeneratorsOfInverseSemigroup(s));
    else
      nrgens := Length(GeneratorsOfSemigroup(s));
    fi;
  fi;

  if HasIsTrivial(s) and not IsTrivial(s) and HasSize(s)
      and Size(s) < 2 ^ 64 then
    Append(str, "\>of size\> ");
    Append(str, String(Size(s)));
    Append(str, ",\<\< ");
  fi;

  Append(str, "\>on \>");
  Append(str, ViewString(DegreeOfBipartitionSemigroup(s)));
  Append(str, "\< pts with\> ");
  Append(str, ViewString(nrgens));
  Append(str, "\< generator");

  if nrgens > 1 or nrgens = 0 then
    Append(str, "s\<");
  else
    Append(str, "\<");
  fi;
  Append(str, ">\<");

  return str;
end);

#

InstallMethod(DegreeOfBipartitionSemigroup, "for a bipartition semigroup",
[IsBipartitionSemigroup], s -> DegreeOfBipartition(Representative(s)));

#

InstallMethod(ZeroSemigroupCons,
"for a filter and a positive integer",
[IsBipartitionSemigroup and IsFinite, IsPosInt],
function(filter, n)
  local zero, out;

  if n = 2 then
    zero := Bipartition([[1], [2], [-1], [-2]]);
    out := Semigroup(Bipartition([[1, -2], [2], [-1]]));
    SetMultiplicativeZero(out, zero);
    return out;
  fi;
  return ZeroSemigroupCons(IsBlockBijectionSemigroup, n);
end);

#

InstallMethod(ZeroSemigroupCons,
"for a filter and a positive integer",
[IsBlockBijectionSemigroup and IsFinite, IsPosInt],
function(filter, n)
  local zero, gens, points, pair, out, i;

  if n = 1 then
    zero := Bipartition([[1, -1]]);
    gens := [zero];
  elif n = 2 then
    points := Concatenation([1 .. 3], [-3 .. -1]);
    zero := Bipartition([points]);
    gens := [Bipartition([[1, -2], [-1, 2, 3, -3]])];
  else
    points := Concatenation([1 .. 2 * (n - 1)], -[1 .. 2 * (n - 1)]);
    zero := Bipartition([points]);
    gens := EmptyPlist(n - 1);
    for i in [1 .. n - 1] do
      pair := [2 * i - 1, -(2 * i)];
      gens[i] := Bipartition([pair, Difference(points, pair)]);
    od;
  fi;
  out := Semigroup(gens);
  SetMultiplicativeZero(out, zero);
  return out;
end);
