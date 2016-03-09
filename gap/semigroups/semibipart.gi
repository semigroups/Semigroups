#############################################################################
##
#W  semibipart.gi
#Y  Copyright (C) 2013-15                                 James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# this file contains methods for every operation/attribute/property that is
# specific to bipartition semigroups.

#############################################################################
## Random - bipartitions
#############################################################################

InstallMethod(RandomSemigroupCons, "for IsBipartitionSemigroup and list",
[IsBipartitionSemigroup, IsList],
function(filt, params)
  return Semigroup(List([1 .. params[1]], i -> RandomBipartition(params[2])));
end);

InstallMethod(RandomMonoidCons, "for IsBipartitionMonoid and list",
[IsBipartitionMonoid, IsList],
function(filt, params)
  return Monoid(List([1 .. params[1]], i -> RandomBipartition(params[2])));
end);

InstallMethod(RandomInverseSemigroupCons,
"for IsBipartitionSemigroup and list", [IsBipartitionSemigroup, IsList],
SEMIGROUPS.DefaultRandomInverseSemigroup);

InstallMethod(RandomInverseMonoidCons,
"for IsBipartitionMonoid and list", [IsBipartitionMonoid, IsList],
SEMIGROUPS.DefaultRandomInverseMonoid);

#############################################################################
## Random - block bijections
#############################################################################

InstallMethod(RandomSemigroupCons,
"for IsBlockBijectionSemigroup and a list",
[IsBlockBijectionSemigroup, IsList],
function(filt, params)
  return Semigroup(List([1 .. params[1]],
                        i -> RandomBlockBijection(params[2])));
end);

InstallMethod(RandomMonoidCons,
"for IsBlockBijectionMonoid and a list",
[IsBlockBijectionMonoid, IsList],
function(filt, params)
  return Monoid(List([1 .. params[1]],
                     i -> RandomBlockBijection(params[2])));
end);

InstallMethod(RandomInverseSemigroupCons,
"for IsBlockBijectionSemigroup and a list",
[IsBlockBijectionSemigroup, IsList],
function(filt, params)
  return InverseSemigroup(List([1 .. params[1]],
                               i -> RandomBlockBijection(params[2])));
end);

InstallMethod(RandomInverseMonoidCons,
"for IsBlockBijectionMonoid and a list",
[IsBlockBijectionMonoid, IsList],
function(filt, params)
  return InverseMonoid(List([1 .. params[1]],
                            i -> RandomBlockBijection(params[2])));
end);

#############################################################################
## Printing and viewing
#############################################################################

InstallMethod(SemigroupViewStringPrefix, "for a bipartition semigroup",
[IsBipartitionSemigroup], S -> "\>bipartition\< ");

InstallMethod(SemigroupViewStringPrefix, "for a block bijection semigroup",
[IsBlockBijectionSemigroup], S -> "\>block bijection\< ");

InstallMethod(SemigroupViewStringSuffix, "for a bipartition semigroup",
[IsBipartitionSemigroup],
function(S)
  return Concatenation("\>degree \>",
                       ViewString(DegreeOfBipartitionSemigroup(S)),
                       "\<\< ");
end);

# same method for ideals

InstallMethod(GroupOfUnits, "for an acting bipartition semigroup",
[IsBipartitionSemigroup and IsActingSemigroup],
function(S)
  local R, G, deg, U, map;

  if MultiplicativeNeutralElement(S) = fail then
    return fail;
  fi;

  R := GreensRClassOfElementNC(S, MultiplicativeNeutralElement(S));
  G := SchutzenbergerGroup(R);
  deg := DegreeOfBipartitionSemigroup(S);

  U := Semigroup(List(GeneratorsOfGroup(G), x -> AsBipartition(x, deg)));
  SetIsGroupAsSemigroup(U, true);
  UseIsomorphismRelation(U, G);

  map := MagmaIsomorphismByFunctionsNC(U,
                                       G,
                                       AsPermutation,
                                       x -> AsBipartition(x, deg));
  SetIsomorphismPermGroup(U, map);

  return U;
end);

InstallImmediateMethod(IsBlockBijectionSemigroup, IsBipartitionSemigroup and
HasGeneratorsOfSemigroup, 0,
function(S)
  return ForAll(GeneratorsOfSemigroup(S), IsBlockBijection);
end);

InstallImmediateMethod(IsPartialPermBipartitionSemigroup, IsSemigroup and
HasGeneratorsOfSemigroup, 0,
function(S)
  return IsBipartitionSemigroup(S)
         and ForAll(GeneratorsOfSemigroup(S), IsPartialPermBipartition);
end);

InstallImmediateMethod(IsPermBipartitionGroup, IsSemigroup and
HasGeneratorsOfSemigroup, 0,
function(S)
  return IsBipartitionSemigroup(S)
         and ForAll(GeneratorsOfSemigroup(S), IsPermBipartition);
end);

InstallMethod(IsBlockBijectionSemigroup, "for a bipartition semigroup ideal",
[IsBipartitionSemigroup and IsSemigroupIdeal],
function(S)
  if IsBlockBijectionSemigroup(SupersemigroupOfIdeal(S)) then
    return true;
  fi;
  return IsBipartitionSemigroup(S)
         and ForAll(GeneratorsOfSemigroup(S), IsBlockBijection);
end);

InstallMethod(IsPartialPermBipartitionSemigroup, "for a semigroup ideal",
[IsSemigroupIdeal],
function(S)
  if IsPartialPermBipartitionSemigroup(SupersemigroupOfIdeal(S)) then
    return true;
  fi;
  return IsBipartitionSemigroup(S)
         and ForAll(GeneratorsOfSemigroup(S), IsPartialPermBipartition);
end);

InstallMethod(IsPermBipartitionGroup, "for a semigroup ideal",
[IsSemigroupIdeal],
function(S)
  if IsPermBipartitionGroup(SupersemigroupOfIdeal(S)) then
    return true;
  fi;
  return IsBipartitionSemigroup(S)
         and ForAll(GeneratorsOfSemigroup(S), IsPermBipartition);
end);

InstallMethod(NaturalLeqInverseSemigroup, "for a bipartition semigroup",
[IsBipartitionSemigroup],
function(S)
  if IsInverseSemigroup(S) then
    if IsBlockBijectionSemigroup(S) then
      return NaturalLeqBlockBijection;
    elif IsPartialPermBipartitionSemigroup(S) then
      return NaturalLeqPartialPermBipartition;
    fi;
    TryNextMethod(); # this should be the default method for a non-inverse op
                     # semigroup
  fi;
  ErrorNoReturn("Semigroups: NaturalLeqInverseSemigroup: usage,\n",
                "the argument is not an inverse semigroup,");
end);

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

  return out;
end);

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

# The relative order of the methods for the constructor IsomorphismSemigroup is
# important do not change it! They should be ordered from lowest rank to
# highest so that the correct method is used.

InstallMethod(IsomorphismMonoid, "for IsBipartitionMonoid and a semigroup",
[IsBipartitionMonoid, IsSemigroup], SEMIGROUPS.DefaultIsomorphismMonoid);

InstallMethod(IsomorphismMonoid, "for IsBipartitionMonoid and a monoid",
[IsBipartitionMonoid, IsMonoid],
function(filter, S)
  return IsomorphismSemigroup(IsBipartitionSemigroup, S);
end);

# this is just a composition of IsomorphismTransformationSemigroup and the
# method below for IsomorphismBipartitionSemigroup...

InstallMethod(IsomorphismSemigroup,
"for IsBipartitionSemigroup and a semigroup",
[IsBipartitionSemigroup, IsSemigroup], SEMIGROUPS.DefaultIsomorphismSemigroup);

InstallMethod(IsomorphismSemigroup,
"for IsBipartitionSemigroup and a transformation semigroup with generators",
[IsBipartitionSemigroup,
 IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(filter, S)
  local n, T;

  n := Maximum(1, DegreeOfTransformationSemigroup(S));
  T := Semigroup(List(GeneratorsOfSemigroup(S), x -> AsBipartition(x, n)));
  UseIsomorphismRelation(S, T);

  return MagmaIsomorphismByFunctionsNC(S,
                                       T,
                                       x -> AsBipartition(x, n),
                                       AsTransformation);
end);

# the converse of the previous method

InstallMethod(IsomorphismSemigroup,
"for IsBipartitionSemigroup and a partial perm semigroup with generators",
[IsBipartitionSemigroup, IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
function(filter, S)
  local n, T;

  n := Maximum(DegreeOfPartialPermSemigroup(S),
               CodegreeOfPartialPermSemigroup(S));
  T := Semigroup(List(GeneratorsOfSemigroup(S), x -> AsBipartition(x, n)));
  UseIsomorphismRelation(S, T);

  return MagmaIsomorphismByFunctionsNC(S,
                                       T,
                                       x -> AsBipartition(x, n),
                                       AsPartialPerm);
end);

InstallMethod(IsomorphismSemigroup,
"for IsBipartitionSemigroup and partial perm inverse semigp with generators",
[IsBipartitionSemigroup, IsPartialPermSemigroup and IsInverseSemigroup and
 HasGeneratorsOfInverseSemigroup],
function(filter, S)
  local n, T;

  n := Maximum(DegreeOfPartialPermSemigroup(S),
               CodegreeOfPartialPermSemigroup(S));
  T := InverseSemigroup(List(GeneratorsOfInverseSemigroup(S),
                             x -> AsBipartition(x, n)));
  UseIsomorphismRelation(S, T);

  return MagmaIsomorphismByFunctionsNC(S,
                                       T,
                                       x -> AsBipartition(x, n),
                                       AsPartialPerm);
end);

InstallMethod(IsomorphismSemigroup,
"for IsBipartitionSemigroup and a perm group with generators",
[IsBipartitionSemigroup, IsPermGroup and HasGeneratorsOfGroup],
function(filt, S)
  local n, T;

  n := LargestMovedPoint(S);
  T := Semigroup(List(GeneratorsOfGroup(S), x -> AsBipartition(x, n)));
  UseIsomorphismRelation(S, T);

  return MagmaIsomorphismByFunctionsNC(S,
                                       T,
                                       x -> AsBipartition(x, n),
                                       AsPermutation);
end);

# this is one way, i.e. no converse method

InstallMethod(IsomorphismSemigroup,
"for IsBlockBijectionSemigroup and a partial perm semigroup with generators",
[IsBlockBijectionSemigroup,
 IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
function(filter, S)
  local n, T, inv;

  n := Maximum(DegreeOfPartialPermSemigroup(S),
               CodegreeOfPartialPermSemigroup(S)) + 1;
  T := Semigroup(List(GeneratorsOfSemigroup(S), x -> AsBlockBijection(x, n)));
  UseIsomorphismRelation(S, T);

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
                                       T,
                                       x -> AsBlockBijection(x, n),
                                       inv);
end);

# fall back method

InstallMethod(IsomorphismSemigroup,
"for IsBlockBijectionSemigroup and an inverse semigroup",
[IsBlockBijectionSemigroup, IsInverseSemigroup],
function(filter, S)
  local iso1, inv1, iso2, inv2;

  iso1 := IsomorphismPartialPermSemigroup(S);
  inv1 := InverseGeneralMapping(iso1);
  iso2 := IsomorphismSemigroup(filter, Range(iso1));
  inv2 := InverseGeneralMapping(iso2);

  return MagmaIsomorphismByFunctionsNC(S,
                                       Range(iso2),
                                       x -> (x ^ iso1) ^ iso2,
                                       x -> (x ^ inv2) ^ inv1);
end);

# this is one way, i.e. no converse method

InstallMethod(IsomorphismSemigroup,
"for an inverse partial perm semigroup with generators",
[IsBlockBijectionSemigroup, IsPartialPermSemigroup and IsInverseSemigroup and
 HasGeneratorsOfInverseSemigroup],
function(filter, S)
  local n, T, inv;

  n := DegreeOfPartialPermSemigroup(S) + 1;
  T := InverseSemigroup(List(GeneratorsOfInverseSemigroup(S),
                             x -> AsBlockBijection(x, n)));
  UseIsomorphismRelation(S, T);

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
                                       T,
                                       x -> AsBlockBijection(x, n),
                                       inv);
end);

InstallMethod(IsomorphismSemigroup,
"for IsBipartitionSemigroup and a semigroup ideal",
[IsBipartitionSemigroup, IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(filt, I)
  local iso, inv, J;

  iso := IsomorphismBipartitionSemigroup(SupersemigroupOfIdeal(I));
  inv := InverseGeneralMapping(iso);
  J := SemigroupIdeal(Range(iso), Images(iso, GeneratorsOfSemigroupIdeal(I)));
  UseIsomorphismRelation(I, J);

  return MagmaIsomorphismByFunctionsNC(I, J, x -> x ^ iso, x -> x ^ inv);
end);

InstallMethod(IsomorphismSemigroup,
"for IsBlockBijectionSemigroup and a semigroup ideal",
[IsBlockBijectionSemigroup, IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(filt, I)
  local iso, inv, J;

  iso := IsomorphismBlockBijectionSemigroup(SupersemigroupOfIdeal(I));
  inv := InverseGeneralMapping(iso);
  J := SemigroupIdeal(Range(iso), Images(iso, GeneratorsOfSemigroupIdeal(I)));
  UseIsomorphismRelation(I, J);

  return MagmaIsomorphismByFunctionsNC(I, J, x -> x ^ iso, x -> x ^ inv);
end);

InstallMethod(IsomorphismSemigroup,
"for IsBlockBijectionSemigroup and a block bijection semigroup",
[IsBlockBijectionSemigroup, IsBlockBijectionSemigroup],
function(filter, S)
  return MagmaIsomorphismByFunctionsNC(S, S, IdFunc, IdFunc);
end);

InstallMethod(IsomorphismSemigroup,
"for IsBipartitionSemigroup and a block bijection semigroup",
[IsBipartitionSemigroup, IsBipartitionSemigroup],
function(filter, S)
  return MagmaIsomorphismByFunctionsNC(S, S, IdFunc, IdFunc);
end);

# TODO could have a method for IsomorphismBlockBijectionSemigroup for
# IsPartialPermBipartitions too..  or just for general inverse semigroups, via
# composing IsomorphismPartialPermSemigroup and IsomorphismBlockBijection

InstallMethod(IsGeneratorsOfInverseSemigroup, "for a bipartition collection",
[IsBipartitionCollection],
function(coll)
  if IsSemigroup(coll) and HasGeneratorsOfSemigroup(coll) then
    TryNextMethod(); # FIXME why is this necessary?
  fi;

  return ForAll(coll, IsBlockBijection)
   or ForAll(coll, IsPartialPermBipartition);
end);

InstallMethod(GeneratorsOfInverseSemigroup,
"for an inverse bipartition semigroup with generators",
[IsBipartitionSemigroup and IsInverseSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local gens, pos, x;

  gens := ShallowCopy(GeneratorsOfSemigroup(S));
  for x in gens do
    pos := Position(gens, x ^ -1);
    if pos <> fail and x <> x ^ -1 then
      Remove(gens, pos);
    fi;
  od;
  MakeImmutable(gens);
  return gens;
end);

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

InstallMethod(IsBipartitionSemigroupGreensClass, "for a Green's class",
[IsGreensClass], x -> IsBipartitionSemigroup(Parent(x)));

InstallMethod(DegreeOfBipartitionSemigroup, "for a bipartition semigroup",
[IsBipartitionSemigroup], s -> DegreeOfBipartition(Representative(s)));
