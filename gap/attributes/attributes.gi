#############################################################################
##
#W  attributes.gi
#Y  Copyright (C) 2013-16                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for finding various attributes of finite
# semigroups, where no better method is known.

# Note about the difference between One and MultiplicativeNeutralElement
# (the same goes for Zero and MultplicativeZero):
#
# One(s) returns One(Representative(s)) if it belongs to s, so that
# One(s) = Transformation([1 .. DegreeOfTransformationSemigroup(s)]) if s is a
# transformation semigroup and it returns fail otherwise, or it returns
# PartialPerm([1 .. DegreeOfPartialPermSemigroup]) if this belongs to s.
#
# MultiplicativeNeutralElement on the other hand returns the element of s that
# acts as the identity, note that this can be equal to One(s) but it can also
# not be equal to One(s).
#
# A semigroup satisfies IsMonoidAsSemigroup(s) if
# MultiplicativeNeutralElement(x) <> fail, so it could be that One(s) returns
# fail but IsMonoidAsSemigroup is still true.

SEMIGROUPS.InjectionPrincipalFactor := function(D, constructor)
  local map, inv, G, mat, rep, R, L, x, RR, LL, rms, iso, hom, i, j;

  map := IsomorphismPermGroup(GroupHClass(D));
  inv := InverseGeneralMapping(map);

  G := Range(map);
  mat := [];

  rep := Representative(GroupHClass(D));
  R := HClassReps(LClass(D, rep));
  L := HClassReps(RClass(D, rep));

  for i in [1 .. Length(L)] do
    mat[i] := [];
    for j in [1 .. Length(R)] do
      x := L[i] * R[j];
      if x in D then
        mat[i][j] := x ^ map;
      else
        mat[i][j] := 0;
      fi;
    od;
  od;

  RR := EmptyPlist(Length(R));
  LL := EmptyPlist(Length(L));

  for j in [1 .. Length(R)] do
    for i in [1 .. Length(L)] do
      if mat[i][j] <> 0 then
        RR[j] := ((mat[i][j] ^ -1) ^ inv) * L[i];
        break;
      fi;
    od;
  od;

  for i in [1 .. Length(L)] do
    for j in [1 .. Length(R)] do
      if mat[i][j] <> 0 then
        LL[i] := R[j] * (mat[i][j] ^ -1) ^ inv;
        break;
      fi;
    od;
  od;

  rms := constructor(G, mat);

  iso := function(x)
    i := PositionProperty(R, y -> y in RClass(D, x));
    j := PositionProperty(L, y -> y in LClass(D, x));

    if i = fail or j = fail then
      return fail;
    fi;
    return Objectify(TypeReesMatrixSemigroupElements(rms),
                     [i, (rep * RR[i] * x * LL[j]) ^ map, j, mat]);
  end;

  hom := MappingByFunction(D, rms, iso,
                           function(x)
                             if x![1] = 0 then
                               return fail;
                             fi;
                             return R[x![1]] * (x![2] ^ inv) * L[x![3]];
                           end);
  SetIsInjective(hom, true);
  SetIsTotal(hom, true);
  return hom;
end;

#############################################################################
## 1. Default methods, for which there are currently no better methods.
#############################################################################

InstallMethod(GeneratorsSmallest, "for a semigroup",
[IsSemigroup],
function(S)
  local iter, T, x;

  iter := IteratorSorted(S);
  T := Semigroup(NextIterator(iter));

  for x in iter do
    if not x in T then
      T := SEMIGROUPS.AddGenerators(T, [x], SEMIGROUPS.OptionsRec(T));
      if T = S then
        break;
      fi;
    fi;
  od;

  return GeneratorsOfSemigroup(T);
end);

InstallMethod(SmallestElementSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  return NextIterator(IteratorSorted(S));
end);

InstallMethod(LargestElementSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  return EnumeratorSorted(S)[Size(S)];
end);

InstallMethod(NrIdempotents, "for a semigroup",
[IsSemigroup],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;

  if SEMIGROUPS.IsCCSemigroup(S) then
    return SEMIGROUP_NR_IDEMPOTENTS(GenericSemigroupData(S));
  fi;
  return Length(Idempotents(S));
end);

InstallMethod(GroupOfUnits, "for a semigroup",
[IsSemigroup],
function(S)
  local H, map, U, iso;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  if MultiplicativeNeutralElement(S) = fail then
    return fail;
  fi;

  H := GreensHClassOfElementNC(S, MultiplicativeNeutralElement(S));
  map := IsomorphismPermGroup(H);

  U := Semigroup(List(GeneratorsOfGroup(Range(map)),
                      x -> x ^ InverseGeneralMapping(map)));

  iso := MappingByFunction(U,
                           Range(map),
                           x -> x ^ map,
                           x -> x ^ InverseGeneralMapping(map));
  SetIsomorphismPermGroup(U, iso);
  SetIsGroupAsSemigroup(U, true);
  UseIsomorphismRelation(U, Range(iso));
  return U;
end);

# same method for ideals

InstallMethod(RightCayleyGraphSemigroup, "for a semigroup",
[IsSemigroup], 3,
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return SEMIGROUP_RIGHT_CAYLEY_GRAPH(GenericSemigroupData(S));
end);

# same method for ideals

InstallMethod(LeftCayleyGraphSemigroup, "for a semigroup",
[IsSemigroup], 3,
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return SEMIGROUP_LEFT_CAYLEY_GRAPH(GenericSemigroupData(S));
end);

# same method for ideals

InstallMethod(IsomorphismReesMatrixSemigroup, "for a D-class",
[IsGreensDClass],
function(D)
  if NrIdempotents(D) <> NrHClasses(D) then
    ErrorNoReturn("Semigroups: IsomorphismReesMatrixSemigroup: usage,\n",
                  "the D-class is not a subsemigroup,");
  fi;

  return InjectionPrincipalFactor(D);
end);

# same method for ideal

InstallMethod(IrredundantGeneratingSubset,
"for a multiplicative element collection",
[IsMultiplicativeElementCollection],
function(coll)
  local gens, nrgens, deg, out, redund, i, x;

  if (IsSemigroup(coll) and HasGeneratorsOfSemigroup(coll))
      or (HasIsSemigroupIdeal(coll) and IsSemigroupIdeal(coll)) then
    coll := ShallowCopy(GeneratorsOfSemigroup(coll));
  elif not IsMutable(coll) then
    coll := ShallowCopy(coll);
  fi;

  if Size(coll) = 1 then
    return coll;
  fi;

  gens := Set(coll);
  nrgens := Length(gens);

  if IsGeneratorsOfActingSemigroup(coll) then
    deg := ActionDegree(coll);
    Shuffle(coll);
    Sort(coll, function(x, y)
                 return ActionRank(x, deg) > ActionRank(y, deg);
               end);
  fi;

  out := EmptyPlist(Length(coll));
  redund := EmptyPlist(Length(coll));
  i := 0;

  repeat
    i := i + 1;
    x := coll[i];
    if InfoLevel(InfoSemigroups) >= 3 then
      Print("at \t", i, " of \t", Length(coll), " with \t", Length(redund),
            " redundant, \t", Length(out), " non-redundant\n");
    fi;

    if not x in redund and not x in out then
      if x in Semigroup(Difference(gens, [x])) then
        AddSet(redund, x);
        gens := Difference(gens, [x]);
      else
        AddSet(out, x);
      fi;
    fi;
  until Length(redund) + Length(out) = nrgens;

  if InfoLevel(InfoSemigroups) >= 3 then
    Print("\n");
  fi;

  return out;
end);

# same method for ideals

InstallMethod(MinimalIdeal, "for a semigroup",
[IsSemigroup],
function(S)
  local I;
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  I := SemigroupIdealByGeneratorsNC(S, [RepresentativeOfMinimalIdeal(S)],
                                    SEMIGROUPS.OptionsRec(S));
  SetIsSimpleSemigroup(I, true);
  return I;
end);

#

InstallMethod(PrincipalFactor, "for a Green's D-class",
[IsGreensDClass], D -> Range(InjectionPrincipalFactor(D)));

#

InstallMethod(NormalizedPrincipalFactor, "for a Green's D-class",
[IsGreensDClass], D -> Range(InjectionNormalizedPrincipalFactor(D)));

# different method for ideals, not yet implemented

InstallMethod(SmallSemigroupGeneratingSet,
"for a multiplicative element collection",
[IsMultiplicativeElementCollection],
function(coll)
  if Length(coll) < 2 then
    return coll;
  fi;
  return GeneratorsOfSemigroup(Semigroup(coll, rec(small := true)));
end);

# different method for ideals, not yet implemented

InstallMethod(SmallSemigroupGeneratingSet,
"for a finite semigroup", [IsSemigroup and IsFinite],
S -> SmallSemigroupGeneratingSet(GeneratorsOfSemigroup(S)));

#

InstallMethod(SmallMonoidGeneratingSet,
"for an multiplicative element with one collection",
[IsMultiplicativeElementWithOneCollection],
function(coll)
  if Length(coll) = 1 then
    if coll[1] = One(coll) then
      return [];
    fi;
    return coll;
  fi;
  return GeneratorsOfMonoid(Monoid(coll, rec(small := true)));
end);

# same method for ideals

InstallMethod(SmallMonoidGeneratingSet, "for a finite monoid",
[IsFinite and IsMonoid],
function(S)
  if IsEmpty(GeneratorsOfMonoid(S)) then
    return [];
  fi;
  return SmallMonoidGeneratingSet(GeneratorsOfMonoid(S));
end);

#

InstallMethod(SmallInverseSemigroupGeneratingSet,
"for a multiplicative element coll",
[IsMultiplicativeElementCollection],
function(coll)
  if not IsGeneratorsOfInverseSemigroup(coll) then
    ErrorNoReturn("Semigroups: SmallInverseSemigroupGeneratingSet: usage,\n",
                  "the argument must satisfy IsGeneratorsOfInverseSemigroup");
  fi;
  if Length(coll) < 2 then
    return coll;
  fi;
  return GeneratorsOfInverseSemigroup(InverseSemigroup(coll,
                                                       rec(small := true)));
end);

#

InstallMethod(SmallInverseSemigroupGeneratingSet,
"for a semigroup with inverse op", [IsSemigroupWithInverseOp],
S -> SmallSemigroupGeneratingSet(GeneratorsOfInverseSemigroup(S)));

#

InstallMethod(SmallInverseMonoidGeneratingSet,
"for generators of an inverse monoid",
[IsMultiplicativeElementWithOneCollection],
function(coll)
  if not IsGeneratorsOfInverseSemigroup(coll) then
    ErrorNoReturn("Semigroups: SmallInverseMonoidGeneratingSet: usage,\n",
                  "the argument must satisfy IsGeneratorsOfInverseSemigroup");
  fi;
  if Length(coll) = 1 then
    if coll[1] = One(coll) then
      return [];
    fi;
    return coll;
  fi;
  return GeneratorsOfInverseMonoid(InverseMonoid(coll, rec(small := true)));
end);

#

InstallMethod(SmallInverseMonoidGeneratingSet,
"for a monoid with inverse op",
[IsSemigroupWithInverseOp and IsMonoid],
function(S)
  if IsEmpty(GeneratorsOfInverseMonoid(S)) then
    return GeneratorsOfInverseMonoid(S);
  fi;
  return SmallMonoidGeneratingSet(GeneratorsOfInverseMonoid(S));
end);

#

InstallMethod(SmallGeneratingSet, "for a semigroup",
[IsSemigroup],
function(S)

  if HasGeneratorsOfSemigroupIdeal(S) then
    return MinimalIdealGeneratingSet(S);
  elif HasGeneratorsOfInverseMonoid(S) then
    return SmallInverseMonoidGeneratingSet(S);
  elif HasGeneratorsOfInverseSemigroup(S) then
    return SmallInverseSemigroupGeneratingSet(S);
  elif HasGeneratorsOfMonoid(S) then
    return SmallMonoidGeneratingSet(S);
  fi;

  return SmallSemigroupGeneratingSet(S);
end);

#

InstallMethod(StructureDescription, "for a Brandt semigroup",
[IsBrandtSemigroup],
function(S)
  local D;

  D := MaximalDClasses(S)[1];
  return Concatenation("B(", StructureDescription(GroupHClass(D)), ", ",
                       String(NrRClasses(D)), ")");
end);

# same method for ideals

# note that IsGroup and IsGroupAsSemigroup are mutually exclusive
# so the following method is not called for PermGroups etc

InstallMethod(StructureDescription, "for a group as semigroup",
[IsGroupAsSemigroup],
function(S)
  if IsGroup(S) then
    TryNextMethod();
  fi;
  return StructureDescription(Range(IsomorphismPermGroup(S)));
end);

# same method for ideals

InstallMethod(MultiplicativeZero, "for a semigroup",
[IsSemigroup],
function(S)
  local D, rep, gens;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  if IsSemigroupIdeal(S)
      and HasMultiplicativeZero(SupersemigroupOfIdeal(S)) then
    return MultiplicativeZero(SupersemigroupOfIdeal(S));
  fi;

  if HasMinimalDClass(S) then
    D := MinimalDClass(S);
    if HasSize(D) then
      if IsTrivial(D) then
        return Representative(D);
      fi;
      return fail;
    fi;
  fi;

  if IsSemigroupIdeal(S) then
    return MultiplicativeZero(SupersemigroupOfIdeal(S));
  fi;

  rep := RepresentativeOfMinimalIdeal(S);
  gens := GeneratorsOfSemigroup(S);

  if ForAll(gens, x -> x * rep = rep and rep * x = rep) then
    return rep;
  fi;

  return fail;
end);

InstallMethod(MultiplicativeZero, "for a free semigroup",
[IsFreeSemigroup], ReturnFail);

InstallMethod(LengthOfLongestDClassChain, "for a finite semigroup",
[IsSemigroup],
function(S)
  local gr, nbs, po, minimal_dclass;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  gr := DigraphRemoveLoops(Digraph(PartialOrderOfDClasses(S)));
  nbs := OutNeighbours(gr);
  po := Digraph(InNeighbours(gr));
  minimal_dclass := First(DigraphVertices(po), x -> IsEmpty(nbs[x]));

  SetMinimalDClass(S, GreensDClasses(S)[minimal_dclass]);
  SetRepresentativeOfMinimalIdeal(S, Representative(
                                     GreensDClasses(S)[minimal_dclass]));

  return DigraphLongestDistanceFromVertex(po, minimal_dclass);
end);

InstallMethod(NilpotencyDegree, "for a finite semigroup",
[IsSemigroup and IsFinite],
function(S)
  if not IsNilpotentSemigroup(S) then
    return fail;
  fi;
  return LengthOfLongestDClassChain(S) + 1;
end);

InstallMethod(MinimalDClass, "for a semigroup", [IsSemigroup],
S -> GreensDClassOfElementNC(S, RepresentativeOfMinimalIdeal(S)));

#############################################################################
## 2. Methods for attributes where there are known better methods for acting
##    semigroups.
#############################################################################

InstallMethod(IsGreensDLeq, "for a finite semigroup",
[IsSemigroup],
function(S)
  local digraph, data, id;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  digraph := Digraph(PartialOrderOfDClasses(S));
  digraph := DigraphReflexiveTransitiveClosure(digraph);
  data := GenericSemigroupData(S);
  id := GreensDRelation(S)!.data.id;
  return function(x, y)
    local i, j;
    i := id[Position(data, x)];
    j := id[Position(data, y)];
    # TODO should be a better way of checking the below
    return j in OutNeighboursOfVertex(digraph, i);
  end;
end);

#

InstallMethod(MaximalDClasses, "for a semigroup",
[IsSemigroup],
function(S)
  local gens, partial, data, id, pos, i, out, classes, x;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  gens    := GeneratorsOfSemigroup(S);
  partial := PartialOrderOfDClasses(S);
  data    := GenericSemigroupData(S);
  id      := GreensDRelation(S)!.data.id;
  pos     := [];

  for x in gens do
    i := id[Position(data, x)];
    #index of the D-class containing x
    AddSet(pos, i);
  od;

  out := [];
  classes := GreensDClasses(S);
  for i in pos do
    if not ForAny([1 .. Length(partial)], j -> j <> i and i in partial[j]) then
      Add(out, classes[i]);
    fi;
  od;

  return out;
end);

# same method for ideals

InstallMethod(StructureDescriptionMaximalSubgroups,
"for a semigroup", [IsSemigroup],
function(S)
  local out, D;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  out := [];
  for D in RegularDClasses(S) do
    AddSet(out, StructureDescription(GroupHClass(D)));
  od;

  return out;
end);

#

InstallMethod(IdempotentGeneratedSubsemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return Semigroup(Idempotents(S), rec(small := true));
end);

InstallMethod(IdempotentGeneratedSubsemigroup,
"for an inverse op semigroup",
[IsSemigroupWithInverseOp],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return InverseSemigroup(Idempotents(S), rec(small := true));
end);

InstallMethod(InjectionPrincipalFactor, "for a Green's D-class (Semigroups)",
[IsGreensDClass],
function(D)
  if not IsRegularDClass(D) then
    ErrorNoReturn("Semigroups: InjectionPrincipalFactor: usage,\n",
                  "the argument <D> must be a regular D-class,");
  fi;
  if NrHClasses(D) = NrIdempotents(D) then
    return SEMIGROUPS.InjectionPrincipalFactor(D, ReesMatrixSemigroup);
  fi;
  return SEMIGROUPS.InjectionPrincipalFactor(D, ReesZeroMatrixSemigroup);
end);

InstallMethod(InjectionNormalizedPrincipalFactor,
"for a Green's D-class (Semigroups)",
[IsGreensDClass],
function(D)
  local iso1, iso2, rms, inv1, inv2, iso, inv, hom;

  if not IsRegularDClass(D) then
    ErrorNoReturn("Semigroups: InjectionNormalizedPrincipalFactor: usage,\n",
                  "the argument <D> must be a regular D-class,");
  fi;
  if NrHClasses(D) = NrIdempotents(D) then
    iso1 := SEMIGROUPS.InjectionPrincipalFactor(D, ReesMatrixSemigroup);
    iso2 := RMSNormalization(Range(iso1));
  else
    iso1 := SEMIGROUPS.InjectionPrincipalFactor(D, ReesZeroMatrixSemigroup);
    iso2 := RZMSNormalization(Range(iso1));
  fi;

  rms := Range(iso2);
  inv1 := InverseGeneralMapping(iso1);
  inv2 := InverseGeneralMapping(iso2);
  iso := x -> (x ^ iso1) ^ iso2;
  inv := x -> (x ^ inv2) ^ inv1;
  hom := MappingByFunction(D, rms, iso, inv);
  SetIsInjective(hom, true);
  SetIsTotal(hom, true);
  return hom;
end);

InstallMethod(MultiplicativeNeutralElement,
"for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local D, e;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  if IsMultiplicativeElementWithOneCollection(S) and One(S) in S then
    return One(S);
  fi;

  if Length(MaximalDClasses(S)) > 1 then
    return fail;
  fi;

  D := MaximalDClasses(S)[1];

  if not NrHClasses(D) = 1 or not IsRegularDClass(D) then
    return fail;
  fi;

  e := Idempotents(D)[1];

  if ForAll(GeneratorsOfSemigroup(S), x -> e * x = x and x * e = x) then
    return e;
  fi;
  return fail;
end);

# same method for inverse/ideals

InstallMethod(RepresentativeOfMinimalIdeal, "for a semigroup",
[IsSemigroup],
function(S)
  local data, comps;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  if IsSemigroupIdeal(S) and
      (HasRepresentativeOfMinimalIdeal(SupersemigroupOfIdeal(S))
       or not HasGeneratorsOfSemigroup(S)) then
    return RepresentativeOfMinimalIdeal(SupersemigroupOfIdeal(S));
  fi;

  # This catches known trivial semigroups
  # WW: The idea is to quickly get at an arbitrary element of the semigroup
  if HasIsSimpleSemigroup(S) and IsSimpleSemigroup(S) then
    return Representative(S);
  fi;

  data := Enumerate(GenericSemigroupData(S));
  comps := GreensRRelation(S)!.data.comps;
  return SEMIGROUP_AS_LIST(data)[comps[1][1]];
  # the first component (i.e. the inner most) of the strongly connected
  # components of the right Cayley graph corresponds the minimal ideal.
end);

#

################################################################################
# SmallDegreeTransRepFromLattice: used for two user-facing functions (see below)
# Returns the smallest degree transformation semigroup corresponding to right
# congruences found using LatticeOfXCongruences with the given record.
################################################################################
SEMIGROUPS.SmallDegreeTransRepFromLattice := function(S, record)
  #TODO: The map should have an invfun included
  local M, l, congs, nrclasses, cong, bestcong, classes, fun, R;
  # If S is not a monoid, append an identity
  if not IsMonoid(S) then
    M := Monoid(S);
  else
    M := S;
  fi;

  # Get all the right congruences which apply here
  l := SEMIGROUPS.LatticeOfXCongruences(M, "Right", record);
  congs := l![2];

  # Find the one with the fewest classes
  nrclasses := infinity;
  for cong in congs do
    if NrEquivalenceClasses(cong) < nrclasses then
      bestcong := cong;
      nrclasses := NrEquivalenceClasses(cong);
    fi;
  od;

  # If nrclasses is not lower than the current degree, just return the identity
  if IsTransformationSemigroup(S)
     and nrclasses >= DegreeOfTransformationSemigroup(S) then
    return IdentityMapping(S);
  fi;

  # Consider the action of M on the classes of cong
  classes := EquivalenceClasses(bestcong);
  fun := function(elm)
    local image;
    image := List(classes, c -> Position(classes,
                                         OnRightCongruenceClasses(c, elm)));
    return Transformation(image);
  end;

  # Construct the range from the original generators (possibly not a monoid)
  R := Semigroup(List(GeneratorsOfSemigroup(S), fun));
  return MappingByFunction(S, R, fun); #, invfun);
end;

#

InstallMethod(SmallerDegreeTransformationRepresentation,
"for a semigroup",
[IsSemigroup],
# Use the best right congruence which contains no congruences
S -> SEMIGROUPS.SmallDegreeTransRepFromLattice(S, rec(transrep := true)));

#

InstallMethod(SmallDegreeTransformationRepresentation,
"for a semigroup",
[IsSemigroup],
# Use the best 1-generated right congruence which contains no congruences
S -> SEMIGROUPS.SmallDegreeTransRepFromLattice(S, rec(transrep := true,
                                                      1gen := true)));

