#############################################################################
##
#W  attr.gi
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
    local i, j;

    if not x in D then
      return fail;
    fi;

    i := PositionProperty(R, y -> y in RClass(D, x));
    j := PositionProperty(L, y -> y in LClass(D, x));

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

# Don't use ClosureSemigroup here since the order of the generators matters
# and ClosureSemigroup shuffles the generators.

BindGlobal("_GeneratorsSmallest",
function(S)
  local iter, gens, T, x;

  iter := IteratorSorted(S);
  gens := [NextIterator(iter)];
  T    := Semigroup(gens);

  for x in iter do
    if not x in T then
      T := SEMIGROUPS.ClosureSemigroupDestructive(T,
                                                  [x],
                                                  SEMIGROUPS.OptionsRec(T));
      Add(gens, x);
      if T = S then
        break;
      fi;
    fi;
  od;
  return gens;
end);

InstallMethod(GeneratorsSmallest, "for a transformation semigroup",
[IsTransformationSemigroup and IsGroup], _GeneratorsSmallest);

InstallMethod(GeneratorsSmallest, "for a semigroup",
[IsEnumerableSemigroupRep], _GeneratorsSmallest);

MakeReadWriteGlobal("_GeneratorsSmallest");
Unbind(_GeneratorsSmallest);

InstallMethod(SmallestElementSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return NextIterator(IteratorSorted(S));
end);

InstallMethod(LargestElementSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return EnumeratorSorted(S)[Size(S)];
end);

InstallMethod(NrIdempotents, "for a semigroup",
[IsSemigroup],
function(S)
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

  if nrgens = 1 then
    return gens;
  fi;

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

InstallMethod(PrincipalFactor, "for a Green's D-class",
[IsGreensDClass], D -> Range(InjectionPrincipalFactor(D)));

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

InstallMethod(SmallMonoidGeneratingSet,
"for a multiplicative element with one collection",
[IsMultiplicativeElementWithOneCollection],
function(coll)
  if Length(coll) = 1 then
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

InstallMethod(SmallInverseSemigroupGeneratingSet,
"for an inverse semigroup with inverse op",
[IsInverseSemigroup and IsGeneratorsOfInverseSemigroup],
S -> SmallSemigroupGeneratingSet(GeneratorsOfInverseSemigroup(S)));

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

InstallMethod(SmallInverseMonoidGeneratingSet,
"for an inverse monoid with inverse op",
[IsInverseMonoid and IsGeneratorsOfInverseSemigroup],
function(S)
  if IsEmpty(GeneratorsOfInverseMonoid(S)) then
    return GeneratorsOfInverseMonoid(S);
  fi;
  return SmallMonoidGeneratingSet(GeneratorsOfInverseMonoid(S));
end);

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

InstallMethod(StructureDescription, "for a Brandt semigroup",
[IsBrandtSemigroup],
function(S)
  local D;

  D := MaximalDClasses(S)[1];
  return Concatenation("B(", StructureDescription(GroupHClass(D)), ", ",
                       String(NrRClasses(D)), ")");
end);

# same method for ideals

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
  elif IsSemigroupIdeal(S)
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

InstallMethod(LengthOfLongestDClassChain, "for a semigroup",
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

InstallMethod(IsGreensDGreaterThanFunc, "for an enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  local gr, id;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  gr := Digraph(PartialOrderOfDClasses(S));
  gr := DigraphReflexiveTransitiveClosure(gr);
  id := GreensDRelation(S)!.data.id;

  return function(x, y)
    local u, v;
    if x = y then
      return false;
    fi;
    u := id[PositionCanonical(S, x)];
    v := id[PositionCanonical(S, y)];
    return u <> v and IsReachable(gr, u, v);
  end;
end);

InstallMethod(MaximalDClasses, "for an enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  local gr;

  if NrDClasses(S) = 1 then
    return DClasses(S);
  fi;

  gr := DigraphRemoveLoops(Digraph(PartialOrderOfDClasses(S)));
  return List(DigraphSources(gr), x -> DClasses(S)[x]);
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

InstallMethod(IdempotentGeneratedSubsemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  local out;
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  out := Semigroup(Idempotents(S), rec(small := true));
  SetIsIdempotentGenerated(out, true);
  if HasIsSemigroupWithCommutingIdempotents(S)
      and IsSemigroupWithCommutingIdempotents(S) then
    SetIsSemigroupWithCommutingIdempotents(out, true);
  fi;
  return out;
end);

InstallMethod(IdempotentGeneratedSubsemigroup,
"for a Rees matrix subsemigroup",
[IsReesMatrixSubsemigroup],
function(R)
  local mat, I, J, nrrows, nrcols, min, max, gens, out, i;

  if not IsFinite(R) or not IsReesMatrixSemigroup(R)
      or not IsGroup(UnderlyingSemigroup(R)) then
    TryNextMethod();
  fi;

  mat := Matrix(R);
  I := Rows(R);
  J := Columns(R);
  nrrows := Length(I);
  nrcols := Length(J);

  min := Minimum(nrrows, nrcols);
  max := Maximum(nrrows, nrcols);
  gens := EmptyPlist(max);
  for i in [1 .. min] do
    Add(gens, RMSElement(R, I[i], mat[J[i]][I[i]] ^ -1, J[i]));
  od;
  for i in [min + 1 .. nrrows] do
    Add(gens, RMSElement(R, I[i], mat[J[1]][I[i]] ^ -1, J[1]));
  od;
  for i in [min + 1 .. nrcols] do
    Add(gens, RMSElement(R, I[1], mat[J[i]][I[1]] ^ -1, J[i]));
  od;
  out := Semigroup(gens);
  SetIsRegularSemigroup(out, true);
  SetIsIdempotentGenerated(out, true);
  SetIsSimpleSemigroup(out, true);
  return out;
end);

InstallMethod(IdempotentGeneratedSubsemigroup,
"for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup],
function(R)
  local mat, gr, gens, I, J, nrrows, k, out, i, j;

  if not IsFinite(R) or not IsReesZeroMatrixSemigroup(R)
      or not IsGroup(UnderlyingSemigroup(R)) then
    TryNextMethod();
  fi;

  mat := Matrix(R);
  gr := RZMSDigraph(R);
  gens := [];

  if IsCompleteBipartiteDigraph(ReducedDigraph(gr)) or IsEmptyDigraph(gr) then
    Add(gens, MultiplicativeZero(R));
  fi;

  gr := OutNeighbours(UndirectedSpanningForest(gr));
  I := Rows(R);
  J := Columns(R);
  nrrows := Length(I);

  for i in [1 .. nrrows] do
    for j in gr[i] do
      k := J[j - nrrows];
      Add(gens, RMSElement(R, I[i], mat[k][I[i]] ^ -1, k));
    od;
  od;

  out := Semigroup(gens);
  SetIsRegularSemigroup(out, true);
  SetIsIdempotentGenerated(out, true);
  return out;
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
"for an enumerable semigroup with generators",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup],
function(S)
  local D, e;

  if not IsFinite(S) then
    TryNextMethod();
  elif IsMultiplicativeElementWithOneCollection(S) and One(S) in S then
    return One(S);
  elif Length(MaximalDClasses(S)) > 1 then
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

  if not IsFinite(S) then
    TryNextMethod();
  elif IsSemigroupIdeal(S) and
      (HasRepresentativeOfMinimalIdeal(SupersemigroupOfIdeal(S))
       or not HasGeneratorsOfSemigroup(S)) then
    return RepresentativeOfMinimalIdeal(SupersemigroupOfIdeal(S));
  elif HasIsSimpleSemigroup(S) and IsSimpleSemigroup(S) then
    # This catches known trivial semigroups
    # WW: The idea is to quickly get at an arbitrary element of the semigroup
    return Representative(S);
  fi;

  return RepresentativeOfMinimalIdealNC(S);
end);

InstallMethod(RepresentativeOfMinimalIdealNC, "for an enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  local comps;

  # The first component (i.e. the inner most) of the strongly connected
  # components of the right Cayley graph corresponds the minimal ideal.

  comps := GreensRRelation(S)!.data.comps;
  return EnumeratorCanonical(S)[comps[1][1]];
end);

InstallMethod(RepresentativeOfMinimalIdealNC, "for a finite semigroup",
[IsSemigroup and IsFinite],
function(S)
  local gr, pos;
  gr  := DigraphRemoveLoops(Digraph(PartialOrderOfDClasses(S)));
  pos := DigraphSinks(gr)[1];

  Assert(1, Length(DigraphSinks(gr)) = 1);
  return Representative(DClasses(S)[pos]);
end);

InstallMethod(InversesOfSemigroupElementNC,
"for a group as semigroup and a multiplicative element",
[IsGroupAsSemigroup, IsMultiplicativeElement],
function(G, x)
  local i, iso, inv;
  if IsMultiplicativeElementWithInverse(x) then
    i := InverseOp(x);
    if i <> fail then
      return [i];
    fi;
  fi;
  iso := IsomorphismPermGroup(G);
  inv := InverseGeneralMapping(iso);
  return [((x ^ iso) ^ -1) ^ inv];
end);

InstallMethod(InversesOfSemigroupElementNC,
"for a semigroup and a multiplicative element",
[IsSemigroup, IsMultiplicativeElement],
function(S, x)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return Filtered(AsSet(S), y -> x * y * x = x and y * x * y = y);
end);

InstallMethod(InversesOfSemigroupElement,
"for a semigroup and a multiplicative element",
[IsSemigroup, IsMultiplicativeElement], 1, # to beat the library method
function(S, x)
  if not IsFinite(S) then
    TryNextMethod();
  elif not x in S then
    ErrorNoReturn("Semigroups: InversesOfSemigroupElement: usage,\n",
                  "the second arg (a mult. element) must belong to the first ",
                  "arg (a semigroup),");
  fi;
  return InversesOfSemigroupElementNC(S, x);
end);

InstallMethod(UnderlyingSemigroupOfSemigroupWithAdjoinedZero,
"for a semigroup",
[IsSemigroup],
function(S)
  local zero, gens, T;

  if HasIsSemigroupWithAdjoinedZero(S)
      and not IsSemigroupWithAdjoinedZero(S) then
    return fail;
  fi;

  zero := MultiplicativeZero(S);
  if zero = fail then
    return fail;
  fi;

  gens := GeneratorsOfSemigroup(S);
  if Length(gens) = 1 then
    return fail;
  fi;
  if not zero in gens then
    return fail;
  fi;

  T := Semigroup(Difference(gens, [zero]));

  if zero in T then
    return fail;
  fi;
  return T;
end);

BindGlobal("_SemigroupSizeByIndexPeriod",
function(S)
  local gen, ind;
  gen := MinimalSemigroupGeneratingSet(S)[1];
  ind := IndexPeriodOfSemigroupElement(gen);
  if ind[1] = 1 then
    SetIsGroupAsSemigroup(S, true);
  fi;
  return Sum(ind) - 1;
end);

InstallMethod(Size,
"for a monogenic transformation semigroup with minimal generating set",
[IsMonogenicSemigroup and HasMinimalSemigroupGeneratingSet and
 IsTransformationSemigroup],
10, # to beat IsActingSemigroup
_SemigroupSizeByIndexPeriod);

InstallMethod(Size,
"for a monogenic partial perm semigroup with minimal generating set",
[IsMonogenicSemigroup and HasMinimalSemigroupGeneratingSet and
 IsPartialPermSemigroup],
10, # to beat IsActingSemigroup
_SemigroupSizeByIndexPeriod);

InstallMethod(Size,
"for a monogenic bipartition semigroup with minimal generating set",
[IsMonogenicSemigroup and HasMinimalSemigroupGeneratingSet and
 IsBipartitionSemigroup],
10, # to beat IsActingSemigroup
_SemigroupSizeByIndexPeriod);

InstallMethod(Size,
"for a monogenic semigroup of matrices over finite field with minimal gen set",
[IsMonogenicSemigroup and HasMinimalSemigroupGeneratingSet and
 IsMatrixOverFiniteFieldCollection],
_SemigroupSizeByIndexPeriod);

MakeReadWriteGlobal("_SemigroupSizeByIndexPeriod");
Unbind(_SemigroupSizeByIndexPeriod);

BindGlobal("_MonoidSizeByIndexPeriod",
function(S)
  local gen, ind;
  gen := MinimalMonoidGeneratingSet(S)[1];
  ind := IndexPeriodOfSemigroupElement(gen);
  if ind[1] = 1 and One(S) in HClass(S, gen) then
    # <gen> generates the One of S, so the One is not an additional element
    # Note that this implies that S is a cyclic group
    SetIsGroupAsSemigroup(S, true);
    return ind[2];
  fi;
  return Sum(ind);
end);

InstallMethod(Size,
"for a monogenic transformation monoid with minimal generating set",
[IsMonogenicMonoid and HasMinimalMonoidGeneratingSet and
 IsTransformationSemigroup],
5, # to beat IsActingSemigroup
_MonoidSizeByIndexPeriod);

InstallMethod(Size,
"for a monogenic partial perm monoid with minimal generating set",
[IsMonogenicMonoid and HasMinimalMonoidGeneratingSet and
 IsPartialPermSemigroup],
5, # to beat IsActingSemigroup
_MonoidSizeByIndexPeriod);

InstallMethod(Size,
"for a monogenic bipartition monoid with minimal generating set",
[IsMonogenicMonoid and HasMinimalMonoidGeneratingSet and
 IsBipartitionSemigroup],
5, # to beat IsActingSemigroup
_MonoidSizeByIndexPeriod);

InstallMethod(Size,
"for a monogenic monoid of matrices over finite field with minimal gen set",
[IsMonogenicMonoid and HasMinimalMonoidGeneratingSet and
 IsMatrixOverFiniteFieldCollection],
5, # to beat IsActingSemigroup
_MonoidSizeByIndexPeriod);

MakeReadWriteGlobal("_MonoidSizeByIndexPeriod");
Unbind(_MonoidSizeByIndexPeriod);

InstallMethod(MultiplicativeZero,
"for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local gens, z;
  # Does a generator act as a zero on all the other generators?
  gens := GeneratorsOfSemigroup(S);
  for z in gens do
    if ForAll(gens, g -> z * g = z and g * z = z) then
      return z;
    fi;
  od;
  TryNextMethod();
end);

InstallMethod(MultiplicativeZero,
"for a monoid with generators",
[IsMonoid and HasGeneratorsOfMonoid],
function(S)
  local gens, z;
  # Does a generator act as a zero on all the other generators?
  gens := GeneratorsOfMonoid(S);
  for z in gens do
    if ForAll(gens, g -> z * g = z and g * z = z) then
      return z;
    fi;
  od;
  TryNextMethod();
end);
