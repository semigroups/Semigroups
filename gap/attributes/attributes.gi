#############################################################################
##
#W  attributes.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
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
# One(s)=Transformation([1..DegreeOfTransformationSemigroup(s)]) if s is a
# transformation semigroup and it returns fail otherwise, or it returns
# PartialPerm([1..DegreeOfPartialPermSemigroup]) if this belongs to s.
#
# MultiplicativeNeutralElement on the other hand returns the element of s that
# acts as the identity, note that this can be equal to One(s) but it can also
# not be equal to One(s).
#
# A semigroup satisfies IsMonoidAsSemigroup(s) if
# MultiplicativeNeutralElement(x)<>fail, so it could be that One(s) returns
# fail but IsMonoidAsSemigroup is still true.

BindGlobal("SEMIGROUPS_InjectionPrincipalFactor",
function(D, constructor)
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
end);

#############################################################################
## 1. Default methods, for which there are currently no better methods.
#############################################################################

InstallMethod(NrIdempotents, "for a semigroup",
[IsSemigroup],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;

  if SEMIGROUPS_IsCCSemigroup(S) then
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

  U := Monoid(List(GeneratorsOfGroup(Range(map)),
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

InstallMethod(IsomorphismFpMonoid, "for a monoid",
[IsMonoid], 3,
function(S)
  local F, A, lookup, pos, data, rules, rels, convert, Q, B, rule;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  F := FreeMonoid(Length(GeneratorsOfMonoid(S)));
  A := GeneratorsOfMonoid(F);
  lookup := List(GeneratorsOfSemigroup(S),
                 x -> Position(GeneratorsOfMonoid(S), x));
  pos := Position(lookup, fail);

  data := GenericSemigroupData(S);
  rules := SEMIGROUP_RELATIONS(data);
  rels := [];

  convert := function(word)
    local out, i;
    out := One(F);
    for i in word do
      if lookup[i] <> fail then
        out := out * A[lookup[i]];
      fi;
    od;
    return out;
  end;

  for rule in rules do
    # only include non-redundant rules
    if Length(rule[1]) <> 2
        or (rule[1][1] <> pos and rule[1][Length(rule[1])] <> pos) then
      Add(rels, [convert(rule[1]), convert(rule[2])]);
    fi;
  od;

  Q := F / rels;
  B := GeneratorsOfSemigroup(Q);
  # gaplint: ignore 3
  return MagmaIsomorphismByFunctionsNC(S, Q,
           x -> EvaluateWord(B, Factorization(S, x)),
           x -> MappedWord(UnderlyingElement(x), A, GeneratorsOfMonoid(S)));
end);

# same method for ideals

InstallMethod(IsomorphismFpSemigroup, "for a semigroup",
[IsSemigroup], 3,
function(S)
  local rules, F, A, rels, Q, B;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  rules := SEMIGROUP_RELATIONS(GenericSemigroupData(S));

  F := FreeSemigroup(Length(GeneratorsOfSemigroup(S)));
  A := GeneratorsOfSemigroup(F);
  rels := List(rules, x -> [EvaluateWord(A, x[1]), EvaluateWord(A, x[2])]);

  Q := F / rels;
  B := GeneratorsOfSemigroup(Q);

  # gaplint: ignore 3
  return MagmaIsomorphismByFunctionsNC(S, Q,
           x -> EvaluateWord(B, Factorization(S, x)),
           x -> MappedWord(UnderlyingElement(x), A, GeneratorsOfSemigroup(S)));
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
    ErrorMayQuit("Semigroups: IsomorphismReesMatrixSemigroup: usage,\n",
                 "the D-class is not a subsemigroup,");
  fi;
  return SEMIGROUPS_InjectionPrincipalFactor(D, ReesMatrixSemigroup);
end);

# same method for ideal

InstallMethod(IrredundantGeneratingSubset,
"for an associative element collection",
[IsAssociativeElementCollection],
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

#

InstallMethod(IsomorphismReesMatrixSemigroup, "for a semigroup", [IsSemigroup],
function(S)
  local D, iso, inv;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  if not IsSimpleSemigroup(S) then
    ErrorMayQuit("Semigroups: IsomorphismReesMatrixSemigroup: usage,\n",
                 "the argument must be a simple semigroup,");
    #TODO is there another method? I.e. can we turn non-simple/non-0-simple
    # semigroups into Rees (0-)matrix semigroups over non-groups?
  fi;

  D := GreensDClasses(S)[1];
  iso := IsomorphismReesMatrixSemigroup(D);
  inv := InverseGeneralMapping(iso);
  return MagmaIsomorphismByFunctionsNC(S, Range(iso),
                                       x -> x ^ iso, x -> x ^ inv);
end);

InstallMethod(IsomorphismReesZeroMatrixSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  local D, iso, inv;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  if not IsZeroSimpleSemigroup(S) then
    ErrorMayQuit("Semigroups: IsomorphismReesZeroMatrixSemigroup: usage,\n",
                 "the argument must be a 0-simple semigroup,");
    #TODO is there another method? I.e. can we turn non-simple/non-0-simple
    # semigroups into Rees (0-)matrix semigroups over non-groups?
  fi;

  D := First(GreensDClasses(S),
             x -> not IsMultiplicativeZero(S, Representative(x)));
  iso := SEMIGROUPS_InjectionPrincipalFactor(D, ReesZeroMatrixSemigroup);
  inv := InverseGeneralMapping(iso);
  return MagmaIsomorphismByFunctionsNC(S, Range(iso),
                                       x -> x ^ iso, x -> x ^ inv);
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
                                    SEMIGROUPS_OptionsRec(S));
  SetIsSimpleSemigroup(I, true);
  return I;
end);

#

InstallMethod(PrincipalFactor, "for a Green's D-class",
[IsGreensDClass], D -> Range(InjectionPrincipalFactor(D)));

# different method for ideals, not yet implemented

InstallMethod(SmallSemigroupGeneratingSet,
"for an associative element collection",
[IsAssociativeElementCollection],
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
"for an associative element with one collection",
[IsAssociativeElementCollection and IsMultiplicativeElementWithOneCollection],
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
    ErrorMayQuit("Semigroups: SmallInverseSemigroupGeneratingSet: usage,\n",
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
    ErrorMayQuit("Semigroups: SmallInverseMonoidGeneratingSet: usage,\n",
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

InstallMethod(StructureDescription, "for a group as semigroup",
[IsGroupAsSemigroup],
function(S)
  if IsGroup(S) then
    # since groups (even perm groups) satisfy IsGroupAsSemigroup
    TryNextMethod(); #this is appropriate, don't change it!
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
      if Size(D) = 1 then
        return Representative(D);
      else
        return fail;
      fi;
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
  return Semigroup(Idempotents(S));
end);

InstallMethod(IdempotentGeneratedSubsemigroup,
"for an inverse op semigroup",
[IsSemigroupWithInverseOp],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return InverseSemigroup(Idempotents(S));
end);

InstallMethod(InjectionPrincipalFactor, "for a Green's D-class (Semigroups)",
[IsGreensDClass],
function(D)
  if not IsRegularDClass(D) then
    ErrorMayQuit("Semigroups: InjectionPrincipalFactor: usage,\n",
                 "the argument <D> must be a regular D-class,");
  fi;
  if NrHClasses(D) = NrIdempotents(D) then
    return SEMIGROUPS_InjectionPrincipalFactor(D, ReesMatrixSemigroup);
  fi;
  return SEMIGROUPS_InjectionPrincipalFactor(D, ReesZeroMatrixSemigroup);
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
  return SEMIGROUP_ELEMENTS(data, infinity)[comps[1][1]];
  # the first component (i.e. the inner most) of the strongly connected
  # components of the right Cayley graph corresponds the minimal ideal.
end);

# fall back method, same method for ideals

InstallMethod(IsomorphismPermGroup, "for a semigroup", [IsSemigroup],
function(S)
  local en, act, gens;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  if not IsGroupAsSemigroup(S)  then
    ErrorMayQuit("Semigroups: IsomorphismPermGroup: usage,\n",
                 "the argument must be a semigroup satisfying ",
                 "IsGroupAsSemigroup,");
  fi;

  en := EnumeratorSorted(S);

  act := function(i, x)
    return Position(en, en[i] * x);
  end;

  gens := List(GeneratorsOfSemigroup(S),
               x -> Permutation(x, [1 .. Length(en)], act));

  # gaplint: ignore 3
  return MagmaIsomorphismByFunctionsNC(S, Group(gens),
           x -> Permutation(x, [1 .. Length(en)], act),
           x -> en[Position(en, MultiplicativeNeutralElement(S)) ^ x]);
end);
