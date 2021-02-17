#############################################################################
##
##  froidure-pin.gi
##  Copyright (C) 2015-2021                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for Green's relations and classes of semigroups
# that satisfy IsSemigroup and CanComputeFroidurePin.

#############################################################################
##
## Contents:
##
##   1. Helper functions for the creation of Green's classes, and lambda-rho
##      stuff.
##
##   2. Technical Green's stuff (types, representative, etc)
##
##   3. Green's relations
##
##   4. Individual Green's classes, and equivalence classes of Green's
##      relations (constructors, size, membership)
##
##   5. Collections of Green's classes (GreensXClasses, XClassReps, NrXClasses)
##
##   6. Idempotents and NrIdempotents
##
##   7. Mapping etc
##
#############################################################################

#############################################################################
## The main idea is to store all of the information about all Green's classes
## of a particular type in the corresponding Green's relation. An individual
## Green's class only stores the index of the equivalence class corresponding
## to the Green's class, then looks everything up in the data contained in the
## Green's relation. The equivalence class data structures for R-, L-, H-,
## D-classes of a semigroup <S> that can compute a Froidure-Pin are stored in
## the !.data component of the corresponding Green's relation.
#############################################################################

#############################################################################
## 1. Helper functions for the creation of Green's classes/relations . . .
#############################################################################

# There are no methods for EquivalenceClassOfElementNC for Green's classes of
# semigroups that can compute Froidure-Pin because if we create a Green's class
# without knowing the Green's relations and the related strongly connected
# components data, then the Green's class won't have the correct type and won't
# have access to the correct methods.

SEMIGROUPS.EquivalenceClassOfElement := function(rel, rep, type)
  local pos, out, S;

  pos := PositionCanonical(Source(rel), rep);
  if pos = fail then
    ErrorNoReturn("the 2nd argument (a mult. elt.) does not belong to the ",
                  "source of the 1st argument (a Green's relation)");
  fi;

  out := rec();
  S := Source(rel);
  ObjectifyWithAttributes(out, type(S), EquivalenceClassRelation, rel,
                          Representative, rep, ParentAttr, S);

  out!.index := rel!.data.id[pos];

  return out;
end;

SEMIGROUPS.GreensXClasses := function(S,
                                      GreensXRelation,
                                      GreensXClassOfElement)
  local comps, enum, out, C, i;

  comps := GreensXRelation(S)!.data.comps;
  enum  := EnumeratorCanonical(S);
  out   := EmptyPlist(Length(comps));

  for i in [1 .. Length(comps)] do
    C := GreensXClassOfElement(S, enum[comps[i][1]]);
    C!.index := i;
    out[i] := C;
  od;
  return out;
end;

SEMIGROUPS.XClassReps := function(S, GreensXRelation)
  local comps, enum, out, i;

  comps := GreensXRelation(S)!.data.comps;
  enum  := EnumeratorCanonical(S);
  out   := EmptyPlist(Length(comps));
  for i in [1 .. Length(comps)] do
    out[i] := enum[comps[i][1]];
  od;
  return out;
end;

SEMIGROUPS.GreensXClassesOfClass := function(C,
                                             GreensXRelation,
                                             GreensXClassOfElement)
  local S, comp, id, seen, enum, out, i;

  S := Parent(C);
  comp := EquivalenceClassRelation(C)!.data.comps[C!.index];
  id := GreensXRelation(Parent(C))!.data.id;
  seen := BlistList([1 .. Maximum(id)], []);
  enum := EnumeratorCanonical(S);
  out := EmptyPlist(Length(comp));

  for i in comp do
    if not seen[id[i]] then
      seen[id[i]] := true;
      C := GreensXClassOfElement(S, enum[i]);
      C!.index := id[i];
      Add(out, C);
    fi;
  od;

  return out;
end;

SEMIGROUPS.XClassRepsOfClass := function(C, GreensXRelation)
  local S, comp, id, seen, enum, out, i;

  S := Parent(C);
  comp := EquivalenceClassRelation(C)!.data.comps[C!.index];
  id := GreensXRelation(Parent(C))!.data.id;
  seen := BlistList([1 .. Maximum(id)], []);
  enum := EnumeratorCanonical(S);
  out := EmptyPlist(Length(comp));

  for i in comp do
    if not seen[id[i]] then
      seen[id[i]] := true;
      Add(out, enum[i]);
    fi;
  od;

  return out;
end;

SEMIGROUPS.XClassIndex := C -> C!.index;

#############################################################################
## 2. Technical Green's classes stuff . . .
#############################################################################

# This should be removed after the library method for AsSSortedList for a
# Green's class is removed. The default AsSSortedList for a collection is what
# should be used (it is identical)! FIXME

InstallMethod(AsSSortedList, "for a Green's class",
[IsGreensClassOfSemigroupThatCanComputeFroidurePinRep],
C -> ConstantTimeAccessList(EnumeratorSorted(C)));

InstallMethod(Size,
"for a Green's class of a semigroup that CanComputeFroidurePin",
[IsGreensClassOfSemigroupThatCanComputeFroidurePinRep],
function(C)
  return Length(EquivalenceClassRelation(C)!.
                data.comps[SEMIGROUPS.XClassIndex(C)]);
end);

InstallMethod(\in,
"for a mult. elt and a Green's class of a semigroup that CanComputeFroidurePin",
[IsMultiplicativeElement,
 IsGreensClassOfSemigroupThatCanComputeFroidurePinRep],
function(x, C)
  local pos;
  pos := PositionCanonical(Parent(C), x);
  return pos <> fail
    and EquivalenceClassRelation(C)!.data.id[pos] = SEMIGROUPS.XClassIndex(C);
end);

InstallMethod(DClassType, "for a semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
function(S)
  return NewType(FamilyObj(S),
                 IsGreensClassOfSemigroupThatCanComputeFroidurePinRep
                 and IsEquivalenceClassDefaultRep
                 and IsGreensDClass);
end);

InstallMethod(HClassType, "for a semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
function(S)
  return NewType(FamilyObj(S),
                 IsGreensClassOfSemigroupThatCanComputeFroidurePinRep
                 and IsEquivalenceClassDefaultRep
                 and IsGreensHClass);
end);

InstallMethod(LClassType, "for a semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
function(S)
  return NewType(FamilyObj(S),
                 IsGreensClassOfSemigroupThatCanComputeFroidurePinRep
                 and IsEquivalenceClassDefaultRep
                 and IsGreensLClass);
end);

InstallMethod(RClassType, "for a semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
function(S)
  return NewType(FamilyObj(S),
                 IsGreensClassOfSemigroupThatCanComputeFroidurePinRep
                 and IsEquivalenceClassDefaultRep
                 and IsGreensRClass);
end);

#############################################################################
## 3. Green's relations
#############################################################################

# same method for ideals

InstallMethod(GreensRRelation, "for a semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
function(S)
  local fam, data, filt, rel;
  if IsActingSemigroup(S) or (HasIsFinite(S) and not IsFinite(S)) then
    TryNextMethod();
  fi;
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));
  data := DigraphStronglyConnectedComponents(RightCayleyDigraph(S));
  filt := IsGreensRelationOfSemigroupThatCanComputeFroidurePinRep;
  rel := Objectify(NewType(fam,
                           IsEquivalenceRelation
                             and IsEquivalenceRelationDefaultRep
                             and IsGreensRRelation
                             and filt),
                   rec(data := data));
  SetSource(rel, S);
  SetRange(rel, S);
  SetIsLeftSemigroupCongruence(rel, true);

  return rel;
end);

# same method for ideals

InstallMethod(GreensLRelation, "for a semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
function(S)
  local fam, data, filt, rel;
  if IsActingSemigroup(S) or (HasIsFinite(S) and not IsFinite(S)) then
    TryNextMethod();
  fi;
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));
  data := DigraphStronglyConnectedComponents(LeftCayleyDigraph(S));
  filt := IsGreensRelationOfSemigroupThatCanComputeFroidurePinRep;
  rel := Objectify(NewType(fam,
                           IsEquivalenceRelation
                             and IsEquivalenceRelationDefaultRep
                             and IsGreensLRelation
                             and filt),
                   rec(data := data));
  SetSource(rel, S);
  SetRange(rel, S);
  SetIsRightSemigroupCongruence(rel, true);

  return rel;
end);

# same method for ideals

InstallMethod(GreensDRelation, "for semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
function(S)
  local fam, data, filt, rel;
  if IsActingSemigroup(S) or (HasIsFinite(S) and not IsFinite(S))
      or (IsFreeBandCategory(S) and Size(GeneratorsOfSemigroup(S)) > 4) then
    TryNextMethod();
  fi;
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));

  data := SCC_UNION_LEFT_RIGHT_CAYLEY_GRAPHS(GreensRRelation(S)!.data,
                                             GreensLRelation(S)!.data);
  filt := IsGreensRelationOfSemigroupThatCanComputeFroidurePinRep;
  rel := Objectify(NewType(fam,
                           IsEquivalenceRelation
                             and IsEquivalenceRelationDefaultRep
                             and IsGreensDRelation
                             and filt),
                   rec(data := data));
  SetSource(rel, S);
  SetRange(rel, S);

  return rel;
end);

# same method for ideals

InstallMethod(GreensHRelation, "for a semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
function(S)
  local fam, data, filt, rel;
  if IsActingSemigroup(S) or (HasIsFinite(S) and not IsFinite(S)) then
    TryNextMethod();
  fi;
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));

  data := FIND_HCLASSES(GreensRRelation(S)!.data,
                        GreensLRelation(S)!.data);

  filt := IsGreensRelationOfSemigroupThatCanComputeFroidurePinRep;
  rel := Objectify(NewType(fam, IsEquivalenceRelation
                                and IsEquivalenceRelationDefaultRep
                                and IsGreensHRelation
                                and filt),
                   rec(data := data));
  SetSource(rel, S);
  SetRange(rel, S);

  return rel;
end);

#############################################################################
## 4. Individual classes . . .
#############################################################################

InstallMethod(Enumerator,
"for a Green's class of a semigroup that CanComputeFroidurePin",
[IsGreensClassOfSemigroupThatCanComputeFroidurePinRep],
function(C)
  local rel, ind;
  rel := EquivalenceClassRelation(C);
  ind := rel!.data.comps[SEMIGROUPS.XClassIndex(C)];
  return EnumeratorCanonical(Range(rel)){ind};
end);

InstallMethod(EquivalenceClassOfElement,
"for a semigroup with CanComputeFroidurePin Green's R-relation + a mult. elt.",
[IsGreensRRelation and IsGreensRelationOfSemigroupThatCanComputeFroidurePinRep,
 IsMultiplicativeElement],
function(rel, rep)
  return SEMIGROUPS.EquivalenceClassOfElement(rel, rep, RClassType);
end);

InstallMethod(EquivalenceClassOfElement,
"for a semigroup with CanComputeFroidurePin Green's L-relation + a mult. elt.",
[IsGreensLRelation and IsGreensRelationOfSemigroupThatCanComputeFroidurePinRep,
 IsMultiplicativeElement],
function(rel, rep)
  return SEMIGROUPS.EquivalenceClassOfElement(rel, rep, LClassType);
end);

InstallMethod(EquivalenceClassOfElement,
"for a semigroup with CanComputeFroidurePin Green's H-relation + a mult. elt.",
[IsGreensHRelation and IsGreensRelationOfSemigroupThatCanComputeFroidurePinRep,
 IsMultiplicativeElement],
function(rel, rep)
  return SEMIGROUPS.EquivalenceClassOfElement(rel, rep, HClassType);
end);

InstallMethod(EquivalenceClassOfElement,
"for a semigroup with CanComputeFroidurePin Green's D-relation + a mult. elt.",
[IsGreensDRelation and IsGreensRelationOfSemigroupThatCanComputeFroidurePinRep,
 IsMultiplicativeElement],
function(rel, rep)
  return SEMIGROUPS.EquivalenceClassOfElement(rel, rep, DClassType);
end);

# No check Green's classes of an element of a semigroup . . .

# The methods for GreensXClassOfElementNC for arbitrary finite semigroup use
# EquivalenceClassOfElementNC which only have a method in the library, and
# hence the created classes could not be in
# IsGreensClassOfSemigroupThatCanComputeFroidurePinRep. In any case, calling
# GreensXRelation(S) (as these methods do) on a semigroup with
# CanComputeFroidurePin completely enumerates it, so the only thing we gain
# here is one constant time check that the representative actually belongs to
# the semigroup.

InstallMethod(GreensRClassOfElementNC,
"for a finite semigroup with CanComputeFroidurePin and multiplicative element",
[IsSemigroup and CanComputeFroidurePin and IsFinite, IsMultiplicativeElement],
GreensRClassOfElement);

InstallMethod(GreensLClassOfElementNC,
"for a finite semigroup with CanComputeFroidurePin and multiplicative element",
[IsSemigroup and CanComputeFroidurePin and IsFinite, IsMultiplicativeElement],
GreensLClassOfElement);

InstallMethod(GreensHClassOfElementNC,
"for a finite semigroup with CanComputeFroidurePin and multiplicative element",
[IsSemigroup and CanComputeFroidurePin and IsFinite, IsMultiplicativeElement],
GreensHClassOfElement);

InstallMethod(GreensDClassOfElementNC,
"for a finite semigroup with CanComputeFroidurePin and multiplicative element",
[IsSemigroup and CanComputeFroidurePin and IsFinite, IsMultiplicativeElement],
GreensDClassOfElement);

#############################################################################
## 5. Collections of classes, and reps
#############################################################################

## numbers of classes

InstallMethod(NrDClasses, "for a semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
S -> Length(GreensDRelation(S)!.data.comps));

InstallMethod(NrLClasses, "for a semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
S -> Length(GreensLRelation(S)!.data.comps));

InstallMethod(NrRClasses, "for a semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
S -> Length(GreensRRelation(S)!.data.comps));

InstallMethod(NrHClasses, "for a semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
S -> Length(GreensHRelation(S)!.data.comps));

# same method for ideals

InstallMethod(GreensLClasses,
"for a finite semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return SEMIGROUPS.GreensXClasses(S, GreensLRelation, GreensLClassOfElement);
end);

# same method for ideals

InstallMethod(GreensRClasses,
"for a finite semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return SEMIGROUPS.GreensXClasses(S, GreensRRelation, GreensRClassOfElement);
end);

# same method for ideals

InstallMethod(GreensHClasses,
"for a finite semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return SEMIGROUPS.GreensXClasses(S, GreensHRelation, GreensHClassOfElement);
end);

# same method for ideals

InstallMethod(GreensDClasses,
"for a finite semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return SEMIGROUPS.GreensXClasses(S, GreensDRelation, GreensDClassOfElement);
end);

## Green's classes of a Green's class

InstallMethod(GreensLClasses,
"for Green's D-class of a semigroup with CanComputeFroidurePin",
[IsGreensDClass and IsGreensClassOfSemigroupThatCanComputeFroidurePinRep],
function(C)
  return SEMIGROUPS.GreensXClassesOfClass(C, GreensLRelation,
                                          GreensLClassOfElement);
end);

InstallMethod(GreensRClasses,
"for a Green's D-class of a semigroup with CanComputeFroidurePin",
[IsGreensDClass and IsGreensClassOfSemigroupThatCanComputeFroidurePinRep],
function(C)
  return SEMIGROUPS.GreensXClassesOfClass(C,
                                          GreensRRelation,
                                          GreensRClassOfElement);
end);

InstallMethod(GreensHClasses,
"for a Green's class of a semigroup with CanComputeFroidurePin",
[IsGreensClass and IsGreensClassOfSemigroupThatCanComputeFroidurePinRep],
function(C)
  if not (IsGreensRClass(C) or IsGreensLClass(C) or IsGreensDClass(C)) then
    ErrorNoReturn("the argument is not a Green's R-, L-, or D-class");
  fi;
  return SEMIGROUPS.GreensXClassesOfClass(C, GreensHRelation,
                                          GreensHClassOfElement);
end);

## Representatives

InstallMethod(DClassReps, "for a semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
S -> SEMIGROUPS.XClassReps(S, GreensDRelation));

InstallMethod(RClassReps, "for a semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
S -> SEMIGROUPS.XClassReps(S, GreensRRelation));

InstallMethod(LClassReps, "for a semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
S -> SEMIGROUPS.XClassReps(S, GreensLRelation));

InstallMethod(HClassReps, "for a semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin],
S -> SEMIGROUPS.XClassReps(S, GreensHRelation));

InstallMethod(RClassReps,
"for a Green's D-class of a semigroup with CanComputeFroidurePin",
[IsGreensDClass and IsGreensClassOfSemigroupThatCanComputeFroidurePinRep],
D -> SEMIGROUPS.XClassRepsOfClass(D, GreensRRelation));

InstallMethod(LClassReps,
"for a Green's D-class of a semigroup with CanComputeFroidurePin",
[IsGreensDClass and IsGreensClassOfSemigroupThatCanComputeFroidurePinRep],
D -> SEMIGROUPS.XClassRepsOfClass(D, GreensLRelation));

InstallMethod(HClassReps,
"for a Green's class of a semigroup with CanComputeFroidurePin",
[IsGreensClass and IsGreensClassOfSemigroupThatCanComputeFroidurePinRep],
C -> SEMIGROUPS.XClassRepsOfClass(C, GreensHRelation));

# There is duplicate code in here and in maximal D-classes.
#
# This cannot be replaced with the method for IsSemigroup and IsFinite since
# the value of GreensDRelation(S)!.data.comps is not the same as the output of
# DigraphStronglyConnectedComponents.

InstallMethod(PartialOrderOfDClasses,
"for a finite semigroup with CanComputeFroidurePin",
[IsSemigroup and CanComputeFroidurePin and IsFinite],
function(S)
  local l, r, gr;
  l  := LeftCayleyDigraph(S);
  r  := RightCayleyDigraph(S);
  gr := DigraphEdgeUnion(l, r);
  gr := QuotientDigraph(gr, GreensDRelation(S)!.data.comps);
  return List(OutNeighbours(gr), Set);
end);

#############################################################################
## 6. Idempotents . . .
#############################################################################

InstallMethod(NrIdempotents,
"for a Green's class of a semigroup that CanComputeFroidurePin",
[IsGreensClassOfSemigroupThatCanComputeFroidurePinRep],
function(C)
  local rel, pos;
  rel := EquivalenceClassRelation(C);
  pos := IdempotentsSubset(Range(rel),
                           rel!.data.comps[SEMIGROUPS.XClassIndex(C)]);
  return Length(pos);
end);

InstallMethod(Idempotents,
"for a Green's class of a semigroup that CanComputeFroidurePin",
[IsGreensClassOfSemigroupThatCanComputeFroidurePinRep],
function(C)
  local rel, pos;
  rel := EquivalenceClassRelation(C);
  pos := IdempotentsSubset(Range(rel),
                           rel!.data.comps[SEMIGROUPS.XClassIndex(C)]);
  return EnumeratorCanonical(Range(rel)){pos};
end);

#############################################################################
## 7. Mappings etc . . .
#############################################################################

InstallMethod(IsomorphismPermGroup, "for H-class of a semigroup",
[IsGreensHClass and IsGreensClassOfSemigroupThatCanComputeFroidurePinRep],
function(H)
  local G, S, N, HH, lookup, pos, x, map, inverses, GG, inv, i;

  if not IsGroupHClass(H) then
    ErrorNoReturn("the argument (a Green's H-class) is not a group");
  fi;

  G := Group(());
  S := EnumeratorCanonical(Parent(H));
  N := Size(H);
  HH := Enumerator(H);
  # Position(S, x) -> Position(H, x)
  lookup := ListWithIdenticalEntries(Length(S), fail);
  for i in [1 .. N] do
    pos := Position(S, HH[i]);
    lookup[pos] := i;
  od;

  for x in H do
    x := PermList(List([1 .. N], i -> lookup[Position(S, HH[i] * x)]));
    if not x in G then
      G := ClosureGroup(G, x);
      if Size(G) = N then
        break;
      fi;
    fi;
  od;

  GG := EnumeratorSorted(G);

  map := function(x)
    if not x in H then
      ErrorNoReturn("the argument does not belong to the domain of the ",
                    "function");
    fi;
    return GG[lookup[Position(S, HH[1] * x)]];
  end;
  inverses := [];
  for i in [1 .. N] do
    inverses[Position(GG, map(HH[i]))] := HH[i];
  od;
  inv := function(x)
    if not x in G then
      ErrorNoReturn("the argument does not belong to the domain of the ",
                    "function");
    fi;
    return inverses[Position(GG, x)];
  end;
  return MappingByFunction(H, G, map, inv);
end);
