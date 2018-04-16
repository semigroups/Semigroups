#############################################################################
##
##  gren.gi
##  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for Green's relations and classes of semigroups
# that satisfy IsEnumerableSemigroupRep.

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
#############################################################################

#############################################################################
## The main idea is to store all of the information about all Green's classes
## of a particular type in the corresponding Green's relation. An individual
## Green's class only stores the index of the equivalence class corresponding
## to the Green's class, then looks everything up in the data contained in the
## Green's relation. The equivalence class data structures for R-, L-, H-,
## D-classes of an enumerable semigroup <S> are stored in the !.data component
## of the corresponding Green's relation.
#############################################################################

#############################################################################
## 1. Helper functions for the creation of Green's classes/relations . . .
#############################################################################

# There are no methods for EquivalenceClassOfElementNC for enumerable semigroup
# Green's classes because if we create a Green's class without knowing the
# Green's relations and the related strongly connected components data, then
# the Green's class won't have the correct type and won't have access to the
# correct methods.

SEMIGROUPS.EquivalenceClassOfElement := function(rel, rep, type)
  local pos, out, S;

  pos := PositionCanonical(Source(rel), rep);
  if pos = fail then
    ErrorNoReturn("Semigroups: SEMIGROUPS.EquivalenceClassOfElement: usage,\n",
                  "the element in the 2nd argument does not belong to the ",
                  "semigroup,");
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

SEMIGROUPS.XClassIndex := function(C)
  # local pos;
  # if not IsBound(C!.index) then
  #   in case of classes created using EquivalenceClassOfElementNC
  #   FIXME I don't think this can ever occur now.
  #   pos := PositionCanonical(Parent(C), Representative(C));
  #   C!.index := EquivalenceClassRelation(C)!.data.id[pos];
  # fi;
  return C!.index;
end;

#############################################################################
## 2. Technical Green's classes stuff . . .
#############################################################################

# This should be removed after the library method for AsSSortedList for a
# Green's class is removed. The default AsSSortedList for a collection is what
# should be used (it is identical)! FIXME

InstallMethod(AsSSortedList, "for a Green's class",
[IsEnumerableSemigroupGreensClassRep],
C -> ConstantTimeAccessList(EnumeratorSorted(C)));

InstallMethod(Size, "for an enumerable semigroup Green's class",
[IsEnumerableSemigroupGreensClassRep],
function(C)
  return Length(EquivalenceClassRelation(C)!.
                data.comps[SEMIGROUPS.XClassIndex(C)]);
end);

InstallMethod(\in,
"for a multiplicative element and an enumerable semigroup Green's class",
[IsMultiplicativeElement, IsEnumerableSemigroupGreensClassRep],
function(x, C)
  local pos;
  pos := PositionCanonical(Parent(C), x);
  return pos <> fail
    and EquivalenceClassRelation(C)!.data.id[pos] = SEMIGROUPS.XClassIndex(C);
end);

InstallMethod(DClassType, "for an enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  return NewType(FamilyObj(S), IsEnumerableSemigroupGreensClassRep
                               and IsEquivalenceClassDefaultRep
                               and IsGreensDClass);
end);

InstallMethod(HClassType, "for an enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  return NewType(FamilyObj(S), IsEnumerableSemigroupGreensClassRep
                               and IsEquivalenceClassDefaultRep
                               and IsGreensHClass);
end);

InstallMethod(LClassType, "for an enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  return NewType(FamilyObj(S), IsEnumerableSemigroupGreensClassRep
                               and IsEquivalenceClassDefaultRep
                               and IsGreensLClass);
end);

InstallMethod(RClassType, "for an enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  return NewType(FamilyObj(S), IsEnumerableSemigroupGreensClassRep
                               and IsEquivalenceClassDefaultRep
                               and IsGreensRClass);
end);

#############################################################################
## 3. Green's relations
#############################################################################

# same method for ideals

InstallMethod(GreensRRelation, "for an enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  local fam, rel;
  if IsActingSemigroup(S) then
    TryNextMethod();
  fi;
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));

  rel := Objectify(NewType(fam,
                           IsEquivalenceRelation
                             and IsEquivalenceRelationDefaultRep
                             and IsGreensRRelation
                             and IsEnumerableSemigroupGreensRelationRep),
                   rec(data := GABOW_SCC(RightCayleyGraphSemigroup(S))));
  SetSource(rel, S);
  SetRange(rel, S);
  SetIsLeftSemigroupCongruence(rel, true);

  return rel;
end);

# same method for ideals

InstallMethod(GreensLRelation, "for an enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  local fam, rel;
  if IsActingSemigroup(S) then
    TryNextMethod();
  fi;
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));

  rel := Objectify(NewType(fam,
                           IsEquivalenceRelation
                             and IsEquivalenceRelationDefaultRep
                             and IsGreensLRelation
                             and IsEnumerableSemigroupGreensRelationRep),
                   rec(data := GABOW_SCC(LeftCayleyGraphSemigroup(S))));
  SetSource(rel, S);
  SetRange(rel, S);
  SetIsRightSemigroupCongruence(rel, true);

  return rel;
end);

# same method for ideals

InstallMethod(GreensDRelation, "for an enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  local fam, data, rel;
  if IsActingSemigroup(S) then
    TryNextMethod();
  fi;
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));

  data := SCC_UNION_LEFT_RIGHT_CAYLEY_GRAPHS(GreensRRelation(S)!.data,
                                             GreensLRelation(S)!.data);
  rel := Objectify(NewType(fam,
                           IsEquivalenceRelation
                             and IsEquivalenceRelationDefaultRep
                             and IsGreensDRelation
                             and IsEnumerableSemigroupGreensRelationRep),
                   rec(data := data));
  SetSource(rel, S);
  SetRange(rel, S);

  return rel;
end);

# same method for ideals

InstallMethod(GreensHRelation, "for an enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  local fam, data, rel;
  if IsActingSemigroup(S) then
    TryNextMethod();
  fi;
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));

  data := FIND_HCLASSES(GreensRRelation(S)!.data,
                        GreensLRelation(S)!.data);

  rel := Objectify(NewType(fam, IsEquivalenceRelation
                                and IsEquivalenceRelationDefaultRep
                                and IsGreensHRelation
                                and IsEnumerableSemigroupGreensRelationRep),
                   rec(data := data));
  SetSource(rel, S);
  SetRange(rel, S);

  return rel;
end);

#############################################################################
## 4. Individual classes . . .
#############################################################################

InstallMethod(Enumerator, "for an enumerable semigroup Green's class",
[IsEnumerableSemigroupGreensClassRep],
function(C)
  local rel, ind;
  rel := EquivalenceClassRelation(C);
  ind := rel!.data.comps[SEMIGROUPS.XClassIndex(C)];
  return EnumeratorCanonical(Range(rel)){ind};
end);

InstallMethod(EquivalenceClassOfElement,
"for an enumerable semigroup Green's R-relation and a multiplicative element",
[IsGreensRRelation and IsEnumerableSemigroupGreensRelationRep,
 IsMultiplicativeElement],
function(rel, rep)
  return SEMIGROUPS.EquivalenceClassOfElement(rel, rep, RClassType);
end);

InstallMethod(EquivalenceClassOfElement,
"for an enumerable semigroup Green's L-relation and a multiplicative element",
[IsGreensLRelation and IsEnumerableSemigroupGreensRelationRep,
 IsMultiplicativeElement],
function(rel, rep)
  return SEMIGROUPS.EquivalenceClassOfElement(rel, rep, LClassType);
end);

InstallMethod(EquivalenceClassOfElement,
"for an enumerable semigroup Green's H-relation and a multiplicative element",
[IsGreensHRelation and IsEnumerableSemigroupGreensRelationRep,
 IsMultiplicativeElement],
function(rel, rep)
  return SEMIGROUPS.EquivalenceClassOfElement(rel, rep, HClassType);
end);

InstallMethod(EquivalenceClassOfElement,
"for an enumerable semigroup Green's D-relation and a multiplicative element",
[IsGreensDRelation and IsEnumerableSemigroupGreensRelationRep,
 IsMultiplicativeElement],
function(rel, rep)
  return SEMIGROUPS.EquivalenceClassOfElement(rel, rep, DClassType);
end);

# No check Green's classes of an element of a semigroup . . .

# The methods for GreensXClassOfElementNC for arbitrary finite semigroup use
# EquivalenceClassOfElementNC which only have a method in the library, and hence
# the created classes could not be in IsEnumerableSemigroupGreensClassRep. In
# any case, calling GreensXRelation(S) (as these methods do) on an
# enumerable semigroup completely enumerates it, so the only thing we gain here
# is one constant time check that the representative actually belongs to the
# semigroup.

InstallMethod(GreensRClassOfElementNC,
"for a finite enumerable semigroup and multiplicative element",
[IsEnumerableSemigroupRep and IsFinite, IsMultiplicativeElement],
GreensRClassOfElement);

InstallMethod(GreensLClassOfElementNC,
"for a finite enumerable semigroup and multiplicative element",
[IsEnumerableSemigroupRep and IsFinite, IsMultiplicativeElement],
GreensLClassOfElement);

InstallMethod(GreensHClassOfElementNC,
"for a finite enumerable semigroup and multiplicative element",
[IsEnumerableSemigroupRep and IsFinite, IsMultiplicativeElement],
GreensHClassOfElement);

InstallMethod(GreensDClassOfElementNC,
"for a finite enumerable semigroup and multiplicative element",
[IsEnumerableSemigroupRep and IsFinite, IsMultiplicativeElement],
GreensDClassOfElement);

#############################################################################
## 5. Collections of classes, and reps
#############################################################################

## numbers of classes

InstallMethod(NrDClasses, "for an enumerable semigroup",
[IsEnumerableSemigroupRep], S -> Length(GreensDRelation(S)!.data.comps));

InstallMethod(NrLClasses, "for an enumerable semigroup",
[IsEnumerableSemigroupRep], S -> Length(GreensLRelation(S)!.data.comps));

InstallMethod(NrRClasses, "for an enumerable semigroup",
[IsEnumerableSemigroupRep], S -> Length(GreensRRelation(S)!.data.comps));

InstallMethod(NrHClasses, "for an enumerable semigroup",
[IsEnumerableSemigroupRep], S -> Length(GreensHRelation(S)!.data.comps));

# same method for ideals

InstallMethod(GreensLClasses, "for a finite enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return SEMIGROUPS.GreensXClasses(S, GreensLRelation, GreensLClassOfElement);
end);

# same method for ideals

InstallMethod(GreensRClasses, "for a finite enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return SEMIGROUPS.GreensXClasses(S, GreensRRelation, GreensRClassOfElement);
end);

# same method for ideals

InstallMethod(GreensHClasses, "for a finite enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return SEMIGROUPS.GreensXClasses(S, GreensHRelation, GreensHClassOfElement);
end);

# same method for ideals

InstallMethod(GreensDClasses, "for a finite enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return SEMIGROUPS.GreensXClasses(S, GreensDRelation, GreensDClassOfElement);
end);

## Green's classes of a Green's class

InstallMethod(GreensLClasses, "for Green's D-class of an enumerable semigroup",
[IsGreensDClass and IsEnumerableSemigroupGreensClassRep],
function(C)
  return SEMIGROUPS.GreensXClassesOfClass(C, GreensLRelation,
                                          GreensLClassOfElement);
end);

InstallMethod(GreensRClasses,
"for a Green's D-class of an enumerable semigroup",
[IsGreensDClass and IsEnumerableSemigroupGreensClassRep],
function(C)
  return SEMIGROUPS.GreensXClassesOfClass(C, GreensRRelation,
                                          GreensRClassOfElement);
end);

InstallMethod(GreensHClasses, "for a Green's class of an enumerable semigroup",
[IsGreensClass and IsEnumerableSemigroupGreensClassRep],
function(C)
  if not (IsGreensRClass(C) or IsGreensLClass(C) or IsGreensDClass(C)) then
    ErrorNoReturn("Semigroups: GreensHClasses ",
                  "(for an enumerable semigroup Green's class): usage,\n",
                  "the argument should be a Green's R-, L-, or D-class,");
  fi;
  return SEMIGROUPS.GreensXClassesOfClass(C, GreensHRelation,
                                          GreensHClassOfElement);
end);

## Representatives

InstallMethod(DClassReps, "for an enumerable semigroup",
[IsEnumerableSemigroupRep], S -> SEMIGROUPS.XClassReps(S, GreensDRelation));

InstallMethod(RClassReps, "for an enumerable semigroup",
[IsEnumerableSemigroupRep], S -> SEMIGROUPS.XClassReps(S, GreensRRelation));

InstallMethod(LClassReps, "for an enumerable semigroup",
[IsEnumerableSemigroupRep], S -> SEMIGROUPS.XClassReps(S, GreensLRelation));

InstallMethod(HClassReps, "for an enumerable semigroup",
[IsEnumerableSemigroupRep], S -> SEMIGROUPS.XClassReps(S, GreensHRelation));

InstallMethod(RClassReps, "for a Green's D-class of an enumerable semigroup",
[IsGreensDClass and IsEnumerableSemigroupGreensClassRep],
D -> SEMIGROUPS.XClassRepsOfClass(D, GreensRRelation));

InstallMethod(LClassReps, "for a Green's D-class of an enumerable semigroup",
[IsGreensDClass and IsEnumerableSemigroupGreensClassRep],
D -> SEMIGROUPS.XClassRepsOfClass(D, GreensLRelation));

InstallMethod(HClassReps, "for a Green's class of an enumerable semigroup",
[IsGreensClass and IsEnumerableSemigroupGreensClassRep],
C -> SEMIGROUPS.XClassRepsOfClass(C, GreensHRelation));

## Partial order of D-classes
# There is duplicate code in here and in maximal D-classes

InstallMethod(PartialOrderOfDClasses, "for a finite enumerable semigroup",
[IsEnumerableSemigroupRep and IsFinite],
function(S)
  local l, r, gr;

  l  := LeftCayleyGraphSemigroup(S);
  r  := RightCayleyGraphSemigroup(S);
  gr := Digraph(List([1 .. Length(l)], i -> Concatenation(l[i], r[i])));
  gr := QuotientDigraph(gr, GreensDRelation(S)!.data.comps);

  return List(OutNeighbours(gr), Set);
end);

#############################################################################
## 6. Idempotents . . .
#############################################################################

InstallMethod(NrIdempotents, "for an enumerable semigroup Green's class",
[IsEnumerableSemigroupGreensClassRep],
function(C)
  local rel, pos;
  rel := EquivalenceClassRelation(C);
  pos := EN_SEMI_IDEMS_SUBSET(Range(rel),
                              rel!.data.comps[SEMIGROUPS.XClassIndex(C)]);
  return Length(pos);
end);

InstallMethod(Idempotents, "for an enumerable semigroup Green's class",
[IsEnumerableSemigroupGreensClassRep],
function(C)
  local rel, pos;

  rel := EquivalenceClassRelation(C);
  pos := EN_SEMI_IDEMS_SUBSET(Range(rel),
                              rel!.data.comps[SEMIGROUPS.XClassIndex(C)]);
  return EnumeratorCanonical(Range(rel)){pos};
end);
