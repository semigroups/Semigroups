#############################################################################
##
#W  greens-generic.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# TODO clean this up!!

# This file contains methods for Green's relations and classes of semigroups
# that satisfy IsSemigroup.

# Green's classes: the idea is to only store the index of the equivalence class
# corresponding to the Green's class, then look everything up in the data. The
# equivalence class data structures for R-, L-, H-, D-classes of a finite
# semigroup <S> are stored in the !.data component of the corresponding Green's
# relation.

InstallMethod(IsRegularClass, "for a Green's class of a semigroup",
[IsGreensClass], D -> First(Enumerator(D), x -> IsIdempotent(x)) <> fail);

InstallTrueMethod(IsRegularClass, IsRegularDClass);
InstallTrueMethod(IsRegularClass, IsInverseOpClass);
InstallTrueMethod(IsHClassOfRegularSemigroup,
                  IsInverseOpClass and IsGreensHClass);

# Semigroups . . .

InstallMethod(NrDClasses, "for a semigroup",
[IsSemigroup], S -> Length(GreensDClasses(S)));

# TODO improve the method below, using a similar method to the one for
# NrRegularDClasses (WW: this doesn't exists)

# WW NrRegularDClasses doesn't exist in this file so I created this
InstallMethod(NrRegularDClasses, "for a semigroup",
[IsSemigroup], S -> Length(RegularDClasses(S)));

InstallMethod(RegularDClasses, "for a semigroup",
[IsSemigroup], S -> Filtered(GreensDClasses(S), IsRegularDClass));

InstallMethod(NrLClasses, "for a semigroup",
[IsSemigroup], S -> Length(GreensLClasses(S)));

InstallMethod(NrRClasses, "for a semigroup",
[IsSemigroup], S -> Length(GreensRClasses(S)));

InstallMethod(NrIdempotents, "for a semigroup",
[IsSemigroup], S -> Length(Idempotents(S)));

# Green's classes . . .

InstallMethod(EquivalenceClassOfElement,
"for Green's D-relation and associative element",
[IsGreensDRelation, IsAssociativeElement],
function(D, x)
  return GreensDClassOfElement(UnderlyingDomainOfBinaryRelation(D), x);
end);

InstallMethod(EquivalenceClassOfElement,
"for Green's R-relation and associative element",
[IsGreensRRelation, IsAssociativeElement],
function(R, x)
  return GreensRClassOfElement(UnderlyingDomainOfBinaryRelation(R), x);
end);

InstallMethod(EquivalenceClassOfElement,
"for Green's L-relation and associative element",
[IsGreensLRelation, IsAssociativeElement],
function(L, x)
  return GreensLClassOfElement(UnderlyingDomainOfBinaryRelation(L), x);
end);

InstallMethod(EquivalenceClassOfElement,
"for Green's H-relation and associative element",
[IsGreensHRelation, IsAssociativeElement],
function(H, x)
  return GreensHClassOfElement(UnderlyingDomainOfBinaryRelation(H), x);
end);

InstallMethod(GreensRClassOfElement,
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement],
function(S, x)
  return EquivalenceClassOfElement(GreensRRelation(S), x);
end);

InstallMethod(GreensLClassOfElement,
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement],
function(S, x)
  return EquivalenceClassOfElement(GreensLRelation(S), x);
end);

InstallMethod(GreensHClassOfElement,
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement],
function(S, x)
  return EquivalenceClassOfElement(GreensHRelation(S), x);
end);

InstallMethod(GreensDClassOfElement,
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement],
function(S, x)
  return EquivalenceClassOfElement(GreensDRelation(S), x);
end);

# same method for regular/inverse

InstallMethod(GreensJClassOfElement,
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement], GreensDClassOfElement);

#

InstallMethod(GreensRClassOfElementNC,
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement],
function(S, x)
  return EquivalenceClassOfElementNC(GreensRRelation(S), x);
end);

InstallMethod(GreensLClassOfElementNC,
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement],
function(S, x)
  return EquivalenceClassOfElementNC(GreensLRelation(S), x);
end);

InstallMethod(GreensHClassOfElementNC,
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement],
function(S, x)
  return EquivalenceClassOfElementNC(GreensHRelation(S), x);
end);

InstallMethod(GreensDClassOfElementNC,
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement],
function(S, x)
  return EquivalenceClassOfElementNC(GreensDRelation(S), x);
end);

# same method for regular/inverse

InstallMethod(GreensJClassOfElementNC,
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement], GreensDClassOfElementNC);

# this should be removed after the library method for AsSSortedList
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)! JDM

InstallMethod(AsSSortedList, "for a Green's class",
[IsGreensClass], C -> ConstantTimeAccessList(EnumeratorSorted(C)));

InstallMethod(\=, "for Green's classes",
[IsGreensClass, IsGreensClass],
function(x, y)
  if   (IsGreensRClass(x) and IsGreensRClass(y))
    or (IsGreensLClass(x) and IsGreensLClass(y))
    or (IsGreensDClass(x) and IsGreensDClass(y))
    or (IsGreensHClass(x) and IsGreensHClass(y)) then
    return Parent(x) = Parent(y) and Representative(x) in y;
  fi;
  return Parent(x) = Parent(y) and Representative(x) in y and
   Size(x) = Size(y);
end);

#
InstallMethod(NrHClasses, "for a semigroup",
[IsSemigroup], S -> Length(GreensHClasses(S)));

#

InstallMethod(NrHClasses, "for a Green's D-class",
[IsGreensDClass], D -> NrRClasses(D) * NrLClasses(D));

#

InstallMethod(NrHClasses, "for a Green's L-class",
[IsGreensLClass], L -> NrRClasses(DClassOfLClass(L)));

#

InstallMethod(NrHClasses, "for a Green's R-class",
[IsGreensRClass], R -> NrLClasses(DClassOfRClass(R)));

InstallMethod(NrDClasses, "for a finite semigroup", [IsSemigroup and IsFinite],
S -> Length(GreensDClasses(S)));

InstallMethod(NrRClasses, "for a Green's D-class",
[IsGreensDClass], D -> Length(GreensRClasses(D)));

InstallMethod(NrLClasses, "for a Green's D-class",
[IsGreensDClass], D -> Length(GreensLClasses(D)));

InstallMethod(NrHClasses, "for a Green's class",
[IsGreensClass], C -> Length(GreensHClasses(C)));

# technical
#JDM: is this necessary? I.e. is there a similar method in the library?

InstallMethod(\=, "for Green's classes",
[IsGreensClass, IsGreensClass],
function(x, y)
  if (IsGreensRClass(x) and IsGreensRClass(y))
      or (IsGreensLClass(x) and IsGreensLClass(y))
      or (IsGreensDClass(x) and IsGreensDClass(y))
      or (IsGreensHClass(x) and IsGreensHClass(y)) then
    return Parent(x) = Parent(y) and Representative(x) in y;
  fi;
  return Parent(x) = Parent(y)
         and Representative(x) in y
         and Size(x) = Size(y);
end);

#JDM: is this necessary? I.e. is there a similar method in the library?

InstallMethod(\<, "for Green's classes",
[IsGreensClass, IsGreensClass],
function(x, y)
  if   (IsGreensRClass(x) and IsGreensRClass(y))
    or (IsGreensLClass(x) and IsGreensLClass(y))
    or (IsGreensDClass(x) and IsGreensDClass(y))
    or (IsGreensHClass(x) and IsGreensHClass(y)) then
    return Parent(x) = Parent(y)
           and Representative(x) < Representative(y)
           and (not Representative(x) in y);
  fi;
  return false;
end);

# D-classes . . .

# FIXME are these really necessary?

InstallMethod(DClass, "for an R-class", [IsGreensRClass], DClassOfRClass);
InstallMethod(DClass, "for an L-class", [IsGreensLClass], DClassOfLClass);
InstallMethod(DClass, "for an H-class", [IsGreensHClass], DClassOfHClass);
InstallMethod(LClass, "for an H-class", [IsGreensHClass], LClassOfHClass);
InstallMethod(RClass, "for an H-class", [IsGreensHClass], RClassOfHClass);

InstallMethod(IsRegularDClass, "for a D-class of a semigroup",
[IsGreensDClass], IsRegularClass);

# H-classes . . .

InstallMethod(MultiplicativeNeutralElement,
"for a H-class of a semigroup", [IsGreensHClass],
function(H)
  if not IsGroupHClass(H) then
    return fail;
  fi;
  return Idempotents(H)[1];
end);

#

InstallMethod(StructureDescription, "for a Green's H-class",
[IsGreensHClass],
function(H)
  if not IsGroupHClass(H) then
    return fail;
  fi;
  return StructureDescription(Range(IsomorphismPermGroup(H)));
end);

# same method for ideals

InstallMethod(GreensRRelation, "for an generic semigroup",
[IsSemigroup],
function(S)
  local fam, rel, data;

  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));

  rel := Objectify(NewType(fam,
                           IsEquivalenceRelation
                             and IsEquivalenceRelationDefaultRep
                             and IsGreensRRelation),
                           rec());
  SetSource(rel, S);
  SetRange(rel, S);
  SetIsLeftSemigroupCongruence(rel, true);

  if HasIsFinite(S) and IsFinite(S) then
    SetIsFiniteSemigroupGreensRelation(rel, true);
  fi;
  if not IsActingSemigroup(S) then
    rel!.data := SEMIGROUPS_GABOW_SCC(RightCayleyGraphSemigroup(S));
    #TODO change the other GreensXRelations to that they look like this!
  fi;

  return rel;
end);

# same method for ideals

InstallMethod(GreensLRelation, "for an generic semigroup",
[IsSemigroup],
function(S)
  local fam, rel, data;

  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
          ElementsFamily(FamilyObj(S)));

  rel := Objectify(NewType(fam,
                           IsEquivalenceRelation
                             and IsEquivalenceRelationDefaultRep
                             and IsGreensLRelation),
                           rec());
  SetSource(rel, S);
  SetRange(rel, S);
  SetIsRightSemigroupCongruence(rel, true);

  if HasIsFinite(S) and IsFinite(S) then
    SetIsFiniteSemigroupGreensRelation(rel, true);
  fi;
  if not IsActingSemigroup(S) then
    data := Enumerate(GenericSemigroupData(S));
    rel!.data := SEMIGROUPS_GABOW_SCC(LeftCayleyGraphSemigroup(S));
  fi;
  return rel;
end);

# same method for ideals

InstallMethod(GreensJRelation, "for an generic semigroup",
[IsSemigroup], GreensDRelation);

# same method for ideals

InstallMethod(GreensDRelation, "for an generic semigroup",
[IsSemigroup],
function(S)
  local fam, rel;

  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));

  rel := Objectify(NewType(fam,
                           IsEquivalenceRelation
                             and IsEquivalenceRelationDefaultRep
                             and IsGreensDRelation),
                           rec());
  SetSource(rel, S);
  SetRange(rel, S);

  if HasIsFinite(S) and IsFinite(S) then
    SetIsFiniteSemigroupGreensRelation(rel, true);
  fi;
  if not IsActingSemigroup(S) then
    rel!.data:=SCC_UNION_LEFT_RIGHT_CAYLEY_GRAPHS(GreensRRelation(S)!.data,
                                                  GreensLRelation(S)!.data);
  fi;

  return rel;
end);

# same method for ideals

InstallMethod(GreensHRelation, "for an generic semigroup",
[IsSemigroup],
function(S)
    local fam, rel;

  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));

  rel := Objectify(NewType(fam, IsEquivalenceRelation
                                and IsEquivalenceRelationDefaultRep
                                and IsGreensHRelation), rec());

  SetSource(rel, S);
  SetRange(rel, S);

  if HasIsFinite(S) and IsFinite(S) then
    SetIsFiniteSemigroupGreensRelation(rel, true);
  fi;
  if not IsActingSemigroup(S) then
    rel!.data:=FIND_HCLASSES(GreensRRelation(S)!.data, GreensLRelation(S)!.data);
  fi;
  return rel;
end);

# TODO make this a proper enumerator method?!

InstallMethod(Enumerator, "for an generic semigroup Green's class",
[IsGreensClass],
function(C)
  local data, rel;
  data:=GenericSemigroupData(Parent(C));
  rel:=EquivalenceClassRelation(C);
  return data!.elts{rel!.data.comps[C!.index]};
end);

#

InstallMethod(Size, "for an generic semigroup Green's class",
[IsGreensClass],
function(C)
  return Length(EquivalenceClassRelation(C)!.data.comps[C!.index]);
end);

#

InstallMethod(\in,
"for an associative element and an generic semigroup Green's class",
[IsAssociativeElement, IsGreensClass],
function(x, C)
  local pos;

  pos:=Position(GenericSemigroupData(Parent(C)), x);
  return pos<>fail and EquivalenceClassRelation(C)!.data.id[pos]=C!.index;
end);

# there are not methods for EquivalenceClassOfElementNC in this case since we
# require the variable <pos> below, and if it can't be determined, then we can't
# make the Green's class.

BindGlobal("SEMIGROUPS_EquivalenceClassOfElement",
function(rel, rep, type)
  local pos, out, S;

  pos := Position(GenericSemigroupData(Source(rel)), rep);
  if pos = fail then
    Error("usage: the element in the 2nd argument does not belong to the ",
          "semigroup,");
    return;
  fi;

  out := rec();
  S := Source(rel);
  ObjectifyWithAttributes(out, type(S), EquivalenceClassRelation, rel,
                          Representative, rep, ParentAttr, S);

  out!.index:=rel!.data.id[pos];

  return out;
end);

#

InstallMethod(EquivalenceClassOfElement,
"for an generic semigroup Green's R-relation and an associative element",
[IsGreensRRelation, IsAssociativeElement],
function(rel, rep)
  return SEMIGROUPS_EquivalenceClassOfElement(rel, rep, RClassType);
end);

InstallMethod(EquivalenceClassOfElement,
"for an generic semigroup Green's L-relation and an associative element",
[IsGreensLRelation, IsAssociativeElement],
function(rel, rep)
  return SEMIGROUPS_EquivalenceClassOfElement(rel, rep, LClassType);
end);

InstallMethod(EquivalenceClassOfElement,
"for an generic semigroup Green's H-relation and an associative element",
[IsGreensHRelation, IsAssociativeElement],
function(rel, rep)
  return SEMIGROUPS_EquivalenceClassOfElement(rel, rep, HClassType);
end);

InstallMethod(EquivalenceClassOfElement,
"for an generic semigroup Green's D-relation and an associative element",
[IsGreensDRelation, IsAssociativeElement],
function(rel, rep)
  return SEMIGROUPS_EquivalenceClassOfElement(rel, rep, DClassType);
end);

#

BindGlobal("SEMIGROUPS_GreensXClasses",
function(S, GreensXRelation, GreensXClassOfElement)
  local comps, elts, out, C, i;

  comps:=GreensXRelation(S)!.data.comps;
  elts:=GenericSemigroupData(S)!.elts;
  out:=EmptyPlist(Length(comps));

  for i in [1..Length(comps)] do
    C:=GreensXClassOfElement(S, elts[comps[i][1]]);
    C!.index:=i;
    out[i]:=C;
  od;
  return out;
end);

# same method for ideals

InstallMethod(GreensLClasses, "for an generic semigroup",
[IsSemigroup],
function(S)
  return SEMIGROUPS_GreensXClasses(S, GreensLRelation, GreensLClassOfElement);
end);

# same method for ideals

InstallMethod(GreensRClasses, "for an generic semigroup",
[IsSemigroup],
function(S)
  return SEMIGROUPS_GreensXClasses(S, GreensRRelation, GreensRClassOfElement);
end);

# same method for ideals

InstallMethod(GreensHClasses, "for an generic semigroup",
[IsSemigroup],
function(S)
  return SEMIGROUPS_GreensXClasses(S, GreensHRelation, GreensHClassOfElement);
end);

# same method for ideals

InstallMethod(GreensDClasses, "for an generic semigroup",
[IsSemigroup],
function(S)
  return SEMIGROUPS_GreensXClasses(S, GreensDRelation, GreensDClassOfElement);
end);

#

BindGlobal("SEMIGROUPS_GreensXClassesOfClass",
function(C, GreensXRelation, GreensXClassOfElement)
  local S, comp, id, seen, elts, out, i;

  S:=Parent(C);
  comp:=EquivalenceClassRelation(C)!.data.comps[C!.index];
  id:=GreensXRelation(Parent(C))!.data.id;
  seen:=BlistList([1..Length(id)], []);
  elts:=GenericSemigroupData(S)!.elts;
  out:=EmptyPlist(Length(comp));

  for i in comp do
    if not seen[id[i]] then
      seen[id[i]]:=true;
      C:=GreensXClassOfElement(S, elts[i]);
      C!.index:=id[i];
      Add(out, C);
    fi;
  od;

  return out;
end);

#

InstallMethod(GreensLClasses, "for a Green's D-class",
[IsGreensDClass and IsGreensClass],
function(C)
  return SEMIGROUPS_GreensXClassesOfClass(C, GreensLRelation, GreensLClassOfElement);
end);

InstallMethod(GreensRClasses, "for a Green's D-class",
[IsGreensDClass and IsGreensClass],
function(C)
  return SEMIGROUPS_GreensXClassesOfClass(C, GreensRRelation, GreensRClassOfElement);
end);

InstallMethod(GreensHClasses, "for an generic semigroup Green's class",
[IsGreensClass],
function(C)
  if IsGreensRClass(C) or IsGreensLClass(C) or IsGreensDClass(C) then
    return SEMIGROUPS_GreensXClassesOfClass(C, GreensHRelation, GreensHClassOfElement);
  fi;
  Error("usage: the argument should be a Greens R-, L-, or D-class,");
  return;
end);

# Methods for things declared in the Semigroups package but not in the GAP
# library

# same method for ideals

InstallMethod(NrRClasses, "for an generic semigroup",
[IsSemigroup],
function(S)
  return Length(GreensRRelation(S)!.data.comps);
end);

# same method for ideals

InstallMethod(NrLClasses, "for an generic semigroup",
[IsSemigroup],
function(S)
  return Length(GreensLRelation(S)!.data.comps);
end);

# same method for ideals

InstallMethod(NrDClasses, "for an generic semigroup",
[IsSemigroup],
function(S)
  return Length(GreensDRelation(S)!.data.comps);
end);

# same method for ideals

InstallMethod(NrHClasses, "for an generic semigroup",
[IsSemigroup],
function(S)
  return Length(GreensHRelation(S)!.data.comps);
end);

#

InstallMethod(DClassType, "for an generic semigroup",
[IsSemigroup],
function(S)
  return NewType( FamilyObj(S), IsEquivalenceClass and
          IsEquivalenceClassDefaultRep and IsGreensDClass);
end);

InstallMethod(HClassType, "for an generic semigroup",
[IsSemigroup],
function(S)
 return NewType( FamilyObj(S), IsEquivalenceClass and
  IsEquivalenceClassDefaultRep and IsGreensHClass);
end);

InstallMethod(LClassType, "for a generic semigroup",
[IsSemigroup],
function(S)
  return NewType( FamilyObj(S), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensLClass);
end);

InstallMethod(RClassType, "for a generic semigroup",
[IsSemigroup],
function(S)
  return NewType( FamilyObj(S), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensRClass);
end);
