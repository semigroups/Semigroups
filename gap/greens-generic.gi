#############################################################################
##
#W  greens-generic.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

## This file contains methods for Green's classes/relations for generic
## semigroups.

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

InstallMethod(\=, "for Green's classes",
[IsGreensClass, IsGreensClass],
function(x, y)
  if (IsGreensRClass(x) and IsGreensRClass(y))
      or (IsGreensLClass(x) and IsGreensLClass(y))
      or (IsGreensDClass(x) and IsGreensDClass(y))
      or (IsGreensHClass(x) and IsGreensHClass(y)) then
    return Parent(x) = Parent(y) and Representative(x) in y;
  fi;
  return Parent(x) = Parent(y) and Representative(x) in y and
   Size(x) = Size(y);
end);

#

InstallMethod(\<, "for Green's classes",
[IsGreensClass, IsGreensClass],
function(x, y)
  if (IsGreensRClass(x) and IsGreensRClass(y))
      or (IsGreensLClass(x) and IsGreensLClass(y))
      or (IsGreensDClass(x) and IsGreensDClass(y))
      or (IsGreensHClass(x) and IsGreensHClass(y)) then
    return Parent(x) = Parent(y) and Representative(x) <
     Representative(y) and (not Representative(x) in y);
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

# H-classes . . .

#

InstallMethod(MultiplicativeNeutralElement,
"for a H-class of a semigroup", [IsGreensHClass],
function(H)
  if not IsGroupHClass(H) then
    return fail;
  fi;
  return Idempotents(H)[1];
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

#

InstallMethod(StructureDescription, "for a Green's H-class",
[IsGreensHClass],
function(H)
  if not IsGroupHClass(H) then
    return fail;
  fi;
  return StructureDescription(Range(IsomorphismPermGroup(H)));
end);
