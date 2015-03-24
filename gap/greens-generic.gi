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
# NrRegularDClasses

InstallMethod(RegularDClasses, "for a semigroup",
[IsSemigroup], S -> Filtered(GreensDClasses(S), IsRegularClass));

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
  return EquivalenceClassOfElement( GreensRRelation(S), x );
end);

InstallMethod(GreensLClassOfElement, 
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement],
function(S, x)
  return EquivalenceClassOfElement( GreensLRelation(S), x );
end);

InstallMethod(GreensHClassOfElement, 
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement],
function(S, x)
  return EquivalenceClassOfElement( GreensHRelation(S), x );
end);

InstallMethod(GreensDClassOfElement, 
"for a finite semigroup and associative element", 
[IsSemigroup and IsFinite, IsAssociativeElement],
function(S, x)
  return EquivalenceClassOfElement( GreensDRelation(S), x );
end);

# same method for regular/inverse

InstallMethod(GreensJClassOfElement, "for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement], GreensDClassOfElement);

#

InstallMethod(GreensRClassOfElementNC, 
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement],
function(S, x)
  return EquivalenceClassOfElementNC( GreensRRelation(S), x );
end);

InstallMethod(GreensLClassOfElementNC, 
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement],
function(S, x)
  return EquivalenceClassOfElementNC( GreensLRelation(S), x );
end);

InstallMethod(GreensHClassOfElementNC, 
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement],
function(S, x)
  return EquivalenceClassOfElementNC( GreensHRelation(S), x );
end);

InstallMethod(GreensDClassOfElementNC, 
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement],
function(S, x)
  return EquivalenceClassOfElementNC( GreensDRelation(S), x );
end);

# same method for regular/inverse

InstallMethod(GreensJClassOfElementNC, 
"for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement], GreensDClassOfElementNC);

# this should be removed after the library method for AsSSortedList 
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)! JDM

InstallMethod(AsSSortedList, "for a Green's class",
[IsGreensClass], 
function(C)
  return ConstantTimeAccessList(EnumeratorSorted(C));
end);

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
function(S)
  return Length(GreensDClasses(S));
end);

InstallMethod(NrRClasses, "for a Green's D-class",
[IsGreensDClass],
function(D)
  return Length(GreensRClasses(D));
end);

InstallMethod(NrLClasses, "for a Green's D-class",
[IsGreensDClass],
function(D)
  return Length(GreensLClasses(D));
end);

InstallMethod(NrHClasses, "for a Green's class",
[IsGreensClass],
function(C)
  return Length(GreensHClasses(C));
end);

# technical
#JDM: is this necessary? I.e. is there a similar method in the library? 

InstallMethod(\=, "for Green's classes",
[IsGreensClass, IsGreensClass],
function(x, y)
  if (IsGreensRClass(x) and IsGreensRClass(y)) or
   (IsGreensLClass(x) and IsGreensLClass(y)) or
   (IsGreensDClass(x) and IsGreensDClass(y)) or
   (IsGreensHClass(x) and IsGreensHClass(y)) then
    return Parent(x)=Parent(y) and Representative(x) in y;
  fi;
  return Parent(x)=Parent(y) and Representative(x) in y and
   Size(x)=Size(y);
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

#EOF
