#############################################################################
##
#W  greens.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# this should be removed after the library method for AsSSortedList 
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)! JDM

InstallMethod(AsSSortedList, "for a Green's class of a semigroup",
[IsGreensClass], 
function(C)
  return ConstantTimeAccessList(EnumeratorSorted(C));
end);

#

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

# Notes:
# - D-class reps must have rectified lambda and rho value

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
"for Green's D-relation and associative element", 
[IsGreensDRelation, IsAssociativeElement],
function(D, x)
  return GreensDClassOfElement(UnderlyingDomainOfBinaryRelation(D), x);
end);

InstallMethod(EquivalenceClassOfElement, 
"for Green's H-relation and associative element", 
[IsGreensHRelation, IsAssociativeElement],
function(H, x)
  return GreensHClassOfElement(UnderlyingDomainOfBinaryRelation(H), x);
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
  if (IsGreensRClass(x) and IsGreensRClass(y)) or
   (IsGreensLClass(x) and IsGreensLClass(y)) or
   (IsGreensDClass(x) and IsGreensDClass(y)) or
   (IsGreensHClass(x) and IsGreensHClass(y)) then
    return Parent(x)=Parent(y) and Representative(x) <
     Representative(y) and (not Representative(x) in y);
  fi;
  return fail;
end);

#

InstallMethod(DClass, "for an R-class", [IsGreensRClass], DClassOfRClass);
InstallMethod(DClass, "for an L-class", [IsGreensLClass], DClassOfLClass);
InstallMethod(DClass, "for an H-class", [IsGreensHClass], DClassOfHClass);
InstallMethod(LClass, "for an H-class", [IsGreensHClass], LClassOfHClass);
InstallMethod(RClass, "for an H-class", [IsGreensHClass], RClassOfHClass);

# same method for regular/inverse

InstallMethod(GreensJClassOfElement, "for a finite semigroup and associative element",
[IsSemigroup and IsFinite, IsAssociativeElement], GreensDClassOfElement);

InstallMethod(IsRegularDClass, "for a D-class of a semigroup",
[IsGreensDClass], IsRegularClass);

#EOF
