#############################################################################
##
#W  attributes-inverse.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell,
##                                                       Wilf Wilson,
##                                                       Rhiannon Dougall,
##                                                       Robert Hancock
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("CharacterTableOfInverseSemigroup", IsInverseSemigroup and 
IsPartialPermSemigroup);

DeclareOperation("IsJoinIrreducible", [IsInverseSemigroup, IsAssociativeElement]);
DeclareAttribute("JoinIrreducibleDClasses", IsInverseSemigroup);
DeclareOperation("IsMajorantlyClosed", [IsNonExhaustiveSemigroupWithInverseOp,
  IsAssociativeElementCollection]);
DeclareOperation("IsMajorantlyClosed", [IsNonExhaustiveSemigroupWithInverseOp,
  IsNonExhaustiveSemigroup]);
DeclareOperation("IsMajorantlyClosedNC", 
  [IsNonExhaustiveSemigroupWithInverseOp, IsAssociativeElementCollection]);
DeclareOperation("MajorantClosure", [IsInverseSemigroup, IsCollection]);
DeclareOperation("MajorantClosureNC", 
  [IsInverseSemigroup, IsAssociativeElementCollection]);
DeclareOperation("Minorants", [IsInverseSemigroup, IsAssociativeElement]);
DeclareAttribute("SameMinorantsSubgroup", 
  IsGroupHClass and IsInverseOpClass and IsNonExhaustiveSemigroupGreensClass);

DeclareOperation("RightCosetsOfInverseSemigroup", 
  [IsNonExhaustiveSemigroupWithInverseOp, IsNonExhaustiveSemigroupWithInverseOp]);
DeclareAttribute("SmallerDegreePartialPermRepresentation", 
  IsInverseSemigroup and IsPartialPermSemigroup);
DeclareGlobalFunction("SupremumIdempotentsNC");

DeclareAttribute("VagnerPrestonRepresentation", IsNonExhaustiveSemigroupWithInverseOp);
DeclareOperation("NaturalLeqInverseSemigroup", 
  [IsAssociativeElement, IsAssociativeElement]);
