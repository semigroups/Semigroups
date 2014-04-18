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
DeclareOperation("IsMajorantlyClosed", [IsActingSemigroupWithInverseOp,
  IsAssociativeElementCollection]);
DeclareOperation("IsMajorantlyClosed", [IsActingSemigroupWithInverseOp,
  IsActingSemigroup]);
DeclareOperation("IsMajorantlyClosedNC", 
  [IsActingSemigroupWithInverseOp, IsAssociativeElementCollection]);
DeclareOperation("MajorantClosure", [IsInverseSemigroup, IsCollection]);
DeclareOperation("MajorantClosureNC", 
  [IsInverseSemigroup, IsAssociativeElementCollection]);
DeclareOperation("Minorants", [IsInverseSemigroup, IsAssociativeElement]);
DeclareAttribute("SameMinorantsSubgroup", 
  IsGroupHClass and IsInverseOpClass and IsActingSemigroupGreensClass);

DeclareOperation("RightCosetsOfInverseSemigroup", 
  [IsActingSemigroupWithInverseOp, IsActingSemigroupWithInverseOp]);
DeclareAttribute("SmallerDegreePartialPermRepresentation", 
  IsInverseSemigroup and IsPartialPermSemigroup);
DeclareGlobalFunction("SupremumIdempotentsNC");

DeclareAttribute("VagnerPrestonRepresentation", IsActingSemigroupWithInverseOp);
DeclareOperation("NaturalLeqInverseSemigroup", 
  [IsAssociativeElement, IsAssociativeElement]);
