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

DeclareOperation("IsJoinIrreducible", [IsInverseSemigroup, IsAssociativeElement]);
DeclareOperation("IsMajorantlyClosed", [IsInverseSemigroup, IsCollection]);
DeclareOperation("IsMajorantlyClosedNC", [IsInverseSemigroup, IsAssociativeElementCollection]);

DeclareAttribute("JoinIrreducibleDClasses", IsInverseSemigroup);

DeclareOperation("MajorantClosure", [IsInverseSemigroup, IsCollection]);
DeclareOperation("MajorantClosureNC", [IsInverseSemigroup, IsAssociativeElementCollection]);
DeclareOperation("Minorants", [IsInverseSemigroup, IsAssociativeElement]);

DeclareOperation("RightCosetsOfInverseSemigroup", [IsActingSemigroupWithInverseOp, IsActingSemigroupWithInverseOp]);

DeclareAttribute("SameMinorantsSubgroup", IsGroupHClass);
DeclareAttribute("SmallerDegreePartialPermRepresentation", IsInverseSemigroup and
IsPartialPermSemigroup);
DeclareGlobalFunction("SupremumIdempotentsNC");

DeclareAttribute("VagnerPrestonRepresentation", IsActingSemigroupWithInverseOp);

DeclareOperation("NaturalLeqInverseSemigroup", [IsAssociativeElement, IsAssociativeElement]);