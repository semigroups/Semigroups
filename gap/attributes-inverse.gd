#############################################################################
##
#W  attributes-inverse.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell,
##                                                       Wilf Wilson,
##                                                       Rhiannon Dougall,
##                                                       Robert Hancock
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("CharacterTableOfInverseSemigroup",
                 IsInverseSemigroup and IsPartialPermSemigroup
                 and IsActingSemigroup);
DeclareOperation("IsJoinIrreducible",
                 [IsInverseSemigroup, IsAssociativeElement]);
DeclareAttribute("JoinIrreducibleDClasses", IsInverseSemigroup);
DeclareOperation("IsMajorantlyClosed",
                 [IsSemigroupWithInverseOp, IsAssociativeElementCollection]);
DeclareOperation("IsMajorantlyClosed",
                 [IsSemigroupWithInverseOp, IsSemigroupWithInverseOp]);
DeclareOperation("IsMajorantlyClosedNC",
                 [IsSemigroupWithInverseOp, IsAssociativeElementCollection]);
DeclareOperation("MajorantClosure", [IsInverseSemigroup, IsCollection]);
DeclareOperation("MajorantClosureNC",
                 [IsInverseSemigroup, IsAssociativeElementCollection]);
DeclareOperation("Minorants", [IsInverseSemigroup, IsAssociativeElement]);
DeclareAttribute("SameMinorantsSubgroup", IsGroupHClass);
DeclareOperation("RightCosetsOfInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsSemigroupWithInverseOp]);
DeclareAttribute("SmallerDegreePartialPermRepresentation",
                 IsInverseSemigroup and IsPartialPermSemigroup);
DeclareAttribute("PrimitiveIdempotents", IsInverseSemigroup);
DeclareGlobalFunction("SupremumIdempotentsNC");
DeclareAttribute("VagnerPrestonRepresentation", IsSemigroupWithInverseOp); 
DeclareOperation("NaturalLeqInverseSemigroup",
                 [IsAssociativeElement, IsAssociativeElement]);
