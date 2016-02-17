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
                 [IsInverseSemigroup, IsMultiplicativeElement]);
DeclareAttribute("JoinIrreducibleDClasses", IsInverseSemigroup);
DeclareOperation("IsMajorantlyClosed",
                 [IsSemigroupWithInverseOp, IsMultiplicativeElementCollection]);
DeclareOperation("IsMajorantlyClosedNC",
                 [IsSemigroupWithInverseOp, IsMultiplicativeElementCollection]);
DeclareOperation("MajorantClosure", [IsInverseSemigroup, IsCollection]);
DeclareOperation("MajorantClosureNC",
                 [IsInverseSemigroup, IsMultiplicativeElementCollection]);
DeclareOperation("Minorants", [IsInverseSemigroup, IsMultiplicativeElement]);
DeclareAttribute("SameMinorantsSubgroup", IsGroupHClass);
DeclareOperation("RightCosetsOfInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsSemigroupWithInverseOp]);
DeclareAttribute("PrimitiveIdempotents", IsSemigroup);
DeclareGlobalFunction("SupremumIdempotentsNC");
DeclareAttribute("VagnerPrestonRepresentation", IsSemigroupWithInverseOp);
DeclareAttribute("NaturalLeqInverseSemigroup", IsSemigroup);
