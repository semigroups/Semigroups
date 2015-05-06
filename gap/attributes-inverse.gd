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
DeclareAttribute("JoinIrreducibleDClasses",
                 IsInverseSemigroup);
DeclareOperation("IsMajorantlyClosed",
                 [IsInverseSemigroup, IsAssociativeElementCollection]);
DeclareOperation("IsMajorantlyClosed",
                 [IsInverseSemigroup, IsSemigroup]);
DeclareOperation("IsMajorantlyClosedNC",
                 [IsInverseSemigroup, IsAssociativeElementCollection]);
DeclareOperation("MajorantClosure", [IsInverseSemigroup,
                 IsCollection]);
DeclareOperation("MajorantClosureNC", [IsInverseSemigroup,
                 IsAssociativeElementCollection]);
DeclareOperation("Minorants", [IsInverseSemigroup, IsAssociativeElement]);
DeclareAttribute("SameMinorantsSubgroup", IsGroupHClass);
DeclareOperation("RightCosetsOfInverseSemigroup",
                 [IsInverseSemigroup and IsSemigroupWithInverseOp,
                  IsInverseSemigroup and IsSemigroupWithInverseOp]);
DeclareAttribute("PrimitiveIdempotents", IsInverseSemigroup and
                 IsSemigroupWithInverseOp and IsActingSemigroup);
DeclareGlobalFunction("SupremumIdempotentsNC");
DeclareAttribute("VagnerPrestonRepresentation",
                 IsInverseSemigroup and IsSemigroupWithInverseOp);
DeclareOperation("NaturalLeqInverseSemigroup",
                 [IsAssociativeElement, IsAssociativeElement]);
