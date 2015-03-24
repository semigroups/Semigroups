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

DeclareAttribute("CharacterTableOfInverseSemigroup", IsInverseSemigroup and
IsPartialPermSemigroup);

DeclareOperation("IsJoinIrreducible", [IsInverseSemigroup,
IsAssociativeElement]);
DeclareAttribute("JoinIrreducibleDClasses", IsInverseSemigroup);
DeclareOperation("IsMajorantlyClosed", [IsSemigroupWithInverseOp and
                                        IsActingSemigroup,
                                        IsAssociativeElementCollection]);
DeclareOperation("IsMajorantlyClosed", [IsSemigroupWithInverseOp and
                                        IsActingSemigroup, IsActingSemigroup]);
DeclareOperation("IsMajorantlyClosedNC",
                 [IsSemigroupWithInverseOp and IsActingSemigroup,
                  IsAssociativeElementCollection]);
DeclareOperation("MajorantClosure", [IsInverseSemigroup, IsCollection]);
DeclareOperation("MajorantClosureNC", [IsInverseSemigroup,
                                       IsAssociativeElementCollection]);
DeclareOperation("Minorants", [IsInverseSemigroup, IsAssociativeElement]);
DeclareAttribute("SameMinorantsSubgroup", IsGroupHClass and IsInverseOpClass
                                          and IsActingSemigroupGreensClass);
DeclareOperation("RightCosetsOfInverseSemigroup",
                 [IsSemigroupWithInverseOp and IsActingSemigroup,
                  IsSemigroupWithInverseOp and IsActingSemigroup]);
DeclareAttribute("SmallerDegreePartialPermRepresentation", IsInverseSemigroup);
DeclareAttribute("PrimitiveIdempotents", IsInverseSemigroup);
DeclareGlobalFunction("SupremumIdempotentsNC");
DeclareAttribute("VagnerPrestonRepresentation", IsSemigroupWithInverseOp and
                                                IsActingSemigroup);
DeclareOperation("NaturalLeqInverseSemigroup", [IsAssociativeElement,
                                                IsAssociativeElement]);
