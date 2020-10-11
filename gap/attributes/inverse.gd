#############################################################################
##
##  attributes/inverse.gd
##  Copyright (C) 2013-2022                              James D. Mitchell
##                                                          Wilf A. Wilson
##                                                        Rhiannon Dougall
##                                                          Robert Hancock
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
                 [IsInverseSemigroup and IsGeneratorsOfInverseSemigroup,
                  IsMultiplicativeElementCollection]);
DeclareOperation("IsMajorantlyClosedNC",
                 [IsInverseSemigroup and IsGeneratorsOfInverseSemigroup,
                  IsMultiplicativeElementCollection]);
DeclareOperation("MajorantClosure", [IsInverseSemigroup, IsCollection]);
DeclareOperation("MajorantClosureNC",
                 [IsInverseSemigroup, IsMultiplicativeElementCollection]);
DeclareOperation("Minorants", [IsInverseSemigroup, IsMultiplicativeElement]);
DeclareAttribute("SameMinorantsSubgroup", IsGroupHClass);
DeclareOperation("RightCosetsOfInverseSemigroup",
                 [IsInverseSemigroup and IsGeneratorsOfInverseSemigroup,
                  IsInverseSemigroup and IsGeneratorsOfInverseSemigroup]);
DeclareAttribute("PrimitiveIdempotents", IsSemigroup);
DeclareGlobalFunction("SupremumIdempotentsNC");
DeclareAttribute("VagnerPrestonRepresentation",
                 IsInverseSemigroup and IsGeneratorsOfInverseSemigroup);
DeclareAttribute("NaturalLeqInverseSemigroup", IsSemigroup);
