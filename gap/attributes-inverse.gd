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
                 IsInverseSemigroup and IsPartialPermSemigroup);

DeclareOperation("IsJoinIrreducible",
                 [IsInverseSemigroup, IsAssociativeElement]);
DeclareAttribute("JoinIrreducibleDClasses", IsInverseSemigroup);
DeclareOperation("IsMajorantlyClosed",
                 [IsActingSemigroup and IsSemigroupWithInverseOp,
                  IsAssociativeElementCollection]);
DeclareOperation("IsMajorantlyClosed",
                 [IsActingSemigroup and IsSemigroupWithInverseOp,
                  IsActingSemigroup]);
DeclareOperation("IsMajorantlyClosedNC",
                 [IsSemigroupWithInverseOp and IsActingSemigroup,
                  IsAssociativeElementCollection]);
DeclareOperation("MajorantClosure", [IsInverseSemigroup, IsCollection]);
DeclareOperation("MajorantClosureNC",
                 [IsInverseSemigroup,
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
DeclareAttribute("VagnerPrestonRepresentation", IsSemigroupWithInverseOp); 
DeclareOperation("NaturalLeqInverseSemigroup",
                 [IsAssociativeElement, IsAssociativeElement]);
