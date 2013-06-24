#############################################################################
##
#W  attributes.gd
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareGlobalFunction("SemigroupActionHomomorphism");

DeclareOperation("EmbeddingNC", [IsSemigroup, IsGreensHClass]);
DeclareAttribute("GroupOfUnits", IsSemigroup);
DeclareAttribute("IdempotentGeneratedSubsemigroup", IsSemigroup);
DeclareAttribute("InjectionPrincipalFactor", IsGreensDClass);

DeclareOperation("InversesOfSemigroupElementNC", [IsActingSemigroup and
HasGeneratorsOfSemigroup, IsAssociativeElement]);

DeclareOperation("IsomorphismBipartitionSemigroup", [IsSemigroup]);
DeclareAttribute("IsomorphismReesMatrixSemigroup", IsGreensDClass);
DeclareAttribute("IsomorphismPermGroup", IsTransformationSemigroup);

DeclareAttribute("MinimalIdeal", IsSemigroup);
DeclareOperation("NrElementsOfRank", [IsSemigroup and
HasGeneratorsOfSemigroup, IsPosInt]);
DeclareAttribute("PosetOfIdempotents", IsSemigroup);
DeclareAttribute("PrimitiveIdempotents", IsInverseSemigroup);
DeclareAttribute("PrincipalFactor", IsGreensDClass);

DeclareAttribute("SameMinorantsSubgroup", IsGroupHClass);
DeclareAttribute("SmallerDegreePartialPermRepresentation", IsInverseSemigroup);
DeclareAttribute("VagnerPrestonRepresentation", IsInverseSemigroup);
DeclareAttribute("JoinIrreducibleDClasses", IsInverseSemigroup);

DeclareOperation("Minorants", [IsInverseSemigroup and IsPartialPermSemigroup,
IsPartialPerm]);
DeclareGlobalFunction("SupremumIdempotentsNC");

DeclareOperation("IsMajorantlyClosed", [IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPermCollection]);
DeclareOperation("IsMajorantlyClosedNC", [IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPermCollection]);
DeclareOperation("MajorantClosure", [IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPermCollection]);
DeclareOperation("MajorantClosureNC", [IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPermCollection]);
DeclareOperation("IsJoinIrreducible", [IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm]);
DeclareOperation("RightCosetsOfInverseSemigroup", [IsInverseSemigroup and IsPartialPermSemigroup, IsInverseSemigroup and IsPartialPermSemigroup]);

DeclareAttribute("MultiplicativeZero", IsActingSemigroup);
DeclareAttribute("SmallGeneratingSet", IsActingSemigroup);
DeclareAttribute("StructureDescription", IsBrandtSemigroup);
DeclareAttribute("StructureDescription", IsGroupAsSemigroup);
