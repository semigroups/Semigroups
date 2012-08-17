#############################################################################
##
#W  attributes.gd
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##


DeclareGlobalFunction("SemigroupActionHomomorphism");

DeclareAttribute("AntiIsomorphismTransformationSemigroup",
 IsSemigroup);
DeclareOperation("EmbeddingNC", [IsSemigroup, IsSemigroup]);
DeclareAttribute("GroupOfUnits", IsSemigroup);
DeclareAttribute("IdempotentGeneratedSubsemigp", IsSemigroup);
DeclareAttribute("InjectionPrincipalFactor", IsGreensDClass);

DeclareOperation("InversesOfSemigroupElement", [IsActingSemigroup and
HasGeneratorsOfSemigroup, IsActingElt]);
DeclareOperation("InversesOfSemigroupElementNC", [IsActingSemigroup and
HasGeneratorsOfSemigroup, IsActingElt]);

DeclareOperation("IsomorphismPartialPermMonoid", [IsPermGroup]);
DeclareOperation("IsomorphismPartialPermSemigroup", [IsPermGroup]);
DeclareOperation("IsomorphismTransformationMonoid", [IsSemigroup]);

DeclareAttribute("MinimalIdeal", IsSemigroup);
DeclareOperation("NrElementsOfRank", [IsSemigroup and
HasGeneratorsOfSemigroup, IsPosInt]);
DeclareAttribute("PosetOfIdempotents", IsSemigroup);
DeclareAttribute("PrimitiveIdempotents", IsInverseSemigroup);
DeclareAttribute("PrincipalFactor", IsGreensDClass);

