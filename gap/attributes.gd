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

DeclareOperation("EmbeddingNC", [IsSemigroup, IsSemigroup]);
DeclareAttribute("GroupOfUnits", IsSemigroup);
DeclareAttribute("IdempotentGeneratedSubsemigroup", IsSemigroup);
DeclareAttribute("InjectionPrincipalFactor", IsGreensDClass);

DeclareOperation("InversesOfSemigroupElement", [IsActingSemigroup and
HasGeneratorsOfSemigroup, IsAssociativeElement]);
DeclareOperation("InversesOfSemigroupElementNC", [IsActingSemigroup and
HasGeneratorsOfSemigroup, IsAssociativeElement]);

DeclareOperation("IsomorphismBipartitionSemigroup", [IsSemigroup]);
DeclareOperation("IsomorphismPartialPermMonoid", [IsPermGroup]);
DeclareOperation("IsomorphismPartialPermSemigroup", [IsPermGroup]);
#DeclareOperation("IsomorphismTransformationMonoid", [IsSemigroup]);

DeclareAttribute("MinimalIdeal", IsSemigroup);
DeclareOperation("NrElementsOfRank", [IsSemigroup and
HasGeneratorsOfSemigroup, IsPosInt]);
DeclareAttribute("PosetOfIdempotents", IsSemigroup);
DeclareAttribute("PrimitiveIdempotents", IsInverseSemigroup);
DeclareAttribute("PrincipalFactor", IsGreensDClass);

DeclareAttribute("SameMinorantsSubgroup", IsGroupHClass);
DeclareAttribute("SmallerDegreePartialPermRep", IsInverseSemigroup);
DeclareAttribute("VagnerPrestonRepresentation", IsInverseSemigroup);
DeclareAttribute("JoinIrreducibleDClasses", IsInverseSemigroup);

DeclareOperation("IsMajorantlyClosed", [IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPermCollection]);
DeclareOperation("IsMajorantlyClosedNC", [IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPermCollection]);
DeclareOperation("MajorantClosure", [IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPermCollection]);
DeclareOperation("MajorantClosureNC", [IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPermCollection]);
