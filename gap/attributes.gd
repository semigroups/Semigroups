#############################################################################
##
#W  attributes.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("IsMaximalSubsemigroup", [IsSemigroup, IsSemigroup]);
DeclareOperation("IrredundantGeneratingSubset",
[IsAssociativeElementCollection]);
DeclareOperation("InversesOfSemigroupElementNC", [IsActingSemigroup and
HasGeneratorsOfSemigroup, IsAssociativeElement]);

DeclareAttribute("GroupOfUnits", IsSemigroup);
DeclareAttribute("IdempotentGeneratedSubsemigroup", IsSemigroup);
DeclareAttribute("InjectionPrincipalFactor", IsGreensDClass);
DeclareAttribute("IsomorphismReesMatrixSemigroup", IsGreensDClass);
DeclareAttribute("MinimalIdeal", IsSemigroup);
DeclareAttribute("PosetOfIdempotents", IsSemigroup);
DeclareAttribute("PrimitiveIdempotents", IsInverseSemigroup);
DeclareAttribute("PrincipalFactor", IsGreensDClass);
DeclareAttribute("MultiplicativeZero", IsActingSemigroup);
DeclareAttribute("SmallGeneratingSet", IsActingSemigroup);
DeclareAttribute("StructureDescription", IsBrandtSemigroup);
DeclareAttribute("StructureDescription", IsGroupAsSemigroup);
DeclareAttribute("StructureDescriptionSchutzenbergerGroups", IsActingSemigroup);
DeclareAttribute("StructureDescriptionMaximalSubgroups", IsActingSemigroup);
DeclareAttribute("MaximalDClasses", IsSemigroup);
DeclareAttribute("MaximalSubsemigroups", IsSemigroup);
DeclareAttribute("MinimalDClass", IsSemigroup);
DeclareAttribute("IsGreensDLeq", IsActingSemigroup);
DeclareAttribute("LinkedTriples", IsReesMatrixSemigroup);
DeclareAttribute("LinkedTriples", IsReesZeroMatrixSemigroup);

DeclareOperation("SemigroupCongruenceByLinkedTriple",
[IsReesZeroMatrixSemigroup and IsFinite,
 IsGroup,
 IsDenseList,
 IsDenseList]);

DeclareOperation("SubsemigroupByIndicesNC", 
[IsReesMatrixSemigroup, IsDenseList, IsDenseList]);
DeclareOperation("SubsemigroupByIndicesNC",
[IsReesZeroMatrixSemigroup, IsDenseList, IsDenseList]);
DeclareOperation("GeneratorsOfReesMatrixSubsemigroupNC",
[IsReesMatrixSemigroup, IsGroup]);
DeclareOperation("GeneratorsOfReesMatrixSubsemigroupNC",
[IsReesZeroMatrixSemigroup, IsGroup]);
