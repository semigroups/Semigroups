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
DeclareOperation("InversesOfSemigroupElementNC", [IsActingSemigroup,
IsAssociativeElement]);

DeclareAttribute("GroupOfUnits", IsSemigroup);
DeclareAttribute("IdempotentGeneratedSubsemigroup", IsSemigroup);
DeclareAttribute("InjectionPrincipalFactor", IsGreensDClass);
DeclareAttribute("IsomorphismReesMatrixSemigroup", IsGreensDClass);
DeclareAttribute("MinimalIdeal", IsSemigroup);
DeclareAttribute("PosetOfIdempotents", IsSemigroup);
DeclareAttribute("PrimitiveIdempotents", IsInverseSemigroup);
DeclareAttribute("PrincipalFactor", IsGreensDClass);
DeclareAttribute("MultiplicativeZero", IsActingSemigroup);

DeclareAttribute("SmallSemigroupGeneratingSet", IsAssociativeElementCollection);
DeclareAttribute("SmallSemigroupGeneratingSet", IsActingSemigroup);
DeclareAttribute("SmallMonoidGeneratingSet", IsAssociativeElementCollection and
IsMultiplicativeElementWithOneCollection);
DeclareAttribute("SmallMonoidGeneratingSet", IsActingSemigroup and IsMonoid);
DeclareAttribute("SmallInverseSemigroupGeneratingSet", 
IsGeneratorsOfInverseSemigroup);
DeclareAttribute("SmallInverseSemigroupGeneratingSet", 
IsInverseSemigroup and IsActingSemigroup);
DeclareAttribute("SmallInverseMonoidGeneratingSet", 
IsGeneratorsOfInverseSemigroup and IsMultiplicativeElementWithOneCollection);
DeclareAttribute("SmallInverseMonoidGeneratingSet", 
IsInverseMonoid and IsActingSemigroup);
DeclareAttribute("SmallGeneratingSet", IsSemigroup);

DeclareAttribute("StructureDescription", IsBrandtSemigroup);
DeclareAttribute("StructureDescription", IsGroupAsSemigroup);
DeclareAttribute("StructureDescriptionSchutzenbergerGroups", IsActingSemigroup);
DeclareAttribute("StructureDescriptionMaximalSubgroups", IsActingSemigroup);
DeclareAttribute("MaximalDClasses", IsSemigroup);
DeclareAttribute("MaximalSubsemigroups", IsSemigroup);
DeclareAttribute("MinimalDClass", IsSemigroup);
DeclareAttribute("IsGreensDLeq", IsActingSemigroup);

DeclareOperation("SubsemigroupByIndicesNC", 
[IsReesMatrixSemigroup, IsDenseList, IsDenseList]);
DeclareOperation("SubsemigroupByIndicesNC",
[IsReesZeroMatrixSemigroup, IsDenseList, IsDenseList]);
DeclareOperation("GeneratorsOfReesMatrixSubsemigroupNC",
[IsReesMatrixSemigroup, IsGroup]);
DeclareOperation("GeneratorsOfReesMatrixSubsemigroupNC",
[IsReesZeroMatrixSemigroup, IsGroup]);
