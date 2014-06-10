#############################################################################
##
#W  attributes.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("IrredundantGeneratingSubset",
[IsAssociativeElementCollection]);
DeclareOperation("InversesOfSemigroupElementNC", [IsSemigroup, IsAssociativeElement]);

DeclareAttribute("GroupOfUnits", IsSemigroup);
DeclareAttribute("IdempotentGeneratedSubsemigroup", IsSemigroup);
DeclareAttribute("InjectionPrincipalFactor", IsGreensDClass);
DeclareAttribute("IsomorphismReesMatrixSemigroup", IsGreensDClass);
DeclareAttribute("MinimalIdeal", IsSemigroup);
DeclareAttribute("PrimitiveIdempotents", IsInverseSemigroup);
DeclareAttribute("PrincipalFactor", IsGreensDClass);
DeclareAttribute("MultiplicativeZero", IsNonExhaustiveSemigroup);

DeclareAttribute("SmallSemigroupGeneratingSet", IsAssociativeElementCollection);
DeclareAttribute("SmallSemigroupGeneratingSet", IsSemigroup and IsFinite);
DeclareAttribute("SmallMonoidGeneratingSet", IsAssociativeElementCollection and
IsMultiplicativeElementWithOneCollection);
DeclareAttribute("SmallMonoidGeneratingSet", IsMonoid and IsFinite);
DeclareAttribute("SmallInverseSemigroupGeneratingSet", 
IsGeneratorsOfInverseSemigroup);
DeclareAttribute("SmallInverseSemigroupGeneratingSet", 
IsSemigroupWithInverseOp);
DeclareAttribute("SmallInverseMonoidGeneratingSet", 
IsGeneratorsOfInverseSemigroup and IsMultiplicativeElementWithOneCollection);
DeclareAttribute("SmallInverseMonoidGeneratingSet", 
IsInverseMonoid and IsSemigroupWithInverseOp);
DeclareAttribute("SmallGeneratingSet", IsSemigroup);

DeclareAttribute("StructureDescription", IsBrandtSemigroup);
DeclareAttribute("StructureDescription", IsGroupAsSemigroup);
DeclareAttribute("StructureDescriptionSchutzenbergerGroups", IsSemigroup and IsFinite);
DeclareAttribute("StructureDescriptionMaximalSubgroups", IsSemigroup and IsFinite);
DeclareAttribute("MaximalDClasses", IsSemigroup);
DeclareAttribute("MinimalDClass", IsSemigroup);
DeclareAttribute("IsGreensDLeq", IsNonExhaustiveSemigroup);

