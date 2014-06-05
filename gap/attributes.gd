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
DeclareOperation("InversesOfSemigroupElementNC", [IsNonExhaustiveSemigroup,
IsAssociativeElement]);

DeclareAttribute("GroupOfUnits", IsSemigroup);
DeclareAttribute("IdempotentGeneratedSubsemigroup", IsSemigroup);
DeclareAttribute("InjectionPrincipalFactor", IsGreensDClass);
DeclareAttribute("IsomorphismReesMatrixSemigroup", IsGreensDClass);
DeclareAttribute("MinimalIdeal", IsSemigroup);
DeclareAttribute("PrimitiveIdempotents", IsInverseSemigroup);
DeclareAttribute("PrincipalFactor", IsGreensDClass);
DeclareAttribute("MultiplicativeZero", IsNonExhaustiveSemigroup);

DeclareAttribute("SmallSemigroupGeneratingSet", IsAssociativeElementCollection);
DeclareAttribute("SmallSemigroupGeneratingSet", IsNonExhaustiveSemigroup);
DeclareAttribute("SmallMonoidGeneratingSet", IsAssociativeElementCollection and
IsMultiplicativeElementWithOneCollection);
DeclareAttribute("SmallMonoidGeneratingSet", IsNonExhaustiveSemigroup and IsMonoid);
DeclareAttribute("SmallInverseSemigroupGeneratingSet", 
IsGeneratorsOfInverseSemigroup);
DeclareAttribute("SmallInverseSemigroupGeneratingSet", 
IsInverseSemigroup and IsNonExhaustiveSemigroup);
DeclareAttribute("SmallInverseMonoidGeneratingSet", 
IsGeneratorsOfInverseSemigroup and IsMultiplicativeElementWithOneCollection);
DeclareAttribute("SmallInverseMonoidGeneratingSet", 
IsInverseMonoid and IsNonExhaustiveSemigroup);
DeclareAttribute("SmallGeneratingSet", IsSemigroup);

DeclareAttribute("StructureDescription", IsBrandtSemigroup);
DeclareAttribute("StructureDescription", IsGroupAsSemigroup);
DeclareAttribute("StructureDescriptionSchutzenbergerGroups", IsNonExhaustiveSemigroup);
DeclareAttribute("StructureDescriptionMaximalSubgroups", IsNonExhaustiveSemigroup);
DeclareAttribute("MaximalDClasses", IsSemigroup);
DeclareAttribute("MinimalDClass", IsSemigroup);
DeclareAttribute("IsGreensDLeq", IsNonExhaustiveSemigroup);

