#############################################################################
##
#W  attr.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains declarations for attributes of semigroups.

DeclareOperation("IrredundantGeneratingSubset",
                 [IsMultiplicativeElementCollection]);

DeclareAttribute("GroupOfUnits", IsSemigroup);
DeclareAttribute("IdempotentGeneratedSubsemigroup", IsSemigroup);
DeclareAttribute("InjectionPrincipalFactor", IsGreensDClass);
DeclareAttribute("InjectionNormalizedPrincipalFactor", IsGreensDClass);
DeclareAttribute("RepresentativeOfMinimalIdeal", IsSemigroup);
DeclareOperation("RepresentativeOfMinimalIdealNC", [IsSemigroup]);
DeclareSynonymAttr("RepresentativeOfMinimalDClass",
                   RepresentativeOfMinimalIdeal);
DeclareAttribute("MinimalIdeal", IsSemigroup);
DeclareAttribute("PrincipalFactor", IsGreensDClass);
DeclareAttribute("NormalizedPrincipalFactor", IsGreensDClass);
DeclareAttribute("MultiplicativeZero", IsSemigroup);
DeclareAttribute("LengthOfLongestDClassChain", IsSemigroup);

DeclareAttribute("SmallSemigroupGeneratingSet",
                 IsMultiplicativeElementCollection);
DeclareAttribute("SmallMonoidGeneratingSet",
                 IsMultiplicativeElementWithOneCollection);
DeclareAttribute("SmallInverseSemigroupGeneratingSet",
                 IsMultiplicativeElementCollection);
DeclareAttribute("SmallInverseMonoidGeneratingSet",
                 IsMultiplicativeElementWithOneCollection);
DeclareAttribute("SmallGeneratingSet", IsSemigroup);

DeclareAttribute("MinimalSemigroupGeneratingSet",
                 IsSemigroup);
DeclareAttribute("MinimalMonoidGeneratingSet",
                 IsMonoid);
DeclareAttribute("MinimalInverseSemigroupGeneratingSet",
                 IsSemigroup);
DeclareAttribute("MinimalInverseMonoidGeneratingSet",
                 IsMonoid);

DeclareAttribute("SmallestElementSemigroup", IsSemigroup);
DeclareAttribute("LargestElementSemigroup", IsSemigroup);

DeclareAttribute("StructureDescription", IsBrandtSemigroup);
DeclareAttribute("StructureDescription", IsGroupAsSemigroup);
DeclareAttribute("StructureDescriptionMaximalSubgroups",
                 IsSemigroup);
DeclareAttribute("MaximalDClasses", IsSemigroup);
DeclareAttribute("MinimalDClass", IsSemigroup);
DeclareAttribute("IsGreensDGreaterThanFunc", IsSemigroup);

DeclareAttribute("UnderlyingSemigroupOfSemigroupWithAdjoinedZero",
                 IsSemigroup);

DeclareOperation("InversesOfSemigroupElementNC",
                 [IsSemigroup, IsMultiplicativeElement]);
