#############################################################################
##
#W  properties.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareSynonym("IsMatrixSemigroup", IsSemigroup and IsRingElementCollCollColl);
DeclareOperation("OneMutable", [IsRingElementCollCollColl]);

DeclareProperty("IsCommutativeSemigroup", IsSemigroupIdeal);

DeclareProperty("IsAbundantSemigroup", IsSemigroup);
DeclareProperty("IsAdequateSemigroup", IsSemigroup);
DeclareProperty("IsBlockGroup", IsSemigroup);
DeclareProperty("IsRTrivial", IsSemigroup);
DeclareProperty("IsRTrivial", IsGreensDClass);
DeclareProperty("IsLTrivial", IsSemigroup);
DeclareProperty("IsLTrivial", IsGreensDClass);
DeclareProperty("IsHTrivial", IsSemigroup);
DeclareProperty("IsHTrivial", IsGreensDClass);

DeclareSynonymAttr("IsDTrivial", IsRTrivial and IsLTrivial);
DeclareSynonymAttr("IsAperiodicSemigroup", IsHTrivial);
DeclareSynonymAttr("IsCombinatorialSemigroup", IsHTrivial);
DeclareProperty("IsFactorisableSemigroup", IsSemigroup);
DeclareProperty("IsLeftSimple", IsSemigroup);
DeclareProperty("IsMonogenicInverseSemigroup", IsSemigroup);
DeclareOperation("IsRegularSemigroupElementNC", [IsSemigroup,
IsMultiplicativeElement and IsAssociativeElement]);
DeclareProperty("IsRightSimple", IsSemigroup);
DeclareSynonymAttr("IsSemigroupWithCommutingIdempotents", IsBlockGroup);
DeclareProperty("IsUnitRegularSemigroup", IsSemigroup);
DeclareProperty("IsZeroRectangularBand", IsSemigroup);
DeclareProperty("IsCongruenceFreeSemigroup", IsSemigroup);
DeclareProperty("IsEUnitaryInverseSemigroup", IsInverseSemigroup);

#EOF
