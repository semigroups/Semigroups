#############################################################################
##
#W  properties.gd
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("IrredundantGeneratingSubset",
[IsAssociativeElementWithActionCollection]);

DeclareProperty("IsAbundantSemigroup", IsSemigroup);
DeclareProperty("IsAdequateSemigroup", IsSemigroup);
DeclareProperty("IsBand", IsSemigroup); 
DeclareProperty("IsBlockGroup", IsSemigroup);
DeclareProperty("IsBrandtSemigroup", IsSemigroup); 
DeclareProperty("IsCliffordSemigroup", IsSemigroup);
DeclareProperty("IsCommutativeSemigroup", IsSemigroup);
DeclareProperty("IsCompletelyRegularSemigroup", IsSemigroup); 
DeclareProperty("IsCompletelySimpleSemigroup", IsSemigroup); 
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
DeclareProperty("IsGroupAsSemigroup", IsSemigroup);
DeclareProperty("IsIdempotentGenerated", IsSemigroup); 
DeclareProperty("IsLeftSimple", IsSemigroup);
DeclareProperty("IsLeftZeroSemigroup", IsSemigroup); 
DeclareProperty("IsMonogenicInverseSemigroup", IsSemigroup);
DeclareProperty("IsMonogenicSemigroup", IsSemigroup); 
DeclareProperty("IsMonoidAsSemigroup", IsSemigroup); 
DeclareProperty("IsOrthodoxSemigroup", IsSemigroup);
DeclareProperty("IsRectangularBand", IsSemigroup); 
DeclareOperation("IsRegularSemigroupElementNC", [IsSemigroup, IsMultiplicativeElement and IsAssociativeElement]);
DeclareProperty("IsRightSimple", IsSemigroup);
DeclareProperty("IsRightZeroSemigroup", IsSemigroup); 
DeclareProperty("IsSemiband", IsSemigroup); 
DeclareSynonymAttr("IsSemigroupWithCommutingIdempotents", IsBlockGroup);
DeclareProperty("IsSemilatticeAsSemigroup", IsSemigroup); 
DeclareProperty("IsSynchronizingSemigroup", IsTransformationSemigroup);
DeclareProperty("IsUnitRegularSemigroup", IsSemigroup);
DeclareProperty("IsZeroRectangularBand", IsSemigroup);
DeclareProperty("IsZeroSemigroup", IsSemigroup); 

InstallTrueMethod(IsAbundantSemigroup, IsRegularSemigroup);
InstallTrueMethod(IsAdequateSemigroup, IsAbundantSemigroup and IsBlockGroup);
InstallTrueMethod(IsBlockGroup, IsInverseSemigroup);
InstallTrueMethod(IsBand, IsSemilatticeAsSemigroup);
InstallTrueMethod(IsBrandtSemigroup, IsInverseSemigroup and IsZeroSimpleSemigroup);
InstallTrueMethod(IsCliffordSemigroup, IsSemilatticeAsSemigroup);
InstallTrueMethod(IsCompletelyRegularSemigroup, IsCliffordSemigroup);
InstallTrueMethod(IsCompletelyRegularSemigroup, IsSimpleSemigroup);
InstallTrueMethod(IsCompletelySimpleSemigroup, IsSimpleSemigroup and IsFinite);
InstallTrueMethod(IsDTrivial, IsSemilatticeAsSemigroup);
InstallTrueMethod(IsGroupAsSemigroup, IsInverseSemigroup and IsSimpleSemigroup);
InstallTrueMethod(IsHTrivial, IsLTrivial);
InstallTrueMethod(IsHTrivial, IsRTrivial);
InstallTrueMethod(IsIdempotentGenerated, IsSemilatticeAsSemigroup);
InstallTrueMethod(IsInverseMonoid, IsInverseSemigroup and IsMonoid);
InstallTrueMethod(IsInverseSemigroup, IsSemilatticeAsSemigroup);
InstallTrueMethod(IsInverseSemigroup, IsCliffordSemigroup);
InstallTrueMethod(IsLeftSimple, IsInverseSemigroup and IsGroupAsSemigroup);
InstallTrueMethod(IsLeftZeroSemigroup, IsInverseSemigroup and IsTrivial);
InstallTrueMethod(IsLTrivial, IsInverseSemigroup and IsRTrivial);
InstallTrueMethod(IsLTrivial, IsDTrivial);
InstallTrueMethod(IsMonoidAsSemigroup, IsGroupAsSemigroup);
InstallTrueMethod(IsOrthodoxSemigroup, IsInverseSemigroup);
InstallTrueMethod(IsRectangularBand, IsHTrivial and IsSimpleSemigroup);
InstallTrueMethod(IsRegularSemigroup, IsInverseSemigroup);
InstallTrueMethod(IsRegularSemigroup, IsSimpleSemigroup);
InstallTrueMethod(IsRightSimple, IsInverseSemigroup and IsGroupAsSemigroup);
InstallTrueMethod(IsRightZeroSemigroup, IsInverseSemigroup and IsTrivial);
InstallTrueMethod(IsRTrivial, IsInverseSemigroup and IsLTrivial);
InstallTrueMethod(IsRTrivial, IsDTrivial);
InstallTrueMethod(IsSemiband, IsIdempotentGenerated);
InstallTrueMethod(IsSemilatticeAsSemigroup, IsDTrivial and IsInverseSemigroup);
InstallTrueMethod(IsSemilatticeAsSemigroup, IsCommutative and IsBand);
InstallTrueMethod(IsSimpleSemigroup, IsGroupAsSemigroup);
InstallTrueMethod(IsZeroSemigroup, IsInverseSemigroup and IsTrivial);
InstallTrueMethod(IsMonogenicInverseSemigroup, IsInverseSemigroup and IsMonogenicSemigroup);
InstallTrueMethod(IsZeroRectangularBand, IsZeroGroup);
InstallTrueMethod(IsZeroGroup, IsZeroRectangularBand and IsInverseSemigroup);
InstallTrueMethod(IsGroupAsSemigroup, IsCommutative and IsSimpleSemigroup);

#EOF
