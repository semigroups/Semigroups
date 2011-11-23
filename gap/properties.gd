#############################################################################
##
#W  properties.gd
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("GroupOfUnits", IsTransformationSemigroup);
DeclareAttribute("IdempotentGeneratedSubsemigp", IsTransformationSemigroup);
DeclareOperation("IrredundantGeneratingSubset", [IsTransformationCollection]);
DeclareProperty("IsAbundantSemigroup", IsTransformationSemigroup);
DeclareProperty("IsAdequateSemigroup", IsTransformationSemigroup);
DeclareProperty("IsBand", IsTransformationSemigroup);
DeclareProperty("IsBlockGroup", IsTransformationSemigroup);
DeclareProperty("IsBrandtSemigroup", IsTransformationSemigroup);
DeclareProperty("IsCliffordSemigroup", IsTransformationSemigroup);
DeclareProperty("IsCommutativeSemigroup", IsTransformationSemigroup);
DeclareProperty("IsCompletelyRegularSemigroup", IsTransformationSemigroup);
DeclareProperty("IsCompletelySimpleSemigroup", IsTransformationSemigroup);   
DeclareProperty("IsGreensRTrivial", IsTransformationSemigroup);
DeclareProperty("IsGreensLTrivial", IsTransformationSemigroup);
DeclareProperty("IsGreensHTrivial", IsTransformationSemigroup);
DeclareSynonymAttr("IsAperiodicSemigroup", IsGreensHTrivial);
DeclareSynonymAttr("IsCombinatorialSemigroup", IsGreensHTrivial);
DeclareProperty("IsGroupAsSemigroup", IsTransformationSemigroup);
DeclareProperty("IsIdempotentGenerated", IsTransformationSemigroup);
DeclareProperty("IsLeftSimple", IsTransformationSemigroup);
DeclareProperty("IsLeftZeroSemigroup", IsTransformationSemigroup);
DeclareProperty("IsMonogenicSemigroup", IsTransformationSemigroup);
DeclareProperty("IsMonoidAsSemigroup", IsTransformationSemigroup);
DeclareOperation("IsomorphismTransformationMonoid",
 [IsTransformationSemigroup]);
DeclareProperty("IsOrthodoxSemigroup", IsTransformationSemigroup);
DeclareProperty("IsRectangularBand", IsTransformationSemigroup);
DeclareProperty("IsRightSimple", IsTransformationSemigroup);
DeclareProperty("IsRightZeroSemigroup", IsTransformationSemigroup);
DeclareProperty("IsSemiband", IsTransformationSemigroup);
DeclareSynonymAttr("IsSemigroupWithCommutingIdempotents", IsBlockGroup);
DeclareProperty("IsSemilatticeAsSemigroup", IsTransformationSemigroup);
DeclareProperty("IsSynchronizingSemigroup", IsTransformationSemigroup);
DeclareProperty("IsZeroRectangularBand", IsTransformationSemigroup);
DeclareProperty("IsZeroSemigroup", IsTransformationSemigroup);
DeclareAttribute("MinimalIdeal", IsTransformationSemigroup);
DeclareOperation("RedundantGenerator", [IsTransformationCollection]);
DeclareAttribute("SmallGeneratingSet", IsTransformationSemigroup);

