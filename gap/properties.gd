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
DeclareInfoClass("InfoCitrusProperties");
DeclareOperation("IrredundantGeneratingSubset", [IsTransformationCollection]);
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
DeclareProperty("IsLeftZeroSemigroup", IsTransformationSemigroup);
DeclareProperty("IsMonoidAsSemigroup", IsTransformationSemigroup);
DeclareProperty("IsMinimalIdeal", IsSemigroupIdeal and 
 IsTransformationSemigroup);
DeclareOperation("IsomorphismTransformationMonoid",
 [IsTransformationSemigroup]);
DeclareProperty("IsOrthodoxSemigroup", IsTransformationSemigroup);
DeclareProperty("IsRectangularBand", IsTransformationSemigroup);
DeclareProperty("IsRightZeroSemigroup", IsTransformationSemigroup);
DeclareProperty("IsSemiband", IsTransformationSemigroup);
DeclareSynonymAttr("IsSemigroupWithCommutingIdempotents", IsBlockGroup);
DeclareProperty("IsSemilatticeAsSemigroup", IsTransformationSemigroup);
DeclareProperty("IsSynchronizingSemigroup", IsTransformationSemigroup);
DeclareProperty("IsZeroSemigroup", IsTransformationSemigroup);
DeclareAttribute("MinimalIdeal", IsTransformationSemigroup);
DeclareOperation("RedundantGenerator", [IsTransformationCollection]);
DeclareAttribute("SmallGeneratingSet", IsTransformationSemigroup);

