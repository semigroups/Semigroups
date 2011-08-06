#############################################################################
##
#W  properties.gd
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("GroupOfUnits", IsSemigroup);
DeclareInfoClass("InfoCitrusProperties");
DeclareOperation("IrredundantGeneratingSubset", [IsTransformationCollection]);
DeclareProperty("IsBand", IsTransformationSemigroup);
DeclareProperty("IsBlockGroup", IsTransformationSemigroup);
DeclareProperty("IsCliffordSemigroup", IsTransformationSemigroup);
DeclareProperty("IsCommutativeSemigroup", IsTransformationSemigroup);
DeclareProperty("IsCompletelyRegularSemigroup", IsTransformationSemigroup);

if not IsBound(IsCompletelySimpleSemigroup) then 
  DeclareProperty("IsCompletelySimpleSemigroup", IsSemigroup);
   InstallTrueMethod(IsCompletelySimpleSemigroup, IsFinite and 
   IsSimpleSemigroup);
fi;

DeclareProperty("IsGreensRTrivial", IsTransformationSemigroup);
DeclareProperty("IsGreensLTrivial", IsTransformationSemigroup);
DeclareProperty("IsGreensHTrivial", IsTransformationSemigroup);
DeclareSynonymAttr("IsAperiodicSemigroup", IsGreensHTrivial);
DeclareSynonymAttr("IsCombinatorialSemigroup", IsGreensHTrivial);
DeclareProperty("IsGroupAsSemigroup", IsTransformationSemigroup);
DeclareProperty("IsIdempotentGenerated", IsSemigroup);
DeclareProperty("IsLeftZeroSemigroup", IsTransformationSemigroup);
DeclareProperty("IsMonoidAsSemigroup", IsTransformationSemigroup);
DeclareProperty("IsMinimalIdeal", IsSemigroupIdeal and 
 IsTransformationSemigroup);
DeclareOperation("IsomorphismTransformationMonoid",
 [IsTransformationSemigroup]);
DeclareProperty("IsOrthodoxSemigroup", IsSemigroup);
DeclareProperty("IsRectangularBand", IsTransformationSemigroup);
DeclareProperty("IsRightZeroSemigroup", IsTransformationSemigroup);
DeclareSynonymAttr("IsSemiband", IsIdempotentGenerated);
DeclareSynonymAttr("IsSemigroupWithCommutingIdempotents", IsBlockGroup);

if not IsBound(IsSemilatticeAsSemigroup) then 
  DeclareProperty("IsSemilatticeAsSemigroup", IsSemigroup);
  InstallTrueMethod(IsSemilatticeAsSemigroup, IsCommutative and IsBand);
fi;

DeclareProperty("IsSynchronizingSemigroup", IsTransformationCollection);
DeclareProperty("IsZeroSemigroup", IsTransformationSemigroup);
DeclareAttribute("MinimalIdeal", IsTransformationSemigroup);
DeclareOperation("RedundantGenerator", [IsTransformationCollection]);
DeclareAttribute("SmallGeneratingSet", IsTransformationSemigroup);
DeclareAttribute("UnderlyingDClassOfMinIdeal", IsSemigroupIdeal and 
 IsTransformationSemigroup);

# the following have been temporarily move to dev/properties.gi until
# ClosureSemigroup is released.

#DeclareOperation("IrredundantGeneratingSubset", [IsTransformationCollection]);
#SmallGeneratingSet
