#############################################################################
##
#W  properties.gd
#Y  Copyright (C) 2006-2011                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id: properties.gd 156 2011-04-03 13:02:19Z jamesm $
##

DeclareAttribute("GroupOfUnits", IsSemigroup);
DeclareInfoClass("InfoMonoidProperties");
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
DeclareProperty("IsIdempotentGenerated", IsTransformationSemigroup);
DeclareOperation("IsIrredundantGeneratingSet", [IsTransformationCollection]);
DeclareProperty("IsLeftZeroSemigroup", IsTransformationSemigroup);
DeclareProperty("IsMonoidAsSemigroup", IsTransformationSemigroup);
DeclareProperty("IsMinimalIdeal", IsSemigroupIdeal and 
 IsTransformationSemigroup);
DeclareProperty("IsOrthodoxSemigroup", IsTransformationSemigroup);
DeclareProperty("IsRectangularBand", IsTransformationSemigroup);
DeclareProperty("IsRightZeroSemigroup", IsTransformationSemigroup);
DeclareSynonymAttr("IsSemiBand", IsIdempotentGenerated);
DeclareSynonymAttr("IsSemigroupWithCommutingIdempotents", IsBlockGroup);

if not IsBound(IsSemilatticeAsSemigroup) then 
  DeclareProperty("IsSemilatticeAsSemigroup", IsSemigroup);
  InstallTrueMethod(IsSemilatticeAsSemigroup, IsCommutative and IsBand);
fi;

DeclareProperty("IsZeroSemigroup", IsTransformationSemigroup);
DeclareAttribute("MinimalIdeal", IsTransformationSemigroup);
DeclareAttribute("UnderlyingDClassOfMinIdeal", IsSemigroupIdeal and 
 IsTransformationSemigroup);

# the following have been temporarily move to dev/properties.gi until
# ClosureSemigroup is released.

#DeclareOperation("IrredundantGeneratingSubset", [IsTransformationCollection]);
#SmallGeneratingSet



