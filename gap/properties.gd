#############################################################################
##
#W  properties.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

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
DeclareOperation("IsRegularSemigroupElementNC",
                 [IsSemigroup, IsMultiplicativeElement and
                               IsAssociativeElement]);
DeclareProperty("IsRightSimple", IsSemigroup);
DeclareSynonymAttr("IsSemigroupWithCommutingIdempotents", IsBlockGroup);
DeclareProperty("IsUnitRegularSemigroup", IsSemigroup);
DeclareProperty("IsZeroRectangularBand", IsSemigroup);
DeclareProperty("IsCongruenceFreeSemigroup", IsSemigroup);
DeclareProperty("IsEUnitaryInverseSemigroup", IsInverseSemigroup);
DeclareProperty("IsSemigroupWithAdjoinedZero", IsSemigroup);

# The following method is invalid. For example, if we have a semigroup of
# bipartitions, which does not consist of partial perm bipartitions or block
# bijections, but which is a group, say, then InverseOp(any element) = fail,
# but it satisfies IsInverseSemigroup, IsRegularStarSemigroup, and
# IsActingSemigroup.
#InstalltrueMethod(IsActingSemigroupWithInverseOp,
#                  IsInverseSemigroup and IsRegularStarSemigroup and
#                  IsActingSemigroup);

InstallTrueMethod(IsAbundantSemigroup, IsRegularSemigroup);
InstallTrueMethod(IsAdequateSemigroup, IsAbundantSemigroup and IsBlockGroup);
InstallTrueMethod(IsBlockGroup, IsInverseSemigroup);
InstallTrueMethod(IsBlockGroup, IsPartialPermSemigroup);
InstallTrueMethod(IsDTrivial, IsSemilattice);
InstallTrueMethod(IsHTrivial, IsLTrivial);
InstallTrueMethod(IsHTrivial, IsRTrivial);
InstallTrueMethod(IsInverseMonoid, IsInverseSemigroup and IsMonoid);
InstallTrueMethod(IsLeftSimple, IsInverseSemigroup and IsGroupAsSemigroup);
InstallTrueMethod(IsLeftZeroSemigroup, IsSemigroup and IsTrivial);
InstallTrueMethod(IsRightZeroSemigroup, IsSemigroup and IsTrivial);
InstallTrueMethod(IsZeroSemigroup, IsSemigroup and IsTrivial);
InstallTrueMethod(IsLTrivial, IsInverseSemigroup and IsRTrivial);
InstallTrueMethod(IsLTrivial, IsDTrivial);
InstallTrueMethod(IsRectangularBand,
                  IsHTrivial and IsCompletelySimpleSemigroup);
InstallTrueMethod(IsRightSimple, IsInverseSemigroup and IsGroupAsSemigroup);
InstallTrueMethod(IsRTrivial, IsInverseSemigroup and IsLTrivial);
InstallTrueMethod(IsRTrivial, IsDTrivial);
InstallTrueMethod(IsSemilattice, IsDTrivial and IsInverseSemigroup);
InstallTrueMethod(IsMonogenicInverseSemigroup,
                  IsInverseSemigroup and IsMonogenicSemigroup);
InstallTrueMethod(IsZeroRectangularBand, IsZeroGroup);
InstallTrueMethod(IsZeroGroup, IsZeroRectangularBand and IsInverseSemigroup);
InstallTrueMethod(IsRegularSemigroup, IsRegularStarSemigroup);
InstallTrueMethod(IsInverseSemigroup, IsGroup);
InstallTrueMethod(IsInverseSemigroup, IsBlockGroup and IsRegularSemigroup);
InstallTrueMethod(IsCommutativeSemigroup, IsZeroSemigroup);
InstallTrueMethod(IsTrivial,
                  IsLeftZeroSemigroup and IsRightZeroSemigroup);
InstallTrueMethod(IsBand, IsRectangularBand);
InstallTrueMethod(IsCompletelySimpleSemigroup, IsSimpleSemigroup and IsFinite);
InstallTrueMethod(IsSemigroupWithAdjoinedZero, IsSemigroup and IsZeroGroup);
