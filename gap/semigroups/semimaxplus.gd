############################################################################
##
##  semigroups/semimaxplus.gd
##  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains declarations for semigroups of max-plus, min-plus,
# tropical max-plus, tropical min-plus, projective max-plus, NTP, and integer
# matrices.

DeclareSynonym("IsMaxPlusMatrixSemigroup",
               IsSemigroup and IsMaxPlusMatrixCollection);

DeclareSynonym("IsMinPlusMatrixSemigroup",
               IsSemigroup and IsMinPlusMatrixCollection);

DeclareSynonym("IsTropicalMatrixSemigroup",
               IsSemigroup and IsTropicalMatrixCollection);

DeclareSynonym("IsTropicalMaxPlusMatrixSemigroup",
               IsTropicalMatrixSemigroup
               and IsTropicalMaxPlusMatrixCollection);

DeclareSynonym("IsTropicalMinPlusMatrixSemigroup",
               IsTropicalMatrixSemigroup
               and IsTropicalMinPlusMatrixCollection);

DeclareSynonym("IsProjectiveMaxPlusMatrixSemigroup",
               IsSemigroup and IsProjectiveMaxPlusMatrixCollection);

DeclareSynonym("IsNTPMatrixSemigroup",
               IsSemigroup and IsNTPMatrixCollection);

DeclareSynonym("IsMaxPlusMatrixMonoid",
               IsMonoid and IsMaxPlusMatrixCollection);

DeclareSynonym("IsMinPlusMatrixMonoid",
               IsMonoid and IsMinPlusMatrixCollection);

DeclareSynonym("IsTropicalMatrixMonoid",
               IsMonoid and IsTropicalMatrixCollection);

DeclareSynonym("IsTropicalMaxPlusMatrixMonoid",
               IsTropicalMatrixMonoid
               and IsTropicalMaxPlusMatrixCollection);

DeclareSynonym("IsTropicalMinPlusMatrixMonoid",
               IsTropicalMatrixMonoid
               and IsTropicalMinPlusMatrixCollection);

DeclareSynonym("IsProjectiveMaxPlusMatrixMonoid",
               IsMonoid and IsProjectiveMaxPlusMatrixCollection);

DeclareSynonym("IsNTPMatrixMonoid",
               IsMonoid and IsNTPMatrixCollection);

InstallTrueMethod(IsFinite, IsTropicalMaxPlusMatrixSemigroup);
InstallTrueMethod(IsFinite, IsTropicalMinPlusMatrixSemigroup);
InstallTrueMethod(IsFinite, IsNTPMatrixSemigroup);

InstallTrueMethod(IsFinite, IsTropicalMaxPlusMatrixMonoid);
InstallTrueMethod(IsFinite, IsTropicalMinPlusMatrixMonoid);
InstallTrueMethod(IsFinite, IsNTPMatrixMonoid);

# examples

DeclareOperation("NormalizeSemigroup", [IsMaxPlusMatrixSemigroup]);
DeclareProperty("IsTorsion", IsMaxPlusMatrixSemigroup);
