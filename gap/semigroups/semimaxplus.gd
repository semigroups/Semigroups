############################################################################
##
#W  semigroups-matrix-max-plus.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains declarations for semigroups of max-plus, min-plus,
# tropical max-plus, and tropical min-plus matrices.

DeclareSynonym("IsMaxPlusMatrixSemigroup",
               IsSemigroup and IsMaxPlusMatrixCollection);

DeclareSynonym("IsMinPlusMatrixSemigroup",
               IsSemigroup and IsMinPlusMatrixCollection);

DeclareSynonym("IsTropicalMatrixSemigroup",
               IsSemigroup and IsTropicalMatrixCollection);

DeclareSynonym("IsTropicalMaxPlusMatrixSemigroup",
               IsTropicalMatrixSemigroup and IsMaxPlusMatrixCollection);

DeclareSynonym("IsTropicalMinPlusMatrixSemigroup",
               IsTropicalMatrixSemigroup and IsMinPlusMatrixCollection);

DeclareSynonym("IsProjectiveMaxPlusMatrixSemigroup",
               IsSemigroup and IsProjectiveMaxPlusMatrixCollection);

DeclareSynonym("IsNaturalMatrixSemigroup",
               IsSemigroup and IsNaturalMatrixCollection);

DeclareOperation("RandomMaxPlusMatrixSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("RandomMinPlusMatrixSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("RandomTropicalMaxPlusMatrixSemigroup",
                 [IsPosInt, IsPosInt, IsPosInt]);
DeclareOperation("RandomTropicalMinPlusMatrixSemigroup",
                 [IsPosInt, IsPosInt, IsPosInt]);
DeclareOperation("RandomMaxPlusMatrixMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("RandomMinPlusMatrixMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("RandomTropicalMaxPlusMatrixMonoid",
                 [IsPosInt, IsPosInt, IsPosInt]);
DeclareOperation("RandomTropicalMinPlusMatrixMonoid",
                 [IsPosInt, IsPosInt, IsPosInt]);
