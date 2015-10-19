############################################################################
##
#W  semimaxplus.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
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
               IsTropicalMatrixSemigroup and IsMaxPlusMatrixCollection);

DeclareSynonym("IsTropicalMinPlusMatrixSemigroup",
               IsTropicalMatrixSemigroup and IsMinPlusMatrixCollection);

DeclareSynonym("IsProjectiveMaxPlusMatrixSemigroup",
               IsSemigroup and IsProjectiveMaxPlusMatrixCollection);

DeclareSynonym("IsNTPMatrixSemigroup",
               IsSemigroup and IsNTPMatrixCollection);

DeclareSynonym("IsIntegerMatrixSemigroup",
               IsSemigroup and IsIntegerMatrixCollection);
