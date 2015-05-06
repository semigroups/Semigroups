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
