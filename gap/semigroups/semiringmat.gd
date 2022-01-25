############################################################################
##
##  semigroups/semiringmat.gd
##  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for semigroups of matrices over semirings.

DeclareSynonym("IsMatrixOverSemiringSemigroup",
               IsSemigroup and IsMatrixOverSemiringCollection);
DeclareSynonym("IsMatrixOverSemiringMonoid",
               IsMonoid and IsMatrixOverSemiringCollection);
