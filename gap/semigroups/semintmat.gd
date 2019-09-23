############################################################################
##
##  semigroups/semintmat.gd
##  Copyright (C) 2022                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsIntegerMatrixSemigroup", IsSemigroup);
DeclareCategory("IsIntegerMatrixMonoid",
                IsMonoid and IsIntegerMatrixSemigroup);
