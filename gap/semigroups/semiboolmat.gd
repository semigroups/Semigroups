############################################################################
##
##  semigroups/semiboolmat.gd
##  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for semigroups of boolean matrices.

DeclareSynonym("IsBooleanMatSemigroup",
               IsSemigroup and IsBooleanMatCollection);

DeclareSynonym("IsBooleanMatMonoid",
               IsMonoid and IsBooleanMatCollection);

InstallTrueMethod(IsFinite, IsBooleanMatSemigroup);

