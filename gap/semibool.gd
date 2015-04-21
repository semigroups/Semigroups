############################################################################
##
#W  semibool.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for semigroups of boolean matrices.

DeclareSynonym("IsBooleanMatSemigroup",
               IsBooleanMatCollection and IsSemigroup);
InstallTrueMethod(IsFinite, IsBipartitionSemigroup);
DeclareOperation("RandomBooleanMatSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("RandomBooleanMatMonoid", [IsPosInt, IsPosInt]);
