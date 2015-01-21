#############################################################################
##
#W  isomorph.gd
#Y  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("SmallestMultiplicationTable", IsSemigroup);
DeclareOperation("IsomorphismSemigroups", [IsSemigroup, IsSemigroup]);
DeclareOperation("IsIsomorphicSemigroup", [IsSemigroup and
HasGeneratorsOfSemigroup, IsSemigroup and HasGeneratorsOfSemigroup]);

#EOF
