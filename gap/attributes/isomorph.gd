#############################################################################
##
##  attributes/isomorph.gd
##  Copyright (C) 2014-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("IsomorphismSemigroups", [IsSemigroup, IsSemigroup]);
DeclareAttribute("SmallestMultiplicationTable", IsSemigroup);
DeclareAttribute("CanonicalMultiplicationTable", IsSemigroup);
DeclareAttribute("CanonicalMultiplicationTablePerm", IsSemigroup);
DeclareOperation("OnMultiplicationTable", [IsRectangularTable, IsPerm]);
DeclareOperation("IsIsomorphicSemigroup", [IsSemigroup, IsSemigroup]);
