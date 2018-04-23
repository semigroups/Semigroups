#############################################################################
##
##  semidp.gd
##  Copyright (C) 2017                                      Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for creating direct products of semigroups

DeclareOperation("DirectProductOp", [IsList, IsSemigroup]);

DeclareAttribute("SemigroupDirectProductInfo", IsSemigroup, "mutable");
