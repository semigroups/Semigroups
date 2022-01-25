#############################################################################
##
##  semigroups/semidp.gd
##  Copyright (C) 2017-2022                                 Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for creating direct products of semigroups

# DirectProductOp was only declared for [IsList, IsGroup] before GAP 4.12, so
# the next 4 lines can be removed when Semigroups requires at least GAP 4.12.
if not List([IsList, IsSemigroup], FLAGS_FILTER)
    in GET_OPER_FLAGS(DirectProductOp) then
  DeclareOperation("DirectProductOp", [IsList, IsSemigroup]);
fi;

DeclareAttribute("SemigroupDirectProductInfo", IsSemigroup, "mutable");
