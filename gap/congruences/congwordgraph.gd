############################################################################
##
##  congruences/congwordgraph.gd
##  Copyright (C) 2022                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
############################################################################
##
## This file contains declarations for left, right, and two-sided
## congruences that are defined in terms of a IsWordGraph.

DeclareCategory("IsCongruenceByWordGraph",
                IsLeftRightOrTwoSidedCongruence
                and CanComputeEquivalenceRelationPartition
                and IsAttributeStoringRep
                and IsFinite);

DeclareOperation("RightCongruenceByWordGraphNC", [IsSemigroup, IsWordGraph]);
DeclareOperation("LeftCongruenceByWordGraphNC", [IsSemigroup, IsWordGraph]);
DeclareAttribute("WordGraph", IsCongruenceByWordGraph);
