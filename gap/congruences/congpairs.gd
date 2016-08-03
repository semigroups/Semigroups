############################################################################
##
#W  congruences/congpairs.gd
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains functions for any finite semigroup congruence with
## generating pairs, using a pair enumeration and union-find method.
##
## See the header of congpairs.gi for a full summary.
##

DeclareOperation("AsSemigroupCongruenceByGeneratingPairs",
                 [IsSemigroupCongruence]);

# This is representation for left/right and two-sided congruences of a finite
# semigroup.
DeclareRepresentation("IsFiniteCongruenceByGeneratingPairsRep",
                      IsEquivalenceRelation and IsAttributeStoringRep,
                      ["range", "genpairs", "type", "report",
                       "factored_genpairs"]);

DeclareCategory("SEMIGROUPS_IsSemigroupCongruenceData", IsRecord);
DeclareOperation("SEMIGROUPS_Enumerate", [IsEquivalenceRelation, IsFunction]);
DeclareOperation("SEMIGROUPS_Enumerate",
                 [SEMIGROUPS_IsSemigroupCongruenceData, IsFunction]);
