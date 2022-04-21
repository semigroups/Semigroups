############################################################################
##
##  congruences/congpairs.gd
##  Copyright (C) 2015-2022                               Michael C. Young
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains functions for any semigroup congruence with generating
## pairs; regardless of representation.

DeclareAttribute("GeneratingPairsOfLeftRightOrTwoSidedCongruence",
                 IsLeftRightOrTwoSidedCongruence);

DeclareOperation("AsSemigroupCongruenceByGeneratingPairs",
                 [IsSemigroupCongruence]);
DeclareOperation("AsRightSemigroupCongruenceByGeneratingPairs",
                 [IsRightSemigroupCongruence]);
DeclareOperation("AsLeftSemigroupCongruenceByGeneratingPairs",
                 [IsLeftSemigroupCongruence]);
