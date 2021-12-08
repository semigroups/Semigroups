############################################################################
##
##  congruences/congpairs.gd
##  Copyright (C) 2015-17                                Michael C. Young
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains functions for any semigroup congruence with generating
## pairs.

DeclareAttribute("GeneratingPairsOfAnyCongruence", IsAnyCongruenceCategory);

DeclareOperation("AnyCongruenceByGeneratingPairs",
                 [IsSemigroup, IsHomogeneousList, IsFunction]);

DeclareSynonym("GeneratingPairsOfLeftSemigroupCongruence",
               GeneratingPairsOfLeftMagmaCongruence);
DeclareSynonym("GeneratingPairsOfRightSemigroupCongruence",
               GeneratingPairsOfRightMagmaCongruence);

DeclareOperation("JoinSemigroupCongruences",
[IsAnyCongruenceCategory and HasGeneratingPairsOfAnyCongruence,
 IsAnyCongruenceCategory and HasGeneratingPairsOfAnyCongruence]);
