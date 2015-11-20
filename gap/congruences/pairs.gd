############################################################################
##
#W  congruences/pairs.gd
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains functions for any finite semigroup congruence with
## generating pairs, using a pair enumeration and union-find method.
##
## See the header of pairs.gi for a full summary.
##

DeclareProperty("IsRightSemigroupCongruence", IsLeftSemigroupCongruence);
DeclareProperty("IsLeftSemigroupCongruence", IsRightSemigroupCongruence);
DeclareProperty("IsSemigroupCongruence", IsLeftSemigroupCongruence);
DeclareProperty("IsSemigroupCongruence", IsRightSemigroupCongruence);

DeclareCategory("SEMIGROUPS_IsSemigroupCongruenceData", IsRecord);
DeclareOperation("SEMIGROUPS_Enumerate", [IsEquivalenceRelation, IsFunction]);
DeclareOperation("SEMIGROUPS_Enumerate",
                 [SEMIGROUPS_IsSemigroupCongruenceData, IsFunction]);
DeclareGlobalFunction("DotCongruences");
