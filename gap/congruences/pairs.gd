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
## generating pairs, using a union-find method.  See Howie 1.5 and see MT's
## MSc thesis "Computing with Semigroup Congruences", chapter 2
##

DeclareGlobalFunction("SEMIGROUPS_SetupCongData");
DeclareCategory("IsSemigroupCongruenceData", IsRecord);
DeclareOperation("Enumerate", [IsSemigroupCongruenceData, IsFunction]);
