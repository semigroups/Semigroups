############################################################################
##
#W  pairs-cong.gd
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains functions for any finite semigroup congruence with
## generating pairs, using a union-find method.
##

DeclareGlobalFunction("SEMIGROUPS_SetupCongData");
DeclareAttribute("AsLookupTable", IsSemigroupCongruence);
DeclareGlobalFunction("SEMIGROUPS_UF_Find");
DeclareGlobalFunction("SEMIGROUPS_UF_Union");

DeclareCategory("IsSemigroupCongruenceData", IsRecord);
DeclareOperation("Enumerate", [IsSemigroupCongruenceData, IsFunction]);
DeclareOperation("Enumerate", [IsSemigroupCongruence, IsFunction]);
