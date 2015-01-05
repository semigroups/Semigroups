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

DeclareGlobalFunction("SetupCongData"); #TODO: prepend SEMIGROUPS_
DeclareAttribute("AsLookupTable", IsSemigroupCongruence);
DeclareOperation("Enumerate", [IsSemigroupCongruence, IsFunction]);
