############################################################################
##
#W  pairs-cong.gd
#Y  Copyright (C) 2014                                      Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareGlobalFunction("SetupCongData");
DeclareAttribute("AsLookupTable", IsSemigroupCongruence);
DeclareOperation("Enumerate", [IsSemigroupCongruence, IsFunction]);
