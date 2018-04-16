############################################################################
##
##  ideallam.gd
##  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareFilter("IsIdealOrb", IsOrbit);

DeclareGlobalFunction("UpdateIdealLambdaOrb");
DeclareGlobalFunction("UpdateIdealRhoOrb");

DeclareOperation("Enumerate", [IsIdealOrb, IsCyclotomic, IsFunction]);
# DeclareOperation("ComponentOfIndex", [IsIdealOrb, IsPosInt]);

DeclareGlobalFunction("SuffixOrb");
