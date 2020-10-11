############################################################################
##
##  ideals/lambda-rho.gd
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareFilter("IsIdealOrb", IsOrbit);

DeclareGlobalFunction("UpdateIdealLambdaOrb");
DeclareGlobalFunction("UpdateIdealRhoOrb");

DeclareOperation("Enumerate", [IsIdealOrb, IsCyclotomic, IsFunction]);
