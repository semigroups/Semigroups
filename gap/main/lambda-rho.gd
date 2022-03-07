############################################################################
##
##  main/lambda-rho.gd
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("LambdaOrb", IsActingSemigroup, "mutable");
DeclareAttribute("RhoOrb", IsActingSemigroup, "mutable");
DeclareFilter("IsLambdaOrb", IsOrbit);
DeclareFilter("IsRhoOrb", IsOrbit);
DeclareFilter("IsInverseOrb", IsOrbit);

DeclareGlobalFunction("LambdaOrbMults");
DeclareGlobalFunction("LambdaOrbMult");
DeclareGlobalFunction("RhoOrbMults");
DeclareGlobalFunction("RhoOrbMult");
DeclareGlobalFunction("LambdaOrbSchutzGp");
DeclareGlobalFunction("LambdaOrbRep");
DeclareGlobalFunction("RhoOrbRep");
DeclareGlobalFunction("RhoOrbSchutzGp");
DeclareGlobalFunction("LambdaOrbStabChain");
DeclareOperation("RhoOrbStabChain", [IsOrbit, IsPosInt]);

DeclareOperation("RelativeLambdaOrb",
                 [IsActingSemigroup, IsActingSemigroup]);
