############################################################################
##
#W  ideals-lambda-rho.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareFilter("IsIdealOrb", IsList);
DeclareFilter("IsIdealLambdaOrb", IsIdealOrb);
DeclareFilter("IsIdealRhoOrb", IsIdealOrb);

DeclareAttribute("IdealLambdaOrb", IsActingSemigroup and IsSemigroupIdeal,
"mutable");
DeclareAttribute("IdealRhoOrb", IsActingSemigroup and IsSemigroupIdeal,
"mutable");

DeclareGlobalFunction("UpdateIdealLambdaOrb");
DeclareGlobalFunction("UpdateIdealRhoOrb");

DeclareOperation("OrbitGraph", [IsIdealOrb]);

DeclareOperation("EvaluateWord", [IsSemigroup, IsList]);
DeclareOperation("EvaluateWord", [IsSemigroup, IsList, IsFunction]);
