############################################################################
##
#W  ideals-lambda-rho.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareFilter("IsIdealOrb", IsOrbit);
DeclareFilter("IsInverseIdealOrb", IsOrbit);

DeclareGlobalFunction("UpdateIdealLambdaOrb");
DeclareGlobalFunction("UpdateIdealRhoOrb");

DeclareOperation("EvaluateWord", [IsSemigroupIdeal, IsList]);
DeclareOperation("EvaluateWord", [IsSemigroup, IsList]);
DeclareOperation("TraceIdealSchreierTreeForward", [IsIdealOrb, IsPosInt]);

DeclareOperation("Enumerate", [IsIdealOrb, IsCyclotomic, IsFunction]);
DeclareOperation("ComponentOfIndex", [IsIdealOrb, IsPosInt]);

DeclareGlobalFunction("SuffixOrb");
