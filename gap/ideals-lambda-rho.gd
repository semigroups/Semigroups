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

DeclareGlobalFunction("UpdateIdealLambdaOrb");
DeclareGlobalFunction("UpdateIdealRhoOrb");

#DeclareOperation("TraceIdealSchreierTreeForward", [IsIdealOrb, IsPosInt]);

DeclareOperation("Enumerate", [IsIdealOrb, IsCyclotomic, IsFunction]);
DeclareOperation("ComponentOfIndex", [IsIdealOrb, IsPosInt]);

DeclareGlobalFunction("SuffixOrb");
