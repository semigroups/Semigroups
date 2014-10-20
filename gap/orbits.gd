#############################################################################
##
#W  orbits.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("EvaluateWord", [IsBipartitionCollection, IsList]);
DeclareOperation("EvaluateWord", [IsPartialPermCollection, IsList]);
DeclareOperation("EvaluateWord", 
[IsReesZeroMatrixSemigroupElementCollection, IsList]);
DeclareOperation("TraceSchreierTreeOfSCCForward",
[IsOrbit, IsPosInt, IsPosInt]);
DeclareOperation("TraceSchreierTreeOfSCCBack",
[IsOrbit, IsPosInt, IsPosInt]);
DeclareGlobalFunction("EnumeratePosition");
DeclareGlobalFunction("LookForInOrb");
DeclareGlobalFunction("OrbSCC");
DeclareGlobalFunction("OrbSCCLookup");
DeclareGlobalFunction("OrbSCCTruthTable");
DeclareGlobalFunction("ReverseSchreierTreeOfSCC");
DeclareGlobalFunction("SchreierTreeOfSCC");

