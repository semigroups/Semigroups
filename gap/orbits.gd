#############################################################################
##
#W  orbits.gd
#Y  Copyright (C) 2011-13                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("EvaluateWord", [IsPartialPermCollection, IsList]);
DeclareOperation("EvaluateWord", 
[IsReesZeroMatrixSemigroupElementCollection, IsList]);
DeclareGlobalFunction("EnumeratePosition");
DeclareGlobalFunction("LookForInOrb");
DeclareGlobalFunction("OrbSCC");
DeclareGlobalFunction("OrbSCCLookup");
DeclareGlobalFunction("OrbSCCTruthTable");
DeclareGlobalFunction("ReverseSchreierTreeOfSCC");
DeclareGlobalFunction("SchreierTreeOfSCC");
DeclareGlobalFunction("TraceSchreierTreeOfSCCForward");
DeclareGlobalFunction("TraceSchreierTreeOfSCCBack");

DeclareAttribute("ComponentRepsOfTransformationSemigroup",
IsTransformationSemigroup);
