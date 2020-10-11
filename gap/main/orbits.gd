#############################################################################
##
##  main/orbits.gd
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("EvaluateWord", [IsMultiplicativeElementCollection, IsList]);
DeclareOperation("EvaluateExtRepObjWord",
                 [IsMultiplicativeElementCollection, IsList]);
DeclareOperation("TraceSchreierTreeOfSCCForward",
                 [IsOrbit, IsPosInt, IsPosInt]);
DeclareOperation("TraceSchreierTreeOfSCCBack",
                 [IsOrbit, IsPosInt, IsPosInt]);
DeclareGlobalFunction("EnumeratePosition");
DeclareGlobalFunction("LookForInOrb");
DeclareGlobalFunction("OrbSCC");
DeclareGlobalFunction("OrbSCCIndex");
DeclareGlobalFunction("OrbSCCLookup");
DeclareGlobalFunction("ReverseSchreierTreeOfSCC");
DeclareGlobalFunction("SchreierTreeOfSCC");
