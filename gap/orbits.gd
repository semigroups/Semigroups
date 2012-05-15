#############################################################################
##
#W  orbits.gd
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##


DeclareGlobalFunction("CitrusEvalWord");
DeclareAttribute("GradedImagesOfTransSemigroup", IsTransformationSemigroup); 
DeclareAttribute("GradedKernelsOfTransSemigroup", IsTransformationSemigroup);

DeclareGlobalFunction("HashFunctionForBlist");
DeclareGlobalFunction("HashTableForImages");
DeclareGlobalFunction("HashTableForKernels");

DeclareAttribute("ImagesOfTransSemigroup", IsTransformationSemigroup, 
 "mutable");
DeclareAttribute("ImagesOfTransSemigroupAsBlists", IsTransformationSemigroup,
"mutable");
DeclareAttribute("KernelsOfTransSemigroup", IsTransformationSemigroup, 
 "mutable");

DeclareGlobalFunction("OnBlist");
DeclareGlobalFunction("OnKernelsAntiAction");
DeclareGlobalFunction("OrbSCC");
DeclareGlobalFunction("OrbSCCLookup");
DeclareGlobalFunction("OrbSCCTruthTable");
DeclareGlobalFunction("ReverseSchreierTreeOfSCC");
DeclareGlobalFunction("SchreierTreeOfSCC");
DeclareAttribute("SchutzGps", IsTransformationSemigroup);
DeclareOperation("TransformationActionNC", [IsObject, IsList, IsFunction]);
DeclareAttribute("CitrusSkeleton", IsTransformationSemigroup);
DeclareGlobalFunction("StrongOrbitsInForwardOrbit");
DeclareGlobalFunction("TraceSchreierTreeOfSCCForward");
DeclareGlobalFunction("TraceSchreierTreeOfSCCBack");
