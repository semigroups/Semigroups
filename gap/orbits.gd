#############################################################################
##
#W  orbits.gd
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("GradedImagesOfTransSemigroup", IsTransformationSemigroup); 
DeclareAttribute("GradedKernelsOfTransSemigroup", IsTransformationSemigroup);

DeclareGlobalFunction("HashFunctionForTransformation");
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
DeclareGlobalFunction("StrongOrbitsInForwardOrbit");

