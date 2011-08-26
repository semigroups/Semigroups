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
DeclareGlobalFunction("HashTableForImages");
DeclareGlobalFunction("HashTableForKernels");

DeclareAttribute("ImagesOfTransSemigroup", IsTransformationSemigroup, 
 "mutable");
DeclareAttribute("KernelsOfTransSemigroup", IsTransformationSemigroup, 
 "mutable");

DeclareGlobalFunction("OnKernelsAntiAction");
DeclareGlobalFunction("StrongOrbitsInForwardOrbit");

