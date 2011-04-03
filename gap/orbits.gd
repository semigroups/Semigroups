#############################################################################
##
#W  orbits.gd
#Y  Copyright (C) 2006-2011                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

DeclareAttribute("GradedImagesOfTransSemigroup", IsTransformationSemigroup); #M
DeclareAttribute("GradedKernelsOfTransSemigroup", IsTransformationSemigroup);#M

DeclareGlobalFunction("HashFunctionForTransformation");
DeclareGlobalFunction("HashTableForImages");
DeclareGlobalFunction("HashTableForKernels");

DeclareAttribute("ImagesOfTransSemigroup", IsTransformationSemigroup, 
 "mutable"); #M
DeclareAttribute("KernelsOfTransSemigroup", IsTransformationSemigroup, 
 "mutable"); #M

DeclareGlobalFunction("OnKernelsAntiAction"); #M
DeclareGlobalFunction("StrongOrbitsInForwardOrbit"); #M

