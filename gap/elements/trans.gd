############################################################################
##
#W  trans.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

DeclareAttribute("CanonicalTransformation", IsTransformation);
DeclareOperation("CanonicalTransformation", [IsTransformation, IsInt]);
DeclareOperation("TransformationByImageAndKernel",
                 [IsHomogeneousList, IsCyclotomicCollColl]);
