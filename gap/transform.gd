#############################################################################
##
#W  transform.gd
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# full transformations

if not IsBound(AsPermutation) then 
  DeclareOperation("AsPermutation",[IsObject]);
fi;
DeclareOperation("AsPermutationNC",[IsObject]);

DeclareGlobalFunction("ConstantTransformation");
DeclareOperation("DegreeOfTransformationCollection", [IsTransformationCollection]);
DeclareGlobalFunction("Idempotent");
DeclareGlobalFunction("IdempotentNC");
DeclareGlobalFunction("IndexPeriodOfTransformation");
DeclareOperation("InversesOfTransformationNC", [IsTransformationSemigroup,  
 IsTransformation]);
DeclareOperation("InversesOfTransformation", [IsTransformationSemigroup,
 IsTransformation]);
DeclareGlobalFunction("AsPermOfKerImg");
DeclareOperation("IsRegularTransformation", [IsTransformationSemigroup,
 IsTransformation]);
DeclareOperation("IsSubsemigroup", [IsTransformationSemigroup, 
 IsTransformationSemigroup]);
DeclareOperation("RandomIdempotent", [IsCyclotomicCollColl]);
DeclareOperation("RandomIdempotentNC", [IsCyclotomicCollColl]);
DeclareOperation("RandomTransformationNC", [IsCyclotomicCollection, 
 IsCyclotomicCollection]);
DeclareOperation("SmallestIdempotentPower", [IsTransformation]);

