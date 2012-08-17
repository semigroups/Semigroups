#############################################################################
##
#W  transform.gd
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# new 

DeclareGlobalFunction("InternalRepOfTransformation");
DeclareOperation("TransformationOp", [IsObject, IsList, IsFunction]);
DeclareGlobalFunction("TransformationAction");
DeclareGlobalFunction("TransformationActionNC");
DeclareGlobalFunction("TransformationActionHomomorphism");

# old

if not IsBound(AsPermutation) then 
  DeclareOperation("AsPermutation",[IsObject]);
fi;
DeclareOperation("AsPermutationNC",[IsObject]);

DeclareGlobalFunction("ConstantTransformation");
DeclareOperation("DegreeOfTransformationCollection", [IsTransformationCollection]);
DeclareGlobalFunction("Idempotent");
DeclareGlobalFunction("IdempotentNC");
DeclareGlobalFunction("IndexPeriodOfTransformation");
DeclareGlobalFunction("AsPermOfKerImg");
DeclareOperation("RandomIdempotent", [IsCyclotomicCollColl]);
DeclareOperation("RandomIdempotentNC", [IsCyclotomicCollColl]);
DeclareOperation("RandomTransformationNC", [IsCyclotomicCollection, 
 IsCyclotomicCollection]);
DeclareOperation("SmallestIdempotentPower", [IsTransformation]);

