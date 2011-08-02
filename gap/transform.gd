#############################################################################
##
#W  transform.gd
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

if not IsBound(AsPermutation) then 
  #JDM should remove AsPermutation from the library
  DeclareOperation("AsPermutation",[IsObject]);
fi;

DeclareGlobalFunction("ConstantTrans");
DeclareOperation("DegreeOfTransformationCollNC", [IsTransformationCollection]);
DeclareGlobalFunction("Idempotent");
DeclareGlobalFunction("IdempotentFromCanonTransImg");
DeclareGlobalFunction("IdempotentNC");
DeclareAttribute("IndexPeriodOfTransformation", IsTransformation);
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
DeclareAttribute("SmallestIdempotentPower", IsTransformation);



