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

DeclareGlobalFunction("ConstantTransformation");
DeclareOperation("DegreeOfTransformationCollection", [IsTransformationCollection]);
DeclareGlobalFunction("Idempotent");
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

# partial permutations

DeclareGlobalFunction("AsPermOfDomRan");

DeclareRepresentation("IsPartialPerm",
IsPositionalObjectRep and IsMultiplicativeElementWithOne and
IsAttributeStoringRep and IsAssociativeElement, [1]);

DeclareAttribute("Dom", IsPartialPerm);
DeclareAttribute("Ran", IsPartialPerm);

DeclareCategoryCollections("IsPartialPerm");

BindGlobal("PartialPermFamily", NewFamily("PartialPermFamily",
IsPartialPerm));

BindGlobal("PartialPermType", NewType(PartialPermFamily,
IsPartialPerm));

DeclareGlobalFunction("PartialPermNC");
DeclareGlobalFunction("RandomPartialPerm");

