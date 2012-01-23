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

DeclareCategory("IsPartialPerm", IsMultiplicativeElementWithOne and   
 IsAssociativeElement);
DeclareCategoryCollections("IsPartialPerm");
DeclareRepresentation("IsPartialPermRep",
IsPositionalObjectRep, [1]);
BindGlobal("PartialPermFamily", NewFamily("PartialPermFamily",
IsPartialPerm));
BindGlobal("PartialPermType", NewType(PartialPermFamily,
IsPartialPerm and IsPartialPermRep));
DeclareGlobalFunction("PartialPermNC");

DeclareAttribute("DegreeOfPartialPerm", IsPartialPerm);
DeclareGlobalFunction("DomainAndRangeOfPartialPerm");
DeclareAttribute("DomainOfPartialPerm", IsPartialPerm);
DeclareSynonymAttr("Dom", DomainOfPartialPerm);
DeclareAttribute("ImageSetOfPartialPerm", IsPartialPerm);
DeclareGlobalFunction("OnIntegerSetsWithPartialPerm");
DeclareAttribute("RangeOfPartialPerm", IsPartialPerm);
DeclareSynonymAttr("Ran", RangeOfPartialPerm);
DeclareAttribute("RankOfPartialPerm", IsPartialPerm);

DeclareGlobalFunction("RandomPartialPerm");

