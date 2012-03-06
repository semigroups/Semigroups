#############################################################################
###
##W  pperm.gd
##Y  Copyright (C) 2011-12                                James D. Mitchell
###
###  Licensing information can be found in the README file of this package.
###
##############################################################################
###

DeclareCategory("IsPartialPerm", IsMultiplicativeElementWithOne and
 IsAssociativeElement);
DeclareCategoryCollections("IsPartialPerm");
BindGlobal("PartialPermFamily", NewFamily("PartialPermFamily",
 IsPartialPerm));
BindGlobal("PartialPermType", NewType(PartialPermFamily,
 IsPartialPerm));
DeclareGlobalFunction("PartialPermNC");
DeclareOperation("PartialPerm", [IsCyclotomicCollection]);

DeclareOperation("AsPartialPermNC", [IsTransformation and IsTransformationRep]);
DeclareGlobalFunction("DenseRangeList");
DeclareOperation("DomainOfPartialPerm", [IsPartialPerm]);
DeclareGlobalFunction("DomPP", DomainOfPartialPerm);
DeclareOperation("FixedPointsOfPartialPerm", [IsPartialPerm]);
DeclareGlobalFunction("InternalRepOfPartialPerm");
DeclareGlobalFunction("MaxDomPP");
DeclareGlobalFunction("MaxDomRanPP");
DeclareGlobalFunction("MaxRanPP");
DeclareGlobalFunction("MinDomPP");
DeclareGlobalFunction("MinDomRanPP");
DeclareGlobalFunction("MinRanPP");
DeclareOperation("OnIntegerSetsWithPartialPerm", [IsCyclotomicCollection, IsPartialPerm]);
DeclareGlobalFunction("PrettyPrintPP");
DeclareGlobalFunction("RandomPartialPerm");
DeclareGlobalFunction("RanPP");
DeclareOperation("RangeOfPartialPerm", [IsPartialPerm]);
DeclareOperation("RangeSetOfPartialPerm", [IsPartialPerm]);
DeclareOperation("RankOfPartialPerm", [IsPartialPerm]);
DeclareOperation("RestrictedPartialPerm", [IsPartialPerm, IsSet and
IsCyclotomicCollection]);



