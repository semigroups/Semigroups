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

DeclareOperation("AsPartialPerm", [IsPerm, IsSet]);
DeclareOperation("AsPartialPermNC", [IsTransformation and IsTransformationRep]);
DeclareOperation("DegreeOfPartialPerm", [IsPartialPerm]);
DeclareGlobalFunction("DenseRangeList");
DeclareOperation("DomainOfPartialPerm", [IsPartialPerm]);
DeclareOperation("FixedPointsOfPartialPerm", [IsPartialPerm]);
DeclareGlobalFunction("IndexPeriodOfPartialPerm");
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
DeclareOperation("RangeOfPartialPerm", [IsPartialPerm]);
DeclareOperation("RangeSetOfPartialPerm", [IsPartialPerm]);
DeclareOperation("RankOfPartialPerm", [IsPartialPerm]);
DeclareOperation("RestrictedPartialPermNC", [IsPartialPerm, IsList and
IsCyclotomicCollection]);
DeclareOperation("RestrictedPartialPerm", [IsPartialPerm, IsList and
IsCyclotomicCollection]);



