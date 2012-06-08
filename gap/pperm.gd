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
 IsAssociativeElement and IsPartialTrans); 
DeclareCategoryCollections("IsPartialTrans");
DeclareCategoryCollections("IsPartialPerm");

DeclareGlobalFunction("PartialPermNC");
DeclareGlobalFunction("PartialPerm");
DeclareOperation("AsPartialPerm", [IsObject]);
DeclareOperation("AsPartialPermNC", [IsObject]);
DeclareGlobalFunction("CITRUS_HashFunctionForPP");
DeclareOperation("DegreeOfPartialPerm", [IsPartialPerm]);
DeclareGlobalFunction("DenseRangeList");
DeclareOperation("DomainOfPartialPerm", [IsPartialPerm]);
DeclareOperation("FixedPointsOfPartialPerm", [IsPartialPerm]);
DeclareGlobalFunction("IndexPeriodOfPartialPerm");
DeclareGlobalFunction("InternalRepOfPartialPerm");
DeclareOperation("NaturalLeqPartialPerm", [IsPartialPerm, IsPartialPerm]);
DeclareOperation("OnIntegerSetsWithPartialPerm", [IsCyclotomicCollection, IsPartialPerm]);
DeclareOperation("OnIntegerTuplesWithPartialPerm", [IsCyclotomicCollection, IsPartialPerm]);
DeclareGlobalFunction("PrettyPrintPP");
DeclareGlobalFunction("RandomPartialPerm");
DeclareOperation("RangeOfPartialPerm", [IsPartialPerm]);
DeclareOperation("RangeSetOfPartialPerm", [IsPartialPerm]);
DeclareOperation("RankOfPartialPerm", [IsPartialPerm]);
DeclareOperation("RestrictedPartialPermNC", [IsPartialPerm, IsList and
IsCyclotomicCollection]);
DeclareOperation("RestrictedPartialPerm", [IsPartialPerm, IsList and
IsCyclotomicCollection]);

