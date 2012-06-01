#############################################################################
###
##W  pperm.gd
##Y  Copyright (C) 2011-12                                James D. Mitchell
###
###  Licensing information can be found in the README file of this package.
###
##############################################################################
###

#JDM this is only here temporarily! See below.
DeclareCategory("IsActingElt", IsMultiplicativeElementWithOne and
IsAssociativeElement);
#JDM IsActingElt should not be necessary below...
# see acting.gd for more details
DeclareCategory("IsPartialPerm", IsMultiplicativeElementWithOne and
 IsAssociativeElement and IsActingElt);
DeclareCategoryCollections("IsPartialPerm");

BindGlobal("PartialPermFamily", NewFamily("PartialPermFamily",
 IsPartialPerm, CanEasilySortElements, CanEasilySortElements));
BindGlobal("PartialPermType", NewType(PartialPermFamily,
 IsPartialPerm and IsDataObjectRep));
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
DeclareGlobalFunction("PrettyPrintPP");
DeclareGlobalFunction("RandomPartialPerm");
DeclareOperation("RangeOfPartialPerm", [IsPartialPerm]);
DeclareOperation("RangeSetOfPartialPerm", [IsPartialPerm]);
DeclareOperation("RankOfPartialPerm", [IsPartialPerm]);
DeclareOperation("RestrictedPartialPermNC", [IsPartialPerm, IsList and
IsCyclotomicCollection]);
DeclareOperation("RestrictedPartialPerm", [IsPartialPerm, IsList and
IsCyclotomicCollection]);

