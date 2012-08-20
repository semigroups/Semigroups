#############################################################################
###
##W  pperm.gd
##Y  Copyright (C) 2011-12                                James D. Mitchell
###
###  Licensing information can be found in the README file of this package.
###
##############################################################################
###

# internal use only
DeclareGlobalFunction("DenseRangeList");
DeclareGlobalFunction("InternalRepOfPartialPerm");
DeclareGlobalFunction("CITRUS_HashFunctionForPP");

# everything else
DeclareCategory("IsPartialPerm", IsMultiplicativeElementWithOne and
 IsAssociativeElement); 
DeclareCategoryCollections("IsPartialPerm");

DeclareGlobalFunction("PartialPerm");
DeclareGlobalFunction("PartialPermNC");
DeclareOperation("AsPartialPerm", [IsObject]);
DeclareOperation("AsPartialPermNC", [IsObject]);

DeclareAttribute("DegreeOfPartialPerm", IsPartialPerm);
DeclareAttribute("DegreeOfPartialPermCollection", IsPartialPerm);
DeclareAttribute("DomainOfPartialPerm", IsPartialPerm);
DeclareAttribute("DomainOfPartialPermCollection", IsPartialPermCollection);
DeclareAttribute("FixedPointsOfPartialPerm", IsPartialPerm);
DeclareAttribute("IndexPeriodOfPartialPerm", IsPartialPerm);
DeclareAttribute("RangeOfPartialPerm", IsPartialPerm);
DeclareAttribute("RangeOfPartialPermCollection", IsPartialPermCollection);
DeclareAttribute("RangeSetOfPartialPerm", IsPartialPerm);
DeclareAttribute("RankOfPartialPerm", IsPartialPerm);

DeclareOperation("NaturalLeqPartialPerm", [IsPartialPerm, IsPartialPerm]);
DeclareOperation("OnIntegerSetsWithPartialPerm", [IsCyclotomicCollection, IsPartialPerm]);
DeclareOperation("OnIntegerTuplesWithPartialPerm", [IsCyclotomicCollection, IsPartialPerm]);
DeclareOperation("RestrictedPartialPermNC", [IsPartialPerm, IsList and
IsCyclotomicCollection]);
DeclareOperation("RestrictedPartialPerm", [IsPartialPerm, IsList and
IsCyclotomicCollection]);

DeclareGlobalFunction("PartialPermOp");
DeclareGlobalFunction("PartialPermAction");
DeclareGlobalFunction("InverseSemigroupAction");
DeclareGlobalFunction("PartialPermActionHomomorphism");
DeclareGlobalFunction("InverseSemigroupActionHomomorphism");

DeclareGlobalFunction("PrettyPrintPP");
DeclareGlobalFunction("RandomPartialPerm");

