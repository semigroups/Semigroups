



# partial permutations

DeclareCategory("IsPartialPerm", IsMultiplicativeElementWithOne and
 IsAssociativeElement);
DeclareCategoryCollections("IsPartialPerm");
BindGlobal("PartialPermFamily", NewFamily("PartialPermFamily",
 IsPartialPerm));
BindGlobal("PartialPermType", NewType(PartialPermFamily,
 IsPartialPerm and IsPartialPermRep));
DeclareGlobalFunction("PartialPermNC");
DeclareOperation("PartialPerm", [IsCyclotomicColl]);

DeclareAttribute("DegreeOfPartialPerm", IsPartialPerm);
DeclareGlobalFunction("DenseCreatePartPerm");
DeclareAttribute("DenseImageListOfPartialPerm", IsPartialPerm);
DeclareGlobalFunction("DomainAndRangeOfPartialPerm");
DeclareAttribute("DomainOfPartialPerm", IsPartialPerm);
DeclareSynonymAttr("Dom", DomainOfPartialPerm);
DeclareGlobalFunction("InternalRepOfPartialPerm");
DeclareAttribute("MaxDomain", IsPartialPerm);
DeclareAttribute("MaxDomainRange", IsPartialPerm);
DeclareAttribute("MaxRange", IsPartialPerm);
DeclareAttribute("MinDomain", IsPartialPerm);
DeclareAttribute("MinDomainRange", IsPartialPerm);
DeclareAttribute("MinRange", IsPartialPerm);
DeclareAttribute("IsEmptyPartialPerm", IsPartialPerm);
DeclareGlobalFunction("OnIntegerSetsWithPartialPerm");
DeclareAttribute("RangeOfPartialPerm", IsPartialPerm);
DeclareAttribute("RangeSetOfPartialPerm", IsPartialPerm);
DeclareSynonymAttr("Ran", RangeOfPartialPerm);
DeclareAttribute("RankOfPartialPerm", IsPartialPerm);
DeclareGlobalFunction("RandomPartialPerm");
DeclareGlobalFunction("SparseCreatePartPerm");
