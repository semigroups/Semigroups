



# partial permutations

DeclareCategory("IsPartialPerm", IsMultiplicativeElementWithOne and
 IsAssociativeElement);
DeclareCategoryCollections("IsPartialPerm");
BindGlobal("PartialPermFamily", NewFamily("PartialPermFamily",
 IsPartialPerm));
BindGlobal("PartialPermType", NewType(PartialPermFamily,
 IsPartialPerm));
DeclareGlobalFunction("PartialPermNC");
DeclareOperation("PartialPerm", [IsCyclotomicCollection]);

DeclareOperation("DomainOfPartialPerm", [IsPartialPerm]);
DeclareGlobalFunction("Dom", DomainOfPartialPerm);
DeclareOperation("RangeSetOfPartialPerm", [IsPartialPerm]);
DeclareGlobalFunction("Ran");

# redone above this line

DeclareAttribute("DegreeOfPartialPerm", IsPartialPerm);
DeclareGlobalFunction("DenseCreatePartPerm");
DeclareAttribute("DenseImageListOfPartialPerm", IsPartialPerm);
DeclareGlobalFunction("DomainAndRangeOfPartialPerm");


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
DeclareAttribute("RankOfPartialPerm", IsPartialPerm);
DeclareGlobalFunction("RandomPartialPerm");
DeclareGlobalFunction("SparseCreatePartPerm");
