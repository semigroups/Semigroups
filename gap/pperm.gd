



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
DeclareGlobalFunction("InternalRepOfPartialPerm");
DeclareGlobalFunction("Ran");
DeclareOperation("RangeOfPartialPerm", [IsPartialPerm]);
DeclareOperation("RangeSetOfPartialPerm", [IsPartialPerm]);

# redone above this line

DeclareAttribute("DegreeOfPartialPerm", IsPartialPerm);
DeclareGlobalFunction("DenseCreatePartPerm");
DeclareAttribute("DenseImageListOfPartialPerm", IsPartialPerm);
DeclareGlobalFunction("DomainAndRangeOfPartialPerm");


DeclareAttribute("MaxDomain", IsPartialPerm);
DeclareAttribute("MaxDomainRange", IsPartialPerm);
DeclareAttribute("MaxRange", IsPartialPerm);
DeclareAttribute("MinDomain", IsPartialPerm);
DeclareAttribute("MinDomainRange", IsPartialPerm);
DeclareAttribute("MinRange", IsPartialPerm);
DeclareAttribute("IsEmptyPartialPerm", IsPartialPerm);
DeclareGlobalFunction("OnIntegerSetsWithPartialPerm");
DeclareAttribute("RankOfPartialPerm", IsPartialPerm);
DeclareGlobalFunction("RandomPartialPerm");
DeclareGlobalFunction("SparseCreatePartPerm");
