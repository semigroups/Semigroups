



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

DeclareOperation("DenseRangeList", [IsPartialPerm]);
DeclareOperation("DomainOfPartialPerm", [IsPartialPerm]);
DeclareGlobalFunction("Dom", DomainOfPartialPerm);
DeclareOperation("FixedPointsOfPartialPerm", [IsPartialPerm]);
DeclareGlobalFunction("InternalRepOfPartialPerm");
DeclareOperation("OnIntegerSetsWithPartialPerm", [IsSet and
IsCyclotomicCollection, IsPartialPerm]);
DeclareGlobalFunction("Ran");
DeclareOperation("RangeOfPartialPerm", [IsPartialPerm]);
DeclareOperation("RangeSetOfPartialPerm", [IsPartialPerm]);
DeclareOperation("RestrictedPartialPerm", [IsPartialPerm, IsSet and
IsCyclotomicCollection]);

# redone above this line

DeclareGlobalFunction("DomainAndRangeOfPartialPerm");

DeclareGlobalFunction("MaxDom");
DeclareOperation("MaxDomOfPartialPerm", [IsPartialPerm]);
DeclareGlobalFunction("MaxDomRan");
DeclareOperation("MaxDomRanOfPartialPerm", IsPartialPerm);

DeclareGlobalFunction("MaxRan");
DeclareGlobalFunction("MinDom");
DeclareGlobalFunction("MinDomRan");
DeclareGlobalFunction("MinRan");
DeclareGlobalFunction("RankOfPP");
DeclareGlobalFunction("RandomPartialPerm");
