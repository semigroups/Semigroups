



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

DeclareGlobalFunction("DenseRangeList");
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

DeclareGlobalFunction("MaxDom");
DeclareGlobalFunction("MaxDomRan");
DeclareGlobalFunction("MaxRan");
DeclareGlobalFunction("MinDom");
DeclareGlobalFunction("MinDomRan");
DeclareGlobalFunction("MinRan");
DeclareOperation("RankOfPartialPerm", [IsPartialPerm]);

DeclareGlobalFunction("RandomPartialPerm");
