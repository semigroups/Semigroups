


DeclareGlobalFunction("InitSemigroupe");
DeclareAttribute("SemigroupData", IsFinite and IsSemigroup and
HasGeneratorsOfSemigroup, "mutable");

DeclareOperation("Position", [IsFinite and IsSemigroup and
HasGeneratorsOfSemigroup, IsAssociativeElement, IsZeroCyc]);

DeclareOperation("Enumerate", [IsSemigroup]);
DeclareOperation("Enumerate", [IsSemigroup, IsCyclotomic]);
DeclareOperation("Enumerate", [IsSemigroup, IsCyclotomic, IsFunction]);

