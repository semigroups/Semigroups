


DeclareAttribute("PinData", IsFinite and IsSemigroup and
HasGeneratorsOfSemigroup, "mutable");
DeclareCategory("IsPinData", IsList);
DeclareFilter("IsClosedPinData", IsPinData);

DeclareOperation("Enumerate", [IsPinData]);
DeclareOperation("Enumerate", [IsPinData, IsCyclotomic]);
DeclareOperation("Enumerate", [IsPinData, IsCyclotomic, IsFunction]);

