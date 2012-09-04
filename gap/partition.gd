

DeclareCategory("IsBipartition", IsMultiplicativeElementWithOne and
 IsAssociativeElement);
DeclareCategoryCollections("IsBipartition");

DeclareSynonym("IsBipartitionSemigroup", IsSemigroup and
IsBipartitionCollection);

DeclareOperation("AsBipartition", [IsPerm, IsPosInt]);
DeclareAttribute("DegreeOfBipartition", IsBipartition);
DeclareAttribute("DegreeOfBipartitionSemigroup", IsBipartitionSemigroup);
DeclareAttribute("DegreeOfBipartitionCollection", IsBipartitionCollection);

DeclareGlobalFunction("ExtRepBipartition");
DeclareProperty("IsBipartitionSemigroupGreensClass", IsGreensClass);
DeclareOperation("OnRightSignedPartitionWithBipartition", [IsList,
IsBipartition]);
DeclareOperation("OnLeftSignedPartitionWithBipartition", [IsList,
IsBipartition]);
DeclareOperation("RankOfBipartition", [IsBipartition]);
