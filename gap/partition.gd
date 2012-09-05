

DeclareCategory("IsBipartition", IsMultiplicativeElementWithOne and
 IsAssociativeElement);
DeclareCategoryCollections("IsBipartition");

DeclareSynonym("IsBipartitionSemigroup", IsSemigroup and
IsBipartitionCollection);

DeclareOperation("AsBipartition", [IsPerm, IsPosInt]);
DeclareAttribute("DegreeOfBipartition", IsBipartition);
DeclareAttribute("DegreeOfBipartitionSemigroup", IsBipartitionSemigroup);
DeclareAttribute("DegreeOfBipartitionCollection", IsBipartitionCollection);
DeclareAttribute("RightSignedPartition", IsBipartition);
DeclareAttribute("LeftSignedPartition", IsBipartition);
DeclareGlobalFunction("ExtRepBipartition");
DeclareProperty("IsBipartitionSemigroupGreensClass", IsGreensClass);
DeclareOperation("OnRightSignedPartitionWithBipartition", [IsList,
IsBipartition]);
DeclareOperation("OnLeftSignedPartitionWithBipartition", [IsList,
IsBipartition]);
DeclareOperation("RankOfBipartition", [IsBipartition]);
