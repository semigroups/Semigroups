

DeclareCategory("IsBipartition", IsMultiplicativeElementWithOne and
 IsAssociativeElementWithAction);
DeclareCategoryCollections("IsBipartition");

DeclareGlobalFunction("BipartitionNC");

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
DeclareOperation("OnRightSignedPartition", [IsList, IsBipartition]);
DeclareOperation("OnLeftSignedPartition", [IsList, IsBipartition]);
DeclareOperation("RandomBipartition", [IsPosInt]);
DeclareOperation("RankOfBipartition", [IsBipartition]);
