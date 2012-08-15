

DeclareCategory("IsBipartition", IsMultiplicativeElementWithOne and
 IsAssociativeElement);
DeclareCategoryCollections("IsBipartition");

DeclareSynonymAttr("IsBipartitionSemigroup", IsSemigroup and
IsBipartitionCollection);


DeclareOperation("AsBipartition", [IsPerm, IsPosInt]);
DeclareOperation("DegreeOfBipartition", [IsBipartition]);
DeclareGlobalFunction("ExtRepBipartition");
