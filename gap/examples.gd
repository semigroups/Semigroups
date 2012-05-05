

DeclareOperation("FullMatrixSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("GeneralLinearSemigroup", [IsPosInt, IsPosInt]);
DeclareGlobalFunction("MonoidOfMultiplicationByN");
DeclareProperty("IsFullMatrixSemigroup", IsSemigroup);
DeclareSynonymAttr("IsGeneralLinearSemigroup", IsFullMatrixSemigroup);
DeclareProperty("IsMatrixSemigroup", IsSemigroup);
DeclareGlobalFunction("POI");
DeclareGlobalFunction("POPI");
DeclareOperation("PowerSemigroup", [IsGroup]);

