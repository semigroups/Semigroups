############################################################################# 
## 
#W  examples.gd
#Y  Copyright (C) 2013                                    James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

DeclareOperation("JonesMonoid", [IsPosInt]);
DeclareSynonym("TemperleyLiebMonoid", JonesMonoid);

DeclareOperation("BrauerMonoid", [IsPosInt]);
DeclareOperation("FactorisableDualSymmetricInverseSemigroup", [IsPosInt]);
DeclareOperation("DualSymmetricInverseSemigroup", [IsPosInt]);
DeclareOperation("FullBinaryRelationSemigroup", [IsPosInt]);
DeclareOperation("FullMatrixSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("GeneralLinearSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("MonoidOfMultiplicationByN", [IsPosInt]);
DeclareProperty("IsFullMatrixSemigroup", IsSemigroup);
DeclareSynonymAttr("IsGeneralLinearSemigroup", IsFullMatrixSemigroup);
DeclareOperation("MunnSemigroup", [IsSemigroup]);
DeclareOperation("OrderEndomorphisms", [IsPosInt]);
DeclareOperation("PartialTransformationSemigroup", [IsPosInt]);
DeclareOperation("PartitionMonoid", [IsPosInt]);
DeclareOperation("POI", [IsPosInt]);
DeclareOperation("POPI", [IsPosInt]);
DeclareOperation("PowerSemigroup", [IsGroup]);
DeclareOperation("RegularBinaryRelationSemigroup", [IsPosInt]);
DeclareSynonym("RB", RegularBinaryRelationSemigroup);
DeclareOperation("SingularSemigroup", [IsPosInt]);
