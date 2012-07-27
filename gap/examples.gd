############################################################################# 
## 
#W  examples.gd
#Y  Copyright (C) 2011-12                                 James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

DeclareOperation("FullBinaryRelationSemigroup", [IsPosInt]);
DeclareSynonymAttr("B", FullBinaryRelationSemigroup);
DeclareOperation("FullMatrixSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("GeneralLinearSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("MonoidOfMultiplicationByN", [IsPosInt]);
DeclareProperty("IsFullMatrixSemigroup", IsSemigroup);
DeclareSynonymAttr("IsGeneralLinearSemigroup", IsFullMatrixSemigroup);
DeclareOperation("MunnSemigroup", [IsSemigroup]);
DeclareOperation("OrderEndomorphisms", [IsPosInt]);
DeclareOperation("PartialTransformationSemigroup", [IsPosInt]);
DeclareOperation("POI", [IsPosInt]);
DeclareOperation("POPI", [IsPosInt]);
DeclareOperation("PowerSemigroup", [IsGroup]);
DeclareOperation("RegularBinaryRelationSemigroup", [IsPosInt]);
DeclareSynonymAttr("RB", RegularBinaryRelationSemigroup);
DeclareOperation("SingularSemigroup", [IsPosInt]);
DeclareOperation("SymmetricInverseSemigroup", [IsPosInt]);
