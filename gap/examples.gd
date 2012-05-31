############################################################################# 
## 
#W  examples.gd
#Y  Copyright (C) 2011-12                                 James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

DeclareOperation("FullMatrixSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("GeneralLinearSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("MonoidOfMultiplicationByN", [IsPosInt]);
DeclareProperty("IsFullMatrixSemigroup", IsSemigroup);
DeclareSynonymAttr("IsGeneralLinearSemigroup", IsFullMatrixSemigroup);
DeclareProperty("IsMatrixSemigroup", IsSemigroup);
DeclareOperation("MunnSemigroup", [IsSemigroup]);
DeclareOperation("OrderEndomorphisms", [IsPosInt]);
DeclareOperation("POI", [IsPosInt]);
DeclareOperation("POPI", [IsPosInt]);
DeclareOperation("PowerSemigroup", [IsGroup]);
DeclareOperation("SingularSemigroup", [IsPosInt]);
DeclareOperation("SymmetricInverseSemigp", [IsPosInt]);
