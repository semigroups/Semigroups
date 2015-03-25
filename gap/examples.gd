#############################################################################
##
#W  examples.gd
#Y  Copyright (C) 2013-15                                 James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("JonesMonoid", [IsPosInt]);
DeclareOperation("SingularJonesMonoid", [IsPosInt]);
DeclareSynonym("TemperleyLiebMonoid", JonesMonoid);

DeclareOperation("BrauerMonoid", [IsPosInt]);
DeclareOperation("PartialBrauerMonoid", [IsPosInt]);
DeclareOperation("SingularBrauerMonoid", [IsPosInt]);
DeclareOperation("FactorisableDualSymmetricInverseSemigroup", [IsPosInt]);
DeclareOperation("SingularFactorisableDualSymmetricInverseSemigroup",
[IsPosInt]);
DeclareOperation("DualSymmetricInverseSemigroup", [IsPosInt]);
DeclareSynonym("DualSymmetricInverseMonoid", DualSymmetricInverseSemigroup);
DeclareOperation("SingularDualSymmetricInverseSemigroup", [IsPosInt]);

DeclareOperation("EndomorphismsPartition", [IsCyclotomicCollection]);

DeclareOperation("TriapsisMonoid", [IsPosInt]);
# DeclareProperty("IsFullMatrixSemigroup", IsSemigroup);
# DeclareSynonymAttr("IsGeneralLinearSemigroup", IsFullMatrixSemigroup);
DeclareOperation("MunnSemigroup", [IsSemigroup]);
DeclareOperation("OrderEndomorphisms", [IsPosInt]);
DeclareOperation("SingularOrderEndomorphisms", [IsPosInt]);
DeclareOperation("PartialTransformationSemigroup", [IsPosInt]);
DeclareOperation("PartitionMonoid", [IsPosInt]);
DeclareOperation("SingularPartitionMonoid", [IsPosInt]);
DeclareOperation("POI", [IsPosInt]);
DeclareOperation("POPI", [IsPosInt]);
DeclareOperation("RegularBinaryRelationSemigroup", [IsPosInt]);
DeclareOperation("SingularTransformationSemigroup", [IsPosInt]);
DeclareSynonym("SingularTransformationMonoid",
SingularTransformationSemigroup);


# Matrix semigroups . . .

DeclareOperation("GeneralLinearSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("SpecialLinearSemigroup", [IsPosInt, IsPosInt]);
DeclareSynonym("GLS", GeneralLinearSemigroup);
DeclareSynonym("SLS", SpecialLinearSemigroup);
DeclareSynonym("FullMatrixSemigroup", GeneralLinearSemigroup);

#not implemented or documented
DeclareOperation("PowerSemigroup", [IsGroup]);
DeclareOperation("MonoidOfMultiplicationByN", [IsPosInt]);
