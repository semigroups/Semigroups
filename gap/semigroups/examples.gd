#############################################################################
##
#W  examples.gd
#Y  Copyright (C) 2013-15                                 James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Transformation semigroups

DeclareOperation("EndomorphismsPartition", [IsCyclotomicCollection]);
DeclareOperation("OrderEndomorphisms", [IsPosInt]);
DeclareOperation("SingularOrderEndomorphisms", [IsPosInt]);
DeclareOperation("PartialTransformationSemigroup", [IsPosInt]);
DeclareOperation("SingularTransformationSemigroup", [IsPosInt]);
DeclareSynonym("SingularTransformationMonoid",
               SingularTransformationSemigroup);

# Partial perm semigroups
DeclareOperation("MunnSemigroup", [IsSemigroup]);
DeclareOperation("POI", [IsPosInt]);
DeclareOperation("POPI", [IsPosInt]);

# Bipartition semigroups
DeclareOperation("JonesMonoid", [IsInt]);
DeclareOperation("PartialJonesMonoid", [IsInt]);
DeclareOperation("MotzkinMonoid", [IsInt]);
DeclareOperation("BrauerMonoid", [IsInt]);
DeclareOperation("PartialBrauerMonoid", [IsInt]);
DeclareOperation("FactorisableDualSymmetricInverseSemigroup", [IsInt]);
DeclareOperation("DualSymmetricInverseSemigroup", [IsInt]);
DeclareOperation("TriapsisMonoid", [IsInt]);
DeclareOperation("PartitionMonoid", [IsInt]);

DeclareSynonym("DualSymmetricInverseMonoid", DualSymmetricInverseSemigroup);
DeclareSynonym("TemperleyLiebMonoid", JonesMonoid);

DeclareOperation("SingularDualSymmetricInverseSemigroup", [IsPosInt]);
DeclareOperation("SingularPartitionMonoid", [IsPosInt]);
DeclareOperation("SingularBrauerMonoid", [IsPosInt]);
DeclareOperation("SingularJonesMonoid", [IsPosInt]);
DeclareOperation("SingularFactorisableDualSymmetricInverseSemigroup",
                 [IsPosInt]);

# Matrix over finite field semigroups

DeclareOperation("FullMatrixSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("GeneralLinearSemigroup", [IsPosInt, IsPosInt]);
DeclareProperty("IsFullMatrixSemigroup", IsSemigroup);
DeclareSynonymAttr("IsGeneralLinearSemigroup", IsFullMatrixSemigroup);

#not implemented or documented
DeclareOperation("PowerSemigroup", [IsGroup]);
DeclareOperation("MonoidOfMultiplicationByN", [IsPosInt]);
