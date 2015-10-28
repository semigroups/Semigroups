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
DeclareOperation("TriapsisMonoid", [IsPosInt]);
DeclareOperation("PartitionMonoid", [IsPosInt]);
DeclareOperation("SingularPartitionMonoid", [IsPosInt]);

# Matrix over finite field semigroups

DeclareOperation("FullMatrixSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("GeneralLinearSemigroup", [IsPosInt, IsPosInt]);
DeclareProperty("IsFullMatrixSemigroup", IsSemigroup);
DeclareSynonymAttr("IsGeneralLinearSemigroup", IsFullMatrixSemigroup);

#not implemented or documented
DeclareOperation("PowerSemigroup", [IsGroup]);
DeclareOperation("MonoidOfMultiplicationByN", [IsPosInt]);
