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
DeclareOperation("PartialTransformationMonoid", [IsPosInt]);
DeclareOperation("CatalanMonoid", [IsPosInt]);
DeclareOperation("SingularTransformationSemigroup", [IsPosInt]);
DeclareSynonym("SingularTransformationMonoid",
               SingularTransformationSemigroup);

# Partial perm semigroups
DeclareAttribute("MunnSemigroup", IsSemigroup);
DeclareAttribute("GeneratorsOfMunnSemigroup", IsSemigroup);
DeclareOperation("POI", [IsPosInt]);
DeclareOperation("POPI", [IsPosInt]);

# Bipartition semigroups
DeclareOperation("JonesMonoid", [IsInt]);
DeclareOperation("SingularJonesMonoid", [IsPosInt]);
DeclareSynonym("TemperleyLiebMonoid", JonesMonoid);
DeclareOperation("AnnularJonesMonoid", [IsInt]);
DeclareOperation("PartialJonesMonoid", [IsInt]);

DeclareOperation("MotzkinMonoid", [IsInt]);

DeclareOperation("BrauerMonoid", [IsInt]);
DeclareOperation("SingularBrauerMonoid", [IsPosInt]);
DeclareOperation("PartialBrauerMonoid", [IsInt]);

DeclareOperation("DualSymmetricInverseSemigroup", [IsInt]);
DeclareOperation("SingularDualSymmetricInverseSemigroup", [IsPosInt]);
DeclareSynonym("DualSymmetricInverseMonoid", DualSymmetricInverseSemigroup);

DeclareOperation("PartitionMonoid", [IsInt]);
DeclareOperation("SingularPartitionMonoid", [IsPosInt]);

DeclareOperation("UniformBlockBijectionMonoid", [IsPosInt]);
DeclareOperation("SingularUniformBlockBijectionMonoid", [IsPosInt]);
DeclareOperation("PlanarUniformBlockBijectionMonoid", [IsPosInt]);
DeclareOperation("SingularPlanarUniformBlockBijectionMonoid", [IsPosInt]);
DeclareSynonym("FactorisableDualSymmetricInverseSemigroup",
               UniformBlockBijectionMonoid);
DeclareSynonym("SingularFactorisableDualSymmetricInverseSemigroup",
               SingularUniformBlockBijectionMonoid);

DeclareOperation("ApsisMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("SingularApsisMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("CrossedApsisMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("SingularCrossedApsisMonoid", [IsPosInt, IsPosInt]);

DeclareOperation("PlanarModularPartitionMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("SingularPlanarModularPartitionMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("PlanarPartitionMonoid", [IsPosInt]);
DeclareOperation("SingularPlanarPartitionMonoid", [IsPosInt]);
DeclareOperation("ModularPartitionMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("SingularModularPartitionMonoid", [IsPosInt, IsPosInt]);

# Matrix over finite field semigroups

DeclareOperation("SEMIGROUPS_MatrixSemigroupConstructor",
                 [IsFunction, IsList, IsString, IsString]);
DeclareOperation("SEMIGROUPS_MatrixGroupConstructor", [IsFunction]);
DeclareAttribute("SEMIGROUPS_MatrixSemigroupViewString",
                 IsMatrixSemigroup);
DeclareAttribute("SEMIGROUPS_MatrixSemigroupPrintString",
                 IsMatrixSemigroup);

# TODO rename these to GeneralLinearMonoid
DeclareOperation("GeneralLinearSemigroup", [IsPosInt, IsPosInt]);
DeclareSynonym("GLS", GeneralLinearSemigroup);
DeclareSynonym("FullMatrixSemigroup", GeneralLinearSemigroup);

DeclareOperation("SpecialLinearSemigroup", [IsPosInt, IsPosInt]);
DeclareSynonym("SLS", SpecialLinearSemigroup);

#not implemented or documented
DeclareOperation("PowerSemigroup", [IsGroup]);
DeclareOperation("MonoidOfMultiplicationByN", [IsPosInt]);
