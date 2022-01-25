#############################################################################
##
##  semigroups/semiex.gd
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Transformation semigroups
DeclareOperation("EndomorphismsPartition", [IsCyclotomicCollection]);
DeclareOperation("OrderEndomorphisms", [IsPosInt]);
DeclareOperation("OrderAntiEndomorphisms", [IsPosInt]);
DeclareOperation("PartialOrderEndomorphisms", [IsPosInt]);
DeclareOperation("PartialOrderAntiEndomorphisms", [IsPosInt]);
DeclareOperation("SingularOrderEndomorphisms", [IsPosInt]);
DeclareOperation("PartialTransformationMonoid", [IsPosInt]);
DeclareOperation("CatalanMonoid", [IsPosInt]);
DeclareOperation("SingularTransformationSemigroup", [IsPosInt]);
DeclareSynonym("SingularTransformationMonoid",
               SingularTransformationSemigroup);

# Partial perm semigroups
DeclareSynonym("RookMonoid", SymmetricInverseMonoid);
DeclareAttribute("MunnSemigroup", IsSemigroup);
DeclareAttribute("GeneratorsOfMunnSemigroup", IsSemigroup);
DeclareOperation("POI", [IsPosInt]);
DeclareOperation("PODI", [IsPosInt]);
DeclareOperation("POPI", [IsPosInt]);
DeclareOperation("PORI", [IsPosInt]);

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

DeclareOperation("DualSymmetricInverseMonoid", [IsInt]);
DeclareOperation("SingularDualSymmetricInverseMonoid", [IsPosInt]);
DeclareOperation("PartialDualSymmetricInverseMonoid", [IsInt]);
DeclareSynonym("DualSymmetricInverseSemigroup", DualSymmetricInverseMonoid);

DeclareOperation("PartitionMonoid", [IsInt]);
DeclareOperation("RookPartitionMonoid", [IsPosInt]);
DeclareOperation("SingularPartitionMonoid", [IsPosInt]);

DeclareOperation("UniformBlockBijectionMonoid", [IsPosInt]);
DeclareOperation("PartialUniformBlockBijectionMonoid", [IsPosInt]);
DeclareOperation("SingularUniformBlockBijectionMonoid", [IsPosInt]);
DeclareOperation("PlanarUniformBlockBijectionMonoid", [IsPosInt]);
DeclareOperation("SingularPlanarUniformBlockBijectionMonoid", [IsPosInt]);
DeclareSynonym("FactorisableDualSymmetricInverseMonoid",
               UniformBlockBijectionMonoid);
DeclareSynonym("SingularFactorisableDualSymmetricInverseMonoid",
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

DeclareOperation("GeneralLinearMonoid", [IsPosInt, IsPosInt]);
DeclareSynonym("GLM", GeneralLinearMonoid);
DeclareSynonym("FullMatrixMonoid", GeneralLinearMonoid);

DeclareOperation("SpecialLinearMonoid", [IsPosInt, IsPosInt]);
DeclareSynonym("SLM", SpecialLinearMonoid);

# Boolean matrix monoids

DeclareOperation("RegularBooleanMatMonoid", [IsPosInt]);
DeclareOperation("GossipMonoid", [IsPosInt]);
DeclareOperation("UnitriangularBooleanMatMonoid", [IsPosInt]);
DeclareOperation("TriangularBooleanMatMonoid", [IsPosInt]);

DeclareOperation("ReflexiveBooleanMatMonoid", [IsPosInt]);
DeclareOperation("HallMonoid", [IsPosInt]);
DeclareOperation("FullBooleanMatMonoid", [IsPosInt]);

# Tropical matrix monoids

DeclareOperation("FullTropicalMaxPlusMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("FullTropicalMinPlusMonoid", [IsPosInt, IsPosInt]);

# PBR monoids

DeclareOperation("FullPBRMonoid", [IsPosInt]);
