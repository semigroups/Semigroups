#############################################################################
##
#W  examples.gd
#Y  Copyright (C) 2013-15                                 James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("MotzkinMonoid", [IsPosInt]);
DeclareOperation("JonesMonoid", [IsPosInt]);
DeclareOperation("PartialJonesMonoid", [IsPosInt]);
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

DeclareConstructor("ZeroSemigroupCons", [IsSemigroup, IsPosInt]);
DeclareConstructor("RectangularBandCons", [IsSemigroup, IsPosInt, IsPosInt]);
DeclareConstructor("MonogenicSemigroupCons",
                   [IsSemigroup, IsPosInt, IsPosInt]);

# Matrix semigroups . . .

DeclareOperation("SEMIGROUPS_MatrixSemigroupConstructor",
                 [IsFunction, IsList, IsString, IsString]);
DeclareOperation("SEMIGROUPS_MatrixGroupConstructor", [IsFunction]);
DeclareAttribute("SEMIGROUPS_MatrixSemigroupViewString",
                 IsMatrixSemigroup);
DeclareAttribute("SEMIGROUPS_MatrixSemigroupPrintString",
                 IsMatrixSemigroup);

# SuzukiSemigroup? ReeSemigroup?

DeclareOperation("GeneralLinearSemigroup", [IsPosInt, IsPosInt]);
DeclareSynonym("GLS", GeneralLinearSemigroup);
DeclareSynonym("FullMatrixSemigroup", GeneralLinearSemigroup);

DeclareOperation("SpecialLinearSemigroup", [IsPosInt, IsPosInt]);
DeclareSynonym("SLS", SpecialLinearSemigroup);

#DeclareOperation("ProjectiveOmegaSemigroup", [IsInt, IsPosInt, IsPosInt]);
#DeclareOperation("GeneralUnitarySemigroup", [IsPosInt, IsPosInt]);
#DeclareSynonym("GUS", GeneralUnitarySemigroup);
#
#DeclareOperation("SpecialUnitarySemigroup", [IsPosInt, IsPosInt]);
#DeclareSynonym("SUS", SpecialUnitarySemigroup);
#
#DeclareOperation("SymplecticSemigroup", [IsPosInt, IsPosInt]);
#DeclareSynonym("SpS", SymplecticSemigroup);
#
#DeclareOperation("GeneralOrthogonalSemigroup", [IsInt, IsPosInt, IsPosInt]);
#DeclareOperation("GeneralOrthogonalSemigroup", [IsPosInt, IsPosInt]);
#DeclareSynonym("GOS", GeneralOrthogonalSemigroup);
#
#DeclareOperation("SpecialOrthogonalSemigroup", [IsInt, IsPosInt, IsPosInt]);
#DeclareOperation("SpecialOrthogonalSemigroup", [IsPosInt, IsPosInt]);
#DeclareSynonym("SOS", SpecialOrthogonalSemigroup);
#
#DeclareOperation("OmegaSemigroup", [IsInt, IsPosInt, IsPosInt]);
#DeclareOperation("OmegaSemigroup", [IsPosInt, IsPosInt]);
#DeclareSynonym("OmegaS", OmegaSemigroup);
#
#DeclareOperation("GeneralSemilinearSemigroup", [IsPosInt, IsPosInt]);
#DeclareSynonym("GammaLS", GeneralSemilinearSemigroup);
#
#DeclareOperation("SpecialSemilinearSemigroup", [IsPosInt, IsPosInt]);
#DeclareSynonym("SigmaLS", SpecialSemilinearSemigroup);

#DeclareOperation("ProjectiveGeneralLinearSemigroup", [IsPosInt, IsPosInt]);
#DeclareSynonym("PGLS", ProjectiveGeneralLinearSemigroup);

#DeclareOperation("ProjectiveSpecialLinearSemigroup", [IsPosInt, IsPosInt]);
#DeclareSynonym("PSLS", ProjectiveSpecialLinearSemigroup);

#DeclareOperation("ProjectiveGeneralUnitarySemigroup", [IsPosInt, IsPosInt]);
#DeclareSynonym("PGUS", ProjectiveGeneralUnitarySemigroup);

#DeclareOperation("ProjectiveSpecialUnitarySemigroup", [IsPosInt, IsPosInt]);
#DeclareSynonym("PSUS", ProjectiveSpecialUnitarySemigroup);

#DeclareOperation("ProjectiveSymplecticSemigroup", [IsPosInt, IsPosInt]);
#DeclareSynonym("PSPS", ProjectiveSymplecticSemigroup);
#DeclareSynonym("PSpS", ProjectiveSymplecticSemigroup);

#DeclareOperation("ProjectiveOmegaSemigroup", [IsPosInt, IsPosInt, IsPosInt]);
#DeclareOperation("ProjectiveOmegaSemigroup", [IsPosInt, IsPosInt]);
#DeclareSynonym("POmegaS", ProjectiveOmegaSemigroup);

#not implemented or documented
DeclareOperation("PowerSemigroup", [IsGroup]);
DeclareOperation("MonoidOfMultiplicationByN", [IsPosInt]);
