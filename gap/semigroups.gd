#############################################################################
##
#W  semigroups.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("SEMIGROUPS_ViewStringPrefix", [IsSemigroup]);
DeclareOperation("SEMIGROUPS_ViewStringSuffix", [IsSemigroup]);

# This is required since IsInverseSemigroup is a property of semigroups and so
# objectifying something using IsInverseSemigroup does not result in a
# semigroup

InstallTrueMethod(IsSemigroup, IsInverseSemigroup);

DeclareCategory("IsSemigroupWithInverseOp",
                IsInverseSemigroup);
DeclareOperation("SemigroupByGenerators",
                 [IsAssociativeElementCollection, IsRecord]);
DeclareOperation("MonoidByGenerators",
                 [IsAssociativeElementCollection, IsRecord]);
DeclareOperation("InverseMonoidByGenerators",
                 [IsAssociativeElementCollection and
                  IsMultiplicativeElementWithOneCollection, IsRecord]);
DeclareOperation("InverseSemigroupByGenerators",
                 [IsAssociativeElementCollection, IsRecord]);

DeclareOperation("ClosureInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsAssociativeElementCollection]);
DeclareOperation("ClosureInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsAssociativeElementCollection,
                  IsRecord]);
DeclareOperation("ClosureInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsAssociativeElement]);
DeclareOperation("ClosureInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsAssociativeElement, IsRecord]);
DeclareGlobalFunction("ClosureInverseSemigroupNC");

DeclareOperation("ClosureSemigroup",
                 [IsSemigroup, IsAssociativeElementCollection, IsRecord]);
DeclareOperation("ClosureSemigroup",
                 [IsSemigroup, IsAssociativeElementCollection]);
DeclareOperation("ClosureSemigroup",
                 [IsSemigroup, IsAssociativeElement, IsRecord]);
DeclareOperation("ClosureSemigroup",
                 [IsSemigroup, IsAssociativeElement]);
DeclareOperation("ClosureSemigroupNC", 
                 [IsSemigroup, IsListOrCollection, IsRecord]);
DeclareGlobalFunction("SEMIGROUPS_AddGenerators");

DeclareAttribute("Generators", IsSemigroup);

DeclareGlobalFunction("RandomSemigroup");
DeclareGlobalFunction("RandomMonoid");

DeclareOperation("RandomBinaryRelationSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("RandomBinaryRelationMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("RandomMatrixSemigroup", [IsRing, IsPosInt, IsPosInt]);
DeclareOperation("RandomBlockGroup", [IsPosInt, IsPosInt]);
DeclareOperation("RandomInverseSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("RandomInverseMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("RandomTransformationMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("RandomTransformationSemigroup", [IsPosInt, IsPosInt]);
DeclareSynonym("RandomPartialPermSemigroup", RandomBlockGroup);
DeclareOperation("RandomPartialPermMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("RandomBipartitionSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("RandomBipartitionMonoid", [IsPosInt, IsPosInt]);

DeclareOperation("SubsemigroupByProperty", [IsSemigroup, IsFunction]);
DeclareOperation("SubsemigroupByProperty",
                 [IsSemigroup, IsFunction, IsPosInt]);

DeclareOperation("InverseSubsemigroupByProperty",
                 [IsSemigroup, IsFunction]);
DeclareOperation("InverseSubsemigroupByProperty",
                 [IsSemigroupWithInverseOp, IsFunction, IsPosInt]);


# undoc

DeclareProperty("IsBinaryRelationSemigroup", IsSemigroup);
DeclareGlobalFunction("RegularSemigroup");
