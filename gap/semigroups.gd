#############################################################################
##
#W  semigroups.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsAssociativeElementWithStar", IsAssociativeElement);
DeclareCategoryCollections("IsAssociativeElementWithStar");
DeclareOperation("StarOp", [IsAssociativeElementWithStar]);
DeclareAttribute("Star", IsAssociativeElementWithStar);

DeclareSynonym("IsStarSemigroup",
               IsSemigroup and IsAssociativeElementWithStarCollection);
DeclareSynonym("IsRegularStarSemigroup",
               IsRegularSemigroup and IsAssociativeElementWithStarCollection);

DeclareOperation("InverseOp", [IsAssociativeElementWithStar]);

DeclareOperation("SemigroupByGenerators",
                 [IsAssociativeElementCollection, IsRecord]);
DeclareOperation("MonoidByGenerators",
                 [IsAssociativeElementCollection, IsRecord]);
DeclareOperation("InverseMonoidByGenerators",
                 [IsAssociativeElementCollection and
                  IsAssociativeElementCollection, IsRecord]);
DeclareOperation("InverseSemigroupByGenerators",
                 [IsAssociativeElementCollection, IsRecord]);

DeclareOperation("ClosureInverseSemigroup",
                 [IsActingSemigroupWithInverseOp,
                  IsAssociativeElementCollection, IsRecord]);
DeclareOperation("ClosureInverseSemigroup",
                 [IsActingSemigroupWithInverseOp,
                  IsAssociativeElementCollection]);
DeclareOperation("ClosureInverseSemigroup",
                 [IsActingSemigroupWithInverseOp, IsAssociativeElement]);
DeclareOperation("ClosureInverseSemigroup",
                 [IsActingSemigroupWithInverseOp, IsAssociativeElement,
                  IsRecord]);
DeclareGlobalFunction("ClosureInverseSemigroupNC");

DeclareOperation("ClosureSemigroup",
                 [IsActingSemigroup, IsAssociativeElementCollection,
                  IsRecord]);
DeclareOperation("ClosureSemigroup",
                 [IsActingSemigroup, IsAssociativeElementCollection]);
DeclareOperation("ClosureSemigroup",
                 [IsActingSemigroup, IsAssociativeElement, IsRecord]);
DeclareOperation("ClosureSemigroup",
                 [IsActingSemigroup, IsAssociativeElement]);
DeclareGlobalFunction("ClosureSemigroupNC");

DeclareAttribute("Generators", IsSemigroup);

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
                 [IsActingSemigroup, IsFunction, IsPosInt]);

DeclareOperation("InverseSubsemigroupByProperty",
                 [IsSemigroup, IsFunction]);
DeclareOperation("InverseSubsemigroupByProperty",
                 [IsActingSemigroupWithInverseOp, IsFunction, IsPosInt]);

# undoc

DeclareProperty("IsBinaryRelationSemigroup", IsSemigroup);
DeclareGlobalFunction("RegularSemigroup");
