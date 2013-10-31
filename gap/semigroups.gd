############################################################################# 
## 
#W  semigroups.gd
#Y  Copyright (C) 2013                                    James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##


DeclareOperation("SemigroupByGenerators",
[IsAssociativeElementCollection, IsRecord]);

DeclareOperation("MonoidByGenerators",
[IsAssociativeElementCollection, IsRecord]);

DeclareOperation("InverseMonoidByGenerators",
[IsAssociativeElementCollection and
IsAssociativeElementCollection, IsRecord]);

DeclareOperation("InverseSemigroupByGenerators",
[IsAssociativeElementCollection, IsRecord]);

DeclareOperation("ClosureInverseSemigroup", [IsActingSemigroupWithInverseOp,
IsAssociativeElementCollection, IsRecord]);
DeclareOperation("ClosureInverseSemigroup",
[IsActingSemigroupWithInverseOp, IsAssociativeElementCollection]);
DeclareOperation("ClosureInverseSemigroup", 
[IsActingSemigroupWithInverseOp, IsAssociativeElement]);
DeclareOperation("ClosureInverseSemigroup",
[IsActingSemigroupWithInverseOp, IsAssociativeElement, IsRecord]);
DeclareGlobalFunction("ClosureInverseSemigroupNC");

DeclareOperation("ClosureSemigroup", 
[IsActingSemigroup, IsAssociativeElementCollection, IsRecord]);
DeclareOperation("ClosureSemigroup", 
[IsActingSemigroup, IsAssociativeElementCollection]);
DeclareOperation("ClosureSemigroup",
[IsActingSemigroup, IsAssociativeElement, IsRecord]);
DeclareOperation("ClosureSemigroup",
[IsActingSemigroup, IsAssociativeElement]);
DeclareGlobalFunction("ClosureSemigroupNC");

DeclareGlobalFunction("RebaseTransformationSemigroupLambdaOrb");

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

DeclareOperation("SubsemigroupByProperty", [IsSemigroup, IsFunction]);
DeclareOperation("SubsemigroupByProperty", 
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsFunction, IsPosInt]);

DeclareOperation("InverseSubsemigroupByProperty", 
[IsSemigroup, IsFunction]);
DeclareOperation("InverseSubsemigroupByProperty", 
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup, IsFunction,       IsPosInt]);

# undoc

DeclareProperty("IsBinaryRelationSemigroup", IsSemigroup);
DeclareGlobalFunction("RegularSemigroup");

#EOF
