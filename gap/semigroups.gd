############################################################################# 
## 
#W  semigroups.gd
#Y  Copyright (C) 2013                                    James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

#DeclareProperty("IsGeneratorsOfInverseSemigroup",
#IsAssociativeElementCollection);

#InstallTrueMethod(IsAssociativeElementWithUniqueSemigroupInverse,
#IsBlockBijection);

DeclareOperation("SemigroupByGenerators",
[IsAssociativeElementWithActionCollection, IsRecord]);

DeclareOperation("MonoidByGenerators",
[IsAssociativeElementWithActionCollection, IsRecord]);

DeclareOperation("InverseMonoidByGenerators",
[IsAssociativeElementWithUniqueSemigroupInverseCollection and
IsAssociativeElementWithActionCollection, IsRecord]);

DeclareOperation("InverseSemigroupByGenerators",
[IsAssociativeElementWithUniqueSemigroupInverseCollection and
IsAssociativeElementWithActionCollection, IsRecord]);

DeclareOperation("ClosureInverseSemigroup", [IsActingSemigroupWithInverseOp,
IsAssociativeElementWithActionCollection, IsRecord]);
DeclareOperation("ClosureInverseSemigroup",
[IsActingSemigroupWithInverseOp, IsAssociativeElementWithActionCollection]);
DeclareOperation("ClosureInverseSemigroup", 
[IsActingSemigroupWithInverseOp, IsAssociativeElementWithAction]);
DeclareOperation("ClosureInverseSemigroup",
[IsActingSemigroupWithInverseOp, IsAssociativeElementWithAction, IsRecord]);
DeclareGlobalFunction("ClosureInverseSemigroupNC");

DeclareOperation("ClosureSemigroup", 
[IsActingSemigroup, IsAssociativeElementWithActionCollection, IsRecord]);
DeclareOperation("ClosureSemigroup", 
[IsActingSemigroup, IsAssociativeElementWithActionCollection]);
DeclareOperation("ClosureSemigroup",
[IsActingSemigroup, IsAssociativeElementWithAction]);
DeclareOperation("ClosureSemigroup",
[IsActingSemigroup, IsAssociativeElementWithAction, IsRecord]);
DeclareOperation("ClosureSemigroup", [IsActingSemigroup, IsList and IsEmpty]); #JDM remove me!
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
DeclareOperation("RandomBipartitionSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("RandomBipartitionMonoid", [IsPosInt, IsPosInt]);

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
