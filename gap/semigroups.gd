############################################################################# 
## 
#W  semigroups.gd
#Y  Copyright (C) 2011-12                                 James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

DeclareGlobalFunction("ClosureInverseSemigroup");
DeclareGlobalFunction("ClosureInverseSemigroupNC");
DeclareGlobalFunction("ClosureSemigroup");
DeclareGlobalFunction("ClosureSemigroupNC");
DeclareAttribute("GeneratorsOfInverseMonoid", IsInverseSemigroup);
DeclareAttribute("GeneratorsOfInverseSemigroup", IsInverseSemigroup);
DeclareGlobalFunction("InverseMonoid");
DeclareGlobalFunction("InverseSemigroup");
DeclareOperation("InverseMonoidByGenerators", [IsPartialPermCollection]);
DeclareOperation("InverseSemigroupByGenerators", [IsPartialPermCollection]);
DeclareOperation("InverseMonoidByGeneratorsNC", [IsPartialPermCollection, 
IsPartialPermCollection, IsRecord]);
DeclareOperation("InverseSemigroupByGeneratorsNC", [IsPartialPermCollection, 
IsPartialPermCollection, IsRecord]);
DeclareOperation("RandomInverseMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("RandomInverseSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("RandomTransformationMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("RandomTransformationSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("SubsemigroupByProperty", [IsSemigroup, IsFunction]);

#EOF
