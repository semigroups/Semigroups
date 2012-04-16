############################################################################# 
## 
#W  closure.gd
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
DeclareAttribute("GeneratorsOfInverseSemigroup", IsInverseSemigroup);
DeclareGlobalFunction("InverseMonoid");
DeclareGlobalFunction("InverseSemigroup");
DeclareOperation("InverseMonoidByGenerators", [IsPartialPermCollection]);
DeclareOperation("InverseSemigroupByGenerators", [IsPartialPermCollection]);
DeclareOperation("InverseSemigroupByGeneratorsNC", [IsPartialPermCollection, IsPartialPermCollection, IsRecord]);
DeclareGlobalFunction("SingularSemigroup");
DeclareGlobalFunction("SymmetricInverseSemigp");
