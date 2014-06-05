#############################################################################
###
##W  setup.gd
##Y  Copyright (C) 2013-14                                James D. Mitchell
###
###  Licensing information can be found in the README file of this package.
###
##############################################################################
###

DeclareCategory("IsNonExhaustiveSemigroup", IsSemigroup and IsFinite);
DeclareCategory("IsExhaustiveSemigroup", IsSemigroup and IsFinite);

DeclareCategory("IsNonExhaustiveSemigroupWithInverseOp", IsNonExhaustiveSemigroup);
DeclareProperty("IsGeneratorsOfNonExhaustiveSemigroup", IsAssociativeElementCollection);
DeclareCategory("IsNonExhaustiveSemigroupGreensClass", IsGreensClass);

DeclareAttribute("ActionDegree", IsAssociativeElement);
DeclareAttribute("ActionDegree", IsAssociativeElementCollection);
DeclareAttribute("ActionRank", IsSemigroup);
DeclareOperation("ActionRank", [IsAssociativeElement, IsInt]);
DeclareAttribute("MinActionRank", IsSemigroup);

DeclareAttribute("RhoAct", IsSemigroup);
DeclareAttribute("LambdaAct", IsSemigroup);

DeclareAttribute("LambdaOrbOpts", IsSemigroup);
DeclareAttribute("RhoOrbOpts", IsSemigroup);

DeclareAttribute("LambdaRank", IsSemigroup);
DeclareAttribute("RhoRank", IsSemigroup);

DeclareAttribute("LambdaFunc", IsSemigroup);
DeclareAttribute("RhoFunc", IsSemigroup);

DeclareAttribute("RhoInverse", IsSemigroup);
DeclareAttribute("LambdaInverse", IsSemigroup);
DeclareAttribute("LambdaPerm", IsSemigroup);
DeclareAttribute("LambdaConjugator", IsSemigroup);

DeclareAttribute("LambdaOrbSeed", IsSemigroup);
DeclareAttribute("RhoOrbSeed", IsSemigroup);

DeclareAttribute("IdempotentTester", IsSemigroup);
DeclareAttribute("IdempotentCreator", IsSemigroup);

DeclareProperty("IsNonExhaustiveSemigroupWithFixedDegreeMultiplication",
IsSemigroup);

DeclareAttribute("StabilizerAction", IsSemigroup);

DeclareOperation("FakeOne", [IsAssociativeElementCollection]);

DeclareGlobalFunction("ORB_HashFunctionReesZeroMatrixSemigroupElements");

#EOF
