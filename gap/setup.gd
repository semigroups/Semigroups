############################################################################
##
#W  setup.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsActingSemigroup", IsSemigroup);
DeclareCategory("IsActingSemigroupWithInverseOp", IsActingSemigroup);
DeclareProperty("IsGeneratorsOfActingSemigroup", IsCollection);
DeclareCategory("IsActingSemigroupGreensClass", IsGreensClass);

DeclareAttribute("ActionDegree", IsMultiplicativeElement);
DeclareAttribute("ActionDegree", IsCollection);
DeclareAttribute("ActionRank", IsSemigroup);
DeclareOperation("ActionRank", [IsMultiplicativeElement, IsInt]);
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
DeclareAttribute("LambdaBound", IsSemigroup);
DeclareAttribute("RhoBound", IsSemigroup);
DeclareAttribute("LambdaIdentity", IsSemigroup);
DeclareAttribute("RhoIdentity", IsSemigroup);
DeclareAttribute("LambdaPerm", IsSemigroup);
DeclareAttribute("LambdaConjugator", IsSemigroup);

DeclareAttribute("LambdaOrbSeed", IsSemigroup);
DeclareAttribute("RhoOrbSeed", IsSemigroup);

DeclareAttribute("IdempotentTester", IsSemigroup);
DeclareAttribute("IdempotentCreator", IsSemigroup);

DeclareProperty("IsActingSemigroupWithFixedDegreeMultiplication",
                IsSemigroup);

DeclareAttribute("StabilizerAction", IsSemigroup);
DeclareAttribute("SchutzGpMembership", IsSemigroup);

DeclareOperation("FakeOne", [IsCollection]);

DeclareGlobalFunction("SEMIGROUPS_HashFunctionRZMSE");
DeclareGlobalFunction("SEMIGROUPS_HashFunctionBipartition");
