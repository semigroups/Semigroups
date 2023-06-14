############################################################################
##
##  main/setup.gd
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains declarations of everything required for a semigroup
# belonging to IsActingSemigroup...

DeclareProperty("IsGeneratorsOfActingSemigroup",
                IsListOrCollection);
DeclareProperty("IsActingSemigroupWithFixedDegreeMultiplication",
                IsActingSemigroup);

DeclareAttribute("ActionDegree", IsMultiplicativeElement);
DeclareAttribute("ActionDegree", IsListOrCollection);
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

DeclareAttribute("StabilizerAction", IsSemigroup);
DeclareAttribute("SchutzGpMembership", IsSemigroup);

DeclareOperation("FakeOne", [IsCollection]);

DeclareOperation("ConvertToInternalElement",
                 [IsSemigroup, IsMultiplicativeElement]);
DeclareOperation("ConvertToExternalElement",
                 [IsSemigroup, IsMultiplicativeElement]);
DeclareOperation("WeakInverse", [IsMultiplicativeElement]);
