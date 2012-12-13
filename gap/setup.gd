#############################################################################
###
##W  setup.gd
##Y  Copyright (C) 2011-12                                James D. Mitchell
###
###  Licensing information can be found in the README file of this package.
###
##############################################################################
###


# Rank and degree
DeclareAttribute("ActionDegree", IsAssociativeElement);
DeclareAttribute("ActionRank", IsAssociativeElement);

# action for use in LambdaOrb etc..
DeclareAttribute("RhoAct", IsActingSemigroup);
DeclareAttribute("LambdaAct", IsActingSemigroup);

DeclareAttribute("LambdaOrbOpts", IsActingSemigroup);

# grading for use in GradedLambdaOrb/GradedRhoOrb
DeclareAttribute("LambdaRank", IsActingSemigroup);
DeclareAttribute("RhoRank", IsActingSemigroup);
DeclareAttribute("LambdaDegree", IsActingSemigroup);
DeclareAttribute("RhoDegree", IsActingSemigroup);

# the actual functions lambda and rho
DeclareAttribute("LambdaFunc", IsActingSemigroup);
DeclareAttribute("RhoFunc", IsActingSemigroup);

DeclareAttribute("RhoInverse", IsActingSemigroup);
DeclareAttribute("LambdaInverse", IsActingSemigroup);
DeclareAttribute("LambdaPerm", IsActingSemigroup);
DeclareAttribute("LambdaConjugator", IsActingSemigroup);

DeclareAttribute("LambdaOrbSeed", IsActingSemigroup);
DeclareAttribute("RhoOrbSeed", IsActingSemigroup);

DeclareAttribute("IdempotentTester", IsActingSemigroup);
DeclareAttribute("IdempotentCreator", IsActingSemigroup);

InstallTrueMethod(IsActingSemigroup, IsTransformationSemigroup);
InstallTrueMethod(IsActingSemigroup, IsPartialPermSemigroup);
InstallTrueMethod(IsActingSemigroup, IsBipartitionSemigroup);

InstallTrueMethod(IsActingSemigroupWithInverseOp, IsPartialPermSemigroup and
IsInverseSemigroup);
InstallTrueMethod(IsActingSemigroupWithInverseOp, IsPartialPermSemigroup and
IsRegularSemigroup);

InstallTrueMethod(IsActingSemigroupGreensClass, IsGreensClassOfTransSemigp);
InstallTrueMethod(IsActingSemigroupGreensClass,                                 IsGreensClassOfPartPermSemigroup);

