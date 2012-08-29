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
DeclareAttribute("ActionDegree", IsActingElt);
DeclareAttribute("ActionRank", IsActingElt);

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

DeclareAttribute("LambdaDomain", IsActingSemigroup);
DeclareAttribute("RhoDomain", IsActingSemigroup);

DeclareAttribute("IdempotentLambdaRhoTester", IsActingSemigroup);
DeclareAttribute("IdempotentLambdaRhoCreator", IsActingSemigroup);

InstallTrueMethod(IsActingSemigroup, IsTransformationSemigroup);
InstallTrueMethod(IsActingSemigroup, IsPartialPermSemigroup);
InstallTrueMethod(IsActingSemigroup, IsMatrixSemigroup);
InstallTrueMethod(IsActingSemigroupWithInverseOp, IsPartialPermSemigroup and
IsInverseSemigroup);
InstallTrueMethod(IsActingSemigroupWithInverseOp, IsPartialPermSemigroup and
IsRegularSemigroup);

InstallTrueMethod(IsActingEltCollection, IsRingElementCollCollColl);

InstallTrueMethod(IsActingElt, IsTransformation);
InstallTrueMethod(IsActingElt, IsPartialPerm);
InstallTrueMethod(IsActingElt, IsMatrix);

InstallTrueMethod(IsInverseActingElt, IsPartialPerm);

InstallTrueMethod(IsActingSemigroupGreensClass, IsGreensClassOfTransSemigp);
InstallTrueMethod(IsActingSemigroupGreensClass,                                 IsGreensClassOfPartPermSemigroup);

