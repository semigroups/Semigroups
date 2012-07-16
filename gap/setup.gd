#############################################################################
###
##W  setup.gd
##Y  Copyright (C) 2011-12                                James D. Mitchell
###
###  Licensing information can be found in the README file of this package.
###
##############################################################################
###

# action for use in LambdaOrb etc..
DeclareAttribute("RhoAct", IsActingSemigroup);
DeclareAttribute("LambdaAct", IsActingSemigroup);

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

