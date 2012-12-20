#############################################################################
###
##W  setup.gd
##Y  Copyright (C) 2011-12                                James D. Mitchell
###
###  Licensing information can be found in the README file of this package.
###
##############################################################################
###

DeclareProperty("IsActingSemigroup", IsSemigroup);
DeclareProperty("IsActingSemigroupWithInverseOp", IsSemigroup);
DeclareProperty("IsActingSemigroupGreensClass", IsGreensClass);

#

DeclareAttribute("ActionDegree", IsAssociativeElement);
DeclareAttribute("ActionRank", IsAssociativeElement);

#

DeclareAttribute("RhoAct", IsSemigroup);
DeclareAttribute("LambdaAct", IsSemigroup);

#

DeclareAttribute("LambdaOrbOpts", IsSemigroup);

#

DeclareAttribute("LambdaRank", IsSemigroup);
DeclareAttribute("RhoRank", IsSemigroup);

#

DeclareAttribute("LambdaFunc", IsSemigroup);
DeclareAttribute("RhoFunc", IsSemigroup);

#

DeclareAttribute("RhoInverse", IsSemigroup);
DeclareAttribute("LambdaInverse", IsSemigroup);
DeclareAttribute("LambdaPerm", IsSemigroup);
DeclareAttribute("LambdaConjugator", IsSemigroup);

#

DeclareAttribute("LambdaOrbSeed", IsSemigroup);
DeclareAttribute("RhoOrbSeed", IsSemigroup);

#

DeclareAttribute("IdempotentTester", IsSemigroup);
DeclareAttribute("IdempotentCreator", IsSemigroup);

#
