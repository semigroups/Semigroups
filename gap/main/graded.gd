############################################################################
##
##  main/graded.gd
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# hash table of all lambda values found so far, HTValue of LambdaHT points
# to where the graded orbit is in GradedLambdaOrbs
# only applies in graded case
DeclareAttribute("GradedLambdaHT", IsActingSemigroup, "mutable");
DeclareAttribute("GradedRhoHT", IsActingSemigroup, "mutable");

DeclareGlobalFunction("GradedLambdaOrb");
DeclareGlobalFunction("GradedRhoOrb");
DeclareFilter("IsGradedLambdaOrb", IsLambdaOrb);
DeclareFilter("IsGradedRhoOrb", IsRhoOrb);

DeclareAttribute("GradedLambdaOrbs", IsActingSemigroup, "mutable");
DeclareGlobalFunction("IteratorOfGradedLambdaOrbs");
DeclareAttribute("GradedRhoOrbs", IsActingSemigroup, "mutable");
DeclareProperty("IsGradedLambdaOrbs", IsOrbit);
DeclareProperty("IsGradedRhoOrbs", IsOrbit);
