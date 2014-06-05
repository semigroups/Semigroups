############################################################################
##
#W  graded.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# hash table of all lambda values found so far, HTValue of LambdaHT points
# to where the graded orbit is in GradedLambdaOrbs
# only applies in graded case
DeclareAttribute("GradedLambdaHT", IsNonExhaustiveSemigroup, "mutable");
DeclareAttribute("GradedRhoHT", IsNonExhaustiveSemigroup, "mutable");

DeclareGlobalFunction("GradedLambdaOrb");
DeclareGlobalFunction("GradedRhoOrb");
DeclareFilter("IsGradedLambdaOrb", IsLambdaOrb);
DeclareFilter("IsGradedRhoOrb", IsRhoOrb);

DeclareAttribute("GradedLambdaOrbs", IsNonExhaustiveSemigroup, "mutable");
DeclareGlobalFunction("IteratorOfGradedLambdaOrbs");
DeclareAttribute("GradedRhoOrbs", IsNonExhaustiveSemigroup, "mutable");
DeclareProperty("IsGradedLambdaOrbs", IsOrbit);
DeclareProperty("IsGradedRhoOrbs", IsOrbit);

