#############################################################################
###
##W  acting.gd
##Y  Copyright (C) 2011-12                                James D. Mitchell
###
###  Licensing information can be found in the README file of this package.
###
##############################################################################
###

DeclareProperty("IsActingSemigroup", IsSemigroup);

# hash table of all lambda values found so far, HTValue of LambdaHT points
# to where the graded orbit is in GradedLambdaOrbs
# only applies in graded case
DeclareAttribute("GradedLambdaHT", IsActingSemigroup, "mutable");
DeclareAttribute("GradedRhoHT", IsActingSemigroup, "mutable");

# hash table of all valid lambda-rho values found so far, HTValue of
# LambdaRhoHT points to where the existing R-class reps with same lambda-rho
# value are in SemigroupData(s).reps. 
DeclareAttribute("LambdaRhoHT", IsActingSemigroup, "mutable");

# LambdaRhoLookup(d)[i]=j if orbit[j][4] in reps[i] (orbit[j][4] is one of the 
# R-reps of the D-class d)
# and LambdaRhoLookup(d) is only bound for those indices i where 
# there is an R-rep in the scc of the D-class in reps[i] 
DeclareAttribute("LambdaRhoLookup", IsGreensDClass and
 IsActingSemigroupGreensClass);

# long orbits <=> LongOrb
DeclareAttribute("LambdaOrb", IsActingSemigroup, "mutable");
DeclareAttribute("RhoOrb", IsActingSemigroup, "mutable");
DeclareProperty("IsLambdaOrb", IsOrbit);
DeclareProperty("IsRhoOrb", IsOrbit);

DeclareGlobalFunction("LambdaOrbMults");
DeclareGlobalFunction("LambdaOrbMult");
DeclareGlobalFunction("RhoOrbMults");
DeclareGlobalFunction("RhoOrbMult");
DeclareGlobalFunction("LambdaOrbSchutzGp");
DeclareGlobalFunction("LambdaOrbRep");
DeclareGlobalFunction("RhoOrbRep");
DeclareGlobalFunction("RhoOrbSchutzGp");
DeclareGlobalFunction("LambdaOrbStabChain");
DeclareAttribute("RhoOrbStabChain", IsActingSemigroupGreensClass);

DeclareGlobalFunction("RectifyLambda");
DeclareGlobalFunction("RhoPos");
DeclareGlobalFunction("LambdaPos");
DeclareGlobalFunction("RectifyRho");
DeclareGlobalFunction("RectifyInverseRho");
DeclareAttribute("ParentSemigroup", IsObject);

# ForwardOrbitOfImage/Kernel/ShortOrb
DeclareGlobalFunction("GradedLambdaOrb");
DeclareGlobalFunction("GradedRhoOrb");
DeclareProperty("IsGradedLambdaOrb", IsOrbit);
DeclareProperty("IsGradedRhoOrb", IsOrbit);

# list of short orbits <=> OrbitsOfImages
DeclareAttribute("GradedLambdaOrbs", IsActingSemigroup, "mutable");
DeclareAttribute("GradedRhoOrbs", IsActingSemigroup, "mutable");
DeclareProperty("IsGradedLambdaOrbs", IsOrbit);
DeclareProperty("IsGradedRhoOrbs", IsOrbit);

DeclareAttribute("SemigroupData", IsActingSemigroup, "mutable");
DeclareCategory("IsSemigroupData", IsRecord);

# INVERSE SEMIGROUPS

DeclareProperty("IsActingSemigroupWithInverseOp", IsActingSemigroup and
IsInverseSemigroup);
DeclareCategory("IsInverseActingElt", IsMultiplicativeElementWithOne and
IsAssociativeElement);
DeclareCategoryCollections("IsInverseActingElt");

#EOF
