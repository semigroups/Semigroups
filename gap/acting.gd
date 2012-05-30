



DeclareProperty("IsActingSemigroup", IsSemigroup);
#DeclareProperty("IsActingSemigroupElt", IsMultiplicativeElementWithOne and
#IsAssociativeElement);
#JDM temp. moved to pperm.gd since InstallTrueMethod(IsActingSemigroupElt,
#IsPartialPerm) refuses to work.

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

# hash table of all lambda/rho values found so far, HTValue of LambdaHT points
# to where the graded orbit is in GradedLambdaOrbs
DeclareAttribute("LambdaHT", IsActingSemigroup, "mutable");
# HTValue of RhoHT points to where in RhoGradedRReps are R-class reps with the
# same lambda orbit and rho value
DeclareAttribute("RhoHT", IsActingSemigroup, "mutable");

# long orbits <=> LongOrb
DeclareAttribute("LambdaOrb", IsActingSemigroup, "mutable");
DeclareAttribute("RhoOrb", IsActingSemigroup, "mutable");
DeclareProperty("IsLambdaOrb", IsOrbit);
DeclareProperty("IsRhoOrb", IsOrbit);

DeclareGlobalFunction("LambdaOrbMults");
DeclareGlobalFunction("LambdaOrbRep");
DeclareGlobalFunction("CreateLambdaOrbMults");
DeclareGlobalFunction("LambdaOrbSchutzGp");
DeclareGlobalFunction("LambdaOrbStabChain");
DeclareGlobalFunction("CreateLambdaOrbGS");

DeclareAttribute("LambdaMult", IsActingSemigroup);
DeclareAttribute("RhoMult", IsActingSemigroup);
DeclareAttribute("LambdaPerm", IsActingSemigroup);
DeclareAttribute("RhoPerm", IsActingSemigroup);

# ForwardOrbitOfImage/Kernel/ShortOrb
DeclareGlobalFunction("GradedLambdaOrb");
DeclareGlobalFunction("GradedRhoOrb");
DeclareProperty("IsGradedLambdaOrb", IsOrbit);
DeclareProperty("IsGradedRhoOrb", IsOrbit);

DeclareGlobalFunction("InGradedLambdaOrbs");

# list of short orbits <=> OrbitsOfImages
DeclareAttribute("GradedLambdaOrbs", IsActingSemigroup, "mutable");
DeclareAttribute("GradedRhoOrbs", IsActingSemigroup, "mutable");
DeclareProperty("IsGradedLambdaOrbs", IsOrbit);
DeclareProperty("IsGradedRhoOrbs", IsOrbit);

DeclareAttribute("LambdaDomain", IsActingSemigroup);
DeclareAttribute("RhoDomain", IsActingSemigroup);

DeclareProperty("IsActingSemigroupGC", IsGreensClass);

# the below is an actual orb obj
DeclareAttribute("ROrb", IsActingSemigroup, "mutable");
DeclareAttribute("RhoGradedRReps", IsActingSemigroup, "mutable");
# the above should be a list of lists organized like GradedLambdaOrbs
# also with a ht storing the Rho grades. 

DeclareGlobalFunction("EnumerateROrb");

InstallTrueMethod(IsActingSemigroup, IsTransformationSemigroup);
InstallTrueMethod(IsActingSemigroup, IsPartialPermSemigroup);
InstallTrueMethod(IsActingSemigroupElt, IsTransformation);
#JDM for some reason the below doesn't work, returns method not found
#InstallTrueMethod(IsActingSemigroupElt, IsPartialPerm);

InstallTrueMethod(IsActingSemigroupGC, IsGreensClassOfTransSemigp);
InstallTrueMethod(IsActingSemigroupGC, IsGreensClassOfPartPermSemigroup);

#EOF
