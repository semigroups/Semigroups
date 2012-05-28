



DeclareProperty("IsActingSemigroup", IsSemigroup);
DeclareProperty("IsActingSemigroupElt", IsMultiplicativeElement and
IsAssociativeElement);

# action for use in LambdaOrb etc..
DeclareAttribute("RhoAct", IsActingSemigroup);
DeclareAttribute("LambdaAct", IsActingSemigroup);

# grading for use in GradedLambdaOrb/GradedRhoOrb
DeclareAttribute("LambdaGrading", IsActingSemigroup);
DeclareAttribute("RhoGrading", IsActingSemigroup);

# the actual functions lambda and rho
DeclareAttribute("LambdaFunc", IsActingSemigroup);
DeclareAttribute("RhoFunc", IsActingSemigroup);

# hash table of all lambda/rho values found so far
DeclareAttribute("LambdaHT", IsActingSemigroup);
DeclareAttribute("RhoHT", IsActingSemigroup);

# long orbits <=> LongOrb
DeclareAttribute("LambdaOrb", IsActingSemigroup);
DeclareAttribute("RhoOrb", IsActingSemigroup);
DeclareProperty("IsLambdaOrb", IsOrbit);
DeclareProperty("IsRhoOrb", IsOrbit);

DeclareGlobalFunction("LambdaOrbSCCMults");
DeclareGlobalFunction("CreateLambdaOrbSCCMults");
DeclareGlobalFunction("LambdaOrbSchutzGp");
DeclareGlobalFunction("CreateLambdaOrbSchutzGp");

# ForwardOrbitOfImage/Kernel/ShortOrb
DeclareGlobalFunction("GradedLambdaOrb");
DeclareGlobalFunction("GradedRhoOrb");
DeclareProperty("IsGradedLambdaOrb", IsOrbit);
DeclareProperty("IsGradedRhoOrb", IsOrbit);

# list of short orbits <=> OrbitsOfImages

DeclareAttribute("LambdaOrbs", IsActingSemigroup);
DeclareAttribute("RhoOrbs", IsActingSemigroup);

DeclareGlobalFunction("OrbSCCRep");

InstallTrueMethod(IsActingSemigroup, IsTransformationSemigroup);
InstallTrueMethod(IsActingSemigroup, IsPartialPermSemigroup);
InstallTrueMethod(IsActingSemigroupElt, IsTransformation);
InstallTrueMethod(IsActingSemigroupElt, IsPartialPerm);

#EOF
