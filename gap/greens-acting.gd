#############################################################################
##
#W  greens-acting.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

# Green's relations, classes, etc for acting semigroups.

# LambdaRhoLookup(d)[i]=j if orbit[j][4] in reps[i] (orbit[j][4] is one of the
# R-reps of the D-class d) and LambdaRhoLookup(d) is only bound for those
# indices i where there is an R-rep in the scc of the D-class in reps[i]

DeclareAttribute("LambdaRhoLookup", IsGreensDClass and
IsActingSemigroupGreensClass);

DeclareAttribute("LambdaOrb", IsActingSemigroupGreensClass);

DeclareAttribute("LambdaOrbSCC", IsActingSemigroupGreensClass);
DeclareAttribute("LambdaOrbSCCIndex", IsActingSemigroupGreensClass);

DeclareAttribute("RhoOrbSCC", IsActingSemigroupGreensClass);
DeclareAttribute("RhoOrbSCCIndex", IsActingSemigroupGreensClass);

DeclareAttribute("LambdaCosets", IsActingSemigroupGreensClass);
DeclareAttribute("RhoCosets", IsActingSemigroupGreensClass);

DeclareAttribute("SemigroupDataSCC", IsActingSemigroupGreensClass);
DeclareAttribute("SemigroupDataSCCIndex", IsActingSemigroupGreensClass);
DeclareAttribute("SemigroupDataIndex", IsActingSemigroupGreensClass);
