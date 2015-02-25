#############################################################################
##
#W  greens-acting.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

# Green's relations, classes, etc for acting semigroups.


DeclareAttribute("LambdaOrb", IsActingSemigroupGreensClass, "mutable");
DeclareAttribute("RhoOrb", IsActingSemigroupGreensClass, "mutable");

DeclareAttribute("LambdaOrbSCC", IsActingSemigroupGreensClass);
DeclareAttribute("LambdaOrbSCCIndex", IsActingSemigroupGreensClass);

DeclareAttribute("RhoOrbSCC", IsActingSemigroupGreensClass);
DeclareAttribute("RhoOrbSCCIndex", IsActingSemigroupGreensClass);

DeclareAttribute("LambdaCosets", IsActingSemigroupGreensClass);
DeclareAttribute("RhoCosets", IsActingSemigroupGreensClass);

DeclareAttribute("SemigroupDataIndex", IsActingSemigroupGreensClass);
