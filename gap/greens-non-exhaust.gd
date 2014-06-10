#############################################################################
##
#W  greens-non-exhaust.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

# this file contains methods for Green's relations and classes of semigroups
# that satisfy IsNonExhaustiveSemigroup.

DeclareProperty("IsGreensClassNC", IsNonExhaustiveSemigroupGreensClass);

# LambdaRhoLookup(d)[i]=j if orbit[j][4] in reps[i] (orbit[j][4] is one of the
# R-reps of the D-class d) and LambdaRhoLookup(d) is only bound for those
# indices i where there is an R-rep in the scc of the D-class in reps[i] 

DeclareAttribute("LambdaRhoLookup", IsGreensDClass and
IsNonExhaustiveSemigroupGreensClass);

DeclareAttribute("LambdaOrbSCC", IsNonExhaustiveSemigroupGreensClass);
DeclareAttribute("LambdaOrbSCCIndex", IsNonExhaustiveSemigroupGreensClass);

DeclareAttribute("RhoOrbSCC", IsNonExhaustiveSemigroupGreensClass);
DeclareAttribute("RhoOrbSCCIndex", IsNonExhaustiveSemigroupGreensClass);

DeclareAttribute("LambdaCosets", IsNonExhaustiveSemigroupGreensClass);
DeclareAttribute("RhoCosets", IsNonExhaustiveSemigroupGreensClass);

DeclareAttribute("NonExhaustiveDataSCC", IsNonExhaustiveSemigroupGreensClass);
DeclareAttribute("NonExhaustiveDataSCCIndex", IsNonExhaustiveSemigroupGreensClass);
DeclareAttribute("NonExhaustiveDataIndex", IsNonExhaustiveSemigroupGreensClass);

#
DeclareGlobalFunction("CreateDClass");
DeclareGlobalFunction("CreateDClassNC");
DeclareGlobalFunction("CreateHClass");
DeclareGlobalFunction("CreateHClassNC");
DeclareGlobalFunction("CreateLClass");
DeclareGlobalFunction("CreateLClassNC");
DeclareGlobalFunction("CreateInverseOpLClassNC");
DeclareGlobalFunction("CreateInverseOpLClass");
DeclareGlobalFunction("CreateRClass");
DeclareGlobalFunction("CreateRClassNC");

#technical...
DeclareGlobalFunction("IsRegularClass@");
DeclareGlobalFunction("Idempotents@");
DeclareGlobalFunction("NrIdempotents@");

