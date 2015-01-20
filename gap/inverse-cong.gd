############################################################################
##
#W  inverse-cong.gd
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for congruences on inverse semigroups, using the
## "kernel and trace" representation - see Howie 5.3
##

# Inverse Congruences By Congruence Pair
DeclareCategory("IsInverseSemigroupCongruenceByKernelTrace",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareGlobalFunction("InverseSemigroupCongruenceByCongruencePair");
DeclareGlobalFunction("InverseSemigroupCongruenceByCongruencePairNC");

DeclareAttribute("TraceOfSemigroupCongruence", IsSemigroupCongruence);
DeclareAttribute("KernelOfSemigroupCongruence", IsSemigroupCongruence);
DeclareAttribute("AsInverseSemigroupCongruenceByCongruencePair",
        IsSemigroupCongruence);

DeclareGlobalFunction("SEMIGROUPS_InverseCongFromPairs");

# Congruence Classes
DeclareCategory("InverseSemigroupCongruenceClassByKernelTrace",
        IsEquivalenceClass and IsAttributeStoringRep and IsAssociativeElement);
DeclareOperation("InverseSemigroupCongruenceClass",
        [IsInverseSemigroupCongruenceByKernelTrace, IsAssociativeElement] );
DeclareOperation("InverseSemigroupCongruenceClassNC",
        [IsInverseSemigroupCongruenceByKernelTrace, IsAssociativeElement] );
