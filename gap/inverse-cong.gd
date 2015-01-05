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
## "kernel and trace" representation.
##

# Inverse Congruences By Congruence Pair
DeclareCategory("SEMIGROUPS_CongInverse",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareGlobalFunction("InverseSemigroupCongruenceByCongruencePair");
DeclareGlobalFunction("InverseSemigroupCongruenceByCongruencePairNC");

DeclareAttribute("TraceOfSemigroupCongruence", IsSemigroupCongruence);
DeclareAttribute("KernelOfSemigroupCongruence", IsSemigroupCongruence);
DeclareAttribute("AsInverseSemigroupCongruenceByCongruencePair",
        IsSemigroupCongruence);

DeclareGlobalFunction("SEMIGROUPS_InverseCongFromPairs");

# Congruence Classes
DeclareCategory("SEMIGROUPS_CongClassInverse",
        IsEquivalenceClass and IsAttributeStoringRep and IsAssociativeElement);
DeclareOperation("InverseSemigroupCongruenceClass",
        [SEMIGROUPS_CongInverse, IsAssociativeElement] );
DeclareOperation("InverseSemigroupCongruenceClassNC",
        [SEMIGROUPS_CongInverse, IsAssociativeElement] );
