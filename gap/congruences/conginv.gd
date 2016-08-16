############################################################################
##
#W  congruences/conginv.gd
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for congruences on inverse semigroups, using the
## "kernel and trace" representation - see Howie 5.3
##

# Inverse Congruences By Kernel and Trace
DeclareCategory("IsInverseSemigroupCongruenceByKernelTrace",
                IsSemigroupCongruence and IsAttributeStoringRep and IsFinite);
DeclareGlobalFunction("InverseSemigroupCongruenceByKernelTrace");
DeclareGlobalFunction("InverseSemigroupCongruenceByKernelTraceNC");

DeclareAttribute("TraceOfSemigroupCongruence", IsSemigroupCongruence);
DeclareAttribute("KernelOfSemigroupCongruence", IsSemigroupCongruence);
DeclareAttribute("AsInverseSemigroupCongruenceByKernelTrace",
                 IsSemigroupCongruence);

# Congruence Classes
DeclareCategory("IsInverseSemigroupCongruenceClassByKernelTrace",
                IsCongruenceClass and IsAttributeStoringRep and
                IsAssociativeElement);

SEMIGROUPS.InverseCongFromPairs := function(S, pairs)
  local cong;
  cong := SemigroupCongruenceByGeneratingPairs(S, pairs);
  cong := AsInverseSemigroupCongruenceByKernelTrace(cong);
  SetGeneratingPairsOfMagmaCongruence(cong, pairs);
  return cong;
end;

# Special congruences
DeclareAttribute("MinimumGroupCongruence", IsSemigroupWithInverseOp);
