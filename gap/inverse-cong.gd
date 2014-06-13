# Inverse Congruences By Congruence Pair
DeclareCategory("IsInverseSemigroupCongruence",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareAttribute("CongruencesOfSemigroup",
        IsInverseSemigroup and IsFinite);
DeclareGlobalFunction("InverseSemigroupCongruenceByCongruencePair");
DeclareGlobalFunction("InverseSemigroupCongruenceByCongruencePairNC");

DeclareAttribute("Trace", IsSemigroupCongruence);
DeclareGlobalFunction("CongruencePair");