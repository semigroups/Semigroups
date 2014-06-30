# Inverse Congruences By Congruence Pair
DeclareCategory("IsInverseSemigroupCongruence",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareAttribute("CongruencesOfSemigroup",
        IsInverseSemigroup and IsFinite);
DeclareGlobalFunction("InverseSemigroupCongruenceByCongruencePair");
DeclareGlobalFunction("InverseSemigroupCongruenceByCongruencePairNC");

DeclareAttribute("TraceOfSemigroupCongruence", IsSemigroupCongruence);
DeclareAttribute("KernelOfSemigroupCongruence", IsSemigroupCongruence);
DeclareAttribute("AsInverseSemigroupCongruenceByCongruencePair",
        IsSemigroupCongruence);

# Congruence Classes
DeclareCategory("IsInverseSemigroupCongruenceClass",
        IsEquivalenceClass and IsAttributeStoringRep and IsAssociativeElement);
DeclareOperation("InverseSemigroupCongruenceClass",
        [IsInverseSemigroupCongruence, IsAssociativeElement] );
DeclareOperation("InverseSemigroupCongruenceClassNC",
        [IsInverseSemigroupCongruence, IsAssociativeElement] );
