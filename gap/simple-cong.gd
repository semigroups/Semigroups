DeclareCategory("IsSimpleSemigroupCongruence",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareCategory("IsZeroSimpleSemigroupCongruence",
        IsSemigroupCongruence and IsAttributeStoringRep);

DeclareGlobalFunction("SimpleSemigroupCongruenceNC");
DeclareAttribute("CongruencesOfSemigroup", IsSimpleSemigroup and IsFinite);
DeclareAttribute("CongruencesOfSemigroup", IsZeroSimpleSemigroup and IsFinite);

DeclareCategory("IsSimpleSemigroupCongruenceClass",
        IsEquivalenceClass and IsAttributeStoringRep and IsAssociativeElement);
DeclareGlobalFunction("SimpleSemigroupCongruenceClassNC");