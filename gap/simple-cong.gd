DeclareCategory("SEMICONG_SIMPLE",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareCategory("SEMICONG_Z_SIMPLE",
        IsSemigroupCongruence and IsAttributeStoringRep);

DeclareGlobalFunction("SIMPLECONG_FROM_RMSCONG");
DeclareAttribute("CongruencesOfSemigroup", IsSimpleSemigroup and IsFinite);
DeclareAttribute("CongruencesOfSemigroup", IsZeroSimpleSemigroup and IsFinite);

DeclareCategory("SEMICONG_SIMPLE_CLASS",
        IsCongruenceClass and IsAttributeStoringRep and IsAssociativeElement);
DeclareGlobalFunction("SIMPLECLASS_FROM_RMSCLASS");