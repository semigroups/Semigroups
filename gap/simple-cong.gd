DeclareCategory("SEMICONG_SIMPLE",
        IsSemigroupCongruence and IsAttributeStoringRep);

DeclareGlobalFunction("SIMPLECONG_FROM_RMSCONG");
DeclareGlobalFunction("SIMPLECONG_FROM_PAIRS");

DeclareCategory("SEMICONG_SIMPLE_CLASS",
        IsCongruenceClass and IsAttributeStoringRep and IsAssociativeElement);

DeclareGlobalFunction("SIMPLECLASS_FROM_RMSCLASS");

DeclareGlobalFunction("SemigroupCongruence");
DeclareAttribute("CongruencesOfSemigroup", IsSemigroup);