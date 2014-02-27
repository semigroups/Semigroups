# NEW
DeclareCategory("IsRMSCongruenceByLinkedTriple",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareGlobalFunction("RMSCongruenceByLinkedTriple");
DeclareGlobalFunction("RMSCongruenceByLinkedTripleNC");

DeclareGlobalFunction("LinkedElement");

DeclareCategory("IsCongruenceClassByLinkedTriple", IsEquivalenceClass);

# OLD (This code to be reformed in new functions)
DeclareAttribute("LinkedTriple", IsSemigroupCongruence);
DeclareOperation("SemigroupCongruenceByLinkedTriple",
        [IsReesZeroMatrixSemigroup and IsFinite,
         IsGroup,
         IsDenseList,
         IsDenseList]);
