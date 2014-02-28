# NEW
DeclareCategory("IsRMSCongruenceByLinkedTriple",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareGlobalFunction("RMSCongruenceByLinkedTriple");
DeclareGlobalFunction("RMSCongruenceByLinkedTripleNC");

DeclareCategory("IsCongruenceClassByLinkedTriple",
        IsEquivalenceClass and IsAttributeStoringRep);

# OLD (This code to be reformed in new functions)
DeclareAttribute("LinkedTriple", IsSemigroupCongruence);
DeclareOperation("SemigroupCongruenceByLinkedTriple",
        [IsReesZeroMatrixSemigroup and IsFinite,
         IsGroup,
         IsDenseList,
         IsDenseList]);
