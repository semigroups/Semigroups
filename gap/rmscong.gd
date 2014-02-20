# NEW
DeclareCategory("IsRMSCongruenceByLinkedTriple", IsSemigroupCongruence and IsAttributeStoringRep);
DeclareGlobalFunction("RMSCongruenceByLinkedTripleNC");

# OLD (This code to be reformed in new functions)
DeclareAttribute("LinkedTriple", IsSemigroupCongruence);
DeclareOperation("SemigroupCongruenceByLinkedTriple",
[IsReesZeroMatrixSemigroup and IsFinite,
 IsGroup,
 IsDenseList,
 IsDenseList]);