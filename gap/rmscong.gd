DeclareAttribute("LinkedTriples", IsReesMatrixSemigroup);
DeclareAttribute("LinkedTriples", IsReesZeroMatrixSemigroup);
DeclareAttribute("LinkedTriple", IsSemigroupCongruence);

DeclareOperation("SemigroupCongruenceByLinkedTriple",
[IsReesZeroMatrixSemigroup and IsFinite,
 IsGroup,
 IsDenseList,
 IsDenseList]);