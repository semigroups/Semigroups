# NEW
DeclareCategory("IsRMSCongruenceByLinkedTriple",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareAttribute("CongruencesOfSemigroup",
        IsReesZeroMatrixSemigroup and IsZeroSimpleSemigroup and IsFinite);
DeclareGlobalFunction("RMSCongruenceByLinkedTriple");
DeclareGlobalFunction("RMSCongruenceByLinkedTripleNC");

DeclareCategory("IsRMSCongruenceClassByLinkedTriple",
        IsEquivalenceClass and IsAttributeStoringRep);
DeclareOperation("RMSCongruenceClassByLinkedTriple",
        [IsRMSCongruenceByLinkedTriple,
         IsRightCoset, IsPosInt, IsPosInt] );
DeclareOperation("RMSCongruenceClassByLinkedTripleNC",
        [IsRMSCongruenceByLinkedTriple,
         IsRightCoset, IsPosInt, IsPosInt] );
DeclareAttribute("CanonicalRepresentative",
        IsRMSCongruenceClassByLinkedTriple);