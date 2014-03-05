# RMS Congruences by linked triple
DeclareCategory("IsRMSCongruenceByLinkedTriple",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareAttribute("CongruencesOfSemigroup",
        IsReesZeroMatrixSemigroup and IsZeroSimpleSemigroup and IsFinite);
DeclareGlobalFunction("IsLinkedTriple");
DeclareGlobalFunction("RMSCongruenceByLinkedTriple");
DeclareGlobalFunction("RMSCongruenceByLinkedTripleNC");

# Universal Congruences
DeclareCategory("IsUniversalSemigroupCongruence",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareGlobalFunction("UniversalSemigroupCongruence");

# RMS Congruence Classes
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