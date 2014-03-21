# RMS Congruences by linked triple
DeclareCategory("IsRMSCongruenceByLinkedTriple",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareAttribute("CongruencesOfSemigroup",
        IsReesZeroMatrixSemigroup and IsZeroSimpleSemigroup and IsFinite);
DeclareGlobalFunction("IsLinkedTriple");
DeclareGlobalFunction("RMSCongruenceByLinkedTriple");
DeclareGlobalFunction("RMSCongruenceByLinkedTripleNC");
DeclareAttribute("NrCongruenceClasses", IsSemigroupCongruence);

# RMS Congruence Classes
DeclareCategory("IsRMSCongruenceClassByLinkedTriple",
        IsEquivalenceClass and IsAttributeStoringRep and IsAssociativeElement);
DeclareOperation("RMSCongruenceClassByLinkedTriple",
        [IsRMSCongruenceByLinkedTriple,
         IsRightCoset, IsPosInt, IsPosInt] );
DeclareOperation("RMSCongruenceClassByLinkedTripleNC",
        [IsRMSCongruenceByLinkedTriple,
         IsRightCoset, IsPosInt, IsPosInt] );
DeclareOperation("\*", [IsRMSCongruenceClassByLinkedTriple, IsList] );
DeclareOperation("\*", [IsList, IsRMSCongruenceClassByLinkedTriple] );
DeclareAttribute("CanonicalRepresentative",
        IsRMSCongruenceClassByLinkedTriple);

