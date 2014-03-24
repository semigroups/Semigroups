# RMS Congruences by linked triple
DeclareCategory("IsRMSCongruenceByLinkedTriple",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareAttribute("CongruencesOfSemigroup",
        IsReesZeroMatrixSemigroup and IsZeroSimpleSemigroup and IsFinite);
DeclareGlobalFunction("IsLinkedTriple");
DeclareGlobalFunction("RMSCongruenceByLinkedTriple");
DeclareGlobalFunction("RMSCongruenceByLinkedTripleNC");
DeclareAttribute("NrCongruenceClasses", IsSemigroupCongruence);

DeclareSynonym("CongruenceClasses", EquivalenceClasses);
DeclareSynonym("CongruenceClassOfElement", EquivalenceClassOfElement);

# RMS Congruence Classes
DeclareCategory("IsRMSCongruenceClassByLinkedTriple",
        IsEquivalenceClass and IsAttributeStoringRep and IsAssociativeElement);
DeclareOperation("RMSCongruenceClassByLinkedTriple",
        [IsRMSCongruenceByLinkedTriple,
         IsRightCoset, IsPosInt, IsPosInt] );
DeclareOperation("RMSCongruenceClassByLinkedTripleNC",
        [IsRMSCongruenceByLinkedTriple,
         IsRightCoset, IsPosInt, IsPosInt] );
DeclareOperation("\*", [IsEquivalenceClass, IsList] );
DeclareOperation("\*", [IsList, IsEquivalenceClass] );
DeclareAttribute("CanonicalRepresentative",
        IsRMSCongruenceClassByLinkedTriple);

# Conversion with semigroup congruences by generating pairs
DeclareOperation("AsSemigroupCongruenceByGeneratingPairs",
        [IsSemigroupCongruence] );
DeclareOperation("AsRMSCongruenceByLinkedTriple",
        [IsSemigroupCongruence] );