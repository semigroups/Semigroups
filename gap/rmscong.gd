# RZMS Congruences by linked triple
DeclareCategory("IsRZMSCongruenceByLinkedTriple",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareAttribute("CongruencesOfSemigroup",
        IsReesZeroMatrixSemigroup and IsZeroSimpleSemigroup and IsFinite);
DeclareGlobalFunction("IsLinkedTriple");
DeclareGlobalFunction("RZMSCongruenceByLinkedTriple");
DeclareGlobalFunction("RZMSCongruenceByLinkedTripleNC");
DeclareAttribute("NrCongruenceClasses", IsSemigroupCongruence);

DeclareSynonym("CongruenceClasses", EquivalenceClasses);
DeclareSynonym("CongruenceClassOfElement", EquivalenceClassOfElement);

# RZMS Congruence Classes
DeclareCategory("IsRZMSCongruenceClassByLinkedTriple",
        IsEquivalenceClass and IsAttributeStoringRep and IsAssociativeElement);
DeclareOperation("RZMSCongruenceClassByLinkedTriple",
        [IsRZMSCongruenceByLinkedTriple,
         IsRightCoset, IsPosInt, IsPosInt] );
DeclareOperation("RZMSCongruenceClassByLinkedTripleNC",
        [IsRZMSCongruenceByLinkedTriple,
         IsRightCoset, IsPosInt, IsPosInt] );
DeclareOperation("\*", [IsEquivalenceClass, IsList] );
DeclareOperation("\*", [IsList, IsEquivalenceClass] );
DeclareAttribute("CanonicalRepresentative",
        IsRZMSCongruenceClassByLinkedTriple);

# Conversion with semigroup congruences by generating pairs
DeclareOperation("AsSemigroupCongruenceByGeneratingPairs",
        [IsSemigroupCongruence] );
DeclareOperation("AsRZMSCongruenceByLinkedTriple",
        [IsSemigroupCongruence] );