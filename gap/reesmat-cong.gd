# Congruences by linked triple
DeclareCategory("IsRMSCongruenceByLinkedTriple",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareCategory("IsRZMSCongruenceByLinkedTriple",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareAttribute("CongruencesOfSemigroup", IsReesZeroMatrixSemigroup and IsZeroSimpleSemigroup and IsFinite);
DeclareAttribute("CongruencesOfSemigroup", IsReesMatrixSemigroup and IsSimpleSemigroup and IsFinite);
DeclareOperation("IsLinkedTriple",
        [IsSemigroup, IsGroup, IsDenseList, IsDenseList] );
DeclareGlobalFunction("RMSCongruenceByLinkedTriple");
DeclareGlobalFunction("RMSCongruenceByLinkedTripleNC");
DeclareGlobalFunction("RZMSCongruenceByLinkedTriple");
DeclareGlobalFunction("RZMSCongruenceByLinkedTripleNC");
DeclareAttribute("NrCongruenceClasses", IsSemigroupCongruence);

DeclareSynonym("CongruenceClasses", EquivalenceClasses);
DeclareSynonym("CongruenceClassOfElement", EquivalenceClassOfElement);

# Congruence Classes
DeclareCategory("IsRMSCongruenceClassByLinkedTriple",
        IsEquivalenceClass and IsAttributeStoringRep and IsAssociativeElement);
DeclareCategory("IsRZMSCongruenceClassByLinkedTriple",
        IsEquivalenceClass and IsAttributeStoringRep and IsAssociativeElement);
DeclareOperation("RMSCongruenceClassByLinkedTriple",
        [IsRMSCongruenceByLinkedTriple,
         IsRightCoset, IsPosInt, IsPosInt] );
DeclareOperation("RZMSCongruenceClassByLinkedTriple",
        [IsRZMSCongruenceByLinkedTriple,
         IsRightCoset, IsPosInt, IsPosInt] );
DeclareOperation("RMSCongruenceClassByLinkedTripleNC",
        [IsRMSCongruenceByLinkedTriple,
         IsRightCoset, IsPosInt, IsPosInt] );
DeclareOperation("RZMSCongruenceClassByLinkedTripleNC",
        [IsRZMSCongruenceByLinkedTriple,
         IsRightCoset, IsPosInt, IsPosInt] );
DeclareOperation("\*", [IsEquivalenceClass, IsList] );
DeclareOperation("\*", [IsList, IsEquivalenceClass] );
DeclareAttribute("CanonicalRepresentative", IsEquivalenceClass);

# Conversion with semigroup congruences by generating pairs
DeclareOperation("AsSemigroupCongruenceByGeneratingPairs",
        [IsSemigroupCongruence] );
DeclareOperation("AsRMSCongruenceByLinkedTriple", [IsSemigroupCongruence] );
DeclareOperation("AsRZMSCongruenceByLinkedTriple", [IsSemigroupCongruence] );
