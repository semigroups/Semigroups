############################################################################
##
#W  congruences.gd
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains declarations for functions, operations and attributes of
## semigroup congruences.  Methods for most of these are implemented for
## specific types of congruence in the following files:
##
##       inverse.gi - Inverse semigroups
##       pairs.gi   - Congruences with generating pairs
##       rees.gi    - Rees congruences
##       reesmat.gi - (0-)simple Rees matrix semigroups
##       simple.gi  - (0-)simple semigroups
##       univ.gi    - Universal congruences
##
## Some general functions are also implemented in congruences.gi
##

# Flexible functions for creating congruences
DeclareGlobalFunction("SemigroupCongruence");
DeclareGlobalFunction("LeftSemigroupCongruence");
DeclareGlobalFunction("RightSemigroupCongruence");

# Separate categories for the classes of left, right, and 2-sided congruences
DeclareCategory("IsLeftCongruenceClass",
                IsEquivalenceClass and IsAttributeStoringRep);
DeclareCategory("IsRightCongruenceClass",
                IsEquivalenceClass and IsAttributeStoringRep);

DeclareAttribute("CongruencesOfSemigroup", IsSemigroup);
DeclareAttribute("LatticeOfCongruences", IsSemigroup);

DeclareSynonym("GeneratingPairsOfLeftSemigroupCongruence",
               GeneratingPairsOfLeftMagmaCongruence);
DeclareSynonym("GeneratingPairsOfRightSemigroupCongruence",
               GeneratingPairsOfRightMagmaCongruence);

DeclareAttribute("NonTrivialEquivalenceClasses", IsEquivalenceRelation);
DeclareOperation("\*", [IsEquivalenceClass, IsList]);
DeclareOperation("\*", [IsList, IsEquivalenceClass]);

DeclareOperation("AsSemigroupCongruenceByGeneratingPairs",
                 [IsSemigroupCongruence]);
DeclareAttribute("AsLookupTable", IsEquivalenceRelation);
DeclareAttribute("NrEquivalenceClasses", IsEquivalenceRelation);

DeclareOperation("JoinLeftSemigroupCongruences",
                 [IsLeftSemigroupCongruence, IsLeftSemigroupCongruence]);
DeclareOperation("JoinRightSemigroupCongruences",
                 [IsRightSemigroupCongruence, IsRightSemigroupCongruence]);

DeclareOperation("IsSubrelation",
                 [IsEquivalenceRelation, IsEquivalenceRelation]);

# Synonyms replacing "equivalence" with "congruence", at user's preference
DeclareSynonym("NonTrivialCongruenceClasses", NonTrivialEquivalenceClasses);
DeclareSynonym("CongruenceClasses", EquivalenceClasses);
DeclareSynonym("CongruenceClassOfElement", EquivalenceClassOfElement);
DeclareSynonym("NrCongruenceClasses", NrEquivalenceClasses);
