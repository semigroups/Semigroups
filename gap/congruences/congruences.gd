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

DeclareGlobalFunction("SemigroupCongruence");
DeclareGlobalFunction("LeftSemigroupCongruence");
DeclareGlobalFunction("RightSemigroupCongruence");
DeclareAttribute("CongruencesOfSemigroup", IsSemigroup);

DeclareAttribute("NonTrivialEquivalenceClasses", IsEquivalenceRelation);
DeclareSynonym("NonTrivialCongruenceClasses", NonTrivialEquivalenceClasses);
DeclareSynonym("CongruenceClasses", EquivalenceClasses);
DeclareSynonym("CongruenceClassOfElement", EquivalenceClassOfElement);
DeclareOperation("\*", [IsEquivalenceClass, IsList]);
DeclareOperation("\*", [IsList, IsEquivalenceClass]);

DeclareOperation("AsSemigroupCongruenceByGeneratingPairs",
        [IsSemigroupCongruence]);
DeclareAttribute("AsLookupTable", IsEquivalenceRelation);
DeclareAttribute("NrEquivalenceClasses", IsEquivalenceRelation);
DeclareSynonym("NrCongruenceClasses", NrEquivalenceClasses);
