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
##       cong-inverse.gi - Inverse semigroups
##       cong-pairs.gi   - Congruences with generating pairs
##       cong-rees.gi    - Rees congruences
##       cong-reesmat.gi - (0-)simple Rees matrix semigroups
##       cong-simple.gi  - (0-)simple semigroups
##       cong-univ.gi    - Universal congruences
##
## Some general functions are also implemented in congruences.gi
##

DeclareGlobalFunction("SemigroupCongruence");
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
