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
##       conginv.gi     - Inverse semigroups
##       congpairs.gi   - Congruences with generating pairs
##       congrees.gi    - Rees congruences
##       congrms.gi     - (0-)simple Rees matrix semigroups
##       congsimple.gi  - (0-)simple semigroups
##       conguniv.gi    - Universal congruences
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

DeclareAttribute("AsLookupTable", IsEquivalenceRelation);
DeclareAttribute("NrEquivalenceClasses", IsEquivalenceRelation);

DeclareOperation("JoinLeftSemigroupCongruences",
                 [IsLeftSemigroupCongruence, IsLeftSemigroupCongruence]);
DeclareOperation("JoinRightSemigroupCongruences",
                 [IsRightSemigroupCongruence, IsRightSemigroupCongruence]);

DeclareOperation("IsSubrelation",
                 [IsEquivalenceRelation, IsEquivalenceRelation]);
DeclareOperation("IsSuperrelation",
                 [IsEquivalenceRelation, IsEquivalenceRelation]);

DeclareProperty("IsRightSemigroupCongruence", IsLeftSemigroupCongruence);
DeclareProperty("IsLeftSemigroupCongruence", IsRightSemigroupCongruence);
DeclareProperty("IsSemigroupCongruence", IsLeftSemigroupCongruence);
DeclareProperty("IsSemigroupCongruence", IsRightSemigroupCongruence);

DeclareOperation("OnLeftCongruenceClasses",
                 [IsLeftCongruenceClass, IsAssociativeElement]);
DeclareOperation("OnRightCongruenceClasses",
                 [IsRightCongruenceClass, IsAssociativeElement]);

# Helper functions to EquivalenceClasses for specific categories
DeclareOperation("CongruenceClasses", [IsSemigroupCongruence]);
DeclareOperation("LeftCongruenceClasses", [IsLeftSemigroupCongruence]);
DeclareOperation("RightCongruenceClasses", [IsRightSemigroupCongruence]);

DeclareOperation("NonTrivialCongruenceClasses",
                 [IsSemigroupCongruence]);
DeclareOperation("NonTrivialLeftCongruenceClasses",
                 [IsLeftSemigroupCongruence]);
DeclareOperation("NonTrivialRightCongruenceClasses",
                 [IsRightSemigroupCongruence]);

DeclareOperation("NrCongruenceClasses", [IsSemigroupCongruence]);
DeclareOperation("NrLeftCongruenceClasses", [IsLeftSemigroupCongruence]);
DeclareOperation("NrRightCongruenceClasses", [IsRightSemigroupCongruence]);

DeclareOperation("CongruenceClassOfElement", [IsSemigroupCongruence,
                                              IsAssociativeElement]);
DeclareOperation("LeftCongruenceClassOfElement", [IsLeftSemigroupCongruence,
                                                  IsAssociativeElement]);
DeclareOperation("RightCongruenceClassOfElement", [IsRightSemigroupCongruence,
                                                   IsAssociativeElement]);
