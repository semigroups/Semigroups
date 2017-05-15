############################################################################
##
#W  cong.gd
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
## Some general functions are also implemented in cong.gi
##

# These are equivalence relations
InstallTrueMethod(IsEquivalenceRelation, IsLeftSemigroupCongruence);
InstallTrueMethod(IsEquivalenceRelation, IsRightSemigroupCongruence);

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
DeclareAttribute("LeftCongruencesOfSemigroup", IsSemigroup);
DeclareAttribute("RightCongruencesOfSemigroup", IsSemigroup);

DeclareAttribute("MinimalCongruencesOfSemigroup", IsSemigroup);
DeclareAttribute("MinimalLeftCongruencesOfSemigroup", IsSemigroup);
DeclareAttribute("MinimalRightCongruencesOfSemigroup", IsSemigroup);

DeclareOperation("MinimalCongruencesOfSemigroup",
                 [IsSemigroup, IsMultiplicativeElementCollection]);
DeclareOperation("MinimalLeftCongruencesOfSemigroup",
                 [IsSemigroup, IsMultiplicativeElementCollection]);
DeclareOperation("MinimalRightCongruencesOfSemigroup",
                 [IsSemigroup, IsMultiplicativeElementCollection]);

DeclareAttribute("PrincipalCongruencesOfSemigroup", IsSemigroup);
DeclareAttribute("PrincipalLeftCongruencesOfSemigroup", IsSemigroup);
DeclareAttribute("PrincipalRightCongruencesOfSemigroup", IsSemigroup);

DeclareOperation("PrincipalCongruencesOfSemigroup",
                 [IsSemigroup, IsMultiplicativeElementCollection]);
DeclareOperation("PrincipalLeftCongruencesOfSemigroup",
                 [IsSemigroup, IsMultiplicativeElementCollection]);
DeclareOperation("PrincipalRightCongruencesOfSemigroup",
                 [IsSemigroup, IsMultiplicativeElementCollection]);

DeclareSynonym("GeneratingPairsOfLeftSemigroupCongruence",
               GeneratingPairsOfLeftMagmaCongruence);
DeclareSynonym("GeneratingPairsOfRightSemigroupCongruence",
               GeneratingPairsOfRightMagmaCongruence);

DeclareAttribute("NonTrivialEquivalenceClasses", IsEquivalenceRelation);

DeclareAttribute("EquivalenceRelationLookup", IsEquivalenceRelation);
DeclareAttribute("EquivalenceRelationCanonicalLookup", IsEquivalenceRelation);
DeclareAttribute("NrEquivalenceClasses", IsEquivalenceRelation);

DeclareAttribute("EquivalenceRelationCanonicalPartition",
                 IsEquivalenceRelation);

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
                 [IsLeftCongruenceClass, IsMultiplicativeElement]);
DeclareOperation("OnRightCongruenceClasses",
                 [IsRightCongruenceClass, IsMultiplicativeElement]);

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
                                              IsMultiplicativeElement]);
DeclareOperation("LeftCongruenceClassOfElement", [IsLeftSemigroupCongruence,
                                                  IsMultiplicativeElement]);
DeclareOperation("RightCongruenceClassOfElement", [IsRightSemigroupCongruence,
                                                   IsMultiplicativeElement]);

# No-checks version of the "\in" operation
DeclareOperation("CongruenceTestMembershipNC", [IsEquivalenceRelation,
                                                IsMultiplicativeElement,
                                                IsMultiplicativeElement]);
