############################################################################
##
##  congruences/cong.gd
##  Copyright (C) 2015-2021                               Michael C. Young
##
##  Licensing information can be found in the README file of this package.
##
############################################################################
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

########################################################################
# Categories
#
# IsLeft/Right/SemigroupCongruence is a property, and so we introduce a
# category for each type of congruence. Many operations are agnostic to the
# "handedness" of the congruence, and so we also introduce the category
# IsAnyCongruenceCategory (meaning a left, right or 2-sided congruence).
#
########################################################################

DeclareCategory("IsAnyCongruenceCategory",
                IsEquivalenceRelation,
                Maximum(RankFilter(IsMagmaCongruence),
                        RankFilter(IsLeftMagmaCongruence),
                        RankFilter(IsRightMagmaCongruence)));
DeclareCategory("IsCongruenceCategory",
                IsAnyCongruenceCategory and IsSemigroupCongruence and
                IsMagmaCongruence);
DeclareCategory("IsLeftCongruenceCategory",
                IsAnyCongruenceCategory and IsLeftSemigroupCongruence and
                IsLeftMagmaCongruence);
DeclareCategory("IsRightCongruenceCategory",
                IsAnyCongruenceCategory and IsRightSemigroupCongruence and
                IsRightMagmaCongruence);

DeclareAttribute("AnyCongruenceCategory", IsAnyCongruenceCategory);
DeclareAttribute("AnyCongruenceString", IsAnyCongruenceCategory);
DeclareCategory("IsAnyCongruenceClass",
                IsEquivalenceClass and IsAttributeStoringRep,
                2);  # to beat IsCongruenceClass

########################################################################
# Congruences
########################################################################

# Flexible functions for creating congruences
DeclareGlobalFunction("SemigroupCongruence");
DeclareGlobalFunction("LeftSemigroupCongruence");
DeclareGlobalFunction("RightSemigroupCongruence");

# Properties of congruences
DeclareProperty("IsRightSemigroupCongruence", IsLeftSemigroupCongruence);
DeclareProperty("IsLeftSemigroupCongruence", IsRightSemigroupCongruence);
DeclareProperty("IsSemigroupCongruence", IsLeftSemigroupCongruence);
DeclareProperty("IsSemigroupCongruence", IsRightSemigroupCongruence);

# Attributes of congruences
# EquivalenceRelationPartition is implemented in libsemigroups/cong.gi
DeclareAttribute("NonTrivialEquivalenceClasses", IsEquivalenceRelation);
DeclareAttribute("EquivalenceRelationLookup", IsEquivalenceRelation);
DeclareAttribute("EquivalenceRelationCanonicalLookup", IsEquivalenceRelation);
DeclareAttribute("NrEquivalenceClasses", IsEquivalenceRelation);
DeclareAttribute("EquivalenceRelationCanonicalPartition",
                 IsEquivalenceRelation);
DeclareAttribute("EquivalenceRelationPartitionWithSingletons",
                 IsEquivalenceRelation);

# No-checks version of the "\in" operation
DeclareOperation("CongruenceTestMembershipNC", [IsEquivalenceRelation,
                                                IsMultiplicativeElement,
                                                IsMultiplicativeElement]);

# Algebraic operators
DeclareOperation("JoinLeftSemigroupCongruences",
                 [IsLeftSemigroupCongruence, IsLeftSemigroupCongruence]);
DeclareOperation("JoinRightSemigroupCongruences",
                 [IsRightSemigroupCongruence, IsRightSemigroupCongruence]);

# Comparison operators
DeclareOperation("IsSubrelation",
                 [IsEquivalenceRelation, IsEquivalenceRelation]);
DeclareOperation("IsSuperrelation",
                 [IsEquivalenceRelation, IsEquivalenceRelation]);

########################################################################
# Congruence classes
########################################################################

# IsCongruenceClass is declared in gap/lib/mgmcong.gd:140
DeclareCategory("IsLeftCongruenceClass",
                IsEquivalenceClass and IsAttributeStoringRep);
DeclareCategory("IsRightCongruenceClass",
                IsEquivalenceClass and IsAttributeStoringRep);

# Actions
DeclareOperation("OnLeftCongruenceClasses",
                 [IsLeftCongruenceClass, IsMultiplicativeElement]);
DeclareOperation("OnRightCongruenceClasses",
                 [IsRightCongruenceClass, IsMultiplicativeElement]);
