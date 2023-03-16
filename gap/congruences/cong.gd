############################################################################
##
##  congruences/cong.gd
##  Copyright (C) 2015-2022                               Michael C. Young
##
##  Licensing information can be found in the README file of this package.
##
############################################################################
##
## This file contains declarations for functions, operations and attributes of
## semigroup congruences that do not depend on any particular representation.
## Methods for most of these are implemented for specific types of congruence
## in the following files:
##
##       congpart.gi    - for congruences that can compute
##                        EquivalenceRelationPartition
##       conginv.gi     - Inverse semigroups
##       congpairs.gi   - Congruences with generating pairs
##       congrees.gi    - Rees congruences
##       congrms.gi     - (0-)simple Rees matrix semigroups
##       congsimple.gi  - (0-)simple semigroups
##       conguniv.gi    - Universal congruences
##
## Some general functions are also implemented in cong.gi, when these do not
## depend on the particular representation of the congruences, or that anything
## other than what is implemented in the GAP library is required. Most of the
## operations etc declared in this file are not implemented in cong.gi, but are
## declared here for congruences in general.

###############################################################################
# IsLeft/Right/SemigroupCongruence is a property, but
# IsLeft/Right/MagmaCongruence is a category so we use them in conjunction to
# mean i.e. a left congruence on a semigroup that was created in the category
# of left congruences. We introduce synonyms for these to simplify their use.
#
# Note that IsMagmaCongruence implies IsLeftMagmaCongruence and
# IsRightMagmaCongruence, and so IsLeftMagmaCongruence and
# IsLeftSemigroupCongruence returns true when applied to a 2-sided congruence.
# In other words, we cannot use IsLeftMagmaCongruence to determine whether or
# not a congruence was created as a left congruence or not (we can use
# IsLeftMagmaCongruence and not IsRightMagmaCongruence).
###############################################################################

DeclareProperty("IsLeftRightOrTwoSidedCongruence",
                IsLeftMagmaCongruence and IsLeftSemigroupCongruence);
DeclareProperty("IsLeftRightOrTwoSidedCongruence",
                IsRightMagmaCongruence and IsRightSemigroupCongruence);
DeclareProperty("IsLeftRightOrTwoSidedCongruence",
                IsMagmaCongruence and IsSemigroupCongruence);

InstallTrueMethod(IsLeftRightOrTwoSidedCongruence,
                  IsLeftMagmaCongruence and IsLeftSemigroupCongruence);
InstallTrueMethod(IsLeftRightOrTwoSidedCongruence,
                  IsRightMagmaCongruence and IsRightSemigroupCongruence);
InstallTrueMethod(IsLeftRightOrTwoSidedCongruence,
                  IsMagmaCongruence and IsSemigroupCongruence);

# The next attributes allows us to recover the category/string from a
# left/right/2-sided congruence object
DeclareAttribute("CongruenceHandednessString",
                 IsLeftRightOrTwoSidedCongruence);

############################################################################
# We introduce the property IsLeftRightOrTwoSidedCongruenceClass in a similar
# vein to IsLeftRightOrTwoSidedCongruence. Since we declare
# IsLeftCongruenceClass and IsRightCongruenceClass we could define them to
# satisfy IsLeftRightOrTwoSidedCongruenceClass, but then we have to declare
# IsLeftRightOrTwoSidedCongruenceClass as a being a property of
# IsEquivalenceClass, which means we then have to fiddle more with ranks of
# methods.
############################################################################

# IsCongruenceClass is declared in gap/lib/mgmcong.gd:140
# but it does not include IsAttributeStoringRep
DeclareCategory("IsLeftCongruenceClass",
                IsEquivalenceClass and IsAttributeStoringRep);
DeclareCategory("IsRightCongruenceClass",
                IsEquivalenceClass and IsAttributeStoringRep);

DeclareProperty("IsLeftRightOrTwoSidedCongruenceClass",
                IsLeftCongruenceClass);
DeclareProperty("IsLeftRightOrTwoSidedCongruenceClass",
                IsRightCongruenceClass);
DeclareProperty("IsLeftRightOrTwoSidedCongruenceClass",
                IsCongruenceClass);

InstallTrueMethod(IsLeftRightOrTwoSidedCongruenceClass,
                  IsLeftCongruenceClass);
InstallTrueMethod(IsLeftRightOrTwoSidedCongruenceClass,
                  IsRightCongruenceClass);
InstallTrueMethod(IsLeftRightOrTwoSidedCongruenceClass,
                  IsCongruenceClass);

########################################################################
# Congruences
########################################################################

# Flexible functions for creating congruences
DeclareGlobalFunction("SemigroupCongruence");
DeclareGlobalFunction("LeftSemigroupCongruence");
DeclareGlobalFunction("RightSemigroupCongruence");

DeclareAttribute("TrivialCongruence", IsSemigroup);

# Properties of congruences
DeclareProperty("IsRightSemigroupCongruence", IsLeftSemigroupCongruence);
DeclareProperty("IsSemigroupCongruence", IsLeftSemigroupCongruence);

DeclareAttribute("NrEquivalenceClasses",
                 IsEquivalenceRelation);
DeclareAttribute("NonTrivialEquivalenceClasses",
                 IsEquivalenceRelation);
DeclareAttribute("EquivalenceRelationLookup",
                 IsEquivalenceRelation);
DeclareAttribute("EquivalenceRelationCanonicalLookup",
                 IsEquivalenceRelation);
DeclareAttribute("EquivalenceRelationCanonicalPartition",
                 IsEquivalenceRelation);
DeclareAttribute("EquivalenceRelationPartitionWithSingletons",
                 IsEquivalenceRelation);

# No-checks version of the "\in" operation
DeclareOperation("CongruenceTestMembershipNC",
                 [IsLeftRightOrTwoSidedCongruence,
                  IsMultiplicativeElement,
                  IsMultiplicativeElement]);

# Algebraic operators
DeclareOperation("JoinLeftSemigroupCongruences",
                 [IsLeftSemigroupCongruence, IsLeftSemigroupCongruence]);
DeclareOperation("JoinRightSemigroupCongruences",
                 [IsRightSemigroupCongruence, IsRightSemigroupCongruence]);
DeclareOperation("MeetLeftSemigroupCongruences",
                 [IsLeftSemigroupCongruence, IsLeftSemigroupCongruence]);
DeclareOperation("MeetRightSemigroupCongruences",
                 [IsRightSemigroupCongruence, IsRightSemigroupCongruence]);

# Comparison operators
DeclareOperation("IsSubrelation",
                 [IsEquivalenceRelation, IsEquivalenceRelation]);
DeclareOperation("IsSuperrelation",
                 [IsEquivalenceRelation, IsEquivalenceRelation]);

########################################################################
# Congruence classes
########################################################################

# Actions
DeclareOperation("OnLeftCongruenceClasses",
                 [IsLeftRightOrTwoSidedCongruenceClass,
                  IsMultiplicativeElement]);
DeclareOperation("OnRightCongruenceClasses",
                 [IsLeftRightOrTwoSidedCongruenceClass,
                  IsMultiplicativeElement]);
