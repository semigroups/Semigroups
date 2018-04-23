############################################################################
##
##  congruences/congpairs.gd
##  Copyright (C) 2015-17                                Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains functions for any semigroup congruence with generating
## pairs.  These act as a wrapper to the congpairs.cc file, which in turn uses
## the congruence methods in libsemigroups.
##

DeclareOperation("AsSemigroupCongruenceByGeneratingPairs",
                 [IsSemigroupCongruence]);

# This is a representation for left/right/two-sided congruences of a finite
# semigroup by generating pairs.
#
# The components are:
#
#   range:    the semigroup over which the congruence is defined
#
#   genpairs: the list of generating pairs (pairs of elements of the range)
#
#   type:     one of "left", "right", or "twosided" to indicate the type of the
#             congruence when it was created (i.e. if it was created as a
#             two-sided, left or right congruence. Remember
#             IsLeftSemigroupCongruence is a property, so a congruence might
#             learn that it is a left congruence after being created as a right
#             congruence. This component only refers to the type of congruence
#             that was created.
#
#   report:   should be true or false, sets whether information is printed
#             during a computation or not.

DeclareCategory("IsCongruenceByGeneratingPairs",
                IsEquivalenceRelation, RankFilter(IsSemigroupCongruence));

DeclareRepresentation("IsCongruenceByGeneratingPairsRep",
                      IsEquivalenceRelation and IsAttributeStoringRep and
                      IsCongruenceByGeneratingPairs,
                      ["range", "genpairs", "type", "report"]);

DeclareCategory("IsEnumerableSemigroupCongruence",
                IsCongruenceByGeneratingPairsRep,
                RankFilter(IsSemigroupCongruence));

DeclareCategory("IsFpSemigroupCongruence",
                IsCongruenceByGeneratingPairsRep,
                RankFilter(IsSemigroupCongruence));

DeclareAttribute("CongruenceByGeneratingPairsPartition",
                 IsCongruenceByGeneratingPairsRep);

# This is a representation for classes of a left/right/two-sided congruence of
# a finite semigroup by generating pairs.
#
# The components are:
#
#   rep:  an arbitrary representative of the class
#
#   cong: the underlying congruence of the class.

DeclareCategory("IsCongruenceClassByGeneratingPairs",
                IsEquivalenceClass, RankFilter(IsCongruenceClass));

DeclareRepresentation("IsCongruenceClassByGeneratingPairsRep",
                      IsEquivalenceClass and IsAttributeStoringRep and
                      IsCongruenceClassByGeneratingPairs,
                      ["rep", "cong"]);

DeclareCategory("IsEnumerableSemigroupCongruenceClass",
                IsCongruenceClassByGeneratingPairsRep,
                RankFilter(IsCongruenceClass));

DeclareCategory("IsFpSemigroupCongruenceClass",
                IsCongruenceClassByGeneratingPairsRep,
                RankFilter(IsCongruenceClass));

DeclareAttribute("CongruenceClassByGeneratingPairsCosetId",
                 IsCongruenceClassByGeneratingPairsRep);

DeclareAttribute("CongruenceClassByGeneratingPairsType",
                 IsCongruenceByGeneratingPairsRep);
