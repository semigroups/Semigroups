############################################################################
##
##  congruences/congpairs.gd
##  Copyright (C) 2015-17                                Michael C. Young
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains functions for any semigroup congruence with generating
## pairs.


DeclareAttribute("GeneratingPairsOfAnyCongruence", IsAnyCongruenceCategory);

DeclareSynonym("GeneratingPairsOfLeftSemigroupCongruence",
               GeneratingPairsOfLeftMagmaCongruence);
DeclareSynonym("GeneratingPairsOfRightSemigroupCongruence",
               GeneratingPairsOfRightMagmaCongruence);

DeclareCategory("IsCongruenceByGeneratingPairs",
                IsEquivalenceRelation, RankFilter(IsSemigroupCongruence));

# 
# FIXME(now) remove this representation completely, I don't think it's required
# any longer
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
DeclareRepresentation("IsCongruenceByGeneratingPairsRep",
                      IsEquivalenceRelation and IsAttributeStoringRep and
                      IsCongruenceByGeneratingPairs,
                      ["range", "genpairs", "type", "report"]);

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

DeclareCategory("IsFpSemigroupCongruenceClass",
                IsCongruenceClassByGeneratingPairsRep,
                RankFilter(IsCongruenceClass));

DeclareAttribute("CongruenceClassByGeneratingPairsCosetId",
                 IsCongruenceClassByGeneratingPairsRep);

DeclareAttribute("CongruenceClassByGeneratingPairsType",
                 IsCongruenceByGeneratingPairsRep);
