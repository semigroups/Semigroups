############################################################################
##
##  congruences/congpart.gd
##  Copyright (C) 2022                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
############################################################################
##
## This file contains declarations for left, right, and two-sided congruences
## that can compute EquivalenceRelationPartition.

###############################################################################
###############################################################################
# The following are the fundamental attributes for a congruence in
# CanComputeEquivalenceRelationPartition.  If defining a new type of congruence
# in this representation it is necessary to provide methods for the following:
#
# * one or both of EquivalenceRelationPartitionWithSingletons or
# EquivalenceRelationPartition (for congruences whose source/range is infinite,
# or otherwise when it is more convenient).
# * ImagesElm (for the calculation of the elements of an equivalence class)
# * CongruenceTestMembershipNC
#
# If the equivalence classes of a congruence have their own representation
# (because they too have better methods than the default ones), then it is also
# necessary to implement a method for:
#
# * EquivalenceClassOfElementNC.
#
# If the congruence is not natively implemented in terms of generating pairs,
# then it is also useful to provide methods for:
#
# * GeneratingPairsOfLeft/Right/MagmaCongruence
# * Left/Right/SemigroupCongruenceByGeneratingPairs
#
# to allow for conversion of the representation.
#
# The following attributes can be computed (by default, if no better
# method is known) from the EquivalenceRelationPartitionWithSingletons:
#
# * EquivalenceRelationPartition (if not implemented above)
# * EquivalenceRelationPartitionWithSingletons (if not implemented above)
# * EquivalenceRelationLookup
# * EquivalenceRelationCanonicalLookup
# * EquivalenceRelationCanonicalPartition
# * NonTrivialEquivalenceClasses
# * EquivalenceClasses
#
# If a congruence <C> knows its generating pairs, knows its source/range,
# CanUseFroidurePin(Range(C)) is true, and HasGeneratorsOfSemigroup(Range(C))
# is true, then <C> automatically satisfies CanUseLibsemigroupsCongruence
# (which is implies CanComputeEquivalenceRelationPartition). In this case the
# generating pairs are used to construct a libsemigroups Congruence object
# representing <C>, and this object will be used to perform many computations
# for <C>. If you never want to construct such a libsemigroups Congruence
# object representing for <C>, then an immediate method should be installed in
# libsemigroups/cong.gi explicitly excluding this type of congruence, and the
# mandatory methods listed above for CanComputeEquivalenceRelationPartition
# should be implemented.
#
# To define a new type of congruence that does not implement any of the above
# (i.e. that uses libsemigroups Congruence objects to compute
# EquivalenceRelationPartition), it's sufficient to ensure that the
# congruence's generating pairs are computed in the function where it is
# Objectified.
#
# There are some types of congruences in the Semigroups package which use both
# a specialised representation for some things, and use a libsemigroups
# Congruence object for others:
#
# * IsCongruenceByWangPair;
# * IsReesCongruence (when *not* constructed directly from an ideal).
#
# there are others that never use libsemigroups Congruence objects:
#
# * IsInverseSemigroupCongruenceByKernelTrace;
# * IsSimpleSemigroupCongruence;
# * IsRMSCongruenceByLinkedTriple;
# * IsUniversalSemigroupCongruence;
# * IsReesCongruence (when constructed directly from an ideal).
#
# and some that always do (congruences defined by generating pairs, not
# belonging to the previous types).
#
###############################################################################
###############################################################################

DeclareProperty("CanComputeEquivalenceRelationPartition",
                IsLeftRightOrTwoSidedCongruence);
