############################################################################
##
##  congruences/congrms.gd
##  Copyright (C) 2015-2022                               Michael C. Young
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for congruences on finite (0-)simple Rees
## (0-)matrix semigroups, using linked triples.  See Howie 3.5-6, and see
## MT's reports "Computing with Congruences on Finite 0-Simple Semigroups"
## and MSc thesis "Computing with Semigroup Congruences" chapter 3.
##

DeclareCategory("IsRMSOrRZMSCongruenceByLinkedTriple",
                IsSemigroupCongruence
                and IsMagmaCongruence
                and CanComputeEquivalenceRelationPartition
                and IsAttributeStoringRep
                and IsFinite);

# Congruences by linked triple
DeclareCategory("IsRMSCongruenceByLinkedTriple",
                IsRMSOrRZMSCongruenceByLinkedTriple);
DeclareCategory("IsRZMSCongruenceByLinkedTriple",
                IsRMSOrRZMSCongruenceByLinkedTriple);

DeclareOperation("IsLinkedTriple",
                 [IsSemigroup, IsGroup, IsDenseList, IsDenseList]);
DeclareGlobalFunction("RMSCongruenceByLinkedTriple");
DeclareGlobalFunction("RMSCongruenceByLinkedTripleNC");
DeclareGlobalFunction("RZMSCongruenceByLinkedTriple");
DeclareGlobalFunction("RZMSCongruenceByLinkedTripleNC");

# Congruence Classes
DeclareCategory("IsRMSCongruenceClassByLinkedTriple",
                IsLeftRightOrTwoSidedCongruenceClass and IsCongruenceClass and
                IsAttributeStoringRep and IsAssociativeElement);
DeclareCategory("IsRZMSCongruenceClassByLinkedTriple",
                IsLeftRightOrTwoSidedCongruenceClass and IsCongruenceClass and
                IsAttributeStoringRep and IsAssociativeElement);
DeclareOperation("RMSCongruenceClassByLinkedTriple",
                 [IsRMSCongruenceByLinkedTriple,
                  IsRightCoset, IsPosInt, IsPosInt]);
DeclareOperation("RZMSCongruenceClassByLinkedTriple",
                 [IsRZMSCongruenceByLinkedTriple,
                  IsRightCoset, IsPosInt, IsPosInt]);
DeclareOperation("RMSCongruenceClassByLinkedTripleNC",
                 [IsRMSCongruenceByLinkedTriple,
                  IsRightCoset, IsPosInt, IsPosInt]);
DeclareOperation("RZMSCongruenceClassByLinkedTripleNC",
                 [IsRZMSCongruenceByLinkedTriple,
                  IsRightCoset, IsPosInt, IsPosInt]);
