############################################################################
##
#W  congruences/congrms.gd
#Y  Copyright (C) 2015                                   Michael C. Torpey
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

# Congruences by linked triple
DeclareCategory("IsRMSCongruenceByLinkedTriple",
                IsSemigroupCongruence and IsAttributeStoringRep and IsFinite);
DeclareCategory("IsRZMSCongruenceByLinkedTriple",
                IsSemigroupCongruence and IsAttributeStoringRep and IsFinite);
DeclareOperation("IsLinkedTriple",
                 [IsSemigroup, IsGroup, IsDenseList, IsDenseList]);
DeclareGlobalFunction("RMSCongruenceByLinkedTriple");
DeclareGlobalFunction("RMSCongruenceByLinkedTripleNC");
DeclareGlobalFunction("RZMSCongruenceByLinkedTriple");
DeclareGlobalFunction("RZMSCongruenceByLinkedTripleNC");

# Congruence Classes
DeclareCategory("IsRMSCongruenceClassByLinkedTriple",
                IsCongruenceClass and IsAttributeStoringRep and
                IsAssociativeElement);
DeclareCategory("IsRZMSCongruenceClassByLinkedTriple",
                IsCongruenceClass and IsAttributeStoringRep and
                IsAssociativeElement);
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
DeclareAttribute("CanonicalRepresentative", IsEquivalenceClass);

# Conversion with semigroup congruences by generating pairs
DeclareOperation("AsRMSCongruenceByLinkedTriple", [IsSemigroupCongruence]);
DeclareOperation("AsRZMSCongruenceByLinkedTriple", [IsSemigroupCongruence]);
