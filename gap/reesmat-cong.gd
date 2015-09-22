############################################################################
##
#W  reesmat-cong.gd
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for congruences on finite (0-)simple Rees
## (0-)matrix semigroups, using linked triples.
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
DeclareAttribute("NrCongruenceClasses", IsSemigroupCongruence);

DeclareSynonym("CongruenceClasses", EquivalenceClasses);
DeclareSynonym("CongruenceClassOfElement", EquivalenceClassOfElement);

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
DeclareOperation("\*", [IsEquivalenceClass, IsList]);
DeclareOperation("\*", [IsList, IsEquivalenceClass]);
DeclareAttribute("CanonicalRepresentative", IsEquivalenceClass);

# Conversion with semigroup congruences by generating pairs
DeclareOperation("AsSemigroupCongruenceByGeneratingPairs",
                 [IsSemigroupCongruence]);
DeclareOperation("AsRMSCongruenceByLinkedTriple", [IsSemigroupCongruence]);
DeclareOperation("AsRZMSCongruenceByLinkedTriple", [IsSemigroupCongruence]);
