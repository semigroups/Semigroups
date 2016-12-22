############################################################################
##
#W  congruences/congfree.gd
#Y  Copyright (C) 2016                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for congruences on free semigroups and free
## monoids, using methods which create the corresponding finitely presented
## semigroup and calling methods on those.  This is not an ideal way of treating
## such congruences, but since an FP semigroup or monoid in GAP is not treated
## simply as a quotient semigroup, and its elements are not regarded as
## congruence classes, it is not possible to implement congruences in the usual
## way.
##

DeclareCategory("IsFreeCongruence",
                IsSemigroupCongruence and IsMagmaCongruence and
                IsAttributeStoringRep);

DeclareCategory("IsFreeCongruenceClass",
                IsCongruenceClass and IsAttributeStoringRep and
                IsAssociativeElement);

DeclareAttribute("FreeCongruenceClassType", IsFreeCongruence);
