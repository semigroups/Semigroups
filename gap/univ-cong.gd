############################################################################
##
#W  univ-cong.gd
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for the unique universal congruence on a
## semigroup.
##

# Universal Congruences
DeclareCategory("IsUniversalSemigroupCongruence",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareOperation("UniversalSemigroupCongruence", [IsSemigroup]);

DeclareCategory("IsUniversalSemigroupCongruenceClass",
        IsEquivalenceClass and IsAttributeStoringRep and IsAssociativeElement);
