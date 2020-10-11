############################################################################
##
##  congruences/conguniv.gd
##  Copyright (C) 2015-2022                               Michael C. Young
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for the unique universal congruence on a
## semigroup, that is the relation SxS on a semigroup S.
##

# Universal Congruences
DeclareProperty("IsUniversalSemigroupCongruence",
                IsSemigroupCongruence);
DeclareCategory("IsUniversalSemigroupCongruenceClass",
                IsCongruenceClass and IsAttributeStoringRep and
                IsAssociativeElement);
DeclareOperation("UniversalSemigroupCongruence", [IsSemigroup]);
