############################################################################
##
#W  univcong.gd
#Y  Copyright (C) 2014                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Universal Congruences
DeclareCategory("IsUniversalSemigroupCongruence",
        IsSemigroupCongruence and IsAttributeStoringRep);
DeclareGlobalFunction("UniversalSemigroupCongruence");

DeclareCategory("IsUniversalSemigroupCongruenceClass",
        IsEquivalenceClass and IsAttributeStoringRep and IsAssociativeElement);