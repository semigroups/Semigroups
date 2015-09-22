############################################################################
##
#W  rees-cong.gi
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for Rees congruences; i.e. semigroup congruences
## defined by a two-sided ideal.
##

DeclareAttribute("SemigroupIdealOfReesCongruence", IsReesCongruence);

DeclareCategory("IsReesCongruenceClass",
                IsCongruenceClass and IsAttributeStoringRep and
                IsAssociativeElement);
