############################################################################
##
##  congruences/congrees.gd
##  Copyright (C) 2015-2021                              Michael C. Young
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for Rees congruences; i.e. semigroup congruences
## defined by a two-sided ideal.  See Howie 1.7
##

DeclareAttribute("SemigroupIdealOfReesCongruence", IsReesCongruence);

DeclareCategory("IsReesCongruenceClass",
                IsCongruenceClass and IsAttributeStoringRep and
                IsMultiplicativeElement);

DeclareProperty("IsReesCongruence", IsAnyCongruenceCategory);
