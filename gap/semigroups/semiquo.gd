#############################################################################
##
##  semigroups/semiquo.gd
##  Copyright (C) 2014-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("OneImmutable", IsQuotientSemigroup);
DeclareOperation("\/", [IsSemigroup, IsSemigroupIdeal]);

InstallTrueMethod(IsAssociativeElement, IsCongruenceClass);
