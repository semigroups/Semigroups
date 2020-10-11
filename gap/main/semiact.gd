#############################################################################
##
##  main/semiact.gd
##  Copyright (C) 2016-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

# The rank of IsActingSemigroup is incremented by 10 so that it is greater than
# IsSemigroup and IsFinite and HasGeneratorsOfSemigroup, and IsSemigroupIdeal
# and IsFinite and HasGeneratorsOfSemigroupIdeal
DeclareCategory("IsActingSemigroup", IsSemigroup and IsFinite, 10);

DeclareCategory("IsRegularActingSemigroupRep",
                IsRegularSemigroup and IsActingSemigroup);

DeclareCategory("IsInverseActingSemigroupRep",
                IsInverseSemigroup and IsGeneratorsOfInverseSemigroup
                and IsRegularActingSemigroupRep);
