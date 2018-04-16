#############################################################################
##
##  semiact.gd
##  Copyright (C) 2016                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

# The rank of IsActingSemigroup is incremented by 8 so that it is greater than
# IsSemigroup and IsFinite and HasGeneratorsOfSemigroup, and IsSemigroupIdeal
# and IsFinite and HasGeneratorsOfSemigroupIdeal
DeclareCategory("IsActingSemigroup", IsEnumerableSemigroupRep and IsFinite, 8);

DeclareCategory("IsRegularActingSemigroupRep",
                IsRegularSemigroup and IsActingSemigroup);

DeclareCategory("IsInverseActingSemigroupRep",
                IsInverseSemigroup and IsGeneratorsOfInverseSemigroup
                and IsRegularActingSemigroupRep);
