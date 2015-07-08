###########################################################################
##
#W  froidure-pin.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for accessing the kernel level version of the
# Froidure-Pin algorithm for enumerating arbitrary semigroups.

#  For details see:
#
#  V. Froidure, and J.-E. Pin, Algorithms for computing finite semigroups.
#  Foundations of computational mathematics (Rio de Janeiro, 1997), 112-126,
#  Springer, Berlin,  1997.

# Note that an acting semigroup can have generic data but not the other way
# around.

DeclareCategory("IsGenericSemigroupData", IsList);

DeclareProperty("SEMIGROUPS_IsCCSemigroup", IsSemigroup);
# TODO immediate methods for IsCppSemigroup?
DeclareGlobalFunction("SEMIGROUPS_DegreeOfSemigroup");

# TODO remove everything from here down
DeclareAttribute("GenericSemigroupData", IsSemigroup, "mutable");

DeclareOperation("Enumerate", [IsGenericSemigroupData]);
DeclareOperation("Enumerate", [IsGenericSemigroupData, IsCyclotomic]);
DeclareOperation("Enumerate", [IsGenericSemigroupData,
                               IsCyclotomic,
                               IsFunction]);
