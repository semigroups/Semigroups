###########################################################################
##
#W  fropin.gd
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

DeclareProperty("IsSemigroupEnumerator", IsEnumeratorByFunctions);

DeclareOperation("PositionSortedOp",
                 [IsSemigroup, IsMultiplicativeElement]);
DeclareOperation("PositionOp",
                 [IsSemigroup, IsMultiplicativeElement, IsZeroCyc]);
DeclareOperation("Position",
                 [IsSemigroup, IsMultiplicativeElement]);
DeclareOperation("Position",
                 [IsSemigroup, IsMultiplicativeElement, IsZeroCyc]);

DeclareAttribute("AsListCanonical", IsSemigroup);
DeclareAttribute("EnumeratorCanonical", IsSemigroup);
DeclareAttribute("IteratorCanonical", IsSemigroup);

# TODO(JDM) increment the rank here. Non-finite semigroups can be enumerable!
DeclareProperty("IsEnumerableSemigroup", IsSemigroup);

InstallTrueMethod(IsEnumerableSemigroup, IsTransformationSemigroup);
InstallTrueMethod(IsEnumerableSemigroup, IsPartialPermSemigroup);
InstallTrueMethod(IsEnumerableSemigroup, IsMatrixOverSemiringSemigroup);
InstallTrueMethod(IsEnumerableSemigroup, IsBipartitionSemigroup);
InstallTrueMethod(IsEnumerableSemigroup, IsPBRSemigroup);

DeclareOperation("Enumerate", [IsSemigroup, IsPosInt]);
DeclareOperation("Enumerate", [IsSemigroup]);
