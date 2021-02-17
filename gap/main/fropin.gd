###########################################################################
##
##  fropin.gd
##  Copyright (C) 2015                                   James D. Mitchell
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

DeclareAttribute("AsListCanonical", IsSemigroup);
DeclareOperation("PositionCanonical",
                 [IsSemigroup, IsMultiplicativeElement]);

DeclareOperation("PositionSortedOp",
                 [IsSemigroup, IsMultiplicativeElement]);
DeclareOperation("PositionOp",
                 [IsSemigroup,
                  IsMultiplicativeElement,
                  IsZeroCyc]);
DeclareOperation("Position",
                 [IsSemigroup, IsMultiplicativeElement]);
DeclareOperation("Position",
                 [IsSemigroup, IsMultiplicativeElement,
                  IsZeroCyc]);

DeclareOperation("Enumerate", [IsSemigroup, IsInt]);
DeclareOperation("Enumerate", [IsSemigroup]);

DeclareAttribute("LeftCayleyDigraph", IsSemigroup);
DeclareAttribute("RightCayleyDigraph", IsSemigroup);

DeclareAttribute("EnumeratorCanonical", IsSemigroup);
DeclareOperation("IteratorCanonical", [IsSemigroup]);

DeclareProperty("IsSemigroupEnumerator", IsEnumeratorByFunctions);

DeclareAttribute("RulesOfSemigroup", IsSemigroup);

DeclareAttribute("GapFroidurePin", IsSemigroup, "mutable");
DeclareProperty("CanComputeGapFroidurePin", IsSemigroup);

DeclareProperty("CanComputeFroidurePin", IsSemigroup);
DeclareOperation("HasFroidurePin", [IsSemigroup]);

DeclareOperation("IdempotentsSubset",
                 [IsSemigroup and CanComputeFroidurePin, IsHomogeneousList]);
