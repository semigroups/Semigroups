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
# Froidure-Pin algorithm for enumerating arbitrary semigroups, i.e. the version
# that is not in libsemigroups.

#  For details see:
#
#  V. Froidure, and J.-E. Pin, Algorithms for computing finite semigroups.
#  Foundations of computational mathematics (Rio de Janeiro, 1997), 112-126,
#  Springer, Berlin,  1997.

DeclareAttribute("GapFroidurePin", IsSemigroup, "mutable");
DeclareProperty("CanComputeGapFroidurePin", IsSemigroup);

DeclareProperty("CanComputeFroidurePin", IsSemigroup);
DeclareOperation("HasFroidurePin", [IsSemigroup]);

DeclareAttribute("AsListCanonical",
                 IsSemigroup and CanComputeFroidurePin);
DeclareOperation("PositionCanonical",
                 [IsSemigroup and CanComputeFroidurePin,
                  IsMultiplicativeElement]);

DeclareOperation("PositionSortedOp",
                 [IsSemigroup and CanComputeFroidurePin,
                 IsMultiplicativeElement]);
DeclareOperation("PositionOp",
                 [IsSemigroup and CanComputeFroidurePin,
                  IsMultiplicativeElement,
                  IsZeroCyc]);
DeclareOperation("Position",
                 [IsSemigroup and CanComputeFroidurePin,
                  IsMultiplicativeElement]);
DeclareOperation("Position",
                 [IsSemigroup and CanComputeFroidurePin,
                  IsMultiplicativeElement,
                  IsZeroCyc]);

DeclareOperation("Enumerate", [IsSemigroup and CanComputeFroidurePin, IsInt]);
DeclareOperation("Enumerate", [IsSemigroup and CanComputeFroidurePin]);

DeclareAttribute("LeftCayleyDigraph", IsSemigroup and CanComputeFroidurePin);
DeclareAttribute("RightCayleyDigraph", IsSemigroup and CanComputeFroidurePin);

DeclareAttribute("EnumeratorCanonical", IsSemigroup and CanComputeFroidurePin);
DeclareOperation("IteratorCanonical", [IsSemigroup and CanComputeFroidurePin]);

DeclareProperty("IsSemigroupEnumerator", IsEnumeratorByFunctions);

DeclareAttribute("RulesOfSemigroup", IsSemigroup and CanComputeFroidurePin);

DeclareOperation("IdempotentsSubset",
                 [IsSemigroup and CanComputeFroidurePin, IsHomogeneousList]);
