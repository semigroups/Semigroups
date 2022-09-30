###########################################################################
##
##  main/froidure-pin.gd
##  Copyright (C) 2015-2022                              James D. Mitchell
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
DeclareProperty("CanUseGapFroidurePin", IsSemigroup);

DeclareProperty("CanUseFroidurePin", IsSemigroup);
DeclareOperation("HasFroidurePin", [IsSemigroup]);

DeclareAttribute("AsListCanonical",
                 IsSemigroup and CanUseFroidurePin);
DeclareOperation("PositionCanonical",
                 [IsSemigroup and CanUseFroidurePin,
                  IsMultiplicativeElement]);

DeclareOperation("PositionSortedOp",
                 [IsSemigroup and CanUseFroidurePin,
                 IsMultiplicativeElement]);
DeclareOperation("PositionOp",
                 [IsSemigroup and CanUseFroidurePin,
                  IsMultiplicativeElement,
                  IsZeroCyc]);
DeclareOperation("Position",
                 [IsSemigroup and CanUseFroidurePin,
                  IsMultiplicativeElement]);
DeclareOperation("Position",
                 [IsSemigroup and CanUseFroidurePin,
                  IsMultiplicativeElement,
                  IsZeroCyc]);

DeclareOperation("Enumerate", [IsSemigroup and CanUseFroidurePin, IsInt]);
DeclareOperation("Enumerate", [IsSemigroup and CanUseFroidurePin]);
DeclareOperation("IsEnumerated", [IsSemigroup]);

DeclareAttribute("LeftCayleyDigraph", IsSemigroup and CanUseFroidurePin);
DeclareAttribute("RightCayleyDigraph", IsSemigroup and CanUseFroidurePin);

DeclareAttribute("EnumeratorCanonical", IsSemigroup and CanUseFroidurePin);
DeclareOperation("IteratorCanonical", [IsSemigroup and CanUseFroidurePin]);

DeclareProperty("IsSemigroupEnumerator", IsEnumeratorByFunctions);

DeclareAttribute("RulesOfSemigroup", IsSemigroup and CanUseFroidurePin);

DeclareOperation("IdempotentsSubset",
                 [IsSemigroup and CanUseFroidurePin, IsHomogeneousList]);

DeclareOperation("FirstLetter",
                 [IsSemigroup and CanUseFroidurePin, IsMultiplicativeElement]);
DeclareOperation("FinalLetter",
                 [IsSemigroup and CanUseFroidurePin, IsMultiplicativeElement]);
DeclareOperation("Prefix",
                 [IsSemigroup and CanUseFroidurePin, IsMultiplicativeElement]);
DeclareOperation("Suffix",
                 [IsSemigroup and CanUseFroidurePin, IsMultiplicativeElement]);
