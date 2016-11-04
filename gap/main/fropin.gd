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


DeclareRepresentation("IsEnumerableSemigroupRep", 
                      IsSemigroup and IsComponentObjectRep,
                      ["__en_semi_fropin", "__en_semi_cpp_semi"]);

DeclareProperty("IsGeneratorsOfEnumerableSemigroup", 
                IsMultiplicativeElementCollection);

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
DeclareOperation("IteratorCanonical", [IsSemigroup]);
DeclareOperation("PositionCanonical", [IsSemigroup, IsMultiplicativeElement]);
#TODO PositionCanonical

DeclareOperation("Enumerate", [IsSemigroup, IsPosInt]);
DeclareOperation("Enumerate", [IsSemigroup]);

DeclareProperty("IsSemigroupEnumerator", IsEnumeratorByFunctions);
