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

DeclareRepresentation("IsEnumerableSemigroupRep",
                      IsSemigroup and IsComponentObjectRep,
                      ["__en_semi_fropin", "__en_semi_cpp_semi"]);

DeclareProperty("IsGeneratorsOfEnumerableSemigroup",
                IsMultiplicativeElementCollection);

DeclareOperation("PositionSortedOp",
                 [IsEnumerableSemigroupRep, IsMultiplicativeElement]);
DeclareOperation("PositionOp",
                 [IsEnumerableSemigroupRep,
                  IsMultiplicativeElement,
                  IsZeroCyc]);
DeclareOperation("Position",
                 [IsEnumerableSemigroupRep, IsMultiplicativeElement]);
DeclareOperation("Position",
                 [IsEnumerableSemigroupRep, IsMultiplicativeElement,
                  IsZeroCyc]);

DeclareAttribute("AsListCanonical", IsEnumerableSemigroupRep);
DeclareAttribute("EnumeratorCanonical", IsEnumerableSemigroupRep);
DeclareOperation("IteratorCanonical", [IsEnumerableSemigroupRep]);
DeclareOperation("PositionCanonical",
                 [IsEnumerableSemigroupRep, IsMultiplicativeElement]);

DeclareOperation("Enumerate", [IsEnumerableSemigroupRep, IsInt]);
DeclareOperation("Enumerate", [IsEnumerableSemigroupRep]);

DeclareOperation("IsFullyEnumerated", [IsEnumerableSemigroupRep]);

DeclareProperty("IsSemigroupEnumerator", IsEnumeratorByFunctions);

DeclareAttribute("LeftCayleyDigraph", IsEnumerableSemigroupRep);
DeclareAttribute("RightCayleyDigraph", IsEnumerableSemigroupRep);
