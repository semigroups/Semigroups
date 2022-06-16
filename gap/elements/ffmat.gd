############################################################################
##
##  elements/ffmat.gd
##  Copyright (C) 2016-2022                              James D. Mitchell
##                                                         Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
############################################################################
##

# This file contains some minimal declarations that allow us to use the
# features of IsActingSemigroup with MatrixObj's of finite fields from the GAP
# library.

# We often need to detect whether or not a given mult. elt. is a MatrixObj with
# entries in a finite field, so we have the following property that answers
# this question.
DeclareProperty("IsMatrixObjOverFiniteField", IsMultiplicativeElement);

#############################################################################
# Declarations specifically for finite field vectors
#############################################################################

# The following relate to LambdaOrb and RhoOrb of a semigroup or monoid of
# matrices over IsMatrixObjOverFiniteField. We do not use GAP library vectors
# here (if they even exist) because these are essentially internal and are not
# expected to be created or manipulated directly by the user.

# Note that because matrices of dimension 0 are not permitted in the GAP
# library (possibly for good reasons, such as Is8BitMatrixRep not storing their
# base domain but using their entries to compute the base domain, which
# obviously doesn't work for 0-dim matrices which have no entries), everything
# inside the "acting" data structures (SemigroupData, LambdaOrb, RhoOrb etc)
# for a semigroup of matrices over a finite field use matrices and row bases
# etc of dimension 1 greater than the dimension of the matrices that are user
# facing.

DeclareCategory("IsRowBasisOverFiniteField", IsCollection);
DeclareCategoryCollections("IsRowBasisOverFiniteField");
DeclareConstructor("NewRowBasisOverFiniteField",
                   [IsRowBasisOverFiniteField, IsRing, IsList]);

DeclareRepresentation("IsPlistRowBasisOverFiniteFieldRep",
                      IsRowBasisOverFiniteField and IsComponentObjectRep and
                      IsAttributeStoringRep, ["rows"]);

DeclareAttribute("BaseDomain", IsRowBasisOverFiniteField);

#############################################################################
# Declarations specifically for finite field matrices
#############################################################################

# We require a number of operations for IsMatrixObjOverFiniteField that are not
# provided by the GAP library AFAIK at time of writing in June 2022.

# These bases are in normal form
DeclareAttribute("RowSpaceBasis", IsMatrixObj);
DeclareAttribute("RowSpaceTransformation", IsMatrixObj);
DeclareAttribute("RowSpaceTransformationInv", IsMatrixObj);

DeclareAttribute("RightInverse", IsMatrixObj);
DeclareAttribute("LeftInverse", IsMatrixObj);

DeclareOperation("OneMutable", [IsFFECollCollColl]);

#############################################################################
# Helper functions
#############################################################################

DeclareGlobalFunction("ComputeRowSpaceAndTransformation");
