############################################################################
##
#W  ffmat.gd
#Y  Copyright (C) 2016                                   James D. Mitchell
##                                                         Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
############################################################################
##

############################################################################
#
# In the best spirit of GAP development we implement our own matrix methods.
#
# We declare our own representation so we do not interfere with the GAP library
# or other libraries and dispatch to things we know to work.
#
# This code is based on the IsPlistMatrixRep code from the GAP library.
#
# There is almost no hope whatsoever that we will ever be able to use MatrixObj
# in general for semigroups, because of MatrixObjs being in CategoryCollections
# of their entries and this leading to conflicts when checking for
# IsAssociativeElementCollection (which is false for collections of
# IsMatrixObj).
#
############################################################################

#############################################################################
## Specializations of declarations for MatrixOverSemiring
#############################################################################

DeclareCategory("IsMatrixOverFiniteField", IsMatrixOverSemiring);

DeclareCategoryCollections("IsMatrixOverFiniteField");
DeclareCategoryCollections("IsMatrixOverFiniteFieldCollection");

DeclareAttribute("AsList", IsPlistMatrixRep);
DeclareOperation("AsMutableList", [IsMatrix]);

#############################################################################
## Declarations specifically for finite field vectors
#############################################################################

DeclareCategory("IsRowBasisOverFiniteField", IsCollection);
DeclareCategoryCollections("IsRowBasisOverFiniteField");
DeclareConstructor("NewRowBasisOverFiniteField",
                   [IsRowBasisOverFiniteField, IsRing, IsList]);

DeclareAttribute("BaseDomain", IsRowBasisOverFiniteField);

#############################################################################
## Declarations specifically for finite field matrices
#############################################################################

DeclareConstructor("NewMatrixOverFiniteField",
                   [IsMatrixOverFiniteField, IsRing, IsList]);
DeclareConstructor("NewMatrixOverFiniteField",
                   [IsMatrixOverFiniteField, IsRing, IsInt, IsPlistMatrixRep]);

#TODO make these cons/opers/attributes for MatrixOverSemiring
DeclareConstructor("NewIdentityMatrixOverFiniteField",
                   [IsMatrixOverFiniteField, IsRing, IsInt]);
DeclareConstructor("NewZeroMatrixOverFiniteField",
                   [IsMatrixOverFiniteField, IsRing, IsInt]);
DeclareOperation("IdentityMatrixOverFiniteField",
                 [IsField and IsFinite, IsPosInt]);
DeclareOperation("IdentityMatrixOverFiniteField",
                 [IsField and IsFinite, IsZeroCyc]);
DeclareOperation("IdentityMatrixOverFiniteField",
                 [IsMatrixOverFiniteField, IsPosInt]);
DeclareOperation("IdentityMatrixOverFiniteField",
                 [IsMatrixOverFiniteField, IsZeroCyc]);

# These bases are in normal form
DeclareAttribute("RowSpaceBasis", IsMatrixOverFiniteField);
DeclareAttribute("RowSpaceTransformation", IsMatrixOverFiniteField);
DeclareAttribute("RowSpaceTransformationInv", IsMatrixOverFiniteField);

#T FIXME: Implement
#DeclareAttribute("ColSpaceBasis", IsMatrixOverFiniteField);
#DeclareAttribute("ColSpaceTransformation", IsMatrixOverFiniteField);
#DeclareAttribute("ColSpaceTransformationInv", IsMatrixOverFiniteField);

DeclareAttribute("RightInverse", IsMatrixOverFiniteField);
DeclareAttribute("LeftInverse", IsMatrixOverFiniteField);
#T FIXME: Implement
#DeclareAttribute("SemigroupInvertable", IsMatrixOverFiniteField);
#DeclareAttribute("SemigroupInverse", IsMatrixOverFiniteField);

DeclareAttribute("RowRank", IsMatrixOverFiniteField);
DeclareAttribute("BaseDomain", IsMatrixOverFiniteField);

# FIXME shouldn't this be IsMultiplicativeZero??
DeclareProperty("IsZero", IsMatrixOverFiniteField);

#############################################################################
## Declarations specifically for finite field matrices collections
#############################################################################

#TODO make these cons/opers/attributes for MatrixOverSemiring
DeclareAttribute("BaseDomain", IsMatrixOverFiniteFieldCollection);

#############################################################################
## Declaration of representations of vectors and matrices
#############################################################################

# We need to wrap vector collections to circumvent problems with zero spaces.

# Here come two concrete implementations of VectorOverFiniteFields and
# MatricesOverFiniteField
#
# We might want to store transforming matrices for ColSpaceBasis/RowSpaceBasis?
# We also need operations for acting on Row/Column spaces.

# For the time being we are "happy" with the PlistMatrixRep representation as
# showcased in the library code. Of course we implement our own variant of it
# just to be sure that we duplicate enough code
#
#T Do the rows of our SPlistMatrixRep need to be SPlistRowVectorRep? Or is #T
# it good enough to allow IsPlistRowVectorRep?
#
# What about AttributeStoringRep? Is it desirable to just store RowSpaceBasis
# ColumnSpaceBasis as Attributes?

# Vectors and matrices stored as GAP Plists

DeclareRepresentation("IsPlistRowBasisOverFiniteFieldRep",
                      IsRowBasisOverFiniteField and IsComponentObjectRep and
                      IsAttributeStoringRep, ["rows"]);
BindGlobal("PlistRowBasisOverFiniteFieldFamily",
           NewFamily("PlistRowBasisOverFiniteFieldFamily",
           IsRowBasisOverFiniteField, CanEasilyCompareElements));
BindGlobal("PlistRowBasisOverFiniteFieldType",
           NewType(PlistRowBasisOverFiniteFieldFamily,
                   IsRowBasisOverFiniteField
                   and IsPlistRowBasisOverFiniteFieldRep));

DeclareRepresentation("IsPlistMatrixOverFiniteFieldRep",
                      IsMatrixOverFiniteField and IsComponentObjectRep and
                      IsAttributeStoringRep, ["mat"]);
BindGlobal("PlistMatrixOverFiniteFieldFamily",
           NewFamily("PlistMatrixOverFiniteFieldFamily",
                     IsMatrixOverFiniteField, CanEasilyCompareElements));
BindGlobal("PlistMatrixOverFiniteFieldType",
           NewType(PlistMatrixOverFiniteFieldFamily,
                   IsMatrixOverFiniteField and
                   IsPlistMatrixOverFiniteFieldRep));

#############################################################################
## Helper functions
#############################################################################

DeclareGlobalFunction("ComputeRowSpaceAndTransformation");
