############################################################################
##
#W  matrix.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##                                                         Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
############################################################################
##

############################################################################
#
# In the best spirit of GAP development we implement our own matrix
# methods.
#
# We declare our own representation so we do not interfere with the GAP
# library or other libraries and dispatch to things we know to work.
#
# This code is based on the IsPlistMatrixRep code from the GAP library
#
# There is almost no hope whatsoever that we will ever be able to use
# MatrixObj in general for semigroups, because of MatrixObjs being in
# CategoryCollections of their entries and this leading to conflicts when
# checking for IsAssociativeElementCollection (which is false for collections
# of IsMatrixObj).
#
############################################################################

# Our Vector objects
DeclareCategory("IsVectorOverFiniteField", IsAdditiveElementWithInverse);
DeclareCategoryCollections("IsVectorOverFiniteField");
DeclareCategoryCollections("IsVectorOverFiniteFieldCollection");

DeclareConstructor("NewVectorOverFiniteField",
                   [IsVectorOverFiniteField, IsRing, IsInt, IsList]);
DeclareConstructor("NewVectorOverFiniteField",
                   [IsVectorOverFiniteField, IsRing, IsInt, IsPlistVectorRep]);
DeclareConstructor("NewZeroVectorOverFiniteField",
                   [IsVectorOverFiniteField, IsRing, IsInt]);

DeclareAttribute("DegreeOfVectorOverFiniteField", IsVectorOverFiniteField);
DeclareAttribute("BaseDomain", IsVectorOverFiniteField);

# mpf:
DeclareCategory("IsRowBasisOverFiniteField", IsCollection);
DeclareCategoryCollections("IsRowBasisOverFiniteField");
DeclareConstructor("NewRowBasisOverFiniteField", [IsRowBasisOverFiniteField, IsRing, IsList]);

# DeclareOperation("Rank", [IsRowBasisOverFiniteField]);
DeclareAttribute("BaseDomain", IsRowBasisOverFiniteField);

# Our Matrix objects
DeclareCategory("IsMatrixOverFiniteField",
                IsMultiplicativeElementWithInverse
                and IsAssociativeElement);
DeclareCategoryCollections("IsMatrixOverFiniteField");
DeclareCategoryCollections("IsMatrixOverFiniteFieldCollection");

DeclareConstructor("NewMatrixOverFiniteField",
                   [IsMatrixOverFiniteField, IsRing, IsInt, IsList]);
DeclareConstructor("NewMatrixOverFiniteField",
                   [IsMatrixOverFiniteField, IsRing, IsInt, IsPlistMatrixRep]);
DeclareConstructor("NewIdentityMatrixOverFiniteField",
                   [IsMatrixOverFiniteField, IsRing, IsInt]);
DeclareConstructor("NewZeroMatrixOverFiniteField",
                   [IsMatrixOverFiniteField, IsRing, IsInt]);

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
DeclareAttribute("DegreeOfMatrixOverFiniteField", IsMatrixOverFiniteField);
DeclareAttribute("RowRank", IsMatrixOverFiniteField);
DeclareAttribute("ColRank", IsMatrixOverFiniteField);
DeclareAttribute("BaseDomain", IsMatrixOverFiniteField);
DeclareAttribute("TransposedMatImmutable", IsMatrixOverFiniteField);
DeclareOperation("AsMatrix", [IsMatrixOverFiniteField]);
DeclareOperation("AsMatrix", [IsMatrixObj]);
DeclareOperation("AsMatrixOverFiniteField",
                 [IsMatrixOverFiniteField, IsMatrix]);
DeclareOperation("AsMatrixOverFiniteField", [IsMatrix]);
DeclareOperation("ConstructingFilter", [IsMatrixOverFiniteField]);

# We need to wrap vector collections to circumvent problems
# with zero spaces.

# Here come two concrete implementations of VectorOverFiniteFields and
# MatricesOverFiniteField
#
# We might want to store transforming matrices for ColSpaceBasis/RowSpaceBasis?
# We also need operations for acting on Row/Column spaces.

# For the time being we are "happy" with the PlistMatrixRep representation
# as showcased in the library code. Of course we implement our own variant
# of it just to be sure that we duplicate enough code
#
#T Do the rows of our SPlistMatrixRep need to be SPlistRowVectorRep? Or is
#T it good enough to allow IsPlistRowVectorRep?
#
# What about AttributeStoringRep? Is it desirable to just store RowSpaceBasis
# ColumnSpaceBasis as Attributes?

# Vectors and matrices stored as GAP Plists
DeclareRepresentation("IsPlistVectorOverFiniteFieldRep",
                      IsVectorOverFiniteField and IsComponentObjectRep
                      and IsAttributeStoringRep, ["vec"]);
BindGlobal("PlistVectorOverFiniteFieldFamily",
           NewFamily("PlistVectorOverFiniteFieldFamily",
                     IsVectorOverFiniteField, CanEasilyCompareElements));
BindGlobal("PlistVectorOverFiniteFieldType",
           NewType(PlistVectorOverFiniteFieldFamily,
                   IsVectorOverFiniteField and
                   IsPlistVectorOverFiniteFieldRep));

DeclareRepresentation("IsPlistRowBasisOverFiniteFieldRep",
                      IsRowBasisOverFiniteField and IsComponentObjectRep and
                      IsAttributeStoringRep, ["rows"]);
BindGlobal("PlistRowBasisOverFiniteFieldFamily",
           NewFamily("PlistRowBasisOverFiniteFieldFamily", IsRowBasisOverFiniteField,
                     CanEasilyCompareElements));
BindGlobal("PlistRowBasisOverFiniteFieldType",
           NewType(PlistRowBasisOverFiniteFieldFamily, IsRowBasisOverFiniteField and IsPlistRowBasisOverFiniteFieldRep));

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

# Vectors and matrices from the CVEC package
DeclareRepresentation("IsCVECVectorOverFiniteFieldRep",
                      IsVectorOverFiniteField and IsComponentObjectRep and
                      IsAttributeStoringRep, []);
BindGlobal("CVECVectorOverFiniteFieldFamily",
           NewFamily("CVECVectorOverFiniteFieldFamily",
           IsVectorOverFiniteField, CanEasilyCompareElements));
BindGlobal("CVECVectorOverFiniteFieldType",
           NewType(CVECVectorOverFiniteFieldFamily,
                   IsVectorOverFiniteField and
                   IsCVECVectorOverFiniteFieldRep));

DeclareRepresentation("IsCVECMatrixOverFiniteFieldRep",
                      IsMatrixOverFiniteField and IsComponentObjectRep and
                      IsAttributeStoringRep, []);
BindGlobal("CVECMatrixOverFiniteFieldFamily",
           NewFamily("CVECMatrixOverFiniteFieldFamily",
                     IsMatrixOverFiniteField, CanEasilyCompareElements));
BindGlobal("CVECMatrixOverFiniteFieldType",
           NewType(CVECMatrixOverFiniteFieldFamily,
                   IsMatrixOverFiniteField and
                   IsCVECMatrixOverFiniteFieldRep));

DeclareGlobalFunction("RandomMatrixOverFiniteField");

DeclareProperty("IsZero", IsMatrixOverFiniteField);

DeclareOperation("IdentityMatrixOverFiniteField",
                 [IsField and IsFinite, IsPosInt]);
DeclareOperation("IdentityMatrixOverFiniteField",
                 [IsField and IsFinite, IsZeroCyc]);
DeclareOperation("IdentityMatrixOverFiniteField",
                 [IsMatrixOverFiniteField, IsPosInt]);
DeclareOperation("IdentityMatrixOverFiniteField",
                 [IsMatrixOverFiniteField, IsZeroCyc]);
DeclareOperation("TransposedSMat", [IsMatrixOverFiniteField]);
DeclareAttribute("DegreeOfMatrixOverFiniteFieldCollection",
                 IsMatrixOverFiniteFieldCollection);
DeclareAttribute("BaseDomain", IsMatrixOverFiniteFieldCollection);
DeclareOperation("OneMutable", [IsMatrixOverFiniteFieldCollection]);

## Helper functions
DeclareGlobalFunction("ComputeRowSpaceAndTransformation");
DeclareGlobalFunction("RandomListOfMatricesWithRanks");
DeclareGlobalFunction("RandomSquareMatrixOverFiniteFieldWithRanks");

## We need a mutable copy of matrices sometimes to do calculations
DeclareGlobalFunction("SEMIGROUPS_MutableCopyMat");
## IsZero is an attribute that is stored, and hence we have
## this function for debugging purposes that checks whether a
## matrix is actually zero by inspecting all entries
DeclareGlobalFunction("SEMIGROUPS_CheckReallyZero");


