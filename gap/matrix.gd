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
DeclareCategory("IsSVector", IsAdditiveElementWithInverse);
DeclareCategoryCollections("IsSVector");
DeclareCategoryCollections("IsSVectorCollection");

DeclareConstructor("NewSVector", [IsSVector, IsRing, IsInt, IsList]);
DeclareConstructor("NewSVector", [IsSVector, IsRing, IsInt, IsPlistVectorRep]);
DeclareConstructor("NewZeroSVector", [IsSVector, IsRing, IsInt]);

DeclareAttribute("DegreeOfSVector", IsSVector);
DeclareAttribute("BaseDomain", IsSVector);

# mpf: 
DeclareCategory("IsSRowBasis", IsObject);
DeclareCategoryCollections("IsSRowBasis");
DeclareConstructor("NewSRowBasis", [IsSRowBasis, IsRing, IsList]);

DeclareOperation("Rank", [IsSRowBasis]);
DeclareAttribute("BaseDomain", IsSRowBasis);

# Our Matrix objects
DeclareCategory("IsSMatrix", IsMultiplicativeElementWithInverse and 
                             IsAssociativeElement);
DeclareCategoryCollections("IsSMatrix");
DeclareCategoryCollections("IsSMatrixCollection");

DeclareConstructor("NewSMatrix", [IsSMatrix, IsRing, IsInt, IsList]);
DeclareConstructor("NewSMatrix", [IsSMatrix, IsRing, IsInt, IsPlistMatrixRep]);
DeclareConstructor("NewIdentitySMatrix", [IsSMatrix, IsRing, IsInt]);
DeclareConstructor("NewZeroSMatrix", [IsSMatrix, IsRing, IsInt]);

# These bases are in normal form
DeclareAttribute("RowSpaceBasis", IsSMatrix);
DeclareAttribute("RowSpaceTransformation", IsSMatrix);
DeclareAttribute("RowSpaceTransformationInv", IsSMatrix);
DeclareAttribute("ColSpaceBasis", IsSMatrix);
DeclareAttribute("ColSpaceTransformation", IsSMatrix);
DeclareAttribute("ColSpaceTransformationInv", IsSMatrix);
DeclareAttribute("RightInverse", IsSMatrix);
DeclareAttribute("LeftInverse", IsSMatrix);
DeclareAttribute("SemigroupInvertable", IsSMatrix);
DeclareAttribute("SemigroupInverse", IsSMatrix);
DeclareAttribute("DegreeOfSMatrix", IsSMatrix);
DeclareAttribute("RowRank", IsSMatrix);
DeclareAttribute("ColRank", IsSMatrix);
DeclareAttribute("BaseDomain", IsSMatrix);
DeclareAttribute("TransposedMatImmutable", IsSMatrix);
DeclareOperation("AsMatrix", [IsSMatrix]);
DeclareOperation("AsMatrix", [IsMatrixObj]);
DeclareOperation("AsSMatrix", [IsSMatrix, IsMatrix]);
DeclareOperation("AsSMatrix", [IsMatrix]);
DeclareOperation("ConstructingFilter", [IsSMatrix]);


# We need to wrap vector collections to circumvent problems
# with zero spaces.

#
# Here come two concrete implementations of SVectors and SMatrices
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
DeclareRepresentation("IsPlistSVectorRep",
  IsSVector and IsComponentObjectRep and IsAttributeStoringRep, ["vec"]);
BindGlobal("PlistSVectorFamily", NewFamily("PlistSVectorFamily",
  IsSVector, CanEasilyCompareElements));
BindGlobal("PlistSVectorType", NewType(PlistSVectorFamily,
  IsSVector and IsPlistSVectorRep ));

DeclareRepresentation("IsPlistListSRowBasisRep",
  IsSRowBasis and IsComponentObjectRep and IsAttributeStoringRep, ["rows"]);
BindGlobal("PlistListSRowBasisFamily", NewFamily("PlistListSRowBasisFamily",
  IsSRowBasis, CanEasilyCompareElements));
BindGlobal("PlistListSRowBasisType", NewType(PlistListSRowBasisFamily,
  IsSRowBasis and IsPlistListSRowBasisRep ));

DeclareRepresentation("IsPlistSMatrixRep",
  IsSMatrix and IsComponentObjectRep and IsAttributeStoringRep, ["mat"]);
BindGlobal("PlistSMatrixFamily", NewFamily("PlistSMatrixFamily",
  IsSMatrix, CanEasilyCompareElements));
BindGlobal("PlistSMatrixType", NewType(PlistSMatrixFamily,
  IsSMatrix and IsPlistSMatrixRep ));

# Vectors and matrices from the CVEC package
DeclareRepresentation("IsCVECSVectorRep",
  IsSVector and IsComponentObjectRep and IsAttributeStoringRep, []);
BindGlobal("CVECSVectorFamily", NewFamily("CVECSVectorFamily",
  IsSVector, CanEasilyCompareElements));
BindGlobal("CVECSVectorType", NewType(CVECSVectorFamily,
  IsSVector and IsCVECSVectorRep));

DeclareRepresentation("IsCVECSMatrixRep",
  IsSMatrix and IsComponentObjectRep and IsAttributeStoringRep, []);
BindGlobal("CVECSMatrixFamily", NewFamily("CVECSMatrixFamily",
  IsSMatrix, CanEasilyCompareElements));
BindGlobal("CVECSMatrixType", NewType(CVECSMatrixFamily,
  IsSMatrix and IsCVECSMatrixRep));

DeclareGlobalFunction( "RandomSMatrix" );

DeclareProperty("IsZero", IsSMatrix);

DeclareOperation("IdentitySMatrix", [IsField and IsFinite, IsPosInt]);
DeclareOperation("IdentitySMatrix", [IsField and IsFinite, IsZeroCyc]);
DeclareOperation("IdentitySMatrix", [IsSMatrix, IsPosInt]);
DeclareOperation("IdentitySMatrix", [IsSMatrix, IsZeroCyc]);
DeclareOperation("TransposedSMat", [IsSMatrix]);
DeclareAttribute("DegreeOfSMatrixCollection", IsSMatrixCollection);
DeclareAttribute("BaseDomain", IsSMatrixCollection);

## Helper functions
DeclareGlobalFunction("ComputeRowSpaceAndTransformation");
DeclareGlobalFunction("RandomListOfMatricesWithRanks");
DeclareGlobalFunction("RandomSquareSMatrixWithRanks");

## We need a mutable copy of matrices sometimes to do calculations
DeclareGlobalFunction("SEMIGROUPS_MutableCopyMat");
## IsZero is an attribute that is stored, and hence we have 
## this function for debugging purposes that checks whether a 
## matrix is actually zero by inspecting all entries
DeclareGlobalFunction("SEMIGROUPS_CheckReallyZero");
;
