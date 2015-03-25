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
# There is almost no hope whatsoever that we will ever be able to use
# MatrixObj in general for semigroups
#
############################################################################
#
# Our Matrix objects
#
DeclareCategory("IsSMatrix", IsMultiplicativeElementWithInverse and 
                             IsAssociativeElement);
DeclareCategoryCollections("IsSMatrix");
DeclareCategoryCollections("IsSMatrixCollection");

#BindGlobal("SMatrixFamily", NewFamily("SMatrixFamily",
# IsSMatrix, CanEasilyCompareElements));
#BindGlobal("SMatrixType", NewType(SMatrixFamily,
# IsSMatrix and IsComponentObjectRep and IsAttributeStoringRep));

DeclareConstructor("NewSMatrix", [IsSMatrix, IsRing, IsInt, IsList]);

# These bases are in normal form
DeclareAttribute("RowSpaceBasis", IsSMatrix);
DeclareAttribute("ColSpaceBasis", IsSMatrix);
DeclareAttribute("RightInverse", IsSMatrix);
DeclareAttribute("LeftInverse", IsSMatrix);
DeclareAttribute("DegreeOfSMatrix", IsSMatrix);
DeclareAttribute("RowRank", IsSMatrix);
DeclareAttribute("ColRank", IsSMatrix);
DeclareAttribute("BaseDomain", IsMatrix);
DeclareOperation("AsMatrix", [IsSMatrix]);

# We might want to store transforming matrices for ColSpaceBasis/RowSpaceBasis?
# We also need operations for acting on Row/Column spaces.

# The following  should really be obsolete, or at the very least not needed
# for our purposes. We need to compute a RowReduced form and a ColumnReduced form
# and store the transformation matrices.
DeclareOperation( "TriangulizeMat", [ IsSMatrix ]);
DeclareOperation( "SemiEchelonMatDestructive",
        [ IsSMatrix and IsMutable ]);
DeclareOperation( "SemiEchelonMatTransformationDestructive",
        [ IsSMatrix and IsMutable ]);
DeclareAttribute( "SemiEchelonMatTransformation",
        IsSMatrix );

# For the time being we are "happy" with the PlistMatrixRep representation
# as showcased in the library code. Of course we implement our own variant
# of it just to be sure that we duplicate enough code
#
#T Do the rows of our SPlistMatrixRep need to be SPlistRowVectorRep? Or is
#T it good enough to allow IsPlistRowVectorRep?
#
# What about AttributeStoringRep? Is it desirable to just store RowSpaceBasis
# ColumnSpaceBasis as Attributes?
DeclareRepresentation("IsPlistSMatrixRep",
  IsSMatrix and IsComponentObjectRep, []);
BindGlobal("PlistSMatrixFamily", NewFamily("PlistSMatrixFamily",
  IsSMatrix, CanEasilyCompareElements));
BindGlobal("PlistSMatrixType", NewType(PlistSMatrixFamily,
  IsSMatrix and IsPlistSMatrixRep));

DeclareRepresentation("IsCVECSMatrixRep",
  IsSMatrix and IsComponentObjectRep, []);
BindGlobal("CVECSMatrixFamily", NewFamily("CVECSMatrixFamily",
  IsSMatrix, CanEasilyCompareElements));
BindGlobal("CVECSMatrixType", NewType(CVECSMatrixFamily,
  IsSMatrix and IsCVECSMatrixRep));

DeclareGlobalFunction( "RandomSMatrix" );

DeclareOperation("IdentitySMatrix", [IsField and IsFinite, IsPosInt]);
DeclareOperation("IdentitySMatrix", [IsField and IsFinite, IsZeroCyc]);
