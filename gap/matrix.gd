############################################################################
##
#W  matrix.gd
#Y  Copyright (C) 2014                                   James D. Mitchell
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
DeclareCategory("IsSMatrix",
  IsMatrixObj );
#and IsAttributeStoringRep);

DeclareAttribute("RowSpaceBasis", IsSMatrix);
DeclareAttribute("ColSpaceBasis", IsSMatrix);
DeclareAttribute("RightInverse", IsSMatrix);
DeclareAttribute("LeftInverse", IsSMatrix);
DeclareAttribute("NrRows", IsSMatrix);
DeclareAttribute("NrCols", IsSMatrix);
DeclareAttribute("RowRank", IsSMatrix);
DeclareAttribute("ColRank", IsSMatrix);
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
DeclareRepresentation("IsSPlistMatrixRep",
  IsRowListMatrix and IsPositionalObjectRep, [] );

BindGlobal("SEMIGROUPS_BDPOS", 1);
BindGlobal("SEMIGROUPS_EMPOS", 2);
BindGlobal("SEMIGROUPS_RLPOS", 3);
BindGlobal("SEMIGROUPS_ROWSPOS", 4);

DeclareGlobalFunction( "RandomSMatrix" );

# Does such a collection need to be of matrices in the same representation,
# or are we happy as long as we get the attributes and operations above?
DeclareOperation("IsSMatrixCollection", [IsCollection]);
