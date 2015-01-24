############################################################################
##
#W  matrix.gd
#Y  Copyright (C) 2014                                   James D. Mitchell
##                                                         Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#
# In the best spirit of GAP development we implement our own matrix
# methods.
#
# This aims to be compatible with the MatrixObj interface in the
# GAP library and might at some point be moved to the library
#
DeclareOperation( "TriangulizeMat",
        [ IsMatrixObj and IsMutable ]);

DeclareOperation( "SemiEchelonMatDestructive",
        [ IsMatrixObj and IsMutable ]);

DeclareAttribute( "SemiEchelonMat",
        IsMatrixObj );

DeclareOperation( "SemiEchelonMatTransformationDestructive",
        [ IsMatrixObj and IsMutable ]);

DeclareAttribute( "SemiEchelonMatTransformation",
        IsMatrixObj );

DeclareGlobalFunction( "BaseSteinitzMatrixObj");


DeclareOperation( "MoorePenroseInverse",
        [ IsMatrixObj ] );

DeclareOperation( "PseudoInverse",
        [ IsMatrixObj ] );

DeclareGlobalFunction( "PedestrianLambdaInverse" );

DeclareGlobalFunction( "RandomMatrixObj" );

# To make the GAP library code happy for now
#DeclareOperation( "ImmutableMatrix", [ IsObject, IsMatrixObj ]);


