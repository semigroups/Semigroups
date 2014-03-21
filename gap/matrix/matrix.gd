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
#############################################################################
##
#M  TriangulizeMat( <mat> ) . . . . . bring a matrix in upper triangular form
##
DeclareOperation( "TriangulizeMat", 
        [ IsMatrixObj and IsMutable ]);
        
DeclareOperation( "SemiEchelonMat"
        [ IsMatrixObj and IsMutable ]);
