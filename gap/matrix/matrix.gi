############################################################################
##
#W  matrix.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##                                                         Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#
#T This will be moved to a more appropriate place
#
InstallMethod( TriangulizeMat,
    "generic method for mutable matrix objects",
    [ IsMatrixObj and IsMutable ],
    function ( mat )
    local d, m, n, i, j, k, row, zero, x, row2;
                  
    Info( InfoMatrix, 1, "TriangulizeMat called" );

    d := DimensionsMat(mat);
    m := d[1]; n := d[2];
    
    if not (m = 0 or n = 0) then  
       # get the size of the matrix
       zero := Zero( BaseDomain(mat) );
              
       # make sure that the rows are mutable
       # for i in [ 1 .. m ] do
       #   if not IsMutable( mat[i] ) then
       #     mat[i]:= ShallowCopy( mat[i] );
       #   fi;
       # od;

       # run through all columns of the matrix
       i := 0;
       for k  in [1..n]  do
           # find a nonzero entry in this column
           j := i + 1;
           while j <= m and mat[j][k] = zero  do j := j + 1;  od;

           # if there is a nonzero entry
           if j <= m  then

               # increment the rank
               Info( InfoMatrix, 2, "  nonzero columns: ", k );
               i := i + 1;

               # make its row the current row and normalize it
               row    := mat[j];
               mat[j] := mat[i];
               x:= Inverse( row[k] );
               if x = fail then
                 TryNextMethod();
               fi;
               MultRowVector( row, x );
               mat[i] := row;

               # clear all entries in this column
               for j  in [1..i-1] do
                   row2 := mat[j];
                   x := row2[k];
                   if   x <> zero  then
                       AddRowVector( row2, row, - x );
                   fi;
               od;
               for j  in [i+1..m] do
                   row2 := mat[j];
                   x := row2[k];
                   if   x <> zero  then
                       AddRowVector( row2, row, - x );
                   fi;
               od;
           fi;
       od;
    fi;

    Info( InfoMatrix, 1, "TriangulizeMat returns" );
end );





#############################################################################
##
#M  SemiEchelonMat( <mat> )
##
InstallMethod( SemiEchelonMatDestructive,
    "generic method for matrix objects", 
    [ IsMatrixObj and IsMutable],
    function( mat )
    local zero,      # zero of the field of <mat>
          dims,      # dims of matrix entries
          nrows,     # number of rows in <mat>
          ncols,     # number of columns in <mat>
          vectors,   # list of basis vectors
          heads,     # list of pivot positions in `vectors'
          i,         # loop over rows
          j,         # loop over columns
          x,         # a current element
          nzheads,   # list of non-zero heads
          row,       # the row of current interest
          inv;       # inverse of a matrix entry

    dims := DimensionsMat(mat);
    
    nrows := dims[1];
    ncols := dims[2];
    
    zero := Zero(BaseDomain(mat));
    
    heads:= ListWithIdenticalEntries( ncols, 0 );
    nzheads := []; 
    vectors := [];
        
    for i in [ 1 .. nrows ] do
       
        row := mat[i];
        # Reduce the row with the known basis vectors.
        for j in [ 1 .. Length(nzheads) ] do
            x := row[nzheads[j]];
            if x <> zero then
              AddRowVector( row, vectors[ j ], - x );
            fi;
        od;
          
        #j := PositionNot( row, zero );
        j := 1;
        while (j <= ncols) and (row[j] = zero) do
            j := j+1;
        od;
        
        if j <= ncols then
          
            # We found a new basis vector.
            inv:= Inverse( row[j] );
            if inv = fail then
                Error("fail");
                return fail;
            fi;
            MultRowVector( row, inv );
            Add( vectors, row );
            Add( nzheads, j );
            heads[j]:= Length( vectors );

        fi;
    
    od;
       
    return rec( heads   := heads,
                vectors := vectors );
end );

InstallMethod( SemiEchelonMat,
        "generic method for matrix objects",
        [ IsMatrixObj ],
function(mat)
    local copy;
    
    copy := StructuralCopy(mat);
    return SemiEchelonMatDestructive(copy);
end);

###
#M MoorePenroseInverse
#
# This is certainly NOT the most efficient way of doing this
#
InstallMethod( MoorePenroseInverse,
        "for a matrix over a field",
        [ IsMatrixObj ],
function(mat)
    local C, D, E, F, n, i, rows;
    
    if not IsField(BaseDomain(mat)) then
        Error("This method only works for matrices over fields\n");
    fi;
    
    D := MutableCopyMat(mat);
    TriangulizeMat(D);
    D := D{ [1..PositionLastNonZero(D)] };
    
    n := DimensionsMat(mat)[1];
    rows := [];
    
    C := TransposedMat(mat);
    
    for i in [1..DimensionsMat(D)[1]] do
        if PositionNonZero(D[i]) <= n then
            Add(rows, C[i]);
        fi;
    od;
    
    C := TransposedMat(NewMatrix(IsPlistMatrixRep, BaseDomain(mat), n, rows));
    
    # Instead of transposedmat, this probably has to be
    # transposed conjugate
    E := C * (TransposedMat(C) * C) ^ (-1);
    F := (D * TransposedMat(D)) ^ (-1) * D;
    
    return E * F;
end);

