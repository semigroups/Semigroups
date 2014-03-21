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


