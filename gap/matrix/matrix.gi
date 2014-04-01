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
          
        j := PositionNonZero( row );
        
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
    
    copy := MutableCopyMat(mat);
    return SemiEchelonMatDestructive(copy);
end);

##########################################################################
##
#F  BaseSteinitzMatrixObj( <bas>, <mat> )
##
##  find vectors extending mat to a basis spanning the span of <bas>.
##  'BaseSteinitz'  returns a
##  record  describing  a base  for the factorspace   and ways   to decompose
##  vectors:
##
##  zero:           zero of <V> and <U>
##  factorzero:     zero of complement
##  subspace:       triangulized basis of <mat>
##  factorspace:    base of a complement of <U> in <V>
##  heads:          a list of integers i_j, such that  if i_j>0 then a vector
##                  with head j is at position i_j  in factorspace.  If i_j<0
##                  then the vector is in subspace.
##
InstallGlobalFunction( BaseSteinitzMatrixObj, function(bas,mat)
    local mdims,	# Dimensions of mat
          bdims,	# Dimensions of bas 
          z,l,b,i,j,k,stop,v,dim,h,zv;
    bdims := DimensionsMat(bas);
    
  # catch trivial case
  if bdims[1] = 0 then
    return rec(subspace:=[],factorspace:=[]);
  fi;

  bas := MutableCopyMat(bas);
  
  z := Zero(BaseDomain(bas));
  zv := Zero(bas[1]);
  
  mdims := DimensionsMat(mat);
  dim := bdims[2];
  l := bdims[1] - mdims[1]; # missing dimension
  b := [];
  h := [];
  i := 1;
  j := 1;
  
  while Length(b) < l do
    stop := false;
    repeat
      if j<=dim and (mdims[1]<i or mat[i][j]=z) then
        # Add vector from bas with j-th component not zero (if any exists)
        
        v:=PositionProperty(Rows(bas),k->k[j]<>z);
        if v<>fail then
          # add the vector
          v:=bas[v];
          v:=1/v[j]*v; # normed
          Add(b,v);
          h[j]:=Length(b);
        # if fail, then this dimension is only dependent (and not needed)
        fi;
      else
        stop:=true;
        # check whether we are running to fake zero columns
        if i<=mdims[1] then
          # has a step, clean with basis vector
          v:=mat[i];
          v:=1/v[j]*v; # normed
          h[j]:=-i;
        else
          v:=fail;
        fi;
      fi;
      if v<>fail then
        # clean j-th component from bas with v
        for k in [1..Length(bas)] do
          if not IsZero(bas[k][j]) then
            bas[k]:=bas[k]-bas[k][j]/v[j]*v;
          fi;
        od;
        v:=Zero(v);
        bas:=Matrix(Filtered(Rows(bas),k->k<>v), bdims[2], bas);
      fi;
      j:=j+1;
    until stop;
    i:=i+1;
  od;
  
  # add subspace indices
  while i <= mdims[1] do
    if mat[i][j]<>z then
      h[j]:=-i;
      i:=i+1;
    fi;
    j:=j+1;
  od;


  return rec(factorspace:=b,
             factorzero:=zv,
             subspace:=mat,
             heads:=h);
end );

InstallMethod( SemiEchelonMatTransformationDestructive,
    "generic method for matrices",
    [ IsMatrixObj and IsMutable],
    function( mat )
    local zero,      # zero of the field of <mat>
          nrows,     # number of rows in <mat>
          ncols,     # number of columns in <mat>
          vectors,   # list of basis vectors
          heads,     # list of pivot positions in 'vectors'
          i,         # loop over rows
          j,         # loop over columns
          T,         # transformation matrix
          coeffs,    # list of coefficient vectors for 'vectors'
          relations, # basis vectors of the null space of 'mat'
          row, head, x, row2,f,dims;

    dims := DimensionsMat(mat);
    
    nrows := dims[1];
    ncols := dims[2];

    f := BaseDomain(mat);
    zero := Zero(f);

    heads   := ListWithIdenticalEntries( ncols, 0 );
    vectors := [];

    T         := IdentityMatrix( nrows, mat );
    coeffs    := [];
    relations := [];

    for i in [ 1 .. nrows ] do

        row := mat[i];
        row2 := T[i];

        # Reduce the row with the known basis vectors.
        for j in [ 1 .. ncols ] do
            head := heads[j];
            if head <> 0 then
                x := - row[j];
                if x <> zero then
                    AddRowVector( row2, coeffs[ head ],  x );
                    AddRowVector( row,  vectors[ head ], x );
                fi;
            fi;
        od;

        j := PositionNonZero( row );
        if j <= ncols then

            # We found a new basis vector.
            x := Inverse( row[j] );
            if x = fail then
              TryNextMethod();
            fi;
            Add( coeffs,  row2 * x );
            Add( vectors, row  * x );
            heads[j]:= Length( vectors );

        else
            Add( relations, row2 );
        fi;

    od;

    return rec( heads     := heads,
                vectors   := vectors,
                coeffs    := coeffs,
                relations := relations );
end );

InstallMethod( SemiEchelonMatTransformation,
    "generic method for matrices",
    [ IsMatrixObj ],
    function( mat )
    local copy;
    
    copy := MutableCopyMat(mat);

    return SemiEchelonMatTransformationDestructive( copy );
end);


InstallGlobalFunction(PedestrianLambdaInverse
        , function(f)
    local z, e, i, ech, info, zh;
    
    Error("wrong");
    
end);

InstallMethod( IsGeneratorsOfMagmaWithInverses,
        "for lists of MatrixObj",
        [ IsHomogeneousList and IsRingElementCollCollColl ],
        function(l)
    if Length(l) > 0 then
        if IsMatrixObj(l[1]) then
            if ForAll(l, x -> x^(-1) <> fail) then
                return true;
            fi;
        fi;
    fi;
    
    TryNextMethod();
            
    end);

InstallMethod( DefaultScalarDomainOfMatrixList,
        [ IsHomogeneousList and IsRingElementCollCollColl ],
        15,
        function(l)
     if Length(l) > 0 then
        if IsMatrixObj(l[1]) then
            return BaseDomain(l[1]);
        fi;
    fi;
    
    TryNextMethod();
end);

