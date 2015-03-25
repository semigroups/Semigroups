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

############################################################################
## Creating a matrix
#############################################################################
#
# Create a new matrix. We will actually check whether all entries are in the
# correct domain, i.e. that the field over which we operate is chosen
# big enough, and that all entries are given inside the field. This will
# prevent us and users from creating stupid matrix objects.

#T Check correct format and correctness of entries
InstallMethod(NewSMatrix, "for IsPlistSMatrixRep, a ring, an int, and a list",
[IsPlistSMatrixRep, IsRing, IsInt, IsList],
function(filter, basedomain, rl, l)
  local m,i,e,filter2;
  filter2 := filter and IsSMatrix;
  if HasCanEasilyCompareElements(Representative(basedomain))
     and CanEasilyCompareElements(Representative(basedomain)) then
    filter2 := filter2 and CanEasilyCompareElements;
  fi;
  m := rec( mat := l );
  Objectify( PlistSMatrixType, m );

  SetDegreeOfSMatrix(m, rl);
  SetBaseDomain(m, basedomain); 

  return m;
end);

InstallMethod(ConstructingFilter, "for a plist s-matrix",
[IsPlistSMatrixRep], m->IsPlistSMatrixRep);

InstallMethod(ConstructingFilter, "for a cvec s-matrix",
[IsCVECSMatrixRep], m->IsCVECSMatrixRep);

############################################################################
## Printing and viewing methods:
#############################################################################
InstallMethod(ViewObj, "for a plist s-matrix",
[IsPlistSMatrixRep],
function(m)
  Print("<s-matrix of degree ");
  Print(DegreeOfSMatrix(m),
         " over ", BaseDomain(m),">");
end );

InstallMethod(PrintObj, "for a plist s-matrix",
[IsPlistSMatrixRep],
function(m)
  Print("NewSMatrix(IsPlistSMatrixRep",BaseDomain(m),
    DegreeOfSMatrix(m),",",m!.mat,")");
end);

InstallMethod(Display, "for a plist s-matrix",
[IsPlistSMatrixRep],
function(m)
  local i;
  Print("<s-matrix of degree ", DegreeOfSMatrix(m), "\n");
  Print(m!.mat);
  Print(">\n");
end);

InstallMethod(String, "for a plist s-matrix",
[IsPlistSMatrixRep ],
function( m )
  local st;
  st := "NewSMatrix(IsPlistSMatrixRep,";
  Append(st,String(BaseDomain(m)));
  Append(st,",");
  Append(st,String(DegreeOfSMatrix(m)));
  Append(st,",");
  Append(st,String(m!.mat));
  Append(st,")");
  return st;
end);

InstallMethod(AsMatrix, "for a plist s-matrix",
[IsSMatrix and IsPlistSMatrixRep],
x -> x!.mat);

InstallMethod(RowSpaceBasis, "for a plist s-matrix",
[IsSMatrix and IsPlistSMatrixRep],
function(m)
   ComputeRowSpaceAndTransformation(m);
   return RowSpaceBasis(m);
end);


############################################################################
## Helper functions to deal with s-matrices.
#############################################################################

InstallGlobalFunction(ComputeRowSpaceAndTransformation,
function(m)
  local deg, rsp, i, zv, bas;

  if not IsPlistSMatrixRep(m) then
    Error("semigroups: Matrix not in the correct representation");
  fi;
  Info(InfoMatrixSemigroups, 2, "ComputeRowSpaceAndTransformation called");

  deg := DegreeOfSMatrix(m);

  rsp := StructuralCopy(m!.mat);
  zv := [1..deg] * Zero(BaseDomain(m));
  for i in [1 .. deg] do
    Append(rsp[i], zv);
    rsp[i][deg + i] := One(BaseDomain(m));
  od;
  TriangulizeMat(rsp);

  # This is dangerous, we are using positions in
  # SMatrix which are undocumented
  # This can be done more efficiently by determining the rank
  # above and then just extracting the basis
  bas := rsp{ [1..deg] }{ [1..deg] };
  for i in [deg, deg - 1 .. 1] do
    if IsZero(bas[i]) then
      Remove(bas, i);
    fi;
  od;
  SetRowSpaceBasis(m, bas);
  SetRowRank(m, Length(bas));
  SetRowSpaceTransformation(m, rsp{[1 .. deg]}{[deg + 1 .. 2 * deg] }); 
  SetRowSpaceTransformationInv(m, RowSpaceTransformation(m)^(-1));
end);


#InstallMethod( TriangulizeMat,
#"for a mutable matrix obj",
#[ IsSMatrix and IsSPlistMatrixRep ],
#function ( mat )
#  local d, m, n, i, j, k, row, zero, x, row2;
#
#  Info( InfoMatrix, 1, "TriangulizeMat called" );
#
#  d := DimensionsMat(mat);
#  m := d[1];
#  n := d[2];
#
#  if not (m = 0 or n = 0) then
#    # get the size of the matrix
#    zero := Zero( BaseDomain(mat) );
#
#    # make sure that the rows are mutable
#    # for i in [ 1 .. m ] do
#    #   if not IsMutable( mat[i] ) then
#    #     mat[i]:= ShallowCopy( mat[i] );
#    #   fi;
#    # od;
#
#    # run through all columns of the matrix
#    i := 0;
#    for k  in [1 .. n]  do
#      # find a nonzero entry in this column
#      j := i + 1;
#      while j <= m and mat[j][k] = zero  do
#        j := j + 1;
#      od;
#
#      # if there is a nonzero entry
#      if j <= m  then
#
#        # increment the rank
#        Info( InfoMatrix, 2, "  nonzero columns: ", k );
#        i := i + 1;
#
#        # make its row the current row and normalize it
#        row    := mat[j];
#        mat[j] := mat[i];
#        x := Inverse( row[k] );
#        if x = fail then
#          TryNextMethod();
#        fi;
#        MultRowVector( row, x );
#        mat[i] := row;
#
#        # clear all entries in this column
#        for j  in [1 .. i - 1] do
#          row2 := mat[j];
#          x := row2[k];
#          if   x <> zero  then
#            AddRowVector( row2, row, - x );
#          fi;
#        od;
#        for j  in [i + 1 .. m] do
#          row2 := mat[j];
#          x := row2[k];
#          if   x <> zero  then
#            AddRowVector( row2, row, - x );
#          fi;
#        od;
#      fi;
#    od;
#  fi;
#
#  Info( InfoMatrix, 1, "TriangulizeMat returns" );
#end );
#
##############################################################################
###
##M  SemiEchelonMat( <mat> )
###
#InstallMethod( SemiEchelonMatDestructive,
#    "generic method for matrix objects",
#    [ IsSMatrix and IsMutable and IsSPlistMatrixRep ],
#    function( mat )
#    local zero,      # zero of the field of <mat>
#          dims,      # dims of matrix entries
#          nrows,     # number of rows in <mat>
#          ncols,     # number of columns in <mat>
#          vectors,   # list of basis vectors
#          heads,     # list of pivot positions in `vectors'
#          i,         # loop over rows
#          j,         # loop over columns
#          x,         # a current element
#          nzheads,   # list of non-zero heads
#          row,       # the row of current interest
#          inv;       # inverse of a matrix entry
#
#    dims := DimensionsMat(mat);
#
#    nrows := dims[1];
#    ncols := dims[2];
#
#    zero := Zero(BaseDomain(mat));
#
#    heads := ListWithIdenticalEntries( ncols, 0 );
#    nzheads := [];
#    vectors := [];
#
#    for i in [ 1 .. nrows ] do
#
#        row := mat[i];
#        # Reduce the row with the known basis vectors.
#        for j in [ 1 .. Length(nzheads) ] do
#            x := row[nzheads[j]];
#            if x <> zero then
#              AddRowVector( row, vectors[ j ], - x );
#            fi;
#        od;
#
#        j := PositionNonZero( row );
#
#        if j <= ncols then
#
#            # We found a new basis vector.
#            inv := Inverse( row[j] );
#            if inv = fail then
#                Error("Semigroups: SemiEchelonMatDestructive:\n",
#                      "fail");
#                return;
#            fi;
#            MultRowVector( row, inv );
#            Add( vectors, row );
#            Add( nzheads, j );
#            heads[j] := Length( vectors );
#
#        fi;
#
#    od;
#
#    return rec( heads   := heads,
#                vectors := vectors );
#end );
#
#InstallMethod( SemiEchelonMatTransformationDestructive,
#    "generic method for matrices",
#    [ IsSMatrix and IsMutable and IsSPlistMatrixRep ],
#    function( mat )
#    local zero,      # zero of the field of <mat>
#          nrows,     # number of rows in <mat>
#          ncols,     # number of columns in <mat>
#          vectors,   # list of basis vectors
#          heads,     # list of pivot positions in 'vectors'
#          i,         # loop over rows
#          j,         # loop over columns
#          T,         # transformation matrix
#          coeffs,    # list of coefficient vectors for 'vectors'
#          relations, # basis vectors of the null space of 'mat'
#          row, head, x, row2,f,dims;
#
#    dims := DimensionsMat(mat);
#
#    nrows := dims[1];
#    ncols := dims[2];
#
#    f := BaseDomain(mat);
#    zero := Zero(f);
#
#    heads   := ListWithIdenticalEntries( ncols, 0 );
#    vectors := [];
#
#    T         := IdentityMatrix( nrows, mat );
#    coeffs    := [];
#    relations := [];
#
#    for i in [ 1 .. nrows ] do
#
#        row := mat[i];
#        row2 := T[i];
#
#        # Reduce the row with the known basis vectors.
#        for j in [ 1 .. ncols ] do
#            head := heads[j];
#            if head <> 0 then
#                x := - row[j];
#                if x <> zero then
#                    AddRowVector( row2, coeffs[ head ],  x );
#                    AddRowVector( row,  vectors[ head ], x );
#                fi;
#            fi;
#        od;
#
#        j := PositionNonZero( row );
#        if j <= ncols then
#
#            # We found a new basis vector.
#            x := Inverse( row[j] );
#            if x = fail then
#              TryNextMethod();
#            fi;
#            Add( coeffs,  row2 * x );
#            Add( vectors, row  * x );
#            heads[j] := Length( vectors );
#
#        else
#            Add( relations, row2 );
#        fi;
#
#    od;
#
#    return rec( heads     := heads,
#                vectors   := vectors,
#                coeffs    := coeffs,
#                relations := relations );
#end );
#
#InstallMethod( SemiEchelonMatTransformation,
#    "generic method for matrices",
#    [ IsSMatrix and IsSPlistMatrixRep ],
#    function( mat )
#    local copy;
#
#    copy := MutableCopyMat(mat);
#
#    return SemiEchelonMatTransformationDestructive( copy );
#end);
#
##############################################################################
###
##F  RandomSMatrix( <m>, <n> [, <R>] ) . . . . . . . .  make a random matrix
###
###  'RandomSMatrix' returns a random semigroups matrix object
###  in IsSMatrixPlistRep with <m> rows and <n> columns with elements taken 
###  from the ring <R>, which defaults to 'Integers'.
###
##W  This returns a matrix in IsSPlistMatrixRep
##T  this function should take either a filter or a sample matrix
#InstallGlobalFunction( RandomSMatrix, function ( arg )
#    local   mat, m, n, R, i, row, k;
#
#    # check the arguments and get the list of elements
#    if Length(arg) = 2  then
#        m := arg[1];
#        n := arg[2];
#        R := Integers;
#    elif Length(arg) = 3  then
#        m := arg[1];
#        n := arg[2];
#        R := arg[3];
#    else
#        Error("Semigroups: RandomMatrixObj: usage\n",
#        "RandomMat( <m>, <n> [, <F>] )");
#    fi;
#
#    # now construct the random matrix
#    mat := [];
#    for i  in [1 .. m]  do
#        row := [];
#        for k  in [1 .. n]  do
#            row[k] := Random( R );
#        od;
#        mat[i] := row;
#    od;
#
#    return NewMatrix(IsSPlistMatrixRep, R, n, One(R) * mat);
#end );
#

InstallMethod(IdentitySMatrix, "for a finite field and pos int",
[IsField and IsFinite, IsPosInt], 
function(R, n)
  return AsSMatrix(IdentityMatrix(n, R));
end);

InstallMethod(InverseOp, "for an s-matrix", 
[IsSMatrix], 
function(smat)
  local mat;
  mat := Inverse(smat!.mat);
  if mat = fail then 
    return fail;
  fi;
  return AsSMatrix(smat, mat);
end);

InstallMethod(AsSMatrix, "for an s-matrix and a matrix", 
[IsSMatrix, IsMatrix],
function(smat, mat)
  return NewSMatrix(ConstructingFilter(smat), BaseDomain(smat),
                    DegreeOfSMatrix(smat), mat);
end);

InstallMethod(DegreeOfSMatrixCollection, "for an s-matrix collection",
[IsSMatrixCollection],
function(coll)
  local deg;

  deg := DegreeOfSMatrix(coll[1]);
  if not ForAll(coll, x -> DegreeOfSMatrix(x) = deg) then
    Error("Semigroups: DegreeOfSMatrixCollection: usage,\n",
          "the argument <coll> must be a collection of s-matrices of ",
          "equal degree,");
    return;
  fi;

  return deg;
end);

InstallMethod(BaseDomain, "for an s-matrix collection",
[IsSMatrixCollection],
function(coll)
  local base;
  base := BaseDomain(coll[1]);
  if not ForAll(coll, x -> BaseDomain(x) = base) then
    Error("Semigroups: DegreeOfSMatrixCollection: usage,\n",
          "the argument <coll> must be a collection of s-matrices of ",
          "with the same base domain,");
    return;
  fi;

  return base;
end);

InstallMethod(OneMutable, "for an s-matrix", [IsSMatrix], 
x-> AsSMatrix(x, One(x!.mat)));

InstallMethod(\=, "for an s-matrix", [IsSMatrix, IsSMatrix], 
function(x, y)
  return BaseDomain(x) = BaseDomain(y) and x!.mat = y!.mat;
end);

InstallMethod(\<, "for an s-matrix", [IsSMatrix, IsSMatrix], 
function(x, y)
  return DegreeOfSMatrix(x) < DegreeOfSMatrix(y) 
    or (DegreeOfSMatrix(x) = DegreeOfSMatrix(y) 
        and BaseDomain(x) < BaseDomain(y)) 
    or (DegreeOfSMatrix(x) = DegreeOfSMatrix(y) 
        and BaseDomain(x) = BaseDomain(y) and x!.mat < y!.mat);
end);
