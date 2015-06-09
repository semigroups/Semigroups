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

InstallMethod(NewSVector, "for IsPlistSVectorRep, a ring, an int, and a list",
[IsPlistSVectorRep, IsRing, IsInt, IsList],
function(filter, basedomain, rl, l)
  local v,i,e,filter2;

  if not Length(l) = rl then
    Error("Semigroups: NewSVector: usage,\n",
          "the arguments are wrong!");
    return;
  fi;

  filter2 := filter and IsSVector;
  if HasCanEasilyCompareElements(Representative(basedomain))
     and CanEasilyCompareElements(Representative(basedomain)) then
    filter2 := filter2 and CanEasilyCompareElements;
  fi;
  v := rec( vec := StructuralCopy(l) );
  Objectify( PlistSVectorType, v );
  MakeImmutable(v!.vec);

  SetDegreeOfSVector(v, rl);
  SetBaseDomain(v, basedomain); 

  return v;
end);

#T Establish the usefulness of this method
#InstallMethod(NewSVector, "for IsPlistSVectorRep, a ring, an int, and a plist vector",
#[IsPlistSVectorRep, IsRing, IsInt, IsPlistVectorRep],
#function(filter, basedomain, rl, l)
#  return NewSVector(filter, basedomain, rl, AsVector(l));
#end);

InstallMethod(NewZeroSVector, "for IsPlistSVectorRep, a ring, and an int",
[IsPlistSVectorRep, IsRing, IsInt],
function(filter, basedomain, rl)
  return NewSVector(filter, basedomain, rl, [1..rl] * Zero(basedomain));
end);

InstallMethod(ViewObj, "for a plist s-vector",
[IsPlistSVectorRep],
function(v)
  Print("<s-vector of degree ");
  Print(DegreeOfSVector(v),
         " over ", BaseDomain(v),">");
end);

InstallMethod(ViewString, "for a plist s-vector",
[IsPlistSVectorRep],
function(v)
  return STRINGIFY("<s-vector of degree ",
                   DegreeOfSVector(v),
                   " over ",
                   BaseDomain(v)); 
end);

InstallMethod(PrintObj, "for a plist s-vector",
[IsPlistSVectorRep],
function(v)
  Print("NewSVector(IsPlistSVectorRep, ",
        BaseDomain(v), ", ",
        DegreeOfSVector(v), ", ",
        v!.vec, ")");
end);

InstallMethod(Display, "for a plist s-vector",
[IsPlistSVectorRep],
function(v)
  local i;
  Print("<s-vector of degree ",
        DegreeOfSVector(v), "\n");
  Print(v!.vec);
  Print(">\n");
end);

InstallMethod(PrintString, "for a plist s-vector",
[IsPlistSVectorRep],
function( v )
  local st;
  st := "NewSMatrix(IsPlistSVectorRep,";
  Append(st,String(BaseDomain(v)));
  Append(st,",");
  Append(st,String(DegreeOfSVector(v)));
  Append(st,",");
  Append(st,String(v!.vec));
  Append(st,")");
  return st;
end);

#############################################################################
##
## SRowBasis
##
#############################################################################
#
InstallMethod(NewSRowBasis, "for IsPlistListSRowBasisRep, a ring, and a list",
[IsPlistListSRowBasisRep, IsRing, IsList],
function(filter, basedomain, l)
  local b,i,e,filter2;
  
  filter2 := filter and IsSRowBasis;
  if HasCanEasilyCompareElements(Representative(basedomain))
     and CanEasilyCompareElements(Representative(basedomain)) then
    filter2 := filter2 and CanEasilyCompareElements;
  fi;
  b := rec( rows := l );
  Objectify( PlistListSRowBasisType, b );

  SetBaseDomain(b, basedomain); 

  return b;
end);

InstallMethod(Rank, "for a plist s-rowbasis",
[IsPlistListSRowBasisRep],
function(v)
  return Length(v!.rows);
end);

InstallMethod(ViewObj, "for a plist s-rowbasis",
[IsPlistListSRowBasisRep],
function(v)
  Print("<s-rowbasis of rank ");
  Print(Length(v!.rows),
         " over ", BaseDomain(v),">");
end);

InstallMethod(ViewString, "for a plist s-rowbasis",
[IsPlistListSRowBasisRep],
function(v)
  return STRINGIFY("<s-rowbasis of rank ",
                   Rank(v),
                   " over ",
                   BaseDomain(v)); 
end);

InstallMethod(PrintObj, "for a plist s-rowbasis",
[IsPlistListSRowBasisRep],
function(v)
  Print("NewSRowBasis(IsPlistListSRowBasisRep, ",
        BaseDomain(v), ", ",
        v!.rows, ")");
end);

InstallMethod(Display, "for a plist s-rowbasis",
[IsPlistListSRowBasisRep],
function(v)
  local i;
  Print("<s-rowbasis of rank ",
        Rank(v), "\n");
  Print(v!.rows);
  Print(">\n");
end);

InstallMethod(PrintString, "for a plist s-rowbasis",
[IsPlistListSRowBasisRep],
function(v)
  local st;
  st := "NewSRowBasis(IsPlistListSRowBasisRep,";
  Append(st,String(BaseDomain(v)));
  Append(st,",");
  Append(st,String(Rank(v)));
  Append(st,",");
  Append(st,String(v!.rows));
  Append(st,")");
  return st;
end);

#############################################################################
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
  
  if not Length(l) = rl then 
    Error("Semigroups: NewSMatrix: usage,\n",
          "the arguments are wrong!");
    return;
  fi;

  filter2 := filter and IsSMatrix;
  if HasCanEasilyCompareElements(Representative(basedomain))
     and CanEasilyCompareElements(Representative(basedomain)) then
    filter2 := filter2 and CanEasilyCompareElements;
  fi;
  m := rec( mat := ImmutableMatrix(basedomain, l));
  Objectify( PlistSMatrixType, m );

  SetDegreeOfSMatrix(m, rl);
  SetBaseDomain(m, basedomain); 

  return m;
end);

InstallMethod(NewSMatrix, 
"for IsPlistSMatrixRep, a ring, an int, and IsPlistMatrixRep",
[IsPlistSMatrixRep, IsRing, IsInt, IsPlistMatrixRep],
function(filter, basedomain, rl, mat)
  return NewSMatrix(filter, basedomain, rl, AsMatrix(mat));
end);

InstallMethod(NewIdentitySMatrix,
"for IsPlistSMatrixRep, a ring, and an int",
[IsPlistSMatrixRep, IsRing, IsPosInt],
function(filter, basedomain, deg)
  return NewSMatrix(filter, basedomain, deg,
                    IdentityMat(deg, basedomain));
end);

InstallMethod(NewIdentitySMatrix,
"for IsPlistSMatrixRep, a ring, and zero",
[IsPlistSMatrixRep, IsRing, IsZeroCyc],
function(filter, basedomain, deg)
  local m;
  m := NewSMatrix(filter, basedomain, deg,
                  IdentityMat(deg, basedomain));
  SetRowSpaceBasis(m, NewSRowBasis(IsPlistListSRowBasisRep,
                                   basedomain, []));
  SetRowRank(m, 0);
  SetRowSpaceTransformation(m, m);
  SetRowSpaceTransformationInv(m, m);
  SetSemigroupInverse(m, m);
  return m;
end);

InstallMethod(NewZeroSMatrix,
"for IsPlistSMatrixRep, a ring, and zero",
[IsPlistSMatrixRep, IsRing, IsZeroCyc],
function(filter, basedomain, deg)
  return NewSMatrix(filter, basedomain, deg,
                    IdentityMat(deg, basedomain));
end);

InstallMethod(NewZeroSMatrix,
"for IsPlistSMatrixRep, a ring, and an int",
[IsPlistSMatrixRep, IsRing, IsPosInt],
function(filter, basedomain, deg)
  return NewSMatrix(filter, basedomain, deg,
                    NullMat(deg, deg, basedomain));
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
end);

InstallMethod(ViewString, "for a plist s-matrix",
[IsPlistSMatrixRep],
function(m)
  return STRINGIFY("<s-matrix of degree ",
                   DegreeOfSMatrix(m),
                   " over ",
                   BaseDomain(m)); 
end);

InstallMethod(PrintObj, "for a plist s-matrix",
[IsPlistSMatrixRep],
function(m)
  Print("NewSMatrix(IsPlistSMatrixRep, ",BaseDomain(m),", ",
    DegreeOfSMatrix(m), ", ", m!.mat, ")");
end);

InstallMethod(Display, "for a plist s-matrix",
[IsPlistSMatrixRep],
function(m)
  local i;
  Print("<s-matrix of degree ", DegreeOfSMatrix(m), "\n");
  Print(m!.mat);
  Print(">\n");
end);

InstallMethod(PrintString, "for a plist s-matrix",
[IsPlistSMatrixRep],
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

#T known information can be copied!
InstallMethod(TransposedMatImmutable, "for a plist s-matrix",
[IsSMatrix and IsPlistSMatrixRep],
function(m)
  local n;
  n := AsSMatrix(m, TransposedMat(m!.mat));
  return n;
end);

InstallMethod(AsMatrix, "for a matrix obj plist matrix rep",
[IsPlistMatrixRep], x-> List(x![ROWSPOS], List));

InstallMethod(AsMatrix, "for a plist s-matrix",
[IsPlistSMatrixRep], x -> x!.mat);

InstallMethod(RowSpaceBasis, "for a plist s-matrix",
[IsSMatrix and IsPlistSMatrixRep],
function(m)
   ComputeRowSpaceAndTransformation(m);
   return RowSpaceBasis(m);
end);

InstallMethod(RowRank, "for a plist s-matrix",
[IsSMatrix and IsPlistSMatrixRep],
function(m)
  ComputeRowSpaceAndTransformation(m);
  return RowRank(m);
end);

#T Should this go in a helper function, it also works
#T similarly to the thing done below. 
InstallMethod(RightInverse, "for a plist s-matrix",
[IsSMatrix and IsPlistSMatrixRep],
function(m)
  local deg, rsp, zv, se, u, i, j, k;

  deg := DegreeOfSMatrix(m);
  u := One(BaseDomain(m));

  rsp := SEMIGROUPS_MutableCopyMat(m!.mat);
  zv := [1..deg] * Zero(BaseDomain(m));
  for i in [1 .. deg] do
    Append(rsp[i], zv);
    rsp[i][deg + i] := u;
  od;
  se := SemiEchelonMat(rsp);

  for i in [1 .. Length(se.vectors)] do
    rsp[i] := ShallowCopy(se.vectors[i]);
  od;
  for i in [1 .. deg] do
    if se.heads[i] = 0 then
      rsp[i][i] := u;
      rsp[i][deg + i] := Zero(BaseDomain(m));
    fi;
  od;
  TriangulizeMat(rsp);

  return AsSMatrix(m, rsp{[1..deg]}{[deg + 1 .. 2 * deg]});
end);

InstallMethod(LeftInverse, "for a plist s-matrix",
[IsSMatrix and IsPlistSMatrixRep],
function(m)
  return TransposedMat(RightInverse(TransposedMat(m)));
end);

#T Trying to use "Inverse" in the semigroup sense
#T here leads to problems with other operations, 
#T so we have to be very careful
InstallMethod(InverseOp, "for a plist s-matrix",
[IsSMatrix and IsPlistSMatrixRep],
function(m)
  local x;
  
  if DegreeOfSMatrix(m) = 0 then
    return m;
  else
    x := m!.mat^(-1);

    if x = fail then
      return fail;
    else 
      return AsSMatrix(m,x);
    fi;
  fi;
end);

############################################################################
## Helper functions to deal with s-matrices.
#############################################################################
InstallGlobalFunction(ComputeRowSpaceAndTransformation,
function(m)
  local deg, rsp, i, j, zv, bas, heads, tm, inv, sinv, tr, tri, bd;

  if not IsPlistSMatrixRep(m) then
    Error("semigroups: Matrix not in the correct representation");
  fi;
  Info(InfoMatrixSemigroups, 2, "ComputeRowSpaceAndTransformation called");

  deg := DegreeOfSMatrix(m);
  bd := BaseDomain(m);
  if IsZero(m) then
    bas := []; 
    tr := NewIdentitySMatrix(IsPlistSMatrixRep, bd, deg);
    tri := tr;
    if deg = 0 then
       inv := tr;
       sinv := tr;
    else
       inv := fail;
       sinv := fail;
    fi;
  else
    rsp := SEMIGROUPS_MutableCopyMat(m!.mat);
    zv := [1..deg] * Zero(bd);
    for i in [1 .. deg] do
      Append(rsp[i], ShallowCopy(zv));
      rsp[i][deg + i] := One(bd);
    od;
    TriangulizeMat(rsp);

    heads := [];
    bas := rsp{ [1..deg] }{ [1..deg] };
    for i in [deg, deg - 1 .. 1] do
      if IsZero(bas[i]) then
        Remove(bas, i);
      else
        heads[PositionNonZero(bas[i])] := i;
      fi;
    od;
    # Check whether this matrix has a semigroup inverse, i.e.
    # a matrix t such that t * m * t = t and m * t * m = m.
    # If it does this matrix is the transformation we computed
    # otherwise we set fail 
    tm := TransposedMat(bas);
    sinv := true;
    for i in [1..deg] do
      if not IsBound(heads[i]) then
        if not IsZero(tm[i]) then
          sinv := fail;
        fi;
      fi;
    od;
    #T This is obviously totally ridiculous to do the same computation
    #T twice
    if sinv = true then
       sinv := RightInverse(m);
    fi;
    if Length(bas) = deg then
       inv := sinv;
    else
       inv := fail;
    fi;
    tr := rsp{[1 .. deg]}{[deg + 1 .. 2 * deg]};
    tri := tr^(-1);
  fi;
 
  ConvertToVectorRep(bas);
  MakeImmutable(bas);
  bas := NewSRowBasis(IsPlistListSRowBasisRep, bd, bas);
  SetRowSpaceBasis(m, bas);
  SetRowRank(m, Rank(bas));
  SetRowSpaceTransformation(m, tr); 
  SetRowSpaceTransformationInv(m, tri);
  SetSemigroupInverse(m, sinv);
  SetInverse(m, inv);
end);

#############################################################################
##
#F  RandomSMatrix( <m>, <n> [, <R>] ) . . . . . . . .  make a random matrix
##
##  'RandomSMatrix' returns a random semigroups matrix object
##  in IsSMatrixPlistRep with <m> rows and <n> columns with elements taken 
##  from the ring <R>, which defaults to 'Integers'.
##
#W  This returns a matrix in IsSPlistMatrixRep
#T  this function should take either a filter or a sample matrix
##
InstallGlobalFunction( RandomSMatrix, function ( arg )
  local   mat, m, n, R, rks, i, row, k;

  # check the arguments and get the list of elements
  if Length(arg) = 2  then
    m := arg[1];
    n := arg[2];
    R := Integers;
  elif Length(arg) = 3  then
    m := arg[1];
    n := arg[2];
    R := arg[3];
  else
    Error("Semigroups: RandomMatrixObj: usage\n",
      "RandomMat( <m>, <n> [, <F>] )");
  fi;

  # now construct the random matrix
  mat := [];
  for i  in [1 .. m]  do
    row := [];
    for k  in [1 .. n]  do
      row[k] := Random( R );
    od;
    mat[i] := row;
  od;

  return NewSMatrix(IsPlistSMatrixRep, R, n, One(R) * mat);
end);

InstallGlobalFunction(RandomSquareSMatrixWithRanks,
function(R, n, ranks)
  local i, j, k, rk, z, zv, mat, conj, gens;

  if ForAny(ranks, x -> (x<0) or (x>n)) then
    Error("Semigroups: RandomSquareSMatrixWithRank usage: the list of ranks ",
          "has to consist of numbers >0 and <n.");
  fi;

  gens := [];
  z := Zero(R);
  # Choose a matrix of given rank
  rk := Random(ranks);
  if rk = 0 then
    return NewZeroSMatrix(IsPlistMatrixRep, R, n);
  else
    mat := SEMIGROUPS_MutableCopyMat(Random(GL(rk, R)));
    # Extend it to n x n
    zv := [1..n-rk] * z;
    for j in [1..rk] do
      Append(mat[j], zv); 
    od;
    zv := [1..n] * z;
    for j in [1..n-rk] do
      Add(mat, zv);
    od;
    # Swirl around
    #T Is Permuting rows/columns enough?
    conj := Random(GL(n, R)); # PermutationMat(Random(Sym(n)), n, R);
    return NewSMatrix(IsPlistSMatrixRep, R, n, mat ^ conj);
  fi; 
end);

InstallGlobalFunction(RandomListOfMatricesWithRanks,
function(R,m,n,ranks)
  local i, j, k, rk, z, zv, mat, conj, gens;

  if ForAny(ranks, x -> (x<0) or (x>n)) then
    Error("Semigroups: RandomListOfMatricesWithRank usage: the list of ranks ",
          "has to consist of numbers >0 and <n.");
  fi;

  return List([1..m], x->RandomSquareSMatrixWithRanks(R,n,ranks));
end);

#T This will break transparency wrt representations, so we should 
#T really not be doing this and instead use a sample object
#T or we should be using NewIdentitySMatrix
InstallMethod(IdentitySMatrix, "for a finite field and zero",
[IsField and IsFinite, IsZeroCyc ],
function(R, n)
  return NewIdentitySMatrix(IsPlistSMatrixRep, R, n);
end);

InstallMethod(IdentitySMatrix, "for a finite field and pos int",
[IsField and IsFinite, IsPosInt], 
function(R, n)
  return NewIdentitySMatrix(IsPlistSMatrixRep, R, n);
end);

InstallMethod(IdentitySMatrix, "for an s-matrix and zero",
[IsSMatrix, IsZeroCyc], 
function(smat, n)
  return NewIdentitySMatrix(ConstructingFilter(smat), BaseDomain(smat),
                    n);
end);

InstallMethod(IdentitySMatrix, "for an s-matrix and pos int",
[IsSMatrix, IsPosInt], 
function(smat, n)
  return NewIdentitySMatrix(ConstructingFilter(smat), BaseDomain(smat),
                    n);
end);

#InstallMethod(InverseOp, "for an s-matrix", 
#[IsSMatrix], 
#function(smat)
#  local mat;
#  mat := Inverse(smat!.mat);
#  if mat = fail then 
#    return fail;
#  fi;
#  return AsSMatrix(smat, mat);
#end);

InstallMethod(AsSMatrix, "for an s-matrix and a matrix", 
[IsSMatrix, IsMatrix],
function(smat, mat)
  return NewSMatrix(ConstructingFilter(smat), BaseDomain(smat),
                    Length(mat), mat);
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

InstallMethod(IsZero, "for an s-matrix",
[IsSMatrix],
x -> IsZero(x!.mat));

InstallMethod(OneMutable, "for an s-matrix",
[IsSMatrix], 
x -> IdentitySMatrix(x, DegreeOfSMatrix(x)));

InstallMethod(\=, "for an s-matrix",
[IsSMatrix, IsSMatrix], 
function(x, y)
  return BaseDomain(x) = BaseDomain(y) and x!.mat = y!.mat;
end);

InstallMethod(\<, "for an s-matrix",
[IsSMatrix, IsSMatrix], 
function(x, y)
  return DegreeOfSMatrix(x) < DegreeOfSMatrix(y) 
    or (DegreeOfSMatrix(x) = DegreeOfSMatrix(y) 
        and BaseDomain(x) < BaseDomain(y)) 
    or (DegreeOfSMatrix(x) = DegreeOfSMatrix(y) 
        and BaseDomain(x) = BaseDomain(y) and x!.mat < y!.mat);
end);

InstallMethod(\*, "for s-matrices", [IsSMatrix, IsSMatrix], 
function(x, y)
  if DegreeOfSMatrix(x) <> DegreeOfSMatrix(y) 
      or BaseDomain(x) <> BaseDomain(y) then 
    Error("\* for s-matrices degree or domain mismatch");
    return;
  fi;

  return AsSMatrix(x, x!.mat * y!.mat);
end);

#T This might call for a separate SVector implementaion actually
#T At least check lengths
InstallOtherMethod(\*, "for an empty list and an s-matrix",
[IsList and IsEmpty, IsSMatrix],
function(l,m)
  return l;
end);

InstallMethod(\*, "for a list of vectors and an s-matrix",
[IsFFECollection, IsSMatrix],
function(l, m)
  return l * m!.mat;
end);

InstallMethod(\*, "for a list of vectors and an s-matrix",
[IsFFECollColl, IsSMatrix],
function(l, m)
  return l * m!.mat;
end);

InstallMethod(TransposedSMat, "for an s-matrix",
[IsSMatrix],
function(m)
  if DegreeOfSMatrix(m) = 0 then
    return m;
  else
    return AsSMatrix(m, TransposedMat(m!.mat));
  fi;
end);

InstallGlobalFunction(SEMIGROUPS_MutableCopyMat,
function(m)
  local res, r;

  res := [];
  for r in m do
    Add(res, ShallowCopy(r));
  od;
  return res;
end);

InstallGlobalFunction(SEMIGROUPS_CheckReallyZero,
function(m)
  local r,e;
  for r in m!.mat do
    for e in r do
      if not IsZero(e) then
        return false;
      fi;
    od;
  od;
  return true;
end);
