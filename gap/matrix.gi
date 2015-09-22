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

InstallMethod(NewVectorOverFiniteField,
"for IsPlistVectorOverFiniteFieldRep, a ring, an int, and a list",
[IsPlistVectorOverFiniteFieldRep, IsRing, IsInt, IsList],
function(filter, basedomain, rl, l)
  local v, filter2;

  if not Length(l) = rl then
    Error("Semigroups: NewVectorOverFiniteField: usage,\n",
          "the arguments are wrong,");
    return;
  fi;

  filter2 := filter and IsVectorOverFiniteField;
  if HasCanEasilyCompareElements(Representative(basedomain))
      and CanEasilyCompareElements(Representative(basedomain)) then
    filter2 := filter2 and CanEasilyCompareElements;
  fi;
  v := rec(vec := StructuralCopy(l));
  Objectify(PlistVectorOverFiniteFieldType, v);
  MakeImmutable(v!.vec);

  SetDegreeOfVectorOverFiniteField(v, rl);
  SetBaseDomain(v, basedomain);

  return v;
end);

#T Establish the usefulness of this method
#InstallMethod(NewVectorOverFiniteField, "for IsPlistVectorOverFiniteFieldRep,
#a ring, an int, and a plist vector",
#[IsPlistVectorOverFiniteFieldRep, IsRing, IsInt, IsPlistVectorRep],
#function(filter, basedomain, rl, l)
#  return NewVectorOverFiniteField(filter, basedomain, rl, AsVector(l));
#end);

InstallMethod(NewZeroVectorOverFiniteField,
"for IsPlistVectorOverFiniteFieldRep, a ring, and an int",
[IsPlistVectorOverFiniteFieldRep, IsRing, IsInt],
function(filter, basedomain, rl)
  return NewVectorOverFiniteField(filter, basedomain, rl,
                                  [1 .. rl] * Zero(basedomain));
end);

InstallMethod(ViewObj, "for a plist vector over finite field",
[IsPlistVectorOverFiniteFieldRep],
function(v)
  Print("<vector over finite field of degree ");
  Print(DegreeOfVectorOverFiniteField(v),
        " over ", BaseDomain(v), ">");
end);

InstallMethod(ViewString, "for a plist vector over finite field",
[IsPlistVectorOverFiniteFieldRep],
function(v)
  return STRINGIFY("<vector over finite field of degree ",
                   DegreeOfVectorOverFiniteField(v),
                   " over ",
                   BaseDomain(v));
end);

InstallMethod(PrintObj, "for a plist vector over finite field",
[IsPlistVectorOverFiniteFieldRep],
function(v)
  Print("NewVectorOverFiniteField(IsPlistVectorOverFiniteFieldRep, ",
        BaseDomain(v), ", ",
        DegreeOfVectorOverFiniteField(v), ", ",
        v!.vec, ")");
end);

InstallMethod(Display, "for a plist vector over finite field",
[IsPlistVectorOverFiniteFieldRep],
function(v)
  Print("<vector over ", BaseDomain(v),
        " of degree ", DegreeOfVectorOverFiniteField(v), "\n");
  Print(v!.vec);
  Print(">\n");
end);

InstallMethod(PrintString, "for a plist vector over finite field",
[IsPlistVectorOverFiniteFieldRep],
function(v)
  local st;
  st := "NewMatrixOverFiniteField(IsPlistVectorOverFiniteFieldRep,";
  Append(st, String(BaseDomain(v)));
  Append(st, ",");
  Append(st, String(DegreeOfVectorOverFiniteField(v)));
  Append(st, ",");
  Append(st, String(v!.vec));
  Append(st, ")");
  return st;
end);

#############################################################################
##
## RowBasisOverFiniteField
##
#############################################################################
#
InstallMethod(NewRowBasisOverFiniteField,
"for IsPlistRowBasisOverFiniteFieldRep, a ring, and a list",
[IsPlistRowBasisOverFiniteFieldRep, IsRing, IsList],
function(filter, basedomain, l)
  local b, filter2;

  filter2 := filter and IsRowBasisOverFiniteField;
  if HasCanEasilyCompareElements(Representative(basedomain))
      and CanEasilyCompareElements(Representative(basedomain)) then
    filter2 := filter2 and CanEasilyCompareElements;
  fi;
  b := rec(rows := l);
  Objectify(PlistRowBasisOverFiniteFieldType, b);

  SetBaseDomain(b, basedomain);

  return b;
end);

InstallMethod(Rank, "for a plist rowbasis",
[IsPlistRowBasisOverFiniteFieldRep],
function(v)
  return Length(v!.rows);
end);

InstallMethod(\=, "for an rowbasis",
[IsPlistRowBasisOverFiniteFieldRep, IsPlistRowBasisOverFiniteFieldRep],
function(x, y)
  return BaseDomain(x) = BaseDomain(y) and x!.rows = y!.rows;
end);

InstallMethod(\<, "for an rowbasis",
[IsPlistRowBasisOverFiniteFieldRep, IsPlistRowBasisOverFiniteFieldRep],
function(x, y)
  return Rank(x) < Rank(y)
    or (Rank(x) = Rank(y)
        and (x!.rows < y!.rows));
end);

InstallMethod(ViewObj, "for a plist rowbasis",
[IsPlistRowBasisOverFiniteFieldRep],
function(v)
  Print("<rowbasis of rank ");
  Print(Length(v!.rows), " over ", BaseDomain(v), ">");
end);

InstallMethod(ViewString, "for a plist rowbasis",
[IsPlistRowBasisOverFiniteFieldRep],
function(v)
  return STRINGIFY("<rowbasis of rank ", Rank(v), " over ", BaseDomain(v));
end);

InstallMethod(PrintObj, "for a plist rowbasis",
[IsPlistRowBasisOverFiniteFieldRep],
function(v)
  Print("NewRowBasisOverFiniteField(IsPlistRowBasisOverFiniteFieldRep, ",
        BaseDomain(v), ", ", v!.rows, ")");
end);

InstallMethod(Display, "for a plist rowbasis",
[IsPlistRowBasisOverFiniteFieldRep],
function(v)
  Print("<rowbasis of rank ", Rank(v), "\n");
  Print(v!.rows);
  Print(">\n");
end);

InstallMethod(PrintString, "for a plist rowbasis",
[IsPlistRowBasisOverFiniteFieldRep],
function(v)
  local st;
  st := "NewRowBasisOverFiniteField(IsPlistRowBasisOverFiniteFieldRep,";
  Append(st, String(BaseDomain(v)));
  Append(st, ",");
  Append(st, String(Rank(v)));
  Append(st, ",");
  Append(st, String(v!.rows));
  Append(st, ")");
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
InstallMethod(NewMatrixOverFiniteField,
"for IsPlistMatrixOverFiniteFieldRep, a ring, an int, and a list",
[IsPlistMatrixOverFiniteFieldRep, IsRing, IsInt, IsList],
function(filter, basedomain, rl, l)
  local m, filter2;

  if not Length(l) = rl then
    Error("Semigroups: NewMatrixOverFiniteField: usage,\n",
          "the arguments are wrong!");
    return;
  fi;

  filter2 := filter and IsMatrixOverFiniteField;
  if HasCanEasilyCompareElements(Representative(basedomain))
      and CanEasilyCompareElements(Representative(basedomain)) then
    filter2 := filter2 and CanEasilyCompareElements;
  fi;
  m := rec(mat := ImmutableMatrix(basedomain, l));
  Objectify(PlistMatrixOverFiniteFieldType, m);

  SetDegreeOfMatrixOverFiniteField(m, rl);
  SetBaseDomain(m, basedomain);

  return m;
end);

InstallMethod(NewMatrixOverFiniteField,
"for IsPlistMatrixOverFiniteFieldRep, a ring, an int, and IsPlistMatrixRep",
[IsPlistMatrixOverFiniteFieldRep, IsRing, IsInt, IsPlistMatrixRep],
function(filter, basedomain, rl, mat)
  return NewMatrixOverFiniteField(filter, basedomain, rl, AsMatrix(mat));
end);

InstallMethod(NewIdentityMatrixOverFiniteField,
"for IsPlistMatrixOverFiniteFieldRep, a ring, and an int",
[IsPlistMatrixOverFiniteFieldRep, IsRing, IsPosInt],
function(filter, basedomain, deg)
  return NewMatrixOverFiniteField(filter,
                                  basedomain,
                                  deg,
                                  IdentityMat(deg, basedomain));
end);

InstallMethod(NewIdentityMatrixOverFiniteField,
"for IsPlistMatrixOverFiniteFieldRep, a ring, and zero",
[IsPlistMatrixOverFiniteFieldRep, IsRing, IsZeroCyc],
function(filter, basedomain, deg)
  local m;
  m := NewMatrixOverFiniteField(filter,
                                basedomain,
                                deg,
                                IdentityMat(deg, basedomain));
  # gaplint: ignore 2
  SetRowSpaceBasis(m, NewRowBasisOverFiniteField(
    IsPlistRowBasisOverFiniteFieldRep, basedomain, []));
  SetRowRank(m, 0);
  SetRowSpaceTransformation(m, m);
  SetRowSpaceTransformationInv(m, m);
  # FIXME:
  #  SetSemigroupInverse(m, m);
  return m;
end);

InstallMethod(NewZeroMatrixOverFiniteField,
"for IsPlistMatrixOverFiniteFieldRep, a ring, and zero",
[IsPlistMatrixOverFiniteFieldRep, IsRing, IsZeroCyc],
function(filter, basedomain, deg)
  return NewMatrixOverFiniteField(filter,
                                  basedomain,
                                  deg,
                                  IdentityMat(deg, basedomain));
end);

InstallMethod(NewZeroMatrixOverFiniteField,
"for IsPlistMatrixOverFiniteFieldRep, a ring, and an int",
[IsPlistMatrixOverFiniteFieldRep, IsRing, IsPosInt],
function(filter, basedomain, deg)
  return NewMatrixOverFiniteField(filter,
                                  basedomain,
                                  deg,
                                  NullMat(deg, deg, basedomain));
end);

InstallMethod(ConstructingFilter, "for a plist matrix over finite field",
[IsPlistMatrixOverFiniteFieldRep], m -> IsPlistMatrixOverFiniteFieldRep);

InstallMethod(ConstructingFilter, "for a cvec matrix over finite field",
[IsCVECMatrixOverFiniteFieldRep], m -> IsCVECMatrixOverFiniteFieldRep);

############################################################################
## Printing and viewing methods:
#############################################################################

InstallMethod(ViewObj, "for a plist matrix over finite field",
[IsPlistMatrixOverFiniteFieldRep],
function(m)
  Print("<matrix over ", BaseDomain(m), " of degree ");
  Print(DegreeOfMatrixOverFiniteField(m), ">");
end);

InstallMethod(ViewString, "for a plist matrix over finite field",
[IsPlistMatrixOverFiniteFieldRep],
function(m)
  return STRINGIFY("<matrix over ", BaseDomain(m), " of degree ",
                   DegreeOfMatrixOverFiniteField(m), ">");
end);

InstallMethod(PrintObj, "for a plist matrix over finite field",
[IsPlistMatrixOverFiniteFieldRep],
function(m)
  Print("NewMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep, ",
        BaseDomain(m), ", ", DegreeOfMatrixOverFiniteField(m), ", ", m!.mat,
        ")");
end);

InstallMethod(Display, "for a plist matrix over finite field",
[IsPlistMatrixOverFiniteFieldRep],
function(m)
  Print("<matrix over ", BaseDomain(m), " of degree ",
        DegreeOfMatrixOverFiniteField(m), "\n");
  Print(m!.mat);
  Print(">\n");
end);

InstallMethod(PrintString, "for a plist matrix over finite field",
[IsPlistMatrixOverFiniteFieldRep],
function(m)
  local st;
  st := "NewMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep,";
  Append(st, String(BaseDomain(m)));
  Append(st, ",");
  Append(st, String(DegreeOfMatrixOverFiniteField(m)));
  Append(st, ",");
  Append(st, String(m!.mat));
  Append(st, ")");
  return st;
end);

#T known information can be copied!
InstallMethod(TransposedMatImmutable, "for a plist matrix over finite field",
[IsMatrixOverFiniteField and IsPlistMatrixOverFiniteFieldRep],
function(m)
  local n;
  n := AsMatrixOverFiniteField(m, TransposedMat(m!.mat));
  return n;
end);

InstallMethod(AsMatrix, "for a matrix obj plist matrix rep",
[IsPlistMatrixRep], x -> List(x![ROWSPOS], List));

InstallMethod(AsMatrix, "for a plist matrix over finite field",
[IsPlistMatrixOverFiniteFieldRep], x -> x!.mat);

InstallMethod(RowSpaceBasis, "for a plist matrix over finite field",
[IsMatrixOverFiniteField and IsPlistMatrixOverFiniteFieldRep],
function(m)
   ComputeRowSpaceAndTransformation(m);
   return RowSpaceBasis(m);
end);

InstallMethod(RowRank, "for a plist matrix over finite field",
[IsMatrixOverFiniteField and IsPlistMatrixOverFiniteFieldRep],
function(m)
  ComputeRowSpaceAndTransformation(m);
  return RowRank(m);
end);

#T Should this go in a helper function, it also works
#T similarly to the thing done below.
InstallMethod(RightInverse, "for a plist matrix over finite field",
[IsMatrixOverFiniteField and IsPlistMatrixOverFiniteFieldRep],
function(m)
  local deg, u, rsp, zv, se, i;

  deg := DegreeOfMatrixOverFiniteField(m);
  u := One(BaseDomain(m));

  rsp := SEMIGROUPS_MutableCopyMat(m!.mat);
  zv := [1 .. deg] * Zero(BaseDomain(m));
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

  return AsMatrixOverFiniteField(m, rsp{[1 .. deg]}{[deg + 1 .. 2 * deg]});
end);

InstallMethod(LeftInverse, "for a plist matrix over finite field",
[IsMatrixOverFiniteField and IsPlistMatrixOverFiniteFieldRep],
function(m)
  return TransposedMat(RightInverse(TransposedMat(m)));
end);

#T Trying to use "Inverse" in the semigroup sense
#T here leads to problems with other operations,
#T so we have to be very careful
InstallMethod(InverseOp, "for a plist matrix over finite field",
[IsMatrixOverFiniteField and IsPlistMatrixOverFiniteFieldRep],
function(m)
  local x;

  if DegreeOfMatrixOverFiniteField(m) = 0 then
    return m;
  else
    x := m!.mat ^ (-1);

    if x = fail then
      return fail;
    else
      return AsMatrixOverFiniteField(m, x);
    fi;
  fi;
end);

############################################################################
## Helper functions to deal with matrices.
#############################################################################

InstallGlobalFunction(ComputeRowSpaceAndTransformation,
function(m)
  local deg, bd, bas, tr, tri, inv, sinv, rsp, zv, heads, tm, i;

  if not IsPlistMatrixOverFiniteFieldRep(m) then
    Error("Semigroups: ComputeRowSpaceAndTransformation: usage,\n",
          "the argument must belong to `IsPlistMatrixOverFiniteFieldRep`,");
    return;
  fi;

  Info(InfoMatrixSemigroups, 2, "ComputeRowSpaceAndTransformation called");

  deg := DegreeOfMatrixOverFiniteField(m);
  bd := BaseDomain(m);
  if IsZero(m) then
    bas := [];
    tr := NewIdentityMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep,
                                           bd,
                                           deg);
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
    zv := [1 .. deg] * Zero(bd);
    for i in [1 .. deg] do
      Append(rsp[i], ShallowCopy(zv));
      rsp[i][deg + i] := One(bd);
    od;
    TriangulizeMat(rsp);

    heads := [];
    bas := rsp{[1 .. deg]}{[1 .. deg]};
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
    for i in [1 .. deg] do
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
    tri := tr ^ (-1);
  fi;

  ConvertToVectorRep(bas);
  MakeImmutable(bas);
  bas := NewRowBasisOverFiniteField(IsPlistRowBasisOverFiniteFieldRep, bd, bas);
  SetRowSpaceBasis(m, bas);
  SetRowRank(m, Rank(bas));
  SetRowSpaceTransformation(m, tr);
  SetRowSpaceTransformationInv(m, tri);
  # FIXME
  #  SetSemigroupInverse(m, sinv);
  SetInverse(m, inv);
end);

#############################################################################
##
#F  RandomMatrixOverFiniteField( <m>, <n> [, <R>] ) . .  make a random matrix
##
##  'RandomMatrixOverFiniteField' returns a random semigroups matrix object
##  in IsMatrixOverFiniteFieldPlistRep with <m> rows and <n> columns with
##  elements taken from the ring <R>, which defaults to 'Integers'.
##
#W  This returns a matrix in IsSPlistMatrixRep
#T  this function should take either a filter or a sample matrix
##
InstallGlobalFunction(RandomMatrixOverFiniteField,
function (arg)
  local m, n, R, mat, row, i, k;

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
    # FIXME make this error message more meaningful
    Error("Semigroups: RandomMatrixOverFiniteField: usage,\n",
           "RandomMat( <m>, <n> [, <F>] ),");
    return;
  fi;

  # now construct the random matrix
  mat := [];
  for i  in [1 .. m]  do
    row := [];
    for k  in [1 .. n]  do
      row[k] := Random(R);
    od;
    mat[i] := row;
  od;

  return NewMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep,
                                  R, n, One(R) * mat);
end);

InstallGlobalFunction(RandomSquareMatrixOverFiniteFieldWithRanks,
function(R, n, ranks)
  local gens, z, rk, mat, zv, conj, j;

  if ForAny(ranks, x -> (x < 0) or (x > n)) then
    Error("Semigroups: RandomSquareMatrixOverFiniteFieldWithRanks: usage,\n",
          "the list of ranks has to consist of numbers > 0 and < n,");
    return;
  fi;

  gens := [];
  z := Zero(R);
  # Choose a matrix of given rank
  rk := Random(ranks);
  if rk = 0 then
    return NewZeroMatrixOverFiniteField(IsPlistMatrixRep, R, n);
  else
    mat := SEMIGROUPS_MutableCopyMat(Random(GL(rk, R)));
    # Extend it to n x n
    zv := [1 .. n - rk] * z;
    for j in [1 .. rk] do
      Append(mat[j], zv);
    od;
    zv := [1 .. n] * z;
    for j in [1 .. n - rk] do
      Add(mat, zv);
    od;
    # Swirl around
    #T Is Permuting rows/columns enough?
    conj := Random(GL(n, R)); # PermutationMat(Random(Sym(n)), n, R);
    return NewMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep,
                                    R, n, mat ^ conj);
  fi;
end);

InstallGlobalFunction(RandomListOfMatricesWithRanks,
function(R, m, n, ranks)

  if ForAny(ranks, x -> (x < 0) or (x > n)) then
    Error("Semigroups: RandomListOfMatricesWithRanks: usage,\n",
          "the list of ranks (4th argument) must consist of ",
          "numbers > 0 and < n,");
    return;
  fi;

  return List([1 .. m],
              x -> RandomSquareMatrixOverFiniteFieldWithRanks(R, n, ranks));
end);

#T This will break transparency wrt representations, so we should
#T really not be doing this and instead use a sample object
#T or we should be using NewIdentityMatrixOverFiniteField
InstallMethod(IdentityMatrixOverFiniteField, "for a finite field and zero",
[IsField and IsFinite, IsZeroCyc],
function(R, n)
  return NewIdentityMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep,
                                          R,
                                          n);
end);

InstallMethod(IdentityMatrixOverFiniteField, "for a finite field and pos int",
[IsField and IsFinite, IsPosInt],
function(R, n)
  return NewIdentityMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep,
                                          R,
                                          n);
end);

InstallMethod(IdentityMatrixOverFiniteField,
"for an matrix over finite field and zero",
[IsMatrixOverFiniteField, IsZeroCyc],
function(smat, n)
  return NewIdentityMatrixOverFiniteField(ConstructingFilter(smat),
                                          BaseDomain(smat),
                                          n);
end);

InstallMethod(IdentityMatrixOverFiniteField,
"for an matrix over finite field and pos int",
[IsMatrixOverFiniteField, IsPosInt],
function(smat, n)
  return NewIdentityMatrixOverFiniteField(ConstructingFilter(smat),
                                          BaseDomain(smat),
                                          n);
end);

#InstallMethod(InverseOp, "for an matrix over finite field",
#[IsMatrixOverFiniteField],
#function(smat)
#  local mat;
#  mat := Inverse(smat!.mat);
#  if mat = fail then
#    return fail;
#  fi;
#  return AsMatrixOverFiniteField(smat, mat);
#end);

InstallMethod(AsMatrixOverFiniteField,
"for an matrix over finite field and a matrix",
[IsMatrixOverFiniteField, IsMatrix],
function(smat, mat)
  return NewMatrixOverFiniteField(ConstructingFilter(smat),
                                  BaseDomain(smat),
                                  Length(mat), mat);
end);

InstallMethod(OneMutable, "for an matrix over finite field collection",
[IsMatrixOverFiniteFieldCollection],
coll -> One(Representative(coll)));

InstallMethod(DegreeOfMatrixOverFiniteFieldCollection,
"for an matrix over finite field collection",
[IsMatrixOverFiniteFieldCollection],
function(coll)
  local deg;

  deg := DegreeOfMatrixOverFiniteField(coll[1]);
  if not ForAll(coll, x -> DegreeOfMatrixOverFiniteField(x) = deg) then
    Error("Semigroups: DegreeOfMatrixOverFiniteFieldCollection: usage,\n",
          "the argument <coll> must be a collection of matrices of ",
          "equal degree,");
    return;
  fi;

  return deg;
end);

InstallMethod(BaseDomain, "for an matrix over finite field collection",
[IsMatrixOverFiniteFieldCollection],
function(coll)
  local base;
  base := BaseDomain(coll[1]);
  if not ForAll(coll, x -> BaseDomain(x) = base) then
    Error("Semigroups: BaseDomain: usage,\n",
          "the argument <coll> must be a collection of matrices over ",
          "the same finite field,");
    return;
  fi;

  return base;
end);

InstallMethod(IsZero, "for an matrix over finite field",
[IsMatrixOverFiniteField],
x -> IsZero(x!.mat));

InstallMethod(OneMutable, "for an matrix over finite field",
[IsMatrixOverFiniteField],
x -> IdentityMatrixOverFiniteField(x, DegreeOfMatrixOverFiniteField(x)));

InstallMethod(\=, "for an matrix over finite field",
[IsMatrixOverFiniteField, IsMatrixOverFiniteField],
function(x, y)
  return BaseDomain(x) = BaseDomain(y) and x!.mat = y!.mat;
end);

InstallMethod(\<, "for an matrix over finite field",
[IsMatrixOverFiniteField, IsMatrixOverFiniteField],
function(x, y)
  return DegreeOfMatrixOverFiniteField(x) < DegreeOfMatrixOverFiniteField(y)
    or (DegreeOfMatrixOverFiniteField(x) = DegreeOfMatrixOverFiniteField(y)
        and BaseDomain(x) < BaseDomain(y))
    or (DegreeOfMatrixOverFiniteField(x) = DegreeOfMatrixOverFiniteField(y)
        and BaseDomain(x) = BaseDomain(y) and x!.mat < y!.mat);
end);

InstallMethod(\*, "for matrices over finite field",
[IsMatrixOverFiniteField, IsMatrixOverFiniteField],
function(x, y)
  if DegreeOfMatrixOverFiniteField(x) <> DegreeOfMatrixOverFiniteField(y)
      or BaseDomain(x) <> BaseDomain(y) then
    Error("Semigroups: \* (for matrices over a finite field): usage,\n",
          "the degree or domain of the arguments do not match,");
    return;
  fi;

  return AsMatrixOverFiniteField(x, x!.mat * y!.mat);
end);

#T This might call for a separate VectorOverFiniteField implementaion actually
#T At least check lengths
InstallOtherMethod(\*, "for an empty list and an matrix over finite field",
[IsList and IsEmpty, IsMatrixOverFiniteField],
function(l, m)
  return l;
end);

InstallMethod(\*, "for a list of vectors and an matrix over finite field",
[IsFFECollection, IsMatrixOverFiniteField],
function(l, m)
  return l * m!.mat;
end);

InstallMethod(\*, "for a list of vectors and an matrix over finite field",
[IsFFECollColl, IsMatrixOverFiniteField],
function(l, m)
  return l * m!.mat;
end);

InstallMethod(TransposedSMat, "for an matrix over finite field",
[IsMatrixOverFiniteField],
function(m)
  if DegreeOfMatrixOverFiniteField(m) = 0 then
    return m;
  else
    return AsMatrixOverFiniteField(m, TransposedMat(m!.mat));
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
  local r, e;
  for r in m!.mat do
    for e in r do
      if not IsZero(e) then
        return false;
      fi;
    od;
  od;
  return true;
end);
