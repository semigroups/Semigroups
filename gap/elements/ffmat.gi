############################################################################
##
##  ffmat.gi
##  Copyright (C) 2016                                   James D. Mitchell
##                                                         Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains an implementation of matrices over finite fields.

InstallMethod(SEMIGROUPS_TypeViewStringOfMatrixOverSemiring,
"for a matrix over finite field",
[IsMatrixOverFiniteField], x -> "finite field");

InstallMethod(SEMIGROUPS_FilterOfMatrixOverSemiring,
"for a plist matrix over a finite field",
[IsPlistMatrixOverFiniteFieldRep], x -> IsPlistMatrixOverFiniteFieldRep);

InstallMethod(SEMIGROUPS_TypeOfMatrixOverSemiringCons,
"for IsPlistMatrixOverFiniteFieldRep",
[IsPlistMatrixOverFiniteFieldRep], x -> PlistMatrixOverFiniteFieldType);

# It is not currently possible to create any type of matrix except
# IsPlistMatrixOverFiniteFieldRep

InstallMethod(ELM_LIST, "for a plist matrix over finite field and pos int",
[IsPlistMatrixOverFiniteFieldRep, IsPosInt],
function(mat, pos)
  if not IsBound(mat[pos]) then
    ErrorNoReturn("Semigroups: ELM_LIST (for a plist matrix over finite",
                  "field):\nthe position is greater than the dimension ",
                  "of the matrix,");
  fi;
  return mat!.mat[pos];
end);

InstallMethod(IsBound\[\],
"for a plist matrix over finite field and pos int",
[IsPlistMatrixOverFiniteFieldRep, IsPosInt],
function(mat, pos)
  return IsBound(mat!.mat[pos]);
end);

# This should be used with caution, it can create corrupt objects

InstallMethod(MatrixNC, "for a matrix over finite field and list",
[IsMatrixOverFiniteField, IsList],
function(sample, mat)
  return MatrixNC(sample, ShallowCopy(mat));
end);

InstallMethod(MatrixNC, "for a matrix over finite field and mutable list",
[IsMatrixOverFiniteField, IsList and IsMutable],
function(sample, mat)
  local filter;

  # Cannot use TypeObj(sample) since it can contain information about
  # properties satisfied (or not) by x.
  filter := SEMIGROUPS_FilterOfMatrixOverSemiring(sample);

  MakeImmutable(mat);
  mat := Objectify(SEMIGROUPS_TypeOfMatrixOverSemiringCons(filter),
                   rec(mat := mat));
  SetBaseDomain(mat, BaseDomain(sample));
  return mat;
end);

InstallMethod(OneImmutable, "for a matrix over a finite field",
[IsMatrixOverFiniteField],
function(x)
  local n, id, one, i;

  n := DimensionOfMatrixOverSemiring(x);
  id := List([1 .. n], y -> [1 .. n] * Zero(BaseDomain(x)));
  one := One(BaseDomain(x));

  for i in [1 .. n] do
    id[i][i] := one;
  od;

  return MatrixNC(x, id);
end);

InstallMethod(RandomMatrixOp, "for a finite field and pos int",
[IsField and IsFinite, IsPosInt],
function(field, n)
  local xy, i, j;

  xy := List([1 .. n], x -> EmptyPlist(n));
  for i in [1 .. n] do
    for j in [1 .. n] do
      xy[i][j] := Random(field);
    od;
  od;
  return Matrix(field, xy);
end);

InstallMethod(RandomMatrixOp,
"for a finite field, dimension, and list of ranks",
[IsField and IsFinite, IsPosInt, IsList],
function(R, n, ranks)
  local z, rk, mat, zv, conj, j;

  if ForAny(ranks, x -> (x < 0) or (x > n)) then
    ErrorNoReturn("Semigroups: RandomMatrixOp: usage,\n",
                  "the list of ranks has to consist of numbers > 0 and < n,");
  fi;

  z := Zero(R);
  # Choose a matrix of given rank
  rk := Random(ranks);
  if rk = 0 then
    return NewZeroMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep, R, n);
  fi;
  mat := AsMutableList(Random(GL(rk, R)));
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
  # Is Permuting rows/columns enough?
  conj := Random(GL(n, R));  # PermutationMat(Random(Sym(n)), n, R);
  return NewMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep,
                                  R,
                                  mat ^ conj);
end);

InstallMethod(RandomMatrixOp,
"for a finite field, dimension, and pos int",
[IsField and IsFinite, IsPosInt, IsPosInt],
function(R, n, rank)
  return RandomMatrixOp(R, n, [rank]);
end);

InstallMethod(RandomMatrixOp,
"for a finite field, 0 dimension, and list of ranks",
[IsField and IsFinite, IsZeroCyc, IsList],
function(R, n, ranks)
  if ForAny(ranks, x -> (x < 0) or (x > n)) then
    ErrorNoReturn("Semigroups: RandomMatrixOp: usage,\n",
                  "the list of ranks has to consist of numbers >= 0 and <= ",
                  n, ",");
  fi;
  return Matrix(R, []);
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

InstallMethod(ViewString, "for a plist rowbasis",
[IsPlistRowBasisOverFiniteFieldRep],
function(v)
  return STRINGIFY("<rowbasis of rank ", Rank(v), " over ",
                   BaseDomain(v), ">");
end);

InstallMethod(String, "for a plist rowbasis",
[IsPlistRowBasisOverFiniteFieldRep],
function(v)
  return STRINGIFY("NewRowBasisOverFiniteField(",
                   "IsPlistRowBasisOverFiniteFieldRep, ",
                   BaseDomain(v), ", ", v!.rows, ")");
end);

#############################################################################
## Creating a matrix
#############################################################################
#
# Create a new matrix. We will actually check whether all entries are in the
# correct domain, i.e. that the field over which we operate is chosen
# big enough, and that all entries are given inside the field. This will
# prevent us and users from creating stupid matrix objects.

# Check correct format and correctness of entries

InstallMethod(NewMatrixOverFiniteField,
"for IsPlistMatrixOverFiniteFieldRep, a finite field, and a list",
[IsPlistMatrixOverFiniteFieldRep, IsField and IsFinite, IsList],
function(filter, field, list)
  local mat, filter2;

  if not ForAll(list, v -> ForAll(v, x -> x in field)) then
    ErrorNoReturn("Semigroups: NewMatrixOverFiniteField: usage,\n",
                  "the entries of the matrix are not all in ",
                  field, ",");
  fi;

  filter2 := filter and IsMatrixOverFiniteField;

  if HasCanEasilyCompareElements(Representative(field))
      and CanEasilyCompareElements(Representative(field)) then
    filter2 := filter2 and CanEasilyCompareElements;
  fi;

  mat := Objectify(PlistMatrixOverFiniteFieldType,
                   rec(mat := ImmutableMatrix(field, list)));
  SetBaseDomain(mat, field);

  return mat;
end);

# InstallMethod(NewMatrixOverFiniteField,
# "for IsPlistMatrixOverFiniteFieldRep, a ring, an int, and IsPlistMatrixRep",
# [IsPlistMatrixOverFiniteFieldRep, IsRing, IsInt, IsPlistMatrixRep],
# function(filter, basedomain, rl, mat)
#   return NewMatrixOverFiniteField(filter, basedomain, rl, AsMatrix(mat));
# end);

InstallMethod(NewIdentityMatrixOverFiniteField,
"for IsPlistMatrixOverFiniteFieldRep, a ring, and an int",
[IsPlistMatrixOverFiniteFieldRep, IsRing, IsPosInt],
function(filter, basedomain, deg)
  return NewMatrixOverFiniteField(filter,
                                  basedomain,
                                  IdentityMat(deg, basedomain));
end);

InstallMethod(NewIdentityMatrixOverFiniteField,
"for IsPlistMatrixOverFiniteFieldRep, a ring, and zero",
[IsPlistMatrixOverFiniteFieldRep, IsRing, IsZeroCyc],
function(filter, basedomain, deg)
  local m;
  m := NewMatrixOverFiniteField(filter,
                                basedomain,
                                IdentityMat(deg, basedomain));
  SetRowSpaceBasis(m, NewRowBasisOverFiniteField(
    IsPlistRowBasisOverFiniteFieldRep, basedomain, []));
  SetRowRank(m, 0);
  SetRowSpaceTransformation(m, m);
  SetRowSpaceTransformationInv(m, m);
  # TODO(later) uncomment the next line
  # SetSemigroupInverse(m, m);
  return m;
end);

InstallMethod(NewZeroMatrixOverFiniteField,
"for IsPlistMatrixOverFiniteFieldRep, a ring, and zero",
[IsPlistMatrixOverFiniteFieldRep, IsRing, IsZeroCyc],
function(filter, basedomain, deg)
  return NewMatrixOverFiniteField(filter,
                                  basedomain,
                                  IdentityMat(deg, basedomain));
end);

InstallMethod(NewZeroMatrixOverFiniteField,
"for IsPlistMatrixOverFiniteFieldRep, a ring, and an int",
[IsPlistMatrixOverFiniteFieldRep, IsRing, IsPosInt],
function(filter, basedomain, deg)
  return NewMatrixOverFiniteField(filter,
                                  basedomain,
                                  NullMat(deg, deg, basedomain));
end);

# TODO known information can be copied!

InstallMethod(TransposedMatImmutable, "for a plist matrix over finite field",
[IsMatrixOverFiniteField and IsPlistMatrixOverFiniteFieldRep],
function(m)
  if DimensionOfMatrixOverSemiring(m) = 0 then
    return m;
  fi;
  return AsMatrix(IsMatrixOverFiniteField, m, TransposedMat(m!.mat));
end);

InstallMethod(AsList, "for a matrix obj plist matrix rep",
[IsPlistMatrixRep], x -> List(x![ROWSPOS], List));

InstallMethod(AsList, "for a plist matrix over finite field",
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

# Should this go in a helper function, it also works
# similarly to the thing done below.
InstallMethod(RightInverse, "for a plist matrix over finite field",
[IsMatrixOverFiniteField and IsPlistMatrixOverFiniteFieldRep],
function(m)
  local deg, u, rsp, zv, se, i;

  deg := DimensionOfMatrixOverSemiring(m);
  u := One(BaseDomain(m));

  rsp := AsMutableList(m!.mat);
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

  return AsMatrix(IsMatrixOverFiniteField,
                  m,
                  rsp{[1 .. deg]}{[deg + 1 .. 2 * deg]});
end);

InstallMethod(LeftInverse, "for a plist matrix over finite field",
[IsMatrixOverFiniteField and IsPlistMatrixOverFiniteFieldRep],
function(m)
  return TransposedMat(RightInverse(TransposedMat(m)));
end);

# Trying to use "Inverse" in the semigroup sense here leads to problems
# with other operations, so we have to be very careful.

InstallMethod(InverseMutable, "for a plist matrix over finite field",
[IsMatrixOverFiniteField and IsPlistMatrixOverFiniteFieldRep],
InverseImmutable);

InstallMethod(InverseImmutable, "for a plist matrix over finite field",
[IsMatrixOverFiniteField and IsPlistMatrixOverFiniteFieldRep],
function(m)
  local x;

  if DimensionOfMatrixOverSemiring(m) = 0 then
    return m;
  fi;

  x := m!.mat ^ (-1);
  if x = fail then
    return fail;
  fi;
  return AsMatrix(IsMatrixOverFiniteField, m, x);
end);

############################################################################
## Helper functions to deal with matrices.
#############################################################################

InstallGlobalFunction(ComputeRowSpaceAndTransformation,
function(m)
  local deg, bd, bas, tr, tri, inv, sinv, rsp, zv, heads, tm, i;

  if not IsPlistMatrixOverFiniteFieldRep(m) then
    ErrorNoReturn("Semigroups: ComputeRowSpaceAndTransformation: usage,\n",
                  "the argument must belong to",
                  "`IsPlistMatrixOverFiniteFieldRep`,");
  fi;

  deg := DimensionOfMatrixOverSemiring(m);
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
    rsp := AsMutableList(m!.mat);
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
    # This is obviously totally ridiculous to do the same computation
    # twice
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
  # TODO(later) uncomment the next line
  # SetSemigroupInverse(m, sinv);
  SetInverse(m, inv);
end);

# This will break transparency wrt representations, so we should
# really not be doing this and instead use a sample object
# or we should be using NewIdentityMatrixOverFiniteField
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
"for a matrix over finite field and zero",
[IsMatrixOverFiniteField, IsZeroCyc],
function(mat, n)
  return
    NewIdentityMatrixOverFiniteField(SEMIGROUPS_FilterOfMatrixOverSemiring(mat),
                                     BaseDomain(mat),
                                     n);
end);

InstallMethod(IdentityMatrixOverFiniteField,
"for a matrix over finite field and pos int",
[IsMatrixOverFiniteField, IsPosInt],
function(mat, n)
  return
    NewIdentityMatrixOverFiniteField(SEMIGROUPS_FilterOfMatrixOverSemiring(mat),
                                     BaseDomain(mat),
                                     n);
end);

InstallMethod(AsMatrix,
"for IsMatrixOverFiniteField, a matrix over finite field, and a matrix",
[IsMatrixOverFiniteField, IsMatrixOverFiniteField, IsMatrix],
function(filt, sample, mat)
  return NewMatrixOverFiniteField(SEMIGROUPS_FilterOfMatrixOverSemiring(sample),
                                  BaseDomain(sample),
                                  mat);
end);

InstallMethod(BaseDomain, "for a matrix over finite field collection",
[IsMatrixOverFiniteFieldCollection],
function(coll)
  local base;
  base := BaseDomain(coll[1]);
  if not ForAll(coll, x -> BaseDomain(x) = base) then
    ErrorNoReturn("Semigroups: BaseDomain: usage,\n",
                  "the argument <coll> must be a collection of matrices over ",
                  "the same finite field,");
  fi;

  return base;
end);

InstallMethod(IsZero, "for a matrix over finite field",
[IsMatrixOverFiniteField],
x -> IsZero(x!.mat));

InstallMethod(OneMutable, "for a matrix over finite field",
[IsMatrixOverFiniteField],
x -> IdentityMatrixOverFiniteField(x, DimensionOfMatrixOverSemiring(x)));

InstallMethod(\=, "for a matrix over finite field",
[IsMatrixOverFiniteField, IsMatrixOverFiniteField],
function(x, y)
  return BaseDomain(x) = BaseDomain(y) and x!.mat = y!.mat;
end);

InstallMethod(\<, "for a matrix over finite field",
[IsMatrixOverFiniteField, IsMatrixOverFiniteField],
function(x, y)
  return DimensionOfMatrixOverSemiring(x) < DimensionOfMatrixOverSemiring(y)
    or (DimensionOfMatrixOverSemiring(x) = DimensionOfMatrixOverSemiring(y)
        and BaseDomain(x) <> BaseDomain(y)
        and Characteristic(BaseDomain(x)) < Characteristic(BaseDomain(y)))
    or (DimensionOfMatrixOverSemiring(x) = DimensionOfMatrixOverSemiring(y)
        and BaseDomain(x) <> BaseDomain(y)
        and Characteristic(BaseDomain(x)) = Characteristic(BaseDomain(y))
        and DegreeOverPrimeField(BaseDomain(x))
            < DegreeOverPrimeField(BaseDomain(y)))
    or (DimensionOfMatrixOverSemiring(x) = DimensionOfMatrixOverSemiring(y)
        and BaseDomain(x) = BaseDomain(y) and x!.mat < y!.mat);
end);

InstallMethod(\*, "for matrices over finite field",
[IsPlistMatrixOverFiniteFieldRep, IsPlistMatrixOverFiniteFieldRep],
function(x, y)
  if DimensionOfMatrixOverSemiring(x) <> DimensionOfMatrixOverSemiring(y)
      or BaseDomain(x) <> BaseDomain(y) then
    ErrorNoReturn("Semigroups: \\* (for matrices over a finite field): ",
                  "usage,\nthe degree or domain of the arguments do not ",
                  "match,");
  fi;

  if DimensionOfMatrixOverSemiring(x) = 0 then
    return x;
  fi;

  # TODO(later) the first arg of the following should be
  # IsPlistMatrixOverFiniteFieldRep, but that does not currently work
  return AsMatrix(IsMatrixOverFiniteField, x, x!.mat * y!.mat);
end);

# This might call for a separate VectorOverFiniteField implementaion actually
# At least check lengths
InstallOtherMethod(\*, "for an empty list and a matrix over finite field",
[IsList and IsEmpty, IsMatrixOverFiniteField],
function(l, m)
  return l;
end);

InstallMethod(\*, "for a list of vectors and a matrix over finite field",
[IsFFECollection, IsMatrixOverFiniteField],
function(l, m)
  return l * m!.mat;
end);

InstallMethod(\*, "for a list of vectors and a matrix over finite field",
[IsFFECollColl, IsMatrixOverFiniteField],
function(l, m)
  return l * m!.mat;
end);

InstallMethod(AsMutableList, "for a matrix", [IsMatrix],
function(m)
  local res, r;

  res := [];
  for r in m do
    Add(res, ShallowCopy(r));
  od;
  return res;
end);

InstallMethod(IndexPeriodOfSemigroupElement, "for a matrix over a finite field",
[IsMatrixOverFiniteField],
x -> SEMIGROUPS.IndexPeriodByRank(x, RowRank));

SEMIGROUPS.HashFunctionForPlistMatricesOverFiniteField := function(x, data)
  local h;

  if DimensionOfMatrixOverSemiring(x) = 0 then
    return 1;
  elif IsInt(data) then
    h := ChooseHashFunction(AsList(x), data);
    data := rec();
    data.func := h.func;
    data.data := h.data;
  fi;

  return data.func(AsList(x), data.data);
end;

SEMIGROUPS.HashFunctionForPlistRowBasisOverFiniteField := function(x, data)
  local h;

  if Rank(x) = 0 then
    return 1;
  elif IsInt(data) then
    h := ChooseHashFunction(x!.rows, data);
    data := rec();
    data.func := h.func;
    data.data := h.data;
  fi;
  return data.func(x!.rows, data.data);
end;

InstallMethod(ChooseHashFunction, "for plist matrices over finite fields",
[IsPlistMatrixOverFiniteFieldRep, IsInt],
function(x, hashlen)
  local data;

  if DimensionOfMatrixOverSemiring(x) <> 0 then
    data := ChooseHashFunction(AsList(x), hashlen);
  else
    data := hashlen;
  fi;
  return rec(func := SEMIGROUPS.HashFunctionForPlistMatricesOverFiniteField,
             data := data);
end);

InstallMethod(ChooseHashFunction, "for plist rowbasis over finite fields",
[IsPlistRowBasisOverFiniteFieldRep, IsInt],
function(x, hashlen)
  local data;

  if Rank(x) <> 0 then
    data := ChooseHashFunction(x!.rows, hashlen);
  else
    data := hashlen;
  fi;
  return rec(func := SEMIGROUPS.HashFunctionForPlistRowBasisOverFiniteField,
             data := data);
end);
