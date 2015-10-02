############################################################################
##
#W  matrix-prime-field.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains an implementation of matrices over prime fields.

InstallMethod(SEMIGROUPS_TypeViewStringOfMatrixOverSemiring,
"for a matrix over prime field",
[IsMatrixOverPrimeField], x -> "prime field");


InstallMethod(SEMIGROUPS_TypePrintStringOfMatrixOverSemiring,
"for a matrix over prime field",
[IsMatrixOverPrimeField], x -> "MatrixOverPrimeFieldNC");

#TODO make it non-NC
InstallGlobalFunction(MatrixOverPrimeFieldNC,
function(arg)
  local mat;
  mat := arg[1];
  if Length(arg) = 2 then
    mat[Length(mat) + 1] := Size(arg[2]);
  fi;
  return Objectify(MatrixOverPrimeFieldType, mat);
end);

InstallMethod(BaseField, "for a matrix over prime field",
[IsMatrixOverPrimeField], x -> GF(x![DimensionOfMatrixOverSemiring(x) + 1]));

InstallMethod(\*, "for matrices over a prime field",
[IsMatrixOverPrimeField, IsMatrixOverPrimeField],
function(x, y)
  local n, xy, i, j, k;

  n := DimensionOfMatrixOverSemiring(x);
  xy := List([1 .. n], x -> EmptyPlist(n));
  xy[n + 1] := Size(BaseField(x));

  for i in [1 .. n] do
    for j in [1 .. n] do
      xy[i][j] := 0;
      for k in [1 .. n] do
        xy[i][j] := xy[i][j] + x![i][k] * y![k][j];
      od;
    od;
  od;
  return MatrixOverPrimeFieldNC(xy);
end);

InstallMethod(OneImmutable, "for a matrix over a prime field",
[IsMatrixOverPrimeField],
function(x)
  local n, id, i;

  n := DimensionOfMatrixOverSemiring(x);
  id := List([1 .. n], y -> [1 .. n] * Zero(BaseField(x)));
  id[n + 1] := Size(BaseField(x));
  for i in [1 .. n] do
    id[i][i] := One(BaseField(x));
  od;
  return MatrixOverPrimeFieldNC(id);
end);

InstallMethod(OneMutable, "for a matrix over a prime field",
[IsMatrixOverPrimeField], OneImmutable);

InstallMethod(RandomMatrixOverPrimeField, "for a pos int and a prime field",
[IsPosInt, IsFinite and IsField],
function(n, field)
  local xy, i, j;

  if not IsPrimeField(field) then
    ErrorMayQuit("Semigroups: RandomMatrixOverPrimeField: usage,\n",
                 "the second argument must be a prime field,");
  fi;

  xy := List([1 .. n], x -> EmptyPlist(n));
  xy[n + 1] := Size(field);
  for i in [1 .. n] do
    for j in [1 .. n] do
      xy[i][j] := Random(field);
    od;
  od;
  return MatrixOverPrimeFieldNC(xy);
end);

InstallMethod(RandomMatrixOverPrimeField, "for pos int, prime int, pos int",
[IsPosInt, IsPosInt, IsPosInt],
function(n, p, d)
  return RandomMatrixOverPrimeField(n, GF(p, d));
end);

InstallMethod(Display, "for a matrix over a prime field",
[IsMatrixOverPrimeField],
function(x)
  Display(AsMatrix(x));
end);

# special methods

InstallMethod(AsMatrix, "for a matrix over prime field",
[IsMatrixOverPrimeField],
function(x)
  return List([1 .. DimensionOfMatrixOverSemiring(x)], i -> x![i]);
end);

InstallMethod(AsMatrixOverPrimeField,
"for a prime field and matrix with entries in a prime field",
[IsFinite and IsField, IsMatrix and IsFFECollColl],
function(field, x)
  local n, i, j;

  if not IsPrimeField(field) then
    ErrorMayQuit("Semigroups: AsMatrixOverPrimeField: usage,\n",
                 "the first argument <field> must be a prime field,");
  fi;

  n := Length(x);

  for i in [1 .. n] do
    for j in [1 .. n] do
      if not x[i][j] in field then
        ErrorMayQuit("Semigroups: AsMatrixOverPrimeField: usage,\n",
                     "the entry ", x[i][j], " does not belong to ", field, ",");
      fi;
    od;
  od;

  x := MutableCopyMat(x);
  Apply(x, PlainListCopy);
  x[Length(x) + 1] := Size(field);

  return MatrixOverPrimeFieldNC(x);
end);

InstallMethod(AsMatrixOverPrimeField,
"for a pos int and matrix with entries in prime field",
[IsPosInt, IsMatrix and IsFFECollColl],
function(p, x)
  if not IsPrimeInt(p) then
    ErrorMayQuit("Semigroups: AsMatrixOverPrimeField: usage,\n",
                 "the first argument <p> must be a prime,");
  fi;

  return MatrixOverPrimeField(GF(p), x);
end);

InstallMethod(AsMatrixOverPrimeField,
"for a prime power int and matrix of integers",
[IsPosInt, IsMatrix and IsCyclotomicCollColl],
function(p, x)
  if not IsPrimeInt(p) then
    ErrorMayQuit("Semigroups: AsMatrixOverPrimeField: usage,\n",
                 "the first argument <p> must be a prime,");
  fi;
  return AsMatrixOverPrimeFieldNC(p, x);
end);

InstallMethod(AsMatrixOverPrimeFieldNC,
"for a prime power int and matrix of integers",
[IsPosInt, IsObject],
function(p, x)
  local n, prim, i, j;

  n := Length(x);
  prim := PrimitiveRoot(GF(p));

  for i in [1 .. n] do
    for j in [1 .. n] do
      x[i][j] := x[i][j] * prim;
    od;
  od;
  Apply(x, PlainListCopy);
  x[n + 1] := p;
  return Objectify(MatrixOverPrimeFieldType, x);
end);
