############################################################################
##
#W  matrix-finite-field.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains an implementation of matrices over finite fields.

InstallMethod(TypeViewStringOfMatrixOverSemiring, "for a matrix over finite field",
[IsMatrixOverFiniteField], x -> "finite field");

#TODO make it non-NC

InstallMethod(TypePrintStringOfMatrixOverSemiring, "for a matrix over finite field",
[IsMatrixOverFiniteField], x -> "MatrixOverFiniteFieldNC");

InstallGlobalFunction(MatrixOverFiniteFieldNC,
x -> Objectify(MatrixOverFiniteFieldType, x));

InstallMethod(BaseField, "for a matrix over finite field",
[IsMatrixOverFiniteField], x -> x![DimensionOfMatrixOverSemiring(x) + 1]);

BindGlobal("SEMIGROUPS_FFEizeMat",
function(mat, field)
  local n, i, j;

  n := Length(mat);
  mat[n + 1] := field;
  for i in [1 .. n] do
    for j in [1 .. n] do
      mat[i][j] := mat[i][j] * PrimitiveRoot(field);
    od;
  od;
  return mat;
end);

InstallMethod(\*, "for matrices over a finite field", 
[IsMatrixOverFiniteField, IsMatrixOverFiniteField],
function(x, y)
  local n, xy, i, j, k;

  n := DimensionOfMatrixOverSemiring(x);
  xy := List([1 .. n], x -> EmptyPlist(n));
  xy[n + 1] := BaseField(x);

  for i in [1 .. n] do
    for j in [1 .. n] do
      for k in [1 .. n] do
        xy[i][j] := xy[i][j] + x![i][k] * y![k][k];
      od;
    od;
  od;
  return MatrixOverFiniteFieldNC(xy);
end);

InstallMethod(OneImmutable, "for a matrix over a finite field",
[IsMatrixOverFiniteField],
function(x)
  local n, id, i;

  n := DimensionOfMatrixOverSemiring(x);
  id := List([1 .. n], x -> [1 .. n] * Zero(BaseField(x)));
  for i in [1 .. n] do
    id[i][i] := One(BaseField(x));
  od;
  return MatrixOverFiniteFieldNC(id);
end);

InstallMethod(OneMutable, "for a matrix over a finite field",
[IsMatrixOverFiniteField], OneImmutable);

InstallMethod(RandomMatrixOverFiniteField, "for a pos int and a finite field", 
[IsPosInt, IsFinite and IsField],
function(n, field)
  local xy, i, j;
  xy := List([1 .. n], x -> EmptyPlist(n));
  for i in [1 .. n] do
    for j in [1 .. n] do
      xy[i][j] := Random(field);
    od;
  od;
  return MatrixOverFiniteFieldNC(xy);
end);

InstallMethod(RandomMatrixOverFiniteField, "for pos int, prime int, pos int", 
[IsPosInt, IsPosInt, IsPosInt],
function(n, p, d)
  return RandomMatrixOverFiniteField(n, GF(p, d));
end);

InstallMethod(Display, "for a matrix over a finite field",
[IsMatrixOverFiniteField],
function(x)
  Display(List([1 .. DimensionOfMatrixOverSemiring(x)], i -> x![i]));
end);
