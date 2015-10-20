############################################################################
##
#W  pfmat.gi
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

InstallMethod(SEMIGROUPS_FilterOfMatrixOverSemiring,
"for a matrix over prime field",
[IsMatrixOverPrimeField], x -> IsMatrixOverPrimeField);

InstallMethod(SEMIGROUPS_TypeOfMatrixOverSemiringCons,
"for IsMatrixOverPrimeField",
[IsMatrixOverPrimeField], x -> MatrixOverPrimeFieldType);

InstallMethod(SEMIGROUPS_MatrixOverSemiringEntryCheckerCons,
"for IsMatrixOverPrimeField", [IsMatrixOverPrimeField],
function(filter)
  return IsFFE;
end);

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
  return MatrixNC(x, xy);
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
  return MatrixNC(x, id);
end);

InstallMethod(RandomMatrixOp, "for a prime field and pos int",
[IsPrimeField, IsPosInt],
function(field, n)
  local xy, i, j;

  xy := List([1 .. n], x -> EmptyPlist(n));
  xy[n + 1] := Size(field);
  for i in [1 .. n] do
    for j in [1 .. n] do
      xy[i][j] := Random(field);
    od;
  od;
  return MatrixNC(IsMatrixOverPrimeField, xy);
end);

# special methods

InstallMethod(BaseField, "for a matrix over prime field",
[IsMatrixOverPrimeField], x -> GF(x![DimensionOfMatrixOverSemiring(x) + 1]));

InstallMethod(Display, "for a matrix over a prime field",
[IsMatrixOverPrimeField],
function(x)
  Display(AsMatrix(x));
end);

InstallMethod(AsMatrix, "for a matrix over prime field",
[IsMatrixOverPrimeField],
function(x)
  return List([1 .. DimensionOfMatrixOverSemiring(x)], i -> x![i]);
end);
