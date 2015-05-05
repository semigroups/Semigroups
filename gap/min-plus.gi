############################################################################
##
#W  min-plus.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains an implementation of min-plus matrices.

InstallMethod(TypeViewStringOfMatrixOverSemiring, "for a min-plus matrix",
[IsMinPlusMatrix], x -> "min-plus");

# TODO change it to non-NC version
InstallMethod(TypePrintStringOfMatrixOverSemiring, "for a min-plus matrix",
[IsMinPlusMatrix], x -> "MinPlusMatrixNC");

InstallGlobalFunction(MinPlusMatrixNC, x -> Objectify(MinPlusMatrixType, x));

InstallMethod(\*, "for min-plus matrices", [IsMinPlusMatrix, IsMinPlusMatrix],
function(x, y)
  local n, xy, val, i, j, k;

  n := DimensionOfMatrixOverSemiring(x);
  xy := List([1 .. n], x -> EmptyPlist(n));

  for i in [1 .. n] do
    for j in [1 .. n] do
      val := infinity;
      for k in [1 .. n] do
        val := Minimum(val, PlusMinMax(x![i][k], y![k][j]));
      od;
      xy[i][j] := val;
    od;
  od;
  return MinPlusMatrixNC(xy);
end);

InstallMethod(OneImmutable, "for a min-plus matrix",
[IsMinPlusMatrix],
function(x)
  local n, id, i;
  n := DimensionOfMatrixOverSemiring(x);
  id := List([1 .. n], x -> [1 .. n] * infinity);
  for i in [1 .. n] do
    id[i][i] := 0;
  od;
  return MinPlusMatrixNC(id);
end);

InstallMethod(OneMutable, "for a min-plus matrix",
[IsMinPlusMatrix], OneImmutable);

InstallMethod(RandomMinPlusMatrix, "for a pos int", [IsPosInt],
n -> SEMIGROUPS_RandomMatrixOverSemiring(n, infinity, MinPlusMatrixType));
