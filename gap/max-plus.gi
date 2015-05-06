############################################################################
##
#W  max-plus.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains an implementation of max-plus matrices.

InstallMethod(TypeViewStringOfMatrixOverSemiring, "for a max-plus matrix",
[IsMaxPlusMatrix], x -> "max-plus");

# TODO change it to non-NC version
InstallMethod(TypePrintStringOfMatrixOverSemiring, "for a max-plus matrix",
[IsMaxPlusMatrix], x -> "MaxPlusMatrixNC");

InstallGlobalFunction(MaxPlusMatrixNC,
x -> Objectify(MaxPlusMatrixType, x));

InstallMethod(\*, "for max-plus matrices", [IsMaxPlusMatrix, IsMaxPlusMatrix],
function(x, y)
  local n, xy, val, i, j, k;

  n := DimensionOfMatrixOverSemiring(x);
  xy := List([1 .. n], x -> EmptyPlist(n));

  for i in [1 .. n] do
    for j in [1 .. n] do
      val := -infinity;
      for k in [1 .. n] do
        val := Maximum(val, PlusMinMax(x![i][k], y![k][j]));
      od;
      xy[i][j] := val;
    od;
  od;
  return MaxPlusMatrixNC(xy);
end);

InstallMethod(OneImmutable, "for a max-plus matrix",
[IsMaxPlusMatrix],
function(x)
  local n, id, i;
  n := DimensionOfMatrixOverSemiring(x);
  id := List([1 .. n], x -> [1 .. n] * -infinity);
  for i in [1 .. n] do
    id[i][i] := 0;
  od;
  return MaxPlusMatrixNC(id);
end);

InstallMethod(OneMutable, "for a max-plus matrix",
[IsMaxPlusMatrix], OneImmutable);

InstallMethod(RandomMaxPlusMatrix, "for a pos int", [IsPosInt],
n -> SEMIGROUPS_RandomMatrixOverSemiring(n, -infinity, MaxPlusMatrixType));
