############################################################################
##
#W  max-plus.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#############################################################################
## This file contains declarations for max-plus, min-plus, tropical max-plus,
## and tropical min-plus matrices.
## 
## It is organized as follows:
##  
##    1. Max-plus matrices
##
##    2. Min-plus matrices
##
#############################################################################

BindGlobal("SEMIGROUPS_PlusMinMax",
function(x, y)
  if x = infinity or y = infinity then
    return infinity;
  elif x = -infinity or y = -infinity then
    return -infinity;
  fi;
  return x + y;
end);

BindGlobal("SEMIGROUPS_MaxPlusIdentity", 
function(x)
  local n, id, i;
  n := DimensionOfMatrixOverSemiring(x);
  id := List([1 .. n], x -> [1 .. n] * -infinity);
  for i in [1 .. n] do
    id[i][i] := 0;
  od;
  return id;
end);

#############################################################################
## 1. Max-plus matrices
#############################################################################

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
        val := Maximum(val, SEMIGROUPS_PlusMinMax(x![i][k], y![k][j]));
      od;
      xy[i][j] := val;
    od;
  od;
  return MaxPlusMatrixNC(xy);
end);

InstallMethod(OneImmutable, "for a max-plus matrix",
[IsMaxPlusMatrix], x -> MaxPlusMatrixNC(SEMIGROUPS_MaxPlusIdentity(x)));

InstallMethod(OneMutable, "for a max-plus matrix",
[IsMaxPlusMatrix], OneImmutable);

InstallMethod(RandomMaxPlusMatrix, "for a pos int", [IsPosInt],
n -> SEMIGROUPS_RandomMatrixOverSemiring(n, -infinity, MaxPlusMatrixNC));

#############################################################################
## 2. Min-plus matrices
#############################################################################

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
        val := Minimum(val, SEMIGROUPS_PlusMinMax(x![i][k], y![k][j]));
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
n -> SEMIGROUPS_RandomMatrixOverSemiring(n, infinity, MinPlusMatrixNC));

#############################################################################
## 3. Tropical max-plus matrices
#############################################################################

InstallMethod(ThresholdTropicalMatrix, "for a tropical max-plus matrix",
[IsTropicalMaxPlusMatrix], x -> x![DimensionOfMatrixOverSemiring(x) + 1]);

InstallMethod(TypeViewStringOfMatrixOverSemiring, "for a tropical max-plus matrix",
[IsTropicalMaxPlusMatrix], x -> "tropical max-plus");

# TODO change it to non-NC version
InstallMethod(TypePrintStringOfMatrixOverSemiring, "for a tropical max-plus matrix",
[IsTropicalMaxPlusMatrix], x -> "TropicalMaxPlusMatrixNC");

InstallGlobalFunction(TropicalMaxPlusMatrixNC, 
function(mat, threshold)
  local n, i, j;

  n := Length(mat);
  mat[n + 1] := threshold;
  for i in [1 .. n] do 
    for j in [1 .. n] do 
      if mat[i][j] > threshold then 
        mat[i][j] := threshold;
      fi;
    od;
  od;
  return Objectify(TropicalMaxPlusMatrixType, mat);
end);

InstallMethod(\*, "for tropical max-plus matrices", 
[IsTropicalMaxPlusMatrix, IsTropicalMaxPlusMatrix],
function(x, y)
  local n, xy, val, i, j, k;

  n := DimensionOfMatrixOverSemiring(x);
  xy := List([1 .. n], x -> EmptyPlist(n));
  
  for i in [1 .. n] do
    for j in [1 .. n] do
      val := -infinity;
      for k in [1 .. n] do
        val := Maximum(val, SEMIGROUPS_PlusMinMax(x![i][k], y![k][j]));
      od;
      if val > ThresholdTropicalMatrix(x) then 
        val := ThresholdTropicalMatrix(x);
      fi;
      xy[i][j] := val;
    od;
  od;
  return TropicalMaxPlusMatrixNC(xy, ThresholdTropicalMatrix(x));
end);

InstallMethod(OneImmutable, "for a max-plus matrix",
[IsTropicalMaxPlusMatrix], 
x -> TropicalMaxPlusMatrixNC(SEMIGROUPS_MaxPlusIdentity(x), 
                             ThresholdTropicalMatrix(x)));

InstallMethod(OneMutable, "for a tropical max-plus matrix",
[IsTropicalMaxPlusMatrix], OneImmutable);

InstallMethod(RandomTropicalMaxPlusMatrix, "for a pos ints", [IsPosInt, IsPosInt],
function(dim, threshold)
  return SEMIGROUPS_RandomMatrixOverSemiring(dim, 
                                             -infinity, 
                                             x -> TropicalMaxPlusMatrixNC(x, threshold));
end);
