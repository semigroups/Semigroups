############################################################################
##
#W  matrix-max-plus.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#############################################################################
## This file contains declarations for max-plus, min-plus, tropical max-plus,
## tropical min-plus matrices, projective. 
##
## It is organized as follows:
##
##   1. Max-plus
##
##   2. Min-plus
##
##   3. Tropical max-plus
## 
##   4. Tropical min-plus
##
##   5. Projective max-plus matrices
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

BindGlobal("SEMIGROUPS_IdentityMat",
function(x, zero, one)
  local n, id, i;
  n := DimensionOfMatrixOverSemiring(x);
  id := List([1 .. n], x -> [1 .. n] * zero);
  for i in [1 .. n] do
    id[i][i] := one;
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
[IsMaxPlusMatrix],
x -> MaxPlusMatrixNC(SEMIGROUPS_IdentityMat(x, -infinity, 0)));

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

InstallMethod(OneImmutable, "for a max-plus matrix",
[IsMaxPlusMatrix],
x -> MaxPlusMatrixNC(SEMIGROUPS_IdentityMat(x, infinity, 0)));

InstallMethod(OneMutable, "for a min-plus matrix",
[IsMinPlusMatrix], OneImmutable);

InstallMethod(RandomMinPlusMatrix, "for a pos int", [IsPosInt],
n -> SEMIGROUPS_RandomMatrixOverSemiring(n, infinity, MinPlusMatrixNC));

#############################################################################
## 3. Tropical matrices
#############################################################################

InstallMethod(ThresholdTropicalMatrix, "for a tropical matrix",
[IsTropicalMatrix], x -> x![DimensionOfMatrixOverSemiring(x) + 1]);

BindGlobal("SEMIGROUPS_TropicalizeMat",
function(mat, threshold)
  local n, i, j;

  n := Length(mat);
  mat[n + 1] := threshold;
  for i in [1 .. n] do
    for j in [1 .. n] do
      # TODO is this correct? In Semigroupe it is not possible to every get
      # infinity in a product
      if mat[i][j] <> infinity and mat[i][j] > threshold then
        mat[i][j] := threshold;
      fi;
      if mat[i][j] <> -infinity then
        mat[i][j] := AbsInt(mat[i][j]);
      fi;
    od;
  od;
  return mat;
end);

#############################################################################
## 4. Tropical max-plus matrices
#############################################################################

InstallMethod(TypeViewStringOfMatrixOverSemiring,
"for a tropical max-plus matrix",
[IsTropicalMaxPlusMatrix], x -> "tropical max-plus");

# TODO change it to non-NC version
InstallMethod(TypePrintStringOfMatrixOverSemiring,
"for a tropical max-plus matrix",
[IsTropicalMaxPlusMatrix], x -> "TropicalMaxPlusMatrixNC");

InstallGlobalFunction(TropicalMaxPlusMatrixNC,
function(mat, threshold)
  return Objectify(TropicalMaxPlusMatrixType,
                   SEMIGROUPS_TropicalizeMat(mat, threshold));
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
x -> TropicalMaxPlusMatrixNC(SEMIGROUPS_IdentityMat(x, -infinity, 0),
                             ThresholdTropicalMatrix(x)));

InstallMethod(OneMutable, "for a tropical max-plus matrix",
[IsTropicalMaxPlusMatrix], OneImmutable);

InstallMethod(RandomTropicalMaxPlusMatrix, "for a pos ints",
[IsPosInt, IsPosInt],
function(dim, threshold)
  # gaplint: ignore 2
  return SEMIGROUPS_RandomMatrixOverSemiring(
    dim, -infinity, x -> TropicalMaxPlusMatrixNC(x, threshold));
end);

#############################################################################
## 5. Tropical min-plus matrices
#############################################################################

InstallMethod(TypeViewStringOfMatrixOverSemiring,
"for a tropical min-plus matrix",
[IsTropicalMinPlusMatrix], x -> "tropical min-plus");

# TODO change it to non-NC version
InstallMethod(TypePrintStringOfMatrixOverSemiring,
"for a tropical min-plus matrix",
[IsTropicalMinPlusMatrix], x -> "TropicalMinPlusMatrixNC");

InstallGlobalFunction(TropicalMinPlusMatrixNC,
function(mat, threshold)
  return Objectify(TropicalMinPlusMatrixType,
                   SEMIGROUPS_TropicalizeMat(mat, threshold));
end);

InstallMethod(\*, "for tropical min-plus matrices",
[IsTropicalMinPlusMatrix, IsTropicalMinPlusMatrix],
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
      if val <> infinity and val > ThresholdTropicalMatrix(x) then
        val := ThresholdTropicalMatrix(x);
      fi;
      xy[i][j] := val;
    od;
  od;
  return TropicalMinPlusMatrixNC(xy, ThresholdTropicalMatrix(x));
end);

InstallMethod(OneImmutable, "for tropical min-plus matrix",
[IsTropicalMinPlusMatrix],
x -> TropicalMinPlusMatrixNC(SEMIGROUPS_IdentityMat(x, infinity, 0),
                             ThresholdTropicalMatrix(x)));

InstallMethod(OneMutable, "for a tropical min-plus matrix",
[IsTropicalMinPlusMatrix], OneImmutable);

InstallMethod(RandomTropicalMinPlusMatrix, "for pos ints",
[IsPosInt, IsPosInt],
function(dim, threshold)
  # gaplint: ignore 2
  return SEMIGROUPS_RandomMatrixOverSemiring(
    dim, infinity, x -> TropicalMinPlusMatrixNC(x, threshold));
end);

#############################################################################
## 6. Projective max-plus matrices
#############################################################################

InstallMethod(TypeViewStringOfMatrixOverSemiring,
"for a projective max-plus matrix",
[IsProjectiveMaxPlusMatrix], x -> "projective max-plus");

# TODO change it to non-NC version
InstallMethod(TypePrintStringOfMatrixOverSemiring,
"for a projective max-plus matrix",
[IsProjectiveMaxPlusMatrix], x -> "ProjectiveMaxPlusMatrixNC");

InstallGlobalFunction(ProjectiveMaxPlusMatrixNC,
x -> Objectify(ProjectiveMaxPlusMatrixType, x));

InstallMethod(\*, "for projective max-plus matrices",
[IsProjectiveMaxPlusMatrix, IsProjectiveMaxPlusMatrix],
function(x, y)
  local n, xy, norm, val, i, j, k;

  n := DimensionOfMatrixOverSemiring(x);
  xy := List([1 .. n], x -> EmptyPlist(n));
  norm := -infinity;

  for i in [1 .. n] do
    for j in [1 .. n] do
      val := -infinity;
      for k in [1 .. n] do
        val := Maximum(val, SEMIGROUPS_PlusMinMax(x![i][k], y![k][j]));
      od;
      xy[i][j] := val;
      if val > norm then 
        norm := val;
      fi;
    od;
  od;

  for i in [1 .. n] do
    for j in [1 .. n] do
      if xy[i][j] <> -infinity then 
        xy[i][j] := xy[i][j] - norm;
      fi;
    od;
  od;
  return ProjectiveMaxPlusMatrixNC(xy);
end);

InstallMethod(OneImmutable, "for a projective max-plus matrix",
[IsProjectiveMaxPlusMatrix],
x -> ProjectiveMaxPlusMatrixNC(SEMIGROUPS_IdentityMat(x, -infinity, 0)));

InstallMethod(RandomProjectiveMaxPlusMatrix, "for a pos int",
[IsPosInt], 
dim -> SEMIGROUPS_RandomMatrixOverSemiring(dim, 
                                           -infinity,
                                           ProjectiveMaxPlusMatrixNC));

#############################################################################
## 7. Natural number matrices
#############################################################################

InstallMethod(ThresholdNaturalMatrix, "for a natural matrix",
[IsNaturalMatrix], x -> x![DimensionOfMatrixOverSemiring(x) + 1]);

InstallMethod(PeriodNaturalMatrix, "for a natural matrix",
[IsNaturalMatrix], x -> x![DimensionOfMatrixOverSemiring(x) + 2]);

BindGlobal("SEMIGROUPS_NaturalizeMat",
function(x, threshold, period)
  local n, i, j;

  n := Length(x);
  x[n + 1] := threshold;
  x[n + 2] := period;
  for i in [1 .. n] do
    for j in [1 .. n] do
      x[i][j] := AbsInt(x[i][j]);
      if x[i][j] > threshold then
        x[i][j] := threshold + (x[i][j] - threshold) mod period; 
      fi;
    od;
  od;
  return x;
end);

InstallMethod(TypeViewStringOfMatrixOverSemiring,
"for a natural number matrix",
[IsNaturalMatrix], x -> "natural");

# TODO change it to non-NC version
InstallMethod(TypePrintStringOfMatrixOverSemiring,
"for a natural number matrix",
[IsNaturalMatrix], x -> "NaturalMatrixNC");

InstallGlobalFunction(NaturalMatrixNC,
function(x, threshold, period)
  return Objectify(NaturalMatrixType, 
                   SEMIGROUPS_NaturalizeMat(x, threshold, period));
end);

InstallMethod(\*, "for natural number matrices",
[IsNaturalMatrix, IsNaturalMatrix],
function(x, y)
  local n, period, threshold, xy, i, j, k;
  
  n := DimensionOfMatrixOverSemiring(x);
  period := PeriodNaturalMatrix(x);
  threshold := ThresholdNaturalMatrix(x);
  # TODO should really check that x and y are matrices over the same semiring!!
  xy := List([1 .. n], x -> EmptyPlist(n));

  for i in [1 .. n] do 
    for j in [1 .. n] do 
      xy[i][j] := 0;
      for k in [1 .. n] do 
        xy[i][j] := xy[i][j] + x![i][k] * y![k][j];
      od;
      if xy[i][j] > threshold then    
        xy[i][j] := threshold + (xy[i][j] - threshold) mod period;
      fi;
    od;
  od;
  return NaturalMatrixNC(xy, threshold, period);
end);

InstallMethod(OneImmutable, "for a natural number matrix",
[IsNaturalMatrix],
x -> NaturalMatrixNC(SEMIGROUPS_IdentityMat(x, 0, 1),
                     ThresholdNaturalMatrix(x), 
                     PeriodNaturalMatrix(x)));

InstallMethod(RandomNaturalMatrix, "for dimension, threshold, period (pos ints)",
[IsPosInt, IsPosInt, IsPosInt], 
function(dim, threshold, period)

  return SEMIGROUPS_RandomMatrixOverSemiring(dim, 
                                             false,
                                             x -> NaturalMatrixNC(x, threshold, period));
end);
