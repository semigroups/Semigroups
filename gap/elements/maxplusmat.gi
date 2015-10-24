############################################################################
##
#W  maxplusmat.gi
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
##   1. Max-plus matrices
##
##   2. Min-plus matrices
##
##   3. Tropical matrices
##
##   4. Tropical max-plus matrices
##
##   5. Tropical min-plus matrices
##
##   6. Projective max-plus matrices
##
##   7. NTP matrices
##
##   8. Integer matrices
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
  n := Length(x![1]);
  id := List([1 .. n], x -> [1 .. n] * zero);
  for i in [1 .. n] do
    id[i][i] := one;
  od;
  return id;
end);

BindGlobal("SEMIGROUPS_RandomIntegerMatrix",
function(n, source)
  local out, i, j;

  out := List([1 .. n], x -> EmptyPlist(n));
  for i in [1 .. n] do
    for j in [1 .. n] do
      out[i][j] := Random(Integers);
      if out[i][j] = 0 and source <> false then
        out[i][j] := source;
      elif out[i][j] < 0 then
        out[i][j] := out[i][j] + 1;
      fi;
    od;
  od;
  return out;
end);

#############################################################################
## 1. Max-plus matrices
#############################################################################

InstallMethod(SEMIGROUPS_TypeViewStringOfMatrixOverSemiring,
"for a max-plus matrix",
[IsMaxPlusMatrix], x -> "max-plus");

InstallMethod(SEMIGROUPS_FilterOfMatrixOverSemiring,
"for a max-plus matrix",
[IsMaxPlusMatrix], x -> IsMaxPlusMatrix);

InstallMethod(SEMIGROUPS_TypeOfMatrixOverSemiringCons, "for IsMaxPlusMatrix",
[IsMaxPlusMatrix], x -> MaxPlusMatrixType);

InstallMethod(SEMIGROUPS_MatrixOverSemiringEntryCheckerCons,
"for IsMaxPlusMatrix", [IsMaxPlusMatrix],
function(filter)
  return x -> IsInt(x) or x = -infinity;
end);

InstallMethod(\*, "for max-plus matrices", [IsMaxPlusMatrix, IsMaxPlusMatrix],
function(x, y)
  local n, xy, val, i, j, k;

  n := Length(x![1]);
  if n <> DimensionOfMatrixOverSemiring(y) then
    ErrorMayQuit("Semigroups: \* (for max-plus matrices): usage,\n",
                 "the arguments must be matrices of the same dimensions,");
  fi;
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
  return MatrixNC(x, xy);
end);

InstallMethod(OneImmutable, "for a max-plus matrix",
[IsMaxPlusMatrix],
x -> MatrixNC(x, SEMIGROUPS_IdentityMat(x, -infinity, 0)));

InstallMethod(RandomMatrixCons, "for IsMaxPlusMatrix and pos int",
[IsMaxPlusMatrix, IsPosInt],
function(filter, n)
  return MatrixNC(filter,
                  SEMIGROUPS_RandomIntegerMatrix(n, -infinity));
end);

InstallMethod(AsMatrixCons,
"for IsMaxPlusMatrix and a tropical max-plus matrix",
[IsMaxPlusMatrix, IsTropicalMaxPlusMatrix],
function(filter, mat)
  return MatrixNC(filter, AsList(mat));
end);

InstallMethod(AsMatrixCons,
"for IsMaxPlusMatrix and a tropical max-plus matrix",
[IsMaxPlusMatrix, IsProjectiveMaxPlusMatrix],
function(filter, mat)
  return MatrixNC(filter, AsList(mat));
end);

#############################################################################
## 2. Min-plus matrices
#############################################################################

InstallMethod(SEMIGROUPS_TypeViewStringOfMatrixOverSemiring,
"for a min-plus matrix",
[IsMinPlusMatrix], x -> "min-plus");

InstallMethod(SEMIGROUPS_FilterOfMatrixOverSemiring,
"for a min-plus matrix",
[IsMinPlusMatrix], x -> IsMinPlusMatrix);

InstallMethod(SEMIGROUPS_TypeOfMatrixOverSemiringCons, "for IsMinPlusMatrix",
[IsMinPlusMatrix], x -> MinPlusMatrixType);

InstallMethod(SEMIGROUPS_MatrixOverSemiringEntryCheckerCons,
"for IsMinPlusMatrix", [IsMinPlusMatrix],
function(filter)
  return x -> IsInt(x) or x = infinity;
end);

InstallMethod(\*, "for min-plus matrices", [IsMinPlusMatrix, IsMinPlusMatrix],
function(x, y)
  local n, xy, val, i, j, k;

  n := Length(x![1]);
  if n <> DimensionOfMatrixOverSemiring(y) then
    ErrorMayQuit("Semigroups: \* (for min-plus matrices): usage,\n",
                 "the arguments must be matrices of the same dimensions,");
  fi;
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
  return MatrixNC(x, xy);
end);

InstallMethod(OneImmutable, "for a min-plus matrix",
[IsMinPlusMatrix],
x -> MatrixNC(x, SEMIGROUPS_IdentityMat(x, infinity, 0)));

InstallMethod(RandomMatrixCons, "for IsMinPlusMatrix and pos int",
[IsMinPlusMatrix, IsPosInt],
function(filter, n)
  return MatrixNC(filter,
                  SEMIGROUPS_RandomIntegerMatrix(n, infinity));
end);

InstallMethod(AsMatrixCons,
"for IsMinPlusMatrix and a tropical min-plus matrix",
[IsMinPlusMatrix, IsTropicalMinPlusMatrix],
function(filter, mat)
  return MatrixNC(filter, AsList(mat));
end);

#############################################################################
## 3. Tropical matrices
#############################################################################

InstallMethod(ThresholdTropicalMatrix, "for a tropical matrix",
[IsTropicalMatrix], x -> x![Length(x![1]) + 1]);

InstallGlobalFunction(SEMIGROUPS_TropicalizeMat,
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

InstallMethod(SEMIGROUPS_TypeViewStringOfMatrixOverSemiring,
"for a tropical max-plus matrix",
[IsTropicalMaxPlusMatrix], x -> "tropical max-plus");

InstallMethod(SEMIGROUPS_FilterOfMatrixOverSemiring,
"for a tropical max-plus matrix",
[IsTropicalMaxPlusMatrix], x -> IsTropicalMaxPlusMatrix);

InstallMethod(SEMIGROUPS_TypeOfMatrixOverSemiringCons,
"for IsTropicalMaxPlusMatrix",
[IsTropicalMaxPlusMatrix], x -> TropicalMaxPlusMatrixType);

InstallMethod(SEMIGROUPS_MatrixOverSemiringEntryCheckerCons,
"for IsTropicalMaxPlusMatrix and pos int",
[IsTropicalMaxPlusMatrix, IsPosInt],
function(filter, threshold)
  return x -> (IsInt(x) and x >= 0 and x <= threshold) or x = -infinity;
end);

InstallMethod(\*, "for tropical max-plus matrices",
[IsTropicalMaxPlusMatrix, IsTropicalMaxPlusMatrix],
function(x, y)
  local n, threshold, xy, val, i, j, k;

  n := DimensionOfMatrixOverSemiring(x);
  threshold := ThresholdTropicalMatrix(x);
  if threshold <> ThresholdTropicalMatrix(y) then
    ErrorMayQuit("Semigroups: \* (for tropical max-plus matrices): usage,\n",
                 "the arguments do not have the same threshold,");
  elif n <> DimensionOfMatrixOverSemiring(y) then
    ErrorMayQuit("Semigroups: \* (for tropical max-plus matrices): usage,\n",
                 "the arguments must be matrices of the same dimensions,");
  fi;
  xy := List([1 .. n], x -> EmptyPlist(n));

  for i in [1 .. n] do
    for j in [1 .. n] do
      val := -infinity;
      for k in [1 .. n] do
        val := Maximum(val, SEMIGROUPS_PlusMinMax(x![i][k], y![k][j]));
      od;
      if val > threshold then
        val := threshold;
      fi;
      xy[i][j] := val;
    od;
  od;
  return MatrixNC(x, xy);
end);

InstallMethod(OneImmutable, "for a tropical max-plus matrix",
[IsTropicalMaxPlusMatrix],
x -> MatrixNC(x, SEMIGROUPS_IdentityMat(x, -infinity, 0)));

InstallMethod(RandomMatrixCons,
"for IsTropicalMaxPlusMatrix, pos int, pos int",
[IsTropicalMaxPlusMatrix, IsPosInt, IsPosInt],
function(filter, dim, threshold)
  local mat;
  mat := SEMIGROUPS_RandomIntegerMatrix(dim, -infinity);
  mat := SEMIGROUPS_TropicalizeMat(mat, threshold);
  return MatrixNC(filter, mat);
end);

InstallMethod(AsMatrixCons,
"for IsTropicalMaxPlusMatrix, max-plus matrix, and pos int",
[IsTropicalMaxPlusMatrix, IsMaxPlusMatrix, IsPosInt],
function(filter, mat, threshold)
  return MatrixNC(filter, SEMIGROUPS_TropicalizeMat(AsMutableList(mat),
                                                    threshold));
end);

InstallMethod(AsMatrixCons,
"for IsTropicalMaxPlusMatrix, projective max-plus matrix, and pos int",
[IsTropicalMaxPlusMatrix, IsProjectiveMaxPlusMatrix, IsPosInt],
function(filter, mat, threshold)
  return MatrixNC(filter, SEMIGROUPS_TropicalizeMat(AsMutableList(mat),
                                                    threshold));
end);

# change the threshold

InstallMethod(AsMatrixCons,
"for IsTropicalMaxPlusMatrix, max-plus matrix, and pos int",
[IsTropicalMaxPlusMatrix, IsTropicalMaxPlusMatrix, IsPosInt],
function(filter, mat, threshold)
  return MatrixNC(filter, SEMIGROUPS_TropicalizeMat(AsMutableList(mat),
                                                    threshold));
end);

#############################################################################
## 5. Tropical min-plus matrices
#############################################################################

InstallMethod(SEMIGROUPS_TypeViewStringOfMatrixOverSemiring,
"for a tropical min-plus matrix",
[IsTropicalMinPlusMatrix], x -> "tropical min-plus");

InstallMethod(SEMIGROUPS_FilterOfMatrixOverSemiring,
"for a tropical min-plus matrix",
[IsTropicalMinPlusMatrix], x -> IsTropicalMinPlusMatrix);

InstallMethod(SEMIGROUPS_TypeOfMatrixOverSemiringCons,
"for IsTropicalMinPlusMatrix",
[IsTropicalMinPlusMatrix], x -> TropicalMinPlusMatrixType);

InstallMethod(SEMIGROUPS_MatrixOverSemiringEntryCheckerCons,
"for IsTropicalMinPlusMatrix and pos int",
[IsTropicalMinPlusMatrix, IsPosInt],
function(filter, threshold)
  return x -> (IsInt(x) and x >= 0 and x <= threshold) or x = infinity;
end);

InstallMethod(\*, "for tropical min-plus matrices",
[IsTropicalMinPlusMatrix, IsTropicalMinPlusMatrix],
function(x, y)
  local n, threshold, xy, val, i, j, k;

  n := DimensionOfMatrixOverSemiring(x);
  threshold := ThresholdTropicalMatrix(x);

  if threshold <> ThresholdTropicalMatrix(y) then
    ErrorMayQuit("Semigroups: \* (for tropical min-plus matrices): usage,\n",
                 "the arguments do not have the same threshold,");
  elif n <> DimensionOfMatrixOverSemiring(y) then
    ErrorMayQuit("Semigroups: \* (for tropical min-plus matrices): usage,\n",
                 "the arguments must be matrices of the same dimensions,");
  fi;

  xy := List([1 .. n], x -> EmptyPlist(n));

  for i in [1 .. n] do
    for j in [1 .. n] do
      val := infinity;
      for k in [1 .. n] do
        val := Minimum(val, SEMIGROUPS_PlusMinMax(x![i][k], y![k][j]));
      od;
      if val <> infinity and val > threshold then
        val := threshold;
      fi;
      xy[i][j] := val;
    od;
  od;
  return MatrixNC(x, xy);
end);

InstallMethod(OneImmutable, "for a tropical min-plus matrix",
[IsTropicalMinPlusMatrix],
x -> MatrixNC(x, SEMIGROUPS_IdentityMat(x, infinity, 0)));

InstallMethod(RandomMatrixCons,
"for IsTropicalMinPlusMatrix, pos int, pos int",
[IsTropicalMinPlusMatrix, IsPosInt, IsPosInt],
function(filter, dim, threshold)
  local mat;
  mat := SEMIGROUPS_RandomIntegerMatrix(dim, infinity);
  mat := SEMIGROUPS_TropicalizeMat(mat, threshold);
  return MatrixNC(filter, mat);
end);

InstallMethod(AsMatrixCons,
"for IsTropicalMinPlusMatrix, min-plus matrix, and pos int",
[IsTropicalMinPlusMatrix, IsMinPlusMatrix, IsPosInt],
function(filter, mat, threshold)
  return MatrixNC(filter, SEMIGROUPS_TropicalizeMat(AsMutableList(mat),
                                                    threshold));
end);

# change the threshold

InstallMethod(AsMatrixCons,
"for IsTropicalMinPlusMatrix, min-plus matrix, and pos int",
[IsTropicalMinPlusMatrix, IsTropicalMinPlusMatrix, IsPosInt],
function(filter, mat, threshold)
  return MatrixNC(filter, SEMIGROUPS_TropicalizeMat(AsMutableList(mat),
                                                    threshold));
end);

#############################################################################
## 6. Projective max-plus matrices
#############################################################################

InstallMethod(SEMIGROUPS_TypeViewStringOfMatrixOverSemiring,
"for a projective max-plus matrix",
[IsProjectiveMaxPlusMatrix], x -> "projective max-plus");

InstallMethod(SEMIGROUPS_FilterOfMatrixOverSemiring,
"for a projective max-plus matrix",
[IsProjectiveMaxPlusMatrix], x -> IsProjectiveMaxPlusMatrix);

InstallMethod(SEMIGROUPS_TypeOfMatrixOverSemiringCons,
"for IsProjectiveMaxPlusMatrix",
[IsProjectiveMaxPlusMatrix], x -> ProjectiveMaxPlusMatrixType);

InstallMethod(SEMIGROUPS_MatrixOverSemiringEntryCheckerCons,
"for IsProjectiveMaxPlusMatrix",
[IsProjectiveMaxPlusMatrix],
function(filter)
  return x -> IsInt(x) or x = -infinity;
end);

InstallMethod(\*, "for projective max-plus matrices",
[IsProjectiveMaxPlusMatrix, IsProjectiveMaxPlusMatrix],
function(x, y)
  local n, xy, norm, val, i, j, k;

  n := DimensionOfMatrixOverSemiring(x);
  if n <> DimensionOfMatrixOverSemiring(y) then
    ErrorMayQuit("Semigroups: \* (for projective max-plus matrices): usage,\n",
                 "the arguments must be matrices of the same dimensions,");
  fi;
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
  return MatrixNC(x, xy);
end);

InstallMethod(OneImmutable, "for a projective max-plus matrix",
[IsProjectiveMaxPlusMatrix],
x -> MatrixNC(x, SEMIGROUPS_IdentityMat(x, -infinity, 0)));

InstallMethod(RandomMatrixCons, "for IsProjectiveMaxPlusMatrix and pos int",
[IsProjectiveMaxPlusMatrix, IsPosInt],
function(filter, n)
  return MatrixNC(filter,
                  SEMIGROUPS_RandomIntegerMatrix(n, -infinity));
end);

InstallMethod(AsMatrixCons,
"for IsProjectiveMaxPlusMatrix, max-plus matrix",
[IsProjectiveMaxPlusMatrix, IsMaxPlusMatrix],
function(filter, mat)
  return MatrixNC(filter, AsList(mat));
end);

InstallMethod(AsMatrixCons,
"for IsProjectiveMaxPlusMatrix, tropical max-plus matrix",
[IsProjectiveMaxPlusMatrix, IsTropicalMaxPlusMatrix],
function(filter, mat)
  return MatrixNC(filter, AsList(mat));
end);

#############################################################################
## 7. NTP matrices
#############################################################################

InstallMethod(ThresholdNTPMatrix, "for a natural matrix",
[IsNTPMatrix], x -> x![DimensionOfMatrixOverSemiring(x) + 1]);

InstallMethod(PeriodNTPMatrix, "for a natural matrix",
[IsNTPMatrix], x -> x![DimensionOfMatrixOverSemiring(x) + 2]);

InstallGlobalFunction(SEMIGROUPS_NaturalizeMat,
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

InstallMethod(SEMIGROUPS_TypeViewStringOfMatrixOverSemiring,
"for a natural number matrix",
[IsNTPMatrix], x -> "ntp");

InstallMethod(SEMIGROUPS_FilterOfMatrixOverSemiring,
"for a ntp matrix",
[IsNTPMatrix], x -> IsNTPMatrix);

InstallMethod(SEMIGROUPS_TypeOfMatrixOverSemiringCons, "for IsNTPMatrix",
[IsNTPMatrix], x -> NTPMatrixType);

InstallMethod(SEMIGROUPS_MatrixOverSemiringEntryCheckerCons,
"for IsNTPMatrix, pos int, pos int", [IsNTPMatrix, IsPosInt, IsPosInt],
function(filter, threshold, period)
  return x -> (IsInt(x) and x >= 0 and x <= threshold + period - 1);
end);

InstallMethod(\*, "for natural number matrices",
[IsNTPMatrix, IsNTPMatrix],
function(x, y)
  local n, period, threshold, xy, i, j, k;

  n := DimensionOfMatrixOverSemiring(x);
  period := PeriodNTPMatrix(x);
  threshold := ThresholdNTPMatrix(x);

  if period <> PeriodNTPMatrix(y) or threshold <> ThresholdNTPMatrix(y) then
    ErrorMayQuit("Semigroups: \* (for ntp matrices): usage,\n",
                 "the arguments must be matrices over the same semiring,");
  elif n <> DimensionOfMatrixOverSemiring(y) then
    ErrorMayQuit("Semigroups: \* (for ntp matrices): usage,\n",
                 "the arguments must be matrices of the same dimensions,");
  fi;

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
  return MatrixNC(x, xy);
end);

InstallMethod(OneImmutable, "for a ntp matrix",
[IsNTPMatrix],
x -> MatrixNC(x, SEMIGROUPS_IdentityMat(x, 0, 1)));

InstallMethod(RandomMatrixCons, "for IsNTPMatrix, pos int, pos int, pos int",
[IsNTPMatrix, IsPosInt, IsPosInt, IsPosInt],
function(filter, dim, threshold, period)
  local mat;
  mat := SEMIGROUPS_RandomIntegerMatrix(dim, false);
  mat := SEMIGROUPS_NaturalizeMat(mat, threshold, period);
  return MatrixNC(filter, mat);
end);

InstallMethod(AsMatrixCons,
"for IsNTPMatrix, ntp matrix, pos int, pos int",
[IsNTPMatrix, IsNTPMatrix, IsPosInt, IsPosInt],
function(filter, mat, threshold, period)
  return MatrixNC(filter, SEMIGROUPS_NaturalizeMat(AsMutableList(mat),
                                                   threshold,
                                                   period));
end);

InstallMethod(AsMatrixCons,
"for IsNTPMatrix, ntp matrix, pos int, pos int",
[IsNTPMatrix, IsIntegerMatrix, IsPosInt, IsPosInt],
function(filter, mat, threshold, period)
  return MatrixNC(filter, SEMIGROUPS_NaturalizeMat(AsMutableList(mat),
                                                   threshold,
                                                   period));
end);

#############################################################################
## 8. Integer matrices
#############################################################################

InstallMethod(SEMIGROUPS_TypeViewStringOfMatrixOverSemiring,
"for an integer matrix",
[IsIntegerMatrix], x -> "integer");

InstallMethod(SEMIGROUPS_FilterOfMatrixOverSemiring,
"for an integer matrix",
[IsIntegerMatrix], x -> IsIntegerMatrix);

InstallMethod(SEMIGROUPS_TypeOfMatrixOverSemiringCons, "for IsIntegerMatrix",
[IsIntegerMatrix], x -> IntegerMatrixType);

InstallMethod(SEMIGROUPS_MatrixOverSemiringEntryCheckerCons,
"for IsIntegerMatrix", [IsIntegerMatrix],
function(filter)
  return IsInt;
end);

InstallMethod(\*, "for integer matrices",
[IsIntegerMatrix, IsIntegerMatrix],
function(x, y)
  local n, xy, i, j, k;

  n := Length(x![1]);
  if n <> DimensionOfMatrixOverSemiring(y) then
    ErrorMayQuit("Semigroups: \* (for integer matrices): usage,\n",
                 "the arguments must be matrices of the same dimensions,");
  fi;
  xy := List([1 .. n], x -> EmptyPlist(n));

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

InstallMethod(OneImmutable, "for an integer matrix",
[IsIntegerMatrix],
x -> MatrixNC(x, SEMIGROUPS_IdentityMat(x, 0, 1)));

InstallMethod(RandomMatrixCons, "for IsIntegerMatrix and pos int",
[IsIntegerMatrix, IsPosInt],
function(filter, dim)
  return MatrixNC(filter, SEMIGROUPS_RandomIntegerMatrix(dim, false));
end);

InstallMethod(RandomMatrixOp, "for Integers and pos int",
[IsIntegers, IsPosInt],
function(semiring, n)
  return RandomMatrix(IsIntegerMatrix, n);
end);


InstallMethod(IsFinite,
"for a semigroup of matrices of positive integers",
[IsIntegerMatrixSemigroup],
function(s)
  local gens, gen, i, x, modgens, imagegen, image, idempots, a, b;

  gens := GeneratorsOfSemigroup(s);
  for gen in gens do
    for i in [1 .. Length(gen![1])] do
      for x in gen![i] do
        if x < 0 then
          TryNextMethod();
        fi;
      od;
    od;
  od;
  # FIXME use AsMatrix when available
  modgens := List(gens, x -> List(AsList(x), row -> List(row, x -> Minimum(x, 2))));
  imagegen := List(modgens, x -> Matrix(IsNTPMatrix, x, 1, 2));
  image := Semigroup(imagegen);
  idempots := Idempotents(image);

  for a in idempots do
    b := List([1 .. Length(a![1])], i -> a![i]);
    if b^2 <> b^3 then
      return false;
    fi;
  od;
  return true;
end);

InstallMethod(AsMatrixCons,
"for IsIntegerMatrix, ntp matrix",
[IsIntegerMatrix, IsNTPMatrix],
function(filter, mat)
  return MatrixNC(filter, AsList(mat));
end);
