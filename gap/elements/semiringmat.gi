############################################################################
##
#W  semiringmat.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains declarations for matrices over semirings.

# A matrix over semiring <mat> is:
#
#   mat![i] = the ith row
#
# it is also square, any additional data (like the threshold for tropical
# matrices), is contained in the positions from Length(mat![1]) + 1 onwards.

InstallMethod(MatrixNC, "for a type and homogeneous list",
[IsType, IsList],
function(type, mat)
  MakeImmutable(mat);
  return Objectify(type, mat);
end);

InstallMethod(MatrixNC, "for a filter and homogeneous list",
[IsOperation and IsFunction, IsList],
function(filter, mat)
  return MatrixNC(SEMIGROUPS_TypeOfMatrixOverSemiringCons(filter), mat);
end);

InstallMethod(MatrixNC, "for a filter, homogeneous list, function",
[IsOperation and IsFunction, IsList, IsFunction],
function(filter, mat, preproc)
  return MatrixNC(SEMIGROUPS_TypeOfMatrixOverSemiringCons(filter),
                  preproc(mat));
end);

InstallMethod(MatrixNC, "for a matrix over semiring and homogeneous list",
[IsMatrixOverSemiring, IsList and IsMutable],
function(sample, mat)
  local n, filter;

  # transfer whatever comes after the rows of the matrix, i.e. threshold,
  # period etc.
  n := Length(sample![1]) + 1;
  while IsBound(sample![n]) do
    mat[n] := sample![n];
    n := n + 1;
  od;

  # Cannot use TypeObj(sample) since it can contain information about
  # properties satisfied (or not) by x.
  filter := SEMIGROUPS_FilterOfMatrixOverSemiring(sample);
  return MatrixNC(SEMIGROUPS_TypeOfMatrixOverSemiringCons(filter), mat);
end);

InstallMethod(Matrix,
"for a filter, homogeneous list, pos int, and pos int",
[IsOperation and IsFunction, IsHomogeneousList, IsPosInt, IsPosInt],
function(filter, mat, threshold, period)
  local checker, row;

  if not IsRectangularTable(mat) or Length(mat) <> Length(mat[1]) then
    ErrorMayQuit("Semigroups: Matrix: usage,\n",
                 "the 1st argument must be a square table,");
  fi;

  if filter <> IsNTPMatrix then
    ErrorMayQuit("Semigroups: Matrix:\n",
                 "cannot create a matrix from the given ",
                 "arguments,");
  fi;

  checker := SEMIGROUPS_MatrixOverSemiringEntryCheckerCons(filter,
                                                           threshold,
                                                           period);
  for row in mat do
    if not ForAll(row, checker) then
      ErrorMayQuit("Semigroups: Matrix: usage,\n",
      "the entries in the 2nd argument do not define a matrix ",
      "of type ", NameFunction(filter), ",");
    fi;
  od;

  return MatrixNC(filter,
                  List(mat, ShallowCopy),
                  x -> SEMIGROUPS_NaturalizeMat(x, threshold, period));
end);

InstallMethod(Matrix,
"for a filter, homogeneous list, and pos int",
[IsOperation and IsFunction, IsHomogeneousList, IsPosInt],
function(filter, mat, threshold)
  local checker, row;

  if not IsRectangularTable(mat) or Length(mat) <> Length(mat[1]) then
    ErrorMayQuit("Semigroups: Matrix: usage,\n",
                 "the 1st argument must be a square table,");
  fi;

  if filter <> IsTropicalMaxPlusMatrix
      and filter <> IsTropicalMinPlusMatrix then
    ErrorMayQuit("Semigroups: Matrix:\n",
                 "cannot create a matrix from the given ",
                 "arguments,");
  fi;

  checker := SEMIGROUPS_MatrixOverSemiringEntryCheckerCons(filter,
                                                           threshold);
  for row in mat do
    if not ForAll(row, checker) then
      ErrorMayQuit("Semigroups: Matrix: usage,\n",
                   "the entries in the 2nd argument do not define a matrix ",
                   "of type ", NameFunction(filter), ",");
    fi;
  od;

  return MatrixNC(filter,
                  List(mat, ShallowCopy),
                  x -> SEMIGROUPS_TropicalizeMat(x, threshold));
end);

InstallMethod(Matrix, "for a filter and homogeneous list",
[IsFunction and IsOperation, IsHomogeneousList],
function(filter, mat)
  local checker, row;

  if not IsRectangularTable(mat) or Length(mat) <> Length(mat[1]) then
    ErrorMayQuit("Semigroups: Matrix: usage,\n",
                 "the 1st argument must be a square table,");
  fi;

  if not filter in [IsBooleanMat, IsMaxPlusMatrix, IsMinPlusMatrix,
                    IsProjectiveMaxPlusMatrix] then
    ErrorMayQuit("Semigroups: Matrix:\n",
                 "cannot create a matrix from the given ",
                 "arguments,");
  fi;

  if filter = IsBooleanMat then
    return BooleanMat(mat);
  fi;

  for row in mat do
    if not ForAll(row, SEMIGROUPS_MatrixOverSemiringEntryCheckerCons(filter)) then
      ErrorMayQuit("Semigroups: Matrix: usage,\n",
                   "the entries in the 2nd argument do not define a matrix ",
                   "of type ", NameFunction(filter), ",");
    fi;
  od;

  return MatrixNC(filter, List(mat, ShallowCopy));
end);

InstallMethod(Matrix, "for a semiring and homogeneous list",
[IsSemiring, IsHomogeneousList],
function(semiring, mat)
  local row;

  if not IsRectangularTable(mat) or Length(mat) <> Length(mat[1]) then
    ErrorMayQuit("Semigroups: Matrix: usage,\n",
                 "the 1st argument must be a square table,");
  fi;

  if not IsPrimeField(semiring) then
    ErrorMayQuit("Semigroups: Matrix:\n",
                 "cannot create a matrix from the given ",
                 "arguments,");
  fi;

  for row in mat do
    if not ForAll(row, x -> IsFFE(x) and x in semiring) then
      ErrorMayQuit("Semigroups: Matrix:\n",
                   "cannot create a matrix from the given ",
                   "arguments,");
    fi;
  od;
  mat := List(mat, ShallowCopy);
  Add(mat, Size(semiring));
  return MatrixNC(MatrixOverPrimeFieldType, mat);
end);

InstallGlobalFunction(RandomMatrix,
function(arg)
  if Length(arg) >= 2 and IsOperation(arg[1]) and IsFunction(arg[1])
      and IsPosInt(arg[2]) then
    if Length(arg) = 2 then
      return RandomMatrixCons(arg[1], arg[2]);
    elif Length(arg) >= 3 and IsPosInt(arg[3]) then
      if Length(arg) = 3 then
        return RandomMatrixCons(arg[1], arg[2], arg[3]);
      elif Length(arg) = 4 and IsPosInt(arg[4]) then
        return RandomMatrixCons(arg[1], arg[2], arg[3], arg[4]);
      fi;
    fi;
  elif Length(arg) = 2 and IsSemiring(arg[1]) and IsPosInt(arg[2]) then
    return RandomMatrixOp(arg[1], arg[2]);
  elif Length(arg) = 2 and IsPosInt(arg[1]) and IsPrimeInt(arg[1])
    and IsPosInt(arg[2]) then
    return RandomMatrixOp(GF(arg[1]), arg[2]);
  fi;

  ErrorMayQuit("Semigroups: RandomMatrix: usage,\n",
               "the arguments must be: filter, pos int[, pos int[,",
               " pos int]],");
end);

InstallMethod(TransposedMat, "for a matrix over semiring",
[IsMatrixOverSemiring],
function(x)
  local n, y, i, j;

  n := Length(x![1]);
  y := EmptyPlist(n + 2);
  for i in [1 .. n] do
    y[i] := [];
    for j in [1 .. n] do
      y[i][j] := x![j][i];
    od;
  od;

  return MatrixNC(x, y);
end);

InstallMethod(OneMutable, "for a matrix over semiring",
[IsMatrixOverSemiring], OneImmutable);

InstallMethod(IsGeneratorsOfInverseSemigroup,
"for a matrix over semiring coll",
[IsMatrixOverSemiringCollection], ReturnFalse);

InstallMethod(DimensionOfMatrixOverSemiring, "for a matrix over a semiring",
[IsMatrixOverSemiring], x -> Length(x![1]));

InstallMethod(Display, "for a matrix over semiring collection",
[IsMatrixOverSemiringCollection],
function(coll)
  Print(DisplayString(coll));
end);

InstallMethod(DisplayString, "for a matrix over semiring collection",
[IsMatrixOverSemiringCollection],
coll -> JoinStringsWithSeparator(List(coll, DisplayString), "\n"));

InstallMethod(DisplayString, "for a matrix over semiring collection",
[IsMatrixOverSemiring],
function(x)
  local n, max, length, pad, str, i, j;

  n := DimensionOfMatrixOverSemiring(x);

  # find the max max
  max := 0;
  for i in [1 .. n] do
    for j in [1 .. n] do
      if x![i][j] = infinity then
        length := 1;
      elif x![i][j] = -infinity then
        length := 2;
      else
        length := Length(String(x![i][j]));
      fi;
      if length > max then
        max := length;
      fi;
    od;
  od;

  pad := function(entry)
    if entry = infinity then
      entry := "∞";
    elif entry = -infinity then
      entry := "-∞";
    else
      entry := String(entry);
    fi;
    return Concatenation(ListWithIdenticalEntries(max - Length(entry), ' '),
                         entry, " ");
  end;

  str := "";
  for i in [1 .. n] do
    for j in [1 .. n] do
      Append(str, pad(x![i][j]));
    od;
    Remove(str, Length(str));
    Append(str, "\n");
  od;
  return str;
end);

InstallMethod(ViewString, "for a matrix over semiring", [IsMatrixOverSemiring],
function(x)
  local str;
  if DimensionOfMatrixOverSemiring(x) < 9 then
    return PrintString(x);
  fi;
  str := "<";
  Append(str, String(DimensionOfMatrixOverSemiring(x)));
  Append(str, "x");
  Append(str, String(DimensionOfMatrixOverSemiring(x)));
  Append(str, " ");
  Append(str, SEMIGROUPS_TypeViewStringOfMatrixOverSemiring(x));
  Append(str, " matrix>");
  return str;
end);

InstallMethod(PrintString, "for a matrix over semiring collection",
[IsMatrixOverSemiringCollection],
function(coll)
  local str, i;
  if IsGreensClass(coll) or IsSemigroup(coll) then
    TryNextMethod();
  fi;
  str := ShallowCopy(PrintString(coll[1]));
  for i in [2 .. Length(coll)]  do
    Append(str, "\>");
    Append(str, PrintString(coll[i]));
    Append(str, "\<, ");
  od;
  Remove(str, Length(str));
  Remove(str, Length(str));
  return str;
end);

InstallMethod(PrintString, "for a matrix over semiring",
[IsMatrixOverSemiring],
function(x)
  local n, str, i, j;

  n := DimensionOfMatrixOverSemiring(x);
  str := "\>\>Matrix(\<\>";
  if IsMatrixOverPrimeField(x) then
    Append(str, String(BaseField(x)));
  else
    Append(str, NameFunction(SEMIGROUPS_FilterOfMatrixOverSemiring(x)));
  fi;
  Append(str, "\<, \>[");
  for i in [1 .. n] do
    Append(str, "\>\>[");
    for j in [1 .. n] do
      if IsBooleanMat(x) then
        if x![i][j] then
          Append(str, String(1));
        else
          Append(str, String(0));
        fi;
      else
        Append(str, String(x![i][j]));
      fi;

      Append(str, ", ");
    od;
    Remove(str, Length(str));
    Remove(str, Length(str));
    Append(str, "]\<, \<");
  od;

  for i in [1 .. 4] do
    Remove(str, Length(str));
  od;
  Append(str, "\<\<]");

  #TODO remove this from here make it a SEMIGROUPS_ function
  if IsNTPMatrix(x) then
    Append(str, ", \>");
    Append(str, PrintString(ThresholdNTPMatrix(x)));
    Append(str, "\<");
    Append(str, ", \>");
    Append(str, PrintString(PeriodNTPMatrix(x)));
    Append(str, "\<");
  elif IsTropicalMatrix(x) then
    Append(str, ", \>");
    Append(str, PrintString(ThresholdTropicalMatrix(x)));
    Append(str, "\<");
  fi;
  Append(str, "\<)\<");

  return str;
end);

InstallMethod(\=, "for matrices over a semiring",
[IsMatrixOverSemiring, IsMatrixOverSemiring],
function(x, y)
  local n, i;

  n := Length(x![1]);
  if Length(y![1]) <> n then
    return false;
  fi;

  for i in [1 .. n] do
    if x![i] <> y![i] then
      return false;
    fi;
  od;
  return true;
end);

InstallMethod(\<, "for matrices over a semiring",
[IsMatrixOverSemiring, IsMatrixOverSemiring],
function(x, y)
  local n, i;

  n := Length(x![1]);
  if n < Length(y![1]) then
    return true;
  elif n > Length(y![1]) then
    return false;
  fi;

  for i in [1 .. n] do
    if x![i] < y![i] then
      return true;
    elif x![i] > y![i] then
      return false;
    fi;
  od;
  return false;
end);

InstallMethod(ChooseHashFunction, "for a matrix over semiring",
[IsMatrixOverSemiring, IsInt],
  function(x, hashlen)
  return rec(func := SEMIGROUPS_HashFunctionMatrixOverSemiring,
             data := hashlen);
end);

InstallGlobalFunction(SEMIGROUPS_HashFunctionMatrixOverSemiring,
function(x, data)
  local n, h, i, j;
  n := DimensionOfMatrixOverSemiring(x);
  h := 0;
  for i in [1 .. n] do
    for j in [1 .. n] do
      if x![i][j] <> infinity and x![i][j] <> -infinity then
        h := ((h / 4) + x![i][j]) mod data;
      fi;
    od;
  od;
  return h + 1;
end);

InstallMethod(OneMutable, "for a matrix over semiring",
[IsMatrixOverSemiring], OneImmutable);
