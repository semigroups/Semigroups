############################################################################
##
#W  semiring-matrix.gi
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

InstallMethod(TransposedMat, "for a matrix over semiring",
[IsMatrixOverSemiring],
function(x)
  local n, y, i, j;

  n := Length(x![1]);
  y := EmptyPlist(2 * n);
  for i in [1 .. n] do 
    y[i] := [];
    for j in [1 .. n] do 
      y[i][j] := x![j][i];
    od;
  od;

  if IsBound(x![n + 1]) then 
    y[n + 1] := x![n + 1];
    if IsBound(x![n + 2]) then 
      y[n + 2] := x![n + 2];
    fi;
  fi;
  return Objectify(TypeObj(x), y);
end);

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
  str := "<";
  Append(str, String(DimensionOfMatrixOverSemiring(x)));
  Append(str, "x");
  Append(str, String(DimensionOfMatrixOverSemiring(x)));
  Append(str, " ");
  Append(str, TypeViewStringOfMatrixOverSemiring(x));
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
  str := Concatenation("\>", TypePrintStringOfMatrixOverSemiring(x), "(\>[");
  
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

  if IsNaturalMatrix(x) then 
    Append(str, "\>");
    Append(str, PrintString(ThresholdNaturalMatrix(x)));
    Append(str, "\<");
    Append(str, ", \>");
    Append(str, PrintString(PeriodNaturalMatrix(x)));
    Append(str, "\<");
  elif IsTropicalMatrix(x) then 
    Append(str, ", \>");
    Append(str, PrintString(ThresholdTropicalMatrix(x)));
    Append(str, "\<");
  elif IsMatrixOverPrimeField(x) then 
    Append(str, ", \>");
    Append(str, String(BaseField(x)));
    Append(str, "\<");
  fi;
  Append(str, "\<)\<");
  
  return str;
end);

InstallMethod(\=, "for matrices over a semiring",
[IsMatrixOverSemiring, IsMatrixOverSemiring],
function(x, y)
  local n, i, j;

  n := DimensionOfMatrixOverSemiring(x);

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
  local n, i, j;

  n := DimensionOfMatrixOverSemiring(x);

  for i in [1 .. n] do
    if x![i] < y![i] then
      return true;
    elif x![i] > y![i] then
      return false;
    fi;
  od;
  return false;
end);

#

InstallMethod(SEMIGROUPS_RandomMatrixOverSemiring, 
"for a pos int, object, object",
[IsPosInt, IsObject, IsObject],
function(n, source, constructor)
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
  return constructor(out);
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
