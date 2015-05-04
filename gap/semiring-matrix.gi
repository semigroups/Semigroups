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
#   mat![i] = the ith row, entries are integers or -infinity

InstallGlobalFunction(PlusMinMax,
function(x, y)
  if x = infinity or y = infinity then 
    return infinity;
  elif x = -infinity or y = -infinity then 
    return -infinity;
  fi;    
  return x + y;
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
  local n, str, i, j;

  n := DimensionOfMatrixOverSemiring(x);
  str := "";
  for i in [1 .. n] do
    for j in [1 .. n] do
      Append(str, String(x![i][j]));
      Append(str, " ");
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
      Append(str, String(x![i][j]));
      Append(str, ", ");
    od;
    Remove(str, Length(str));
    Remove(str, Length(str));
    Append(str, "]\<, \<");
  od;
  for i in [1 .. 4] do
    Remove(str, Length(str));
  od;
  Append(str, "\<\<]\<)\<");
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

# FIXME this is not general enough, either fix it or rename it

InstallMethod(SEMIGROUPS_RandomMatrixOverSemiring, "for a pos int", 
[IsPosInt, IsObject, IsObject],
function(n, source, type)
  local out, i, j;
  out := List([1 .. n], x -> EmptyPlist(n));
  for i in [1 .. n] do
    for j in [1 .. n] do
      out[i][j] := Random(Integers);
      if out[i][j] = 0 then 
        out[i][j] := source;
      elif out[i][j] < 0 then 
        out[i][j] := out[i][j] + 1;
      fi;
    od;
  od;
  return Objectify(type, out);
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
