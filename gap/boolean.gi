############################################################################
##
#W  boolean.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains an implementation of boolean matrices.

# A boolean matrix <mat> is:
#   ![1] = dimension
#   ![(i - 1) * dim + j + 1] = mat[i][j] for all i > 1

# TODO
# 1) more conversion methods AsBooleanMat for a transformation, partial perm,
# etc
# 2) BooleanMat non-NC version
# 3) BooleanMat for a blist, and successors

InstallGlobalFunction(BooleanMatByIntRep, x -> Objectify(BooleanMatType, x));

InstallGlobalFunction(BooleanMatNC,
function(x)
  local out, row, col;

  out := [0];
  for row in x do
    out[1] := out[1] + 1;
    for col in row do
      Add(out, col);
    od;
  od;
  Objectify(BooleanMatType, out);
  return out;
end);

InstallMethod(DimensionOfBooleanMat, "for a boolean mat",
[IsBooleanMat], x -> x![1]);

InstallMethod(ViewString, "for a boolean mat", [IsBooleanMat],
function(x)
  local str;
  str := "<";
  Append(str, String(x![1]));
  Append(str, "x");
  Append(str, String(x![1]));
  Append(str, " boolean matrix>");

  return str;
end);

InstallMethod(PrintString, "for a boolean mat", [IsBooleanMat],
function(x)
  local str, i, j;
  str := "BooleanMat([";
  for i in [1 .. x![1]] do
    Append(str, "[");
    Append(str, String(x![(i - 1) * x![1] + 2]));
    for j in [3 .. x![1] + 1] do
      Append(str, ", ");
      Append(str, String(x![(i - 1) * x![1] + j]));
    od;
    Append(str, "], ");
  od;
  Remove(str, Length(str));
  Remove(str, Length(str));
  Append(str, "])");
  return str;
end);

InstallMethod(\=, "for a boolean mat", [IsBooleanMat, IsBooleanMat],
function(x, y)
  local i;

  for i in [1 .. x![1] ^ 2 + 1] do
    if x![i] <> y![i] then
      return false;
    fi;
  od;
  return true;
end);

InstallMethod(\<, "for a boolean mat", [IsBooleanMat, IsBooleanMat],
function(x, y)
  local i;

  for i in [1 .. x![1] ^ 2 + 1] do
    if x![i] < y![i] then
      return true;
    elif x![i] > y![i] then
      return false;
    fi;
  od;
  return false;
end);

InstallMethod(\*, "for a boolean mat", [IsBooleanMat, IsBooleanMat],
function(x, y)
  local xy, i, j, k;

  xy := [1 .. x![1] ^ 2 + 1] * 0;
  xy[1] := x![1];
  for i in [1 .. x![1]] do
    for j in [1 .. x![1]] do
      for k in [1 .. x![1]] do
        if x![(i - 1) * x![1] + k + 1] = 1
            and y![(k - 1) * y![1] + j + 1] = 1 then
          xy[(i - 1) * x![1] + j + 1] := 1;
          break;
        fi;
      od;
    od;
  od;
  return Objectify(BooleanMatType, xy);
end);

InstallMethod(OneImmutable, "for a boolean mat",
[IsBooleanMat],
function(x)
  local out, i;

  out := List([1 .. x![1]], y -> [1 .. x![1]] * 0);
  for i in [1 .. x![1]] do
    out[i][i] := 1;
  od;
  return BooleanMatNC(out);
end);

InstallMethod(RandomBooleanMat, "for a pos int", [IsPosInt],
function(n)
  local out, i;

  out := [n];
  for i in [1 .. n ^ 2] do
    Add(out, Random([0, 1]));
  od;
  return Objectify(BooleanMatType, out);
end);

InstallMethod(AsBooleanMat, "for a perm and pos int", [IsPerm, IsPosInt],
function(p, n)
  local out, i;
  if ForAny([1 .. n], i -> i ^ p > n) then
    Error("Semigroups: AsBooleanMat: usage\n",
          "the first arg <p> must be map ...");
  fi;

  out := List([1 .. n], y -> [1 .. n] * 0);
  for i in [1 .. n] do
    out[i][i ^ p] := 1;
  od;
  return BooleanMatNC(out);
end);

InstallMethod(ChooseHashFunction, "for a boolean matrix",
[IsBooleanMat, IsInt],
  function(x, hashlen)
  return rec(func := SEMIGROUPS_HashFunctionBooleanMat,
             data := hashlen);
end);

InstallGlobalFunction(SEMIGROUPS_HashFunctionBooleanMat,
function(x, data)
  local h, i;
  h := 0;
  for i in [2 .. x![1] ^ 2 + 1] do
    h := ((h / 2) + x![i]) mod data;
  od;
  return h + 1;
end);
