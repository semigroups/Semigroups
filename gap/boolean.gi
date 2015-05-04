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
# 0) convert to blists for storage compression
# 1) more conversion methods AsBooleanMat for a transformation, partial perm,
# etc
# 2) BooleanMat non-NC version
# 3) BooleanMat for a blist, and successors

InstallMethod(TypeViewStringOfMatrixOverSemiring, "for a boolean matrix",
[IsBooleanMat], x -> "boolean");

InstallMethod(TypePrintStringOfMatrixOverSemiring, "for a boolean matrix",
[IsBooleanMat], x -> "BooleanNC");

InstallGlobalFunction(BooleanMatNC,
x -> Objectify(BooleanMatType, x));

InstallMethod(\*, "for boolean matrices", [IsBooleanMat, IsBooleanMat],
function(x, y)
  local n, xy, val, i, j, k;

  n := DimensionOfMatrixOverSemiring(x);
  xy := List([1 .. n], x -> BlistList([1 .. n], []));

  for i in [1 .. n] do
    for j in [1 .. n] do
      for k in [1 .. n] do
        if x![i][k] and y![k][j] then 
          xy![i][j] := true;
          break;
        fi;
      od;
    od;
  od;
  return BooleanMatNC(xy);
end);

InstallMethod(OneImmutable, "for a boolean mat",
[IsBooleanMat],
function(x)
  local n, id, i;

  n := DimensionOfMatrixOverSemiring(x);
  id := List([1 .. n], x -> BlistList([1 .. n], []));
  for i in [1 .. n] do
    id[i][i] := true;
  od;
  return BooleanMatNC(id);
end);

InstallMethod(OneMutable, "for a boolean mat",
[IsBooleanMat], OneImmutable);

InstallMethod(RandomBooleanMat, "for a pos int", [IsPosInt],
function(n)
  local x, i, j;

  n := DimensionOfMatrixOverSemiring(x);
  x := List([1 .. n], x -> BlistList([1 .. n], []));
  for i in [1 .. n] do
    for j in [1 .. n] do
      x[i][j] := Random([true, false]);
    od;
  od;
  return BooleanMatNC(x);
end);

InstallMethod(NumberBooleanMat, "for a boolean mat", [IsBooleanMat], 
function(x) 
  local n, nr, i, j;
  
  n := DimensionOfMatrixOverSemiring(x); 
  nr := 0; 
  for i in [1 .. n] do 
    for j in [1 .. n] do 
      nr := 2 * n + x![i][j]; 
    od;
  od; 
  return nr + 1;   # to be in [1 .. 2 ^ (n ^ 2)] 
end); 

InstallMethod(BooleanMatNumber,  
"for a positive integer and positive integer", 
[IsPosInt, IsPosInt],
function(nr, n) 
  local x, q, i, j;

  x := List([1 .. n], x -> BlistList([1 .. n], []));
  nr := nr - 1;   # to be in [0 .. 2 ^ (n ^ 2) - 1] 
  for i in [n, n - 1 .. 1] do
    for j in [n, n - 1 .. 1] do 
      q := nr mod 2;
      if q = 0 then 
        x[i][j] := false;
      else 
        x[i][j] := true;
      fi;
      nr := (nr - q) / 2;
    od;
  od; 
  return BooleanMatNC(x); 
end); 

InstallMethod(AsBooleanMat, "for a perm and pos int", [IsPerm, IsPosInt],
function(p, n)
  local out, i;
  if ForAny([1 .. n], i -> i ^ p > n) then
    Error("Semigroups: AsBooleanMat: usage,\n",
          "the permutation in the first argument must permute ",
          "[1 .. ", String(n), "],");
    return;
  fi;

  out := List([1 .. n], x -> BlistList([1 .. n], []));
  for i in [1 .. n] do
    out[i][i ^ p] := true;
  od;
  return BooleanMatNC(out);
end);

InstallMethod(AsBooleanMat, "for a transformation and pos int",
[IsTransformation, IsPosInt],
function(x, n)
  local out, i;
  if ForAny([1 .. n], i -> i ^ x > n) then
    Error("Semigroups: AsBooleanMat: usage,\n",
          "the transformation in the first argument must map ",
          "[1 .. ", String(n), "] to itself,");
    return;
  fi;

  out := List([1 .. n], x -> BlistList([1 .. n], []));
  for i in [1 .. n] do
    out[i][i ^ x] := true;
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
  local n, h, i, j;
  
  n := DimensionOfMatrixOverSemiring(x);
  h := 0;
  for i in [1 .. n] do
    for j in [1 .. n] do
      if x![i][j] then 
        h := ((h / 2) + 1) mod data;
      else
        h := (h / 2) mod data;
      fi;
    od;
  od;
  return h + 1;
end);
