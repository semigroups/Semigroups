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

# A boolean matrix <mat> is a positional object where mat![i] is the i-th row
# of the matrix, and it is a blist in blist rep.

#############################################################################
## Specializations of methods for MatrixOverSemiring
#############################################################################

InstallMethod(SEMIGROUPS_TypeViewStringOfMatrixOverSemiring, 
"for a boolean matrix",
[IsBooleanMat], x -> "boolean");

InstallMethod(SEMIGROUPS_TypePrintStringOfMatrixOverSemiring, 
"for a boolean matrix",
[IsBooleanMat], x -> "BooleanMat");

InstallGlobalFunction(BooleanMatNC,
function(x)
  MakeImmutable(x);
  return Objectify(BooleanMatType, x);
end);

InstallGlobalFunction(BooleanMat,
function(mat)
  local n, x, blist, i, j, row;

  if (not IsHomogeneousList(mat))
      or IsEmpty(mat)
      or not ForAll(mat, IsHomogeneousList) then
    ErrorMayQuit("Semigroups: BooleanMat: usage,\n",
                 "the argmuent must be a non-empty homogeneous list ",
                 "of homogeneous lists,\n");
  elif IsRectangularTable(mat) then #0s and 1s or blists
    if ForAll(mat, row -> ForAll(row, x -> x = 0 or x = 1)) then
      # 0s and 1s
      n := Length(mat[1]);
      x := EmptyPlist(n);
      for i in [1 .. n] do
        blist := BlistList([1 .. n], []);
        for j in [1 .. n] do
          if mat[i][j] = 1 then
            blist[j] := true;
          fi;
        od;
        Add(x, blist);
      od;
      return BooleanMatNC(x);
    elif ForAll(mat, row -> ForAll(row, x -> x = true or x = false)) then
      # blists
      x := ShallowCopy(mat);
      for row in x do
        if not IsBlistRep(row) then
          ConvertToBlistRep(row);
        fi;
      od;
      return BooleanMatNC(x);
    fi;
  fi;
  # successors
  n := Length(mat);
  x := EmptyPlist(n);
  for i in [1 .. n] do
    if not ForAll(mat[i], x -> IsPosInt(x) and x <= n) then
      ErrorMayQuit("Semigroups: BooleanMat:\n",
                   "the entries of each list must not exceed ", n, ",");
    fi;
    Add(x, BlistList([1 .. n], mat[i]));
  od;
  return BooleanMatNC(x);
end);

InstallMethod(\*, "for boolean matrices", [IsBooleanMat, IsBooleanMat],
function(x, y)
  local n, xy, i, j, k;

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

  x := List([1 .. n], x -> BlistList([1 .. n], []));
  for i in [1 .. n] do
    for j in [1 .. n] do
      x[i][j] := Random([true, false]);
    od;
  od;
  Perform(x, ConvertToBlistRep);
  return BooleanMatNC(x);
end);

#############################################################################
## Methods for Boolean matrices
#############################################################################

InstallMethod(\in, "for a boolean mat and boolean mat",
[IsBooleanMat, IsBooleanMat],
function(x, y)
  local n, i, j;

  n := Length(x![1]);

  if n <> Length(y![1]) then
    ErrorMayQuit("Semigroups: \in: usage,\n",
                 "the arguments <x> and <y> must be boolean matrix of equal ",
                 "size,");
  fi;

  for i in [1 .. n] do
    for j in [1 .. n] do
      if x![i][j] and not y![i][j] then
        return false;
      fi;
    od;
  od;

  return true;
end);

InstallGlobalFunction(OnBlists,
function(blist1, x)
  local n, blist2, i, j;

  n := Length(blist1);
  blist2 := BlistList([1 .. n], []);

  for i in [1 .. n] do
    for j in [1 .. n] do
      blist2[i] := blist2[i] or (blist1[j] and x![j][i]);
    od;
  od;
  return blist2;
end);

InstallMethod(Successors, "for a boolean matrix",
[IsBooleanMat],
function(x)
  local n;
  n := Length(x![1]);
  return List([1 .. n], i -> ListBlist([1 .. n], x![i]));
end);

InstallGlobalFunction(BooleanMatBySuccessorsNC,
function(x)
  local n, y, i;
  n := Length(x);
  y := EmptyPlist(n);
  for i in [1 .. n] do
    y[i] := BlistList([1 .. n], x[i]);
  od;
  return Objectify(BooleanMatType, y);
end);

InstallMethod(IsRowTrimBooleanMat, "for a boolean matrix",
[IsBooleanMat],
function(x)
  local n, i, j;

  n := Length(x![1]);
  for i in [1 .. n - 1] do
    for j in [i + 1 .. n] do
      if IsSubsetBlist(x![i], x![j]) then
        return false;
      fi;
    od;
  od;
  return true;
end);

InstallMethod(IsColTrimBooleanMat, "for a boolean matrix",
[IsBooleanMat], IS_COL_TRIM_BOOLEAN_MAT);

InstallMethod(IsTrimBooleanMat, "for a boolean matrix",
[IsBooleanMat], x -> IsRowTrimBooleanMat(x) and IsColTrimBooleanMat(x));

InstallMethod(DisplayString, "for a boolean matrix",
[IsBooleanMat],
function(x)
  local n, str, i, j;

  n := DimensionOfMatrixOverSemiring(x);
  str := "";
  for i in [1 .. n] do
    for j in [1 .. n] do
      if x![i][j] then
        Append(str, String(1));
      else
        Append(str, String(0));
      fi;
      Append(str, " ");
    od;
    Remove(str, Length(str));
    Append(str, "\n");
  od;
  return str;
end);

InstallMethod(NumberBooleanMat, "for a boolean mat", [IsBooleanMat],
function(x)
  local n, nr, i, j;

  n := DimensionOfMatrixOverSemiring(x);
  nr := 0;
  for i in [1 .. n] do
    for j in [1 .. n] do
      if x![i][j] then
        nr := 2 * nr + 1;
      else
        nr := 2 * nr;
      fi;
    od;
  od;
  return nr + 1;   # to be in [1 .. 2 ^ (n ^ 2)]
end);

InstallMethod(BooleanMatNumber,
"for a positive integer and positive integer",
[IsPosInt, IsPosInt],
function(nr, deg)
  local x, q, i, j;

  x := List([1 .. deg], x -> BlistList([1 .. deg], []));
  nr := nr - 1;   # to be in [0 .. 2 ^ (deg ^ 2) - 1]
  for i in [deg, deg - 1 .. 1] do
    for j in [deg, deg - 1 .. 1] do
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

InstallGlobalFunction(NumberBlist,
function(blist)
  local n, nr, i;
  n := Length(blist);
  nr := 0;
  for i in [1 .. n] do
    if blist[i] then
      nr := 2 * nr + 1;
    else
      nr := 2 * nr;
    fi;
  od;
  return nr + 1;   # to be in [1 .. 2 ^ n]
end);

InstallGlobalFunction(BlistNumber,
function(nr, n)
  local x, q, i;

  x := BlistList([1 .. n], []);
  nr := nr - 1;   # to be in [0 .. 2 ^ n - 1]
  for i in [n, n - 1 .. 1] do
    q := nr mod 2;
    if q = 0 then
      x[i] := false;
    else
      x[i] := true;
    fi;
    nr := (nr - q) / 2;
  od;
  return x;
end);

InstallMethod(AsBooleanMat, "for a partitioned binary relation",
[IsPBR], x -> AsBooleanMat(x, 2 * DegreeOfPBR(x)));

InstallMethod(AsBooleanMat, "for a partitioned binary relation and pos int",
[IsPBR, IsPosInt],
function(x, n)
  local y, i;

  if n < 2 * x![1] then
    ErrorMayQuit("Semigroups: AsBooleanMat: usage,\n",
                 "the pbr in the first argument is defined on more than ",
                 String(n), " points,");
  fi;

  y := EmptyPlist(n);
  for i in [2 ..  2 * x![1] + 1] do
    Add(y, BlistList([1 .. n], x![i]));
  od;
  for i in [2 * x![1] + 1 .. n] do
    Add(y, BlistList([1 .. n], []));
  od;
  return BooleanMat(y);
end);

InstallMethod(AsBooleanMat, "for a bipartition",
[IsBipartition], x -> AsBooleanMat(x, DegreeOfBipartition(x)));

InstallMethod(AsBooleanMat, "for a bipartition and pos int",
[IsBipartition, IsPosInt],
function(x, n)
  local deg, blocks, out, i, block;

  deg := DegreeOfBipartition(x);
  if 2 * n < deg then
    ErrorMayQuit("Semigroups: AsBooleanMat: usage,\n",
                 "the bipartition in the first argument must have degree ",
                 "at least ", String(2 * n), ",");
  fi;

  blocks := ShallowCopy(ExtRepOfBipartition(x));
  out := EmptyPlist(2 * n);

  for block in blocks do
    block := ShallowCopy(block);
    for i in [1 .. Length(block)] do
      if block[i] < 0 then
        block[i] := -block[i] + deg;
      fi;
    od;
    for i in block do
      out[i] := BlistList([1 .. 2 * n], block);
    od;
  od;

  return BooleanMat(out);
end);

InstallMethod(AsBooleanMat, "for a transformation",
[IsTransformation], x -> AsBooleanMat(x, DegreeOfTransformation(x)));

InstallMethod(AsBooleanMat, "for a transformation and pos int",
[IsTransformation, IsPosInt],
function(x, n)
  local out, i;
  if ForAny([1 .. n], i -> i ^ x > n) then
    ErrorMayQuit("Semigroups: AsBooleanMat: usage,\n",
                 "the transformation in the first argument must map ",
                 "[1 .. ", String(n), "] to itself,");
  fi;

  out := List([1 .. n], x -> BlistList([1 .. n], []));
  for i in [1 .. n] do
    out[i][i ^ x] := true;
  od;
  return BooleanMatNC(out);
end);

InstallMethod(AsBooleanMat, "for a perm",
[IsPerm], x -> AsBooleanMat(AsTransformation(x)));

InstallMethod(AsBooleanMat, "for a perm",
[IsPerm, IsPosInt],
function(x, n)
  return AsBooleanMat(AsTransformation(x), n);
end);

InstallMethod(AsBooleanMat, "for a partial perm",
[IsPartialPerm],
x -> AsBooleanMat(x, Maximum(DegreeOfPartialPerm(x),
                             CodegreeOfPartialPerm(x))));

InstallMethod(AsBooleanMat, "for a transformation and pos int",
[IsPartialPerm, IsPosInt],
function(x, n)
  local out, j, i;

  if ForAny([1 .. n], i -> i ^ x > n) then
    ErrorMayQuit("Semigroups: AsBooleanMat: usage,\n",
                 "the partial perm in the first argument must map ",
                 "[1 .. ", String(n), "] into itself,");
  fi;

  out := List([1 .. n], x -> BlistList([1 .. n], []));
  for i in [1 .. n] do
    j := i ^ x;
    if j <> 0 then
      out[i][j] := true;
    fi;
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

  n := Length(x![1]);
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

InstallMethod(SetBooleanMat, "for a boolean matrix",
[IsBooleanMat],
function(x)
  local n, out, i;
  n := Length(x![1]);
  out := EmptyPlist(n);
  for i in [1 .. n] do
    out[i] := NumberBlist(x![i]) + (i - 1) * 2 ^ n;
  od;
  return out;
end);

InstallMethod(BooleanMatSet, "for a homogeneous set",
[IsHomogeneousList and IsSSortedList],
function(set)
  local n, x, i;
  n := Length(set);
  x := EmptyPlist(n);
  for i in [1 .. n] do
    x[i] := BlistNumber(set[i] - (i - 1) * 2 ^ n, n);
  od;
  return BooleanMatNC(x);
end);

InstallMethod(CanonicalBooleanMat, "for boolean mat",
[IsBooleanMat],
function(x)
  local n;
  n := Length(x![1]);
  return CanonicalBooleanMatNC(SymmetricGroup(n), SymmetricGroup(n), x);
end);

InstallMethod(CanonicalBooleanMat, "for perm group and boolean mat",
[IsPermGroup, IsBooleanMat],
function(G, x)
  return CanonicalBooleanMatNC(G, G, x);
end);

InstallMethod(CanonicalBooleanMat, "for perm group and boolean mat",
[IsPermGroup, IsPermGroup, IsBooleanMat],
function(G, H, x)
  local n;
  n := Length(x![1]);
  if LargestMovedPoint(G) > n or LargestMovedPoint(H) > n then
    ErrorMayQuit("Semigroups: CanonicalBooleanMat: usage,\n",
                 "the largest moved point of the first argument must not",
                 " exceed the dimension of the Boolean matrix,");
  fi;
  return CanonicalBooleanMatNC(G, H, x);
end);

if IsGrapeLoaded then
  InstallMethod(CanonicalBooleanMatNC,
  "for perm group, perm group, and boolean mat",
  [IsPermGroup, IsPermGroup, IsBooleanMat],
  function(G, H, x)
    local n, V, phi, act, map;
    n := Length(x![1]);
    V := DirectProduct(G, H);
    phi := Projection(V, 2);

    act := function(pt, p)
      local q, r, nr;
      pt := pt - 1;
      q := QuoInt(pt, 2 ^ n); #row
      r := pt - q * 2 ^ n; # number blist of the row
      # permute columns
      nr := NumberBlist(Permuted(BlistNumber(r + 1, n), p));
      return nr + ((q + 1) ^ (p ^ phi) - 1) * 2 ^ n; # and then the row
    end;

     map := ActionHomomorphism(V, [1 .. n * 2 ^ n], act);
     return BooleanMatSet(SmallestImageSet(Image(map), SetBooleanMat(x)));
  end);
fi;

InstallMethod(IsSymmetricBooleanMat, "for a boolean matrix",
[IsBooleanMat],
function(x)
  local n, i, j;
  n := Length(x![1]);
  for i in [1 .. n - 1] do
    for j in [i + 1 .. n] do
      if x![i][j] <> x![j][i] then
        return false;
      fi;
    od;
  od;
  return true;
end);

InstallMethod(IsReflexiveBooleanMat, "for a boolean matrix",
[IsBooleanMat],
function(x)
  local n, i;
  n := Length(x![1]);
  for i in [1 .. n] do
    if not x![i][i] then
      return false;
    fi;
  od;
  return true;
end);

InstallMethod(IsTransitiveBooleanMat, "for a boolean matrix",
[IsBooleanMat],
function(x)
  local n, i, j, k;
  n := [1 .. Length(x![1])];
  for i in n do
    for j in n do
      for k in n do
        if x![i][k] and x![k][j] and not x![i][j] then
          return false;
        fi;
      od;
    od;
  od;
  return true;
end);

InstallMethod(IsAntiSymmetricBooleanMat, "for a boolean matrix",
[IsBooleanMat],
function(x)
  local n, i, j;
  n := Length(x![1]);
  for i in [1 .. n - 1] do
    for j in [i + 1 .. n] do
      if x![i][j] and x![j][i] then
        return false;
      fi;
    od;
  od;
  return true;
end);
