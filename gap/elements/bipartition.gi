############################################################################
##
#W  bipartition.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#############################################################################
# Pickler
#############################################################################

InstallMethod(IO_Pickle, "for a bipartition",
[IsFile, IsBipartition],
function(file, x)
  if IO_Write(file, "BIPA") = fail then
    return IO_Error;
  fi;
  if IO_Pickle(file, BIPART_INT_REP(x)) = IO_Error then
    return IO_Error;
  fi;
  return IO_OK;
end);

IO_Unpicklers.BIPA := function(file)
  local blocks;

  blocks := IO_Unpickle(file);
  if blocks = IO_Error then
    return IO_Error;
  fi;
  return BIPART_NC(blocks);
end;

#############################################################################
# Implications
#############################################################################

InstallTrueMethod(IsPermBipartition, IsTransBipartition
                                     and IsDualTransBipartition);

InstallTrueMethod(IsBlockBijection, IsPermBipartition);

#############################################################################
# GAP level - directly using interface to C/C++ level
#############################################################################

# Fundamental properties

InstallMethod(DegreeOfBipartition, "for a bipartition",
[IsBipartition], BIPART_DEGREE);

InstallMethod(NrBlocks, "for a bipartition",
[IsBipartition], BIPART_NR_BLOCKS);

InstallMethod(NrLeftBlocks, "for a bipartition",
[IsBipartition], BIPART_NR_LEFT_BLOCKS);

InstallMethod(RankOfBipartition, "for a bipartition",
[IsBipartition], BIPART_RANK);

# Constructors

InstallGlobalFunction(Bipartition,
function(classes)
  local n, copy, i, j;

  if not IsList(classes)
      or ForAny(classes, x -> not IsHomogeneousList(x)
                                  or not IsDuplicateFree(x)) then
    ErrorMayQuit("Semigroups: Bipartition: usage,\n",
                 "the argument <classes> must consist of duplicate-free ",
                 "homogeneous lists,");
  fi;

  if not ForAll(classes, x -> ForAll(x, i -> IsPosInt(i) or IsNegInt(i))) then
    ErrorMayQuit("Semigroups: Bipartition: usage,\n",
                 "the argument <classes> must consist of positive and/or ",
                 "negative integers,\n");
  fi;

  copy := Union(classes);
  if not IsEmpty(classes)
      and (copy <> Concatenation([Minimum(copy) .. -1], [1 .. Maximum(copy)])
           or Minimum(copy) > 0) then
    ErrorMayQuit("Semigroups: Bipartition: usage,\n",
                 "the union of the argument <classes> must be ",
                 "[-n..-1, 1..n],");
  fi;

  n := Sum(List(classes, Length)) / 2;
  copy := List(classes, ShallowCopy);
  for i in [1 .. Length(copy)] do
    for j in [1 .. Length(copy[i])] do
      if copy[i][j] < 0 then
        copy[i][j] := AbsInt(copy[i][j]) + n;
      fi;
    od;
  od;

  Perform(copy, Sort);
  Sort(copy);

  for i in [1 .. Length(copy)] do
    for j in [1 .. Length(copy[i])] do
      if copy[i][j] > n then
        copy[i][j] := -copy[i][j] + n;
      fi;
    od;
  od;
  return BIPART_NC(copy);
end);

# TODO roll into Bipartition?

InstallMethod(BipartitionByIntRep, "for a list", [IsList],
function(blocks)
  local n, next, seen, nrleft, out, i;

  n := Length(blocks);

  if not IsEvenInt(n) then
    ErrorMayQuit("Semigroups: BipartitionByIntRep: usage,\n",
                 "the length of the argument <blocks> must be an even ",
                 "integer,");
  fi;

  n := n / 2;
  if not ForAll(blocks, IsPosInt) then
    ErrorMayQuit("Semigroups: BipartitionByIntRep: usage,\n",
                 "the elements of the argument <blocks> must be positive ",
                 "integers,");
  fi;

  next := 0;
  seen := BlistList([1 .. 2 * Maximum(blocks)], []);

  for i in [1 .. n] do
    if not seen[blocks[i]] then
      next := next + 1;
      if blocks[i] <> next then
        ErrorMayQuit("Semigroups: BipartitionByIntRep: usage,\n",
                     "expected ", next, " but found ", blocks[i],
                     ", in position ", i);
      fi;
      seen[blocks[i]] := true;
    fi;
  od;

  nrleft := next;

  for i in [n + 1 .. 2 * n] do
    if not seen[blocks[i]] then
      next := next + 1;
      if blocks[i] <> next then
        ErrorMayQuit("Semigroups: BipartitionByIntRep: usage,\n",
                     "expected ", next, " but found ", blocks[i],
                     ", in position ", i);
      fi;
      seen[blocks[i]] := true;
    fi;
  od;

  out := BIPART_NC(blocks);

  SetDegreeOfBipartition(out, n);
  SetNrLeftBlocks(out, nrleft);
  SetNrBlocks(out, next);
  return out;
end);

InstallMethod(IdentityBipartition, "for a positive integer", [IsPosInt],
function(n)
  local blocks, out, i;

  blocks := EmptyPlist(2 * n);
  for i in [1 .. n] do
    blocks[i] := i;
    blocks[i + n] := i;
  od;

  out := BIPART_NC(blocks);
  SetDegreeOfBipartition(out, n);
  SetRankOfBipartition(out, n);
  SetNrLeftBlocks(out, n);
  SetNrBlocks(out, n);

  return out;
end);

InstallMethod(RandomBipartition, "for a pos int", [IsPosInt],
function(n)
  local out, nrblocks, vals, j, i;

  out := EmptyPlist(2 * n);
  nrblocks := 0;
  vals := [1];

  for i in [1 .. 2 * n] do
    j := Random(vals);
    if j = nrblocks + 1 then
      nrblocks := nrblocks + 1;
      Add(vals, nrblocks + 1);
    fi;
    out[i] := j;
  od;

  return BIPART_NC(out);
end);

InstallMethod(RandomBlockBijection, "for a pos int", [IsPosInt],
function(n)
  local out, nrblocks, j, free, i;

  out := EmptyPlist(2 * n);
  out[1] := 1;
  nrblocks := 1;

  for i in [2 .. n] do
    j := Random([1 .. nrblocks + 1]);
    if j = nrblocks + 1 then
      nrblocks := nrblocks + 1;
    fi;
    out[i] := j;
  od;

  free := [n + 1 .. 2 * n];
  for i in [1 .. nrblocks] do
    j := Random(free);
    out[j] := i;
    RemoveSet(free, j);
  od;

  for i in free do
    out[i] := Random([1 .. nrblocks]);
  od;

  out := BIPART_NC(out);
  SetDegreeOfBipartition(out, n);
  SetNrLeftBlocks(out, nrblocks);
  SetNrBlocks(out, nrblocks);
  SetNrRightBlocks(out, nrblocks);

  return out;
end);

# Operators

InstallMethod(\*, "for a bipartition and bipartition",
[IsBipartition, IsBipartition], BIPART_PROD);

InstallMethod(\<, "for a bipartition and bipartition",
[IsBipartition, IsBipartition], BIPART_LT);

InstallMethod(\=, "for a bipartition and bipartition",
[IsBipartition, IsBipartition], BIPART_EQ);

InstallMethod(PermLeftQuoBipartition, "for a bipartition and bipartition",
[IsBipartition, IsBipartition],
function(x, y)

  if LeftBlocks(x) <> LeftBlocks(y) or RightBlocks(x) <> RightBlocks(y) then
    ErrorMayQuit("Semigroups: PermLeftQuoBipartition: usage,\n",
                 "the arguments must have equal left and right blocks,");
  fi;
  return BIPART_PERM_LEFT_QUO(x, y);
end);

# Attributes

# xx ^ * - linear - 2 * degree

InstallMethod(LeftProjection, "for a bipartition", [IsBipartition],
BIPART_LEFT_PROJ);

# linear - 2 * degree

InstallMethod(StarOp, "for a bipartition", [IsBipartition], BIPART_STAR);

# Changing representation . . .

#############################################################################
# GAP level
#############################################################################

# Attributes

# not a synonym since NrTransverseBlocks also applies to blocks

InstallMethod(NrTransverseBlocks, "for a bipartition", [IsBipartition],
RankOfBipartition);

InstallMethod(NrRightBlocks, "for a bipartition", [IsBipartition],
x -> NrBlocks(x) - NrLeftBlocks(x) + NrTransverseBlocks(x));

InstallMethod(OneMutable, "for a bipartition",
[IsBipartition], x -> IdentityBipartition(DegreeOfBipartition(x)));

InstallMethod(OneMutable, "for a bipartition collection",
[IsBipartitionCollection], x ->
IdentityBipartition(DegreeOfBipartitionCollection(x)));

# the Other is to avoid warning on opening GAP

InstallOtherMethod(InverseMutable, "for a bipartition", [IsBipartition],
function(x)
  if IsBlockBijection(x) or IsPartialPermBipartition(x) then
    return Star(x);
  fi;
  return fail;
end);

# Properties

InstallMethod(IsBlockBijection, "for a bipartition",
[IsBipartition],
function(x)
  return NrBlocks(x) = NrLeftBlocks(x) and NrRightBlocks(x) = NrLeftBlocks(x);
end);

InstallMethod(IsPartialPermBipartition, "for a bipartition",
[IsBipartition],
function(x)
  return NrLeftBlocks(x) = DegreeOfBipartition(x)
    and NrRightBlocks(x) = DegreeOfBipartition(x);
end);

# a bipartition is a transformation if and only if the second row is a
# permutation of [1 .. n], where n is the degree.

InstallMethod(IsTransBipartition, "for a bipartition",
[IsBipartition],
function(x)
  return NrLeftBlocks(x) = NrTransverseBlocks(x)
   and NrRightBlocks(x) = DegreeOfBipartition(x);
end);

InstallMethod(IsDualTransBipartition, "for a bipartition", [IsBipartition],
function(x)
  return NrRightBlocks(x) = NrTransverseBlocks(x)
   and NrLeftBlocks(x) = DegreeOfBipartition(x);
end);

InstallMethod(IsPermBipartition, "for a bipartition",
[IsBipartition],
function(x)
  return IsPartialPermBipartition(x)
    and NrTransverseBlocks(x) = DegreeOfBipartition(x);
end);

# Fundamental operators

InstallMethod(\*, "for a bipartition and a perm",
[IsBipartition, IsPerm],
function(f, g)
  if LargestMovedPoint(g) <= DegreeOfBipartition(f) then
    return f * AsBipartition(g, DegreeOfBipartition(f));
  fi;
  ErrorMayQuit("Semigroups: \* (for a bipartition and perm): usage,\n",
               "the largest moved point of the perm must not be greater\n",
               "than the degree of the bipartition,");
end);

InstallMethod(\*, "for a perm and a bipartition",
[IsPerm, IsBipartition],
function(f, g)
  if LargestMovedPoint(f) <= DegreeOfBipartition(g) then
    return AsBipartition(f, DegreeOfBipartition(g)) * g;
  fi;
  ErrorMayQuit("Semigroups: \* (for a perm and bipartition): usage,\n",
               "the largest moved point of the perm must not be greater\n",
               "than the degree of the bipartition,");
end);

InstallMethod(\*, "for a bipartition and a transformation",
[IsBipartition, IsTransformation],
function(f, g)
  if DegreeOfTransformation(g) <= DegreeOfBipartition(f) then
    return f * AsBipartition(g, DegreeOfBipartition(f));
  fi;
  ErrorMayQuit("Semigroups: \* (for a bipartition and transformation): ",
               "usage,\n",
               "the degree of the transformation must not be greater\n",
               "than the degree of the bipartition,");
end);

InstallMethod(\*, "for a transformation and a bipartition",
[IsTransformation, IsBipartition],
function(f, g)
  if DegreeOfTransformation(f) <= DegreeOfBipartition(g) then
    return AsBipartition(f, DegreeOfBipartition(g)) * g;
  fi;
  ErrorMayQuit("Semigroups: \* (for a transformation and bipartition): ",
               "usage,\n",
               "the degree of the transformation must not be greater\n",
               "than the degree of the bipartition,");
end);

InstallMethod(\*, "for a bipartition and a partial perm",
[IsBipartition, IsPartialPerm],
function(f, g)
  local n;
  n := DegreeOfBipartition(f);
  if ForAll([1 .. n], i -> i ^ g <= n) then
    return f * AsBipartition(g, DegreeOfBipartition(f));
  fi;
  ErrorMayQuit("Semigroups: \* (for a bipartition and partial perm): usage,\n",
               "the partial perm must map [1 .. ", String(n), "] into\n",
               "[1 .. ", String(n), "],");
end);

InstallMethod(\*, "for a partial perm and a bipartition",
[IsPartialPerm, IsBipartition],
function(f, g)
  local n;
  n := DegreeOfBipartition(g);
  if ForAll([1 .. n], i -> i ^ f <= n) then
    return AsBipartition(f, DegreeOfBipartition(g)) * g;
  fi;
  ErrorMayQuit("Semigroups: \* (for a partial perm and a bipartition): ",
               "usage,\n",
               "the partial perm must map [1 .. ", String(n), "] into\n",
               "[1 .. ", String(n), "],");
end);

InstallMethod(\^, "for a bipartition and permutation",
[IsBipartition, IsPerm],
function(f, p)
  return p ^ -1 * f * p;
end);

# Other operators

InstallMethod(PartialPermLeqBipartition, "for a bipartition and a bipartition",
[IsBipartition, IsBipartition],
function(x, y)

  if not (IsPartialPermBipartition(x) and IsPartialPermBipartition(y)) then
    ErrorMayQuit("Semigroups: PartialPermLeqBipartition: usage,\n",
                 "the arguments must be partial perm bipartitions,");
  elif DegreeOfBipartition(x) <> DegreeOfBipartition(y) then
    ErrorMayQuit("Semigroups: PartialPermLeqBipartition: usage,\n",
                 "the arguments must have equal degree,");
  fi;

  return AsPartialPerm(x) < AsPartialPerm(y);
end);

# Changing representations

InstallMethod(AsBipartition, "for a permutation and zero",
[IsPerm, IsZeroCyc],
function(f, n)
  return Bipartition([]);
end);

InstallMethod(AsBipartition, "for a permutation",
[IsPerm], x -> AsBipartition(x, LargestMovedPoint(x)));

InstallMethod(AsBipartition, "for a partial perm",
[IsPartialPerm],
function(x)
  return AsBipartition(x, Maximum(DegreeOfPartialPerm(x),
                                  CodegreeOfPartialPerm(x)));
end);

InstallMethod(AsBipartition, "for a partial perm and zero",
[IsPartialPerm, IsZeroCyc],
function(f, n)
  return Bipartition([]);
end);

InstallMethod(AsBipartition, "for a transformation",
[IsTransformation], x -> AsBipartition(x, DegreeOfTransformation(x)));

InstallMethod(AsBipartition, "for a transformation and zero",
[IsTransformation, IsZeroCyc],
function(f, n)
  return Bipartition([]);
end);

InstallMethod(AsBipartition, "for a bipartition", [IsBipartition], IdFunc);

InstallMethod(AsBipartition, "for a bipartition", [IsBipartition, IsZeroCyc],
function(f, n)
  return Bipartition([]);
end);

InstallMethod(AsBipartition, "for a pbr and pos int",
[IsPBR, IsZeroCyc],
function(x, deg)
  return Bipartition([]);
end);

InstallMethod(AsBipartition, "for a pbr and pos int",
[IsPBR, IsPosInt],
function(x, deg)
  if not IsBipartitionPBR(x) then
    ErrorMayQuit("Semigroups: AsBipartition (for a pbr): usage,\n",
                 "the argument does not satisfy 'IsBipartitionPBR',");
  fi;

  return AsBipartition(AsBipartition(x), deg);
end);

InstallMethod(AsBipartition, "for a pbr",
[IsPBR],
function(x)
  if not IsBipartitionPBR(x) then
    ErrorMayQuit("Semigroups: AsBipartition (for a pbr): usage,\n",
                 "the argument does not satisfy 'IsBipartitionPBR',");
  fi;
  return Bipartition(Union(ExtRepOfPBR(x)));
end);

InstallMethod(AsBlockBijection, "for a partial perm",
[IsPartialPerm],
function(x)
  return AsBlockBijection(x, Maximum(DegreeOfPartialPerm(x),
                                     CodegreeOfPartialPerm(x)) + 1);
end);

InstallMethod(AsBlockBijection, "for a partial perm and zero",
[IsPartialPerm, IsZeroCyc],
function(x, n)
  return Bipartition([]);
end);

# Viewing, printing etc

InstallMethod(ViewString, "for a bipartition",
[IsBipartition],
function(x)
  local str, ext, i;

  if DegreeOfBipartition(x) = 0 then
    return "\><empty bipartition>\<";
  fi;

  if IsBlockBijection(x) then
    str := "\>\><block bijection:\< ";
  else
    str := "\>\><bipartition:\< ";
  fi;

  ext := BIPART_EXT_REP(x);
  Append(str, "\>");
  Append(str, String(ext[1]));
  Append(str, "\<");

  for i in [2 .. Length(ext)] do
    Append(str, ", \>");
    Append(str, String(ext[i]));
    Append(str, "\<");
  od;
  Append(str, ">\<");
  return str;
end);

InstallMethod(PrintString, "for a bipartition",
[IsBipartition],
function(x)
  local ext, str, i;
  ext := BIPART_EXT_REP(x);
  str := Concatenation("\>\>Bipartition(\< \>[ ", PrintString(ext[1]));
  for i in [2 .. Length(ext)] do
    Append(str, ",\< \>");
    Append(str, PrintString(ext[i]));
  od;
  Append(str, " \<]");
  Append(str, " )\<");
  return str;
end);

InstallMethod(PrintString, "for a bipartition collection",
[IsBipartitionCollection],
function(coll)
  local str, i;

  if IsGreensClass(coll) or IsSemigroup(coll) then
    TryNextMethod();
  fi;

  str := "\>[ ";
  for i in [1 .. Length(coll)] do
    if not i = 1 then
      Append(str, " ");
    fi;
    Append(str, "\>");
    Append(str, PrintString(coll[i]));
    if not i = Length(coll) then
      Append(str, ",\<\n");
    else
      Append(str, " ]\<\n");
    fi;
  od;
  return str;
end);

# Bipartition collections

InstallMethod(DegreeOfBipartitionCollection, "for a bipartition collection",
[IsBipartitionCollection],
function(coll)
  local deg;

  if IsBipartitionSemigroup(coll) then
    return DegreeOfBipartitionSemigroup(coll);
  fi;

  deg := DegreeOfBipartition(coll[1]);
  if not ForAll(coll, x -> DegreeOfBipartition(x) = deg) then
    ErrorMayQuit("Semigroups: DegreeOfBipartitionCollection: usage,\n",
                 "the argument <coll> must be a collection of bipartitions ",
                 "of equal degree,");
  fi;

  return deg;
end);

#############################################################################
# C/C++ level
#############################################################################

#InstallMethod(LeftBlocks, "for a bipartition", [IsBipartition],
#function(f)
#  local n, blocks, tab, out, nrblocks, i;
#
#  n := DegreeOfBipartition(f);
#  blocks := f!.blocks;
#  tab := List([1 .. n], x -> false);
#  out := EmptyPlist(n + 2);
#  out[1] := 0;
#  out[n + 2] := [];
#  nrblocks := 0;
#
#  for i in [1 .. n] do
#    out[i + 1] := blocks[i];
#    if not tab[blocks[i]] then
#      out[1] := out[1] + 1;
#      out[n + 1 + blocks[i]] := 0;
#      tab[blocks[i]] := true;
#    fi;
#  od;
#
#  for i in [n + 1 .. 2 * n] do
#    if blocks[i] <= out[1] then #transverse block!
#      out[n + 1 + blocks[i]] := 1;
#    fi;
#  od;
#
#  out := Objectify(BlocksType, rec(blocks := out));
#  return out;
#end);
#
#InstallMethod(RightBlocks, "for a bipartition", [IsBipartition],
#function(f)
#  local n, blocks, tab, out, nrblocks, i;
#
#  n := DegreeOfBipartition(f);
#  blocks := f!.blocks;
#  tab := EmptyPlist(2 * n);
#  out := [];
#  nrblocks := 0;
#
#  for i in [n + 1 .. 2 * n] do
#    if not IsBound(tab[blocks[i]]) then
#      nrblocks := nrblocks + 1;
#      tab[blocks[i]] := nrblocks;
#      if blocks[i] <= NrLeftBlocks(f) then
#        out[n + 1 + nrblocks] := 1; #signed
#      else
#        out[n + 1 + nrblocks] := 0; #unsigned
#      fi;
#    fi;
#    out[i - n + 1] := tab[blocks[i]];
#  od;
#  out[1] := nrblocks;
#  out := Objectify(BlocksType, rec(blocks := out));
#  return out;
#end);

# Operators

# Constructors



# linear - 2 * degree

#InstallMethod(RightProjection, "for a bipartition",
#[IsBipartition],
#function(f)
#  local n, blocks, table, out, next, nrleft, lookup, i;
#
#  n := DegreeOfBipartition(f);
#  blocks := f!.blocks;
#  table := [];
#  out := [];
#  next := 0;
#
#  for i in [1 .. n] do
#    if IsBound(table[blocks[i + n]]) then
#      out[i] := table[blocks[i + n]];
#    else
#      next := next + 1;
#      table[blocks[i + n]] := next;
#      out[i] := next;
#    fi;
#  od;
#
#  nrleft := next;
#  table := [];
#  lookup := SEMIGROUPS.TransBlocksLookup(f);
#
#  for i in [1 .. n] do
#    if blocks[i + n] <= NrLeftBlocks(f) and lookup[blocks[i + n]] then
#      out[i + n] := out[i];
#    elif IsBound(table[blocks[i + n]]) then
#      out[i + n] := table[blocks[i + n]];
#    else
#      next := next + 1;
#      table[blocks[i + n]] := next;
#      out[i + n] := next;
#    fi;
#  od;
#
#  out := Objectify(BipartitionType, rec(blocks := out));
#
#  SetDegreeOfBipartition(out, n);
#  SetNrLeftBlocks(out, nrleft);
#  SetNrBlocks(out, next);
#  return out;
#end);

#############################################################################
# All of the methods in this section could be done in C/C++
#############################################################################

# Change representations . . .

InstallMethod(AsBipartition, "for a permutation and pos int",
[IsPerm, IsPosInt],
function(x, n)
  if OnSets([1 .. n], x) <> [1 .. n] then
    ErrorMayQuit("Semigroups: AsBipartition (xor a permutation and pos int):",
                 "\nthe permutation <p> in the 1st argument must permute ",
                 "[1 .. ", String(n), "],");
  fi;
  return BIPART_NC(Concatenation([1 .. n], ListPerm(x ^ -1, n)));
end);

InstallMethod(AsPartialPerm, "for a bipartition", [IsBipartition],
function(x)
  local n, blocks, nrleft, im, out, i;

  if not IsPartialPermBipartition(x) then
    ErrorMayQuit("Semigroups: AsPartialPerm (for a bipartition):\n",
                 "the argument does not define a partial perm,");
  fi;

  n      := DegreeOfBipartition(x);
  blocks := BIPART_INT_REP(x);
  nrleft := NrLeftBlocks(x);
  im     := [1 .. n] * 0;

  for i in [n + 1 .. 2 * n] do
    if blocks[i] <= nrleft then
      im[blocks[i]] := i - n;
    fi;
  od;

  out := EmptyPlist(n);
  for i in [1 .. n] do
    out[i] := im[blocks[i]];
  od;
  return PartialPermNC(out);
end);

InstallMethod(AsPermutation, "for a bipartition", [IsBipartition],
function(x)
  local n, blocks, im, out, i;

  if not IsPermBipartition(x) then
    ErrorMayQuit("Semigroups: AsPermutation (for a bipartition):\n",
                 "the argument does not define a permutation,");
  fi;

  n      := DegreeOfBipartition(x);
  blocks := BIPART_INT_REP(x);
  im     := EmptyPlist(n);

  for i in [n + 1 .. 2 * n] do
    im[blocks[i]] := i - n;
  od;

  out := EmptyPlist(n);
  for i in [1 .. n] do
    out[i] := im[blocks[i]];
  od;
  return PermList(out);
end);

InstallMethod(AsTransformation, "for a bipartition", [IsBipartition],
function(x)
  local n, blocks, nr, im, out, i;

  if not IsTransBipartition(x) then
    ErrorMayQuit("Semigroups: AsTransformation (for a bipartition):\n",
                 "the argument does not define a transformation,");
  fi;

  n      := DegreeOfBipartition(x);
  blocks := BIPART_INT_REP(x);
  nr     := NrLeftBlocks(x);
  im     := EmptyPlist(n);

  for i in [n + 1 .. 2 * n] do
    if blocks[i] <= nr then
      im[blocks[i]] := i - n;
    fi;
  od;

  out := EmptyPlist(n);
  for i in [1 .. n] do
    out[i] := im[blocks[i]];
  od;
  return TransformationNC(out);
end);

InstallMethod(AsBipartition, "for a partial perm and pos int",
[IsPartialPerm, IsPosInt],
function(x, n)
  local r, out, j, i;

  r := n;
  out := EmptyPlist(2 * n);

  for i in [1 .. n] do
    out[i] := i;
    j := PreImagePartialPerm(x, i);
    if j <> fail then
      out[n + i] := j;
    else
      r := r + 1;
      out[n + i] := r;
    fi;
  od;
  out := BIPART_NC(out);
  SetIsPartialPermBipartition(out, true);
  return out;
end);

InstallMethod(AsBipartition, "for a transformation and a positive integer",
[IsTransformation, IsPosInt],
function(f, n)
  local r, ker, out, g, i;

  if n < DegreeOfTransformation(f) then
    #verify <f> is a transformation on [1..n]
    for i in [1 .. n] do
      if i ^ f > n then
        ErrorMayQuit("Semigroups: AsBipartition (for a transformation and ",
                     "pos int):\n",
                     "the argument must map [1 .. ", String(n), "] to ",
                     "itself,");
      fi;
    od;
  fi;

  r := RankOfTransformation(f, n);
  ker := FlatKernelOfTransformation(f, n);

  out := EmptyPlist(2 * n);
  g := List([1 .. n], x -> 0);

  #inverse of f
  for i in [1 .. n] do
    g[i ^ f] := i;
  od;

  for i in [1 .. n] do
    out[i] := ker[i];
    if g[i] <> 0 then
      out[n + i] := ker[g[i]];
    else
      r := r + 1;
      out[n + i] := r;
    fi;
  od;
  out := BIPART_NC(out);
  SetIsTransBipartition(out, true);
  return out;
end);

InstallMethod(AsBipartition, "for a bipartition and pos int",
[IsBipartition, IsPosInt],
function(f, n)
  local deg, blocks, out, nrblocks, nrleft, lookup, j, i;

  deg := DegreeOfBipartition(f);
  if n = deg then
    return f;
  fi;
  blocks := f!.blocks;
  out := [];
  nrblocks := 0;

  if n < deg then
    for i in [1 .. n] do
      out[i] := blocks[i];
      if out[i] > nrblocks then
        nrblocks := nrblocks + 1;
      fi;
    od;
    nrleft := nrblocks;
    lookup := EmptyPlist(NrBlocks(f));
    for i in [n + 1 .. 2 * n] do
      j := blocks[i + deg - n];
      if j > nrleft then
        if not IsBound(lookup[j]) then
          nrblocks := nrblocks + 1;
          lookup[j] := nrblocks;
        fi;
        j := lookup[j];
      fi;
      out[i] := j;
    od;
  else # n>deg
    for i in [1 .. deg] do
      out[i] := blocks[i];
    od;
    nrblocks := NrLeftBlocks(f);
    for i in [deg + 1 .. n] do
      nrblocks := nrblocks + 1;
      out[i] := nrblocks;
    od;
    nrleft := nrblocks; # = n - deg + NrLeftBlocks(f)
    for i in [n + 1 .. n + deg] do
      if blocks[i - n + deg] <= nrleft - n + deg then #it's a left block
        out[i] := blocks[i - n + deg];
      else
        out[i] := blocks[i - n + deg] + n - deg;
      fi;
    od;
    nrblocks := NrBlocks(f) + n - deg;
    for i in [n + deg + 1 .. 2 * n] do
      nrblocks := nrblocks + 1;
      out[i] := nrblocks;
    od;
  fi;
  out := BIPART_NC(out);
  SetDegreeOfBipartition(out, n);
  SetNrBlocks(out, nrblocks);
  SetNrLeftBlocks(out, nrleft);
  return out;
end);

# same as AsBipartition except that all undefined points are in a single block
# together with an extra (pair of) points.

InstallMethod(AsBlockBijection, "for a partial perm and pos int",
[IsPartialPerm, IsPosInt],
function(f, n)
  local bigblock, nr, out, i;

  if n <= Maximum(DegreeOfPartialPerm(f), CodegreeOfPartialPerm(f)) then
    ErrorMayQuit("Semigroups: AsBlockBijection (for a partial perm and pos ",
                 "int):\n",
                 "the 2nd argument must be at least the maximum of the ",
                 "degree and\ncodegree of the 1st argument,");
  fi;

  nr := 0;
  out := [1 .. 2 * n] * 0;
  bigblock := n;

  for i in [1 .. n - 1] do
    if i ^ f = 0 then
      if bigblock = n then
        nr := nr + 1;
        bigblock := nr;
      fi;
      out[i] := bigblock;
    else
      nr := nr + 1;
      out[i] := nr;
      out[n + i ^ f] := nr;
    fi;
  od;

  out[n] := bigblock;
  out[2 * n] := bigblock;

  for i in [n + 1 .. 2 * n - 1] do
    if out[i] = 0 then
      out[i] := bigblock;
    fi;
  od;

  out := BIPART_NC(out);
  SetIsBlockBijection(out, true);
  return out;
end);

InstallMethod(NaturalLeqBlockBijection, "for bipartitions",
[IsBipartition, IsBipartition],
function(x, y)
  local xblocks, yblocks, n, lookup, i;

  if not IsBlockBijection(x) or not IsBlockBijection(y) then
    ErrorMayQuit("Semigroups: NaturalLeqBlockBijection: usage,\n",
                 "the arguments must be block bijections,");
  elif DegreeOfBipartition(x) <> DegreeOfBipartition(y) then
    ErrorMayQuit("Semigroups: NaturalLeqBlockBijection: usage,\n",
                 "the arguments must be block bijections of equal degree,");
  elif NrBlocks(x) > NrBlocks(y) then
    return false;
  fi;

  xblocks := BIPART_INT_REP(x);
  yblocks := BIPART_INT_REP(y);
  n       := DegreeOfBipartition(x);

  lookup := [];
  for i in [1 .. n] do
    if IsBound(lookup[yblocks[i]]) and lookup[yblocks[i]] <> xblocks[i] then
      return false;
    else
      lookup[yblocks[i]] := xblocks[i];
    fi;
  od;
  for i in [n + 1 .. 2 * n] do
    if lookup[yblocks[i]] <> xblocks[i] then
      return false;
    fi;
  od;
  return true;
end);

InstallMethod(NaturalLeqPartialPermBipartition, "for bipartitions",
[IsBipartition, IsBipartition],
function(x, y)
  local n, xblocks, yblocks, val, i;

  if not IsPartialPermBipartition(x) or not IsPartialPermBipartition(y) then
    ErrorMayQuit("Semigroups: NaturalLeqPartialPermBipartition: usage,\n",
                 "the arguments must be partial perm bipartitions,");
  fi;

  n := DegreeOfBipartition(x);

  if n <> DegreeOfBipartition(y) then
    ErrorMayQuit("Semigroups: NaturalLeqPartialPermBipartition: usage,\n",
                 "the arguments must have equal degree,");
  fi;

  xblocks := BIPART_INT_REP(x);
  yblocks := BIPART_INT_REP(y);

  for i in [n + 1 .. 2 * n] do
    val := xblocks[i];
    if val <= n and val <> yblocks[i] then
      return false;
    fi;
  od;
  return true;
end);

InstallMethod(IsUniformBlockBijection, "for a bipartition",
[IsBipartition],
function(x)
  local blocks, n, sizesleft, sizesright, i;

  if not IsBlockBijection(x) then
    return false;
  fi;

  blocks := BIPART_EXT_REP(x);
  n := DegreeOfBipartition(x);
  sizesleft := [1 .. NrBlocks(x)] * 0;
  sizesright := [1 .. NrBlocks(x)] * 0;

  for i in [1 .. n] do
    sizesleft[blocks[i]] := sizesleft[blocks[i]] + 1;
  od;
  for i in [n + 1 .. 2 * n] do
    sizesright[blocks[i]] := sizesright[blocks[i]] + 1;
  od;
  for i in [1 .. NrBlocks(x)] do
    if sizesright[i] <> sizesleft[i] then
      return false;
    fi;
  od;

  return true;
end);
