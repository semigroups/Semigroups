############################################################################
##
#W  blocks.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#############################################################################
# Blocks are stored internally as a list consisting of:
#
# [ nr of blocks, internal rep of blocks, transverse blocks ]
#
# <nr of blocks> is a non-negative integer, <internal rep of blocks>[i] = j if
# <i> belongs to the <j>th block, <transverse blocks>[j] = 1 if block <j> is
# transverse and 0 if it is not.
#############################################################################

#############################################################################
# GAP level - directly using interface to C/C++ level
#############################################################################

InstallMethod(LeftBlocks, "for a bipartition", [IsBipartition],
BIPART_LEFT_BLOCKS);

InstallMethod(RightBlocks, "for a bipartition", [IsBipartition],
BIPART_RIGHT_BLOCKS);

InstallMethod(DegreeOfBlocks, "for blocks", [IsBlocks], BLOCKS_DEGREE);

InstallMethod(ELM_LIST, "for blocks", [IsBlocks, IsPosInt], BLOCKS_ELM_LIST);

InstallMethod(NrBlocks, "for blocks", [IsBlocks], BLOCKS_NR_BLOCKS);

#############################################################################
# GAP level - NOT directly using interface to C/C++ level
#############################################################################

# TODO C/C++

InstallMethod(RankOfBlocks, "for blocks", [IsBlocks],
function(blocks)
  return SizeBlist(blocks!.blist);
end);

# FIXME
BindGlobal("EmptyBlocks", Objectify(BlocksType, rec(blocks := [0])));

InstallMethod(ExtRepOfBlocks, "for blocks", [IsBlocks],
function(blocks)
  local n, lookup, ext, i;

  n := DegreeOfBlocks(blocks);
  lookup := blocks!.blist;
  ext := [];

  for i in [1 .. n] do
    if not IsBound(ext[blocks[i]]) then
      ext[blocks[i]] := [];
    fi;
    if lookup[blocks[i]] then
      Add(ext[blocks[i]], i);
    else
      Add(ext[blocks[i]], -i);
    fi;
  od;
  return ext;
end);

# not a synonym since NrTransverseBlocks applies to a bipartition also
InstallMethod(NrTransverseBlocks, "for blocks", [IsBlocks], RankOfBlocks);

# TODO C/C++?

InstallMethod(ProjectionFromBlocks, "for blocks", [IsBlocks],
function(blocks)
  local deg, blist, nr_blocks, out, lookup, i;

  deg       := DegreeOfBlocks(blocks);
  blist     := blocks!.blist;
  nr_blocks := NrBlocks(blocks);             #nr of blocks
  blocks    := blocks!.blocks;
  out       := [];
  lookup    := [];

  for i in [1 .. deg] do
    out[i] := blocks[i];
    if blist[blocks[i]] then
      out[i + deg] := blocks[i];
    else
      if not IsBound(lookup[blocks[i]]) then
        nr_blocks := nr_blocks + 1;
        lookup[blocks[i]] := nr_blocks;
      fi;
      out[i + deg] := lookup[blocks[i]];
    fi;
  od;
  return BIPART_NC(out);
end);

# Operators . . .

# TODO C/C++?

InstallMethod(\=, "for blocks", [IsBlocks, IsBlocks],
function(a, b)
  return a!.blocks = b!.blocks;
end);

# TODO C/C++?

InstallMethod(\<, "for blocks", [IsBlocks, IsBlocks],
function(a, b)
  return a!.blocks < b!.blocks;
end);

# Printing, viewing etc . . .

InstallMethod(PrintObj, "for blocks", [IsBlocks], 10,
function(blocks)
  Print(PrintString(blocks));
  return;
end);

InstallMethod(PrintString, "for blocks", [IsBlocks],
x -> Concatenation("BlocksNC(", String(ExtRepOfBlocks(x)), ")"));

InstallMethod(ViewObj, "for blocks", [IsBlocks],
function(blocks)
  local ext, i;

  ext := ExtRepOfBlocks(blocks);
  if Length(ext) > 0 then
    Print("<blocks: ");
    Print(ext[1]);
    for i in [2 .. Length(ext)] do
      Print(", ", ext[i]);
    od;
  else
    Print("<empty blocks");
  fi;

  Print(">");
  return;
end);

#############################################################################
# Not yet processed . . .
#############################################################################


#############################################################################
# Internal
#############################################################################

SEMIGROUPS.HashFunctionForBlocks := function(blocks, data)
  return ORB_HashFunctionForPlainFlatList(blocks!.blocks, data);
end;

SEMIGROUPS.BlocksIdempotentTester := function(lambda, rho)
  local n, lambdanr, rhonr, fuse, fuseit, sign, x, y, seen, i;

  if DegreeOfBlocks(lambda) <> DegreeOfBlocks(rho) then
    ErrorMayQuit("Semigroups: SEMIGROUPS.BlocksIdempotentTester: usage,\n",
                 "the degrees of the blocks <lambda> and <rho> must be equal,");
  fi;

  if RankOfBlocks(lambda) <> RankOfBlocks(rho) then
    return false;
  fi;

  if RankOfBlocks(lambda) = 0 then
    return true;
  fi;

  n := DegreeOfBlocks(lambda);
  lambdanr := NrBlocks(lambda);
  rhonr := NrBlocks(rho);

  fuse := [1 .. lambdanr + rhonr];
  fuseit := function(i)
    while fuse[i] < i do
      i := fuse[i];
    od;
    return i;
  end;

  sign := [1 .. lambdanr] * 0;
  for i in [lambdanr + 1 .. lambdanr + rhonr] do #copy the signs from <rho>
    sign[i] := rho[n + i - lambdanr];
  od;

  for i in [1 .. n] do
    x := fuseit(lambda[i]);
    y := fuseit(rho[i] + lambdanr);
    if x <> y then
      if x < y then
        fuse[y] := x;
        if sign[y] = 1 then
          sign[x] := 1;
        fi;
      else
        fuse[x] := y;
        if sign[x] = 1 then
          sign[y] := 1;
        fi;
      fi;
    fi;
  od;

  #check if we are injective on signed classes of <lambda> and that the fused
  #blocks are also signed.

  seen := BlistList([1 .. lambdanr], []);
  for i in [1 .. lambdanr] do
    if lambda[n + i] = 1 then # is block <i> a signed block?
      x := fuseit(i);
      if seen[x] or sign[x] = 0 then
        return false;
      fi;
      seen[x] := true;
    fi;
  od;
  return true;
end;

# assumes that SEMIGROUPS.BlocksIdempotentTester returns true!

SEMIGROUPS.BlocksIdempotentCreator := function(lambda, rho)
  local n, lambdanr, rhonr, fuse, fuseit, x, y, tab1, tab2, out, next, i;

  n := DegreeOfBlocks(lambda);
  lambdanr := NrBlocks(lambda);
  rhonr := NrBlocks(rho);

  fuse := [1 .. lambdanr + rhonr];
  fuseit := function(i)
    while fuse[i] < i do
      i := fuse[i];
    od;
    return i;
  end;

  for i in [1 .. n] do
    x := fuseit(lambda[i]);
    y := fuseit(rho[i] + lambdanr);
    if x <> y then
      if x < y then
        fuse[y] := x;
      else
        fuse[x] := y;
      fi;
    fi;
  od;

  tab1 := EmptyPlist(lambdanr);

  #find new names for the signed blocks of rho
  for i in [1 .. rhonr] do
    if rho[n + i] = 1 then
      tab1[fuseit(i + lambdanr)] := i;
    fi;
  od;

  tab2 := EmptyPlist(lambdanr);
  out := EmptyPlist(2 * n);
  next := rhonr;

  for i in [1 .. n] do
    out[i] := rho[i];
    if lambda[n + lambda[i]] = 1 then
      out[i + n] := tab1[fuseit(lambda[i])];
    else
      if not IsBound(tab2[lambda[i]]) then
        next := next + 1;
        tab2[lambda[i]] := next;
      fi;
      out[i + n] := tab2[lambda[i]];
    fi;
  od;

  out := Objectify(BipartitionType, rec(blocks := out));
  SetDegreeOfBipartition(out, n);
  SetRankOfBipartition(out, RankOfBlocks(rho));
  SetNrLeftBlocks(out, rhonr);
  SetNrBlocks(out, next);
  return out;
end;

# fuse <blocks> with <f>. <sign> should be true to keep track of signed and
# unsigned blocks and false not to keep track.

BindGlobal("FuseRightBlocks",
function(blocks, f, sign)
  local n, fblocks, nrblocks, nrfblocks, fuse, fuseit, x, y, i;

  n := DegreeOfBlocks(blocks);
  fblocks := f!.blocks;
  nrblocks := NrBlocks(blocks);
  nrfblocks := NrBlocks(f);

  fuse := [1 .. nrblocks + nrfblocks];
  fuseit := function(i)
    while fuse[i] < i do
      i := fuse[i];
    od;
    return i;
  end;

  if sign then
    sign := EmptyPlist(nrfblocks + nrblocks);

    for i in [1 .. nrblocks] do
      sign[i] := blocks[n + i];
    od;
    for i in [nrblocks + 1 .. nrfblocks + nrblocks] do
      sign[i] := 0;
    od;

    for i in [1 .. n] do
      x := fuseit(blocks[i]);
      y := fuseit(fblocks[i] + nrblocks);
      if x <> y then
        if x < y then
          fuse[y] := x;
          if sign[y] = 1 then
            sign[x] := 1;
          fi;
        else
          fuse[x] := y;
          if sign[x] = 1 then
            sign[y] := 1;
          fi;
        fi;
      fi;
    od;
    return [fuseit, sign];
  else
    for i in [1 .. n] do
      x := fuseit(blocks[i]);
      y := fuseit(fblocks[i] + nrblocks);
      if x <> y then
        if x < y then
          fuse[y] := x;
        else
          fuse[x] := y;
        fi;
      fi;
    od;
    return fuseit;
  fi;
end);

# JDM use FuseRightBlocks here!

InstallGlobalFunction(OnRightBlocks,
function(blocks, f)
  local n, nrblocks, nrfblocks, fblocks, fuse, sign, fuseit, x, y, tab, out,
   next, i;

  if blocks!.blocks = [0] then   # special case for dummy/seed
    return RightBlocks(f);
  fi;

  n := DegreeOfBlocks(blocks); # length of partition!!
  nrblocks := NrBlocks(blocks);

  nrfblocks := NrBlocks(f);
  fblocks := f!.blocks;

  fuse := [1 .. nrblocks + nrfblocks];
  sign := EmptyPlist(nrfblocks + nrblocks);

  for i in [1 .. nrblocks] do
    sign[i] := blocks[n + i];
  od;
  for i in [nrblocks + 1 .. nrfblocks + nrblocks] do
    sign[i] := 0;
  od;

  fuseit := function(i)
    while fuse[i] < i do
      i := fuse[i];
    od;
    return i;
  end;

  for i in [1 .. n] do
    x := fuseit(blocks[i]);
    y := fuseit(fblocks[i] + nrblocks);
    if x <> y then
      if x < y then
        fuse[y] := x;
        if sign[y] = 1 then
          sign[x] := 1;
        fi;
      else
        fuse[x] := y;
        if sign[x] = 1 then
          sign[y] := 1;
        fi;
      fi;
    fi;
  od;

  tab := 0 * fuse;
  out := [];
  next := 0;

  for i in [n + 1 .. 2 * n] do
    x := fuseit(fblocks[i] + nrblocks);
    if tab[x] = 0 then
      next := next + 1;
      tab[x] := next;
    fi;
    out[i - n + 1] := tab[x];
    out[n + 1 + tab[x]] := sign[x];
  od;
  out[1] := next;
  out := Objectify(BlocksType, rec(blocks := out));
  return out;
end);
#

InstallGlobalFunction(OnLeftBlocks,
function(blocks, f)
  local n, nrblocks, nrfblocks, fblocks, fuse, sign, fuseit, x, y, tab, out,
  next, i;

  if blocks!.blocks = [0] then
    return LeftBlocks(f);
  fi;

  n := DegreeOfBlocks(blocks);  # length of <blocks>
  nrblocks := NrBlocks(blocks);

  nrfblocks := NrBlocks(f);
  fblocks := f!.blocks;

  fuse := [1 .. nrfblocks + nrblocks];
  sign := EmptyPlist(nrfblocks + nrblocks);

  for i in [1 .. nrfblocks] do
    sign[i] := 0;
  od;
  for i in [1 .. nrblocks] do
    sign[i + nrfblocks] := blocks[n + i];
  od;

  fuseit := function(i)
    while fuse[i] < i do
      i := fuse[i];
    od;
    return i;
  end;

  for i in [1 .. n] do
    x := fuseit(fblocks[n + i]);
    y := fuseit(blocks[i] + nrfblocks);
    if x <> y then
      if x < y then
        fuse[y] := x;
        if sign[y] = 1 then
          sign[x] := 1;
        fi;
      else
        fuse[x] := y;
        if sign[x] = 1 then
          sign[y] := 1;
        fi;
      fi;
    fi;
  od;

  tab := 0 * fuse;
  out := [];
  next := 0;

  for i in [1 .. n] do
    x := fuseit(fblocks[i]);
    if tab[x] = 0 then
      next := next + 1;
      tab[x] := next;
    fi;
    out[i + 1] := tab[x];
    out[n + 1 + tab[x]] := sign[x];
  od;
  out[1] := next;
  out := Objectify(BlocksType, rec(blocks := out));
  return out;
end);

# LambdaInverse - fuse <blocks> with the left blocks of <f> keeping track of
# the signs of the fused classes.
#
# The left blocks of the output are then:
# 1) disconnected right blocks of <f> (before fusing)
# 2) disconnected right blocks of <f> (after fusing)
# 3) connected right blocks of <f> (after fusing)
# both types 1+2 of the disconnected blocks are unioned into one left block of
# the output with index <junk>. The connected blocks 3 of <f> are given the
# next available index, if they have not been seen before. The table <tab1>
# keeps track of which connected right blocks of <f> have been seen before and
# the corresponding index in the output, i.e. <tab1[x]> is the index in <out>
# of the fused block with index <x>.

# The right blocks of the output are:
# 1) disconnected blocks of <blocks>; or
# 2) connected blocks of <blocks>.
# The disconnected blocks 1 are given the next available index, if they have
# not been seen before. The table <tab2> keeps
# track of which disconnected blocks of <blocks> have been seen before and the
# corresponding index in the output, i.e. <tab2[x]> is the index in <out> of
# the disconnected block of <blocks> with index <x>. The connected blocks 2 of
# <blocks> is given the index <tab1[x]> where <x> is the fused index of the
# block.

InstallGlobalFunction(InverseRightBlocks,
function(blocks, f)
  local n, nrblocks, fblocks, fusesign, sign, fuseit, out, junk, next,
   tab1, x, nrleft, tab2, i;

  n := DegreeOfBlocks(blocks); # length of partition!!
  nrblocks := NrBlocks(blocks);
  fblocks := f!.blocks;

  fusesign := FuseRightBlocks(blocks, f, true);
  fuseit := fusesign[1];
  sign := fusesign[2];

  out := [];
  junk := 0;
  next := 0;

  # find the left blocks of the output
  tab1 := [];
  for i in [1 .. n] do
    if fblocks[i + n] > NrLeftBlocks(f) then #disconnected before fusing
      if junk = 0 then
        next := next + 1;
        junk := next;
      fi;
      out[i] := junk;
    else
      x := fuseit(fblocks[i + n] + nrblocks);
      if sign[x] = 0 then #disconnected after fusing
        if junk = 0 then
          next := next + 1;
          junk := next;
        fi;
        out[i] := junk;
      else              # connected block
        if not IsBound(tab1[x]) then
          next := next + 1;
          tab1[x] := next;
        fi;
        out[i] := tab1[x];
      fi;
    fi;
  od;
  nrleft := next;

  # find the right blocks of the output
  tab2 := [];
  for i in [n + 1 .. 2 * n] do
    x := blocks[i - n];
    if blocks[n + x] = 1 then
      x := fuseit(x);
      out[i] := tab1[x];
    else
      if not IsBound(tab2[x]) then
        next := next + 1;
        tab2[x] := next;
      fi;
      out[i] := tab2[x];
    fi;
  od;

  out := Objectify(BipartitionType, rec(blocks := out));

  SetDegreeOfBipartition(out, n);
  SetNrLeftBlocks(out, nrleft);
  SetNrBlocks(out, next);
  return out;
end);

# RhoInverse

InstallGlobalFunction(InverseLeftBlocks,
function(blocks, f)
  local n, nrblocks, fblocks, nrfblocks, fuse, fuseit, x, y, out, tab, i;

  n := DegreeOfBlocks(blocks); # length of partition!!
  nrblocks := NrBlocks(blocks);
  fblocks := f!.blocks;
  nrfblocks := NrBlocks(f);

  fuse := [1 .. nrblocks + nrfblocks];
  fuseit := function(i)
    while fuse[i] < i do
      i := fuse[i];
    od;
    return i;
  end;

  for i in [1 .. n] do
    x := fuseit(blocks[i]);
    y := fuseit(fblocks[n + i] + nrblocks);
    if x <> y then
      if x < y then
        fuse[y] := x;
      else
        fuse[x] := y;
      fi;
    fi;
  od;

  out := [];
  tab := [];

  for i in [1 .. nrblocks] do
    if blocks[n + i] = 1 then
      tab[fuseit(i)] := i;
    fi;
  od;

  # find the left blocks of the output
  for i in [1 .. n] do
    out[i] := blocks[i];
    x := fuseit(fblocks[i] + nrblocks);
    if x > nrblocks or not IsBound(tab[x]) then
      out[i + n] := nrblocks + 1; #junk
    else
      out[i + n] := tab[x];
    fi;
  od;

  out := Objectify(BipartitionType, rec(blocks := out));
  SetDegreeOfBipartition(out, n);
  SetNrLeftBlocks(out, nrblocks);
  SetNrBlocks(out, nrblocks + 1);
  return out;
end);
