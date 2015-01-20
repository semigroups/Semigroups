############################################################################
##
#W  reesmat-cong.gi
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for congruences on finite (0-)simple Rees
## (0-)matrix semigroups, using linked triples.
##

InstallGlobalFunction(RMSCongruenceByLinkedTriple,
function(s, n, colBlocks, rowBlocks)
  local g, mat, block, i, j, u, v, bi, bj, bu, bv;
  mat := Matrix(s);
  g := UnderlyingSemigroup(s);

  # Basic checks
  if not IsNormal(g,n) then
    Error("Semigroups: RMSCongruenceByLinkedTriple: usage,\n",
          "the second arg <n> must be a normal subgroup,");
    return;
  fi;
  if not ForAll(colBlocks, IsList) then
    Error("Semigroups: RMSCongruenceByLinkedTriple: usage,\n",
          "the third arg <colBlocks> must be a list of lists,");
    return;
  fi;
  if not ForAll(rowBlocks, IsList) then
    Error("Semigroups: RMSCongruenceByLinkedTriple: usage,\n",
          "the fourth arg <rowBlocks> must be a list of lists,");
    return;
  fi;
  if SortedList(Flat(colBlocks)) <> [1 .. Size(mat[1])] then
    Error("Semigroups: RMSCongruenceByLinkedTriple: usage,\n",
          "the third arg <colBlocks> must be a partition ",
          "of the columns of the matrix of <s>,");
    return;
  fi;
  if SortedList(Flat(rowBlocks)) <> [1 .. Size(mat)] then
    Error("Semigroups: RMSCongruenceByLinkedTriple: usage,\n",
          "the fourth arg <rowBlocks> must be a partition ",
          "of the rows of the matrix of <s>,");
    return;
  fi;

  if IsLinkedTriple(s, n, colBlocks, rowBlocks) then
    return RMSCongruenceByLinkedTripleNC(s, n, colBlocks, rowBlocks);
  else
    Error("Semigroups: RMSCongruenceByLinkedTriple:\n",
          "invalid triple,");
    return;
  fi;
end);

#

InstallGlobalFunction(RZMSCongruenceByLinkedTriple,
function(s, n, colBlocks, rowBlocks)
  local g, mat, block, i, j, u, v, bi, bj, bu, bv;
  mat := Matrix(s);
  g := UnderlyingSemigroup(s);

  # Basic checks
  if not (IsGroup(g) and IsGroup(n)) then
    Error("Semigroups: RZMSCongruenceByLinkedTriple: usage,\n",
          "the first arg <s> must be a Rees 0-matrix semigroup over a group,");
    return;
  fi;

  if not IsNormal(g,n) then
    Error("Semigroups: RZMSCongruenceByLinkedTriple: usage,\n",
          "the second arg <n> must be a normal subgroup,");
    return;
  fi;
  if not ForAll(colBlocks, IsList) then
    Error("Semigroups: RZMSCongruenceByLinkedTriple: usage,\n",
          "the third arg <colBlocks> must be a list of lists,");
    return;
  fi;
  if not ForAll(rowBlocks, IsList) then
    Error("Semigroups: RZMSCongruenceByLinkedTriple: usage,\n",
          "the fourth arg <rowBlocks> must be a list of lists,");
    return;
  fi;
  if SortedList(Flat(colBlocks)) <> [1 .. Size(mat[1])] then
    Error("Semigroups: RZMSCongruenceByLinkedTriple: usage,\n",
          "the third arg <colBlocks> must be a partition ",
          "of the columns of the matrix of <s>,");
    return;
  fi;
  if SortedList(Flat(rowBlocks)) <> [1 .. Size(mat)] then
    Error("Semigroups: RZMSCongruenceByLinkedTriple: usage,\n",
          "the fourth arg <rowBlocks> must be a partition ",
          "of the rows of the matrix of <s>,");
    return;
  fi;

  if IsLinkedTriple(s, n, colBlocks, rowBlocks) then
    return RZMSCongruenceByLinkedTripleNC(s, n, colBlocks, rowBlocks);
  else
    Error("Semigroups: RZMSCongruenceByLinkedTriple:\n",
          "invalid triple,");
    return;
  fi;
end);

#

InstallGlobalFunction(RMSCongruenceByLinkedTripleNC,
function(s, n, colBlocks, rowBlocks)
  local fam, cong, colLookup, rowLookup, i, j;
  # Sort the blocks
  colBlocks := SSortedList(colBlocks);
  rowBlocks := SSortedList(rowBlocks);
  # Calculate lookup table for equivalence relations
  colLookup := [];
  rowLookup := [];
  for i in [1 .. Length(colBlocks)] do
    for j in colBlocks[i] do
      colLookup[j] := i;
    od;
  od;
  for i in [1 .. Length(rowBlocks)] do
    for j in rowBlocks[i] do
      rowLookup[j] := i;
    od;
  od;
  # Construct the object
  fam := GeneralMappingsFamily(
                 ElementsFamily(FamilyObj(s)),
                 ElementsFamily(FamilyObj(s)) );
  cong := Objectify(
                  NewType(fam, IsRMSCongruenceByLinkedTriple),
                  rec(n := n,
                      colBlocks := colBlocks, colLookup := colLookup,
                      rowBlocks := rowBlocks, rowLookup := rowLookup) );
  SetSource(cong, s);
  SetRange(cong, s);
  return cong;
end);

#

InstallGlobalFunction(RZMSCongruenceByLinkedTripleNC,
function(s, n, colBlocks, rowBlocks)
  local fam, cong, colLookup, rowLookup, i, j;
  # Sort the blocks
  colBlocks := SSortedList(colBlocks);
  rowBlocks := SSortedList(rowBlocks);
  # Calculate lookup table for equivalence relations
  colLookup := [];
  rowLookup := [];
  for i in [1 .. Length(colBlocks)] do
    for j in colBlocks[i] do
      colLookup[j] := i;
    od;
  od;
  for i in [1 .. Length(rowBlocks)] do
    for j in rowBlocks[i] do
      rowLookup[j] := i;
    od;
  od;
  # Construct the object
  fam := GeneralMappingsFamily(
                 ElementsFamily(FamilyObj(s)),
                 ElementsFamily(FamilyObj(s)) );
  cong := Objectify(
                  NewType(fam, IsRZMSCongruenceByLinkedTriple),
                  rec(n := n,
                      colBlocks := colBlocks, colLookup := colLookup,
                      rowBlocks := rowBlocks, rowLookup := rowLookup) );
  SetSource(cong, s);
  SetRange(cong, s);
  return cong;
end);

#

InstallMethod(ViewObj,
"for Rees matrix semigroup congruence by linked triple",
[IsRMSCongruenceByLinkedTriple],
function(cong)
  Print("<semigroup congruence over ");
  ViewObj(Range(cong));
  Print(" with linked triple (",
        StructureDescription(cong!.n:short), ",",
        Size(cong!.colBlocks), ",",
        Size(cong!.rowBlocks),")>");
end);

#

InstallMethod(ViewObj,
"for Rees zero-matrix semigroup congruence by linked triple",
[IsRZMSCongruenceByLinkedTriple],
function(cong)
  Print("<semigroup congruence over ");
  ViewObj(Range(cong));
  Print(" with linked triple (",
        StructureDescription(cong!.n:short), ",",
        Size(cong!.colBlocks), ",",
        Size(cong!.rowBlocks),")>");
end);

#

InstallMethod(CongruencesOfSemigroup,
"for finite simple Rees matrix semigroup",
[IsReesMatrixSemigroup and IsSimpleSemigroup and IsFinite],
function(s)
  local subpartitions, congs, mat, g, AddRelation, colBlocksList, rowBlocksList,
        n, colBlocks, rowBlocks;

  # Function to compute all subsets of a relation given by partitions
  subpartitions := function(part)
    local l;
    # Replace each class with a list of all partitions of that class
    l := List(part, PartitionsSet);
    # Produce all the combinations of partitions of classes
    l := Cartesian(l);
    # Concatenate these lists to produce complete partitions of the set
    l := List(l, Concatenation);
    # Finally sort each of these into the canonical order of its new classes
    l := List(l, SSortedList);
    return l;
  end;

  congs := [];
  mat := Matrix(s);
  g := UnderlyingSemigroup(s);

  # This function combines two congruence classes
  AddRelation := function(R, x, y)
    local xClass, yClass;
    xClass := PositionProperty(R, class -> x in class);
    yClass := PositionProperty(R, class -> y in class);
    if xClass <> yClass then
      Append(R[xClass],R[yClass]);
      Remove(R,yClass);
    fi;
  end;

  # No need to add the universal congruence

  # Compute all column and row relations which are subsets of the max relations
  colBlocksList := subpartitions( [ [1 .. Size(mat[1])] ] );
  rowBlocksList := subpartitions( [ [1 .. Size(mat)] ] );

  # Go through all triples and check
  for n in NormalSubgroups(g) do
    for colBlocks in colBlocksList do
      for rowBlocks in rowBlocksList do
        if IsLinkedTriple(s, n, colBlocks, rowBlocks) then
          Add(congs, RMSCongruenceByLinkedTripleNC(s, n, colBlocks, rowBlocks));
        fi;
      od;
    od;
  od;

  return congs;
end);

#

InstallMethod(CongruencesOfSemigroup,
"for finite 0-simple Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup and IsZeroSimpleSemigroup and IsFinite],
function(s)
  local congs, mat, g, AddRelation, maxColBlocks, maxRowBlocks,
        i, j, u, v, n, colBlocks, rowBlocks, colBlocksList, rowBlocksList,
        subpartitions;

  # Function to compute all subsets of a relation given by partitions
  subpartitions := function(part)
    local l;
    # Replace each class with a list of all partitions of that class
    l := List(part, PartitionsSet);
    # Produce all the combinations of partitions of classes
    l := Cartesian(l);
    # Concatenate these lists to produce complete partitions of the set
    l := List(l, Concatenation);
    # Finally sort each of these into the canonical order of its new classes
    l := List(l, SSortedList);
    return l;
  end;

  congs := [];
  mat := Matrix(s);
  g := UnderlyingSemigroup(s);

  # This function combines two congruence classes
  AddRelation := function(R, x, y)
    local xClass, yClass;
    xClass := PositionProperty(R, class -> x in class);
    yClass := PositionProperty(R, class -> y in class);
    if xClass <> yClass then
      Append(R[xClass],R[yClass]);
      Remove(R,yClass);
    fi;
  end;

  # Construct maximum column relation
  maxColBlocks := List([1 .. Size(mat[1])], i -> [i]);
  for i in [1 .. Size(mat[1])] do
    for j in [i + 1 .. Size(mat[1])] do
      if ForAll([1 .. Size(mat)],
       u -> ( (mat[u][i] = 0) = (mat[u][j] = 0) ) ) then
        AddRelation(maxColBlocks, i, j);
      fi;
    od;
  od;

  # Construct maximum row relation
  maxRowBlocks := List([1 .. Size(mat)], u -> [u]);
  for u in [1 .. Size(mat)] do
    for v in [u + 1 .. Size(mat)] do
      if ForAll([1 .. Size(mat[1])],
       i -> ( (mat[u][i] = 0) = (mat[v][i] = 0) ) ) then
        AddRelation(maxRowBlocks, u, v);
      fi;
    od;
  od;

  # Add the universal congruence
  Add(congs, UniversalSemigroupCongruence(s));

  # Compute all column and row relations which are subsets of the max relations
  colBlocksList := subpartitions(maxColBlocks);
  rowBlocksList := subpartitions(maxRowBlocks);

  # Go through all triples and check
  for n in NormalSubgroups(g) do
    for colBlocks in colBlocksList do
      for rowBlocks in rowBlocksList do
        if IsLinkedTriple(s, n, colBlocks, rowBlocks) then
          Add(congs,
           RZMSCongruenceByLinkedTripleNC(s, n, colBlocks, rowBlocks));
        fi;
      od;
    od;
  od;

  return congs;
end);

#

InstallMethod(IsLinkedTriple,
"for Rees matrix semigroup, group, and two lists",
[IsReesMatrixSemigroup,
 IsGroup, IsDenseList, IsDenseList],
function(s, n, colBlocks, rowBlocks)
  local mat, block, bi, bj, i, j, u, v, bu, bv;
  # Check the semigroup is valid
  if not (IsFinite(s) and IsSimpleSemigroup(s)) then
    Error("Semigroups: IsLinkedTriple: usage,\n",
          "first arg <s> must be a finite simple Rees matrix semigroup,");
    return;
  fi;
  mat := Matrix(s);
  # Check axiom (L2) from Howie p.86, then call NC function
  # Go through the column blocks
  for block in colBlocks do
    # Check q-condition for all pairs of columns in this block (L2)
    for bi in [1 .. Size(block)] do
      for bj in [bi + 1 .. Size(block)] do
        i := block[bi];
        j := block[bj];
        # Check all pairs of rows (u,v)
        for u in [1 .. Size(mat)] do
          for v in [u + 1 .. Size(mat)] do
            if not (mat[u][i] * mat[v][i] ^ - 1 * mat[v][j] * mat[u][j] ^ - 1)
             in n then
              return false;
            fi;
          od;
        od;
      od;
    od;
  od;

  # Go through the row blocks
  for block in rowBlocks do
    # Check q-condition for all pairs of rows in this block (L2)
    for bu in [1 .. Size(block)] do
      for bv in [bu + 1 .. Size(block)] do
        u := block[bu];
        v := block[bv];
        # Check all pairs of columns (i,j)
        for i in [1 .. Size(mat[1])] do
          for j in [i + 1 .. Size(mat[1])] do
            if not (mat[u][i] * mat[v][i] ^ - 1 * mat[v][j] * mat[u][j] ^ - 1)
            in n then
              return false;
            fi;
          od;
        od;
      od;
    od;
  od;
  return true;
end);

#

InstallMethod(IsLinkedTriple,
"for Rees 0-matrix semigroup, group, and two lists",
[IsReesZeroMatrixSemigroup,
 IsGroup, IsDenseList, IsDenseList],
function(s, n, colBlocks, rowBlocks)
  local mat, block, i, j, u, v, bi, bj, bu, bv;
  # Check the semigroup is valid
  if not (IsFinite(s) and IsZeroSimpleSemigroup(s)) then
    Error("Semigroups: IsLinkedTriple: usage,\n",
          "the first arg <s> must be a finite 0-simple Rees 0-matrix ",
          "semigroup");
    return;
  fi;
  mat := Matrix(s);
  # Check axioms (L1) and (L2) from Howie p.86, then call NC function
  # Go through the column blocks
  for block in colBlocks do
    for bj in [2 .. Size(block)] do
      # Check columns have zeroes in all the same rows (L1)
      for u in [1 .. Size(mat)] do
        if (mat[u][block[1]] = 0) <> (mat[u][block[bj]] = 0) then
          return false;
        fi;
      od;
    od;
    # Check q-condition for all pairs of columns in this block (L2)
    for bi in [1 .. Size(block)] do
      for bj in [bi + 1 .. Size(block)] do
        i := block[bi];
        j := block[bj];
        # Check all pairs of rows (u,v)
        for u in [1 .. Size(mat)] do
          if mat[u][i] = 0 then
            continue;
          fi;
          for v in [u + 1 .. Size(mat)] do
            if mat[v][i] = 0 then
              continue;
            fi;
            if not (mat[u][i] * mat[v][i] ^ - 1 * mat[v][j] * mat[u][j] ^ - 1)
             in n then
              return false;
            fi;
          od;
        od;
      od;
    od;
  od;

  # Go through the row blocks
  for block in rowBlocks do
    for bv in [2 .. Size(block)] do
      # Check rows have zeroes in all the same columns (L1)
      for i in [1 .. Size(mat[1])] do
        if (mat[block[1]][i] = 0) <> (mat[block[bv]][i] = 0) then
          return false;
        fi;
      od;
    od;
    # Check q-condition for all pairs of rows in this block (L2)
    for bu in [1 .. Size(block)] do
      for bv in [bu + 1 .. Size(block)] do
        u := block[bu];
        v := block[bv];
        # Check all pairs of columns (i,j)
        for i in [1 .. Size(mat[1])] do
          if mat[u][i] = 0 then
            continue;
          fi;
          for j in [i + 1 .. Size(mat[1])] do
          if mat[u][j] = 0 then
          continue;
          fi;
            if not (mat[u][i] * mat[v][i] ^ - 1 * mat[v][j] * mat[u][j] ^ - 1)
            in n then
              return false;
            fi;
          od;
        od;
      od;
    od;
  od;
  return true;
end);

#

BindGlobal("LinkedElement",
function(elm)
  local mat, i, u, v, j;
  mat := Matrix(ReesMatrixSemigroupOfFamily(FamilyObj(elm)));
  i := elm[1];  # Column no
  u := elm[3];  # Row no
  if IsReesMatrixSemigroupElement(elm) then
    # RMS case
    return(mat[1][i] * elm[2] * mat[u][1]);
  else
    # RZMS case
    for v in [1 .. Size(mat)] do
      if mat[v][i] <> 0 then
        break;
      fi;
    od;
    for j in [1 .. Size(mat[1])] do
      if mat[u][j] <> 0 then
        break;
      fi;
    od;
    return mat[v][i] * elm[2] * mat[u][j];
  fi;
end);

#

InstallMethod( \=,
"for two Rees matrix semigroup congruences by linked triple",
[IsRMSCongruenceByLinkedTriple, IsRMSCongruenceByLinkedTriple],
function(c1, c2)
  return( Range(c1) = Range(c2) and
          c1!.n = c2!.n and
          c1!.colBlocks = c2!.colBlocks and
          c1!.rowBlocks = c2!.rowBlocks );
end);

#

InstallMethod( \=,
"for two Rees 0-matrix semigroup congruences by linked triple",
[IsRZMSCongruenceByLinkedTriple, IsRZMSCongruenceByLinkedTriple],
function(c1, c2)
  return( Range(c1) = Range(c2) and
          c1!.n = c2!.n and
          c1!.colBlocks = c2!.colBlocks and
          c1!.rowBlocks = c2!.rowBlocks );
end);

#

InstallMethod(\in,
"for RMS element coll and a semigroup congruence by linked triple",
[IsReesMatrixSemigroupElementCollection, IsRMSCongruenceByLinkedTriple],
function(pair, cong)
  local s, i, a, u, j, b, v, mat, gpElm;

  # Check for validity
  if Size(pair) <> 2 then
    Error("Semigroups: \in: usage,\n",
          "the first arg <pair> must be a list of length 2,");
    return;
  fi;
  s := Range(cong);
  if not ForAll(pair, x -> x in s) then
    Error("Semigroups: \in: usage,\n",
          "the elements of the first arg <pair> ",
          "must be in the range of the second arg <cong>,");
    return;
  fi;

  # Read the elements as (i,a,u) and (j,b,v)
  i := pair[1][1];
  a := pair[1][2];
  u := pair[1][3];
  j := pair[2][1];
  b := pair[2][2];
  v := pair[2][3];

  # First, the columns and rows must be related
  if not (cong!.colLookup[i] = cong!.colLookup[j] and
          cong!.rowLookup[u] = cong!.rowLookup[v]) then
    return false;
  fi;

  # Finally, check Lemma 3.5.6(2) in Howie, with row 1 and column 1
  mat := Matrix(s);
  gpElm := mat[1][i] * a * mat[u][1] * Inverse(mat[1][j] * b * mat[v][1]);
  return(gpElm in cong!.n);
end);

#

InstallMethod(\in,
"for RZMS elements and semigroup congruence by linked triple",
[IsReesZeroMatrixSemigroupElementCollection, IsRZMSCongruenceByLinkedTriple],
function(pair, cong)
  local s, mat, gpElm, row, col, rows, cols, a, i, u, j, b, v;

  # Check for validity
  if Size(pair) <> 2 then
    Error("Semigroups: \in: usage,\n",
          "the first arg <pair> must be a list of length 2,");
    return;
  fi;
  s := Range(cong);
  if not ForAll(pair, x -> x in s) then
    Error("Semigroups: \in: usage,\n",
          "the elements of the first arg <pair> ",
          "must be in the range of the second arg <cong>,");
    return;
  fi;

  # Handling the case when one or more of the pair are zero
  if pair[1] = pair[2] then
    return true;
  elif MultiplicativeZero(s) in pair then
    return false;
  fi;

  # Read the elements as (i,a,u) and (j,b,v)
  i := pair[1][1];
  a := pair[1][2];
  u := pair[1][3];
  j := pair[2][1];
  b := pair[2][2];
  v := pair[2][3];

  # First, the columns and rows must be related
  if not (cong!.colLookup[i] = cong!.colLookup[j] and
          cong!.rowLookup[u] = cong!.rowLookup[v]) then
    return false;
  fi;

  # Finally, check Lemma 3.5.6(2) in Howie
  mat := Matrix(s);
  rows := mat;
  cols := TransposedMat(mat);
  # Pick a valid column and row
  col := PositionProperty(rows[u], x -> x <> 0);
  row := PositionProperty(cols[i], x -> x <> 0);
  gpElm := mat[row][i] * a * mat[u][col] *
           Inverse(mat[row][j] * b * mat[v][col]);
  return(gpElm in cong!.n);
end);

#

InstallMethod(ImagesElm,
"for Rees matrix semigroup congruence by linked triple and element",
[IsRMSCongruenceByLinkedTriple, IsReesMatrixSemigroupElement],
function(cong, elm)
  local s, mat, images, i, a, u, j, v, nElm, b;
  s := Range(cong);
  mat := Matrix(s);
  if not elm in s then
    Error("Semigroups: ImagesElm: usage,\n",
          "the args <cong> and <elm> must refer to the same semigroup,");
    return;
  fi;
  # List of all elements congruent to elm under cong
  images := [];
  # Read the element as (i,a,u)
  i := elm[1];
  a := elm[2];
  u := elm[3];
  # Construct congruent elements as (j,b,v)
  for j in cong!.colBlocks[cong!.colLookup[i]] do
    for v in cong!.rowBlocks[cong!.rowLookup[u]] do
      for nElm in cong!.n do
        # Might be better to use congruence classes after all
        b := mat[1][j] ^ - 1 * nElm * mat[1][i] * a * mat[u][1]
             * mat[v][1] ^ - 1;
        Add(images, RMSElement(s, j, b, v));
      od;
    od;
  od;
  return images;
end);

#

InstallMethod(ImagesElm,
"for Rees 0-matrix semigroup congruence by linked triple and element",
[IsRZMSCongruenceByLinkedTriple, IsReesZeroMatrixSemigroupElement],
function(cong, elm)
  local s, mat, images, i, a, u, row, col, j, b, v, nElm;
  s := Range(cong);
  mat := Matrix(s);
  if not elm in s then
    Error("Semigroups: ImagesElm: usage,\n",
          "the args <cong> and <elm> must refer to the same semigroup,");
    return;
  fi;
  # Special case for 0
  if elm = MultiplicativeZero(s) then
    return [elm];
  fi;
  # List of all elements congruent to elm under cong
  images := [];
  # Read the element as (i,a,u)
  i := elm[1];
  a := elm[2];
  u := elm[3];
  # Find a non-zero row for this class of columns
  for row in [1 .. Size(mat)] do
    if mat[row][i] <> 0 then
      break;
    fi;
  od;
  # Find a non-zero column for this class of rows
  for col in [1 .. Size(mat[1])] do
    if mat[u][col] <> 0 then
      break;
    fi;
  od;

  # Construct congruent elements as (j,b,v)
  for j in cong!.colBlocks[cong!.colLookup[i]] do
    for v in cong!.rowBlocks[cong!.rowLookup[u]] do
      for nElm in cong!.n do
        # Might be better to use congruence classes after all
        b := mat[row][j] ^ - 1
             * nElm
             * mat[row][i]
             * a
             * mat[u][col]
             * mat[v][col] ^ - 1;
        Add(images, RMSElement(s, j, b, v));
      od;
    od;
  od;
  return images;
end);

#

InstallMethod(EquivalenceClasses,
"for Rees matrix semigroup congruence by linked triple",
[IsRMSCongruenceByLinkedTriple],
function(cong)
  local list, s, g, n, colBlocks, rowBlocks, colClass, rowClass, rep, elm;
  list := [];
  s := Range(cong);
  g := UnderlyingSemigroup(s);
  n := cong!.n;
  colBlocks := cong!.colBlocks;
  rowBlocks := cong!.rowBlocks;
  for colClass in [1 .. Size(colBlocks)] do
    for rowClass in [1 .. Size(rowBlocks)] do
      for rep in List(RightCosets(g,n), Representative) do
        elm := RMSElement(
                       s, colBlocks[colClass][1], rep, rowBlocks[rowClass][1] );
        Add(list, EquivalenceClassOfElement(cong, elm));
      od;
    od;
  od;
  return list;
end);

#

InstallMethod(EquivalenceClasses,
"for Rees 0-matrix semigroup congruence by linked triple",
[IsRZMSCongruenceByLinkedTriple],
function(cong)
  local list, s, g, n, colBlocks, rowBlocks, colClass, rowClass, rep, elm;
  list := [];
  s := Range(cong);
  g := UnderlyingSemigroup(s);
  n := cong!.n;
  colBlocks := cong!.colBlocks;
  rowBlocks := cong!.rowBlocks;
  for colClass in [1 .. Size(colBlocks)] do
    for rowClass in [1 .. Size(rowBlocks)] do
      for rep in List(RightCosets(g,n), Representative) do
        elm := RMSElement(
                       s, colBlocks[colClass][1], rep, rowBlocks[rowClass][1] );
        Add(list, EquivalenceClassOfElement(cong, elm));
      od;
    od;
  od;
  # Add the zero class
  Add(list, EquivalenceClassOfElement(cong, MultiplicativeZero(s)));
  return list;
end);

#

InstallMethod(NrCongruenceClasses,
"for Rees matrix semigroup congruence by linked triple",
[IsRMSCongruenceByLinkedTriple],
function(cong)
  local s, g;
  s := Range(cong);
  g := UnderlyingSemigroup(s);
  return( Index(g, cong!.n)             # Number of cosets of n
          * Size(cong!.colBlocks)       # Number of column blocks
          * Size(cong!.rowBlocks) );    # Number of row blocks
end);

#

InstallMethod(NrCongruenceClasses,
"for Rees 0-matrix semigroup congruence by linked triple",
[IsRZMSCongruenceByLinkedTriple],
function(cong)
  local s, g;
  s := Range(cong);
  g := UnderlyingSemigroup(s);
  return( Index(g, cong!.n)    # Number of cosets of n
          * Size(cong!.colBlocks)       # Number of column blocks
          * Size(cong!.rowBlocks)       # Number of row blocks
          + 1 );                        # Class containing zero
end);

#

InstallMethod(JoinSemigroupCongruences,
"for two Rees matrix semigroup congruences by linked triple",
[IsRMSCongruenceByLinkedTriple, IsRMSCongruenceByLinkedTriple],
function(c1, c2)
  local gens, n, colBlocks, rowBlocks, block, b1, j, pos;
  if Range(c1) <> Range(c2) then
    Error("Semigroups: JoinSemigroupCongruences: usage,\n",
          "congruences must be defined over the same semigroup,");
    return;
  fi;
  # n is the product of the normal subgroups
  gens := Concatenation(GeneratorsOfGroup(c1!.n), GeneratorsOfGroup(c2!.n));
  n := Subgroup(UnderlyingSemigroup(Range(c1)), gens);
  # Calculate the join of the column and row relations
  colBlocks := StructuralCopy(c1!.colBlocks);
  rowBlocks := StructuralCopy(c1!.rowBlocks);
  for block in c2!.colBlocks do
    b1 := PositionProperty(colBlocks, cb -> block[1] in cb);
    for j in [2 .. Size(block)] do
      if not block[j] in colBlocks[b1] then
        # Combine the classes
        pos := PositionProperty(colBlocks, cb -> block[j] in cb);
        Append(colBlocks[b1], colBlocks[pos]);
        Unbind(colBlocks[pos]);
      fi;
    od;
    colBlocks := Compacted(colBlocks);
  od;
  for block in c2!.rowBlocks do
    b1 := PositionProperty(rowBlocks, rb -> block[1] in rb);
    for j in [2 .. Size(block)] do
      if not block[j] in rowBlocks[b1] then
        # Combine the classes
        pos := PositionProperty(rowBlocks, rb -> block[j] in rb);
        Append(rowBlocks[b1], rowBlocks[pos]);
        Unbind(rowBlocks[pos]);
      fi;
    od;
    rowBlocks := Compacted(rowBlocks);
  od;
  colBlocks := SortedList(List(colBlocks, block -> SortedList(block)));
  rowBlocks := SortedList(List(rowBlocks, block -> SortedList(block)));
  # Make the congruence and return it
  return RMSCongruenceByLinkedTripleNC(Range(c1), n, colBlocks, rowBlocks);
end);

#

InstallMethod(JoinSemigroupCongruences,
"for two Rees 0-matrix semigroup congruences by linked triple",
[IsRZMSCongruenceByLinkedTriple, IsRZMSCongruenceByLinkedTriple],
function(c1, c2)
  local gens, n, colBlocks, rowBlocks, block, b1, j, pos;
  if Range(c1) <> Range(c2) then
    Error("Semigroups: JoinSemigroupCongruences: usage,\n",
          "the args must be defined over the same semigroup,");
    return;
  fi;
  # n is the product of the normal subgroups
  gens := Concatenation(GeneratorsOfGroup(c1!.n), GeneratorsOfGroup(c2!.n));
  n := Subgroup(UnderlyingSemigroup(Range(c1)), gens);
  # Calculate the join of the column and row relations
  colBlocks := StructuralCopy(c1!.colBlocks);
  rowBlocks := StructuralCopy(c1!.rowBlocks);
  for block in c2!.colBlocks do
    b1 := PositionProperty(colBlocks, cb -> block[1] in cb);
    for j in [2 .. Size(block)] do
      if not block[j] in colBlocks[b1] then
        # Combine the classes
        pos := PositionProperty(colBlocks, cb -> block[j] in cb);
        Append(colBlocks[b1], colBlocks[pos]);
        Unbind(colBlocks[pos]);
      fi;
    od;
    colBlocks := Compacted(colBlocks);
  od;
  for block in c2!.rowBlocks do
    b1 := PositionProperty(rowBlocks, rb -> block[1] in rb);
    for j in [2 .. Size(block)] do
      if not block[j] in rowBlocks[b1] then
        # Combine the classes
        pos := PositionProperty(rowBlocks, rb -> block[j] in rb);
        Append(rowBlocks[b1], rowBlocks[pos]);
        Unbind(rowBlocks[pos]);
      fi;
    od;
    rowBlocks := Compacted(rowBlocks);
  od;
  colBlocks := SortedList(List(colBlocks, block -> SortedList(block)));
  rowBlocks := SortedList(List(rowBlocks, block -> SortedList(block)));
  # Make the congruence and return it
  return RZMSCongruenceByLinkedTriple(Range(c1), n, colBlocks, rowBlocks);
end);

#

InstallMethod(MeetSemigroupCongruences,
"for two Rees matrix semigroup congruences by linked triple",
[IsRMSCongruenceByLinkedTriple, IsRMSCongruenceByLinkedTriple],
function(c1, c2)
  local n, colBlocks, cols, rowBlocks, rows, i, block, j, u, v;
  if Range(c1) <> Range(c2) then
    Error("Semigroups: MeetSemigroupCongruences: usage,\n",
          "the args must be defined over the same semigroup,");
    return;
  fi;
  # n is the intersection of the two normal subgroups
  n := Intersection(c1!.n, c2!.n);
  # Calculate the intersection of the column and row relations
  colBlocks := [];
  cols := [1 .. Size(c1!.colLookup)];
  rowBlocks := [];
  rows := [1 .. Size(c1!.rowLookup)];
  for i in [1 .. Size(cols)] do
    if cols[i] = 0 then
      continue;
    fi;
    block := Intersection(c1!.colBlocks[c1!.colLookup[i]],
                          c2!.colBlocks[c2!.colLookup[i]]);
    for j in block do
      cols[j] := 0;
    od;
    Add(colBlocks, block);
  od;
  for u in [1 .. Size(rows)] do
    if rows[u] = 0 then
      continue;
    fi;
    block := Intersection(c1!.rowBlocks[c1!.rowLookup[u]],
                          c2!.rowBlocks[c2!.rowLookup[u]]);
    for v in block do
      rows[v] := 0;
    od;
    Add(rowBlocks, block);
  od;
  # Make the congruence and return it
  return RMSCongruenceByLinkedTripleNC(Range(c1), n, colBlocks, rowBlocks);
end);

#

InstallMethod(MeetSemigroupCongruences,
"for two Rees 0-matrix semigroup congruences by linked triple",
[IsRZMSCongruenceByLinkedTriple, IsRZMSCongruenceByLinkedTriple],
function(c1, c2)
  local n, colBlocks, cols, rowBlocks, rows, i, block, j, u, v;
  if Range(c1) <> Range(c2) then
    Error("Semigroups: MeetSemigroupCongruences: usage,\n",
          "the args must be defined over the same semigroup,");
    return;
  fi;
  # n is the intersection of the two normal subgroups
  n := Intersection(c1!.n, c2!.n);
  # Calculate the intersection of the column and row relations
  colBlocks := [];
  cols := [1 .. Size(c1!.colLookup)];
  rowBlocks := [];
  rows := [1 .. Size(c1!.rowLookup)];
  for i in [1 .. Size(cols)] do
    if cols[i] = 0 then
      continue;
    fi;
    block := Intersection(c1!.colBlocks[c1!.colLookup[i]],
                          c2!.colBlocks[c2!.colLookup[i]]);
    for j in block do
      cols[j] := 0;
    od;
    Add(colBlocks, block);
  od;
  for u in [1 .. Size(rows)] do
    if rows[u] = 0 then
      continue;
    fi;
    block := Intersection(c1!.rowBlocks[c1!.rowLookup[u]],
                          c2!.rowBlocks[c2!.rowLookup[u]]);
    for v in block do
      rows[v] := 0;
    od;
    Add(rowBlocks, block);
  od;
  # Make the congruence and return it
  return RZMSCongruenceByLinkedTripleNC(Range(c1), n, colBlocks, rowBlocks);
end);

#

InstallMethod(RMSCongruenceClassByLinkedTriple,
"for semigroup congruence by linked triple, a coset and two positive integers",
[IsRMSCongruenceByLinkedTriple,
 IsRightCoset, IsPosInt, IsPosInt],
function(cong, nCoset, colClass, rowClass)
  local g;
  g := UnderlyingSemigroup(Range(cong));
  if not (ActingDomain(nCoset) = cong!.n and IsSubset(g, nCoset)) then
    Error("Semigroups: RMSCongruenceClassByLinkedTriple: usage,\n",
          "the second arg <nCoset> must be a right coset of ", cong!.n,
          " inside ", g, ",");
    return;
  fi;
  if not colClass in [1 .. Size(cong!.colBlocks)] then
    Error("Semigroups: RMSCongruenceClassByLinkedTriple: usage,\n",
          "the third arg <colClass> is out of range,");
    return;
  fi;
  if not rowClass in [1 .. Size(cong!.rowBlocks)] then
    Error("Semigroups: RMSCongruenceClassByLinkedTriple: usage,\n",
          "the fourth arg <rowClass> is out of range,");
    return;
  fi;
  return RMSCongruenceClassByLinkedTripleNC(cong, nCoset, colClass, rowClass);
end);

#

InstallMethod(RZMSCongruenceClassByLinkedTriple,
"for semigroup congruence by linked triple, a coset and two positive integers",
[IsRZMSCongruenceByLinkedTriple,
 IsRightCoset, IsPosInt, IsPosInt],
function(cong, nCoset, colClass, rowClass)
  local g;
  g := UnderlyingSemigroup(Range(cong));
  if not (ActingDomain(nCoset) = cong!.n and IsSubset(g, nCoset)) then
    Error("Semigroups: RZMSCongruenceClassByLinkedTriple: usage,\n",
          "the second arg <nCoset> must be a right coset of ", cong!.n,
          " inside ", g, ",");
    return;
  fi;
  if not colClass in [1 .. Size(cong!.colBlocks)] then
    Error("Semigroups: RZMSCongruenceClassByLinkedTriple: usage,\n",
          "the third arg <colClass> is out of range,");
    return;
  fi;
  if not rowClass in [1 .. Size(cong!.rowBlocks)] then
    Error("Semigroups: RZMSCongruenceClassByLinkedTriple: usage,\n",
          "the fourth arg <rowClass> is out of range,");
    return;
  fi;
  return RZMSCongruenceClassByLinkedTripleNC(cong, nCoset, colClass, rowClass);
end);

#

InstallMethod(RMSCongruenceClassByLinkedTripleNC,
"for semigroup congruence by linked triple, a coset and two positive integers",
[IsRMSCongruenceByLinkedTriple,
 IsRightCoset, IsPosInt, IsPosInt],
function(cong, nCoset, colClass, rowClass)
  local fam, class;
  fam := FamilyObj(Range(cong));
  class := Objectify(NewType(fam, IsRMSCongruenceClassByLinkedTriple),
                   rec(nCoset := nCoset,
                       colClass := colClass,
                       rowClass := rowClass) );
  SetParentAttr(class, cong);
  SetRepresentative(class, CanonicalRepresentative(class));
  return class;
end);

#

InstallMethod(RZMSCongruenceClassByLinkedTripleNC,
"for semigroup congruence by linked triple, a coset and two positive integers",
[IsRZMSCongruenceByLinkedTriple,
 IsRightCoset, IsPosInt, IsPosInt],
function(cong, nCoset, colClass, rowClass)
  local fam, class;
  fam := FamilyObj(Range(cong));
  class := Objectify(NewType(fam, IsRZMSCongruenceClassByLinkedTriple),
                   rec(nCoset := nCoset,
                       colClass := colClass,
                       rowClass := rowClass) );
  SetParentAttr(class, cong);
  SetRepresentative(class, CanonicalRepresentative(class));
  return class;
end);

#

InstallMethod(EquivalenceClassOfElement,
"for Rees matrix semigroup congruence by linked triple and element",
[IsRMSCongruenceByLinkedTriple, IsReesMatrixSemigroupElement],
function(cong, elm)
  # Check that the args make sense
  if not elm in Range(cong) then
    Error("Semigroups: EquivalenceClassOfElement: usage,\n",
          "the second arg <elm> must be in the semigroup of ",
          "first arg <cong>");
    return;
  fi;
  return EquivalenceClassOfElementNC(cong, elm);
end);

#

InstallMethod(EquivalenceClassOfElement,
"for Rees 0-matrix semigroup congruence by linked triple and an element",
[IsRZMSCongruenceByLinkedTriple, IsReesZeroMatrixSemigroupElement],
function(cong, elm)
  # Check that the args make sense
  if not elm in Range(cong) then
    Error("Semigroups: EquivalenceClassOfElement: usage,\n",
          "the second arg <elm> must be ",
          "in the semigroup of first arg <cong>");
    return;
  fi;
  return EquivalenceClassOfElementNC(cong, elm);
end);

#

InstallMethod(EquivalenceClassOfElementNC,
"for Rees matrix semigroup congruence by linked triple and element",
[IsRMSCongruenceByLinkedTriple, IsReesMatrixSemigroupElement],
function(cong, elm)
  local fam, class, mat, nCoset, colClass, rowClass;
  fam := CollectionsFamily( FamilyObj(elm) );
  nCoset := RightCoset(cong!.n, LinkedElement(elm));
  colClass := cong!.colLookup[elm[1]];
  rowClass := cong!.rowLookup[elm[3]];
  class := Objectify(NewType(fam, IsRMSCongruenceClassByLinkedTriple),
                   rec(nCoset := nCoset,
                       colClass := colClass,
                       rowClass := rowClass) );
  SetParentAttr(class, cong);
  SetEquivalenceClassRelation(class, cong);
  SetRepresentative(class, elm);
  return class;
end);

#

InstallMethod(EquivalenceClassOfElementNC,
"for Rees 0-matrix semigroup congruence by linked triple and element",
[IsRZMSCongruenceByLinkedTriple, IsReesZeroMatrixSemigroupElement],
function(cong, elm)
  local fam, class, mat, nCoset, colClass, rowClass;
  fam := CollectionsFamily( FamilyObj(elm) );
  if elm = MultiplicativeZero(Range(cong)) then
    class := Objectify(NewType(fam, IsRZMSCongruenceClassByLinkedTriple),
                       rec(nCoset := 0) );
    SetAsSSortedList(class,[elm]);
  else
    nCoset := RightCoset(cong!.n, LinkedElement(elm));
    colClass := cong!.colLookup[elm[1]];
    rowClass := cong!.rowLookup[elm[3]];
    class := Objectify(NewType(fam, IsRZMSCongruenceClassByLinkedTriple),
                       rec(nCoset := nCoset,
                           colClass := colClass,
                           rowClass := rowClass) );
  fi;
  SetParentAttr(class, cong);
  SetEquivalenceClassRelation(class, cong);
  SetRepresentative(class, elm);
  return class;
end);

#

InstallMethod( \in,
"for Rees matrix semigroup element and a congruence class by linked triple",
[IsReesMatrixSemigroupElement, IsRMSCongruenceClassByLinkedTriple],
function(elm, class)
  local s, cong;
  cong := ParentAttr(class);
  s := Range(cong);
  return( elm in s and
          cong!.colLookup[elm[1]] = class!.colClass and
          cong!.rowLookup[elm[3]] = class!.rowClass and
          LinkedElement(elm) in class!.nCoset );
end);

#

InstallMethod( \in,
"for Rees 0-matrix semigroup element and a congruence class by linked triple",
[IsReesZeroMatrixSemigroupElement, IsRZMSCongruenceClassByLinkedTriple],
function(elm, class)
  local s, cong;
  cong := ParentAttr(class);
  s := Range(cong);
  # Special case for 0 and {0}
  if elm = MultiplicativeZero(s) then
    return(class!.nCoset = 0);
  elif class!.nCoset = 0 then
    return false;
  fi;
  # Otherwise
  return( elm in s and
          cong!.colLookup[elm[1]] = class!.colClass and
          cong!.rowLookup[elm[3]] = class!.rowClass and
          LinkedElement(elm) in class!.nCoset );
end);

#

InstallMethod( \*,
"for two RMS congruence classes by linked triple",
[IsRMSCongruenceClassByLinkedTriple, IsRMSCongruenceClassByLinkedTriple],
function(c1, c2)
  local elm;
  if not Parent(c1) = Parent(c2) then
    Error("Semigroups: \*: usage,\n",
          "the args <c1> and <c2> must be classes of the same congruence,");
    return;
  fi;
  elm := Representative(c1) * Representative(c2);
  return( EquivalenceClassOfElementNC(Parent(c1), elm) );
end);

#

InstallMethod( \*,
"for two RZMS congruence classes by linked triple",
[IsRZMSCongruenceClassByLinkedTriple, IsRZMSCongruenceClassByLinkedTriple],
function(c1, c2)
  local elm;
  if not Parent(c1) = Parent(c2) then
    Error("Semigroups: \*: usage,\n",
          "<c1> and <c2> must be classes of the same congruence,");
    return;
  fi;
  elm := Representative(c1) * Representative(c2);
  return( EquivalenceClassOfElementNC(Parent(c1), elm) );
end);

#

InstallMethod( \*,
"for an equivalence class and a list",
[IsEquivalenceClass, IsList],
function(class, list)
  return List(list, x -> class * x);
end);

#

InstallMethod( \*,
"for a list and an equivalence class",
[IsList, IsEquivalenceClass],
function(list, class)
  return List(list, x -> x * class);
end);

#

InstallMethod(Size,
"for RMS congruence class by linked triple",
[IsRMSCongruenceClassByLinkedTriple],
function(class)
  local cong;
  cong := Parent(class);
  return( Size(cong!.n) *
          Size(cong!.colBlocks[class!.colClass]) *
          Size(cong!.rowBlocks[class!.rowClass]) );
end);

#

InstallMethod(Size,
"for RZMS congruence class by linked triple",
[IsRZMSCongruenceClassByLinkedTriple],
function(class)
  local cong;
  # Special case for {0}
  if class!.nCoset = 0 then
    return 1;
  fi;
  # Otherwise
  cong := Parent(class);
  return( Size(cong!.n) *
          Size(cong!.colBlocks[class!.colClass]) *
          Size(cong!.rowBlocks[class!.rowClass]) );
end);

#

InstallMethod( \=,
"for two congruence classes by linked triple",
[IsRMSCongruenceClassByLinkedTriple, IsRMSCongruenceClassByLinkedTriple],
function(c1, c2)
  return( c1!.nCoset = c2!.nCoset and
          c1!.colClass = c2!.colClass and
          c1!.rowClass = c2!.rowClass );
end);

#

InstallMethod( \=,
"for two congruence classes by linked triple",
[IsRZMSCongruenceClassByLinkedTriple, IsRZMSCongruenceClassByLinkedTriple],
function(c1, c2)
  # Special case for {0}
  if c1!.nCoset = 0 and c2!.nCoset = 0 then
    return true;
  fi;
  # Otherwise
  return( c1!.nCoset = c2!.nCoset and
          c1!.colClass = c2!.colClass and
          c1!.rowClass = c2!.rowClass );
end);

#

InstallMethod(CanonicalRepresentative,
"for Rees matrix semigroup congruence class by linked triple",
[IsRMSCongruenceClassByLinkedTriple],
function(class)
  local cong, s, i, u, mat, a;
  cong := Parent(class);
  s := Range(cong);
  # Pick the first row and column from the classes
  i := cong!.colBlocks[class!.colClass][1];
  u := cong!.rowBlocks[class!.rowClass][1];
  # Pick another row and column
  mat := Matrix(s);
  a := mat[1][i] ^ - 1
       * CanonicalRightCosetElement(cong!.n, Representative(class!.nCoset))
       * mat[u][1] ^ - 1;
  return RMSElement(s, i, a, u);
end);

#

InstallMethod(CanonicalRepresentative,
"for Rees 0-matrix semigroup congruence class by linked triple",
[IsRZMSCongruenceClassByLinkedTriple],
function(class)
  local cong, s, mat, i, u, v, j, a;
  cong := Parent(class);
  s := Range(cong);
  # Special case for {0}
  if class!.nCoset = 0 then
    return MultiplicativeZero(s);
  fi;
  # Pick the first row and column from the classes
  i := cong!.colBlocks[class!.colClass][1];
  u := cong!.rowBlocks[class!.rowClass][1];
  # Pick another row and column with appropriate non-zero entries
  mat := Matrix(s);
  for v in [1 .. Size(mat)] do
    if mat[v][i] <> 0 then
      break;
    fi;
  od;
  for j in [1 .. Size(mat[1])] do
    if mat[u][j] <> 0 then
      break;
    fi;
  od;
  a := mat[v][i] ^ - 1
       * CanonicalRightCosetElement(cong!.n, Representative(class!.nCoset))
       * mat[u][j] ^ - 1;
  return RMSElement(s, i, a, u);
end);

#

InstallMethod(GeneratingPairsOfMagmaCongruence,
"for Rees matrix semigroup congruence by linked triple",
[IsRMSCongruenceByLinkedTriple],
function(cong)
  local s, g, m, pairs, i1, x, bl, j, rowNo, i, colNo;
  s := Range(cong);
  g := UnderlyingSemigroup(s);
  m := Matrix(s);

  # Create a list of generating pairs
  pairs := [];

  # PAIRS FROM THE NORMAL SUBGROUP
  # for each x in the subgroup,
  # (1,x,1) is related to (1,id,1)
  for x in cong!.n do
    Add(pairs, [RMSElement(s,1,x,1),
                RMSElement(s,1,One(g),1) ] );
  od;

  # PAIRS FROM THE COLUMNS RELATION
  # For each class in the relation...
  for bl in cong!.colBlocks do
    # For each column in the class...
    for j in [2 .. Size(bl)] do
      # For each row in the matrix...
      for rowNo in [1 .. Size(m)] do
        Add(pairs,
            [RMSElement(s,bl[1],m[rowNo][bl[1]] ^ - 1,rowNo),
             RMSElement(s,bl[j],m[rowNo][bl[j]] ^ - 1,rowNo)]);
      od;
    od;
  od;

  # PAIRS FROM THE ROWS RELATION
  # For each class in the relation...
  for bl in cong!.rowBlocks do
    # For each row in the class...
    for i in [2 .. Size(bl)] do
      # For each column in the matrix...
      for colNo in [1 .. Size(m[1])] do
        Add(pairs,
            [RMSElement(s,colNo,m[bl[1]][colNo] ^ - 1,bl[1]),
             RMSElement(s,colNo,m[bl[i]][colNo] ^ - 1,bl[i])]);
      od;
    od;
  od;
  return pairs;
end);

#

InstallMethod(GeneratingPairsOfMagmaCongruence,
"for Rees 0-matrix semigroup congruence by linked triple",
[IsRZMSCongruenceByLinkedTriple],
function(cong)
  local s, g, m, pairs, i1, x, bl, j, rowNo, i, colNo;
  s := Range(cong);
  g := UnderlyingSemigroup(s);
  m := Matrix(s);

  # Create a list of generating pairs
  pairs := [];

  # PAIRS FROM THE NORMAL SUBGROUP
  # First, find a matrix entry not equal to zero
  i1 := PositionProperty(m[1], x -> x <> 0);

  # for each x in the subgroup,
  # (i1,x,1) is related to (i1,id,1)
  for x in cong!.n do
    Add(pairs, [RMSElement(s,i1,x,1),
                RMSElement(s,i1,One(g),1) ] );
  od;

  # PAIRS FROM THE COLUMNS RELATION
  # For each class in the relation...
  for bl in cong!.colBlocks do
    # For each column in the class...
    for j in [2 .. Size(bl)] do
      # For each row in the matrix...
      for rowNo in [1 .. Size(m)] do
        if m[rowNo][bl[1]] <> 0 then
          Add(pairs,
          [RMSElement(s,bl[1],m[rowNo][bl[1]] ^ - 1,rowNo),
          RMSElement(s,bl[j],m[rowNo][bl[j]] ^ - 1,rowNo)] );
        fi;
      od;
    od;
  od;

  # PAIRS FROM THE ROWS RELATION
  # For each class in the relation...
  for bl in cong!.rowBlocks do
    # For each row in the class...
    for i in [2 .. Size(bl)] do
      # For each column in the matrix...
      for colNo in [1 .. Size(m[1])] do
        if m[bl[1]][colNo] <> 0 then
          Add(pairs,
              [RMSElement(s,colNo,m[bl[1]][colNo] ^ - 1,bl[1]),
               RMSElement(s,colNo,m[bl[i]][colNo] ^ - 1,bl[i])] );
        fi;
      od;
    od;
  od;
  return pairs;
end);

#

InstallMethod(AsSemigroupCongruenceByGeneratingPairs,
"for semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  local s, pairs;
  s := Range(cong);
  pairs := GeneratingPairsOfMagmaCongruence(cong);
  return SemigroupCongruenceByGeneratingPairs(s, pairs);
end);

#

InstallMethod(AsRMSCongruenceByLinkedTriple,
"for semigroup congruence by generating pairs",
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
function(cong)
  local pairs, s, g, mat, colLookup, rowLookup, n, find, union, pair, u, v, i,
        j, normalise, colBlocks, rowBlocks;
  # Extract some information
  pairs := GeneratingPairsOfSemigroupCongruence(cong);
  s := Range(cong);
  g := UnderlyingSemigroup(s);
  mat := Matrix(s);

  # Lookup tables for the column and row equivalences
  colLookup := [1 .. Size(mat[1])];
  rowLookup := [1 .. Size(mat)];

  # Normal subgroup
  n := Subgroup(g, []);

  # Functions for union-find
  find := function(table, n)
    while table[n] <> n do
      n := table[n];
    od;
    return n;
  end;

  union := function(table, x, y)
    x := find(table, x);
    y := find(table, y);
    if x < y then
      table[y] := x;
    elif y < x then
      table[x] := y;
    fi;
  end;

  for pair in pairs do
    # If this pair adds no information, ignore it
    if pair[1] = pair[2] then
      continue;
    fi;

    # Associate the columns and rows
    union(colLookup, pair[1][1], pair[2][1]);
    union(rowLookup, pair[1][3], pair[2][3]);

    # Associate group entries in the normal subgroup
    n := ClosureGroup(n, LinkedElement(pair[1]) * LinkedElement(pair[2]) ^ - 1);

    # Ensure linkedness
    for v in [2 .. Size(mat)] do
      n := ClosureGroup( n,
                   mat[1][pair[1][1]]
                   * mat[v][pair[1][1]] ^ - 1
                   * mat[v][pair[2][1]]
                   * mat[1][pair[2][1]] ^ - 1 );
    od;
    for j in [2 .. Size(mat[1])] do
      n := ClosureGroup( n,
                   mat[pair[1][3]][1]
                   * mat[pair[2][3]][1] ^ - 1
                   * mat[pair[2][3]][j]
                   * mat[pair[1][3]][j] ^ - 1 );
    od;
  od;

  # Normalise lookup tables
  normalise := function(table)
    local ht, next, newtab, i, ii;
    ht := HTCreate(1);
    next := 1;
    newtab := [];
    for i in [1 .. Size(table)] do
      ii := find(table, i);
      newtab[i] := HTValue(ht, ii);
      if newtab[i] = fail then
        newtab[i] := next;
        HTAdd(ht, ii, next);
        next := next + 1;
      fi;
    od;
    return newtab;
  end;
  colLookup := normalise(colLookup);
  rowLookup := normalise(rowLookup);

  # Make blocks
  colBlocks := List([1 .. Maximum(colLookup)], x -> []);
  rowBlocks := List([1 .. Maximum(rowLookup)], x -> []);
  for i in [1 .. Size(colLookup)] do
    Add(colBlocks[colLookup[i]], i);
  od;
  for u in [1 .. Size(rowLookup)] do
    Add(rowBlocks[rowLookup[u]], u);
  od;

  # Make n normal
  n := NormalClosure(g,n);

  cong := RMSCongruenceByLinkedTriple(s, n, colBlocks, rowBlocks);
  SetGeneratingPairsOfMagmaCongruence(cong, pairs);
  return cong;
end);

#

InstallMethod(AsRZMSCongruenceByLinkedTriple,
"for semigroup congruence by generating pairs",
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
function(cong)
  local pairs, s, g, mat, colLookup, rowLookup, n, find, union, pair, u, v, i,
        j, normalise, colBlocks, rowBlocks;

  # Extract some information
  pairs := GeneratingPairsOfSemigroupCongruence(cong);
  s := Range(cong);
  g := UnderlyingSemigroup(s);
  mat := Matrix(s);

  # Lookup tables for the column and row equivalences
  colLookup := [1 .. Size(mat[1])];
  rowLookup := [1 .. Size(mat)];

  # Normal subgroup
  n := Subgroup(g, []);

  # Functions for union-find
  find := function(table, n)
    while table[n] <> n do
      n := table[n];
    od;
    return n;
  end;

  union := function(table, x, y)
    x := find(table, x);
    y := find(table, y);
    if x < y then
      table[y] := x;
    elif y < x then
      table[x] := y;
    fi;
  end;

  for pair in pairs do
    # If this pair adds no information, ignore it
    if pair[1] = pair[2] then
      continue;
    fi;

    # Does this relate any non-zero elements to zero?
    if pair[1] = MultiplicativeZero(s)
       or pair[2] = MultiplicativeZero(s)
       or ForAny( [1 .. Size(mat)],
               u -> (mat[u][pair[1][1]] = 0)
               <>   (mat[u][pair[2][1]] = 0) )
       or ForAny( [1 .. Size(mat[1])],
               i -> (mat[pair[1][3]][i] = 0)
               <>   (mat[pair[2][3]][i] = 0) ) then
      return UniversalSemigroupCongruence(s);
    fi;

    # Associate the columns and rows
    union(colLookup, pair[1][1], pair[2][1]);
    union(rowLookup, pair[1][3], pair[2][3]);

    # Associate group entries in the normal subgroup
    n := ClosureGroup(n, LinkedElement(pair[1]) * LinkedElement(pair[2]) ^ - 1);

    # Ensure linkedness
    u := PositionProperty([1 .. Size(mat)], u -> mat[u][pair[1][1]] <> 0);
    for v in [u + 1 .. Size(mat)] do
      if mat[v][pair[1][1]] = 0 then
        continue;
      fi;
      n := ClosureGroup( n,
                   mat[u][pair[1][1]]
                   * mat[v][pair[1][1]] ^ - 1
                   * mat[v][pair[2][1]]
                   * mat[u][pair[2][1]] ^ - 1 );
    od;
    i := PositionProperty([1 .. Size(mat[1])], k -> mat[pair[1][3]][k] <> 0);
    for j in [i + 1 .. Size(mat[1])] do
      if mat[pair[1][3]][j] = 0 then
        continue;
      fi;
      n := ClosureGroup( n,
                   mat[pair[1][3]][i]
                   * mat[pair[2][3]][i] ^ - 1
                   * mat[pair[2][3]][j]
                   * mat[pair[1][3]][j] ^ - 1 );
    od;
  od;

  # Normalise lookup tables
  normalise := function(table)
    local ht, next, newtab, i, ii;
    ht := HTCreate(1);
    next := 1;
    newtab := [];
    for i in [1 .. Size(table)] do
      ii := find(table, i);
      newtab[i] := HTValue(ht, ii);
      if newtab[i] = fail then
        newtab[i] := next;
        HTAdd(ht, ii, next);
        next := next + 1;
      fi;
    od;
    return newtab;
  end;
  colLookup := normalise(colLookup);
  rowLookup := normalise(rowLookup);

  # Make blocks
  colBlocks := List([1 .. Maximum(colLookup)], x -> []);
  rowBlocks := List([1 .. Maximum(rowLookup)], x -> []);
  for i in [1 .. Size(colLookup)] do
    Add(colBlocks[colLookup[i]], i);
  od;
  for u in [1 .. Size(rowLookup)] do
    Add(rowBlocks[rowLookup[u]], u);
  od;

  # Make n normal
  n := NormalClosure(g,n);

  cong := RZMSCongruenceByLinkedTriple(s, n, colBlocks, rowBlocks);
  SetGeneratingPairsOfMagmaCongruence(cong, pairs);
  return cong;
end);

#
