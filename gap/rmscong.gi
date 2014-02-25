InstallMethod(ViewObj,
"for Rees zero-matrix semigroup congruence by linked triple",
[IsRMSCongruenceByLinkedTriple],
function(cong)
  local rows, cols;
  rows := Size(Rows(Range(cong)));
  cols := Size(Columns(Range(cong)));
  Print("<semigroup congruence by linked triple on ",
        rows, "x", cols, " Rees 0-matrix semigroup>");
end);

#

InstallGlobalFunction(RMSCongruenceByLinkedTriple,
[IsReesZeroMatrixSemigroup and IsFinite,
 IsGroup, IsDenseList, IsDenseList],
# Check axioms (L1) and (L2) from Howie p.86, then call NC function
function(s, n, colBlocks, rowBlocks)
  local mat, block, i, j, u, v, bi, bj, bu, bv;
  mat := Matrix(s);
  # Go through the column blocks
  for block in colBlocks do
    for bj in [2..Size(block)] do
      # Check columns have zeroes in all the same rows (L1)
      for u in [1..Size(mat)] do
        if (mat[u][block[1]] = 0) <> (mat[u][block[bj]] = 0) then
          Error("columns in the same block of 3rd argument <colBlocks> ",
                "must have zeroes in precisely the same rows,");
          return;
        fi;
      od;
    od;
    # Check q-condition for all pairs of rows in this block (L2)
    for bi in [1..Size(block)] do
      for bj in [bi+1..Size(block)] do
        i := block[bi];
        j := block[bj];
        # Check all pairs of rows (u,v)
        for u in [1..Size(mat)] do
          if mat[u][i] = 0 then continue; fi;
          for v in [u+1..Size(mat)] do
            if mat[v][i] = 0 then continue; fi;
            if not (mat[u][i]*mat[v][i]^-1*mat[v][j]*mat[u][j]^-1) in n then
              Error("not a valid linked triple for this semigroup's matrix, ",
                    "cols ",i," and ",j,", rows ",u," and ",v,",");
            fi;
          od;
        od;
      od;
    od;
  od;
  
  # Go through the row blocks
  for block in rowBlocks do
    for bv in [2..Size(block)] do
      # Check rows have zeroes in all the same columns (L1)
      for i in [1..Size(mat[1])] do
        if (mat[block[1]][i] = 0) <> (mat[block[bv]][i] = 0) then
          Error("rows in the same block of 4th argument <rowBlocks> ",
                "must have zeroes in precisely the same columns,");
          return;
        fi;
      od;
    od;
    # Check q-condition for all pairs of columns in this block (L2)
    for bu in [1..Size(block)] do
      for bv in [bi+1..Size(block)] do
        u := block[bu];
        v := block[bv];
        # Check all pairs of columns (i,j)
        for i in [1..Size(mat[1])] do
          if mat[u][i] = 0 then continue; fi;
          for j in [i+1..Size(mat[1])] do
            if mat[u][j] = 0 then continue; fi;
            if not (mat[u][i]*mat[v][i]^-1*mat[v][j]*mat[u][j]^-1) in n then
              Error("not a valid linked triple for this semigroup's matrix, ",
                    "rows ",u," and ",v,", cols ",i," and ",j,",");
            fi;
          od;
        od;
      od;
    od;
  od;
  return RMSCongruenceByLinkedTripleNC(s, n, colBlocks, rowBlocks);
end);

#

InstallGlobalFunction(RMSCongruenceByLinkedTripleNC,
[IsReesZeroMatrixSemigroup and IsFinite,
 IsGroup, IsDenseList, IsDenseList],
function(s, n, colBlocks, rowBlocks)
  local fam, cong, colLookup, rowLookup, i, j;
  # Calculate lookup table for equivalence relations
  colLookup := [];
  rowLookup := [];
  for i in [1..Length(colBlocks)] do
    for j in colBlocks[i] do
      colLookup[j] := i;
    od;
  od;
  for i in [1..Length(rowBlocks)] do
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

InstallMethod(\in,
"for a Rees 0-matrix semigroup element collection and a semigroup congruence by linked triple",
[IsReesZeroMatrixSemigroupElementCollection, IsRMSCongruenceByLinkedTriple],
function(pair, cong)
  local s, mat,
        row, col, rows, cols,
        a, i, u, j, b, v,
        gpElt;
  
  # Check for validity
  if Size(pair) <> 2 then
    Error("usage: 1st argument <pair> must be a list of length 2,");
    return;
  fi;
  s := Range(cong);
  if not ForAll(pair, x-> x in s) then
    Error("usage: the elements of the 1st argument <pair> ",
          "must be in the range of the 2nd argument <cong>,");
    return;
  fi;
  
  # Handling the case when one or more of the pair are zero
  if pair[1] = pair[2] then
    return true;
  elif MultiplicativeZero(s) in pair then
    return false;
  fi;
  
  # Read the elements as (i,a,u) and (j,b,v)
  i := pair[1][1]; a := pair[1][2]; u := pair[1][3];
  j := pair[2][1]; b := pair[2][2]; v := pair[2][3];
  
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
  col := PositionProperty(rows[u], x-> x <> 0);
  row := PositionProperty(cols[i], x-> x <> 0);
  gpElt := mat[row][i] * a * mat[u][col] *
           Inverse(mat[row][j] * b * mat[v][col]);
  return(gpElt in cong!.n);
end);

#

InstallMethod(EquivalenceClassOfElement,
"for a Rees 0-matrix semigroup congruence by linked triple and a Rees 0-matrix semigroup element",
[IsRMSCongruenceByLinkedTriple, IsReesZeroMatrixSemigroupElement],
function(cong, elt)
  if not elt in Range(cong) then
    Error("usage: 2nd argument <elt> should be ",
          "in the semigroup of 1st argument <cong>");
    return;
  fi;
  # CODE GOES HERE
end);

#

InstallMethod(SemigroupCongruenceByLinkedTriple,
"for a Rees zero matrix semigroup and a linked triple",
[IsReesZeroMatrixSemigroup and IsFinite,
 IsGroup,
 IsDenseList,
 IsDenseList],
function(s, n, colRel, rowRel)
  local g, m, pairs,
        IsRegularMatrix,
        i1, x,
        c, i, j,
        rowNo, colNo;
  g := UnderlyingSemigroup(s);
  m := Matrix(s);
  
  IsRegularMatrix := function(m)
    # Checks no row or column is all-zero
    local row;
    if not IsRegularSemigroup(UnderlyingSemigroup(s)) then
      return false;
    fi;
    for row in Union(m, TransposedMat(m)) do
      if ForAll(row, x-> x=0) then
        return false;
      fi;
    od;
    return true;
  end;
  
  # Check that the arguments are valid
  if not (IsRegularMatrix(m) and IsGroup(g)) then
    Error("usage: the argument must be a Rees 0-matrix semigroup with regular matrix over a group");
    return;
  fi;
  if not IsSubgroup(g,n) and IsNormal(g,n) then
    return fail;
  fi;
  
  # Create a list of generating pairs
  pairs := [];
  
  # PAIRS FROM THE NORMAL SUBGROUP
  # First, find a matrix entry not equal to zero
  i1 := PositionProperty(m[1],x-> x<>0);
  
  # for each x in the subgroup,
  # (i1,x,1) is related to (i1,id,1)
  for x in n do
    Add(pairs, [
            ReesZeroMatrixSemigroupElement(s,i1,x,1),
            ReesZeroMatrixSemigroupElement(s,i1,One(g),1) ] );
  od;
  
  # PAIRS FROM THE COLUMNS RELATION
  # For each class in the relation...
  for c in colRel do
    # For each column in the class...
    for j in [2..Size(c)] do
      # For each row in the matrix...
      for rowNo in [1..Size(m)] do
        if m[rowNo][c[1]] <> 0 then
          Add(pairs, [
                  ReesZeroMatrixSemigroupElement(s,c[1],Inverse(m[rowNo][c[1]]),rowNo),
                  ReesZeroMatrixSemigroupElement(s,c[j],Inverse(m[rowNo][c[j]]),rowNo) ] );
        fi;
      od;
    od;
  od;
  
  # PAIRS FROM THE ROWS RELATION
  # For each class in the relation...
  for c in rowRel do
    # For each row in the class...
    for i in [2..Size(c)] do
      # For each column in the matrix...
      for colNo in [1..Size(m[1])] do
        if m[c[1]][colNo] <> 0 then
          Add(pairs, [
                  ReesZeroMatrixSemigroupElement(s,colNo,Inverse(m[c[1][colNo]]),c[1]),
                  ReesZeroMatrixSemigroupElement(s,colNo,Inverse(m[c[i][colNo]]),c[i]) ] );
        fi;
      od;
    od;
  od;
  return SemigroupCongruenceByGeneratingPairs(s, pairs);
end);

#

InstallMethod(LinkedTriple,
"for a semigroup congruence over a finite 0-simple semigroup",
[IsSemigroupCongruence],
function(cong)
  local n, colRel, rowRel,
        s, m, g, p,
        i1, foo, class, rmsElts, gpElts,
        zeroMask, pairs,
        cols, rows,
        colNo, rowNo,
        i, j, la, mu;
  # Extract some information
  s := Range(cong);
  m := IsomorphismReesMatrixSemigroup(s);
  g := UnderlyingSemigroup(m);
  p := Matrix(m);
  # Checks
  if not (IsZeroSimpleSemigroup(s) or IsSimpleSemigroup(s)) then
    Error("usage: the argument must be a finite 0-simple semigroup");
    return;
  fi;
  
  # FIND THE NORMAL SUBGROUP N
  # First find a matrix entry not equal to zero
  i1 := PositionProperty(m[1],x-> x<>0);
  # N consists of all the x s.t. (i1,x,1) is related to (i1,id,1)
  foo := ReesZeroMatrixSemigroupElement(m, i1, One(g), 1);
  class := EquivalenceClassOfElement(cong, foo);
  rmsElts := Filtered(Elements(class), elt->(elt[1]=1 and elt[3]=1));
  gpElts := List(rmsElts, elt->elt[2]);
  n := Subgroup(g, gpElts);
  
  # FIND THE RELATION ON THE SET OF COLUMNS
  cols := TransposedMat(m);
  zeroMask := List(cols, col-> List(col, x-> x<>0));
  pairs := [];
  for i in [1..Size(cols)] do
    for j in Filtered([i+1..Size(cols)], j-> zeroMask[i]=zeroMask[j]) do
      for rowNo in Filtered([1..Size(cols[1])], zeroMask[i]) do
        if ReesZeroMatrixSemigroupElement(m, i, Inverse(p[rowNo][i]), rowNo) =
           ReesZeroMatrixSemigroupElement(m, j, Inverse(p[rowNo][j]), rowNo) then
          Add(pairs, [i,j]);
        fi;
      od;
    od;
  od;
  colRel := EquivalenceRelationByPairsNC([1..Size(cols)], pairs);
  
  # FIND THE RELATION ON THE SET OF ROWS
  rows := m;
  zeroMask := List(rows, row-> List(row, x-> x<>0));
  pairs := [];
  for la in [1..Size(rows)] do
    for mu in Filtered([la+1..Size(rows)], mu-> zeroMask[la]=zeroMask[mu]) do
      for colNo in Filtered([1..Size(rows[1])], zeroMask[la]) do
        if ReesZeroMatrixSemigroupElement(m, colNo, Inverse(p[la][colNo]), la) =
           ReesZeroMatrixSemigroupElement(m, colNo, Inverse(p[mu][colNo]), mu) then
          Add(pairs, [la, mu]);
        fi;
      od;
    od;
  od;
  rowRel := EquivalenceRelationByPairsNC([1..Size(rows)], pairs);
  
  return [n, colRel, rowRel];
end);

#