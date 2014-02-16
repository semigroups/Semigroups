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
  colRel := EquivalenceRelationByPairsNC([1..Size(cols)], pairs); #TODO: Find out if this is okay
  
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
  rowRel := EquivalenceRelationByPairsNC([1..Size(rows)], pairs); #TODO: Find out if this is okay
  
  return [n, colRel, rowRel];
end);

#

# InstallMethod(\in,
# "for an associative element collection and a semigroup congruence with linked triple",
# [IsAssociativeElementCollection,
#  IsSemigroupCongruence and HasLinkedTriple],
# function(pair, cong)
#   return fail;
# end);
