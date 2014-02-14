InstallMethod(LinkedTriples, "for a Rees matrix semigroup",
[IsReesMatrixSemigroup],
#TODO
function(s)
  return fail;
end);

#

InstallMethod(LinkedTriples, "for a Rees zero matrix semigroup",
[IsReesZeroMatrixSemigroup and IsZeroSimpleSemigroup and IsFinite],
#TODO
function(s)
  local g;
  g := UnderlyingSemigroup(s);
  if not IsGroup(g) then
    return fail;
  fi;
  return fail;
end);

#

InstallMethod(SemigroupCongruenceByLinkedTriple,
"for a Rees zero matrix semigroup and a linked triple",
[IsReesZeroMatrixSemigroup and IsFinite,
 IsGroup,
 IsDenseList,
 IsDenseList],
function(s, n, colCong, rowCong)
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
  
  # PAIRS FROM THE COLUMNS CONGRUENCE
  # For each class in the congruence...
  for c in colCong do
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
  
  # PAIRS FROM THE ROWS CONGRUENCE
  # For each class in the congruence...
  for c in rowCong do
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
"for a semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  local n, colCong, rowCong,
        s, m, g, p,
        i1, foo, class, rmsElts, gpElts;
  # Extract some information
  s := Range(cong);
  m := IsomorphismReesMatrixSemigroup(s);
  g := UnderlyingSemigroup(m);
  p := Matrix(m);
  # Checks
  if not IsGroup(g) then
    return fail;
  fi;
  if not (IsZeroSimpleSemigroup(s) or IsSimpleSemigroup(s)) then
    return fail;
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
  
  #TODO
  colCong := [];
  rowCong := [];
  return [n, colCong, rowCong];
end);

#

# InstallMethod(\in,
# "for an associative element collection and a semigroup congruence with linked triple",
# [IsAssociativeElementCollection,
#  IsSemigroupCongruence and HasLinkedTriple],
# function(pair, cong)
#   return fail;
# end);
