BindGlobal("LinkedElement",
function(elt)
  local mat, i, u, v, j;
  mat := Matrix(ReesMatrixSemigroupOfFamily(FamilyObj(elt)));
  i := elt[1];  # Column no
  u := elt[3];  # Row no
  for v in [1..Size(mat)] do
    if mat[v][i] <> 0 then break; fi;
  od;
  for j in [1..Size(mat[1])] do
    if mat[u][j] <> 0 then break; fi;
  od;
  return(mat[v][i] * elt[2] * mat[u][j]);
end);

#

InstallMethod(CanonicalRepresentative,
"for a Rees 0-matrix semigroup congruence class by linked triple",
[IsRMSCongruenceClassByLinkedTriple],
function(class)
  local cong, s, mat, i, u, v, j, a;
  cong := Parent(class);
  s := Range(cong);
  mat := Matrix(s);
  # Pick the first row and column from the classes
  i := cong!.colBlocks[class!.colClass][1];
  u := cong!.rowBlocks[class!.rowClass][1];
  # Pick another row and column with appropriate non-zero entries
  for v in [1..Size(mat)] do
    if mat[v][i] <> 0 then break; fi;
  od;
  for j in [1..Size(mat[1])] do
    if mat[u][j] <> 0 then break; fi;
  od;
  #TODO: Representative(coset) is NOT CANONICAL, so this function is wrong
  a := mat[v][i]^-1 * Representative(class!.nCoset) * mat[u][j]^-1;
  return ReesZeroMatrixSemigroupElement(s, i, a, u);
end);

#

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

InstallMethod(CongruencesOfSemigroup,
"for a finite 0-simple Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup and IsZeroSimpleSemigroup and IsFinite],
function(s)
  congs := [];
  mat := Matrix(s);
  g := UnderlyingSemigroup(s);
  
  # Construct maximum column relation
  pairs := [];
  for i in [1..Size(mat[1])] do
    for j in [i+1..Size(mat[1])] do
      if Forall([1..Size(mat)], u->( (mat[u][i]=0) = (mat[u][j]=0) ) ) then
        Add(pairs,[i,j]);
      fi;
    od;
  od;
  maxColRel := EquivalenceRelationByPairs(Domain([1..Size(mat[1])]), pairs);
  # Construct maximum row relation
  pairs := [];
  for u in [1..Size(mat)] do
    for v in [u+1..Size(mat)] do
      if Forall([1..Size(mat[1])], i->( (mat[u][i]=0) = (mat[v][i]=0) ) ) then
        Add(pairs,[u,v]);
      fi;
    od;
  od;
  maxRowRel := EquivalenceRelationByPairs(Domain([1..Size(mat)]), pairs);
  
  for n in NormalSubgroups(g) do
    
  od;
  
  return congs;
end);

#

InstallGlobalFunction(RMSCongruenceByLinkedTriple,
[IsReesZeroMatrixSemigroup and IsFinite,
 IsGroup, IsDenseList, IsDenseList],
function(s, n, colBlocks, rowBlocks)
  local g, mat, block, i, j, u, v, bi, bj, bu, bv;
  mat := Matrix(s);
  g := UnderlyingSemigroup(s);
  
  # Basic checks
  if not IsNormal(g,n) then
    Error("2nd argument <n> must be a normal subgroup,"); return;
  fi;
  if not ForAll(colBlocks, IsList) then
    Error("3rd argument <colBlocks> must be a list of lists,"); return;
  fi;
  if not ForAll(rowBlocks, IsList) then
    Error("4th argument <rowBlocks> must be a list of lists,"); return;
  fi;
  if SortedList(Flat(colBlocks)) <> [1..Size(mat[1])] then
    Error("3rd argument <colBlocks> must be a partition ",
          "of the columns of the matrix of <s>,"); return;
  fi;
  if SortedList(Flat(rowBlocks)) <> [1..Size(mat)] then
    Error("4th argument <rowBlocks> must be a partition ",
          "of the rows of the matrix of <s>,"); return;
  fi;
  
  # Check axioms (L1) and (L2) from Howie p.86, then call NC function
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

InstallMethod(EquivalenceClasses,
"for a Rees 0-matrix semigroup congruence by linked triple",
[IsRMSCongruenceByLinkedTriple],
function(cong)
  local list, s, g, n, colBlocks, rowBlocks, colClass, rowClass, rep, elt;
  list := [];
  s := Range(cong);
  g := UnderlyingSemigroup(s);
  n := cong!.n;
  colBlocks := cong!.colBlocks;
  rowBlocks := cong!.rowBlocks;
  for colClass in [1..Size(colBlocks)] do
    for rowClass in [1..Size(rowBlocks)] do
      for rep in List(RightCosets(g,n), Representative) do
        elt := ReesZeroMatrixSemigroupElement(
                       s, colBlocks[colClass][1], rep, rowBlocks[rowClass][1] );
        # nCoset := RightCoset(n, LinkedElement(elt));
        # Could be faster?
        Add(list, EquivalenceClassOfElement(cong, elt));
      od;
    od;
  od;
  return list;
end);

#

InstallMethod(RMSCongruenceClassByLinkedTriple,
"for a semigroup congruence by linked triple, a coset and two positive integers",
[IsRMSCongruenceByLinkedTriple,
 IsRightCoset, IsPosInt, IsPosInt],
function(cong, nCoset, colClass, rowClass)
  #TODO: Checks
  return RMSCongruenceClassByLinkedTripleNC(cong, nCoset, colClass, rowClass);
end);

#

InstallMethod(RMSCongruenceClassByLinkedTripleNC,
"for a semigroup congruence by linked triple, a coset and two positive integers",
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

InstallMethod(EquivalenceClassOfElement,
"for a Rees 0-matrix semigroup congruence by linked triple and a Rees 0-matrix semigroup element",
[IsRMSCongruenceByLinkedTriple, IsReesZeroMatrixSemigroupElement],
function(cong, elt)
  local fam, class, mat, nCoset, colClass, rowClass;
  # Check that the arguments make sense
  if not elt in Range(cong) then
    Error("usage: 2nd argument <elt> should be ",
          "in the semigroup of 1st argument <cong>");
    return;
  fi;
  
  # Construct the object
  fam := CollectionsFamily( FamilyObj(elt));
  if elt = MultiplicativeZero(Range(cong)) then
    class := Objectify(NewType(fam, IsEquivalenceClass and
                                    IsEquivalenceClassDefaultRep),
                       rec(nCoset := 0) );
    SetAsSSortedList([elt]);
  else
    nCoset := RightCoset(cong!.n, LinkedElement(elt));
    colClass := cong!.colLookup[elt[1]];
    rowClass := cong!.rowLookup[elt[3]];
    class := Objectify(NewType(fam, IsRMSCongruenceClassByLinkedTriple),
                       rec(nCoset := nCoset,
                           colClass := colClass,
                           rowClass := rowClass) );
  fi;
  SetParentAttr(class, cong);
  SetRepresentative(class, elt);
  return class;
end);

#

InstallMethod( \in,
"for a Rees 0-matrix semigroup element and a congruence class by linked triple",
[IsReesZeroMatrixSemigroupElement, IsRMSCongruenceClassByLinkedTriple],
function(elt, class)
  local s, cong;
  cong := ParentAttr(class);
  s := Range(cong);
  return( elt in s and
          cong!.colLookup[elt[1]] = class!.colClass and
          cong!.rowLookup[elt[3]] = class!.rowClass and
          LinkedElement(elt) in class!.nCoset );
end);

#

InstallMethod(Size,
"for a congruence class by linked triple",
[IsRMSCongruenceClassByLinkedTriple],
function(class)
  local cong;
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

#UnbindGlobal("LinkedElement");