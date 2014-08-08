AsCongByLinkedTriple := function(cong)
  local pairs, s, g, mat, colLookup, rowLookup, n, find, union, pair, u, v, i, 
        j, normalise, colBlocks, rowBlocks;
  
  # Extract some information
  pairs := GeneratingPairsOfSemigroupCongruence(cong);
  s := Range(cong);
  g := UnderlyingSemigroup(s);
  mat := Matrix(s);
  
  # Lookup tables for the column and row equivalences
  colLookup := [1..Size(mat[1])];
  rowLookup := [1..Size(mat)];
  
  # Normal subgroup
  n := Subgroup(g, []);
  
  # Functions for union-find
  find := function(table, n)
    while table[n]<>n do 
      n:=table[n];
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
       or ForAny( [1..Size(mat)],
               u -> (mat[u][pair[1][1]] = 0) 
               <>   (mat[u][pair[2][1]] = 0) )
       or ForAny( [1..Size(mat[1])],
               i -> (mat[pair[1][3]][i] = 0)
               <>   (mat[pair[2][3]][i] = 0) ) then
      return UniversalSemigroupCongruence(s);
    fi;
    
    # Associate the columns and rows
    union(colLookup, pair[1][1], pair[2][1]);
    union(rowLookup, pair[1][3], pair[2][3]);
    
    # Associate group entries in the normal subgroup
    n := ClosureGroup(n, LinkedElement(pair[1]) * LinkedElement(pair[2])^-1);
#    row1 := Position([1..Size(mat)], w-> mat[w][pair[1][1]]<>0);
#    col1 := Position([1..Size(mat[1])], k-> mat[pair[1][3]][k]<>0);
    
    # Ensure linkedness
    u := PositionProperty([1..Size(mat)], u-> mat[u][pair[1][1]]<>0);
    if mat[u][pair[1][1]] = 0 then continue; fi;
    for v in [u+1..Size(mat)] do
      if mat[v][pair[1][1]] = 0 then
        continue;
      fi;
      n := ClosureGroup( n,
                   mat[u][pair[1][1]]
                   * mat[v][pair[1][1]] ^-1
                   * mat[v][pair[2][1]]
                   * mat[u][pair[2][1]] ^-1 );
    od;
    i := PositionProperty([1..Size(mat[1])], k-> mat[pair[1][3]][k]<>0);
    if mat[pair[1][3]][i] = 0 then continue; fi;
    for j in [i+1..Size(mat[1])] do
      if mat[pair[1][3]][j] = 0 then continue; fi;
      n := ClosureGroup( n,
                   mat[pair[1][3]][i]
                   * mat[pair[2][3]][i] ^-1
                   * mat[pair[2][3]][j]
                   * mat[pair[1][3]][j] ^-1 );
    od;
  od;
  
  # Normalise lookup tables
  normalise := function(table)
    local ht, next, i, ii;
    ht := HTCreate(1);
    next := 1;
    for i in [1..Size(table)] do
      ii := find(table, i);
      table[i] := HTValue(ht, ii);
      if table[i] = fail then
        table[i] := next;
        HTAdd(ht, ii, next);
        next := next + 1;
      fi;
    od;
  end;
  normalise(colLookup);
  normalise(rowLookup);
  
  # Make blocks
  colBlocks := List([1..Maximum(colLookup)], x->[]);
  rowBlocks := List([1..Maximum(rowLookup)], x->[]);
  for i in [1..Size(colLookup)] do
    Add(colBlocks[colLookup[i]], i);
  od;
  for u in [1..Size(rowLookup)] do
    Add(rowBlocks[rowLookup[u]], u);
  od;
  
  # Make n normal
  n := NormalClosure(g,n);
  
  return RZMSCongruenceByLinkedTriple(s, n, colBlocks, rowBlocks);
end;
