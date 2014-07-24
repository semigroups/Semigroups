InstallMethod(\in,
"for dense list and semigroup congruence",
[IsDenseList, IsSemigroupCongruence],
function(pair, cong)
  local s, enum, p1, p2, table, pairs, right, left, find, union, o, ht, 
        treehashsize, x, i, nr, genstoapply, j, y;
  
  # Input checks
  if not Size(pair) = 2 then
    Error("1st arg <pair> must be a list of length 2,"); return;
  fi;
  s := Range(cong);
  if not (pair[1] in s and pair[2] in s) then
    Error("Elements of <pair> must be in range of <cong>,"); return;
  fi;
  
  enum:=Enumerator(s);
  p1 := Position(enum, pair[1]);
  p2 := Position(enum, pair[2]);

  # Use lookup table if available
  if "AsLookupTable" in KnownAttributesOfObject(cong) then
    table := AsLookupTable(cong);
    return table[p1] = table[p2];
  fi;
  
  # Otherwise, begin calculating the lookup table and look for this pair
  pairs := GeneratingPairsOfSemigroupCongruence(cong);
  right:=RightCayleyGraphSemigroup(s);
  left:=LeftCayleyGraphSemigroup(s);
  
  table:=[1..Size(s)];
  
  find:=function(i)
    while table[i]<>i do 
      i:=table[i];
    od;
    return i;
  end;
  
  union:=function(pair)
    local ii, jj; 
    
    ii:=find(pair[1]);
    jj:=find(pair[2]);
    
    if ii<jj then 
      table[jj]:=ii;
    elif jj<ii then 
      table[ii]:=jj;
    fi;
    
    # Have we found our pair?
    return find(p1) = find(p2);
  end;
  
  o:=List(pairs, x-> [Position(enum, x[1]), Position(enum, x[2])]);
  
  ht:=HTCreate(o[1], rec(forflatplainlists:=true,
              treehashsize:=100003));
  for x in o do 
    HTAdd(ht, x, true);
    if union(x) then
      return true;
    fi;
  od;
  
  i:=0; nr:=Length(o);
  genstoapply:=[1..Length(GeneratorsOfSemigroup(s))]; # take care if s is a
                                                      # monoid!!
  while i<nr do 
    i:=i+1;
    x:=o[i];
    for j in genstoapply do 
      y := [right[x[1]][j], right[x[2]][j]];
      if HTValue(ht, y) = fail and
         HTValue(ht, [y[2], y[1]]) = fail then 
        HTAdd(ht, y, true);
        nr:=nr+1;
        o[nr]:=y;
        # Use the return value of union
        if union(o[nr]) then
          return true;
        fi;
      fi;
      
      y := [left[x[1]][j], left[x[2]][j]];
      if HTValue(ht, y) = fail and
         HTValue(ht, [y[2], y[1]]) = fail then 
        HTAdd(ht, y, true);
        nr:=nr+1;
        o[nr]:=y;
        if union(o[nr]) then
          return true;
        fi;
      fi;
    od;
  od;
  
  SetAsLookupTable(cong, table);
  return false;
end);

#

InstallMethod(AsLookupTable,
"for semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  local pairs, s, elts, right, left, table, find, union, o, ht, treehashsize, x, 
        i, nr, genstoapply, j, y, normalise;
  
  pairs := GeneratingPairsOfSemigroupCongruence(cong);
  s := Range(cong);
  elts:=Elements(s);
  right:=RightCayleyGraphSemigroup(s);
  left:=LeftCayleyGraphSemigroup(s);
  
  table:=[1..Size(s)];

  find:=function(i)
    while table[i]<>i do 
      i:=table[i];
    od;
    return i;
  end;

  union:=function(pair)
    local ii, jj; 

    ii:=find(pair[1]);
    jj:=find(pair[2]);

    if ii<jj then 
      table[jj]:=ii;
    elif jj<ii then 
      table[ii]:=jj;
    fi;

  end;

  o:=List(pairs, x-> [Position(elts, x[1]), Position(elts, x[2])]);
  
  ht:=HTCreate(o[1], rec(forflatplainlists:=true,
    treehashsize:=100003));
  for x in o do 
    HTAdd(ht, x, true);
    union(x);
  od;

  i:=0; nr:=Length(o);
  genstoapply:=[1..Length(GeneratorsOfSemigroup(s))]; # take care if s is a
                                                      # monoid!!
  while i<nr do 
    i:=i+1;
    x:=o[i];
    for j in genstoapply do 
      y := [right[x[1]][j], right[x[2]][j]];
      if HTValue(ht, y) = fail and
         HTValue(ht, [y[2], y[1]]) = fail then 
        HTAdd(ht, y, true);
        nr:=nr+1;
        o[nr]:=y;
        union(o[nr]);
      fi;

      y := [left[x[1]][j], left[x[2]][j]];
      if HTValue(ht, y) = fail and
         HTValue(ht, [y[2], y[1]]) = fail then 
        HTAdd(ht, y, true);
        nr:=nr+1;
        o[nr]:=y;
        union(o[nr]);
      fi;
    od;
  od;

  normalise := function(table)
    local ht, next, i, ii;
    ht := HTCreate(1);
    next := 1;
    for i in [1..Size(table)] do
      ii := find(i);
      table[i] := HTValue(ht, ii);
      if table[i] = fail then
        table[i] := next;
        HTAdd(ht, ii, next);
        next := next + 1;
      fi;
    od;
    return table;
  end;
  
  return normalise(table);
end);