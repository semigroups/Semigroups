InstallMethod(\in,
"for dense list and semigroup congruence",
[IsDenseList, IsSemigroupCongruence],
function(pair, cong)
  local s, enum, p1, p2, table, find;
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
  if HasAsLookupTable(cong) then
    table := AsLookupTable(cong);
    return table[p1] = table[p2];
  else
    # Otherwise, begin calculating the lookup table and look for this pair
    find:=function(table,i)
      while table[i]<>i do 
        i:=table[i];
      od;
      return i;
    end;
    return Enumerate(cong, table->find(table,p1)=find(table,p2));
  fi;
end);

#

InstallMethod(AsLookupTable,
"for semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  Enumerate(cong, x->false);
  return AsLookupTable(cong);
end);

#

InstallMethod(Enumerate,
"for a semigroup congruence and a function",
[IsSemigroupCongruence, IsFunction],
function(cong, lookfunc)
  local s, enum, pairs, right, left, table, find, union, o, ht, treehashsize, x, 
        i, nr, genstoapply, j, y, normalise, result;
  
  s := Range(cong);
  enum := Enumerator(s);
  
  # Begin calculating the lookup table
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
    
    # Have we found what we were looking for?
    return lookfunc(table);
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
      if y[1] <> y[2] and                       # Ignore a=b (reflexive)
         HTValue(ht, y) = fail and              # Check for (a,b)
         HTValue(ht, [y[2], y[1]]) = fail then  # Check for (b,a) (symmetric)
        HTAdd(ht, y, true);
        nr:=nr+1;
        o[nr]:=y;
        # Use the return value of union
        if union(o[nr]) then
          return true;
        fi;
      fi;
      
      y := [left[x[1]][j], left[x[2]][j]];
      if y[1] <> y[2] and
         HTValue(ht, y) = fail and
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
  
  normalise := function(cong)
    local ht, next, i, ii;
    ht := HTCreate(1);
    next := 1;
    for i in [1..Size(cong)] do
      ii := find(i);
      cong[i] := HTValue(ht, ii);
      if cong[i] = fail then
        cong[i] := next;
        HTAdd(ht, ii, next);
        next := next + 1;
      fi;
    od;
    return cong;
  end;
  
  result := lookfunc(table);
  SetAsLookupTable(cong, normalise(table));
  return result;
end);