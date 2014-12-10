InstallGlobalFunction(SetupCongData,
function(cong)
  local s, elms, pairs, ht, treehashsize, pair, lookup, pairstoapply;
  s := Range(cong);
  elms := Elements(s);
  pairs := List( GeneratingPairsOfSemigroupCongruence(cong),
                 x-> [Position(elms, x[1]), Position(elms, x[2])] );
  ht:=HTCreate( pairs[1], rec(forflatplainlists:=true,
              treehashsize:=100003) );
  cong!.data := rec( cong := cong,
                     lookup := [1..Size(s)],
                     pairstoapply := pairs,
                     ht := ht,
                     elms := elms );
  return;
end);

#

InstallMethod(\in,
"for dense list and semigroup congruence",
[IsDenseList, IsSemigroupCongruence],
function(pair, cong)
  local s, elms, p1, p2, table, find;
  # Input checks
  if not Size(pair) = 2 then
    Error("1st arg <pair> must be a list of length 2,"); return;
  fi;
  s := Range(cong);
  if not (pair[1] in s and pair[2] in s) then
    Error("Elements of <pair> must be in range of <cong>,"); return;
  fi;
  
  if not IsBound(cong!.data) then
    SetupCongData(cong);
  fi;
  elms:=cong!.data.elms;
  p1 := Position(elms, pair[1]);
  p2 := Position(elms, pair[2]);

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
  local s, elms, data, table, pairstoapply, ht, right, left, find, union, 
        genstoapply, i, nr, x, j, y, normalise, result;
  
  if not IsBound(cong!.data) then
    SetupCongData(cong);
  fi;
  
  s := Range(cong);
  data := cong!.data;
  
  table := data.lookup;
  pairstoapply := data.pairstoapply;
  ht := data.ht;
  
  right:=RightCayleyGraphSemigroup(s);
  left:=LeftCayleyGraphSemigroup(s);
  
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
  
  genstoapply:=[1..Size(right[1])];
  i := 0; nr := Size(pairstoapply);
  while i<nr do
    # Have we found what we were looking for?
    if lookfunc(table) then
      # Save our place
      data.pairstoapply := pairstoapply{[i+1..nr]};
      return true;
    fi;
    
    i:=i+1;
    x:=pairstoapply[i];
    # Add the pair itself
    if x[1] <> x[2] and HTValue(ht, x) = fail then
      HTAdd(ht, x, true);
      union(x);
    fi;
    for j in genstoapply do 
      # Add the pair's left-multiples
      y := [right[x[1]][j], right[x[2]][j]];
      if y[1] <> y[2] and HTValue(ht, y) = fail then
        HTAdd(ht, y, true);
        nr:=nr+1;
        pairstoapply[nr]:=y;
        union(y);
      fi;
      
      # Add the pair's right-multiples
      y := [left[x[1]][j], left[x[2]][j]];
      if y[1] <> y[2] and HTValue(ht, y) = fail then
        HTAdd(ht, y, true);
        nr:=nr+1;
        pairstoapply[nr]:=y;
        union(y);
      fi;
    od;
  od;
  
  normalise := function(cong)
    local ht, next, i, ii, newcong;
    ht := HTCreate(1);
    next := 1;
    newcong := [];
    for i in [1..Size(cong)] do
      ii := find(i);
      newcong[i] := HTValue(ht, ii);
      if newcong[i] = fail then
        newcong[i] := next;
        HTAdd(ht, ii, next);
        next := next + 1;
      fi;
    od;
    return newcong;
  end;
  
  result := lookfunc(table);
  SetAsLookupTable(cong, normalise(table));
  Unbind(cong!.data);
  return result;
end);

#

InstallMethod(\=,
"for two semigroup congruences",
[IsSemigroupCongruence, IsSemigroupCongruence],
function(cong1, cong2)
  return Range(cong1) = Range(cong2) and
         Set(GeneratingPairsOfSemigroupCongruence(cong1)) =
         Set(GeneratingPairsOfSemigroupCongruence(cong2));
end);

#

InstallMethod(\*,
"for two semigroup congruence classes",
[IsCongruenceClass, IsCongruenceClass],
function(class1, class2)
  if EquivalenceClassRelation(class1) <> EquivalenceClassRelation(class2) then
    Error("usage: arguments must be classes of the same congruence,");
    return;
  fi;
  return CongruenceClassOfElement(EquivalenceClassRelation(class1),
                 Representative(class1) * Representative(class2));
end);

#
