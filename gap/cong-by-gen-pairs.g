CongByGenPairs:=function(S, pairs)
  local elts, right, left, cong, find, union, o, ht, treehashsize, x, i, nr, 
        genstoapply, j, y, normalise;
  
  elts:=Elements(S);
  right:=RightCayleyGraphSemigroup(S);
  left:=LeftCayleyGraphSemigroup(S);
  
  cong:=[1..Size(S)];

  find:=function(i)
    while cong[i]<>i do 
      i:=cong[i];
    od;
    return i;
  end;

  union:=function(pair)
    local ii, jj; 

    ii:=find(pair[1]);
    jj:=find(pair[2]);

#    if ii<jj then 
#      cong[pair[2]]:=ii;
#    elif jj<ii then 
#      cong[pair[1]]:=jj;
#    fi;
    if ii<jj then 
      cong[jj]:=ii;
    elif jj<ii then 
      cong[ii]:=jj;
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
  genstoapply:=[1..Length(GeneratorsOfSemigroup(S))]; # take care if S is a
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
#  return rec(l:=List(cong, find),
#             lookup:=normalise(cong),
#             ht:=ht);
  return normalise(cong);
end;