45, 58, 76, 95, 119, 146, 177, 216

NrCyclicPermGroups := n -> Length(Set(Partitions(n), Lcm));
NrMonogenicTransformationSemigroups := n -> Length(Union(Union(List([1 .. n - 1], m ->
                                       Set(Partitions(m), k -> [n - m,
                                       Lcm(k)]))), Set(Partitions(n), k -> [1,
                                       Lcm(k)])));
PARTS := [[[1]]];
LCMS := [[1]];

Parts := function(n)
  local p;

  if not IsBound(PARTS[n]) then 
    PARTS[n] := [];
    LCMS[n] := ShallowCopy(Lcms(n - 1));
    for p in Parts(n - 1) do 
      Add(PARTS[n], Concatenation([1], p));
      if Length(p) < 2 or p[2] > p[1] then 
        p := ShallowCopy(p);
        p[1] := p[1] + 1;
        Add(PARTS[n], p);
        AddSet(LCMS[n], Lcm(p));
      fi;
    od;
  fi;
  return PARTS[n];
end;

Lcms := function(n)
  if not IsBound(LCMS[n]) then 
    Parts(n);
  fi;
  return LCMS[n];
end;

NrMonogenicTransformationSemigroups := function(n)
  local out, m;
  out := [];
  for m in [1 .. n - 1] do 
    Append(out, List(Lcms(m), k -> [n - m, k]));
  od;
  out := Union(out, List(Lcms(n), k -> [1, k]));
  return Length(out);
end;

MyPrimes := function(n)
  local A, limit, j, i;

  A := BlistList([1 .. n], [2 .. n]);
  limit := Int(Sqrt(Float(n)));
  for i in [1 .. limit] do 
    if A[i] then 
      j := i ^ 2; 
      while j <= n do 
        A[j] := false;
        j := j + i;
      od;
    fi;
  od;
  return ListBlist([1 .. n], A);
end;

PRIMES := MyPrimes(10 ^ 6);;

B := [];

a := function(n, i)
  local p, nr, j;

  if IsBound(B[n + 1]) then 
    if  IsBound(B[n + 1][i + 1]) then 
      return B[n + 1][i + 1];
    fi;
  else 
    B[n + 1] := [];
  fi;
  
  if n = 0 then 
    B[n + 1][i + 1] := 1;
    return B[n + 1][i + 1];
  elif i < 1 then 
    B[n + 1][i + 1] := 0;
    return B[n + 1][i + 1];
  fi;
  p := PRIMES[i];
  
  nr := a(n, i -1);

  for j in [1 .. LogInt(n, p)] do 
    nr := nr + a(n - p ^ j, i - 1);
  od;
  
  B[n + 1][i + 1] := nr;
  return nr;
end;

b := n -> a(n, Length(MyPrimes(n)));

LCMS := [];

NrCyclicPermGroups := function(n)
  if not IsBound(LCMS[n]) then 
    LCMS[n] := Sum(List([0 .. n], k -> b(k)));
  fi;
  return LCMS[n];
end;

NrMonogenicTransformationSemigroups := function(n)
  local nr, m;
  nr := 0;
  for m in [1 .. n - 2] do
    nr := nr + NrCyclicPermGroups(m);
  od;
  return nr + NrCyclicPermGroups(n);
end;
