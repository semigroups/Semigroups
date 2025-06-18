TransfsFromTwistedBipartition := function(S, x, l)
  local n, L, i, rows, j, p, q;
  # quotient of twisted diagram semigroup S by making elements with l loops all equal to 0

  n := Size(S);
  L := AsSortedList(S);

  i := Position(L, x);
  rows := List([1 .. l], x -> ListWithIdenticalEntries(l * n + 1, l * n + 1));

    for j in [1..n] do
      for p in [0..l-1] do
        for q in [0..l-1] do
          if NrFloatingBlocks(L[i],L[j])+p+q<l then
            rows[p+1][q*n+j] := (NrFloatingBlocks(L[i],L[j])+p+q)*n+Position(L, L[i]*L[j]);
          fi;
        od;
      od;
    od;
    return List(rows, Transformation);
end;

TruncatedTwistedDiagramSemigroup := function(S,l)
  local n, L, T, pos;

  n := Size(S);
  L := Reversed(AsSortedList(S));
  T := Semigroup(IdentityTransformation);
  pos := 1;

  while Size(T) < l * Size(S) + 1 do
     T := ClosureSemigroup(T, TransfsFromTwistedBipartition(S, L[pos], l));
     pos := pos + 1;
  od;
  return T;
end;
