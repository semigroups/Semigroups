RandomIsomorphicRMS := function(R)
  local table, p, S;
  if Size(R) > 384 then
    ErrorNoReturn("Tillman what were you thinking???");
  fi;
  table := MultiplicationTable(R);
  p := Random(SymmetricGroup(Size(R)));
  S := SemigroupByMultiplicationTableNC(OnMultiplicationTable(table, p));
  return Range(IsomorphismReesMatrixSemigroup(S));
end;

# TODO check that this is faster than computing CanonicalMultiplicationTable

IsIsomorphicRMS := function(R, S)
  local uR, uS, map, mat, next, row, entry;
  # TODO check that R and S have the same dimensions, and same size underlying group
  R := CanonicalReesMatrixSemigroup(R);
  uR := UnderlyingSemigroup(R);
  uS := UnderlyingSemigroup(S);
  # TODO check if uR, uS is a group
  map := IsomorphismGroups(uS, uR);
  if map = fail then
    return false;
  fi;

  mat := [];
  for row in Matrix(S) do
    next := [];
    for entry in row do
      Add(next, entry ^ map);
    od;
    Add(mat, next);
  od;
  S := ReesMatrixSemigroup(uR, mat);
  S := CanonicalReesMatrixSemigroup(S);
  return Matrix(R) = Matrix(S);
end;

# TODO similar thing for RZMS

MakeSomeRMS := function(n)
  local semigroups_list, S;

  semigroups_list := [];
  while Length(semigroups_list) < n do
    S := RandomSemigroup(IsReesMatrixSemigroup, 2, 2);
    if Size(S) < 384 then
      Append(semigroups_list, [S]);
    fi;
  od;

  return semigroups_list;
end;
