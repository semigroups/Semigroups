RandomIsomorphicRMS := function(R)
  local table, p, S;
  if Size(R) > 1000 then
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
  # Returns a list of n small Rees matrix semigroups
  local semigroups_list, S;

  semigroups_list := [];
  while Length(semigroups_list) < n do
    S := RandomSemigroup(IsReesMatrixSemigroup, 4, 5, AlternatingGroup(4));
    if Size(S) < 1000 then
      Append(semigroups_list, [S]);
    fi;
  od;

  return semigroups_list;
end;

TestRMS := function(S, n)
  # Tests isomorphism algorithms on given semigroup S
  # by comparing it to n random isomorphic RMS
  local T, i, runtime, oldres, oldtime, newres, newtime, score, mismatches;

  score := [0, 0];
  mismatches := 0;
  for i in [1 .. n] do
    T := RandomIsomorphicRMS(S);

    runtime := Runtime();
    newres := IsIsomorphicRMS(S, T);
    newtime := Runtime() - runtime;

    runtime := Runtime();
    oldres := IsIsomorphicSemigroup(S, T);
    oldtime := Runtime() - runtime;

    if not newres = oldres then
      mismatches := mismatches + 1;
    fi;

    if newtime < oldtime then
      score[1] := score[1] + 1;
    else
      score[2] := score[2] + 1;
    fi;
  od;

  Print("Score: ", score, "\n");
  Print("Mismatches: ", mismatches);
end;