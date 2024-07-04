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
  # Makes n random Rees matrix semigroups which aren't too large
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
  # Runs both the new and old isomorphism algorithms on
  # n semigroups isomorphic to S and n randomly generated
  # Rees matrix semigroups, then compares times and answers
  local T, i, runtime, oldres, oldtime, newres, newtime, score, mismatches, testlist;

  score := [0, 0];
  mismatches := 0;

  testlist := MakeSomeRMS(n);
  for i in [1 .. n] do
    T := RandomIsomorphicRMS(S);
    Append(testlist, [T]);
  od;

  Print("Finished making test list \n");

  for i in [1 .. (2*n)] do

    runtime := Runtime();
    newres := IsIsomorphicRMS(S, testlist[i]);
    newtime := Runtime() - runtime;

    runtime := Runtime();
    oldres := IsIsomorphicSemigroup(S, testlist[i]);
    oldtime := Runtime() - runtime;

    if not newres = oldres then
      mismatches := mismatches + 1;
    fi;

    if newtime < oldtime then
      score[1] := score[1] + 1;
    else
      score[2] := score[2] + 1;
    fi;

    Print("Finished test ", i, "\n");

  od;

  Print("Score: ", score, "\n");
  Print("Mismatches: ", mismatches);
end;