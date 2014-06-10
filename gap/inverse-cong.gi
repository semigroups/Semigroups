InstallGlobalFunction(Trace,
function(cong)
  local s, e, elms, trace, i, class, j;
  if not IsSemigroupCongruence(cong) then
    Error("<cong> must be a semigroup congruence,"); return;
  fi;
  s := Range(cong);
  if not IsInverseSemigroup(s) then
    Error("<cong> must be declared over an inverse semigroup,"); return;
  fi;
  e := IdempotentGeneratedSubsemigroup(s);
  elms := Elements(e);
  trace := [];
  for i in [1..Size(elms)] do
    if elms[i] <> fail then
      class := [ elms[i] ];
      for j in [i+1..Size(elms)] do
        if j in EquivalenceClassOfElement(cong, i) then
          Add(class, elms[j]);
          elms[j] := fail;
        fi;
      od;
      Add(trace, class);
    fi;
  od;
  return trace;
end);

#

InstallGlobalFunction(CongruencePair,
function(cong)
  return [trace];
end);