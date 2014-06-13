InstallGlobalFunction(InverseSemigroupCongruenceByCongruencePairNC,
[IsInverseSemigroup and IsFinite, IsSemigroup, IsDenseList],
function(s, kernel, traceBlocks)
  local traceLookup, i, elm, fam, cong;
  # Calculate lookup table for trace
  # Might remove lookup - might never be better than blocks
  traceLookup := [];
  for i in [1..Length(traceBlocks)] do
    for elm in traceBlocks[i] do
      traceLookup[Position(Idempotents(s),elm)] := i;
    od;
  od;
  # Construct the object
  fam := GeneralMappingsFamily(
                 ElementsFamily(FamilyObj(s)),
                 ElementsFamily(FamilyObj(s)) );
  cong := Objectify(
                  NewType(fam, IsInverseSemigroupCongruence),
                  rec(kernel := kernel,
                      traceBlocks := traceBlocks,
                      traceLookup := traceLookup) );
  SetSource(cong, s);
  SetRange(cong, s);
  return cong;
end);

#

InstallMethod(ViewObj,
"for inverse semigroup congruence",
[IsInverseSemigroupCongruence],
function(cong)
  Print("<inverse semigroup congruence by congruence pair: ",
        Size(cong!.kernel), " ",
        Size(cong!.traceBlocks),">");
end);

#

InstallMethod(\in,
"for dense list and inverse semigroup congruence",
[IsDenseList, IsInverseSemigroupCongruence],
function(pair, cong)
  local s;
  if Size(pair) <> 2 then
    Error("1st arg <pair> must be a list of length 2,"); return;
  fi;
  s := Range(cong);
  if not (pair[1] in s and pair[2] in s) then
    Error("<pair> must have entries from the semigroup of <cong>,"); return;
  fi;
  # Is (a^-1 a, b^-1 b) in the trace?
  if pair[1]^-1 * pair[1] in
     First(cong!.traceBlocks, c-> pair[2]^-1*pair[2] in c) then
    # Is ab^-1 in the kernel?
    if pair[1] * pair[2]^-1 in cong!.kernel then
      return true;
    fi;
  fi;
  return false;
end);

#

InstallMethod(Trace,
"for inverse semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  local s, elms, trace, i, class, congClass, j;
  s := Range(cong);
  if not IsInverseSemigroup(s) then
    Error("<cong> must be declared over an inverse semigroup,"); return;
  fi;
  elms := ShallowCopy(Idempotents(s));
  trace := [];
  for i in [1..Size(elms)] do
    if elms[i] <> fail then
      class := [ elms[i] ];
      congClass := EquivalenceClassOfElementNC(cong, elms[i]);
      for j in [i+1..Size(elms)] do
        if elms[j] in congClass then
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

InstallMethod(Kernel,
"for inverse semigroup congruence",
[IsSemigroupCongruence],
function(cong)

end);

InstallGlobalFunction(CongruencePair,
function(cong)
  return [Trace(cong)];
end);