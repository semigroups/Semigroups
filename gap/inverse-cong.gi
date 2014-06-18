InstallGlobalFunction(InverseSemigroupCongruenceByCongruencePair,
[IsInverseSemigroup and IsFinite, IsSemigroup, IsDenseList],
function(s, kernel, traceBlocks)
  local a, ids, aa_class, e;
  if not IsSubsemigroup(s, kernel) then
    Error("2nd arg <kernel> must be a subsemigroup of 1st arg <s>,"); return;
  fi;
#  if not ForAll(Flat(traceBlocks), IsIdempotent) then
#    Error("3rd arg <traceBlocks> must be a list of lists of idempotents");
#  fi;
  # Check conditions for a congruence pair: Howie p.156
  # Condition (C2): aa' related to a'a
  for a in kernel do
    if not a^-1 * a in First(traceBlocks, c-> a * a^-1 in c) then
      Error("Not a valid congruence pair,"); return;
    fi;
  od;
  # Condition (C1): (ae in kernel && e related to a'a) => a in kernel
  ids := Idempotents(s);
  # Would rather do this without looping over the whole semigroup
  for a in s do
    if a in kernel then
      continue;
    fi;
    # Find the trace class containing a^-1 a
    aa_class := First(traceBlocks, c-> a^-1 * a in c);
    for e in aa_class do
      if a*e in kernel then
        Error("Not a valid congruence pair,"); return;
      fi;
    od;
  od;
  return InverseSemigroupCongruenceByCongruencePairNC(s, kernel, traceBlocks);
end);

#

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

InstallMethod(\=,
"for two inverse semigroup congruences",
[IsInverseSemigroupCongruence, IsInverseSemigroupCongruence],
function(cong1, cong2)
  return(Range(cong1) = Range(cong2) and
         cong1!.kernel = cong2!.kernel and
         cong1!.traceBlocks = cong2!.traceBlocks);
end);

#

InstallMethod(ImagesElm,
"for inverse semigroup congruence and associative element",
[IsInverseSemigroupCongruence, IsAssociativeElement],
function(cong, elm)
  local s, images, e, b;
  s := Range(cong);
  if not elm in s then
    Error("<cong> must be defined over the semigroup of the element <elm>,");
    return;
  fi;
  images := [];
  # Consider all idempotents trace-related to (a^-1 a)
  for e in First(cong!.traceBlocks, c-> (elm^-1 * elm) in c) do
    for b in LClass(s,e) do
      if elm * b^-1 in cong!.kernel then
        Add(images, b);
      fi;
    od;
  od;
  return images;
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

InstallMethod(TraceOfSemigroupCongruence,
"for semigroup congruence",
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

InstallMethod(KernelOfSemigroupCongruence,
"for semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  local s;
  s := Range(cong);
  if not IsInverseSemigroup(s) then
    Error("1st arg <cong> must be over an inverse semigroup,"); return;
  fi;
  return Union(List(Idempotents(s), e->EquivalenceClassOfElementNC(cong,e)));
end);

InstallGlobalFunction(CongruencePair,
function(cong)
  return [Trace(cong)];
end);