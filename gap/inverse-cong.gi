InstallGlobalFunction(InverseSemigroupCongruenceByCongruencePair,
[IsInverseSemigroup and IsFinite, IsInverseSemigroup, IsDenseList],
function(s, kernel, traceBlocks)
  local a, x, traceClass, f, l, e;
  # Check that the kernel is a subsemigroup
  if not IsSubsemigroup(s, kernel) then
    Error("2nd arg <kernel> must be a subsemigroup of 1st arg <s>,"); return;
  fi;
  # CHECK KERNEL IS NORMAL:
  # (1) Must contain all the idempotents of s
  if NrIdempotents(kernel) <> NrIdempotents(s) then
    Error("2nd arg <kernel> must contain all idempotents of 1st arg <s>,");
    return;
  fi;
  # (2) Must be self-conjugate
  for a in kernel do
    for x in GeneratorsOfSemigroup(s) do
      if not a^x in kernel then
        Error("2nd arg <kernel> must be self-conjugate,"); return;
      fi;
    od;
  od;
  # Check conditions for a congruence pair: Howie p.156
  for traceClass in traceBlocks do
    for f in traceClass do
      l := LClass(s,f);
      for a in l do
        if a in kernel then
          # Condition (C2): aa' related to a'a
          if not a * a^-1 in traceClass then
            Error("Not a valid congruence pair,"); return;
          fi;
        else          
          # Condition (C1): (ae in kernel && e related to a'a) => a in kernel
          for e in traceClass do
            if a*e in kernel then
              Error("Not a valid congruence pair,"); return;
            fi;
          od;
        fi;
      od;
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

InstallMethod(EquivalenceClassOfElementNC,
"for inverse semigroup congruence and associative element",
[IsInverseSemigroupCongruence, IsAssociativeElement],
function(cong, elm)
  local fam, class;
  fam := FamilyObj(Range(cong));
  class := Objectify(NewType(fam, IsInverseSemigroupCongruenceClass),
                   rec(rep := elm) );
  SetParentAttr(class, cong);
  SetRepresentative(class, elm);
  return class;
end);

#

InstallMethod(\=,
"for two inverse semigroup congruence classes",
[IsInverseSemigroupCongruenceClass, IsInverseSemigroupCongruenceClass],
function(c1, c2)
  return( ParentAttr(c1) = ParentAttr(c2) and
          [c1!.rep, c2!.rep] in ParentAttr(c1) );
end);

#

InstallMethod( \in,
"for associative element and inverse semigroup congruence class",
[IsAssociativeElement, IsInverseSemigroupCongruenceClass],
function(elm, class)
  local cong;
  cong := ParentAttr(class);
  return( elm in Range(cong) and [elm, class!.rep] in cong );
end);

#

InstallMethod( \*,
"for two inverse semigroup congruence classes",
[IsInverseSemigroupCongruenceClass, IsInverseSemigroupCongruenceClass],
function(c1, c2)
  if not Parent(c1) = Parent(c2) then
    Error("<c1> and <c2> must be classes of the same congruence,"); return;
  fi;
  return( EquivalenceClassOfElementNC(Parent(c1), c1!.rep * c2!.rep) );
end);

#

InstallMethod(AsSSortedList,
"for inverse semigroup congruence class",
[IsInverseSemigroupCongruenceClass],
function(class)
  return SSortedList(ImagesElm(ParentAttr(class), class!.rep));
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
  local s, gens;
  s := Range(cong);
  if not IsInverseSemigroup(s) then
    Error("1st arg <cong> must be over an inverse semigroup,"); return;
  fi;
  gens := Union(List(Idempotents(s), e->EquivalenceClassOfElementNC(cong,e)));
  return InverseSemigroup(gens, rec(small := true) );
end);

#

InstallMethod(AsInverseSemigroupCongruenceByCongruencePair,
"for semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  if not IsInverseSemigroup(Range(cong)) then
    Error("<cong> must be over an inverse semigroup,");
  fi;
  return InverseSemigroupCongruenceByCongruencePairNC( Range(cong),
                 KernelOfSemigroupCongruence(cong),
                 TraceOfSemigroupCongruence(cong) );
end);