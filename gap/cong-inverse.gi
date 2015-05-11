############################################################################
##
#W  cong-inverse.gi
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for congruences on inverse semigroups, using the
## "kernel and trace" representation - see Howie 5.3
##

# TODO: make a method for NrCongruenceClasses for an inverse semigroup by
# congreunce pair JDM

InstallGlobalFunction(InverseSemigroupCongruenceByKernelTrace,
[IsInverseSemigroup and IsFinite, IsInverseSemigroup, IsDenseList],
function(s, kernel, traceBlocks)
  local a, x, traceClass, f, l, e;
  # Check that the kernel is an inverse subsemigroup
  if not IsInverseSubsemigroup(s, kernel) then
    Error("Semigroups: InverseSemigroupCongruenceByKernelTrace: usage,\n",
          "the second arg <kernel> must be an inverse subsemigroup of the\n",
          "first arg <S>,");
    return;
  fi;
  # CHECK KERNEL IS NORMAL:
  # (1) Must contain all the idempotents of s
  if NrIdempotents(kernel) <> NrIdempotents(s) then
    Error("Semigroups: InverseSemigroupCongruenceByKernelTrace: usage,\n",
          "the second arg <kernel> must contain all the idempotents of the\n",
          "first arg <S>,");
    return;
  fi;
  # (2) Must be self-conjugate
  for a in kernel do
    for x in GeneratorsOfSemigroup(s) do
      if not a ^ x in kernel then
  Error("Semigroups: InverseSemigroupCongruenceByKernelTrace: usage,\n",
        "the second arg <kernel> must be self-conjugate,");
        return;
      fi;
    od;
  od;
  # Check conditions for a congruence pair: Howie p.156
  for traceClass in traceBlocks do
    for f in traceClass do
      l := LClass(s, f);
      for a in l do
        if a in kernel then
          # Condition (C2): aa' related to a'a
          if not a * a ^ -1 in traceClass then
            Error("Semigroups: InverseSemigroupCongruenceByKernelTrace:\n",
                  "not a valid congruence pair 1,");
            return;
          fi;
        else
          # Condition (C1): (ae in kernel && e related to a'a) => a in kernel
          for e in traceClass do
            if a * e in kernel then
              Error(
                "Semigroups: InverseSemigroupCongruenceByKernelTrace:\n",
                "not a valid congruence pair 2,");
              return;
            fi;
          od;
        fi;
      od;
    od;
  od;
  return InverseSemigroupCongruenceByKernelTrace(s, kernel, traceBlocks);
end);

#

InstallGlobalFunction(InverseSemigroupCongruenceByKernelTraceNC,
[IsInverseSemigroup and IsFinite, IsSemigroup, IsDenseList],
function(s, kernel, traceBlocks)
  local traceLookup, i, elm, fam, cong;
  # Calculate lookup table for trace
  # Might remove lookup - might never be better than blocks
  traceLookup := [];
  for i in [1 .. Length(traceBlocks)] do
    for elm in traceBlocks[i] do
      traceLookup[Position(Idempotents(s), elm)] := i;
    od;
  od;
  # Construct the object
  fam := GeneralMappingsFamily(
                 ElementsFamily(FamilyObj(s)),
                 ElementsFamily(FamilyObj(s)));
  cong := Objectify(
                  NewType(fam, IsInverseSemigroupCongruenceByKernelTrace),
                  rec(kernel := kernel,
                      traceBlocks := traceBlocks,
                      traceLookup := traceLookup));
  SetSource(cong, s);
  SetRange(cong, s);
  return cong;
end);

#

InstallMethod(ViewObj,
"for inverse semigroup congruence",
[IsInverseSemigroupCongruenceByKernelTrace],
function(cong)
  Print("<semigroup congruence over ");
  ViewObj(Range(cong));
  Print(" with congruence pair (",
        Size(cong!.kernel), ",",
        Size(cong!.traceBlocks), ")>");
end);

#

InstallMethod(\=,
"for two inverse semigroup congruences",
[IsInverseSemigroupCongruenceByKernelTrace,
IsInverseSemigroupCongruenceByKernelTrace],
function(cong1, cong2)
  return(Range(cong1) = Range(cong2) and
         cong1!.kernel = cong2!.kernel and
         cong1!.traceBlocks = cong2!.traceBlocks);
end);

#

InstallMethod(ImagesElm,
"for inverse semigroup congruence and associative element",
[IsInverseSemigroupCongruenceByKernelTrace, IsAssociativeElement],
function(cong, elm)
  local s, images, e, b;
  s := Range(cong);
  if not elm in s then
    Error("Semigroups: ImagesElm: usage,\n",
          "the first arg <cong> is not defined over the semigroup of the",
          "second\nargument <elm>,");
    return;
  fi;
  images := [];
  # Consider all idempotents trace-related to (a^-1 a)
  for e in First(cong!.traceBlocks, c -> (elm ^ -1 * elm) in c) do
    for b in LClass(s, e) do
      if elm * b ^ -1 in cong!.kernel then
        Add(images, b);
      fi;
    od;
  od;
  return images;
end);

#

InstallMethod(\in,
"for dense list and inverse semigroup congruence",
[IsDenseList, IsInverseSemigroupCongruenceByKernelTrace],
function(pair, cong)
  local s;
  if Size(pair) <> 2 then
    Error("Semigroups: \in: usage,\n",
          "the first arg <pair> must be a list of length 2,");
    return;
  fi;
  s := Range(cong);
  if not (pair[1] in s and pair[2] in s) then
    Error("Semigroups: \in: usage,\n",
          "the entries of the first arg <pair> must belong to the semigroup",
          " of <cong>,");
    return;
  fi;
  # Is (a^-1 a, b^-1 b) in the trace?
  if pair[1] ^ -1 * pair[1] in
     First(cong!.traceBlocks, c -> pair[2] ^ -1 * pair[2] in c) then
    # Is ab^-1 in the kernel?
    if pair[1] * pair[2] ^ -1 in cong!.kernel then
      return true;
    fi;
  fi;
  return false;
end);

#

InstallMethod(EquivalenceClassOfElement,
"for inverse semigroup congruence and associative element",
[IsInverseSemigroupCongruenceByKernelTrace, IsAssociativeElement],
function(cong, elm)
  if not elm in Range(cong) then
    Error("Semigroups: EquivalenceClassOfElement: usage,\n",
    "the second arg <elm> must be in the semigroup of the first arg <cong>,");
    return;
  fi;
  return EquivalenceClassOfElementNC(cong, elm);
end);

#

InstallMethod(EquivalenceClassOfElementNC,
"for inverse semigroup congruence and associative element",
[IsInverseSemigroupCongruenceByKernelTrace, IsAssociativeElement],
function(cong, elm)
  local fam, class;
  fam := FamilyObj(Range(cong));
  class := Objectify(NewType(fam,
                     IsInverseSemigroupCongruenceClassByKernelTrace),
                     rec(rep := elm));
  SetParentAttr(class, cong);
  SetRepresentative(class, elm);
  return class;
end);

#

InstallMethod(\=,
"for two inverse semigroup congruence classes",
[IsInverseSemigroupCongruenceClassByKernelTrace,
IsInverseSemigroupCongruenceClassByKernelTrace],
function(c1, c2)
  return(ParentAttr(c1) = ParentAttr(c2) and
          [c1!.rep, c2!.rep] in ParentAttr(c1));
end);

#

InstallMethod(\in,
"for associative element and inverse semigroup congruence class",
[IsAssociativeElement, IsInverseSemigroupCongruenceClassByKernelTrace],
function(elm, class)
  local cong;
  cong := ParentAttr(class);
  return elm in Range(cong) and [elm, class!.rep] in cong;
end);

#

InstallMethod(\*,
"for two inverse semigroup congruence classes",
[IsInverseSemigroupCongruenceClassByKernelTrace,
IsInverseSemigroupCongruenceClassByKernelTrace],
function(c1, c2)
  if not Parent(c1) = Parent(c2) then
    Error("Semigroups: \*: usage,\n",
          "the arguments must be classes of the same congruence,");
    return;
  fi;
  return EquivalenceClassOfElementNC(Parent(c1), c1!.rep * c2!.rep);
end);

#

InstallMethod(AsSSortedList,
"for inverse semigroup congruence class",
[IsInverseSemigroupCongruenceClassByKernelTrace],
function(class)
  return SSortedList(ImagesElm(ParentAttr(class), class!.rep));
end);

#

InstallMethod(Size,
"for inverse semigroup congruence class",
[IsInverseSemigroupCongruenceClassByKernelTrace],
function(class)
  return Size(Elements(class));
end);

#

InstallMethod(TraceOfSemigroupCongruence,
"for semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  local s, elms, trace, i, class, congClass, j;
  s := Range(cong);
  if not IsInverseSemigroup(s) then
    Error("Semigroups: TraceOfSemigroupCongruence: usage,\n",
          "the argument <cong> must be over an inverse semigroup,");
    return;
  fi;
  elms := ShallowCopy(Idempotents(s));
  trace := [];
  for i in [1 .. Size(elms)] do
    if elms[i] <> fail then
      class := [elms[i]];
      congClass := EquivalenceClassOfElementNC(cong, elms[i]);
      for j in [i + 1 .. Size(elms)] do
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

# FIXME this method should be updated, in case that the congruence is given by
# a congruence pair.

InstallMethod(KernelOfSemigroupCongruence,
"for semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  local s, gens;
  s := Range(cong);
  if not IsInverseSemigroup(s) then
    Error("Semigroups: KernelOfSemigroupCongruence: usage,\n",
          "the first arg <cong> must be over an inverse semigroup,");
    return;
  fi;
  gens := Union(List(Idempotents(s),
           e -> EquivalenceClassOfElementNC(cong, e)));
  return InverseSemigroup(gens, rec(small := false));
end);

# assumes K is normal in the beginning!!!! 

NormalClosureInverseSemigroup := function(S, K, coll)
  local T, opts, x;
  
  T := ClosureInverseSemigroup(K, coll);

  while K <> T do 
    K := T;
    opts := rec();
    opts.gradingfunc := function(o, x)
                          return x in K;
                        end;

    opts.onlygrades := function(x, data)
                        return x = false;
                       end;

    opts.onlygradesdata := fail;

    for x in K do 
      T := ClosureInverseSemigroup(T, 
                                   AsList(Orb(GeneratorsOfSemigroup(S), x, POW,
                                   opts)));
    od;
  od;

  return K;
end;

InstallMethod(AsInverseSemigroupCongruenceByKernelTrace,
"for semigroup congruence with generating pairs",
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
function(cong)
  local s, idsmgp, ids, pos, hashlen, ht, right, left, genstoapply, find,
  union, enumerate_trace, enforce_conditions, compute_kernel, genpairs,
  pairstoapply, kernelgenstoapply, nr, nrk, traceLookup, kernel, StartTiming,
  StopTiming, timing, oldLookup, oldKernel, traceBlocks, ht_e, i;

  # Check that the argument makes sense
  s := Range(cong);
  if not IsInverseSemigroup(s) then
    Error("Semigroups: AsInverseSemigroupCongruenceByKernelTrace: usage,\n",
          "the argument <cong> must be over an inverse semigroup,");
    return;
  fi;

  # Setup some data structures for the trace
  idsmgp := IdempotentGeneratedSubsemigroup(s);
  ids := ELEMENTS_SEMIGROUP(GenericSemigroupData(idsmgp), infinity);
  ht_e := HTCreate(ids[1]);
  for i in [1 .. Length(ids)] do 
    HTAdd(ht_e, ids[i], i);
  od;

  StartTiming := function(record)
    record.timeofday := IO_gettimeofday();
  end;

  StopTiming := function(record)
    local timeofday, elapsed;
    timeofday := IO_gettimeofday();
    elapsed := (timeofday.tv_sec - record.timeofday.tv_sec) * 1000
               + Int((timeofday.tv_usec - record.timeofday.tv_usec) / 1000);
    Print("elapsed time: ", String(elapsed), "ms\n");
  end;

  pos := 0;
  hashlen := SEMIGROUPS_OptionsRec(s).hashlen.L;

  ht := HTCreate( [ids[1],ids[1]], rec(forflatplainlists := true,
              treehashsize := hashlen ) );

  right := RightCayleyGraphSemigroup(idsmgp);
  left := LeftCayleyGraphSemigroup(idsmgp);
  genstoapply := [1 .. Length(right[1])];

  find := function(i)
    while traceLookup[i] <> i do
      i := traceLookup[i];
    od;
    return i;
  end;

  union := function(pair)
    local ii, jj;
    ii := find(pair[1]);
    jj := find(pair[2]);
    if ii < jj then
      traceLookup[jj] := ii;
    elif jj < ii then
      traceLookup[ii] := jj;
    fi;
  end;

  # STEPS (2)+(1)
  enumerate_trace := function()
    local x, a, y, j;
    if pos = 0 then
      # Add the generating pairs themselves
      for x in pairstoapply do
        if x[1] <> x[2] and HTValue(ht, x) = fail then
          HTAdd(ht, x, true);
          union(x);
          # Add each pair's "conjugate" pairs
          for a in GeneratorsOfSemigroup(s) do
            y := [HTValue(ht_e, a^-1 * ids[x[1]] * a),
                  HTValue(ht_e, a^-1 * ids[x[2]] * a)];
            if y[1] <> y[2] and HTValue(ht, y) = fail then
              HTAdd(ht, y, true);
              nr := nr + 1;
              pairstoapply[nr] := y;
              union(y);
            fi;
          od;
        fi;
      od;
    fi;

    while pos < nr do
      pos := pos + 1;
      x := pairstoapply[pos];
      for j in genstoapply do
        # Add the pair's left-multiples
        y := [right[x[1]][j], right[x[2]][j]];
        if y[1] <> y[2] and HTValue(ht, y) = fail then
          HTAdd(ht, y, true);
          nr := nr + 1;
          pairstoapply[nr] := y;
          union(y);
        fi;

        # Add the pair's right-multiples
        y := [left[x[1]][j], left[x[2]][j]];
        if y[1] <> y[2] and HTValue(ht, y) = fail then
          HTAdd(ht, y, true);
          nr := nr + 1;
          pairstoapply[nr] := y;
          union(y);
        fi;
      od;
    od;
    traceLookup := List(traceLookup, find);
  end;

  # STEPS (6)+(5)
  enforce_conditions := function()
    local a, e, f, fclass, outer, classes;
    classes := [];
    for e in [1 .. Length(traceLookup)] do
      if not IsBound(classes[traceLookup[e]]) then 
        classes[traceLookup[e]] := [e];
      else 
        Add(classes[traceLookup[e]], e);
      fi;
    od;

    for a in Elements(s) do
      if a in kernel then

        e := HTValue(ht_e, LeftOne(a));
        f := HTValue(ht_e, RightOne(a));
        if traceLookup[e] <> traceLookup[f] then
          nr := nr + 1;
          pairstoapply[nr] := [e,f];
          #union([e,f]);
        fi;
      else
        fclass := traceLookup[HTValue(ht_e, RightOne(a))];
        for e in classes[fclass] do
          if a * ids[e] in kernel then
            nrk := nrk + 1;
            AddSet(kernelgenstoapply, a);
          fi;
        od;
      fi;
    od;
  end;

  # STEPS (3)+(4)
  compute_kernel := function()
    # Take the normal closure inverse semigroup containing the new elements
    if kernelgenstoapply <> [] then 
      kernel := NormalClosureInverseSemigroup(s, kernel, kernelgenstoapply);
      Elements(kernel);
      kernelgenstoapply := [];
      nrk := 0;
    fi;
  end;

  # Retrieve the initial information
  genpairs := GeneratingPairsOfSemigroupCongruence(cong);
  pairstoapply := List(genpairs, x -> [HTValue(ht_e, RightOne(x[1])),
                                       HTValue(ht_e, RightOne(x[2]))] );
  kernelgenstoapply := Set(genpairs, x -> x[1] * x[2]^-1);
  nr := Length(pairstoapply);
  nrk := Length(kernelgenstoapply);
  traceLookup := [1 .. Length(ids)];
  kernel := IdempotentGeneratedSubsemigroup(s);
  Elements(kernel); 

  timing := rec();
  # Keep applying the method until no new info is found
  repeat
    oldLookup := StructuralCopy(traceLookup);
    oldKernel := kernel;
    StartTiming(timing);
    Print("compute_kernel: ");
    compute_kernel();
    StopTiming(timing);
    StartTiming(timing);
    Print("enforce_conditions: ");
    enforce_conditions();
    StopTiming(timing);
    StartTiming(timing);
    Print("enumerate_trace: ");
    enumerate_trace();
    StopTiming(timing);
    Info(InfoSemigroups, 1, "lookup: ", oldLookup=traceLookup);
    Info(InfoSemigroups, 1, "kernel: ", oldKernel=kernel);
  until (oldLookup = traceLookup) and (nrk = 0);

  # Convert traceLookup to traceBlocks
  traceBlocks := Filtered(List([1 .. Maximum(traceLookup)],
                         i-> List(Positions(traceLookup, i),
                                 j-> ids[j])), x-> not IsEmpty(x));

  return InverseSemigroupCongruenceByKernelTraceNC(s, kernel, traceBlocks);
end);

#

InstallGlobalFunction(SEMIGROUPS_InverseCongFromPairs,
function(s, pairs)
  local cong;
  cong := AsInverseSemigroupCongruenceByKernelTrace(
                  SemigroupCongruenceByGeneratingPairs(s, pairs));
  SetGeneratingPairsOfMagmaCongruence(cong, pairs);
  return cong;
end);

#
