############################################################################
##
#W  congruences/conginv.gi
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for congruences on inverse semigroups, using the
## "kernel and trace" representation.
##
## See J.M. Howie's "Fundamentals of Semigroup Theory" Section 5.3, and see
## Michael Torpey's MSc thesis "Computing with Semigroup Congruences" Chapter 5
## (www-circa.mcs.st-and.ac.uk/~mct25/files/mt5099-report.pdf) for more details.
##

InstallGlobalFunction(InverseSemigroupCongruenceByKernelTrace,
[IsInverseSemigroup and IsGeneratorsOfInverseSemigroup and IsFinite,
 IsInverseSemigroup and IsGeneratorsOfInverseSemigroup,
 IsDenseList],
function(S, kernel, traceBlocks)
  local a, x, traceClass, f, l, e;
  # Check that the kernel is an inverse subsemigroup
  if not IsInverseSubsemigroup(S, kernel) then
    ErrorNoReturn("Semigroups: InverseSemigroupCongruenceByKernelTrace: ",
                  "usage,\nthe second arg <kernel> must be an inverse ",
                  "subsemigroup of the\nfirst arg <S>,");
  fi;
  # CHECK KERNEL IS NORMAL:
  # (1) Must contain all the idempotents of S
  if NrIdempotents(kernel) <> NrIdempotents(S) then
    ErrorNoReturn("Semigroups: InverseSemigroupCongruenceByKernelTrace: ",
                  "usage,\n",
                  "the second arg <kernel> must contain all the\n",
                  "idempotents of the first arg <S>,");
  fi;
  # (2) Must be self-conjugate
  for a in kernel do
    for x in GeneratorsOfSemigroup(S) do
      if not a ^ x in kernel then
        ErrorNoReturn("Semigroups: InverseSemigroupCongruenceByKernelTrace: ",
                      "usage,\nthe second arg <kernel> must be ",
                      "self-conjugate,");
      fi;
    od;
  od;
  # Check conditions for a congruence pair: Howie p.156
  for traceClass in traceBlocks do
    for f in traceClass do
      l := LClass(S, f);
      for a in l do
        if a in kernel then
          # Condition (C2): aa' related to a'a
          if not a * a ^ -1 in traceClass then
            ErrorNoReturn("Semigroups: ",
                          "InverseSemigroupCongruenceByKernelTrace:\n",
                          "not a valid congruence pair (C2),");
          fi;
        else
          # Condition (C1): (ae in kernel && e related to a'a) => a in kernel
          for e in traceClass do
            if a * e in kernel then
              ErrorNoReturn("Semigroups: ",
                            "InverseSemigroupCongruenceByKernelTrace:\n",
                            "not a valid congruence pair (C1),");
            fi;
          od;
        fi;
      od;
    od;
  od;
  #TODO: check trace is *normal*
  return InverseSemigroupCongruenceByKernelTraceNC(S, kernel, traceBlocks);
end);

InstallGlobalFunction(InverseSemigroupCongruenceByKernelTraceNC,
[IsInverseSemigroup and IsGeneratorsOfInverseSemigroup
 and IsFinite and IsEnumerableSemigroupRep,
 IsSemigroup,
 IsDenseList],
function(S, kernel, traceBlocks)
  local traceLookup, ES, fam, cong, i, elm;

  # Sort blocks
  traceBlocks := SortedList(List(traceBlocks, SortedList));

  # Calculate lookup table for trace
  # Might remove lookup - might never be better than blocks
  traceLookup := [];
  ES := IdempotentGeneratedSubsemigroup(S);

  for i in [1 .. Length(traceBlocks)] do
    for elm in traceBlocks[i] do
      traceLookup[PositionCanonical(ES, elm)] := i;
    od;
  od;
  # Construct the object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));
  cong := Objectify(NewType(fam, IsInverseSemigroupCongruenceByKernelTrace),
                    rec(kernel := kernel,
                        traceBlocks := traceBlocks,
                        traceLookup := traceLookup));
  SetSource(cong, S);
  SetRange(cong, S);
  SetKernelOfSemigroupCongruence(cong, kernel);
  SetTraceOfSemigroupCongruence(cong, traceBlocks);
  return cong;
end);

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

InstallMethod(\=,
"for two inverse semigroup congruences",
[IsInverseSemigroupCongruenceByKernelTrace,
 IsInverseSemigroupCongruenceByKernelTrace],
function(cong1, cong2)
  return(Range(cong1) = Range(cong2) and
         cong1!.kernel = cong2!.kernel and
         cong1!.traceBlocks = cong2!.traceBlocks);
end);

InstallMethod(IsSubrelation,
"for two inverse semigroup congruences",
[IsInverseSemigroupCongruenceByKernelTrace,
 IsInverseSemigroupCongruenceByKernelTrace],
function(cong1, cong2)
  # Tests whether cong2 is a subcongruence of cong1
  if Range(cong1) <> Range(cong2) then
    ErrorNoReturn("Semigroups: IsSubrelation: usage,\n",
                  "congruences must be defined over the same semigroup,");
  fi;
  return IsSubsemigroup(cong1!.kernel, cong2!.kernel)
         and ForAll(cong2!.traceBlocks,
                    b2 -> ForAny(cong1!.traceBlocks, b1 -> IsSubset(b1, b2)));
end);

InstallMethod(ImagesElm,
"for inverse semigroup congruence and multiplicative element",
[IsInverseSemigroupCongruenceByKernelTrace, IsMultiplicativeElement],
function(cong, elm)
  local S, images, e, b;
  S := Range(cong);
  if not elm in S then
    ErrorNoReturn("Semigroups: ImagesElm: usage,\n",
                  "the first arg <cong> is not defined over the semigroup of ",
                  "the second\nargument <elm>,");
  fi;
  images := [];
  # Consider all idempotents trace-related to (a^-1 a)
  for e in First(cong!.traceBlocks, c -> (elm ^ -1 * elm) in c) do
    for b in LClass(S, e) do
      if elm * b ^ -1 in cong!.kernel then
        Add(images, b);
      fi;
    od;
  od;
  return images;
end);

InstallMethod(EquivalenceClasses,
"for inverse semigroup congruence",
[IsInverseSemigroupCongruenceByKernelTrace],
function(cong)
  local S, reps, elmlists, kernel, traceBlock, blockreps, blockelmlists, id,
        elm, pos, classes, i;
  S := Range(cong);
  reps := [];
  elmlists := [];
  kernel := Elements(cong!.kernel);

  # Consider each trace-class in turn
  for traceBlock in cong!.traceBlocks do
    # Consider all the congruence classes corresponding to this trace-class
    blockreps := [];       # List of class reps
    blockelmlists := [];   # List of lists of elms in class
    for id in traceBlock do
      for elm in LClass(S, id) do
        # Find the congruence class that this element lies in
        pos := PositionProperty(blockreps, rep -> elm * rep ^ -1 in kernel);
        if pos = fail then
          # New class
          Add(blockreps, elm);
          Add(blockelmlists, [elm]);
        else
          # Add to the old class
          Add(blockelmlists[pos], elm);
        fi;
      od;
    od;
    Append(reps, blockreps);
    Append(elmlists, blockelmlists);
  od;

  # Create the class objects
  classes := [];
  for i in [1 .. Length(reps)] do
    classes[i] := EquivalenceClassOfElementNC(cong, reps[i]);
    SetAsList(classes[i], elmlists[i]);
  od;
  return classes;
end);

InstallMethod(NrEquivalenceClasses,
"for inverse semigroup congruence",
[IsInverseSemigroupCongruenceByKernelTrace],
function(cong)
  return Length(EquivalenceClasses(cong));
end);

InstallMethod(EquivalenceRelationCanonicalLookup,
"for inverse semigroup congruence",
[IsInverseSemigroupCongruenceByKernelTrace],
function(cong)
  local S, n, classes, elms, table, next, i, x;
  S := Range(cong);
  n := Size(S);
  classes := EquivalenceClasses(cong);
  elms := AsListCanonical(S);
  table := EmptyPlist(n);
  next := 1;
  for i in [1 .. n] do
    if not IsBound(table[i]) then
      for x in First(classes, class -> elms[i] in class) do
        table[Position(S, x)] := next;
      od;
      next := next + 1;
    fi;
  od;
  return table;
end);

InstallMethod(CongruenceTestMembershipNC,
"for inverse semigroup congruence and two multiplicative elements",
[IsInverseSemigroupCongruenceByKernelTrace,
 IsMultiplicativeElement, IsMultiplicativeElement],
function(cong, elm1, elm2)
  # Is (a^-1 a, b^-1 b) in the trace?
  if elm1 ^ -1 * elm1 in
      First(cong!.traceBlocks, c -> elm2 ^ -1 * elm2 in c) then
    # Is ab^-1 in the kernel?
    if elm1 * elm2 ^ -1 in cong!.kernel then
      return true;
    fi;
  fi;
  return false;
end);

InstallMethod(EquivalenceClassOfElement,
"for inverse semigroup congruence and multiplicative element",
[IsInverseSemigroupCongruenceByKernelTrace, IsMultiplicativeElement],
function(cong, elm)
  if not elm in Range(cong) then
    ErrorNoReturn("Semigroups: EquivalenceClassOfElement: usage,\n",
                  "the second arg <elm> must be in the\n",
                  "semigroup of the first arg <cong>,");
  fi;
  return EquivalenceClassOfElementNC(cong, elm);
end);

InstallMethod(EquivalenceClassOfElementNC,
"for inverse semigroup congruence and multiplicative element",
[IsInverseSemigroupCongruenceByKernelTrace, IsMultiplicativeElement],
function(cong, elm)
  local class;
  class := Objectify(InverseSemigroupCongruenceClassByKernelTraceType(cong),
                     rec(rep := elm));
  SetParentAttr(class, Range(cong));
  SetEquivalenceClassRelation(class, cong);
  SetRepresentative(class, elm);
  return class;
end);

InstallMethod(InverseSemigroupCongruenceClassByKernelTraceType,
"for an inverse semigroup congruence",
[IsInverseSemigroupCongruenceByKernelTrace],
function(cong)
  return NewType(FamilyObj(Range(cong)),
                 IsInverseSemigroupCongruenceClassByKernelTrace);
end);

InstallMethod(\=,
"for two inverse semigroup congruence classes",
[IsInverseSemigroupCongruenceClassByKernelTrace,
 IsInverseSemigroupCongruenceClassByKernelTrace],
              function(c1, c2)
  return(EquivalenceClassRelation(c1) = EquivalenceClassRelation(c2) and
         [c1!.rep, c2!.rep] in EquivalenceClassRelation(c1));
end);

InstallMethod(\in,
"for multiplicative element and inverse semigroup congruence class",
[IsMultiplicativeElement, IsInverseSemigroupCongruenceClassByKernelTrace],
function(elm, class)
  local cong;
  cong := EquivalenceClassRelation(class);
  return elm in Range(cong) and [elm, class!.rep] in cong;
end);

InstallMethod(\*,
"for two inverse semigroup congruence classes",
[IsInverseSemigroupCongruenceClassByKernelTrace,
 IsInverseSemigroupCongruenceClassByKernelTrace],
function(c1, c2)
  if not EquivalenceClassRelation(c1) = EquivalenceClassRelation(c2) then
    ErrorNoReturn("Semigroups: \\*: usage,\n",
                  "the arguments must be classes of the same congruence,");
  fi;
  return EquivalenceClassOfElementNC(EquivalenceClassRelation(c1),
                                     c1!.rep * c2!.rep);
end);

InstallMethod(Enumerator,
"for inverse semigroup congruence class",
[IsInverseSemigroupCongruenceClassByKernelTrace],
function(class)
  return AsList(class);
end);

InstallMethod(AsList,
"for inverse semigroup congruence class",
[IsInverseSemigroupCongruenceClassByKernelTrace],
function(class)
  return ImagesElm(EquivalenceClassRelation(class), class!.rep);
end);

InstallMethod(AsSSortedList,
"for inverse semigroup congruence class",
[IsInverseSemigroupCongruenceClassByKernelTrace],
function(class)
  return SSortedList(AsList(class));
end);

InstallMethod(Size,
"for inverse semigroup congruence class",
[IsInverseSemigroupCongruenceClassByKernelTrace],
function(class)
  return Size(AsList(class));
end);

InstallMethod(TraceOfSemigroupCongruence,
"for semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  local S, invcong;
  S := Range(cong);
  if not (IsInverseSemigroup(S) and IsGeneratorsOfInverseSemigroup(S)) then
    ErrorNoReturn("Semigroups: TraceOfSemigroupCongruence: usage,\n",
                  "<cong> must be over an inverse semigroup with inverse op,");
  fi;
  invcong := AsInverseSemigroupCongruenceByKernelTrace(cong);
  return invcong!.traceBlocks;
end);

InstallMethod(KernelOfSemigroupCongruence,
"for semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  local S, invcong;
  S := Range(cong);
  if not (IsInverseSemigroup(S) and IsGeneratorsOfInverseSemigroup(S)) then
    ErrorNoReturn("Semigroups: KernelOfSemigroupCongruence: usage,\n",
                  "<cong> must be over an inverse semigroup with inverse op,");
  fi;
  invcong := AsInverseSemigroupCongruenceByKernelTrace(cong);
  return invcong!.kernel;
end);

InstallMethod(AsInverseSemigroupCongruenceByKernelTrace,
"for semigroup congruence with generating pairs",
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
function(cong)
  local S;
  S := Range(cong);
  if not (IsInverseSemigroup(S) and IsGeneratorsOfInverseSemigroup(S)) then
    ErrorNoReturn("Semigroups: AsInverseSemigroupCongruenceByKernelTrace: ",
                  "usage,\n",
                  "<cong> must be over an inverse semigroup with inverse op,");
  fi;
  return
    SEMIGROUPS.KernelTraceClosure(S,
                                  IdempotentGeneratedSubsemigroup(S),
                                  List(Idempotents(S), e -> [e]),
                                  GeneratingPairsOfSemigroupCongruence(cong));
end);

InstallMethod(JoinSemigroupCongruences,
"for inverse semigroup congruence",
[IsInverseSemigroupCongruenceByKernelTrace,
 IsInverseSemigroupCongruenceByKernelTrace],
function(c1, c2)
  local S, gens1, gens2, kernel, traceBlocks, block, b1, j, pos;
  S := Range(c1);
  if S <> Range(c2) then
    ErrorNoReturn("Semigroups: JoinSemigroupCongruences: usage,\n",
                  "congruences must be defined over the same semigroup,");
  fi;

  # kernel generated by union of c1's kernel and c2's kernel
  gens1 := GeneratorsOfInverseSemigroup(c1!.kernel);
  gens2 := GeneratorsOfInverseSemigroup(c2!.kernel);
  kernel := InverseSemigroup(Concatenation(gens1, gens2));

  # trace is join of c1's trace and c2's trace
  traceBlocks := StructuralCopy(c1!.traceBlocks);
  for block in c2!.traceBlocks do
    b1 := PositionProperty(traceBlocks, b -> block[1] in b);
    for j in [2 .. Size(block)] do
      if not block[j] in traceBlocks[b1] then
        # Combine the classes
        pos := PositionProperty(traceBlocks, b -> block[j] in b);
        Append(traceBlocks[b1], traceBlocks[pos]);
        Unbind(traceBlocks[pos]);
      fi;
    od;
    traceBlocks := Compacted(traceBlocks);
  od;

  # Take the kernel-trace closure to ensure we have a correct congruence
  return SEMIGROUPS.KernelTraceClosure(S, kernel, traceBlocks, []);
end);

InstallMethod(MeetSemigroupCongruences,
"for two inverse semigroup congruence",
[IsInverseSemigroupCongruenceByKernelTrace,
 IsInverseSemigroupCongruenceByKernelTrace],
function(c1, c2)
  local S, kernel, traceBlocks, ids, c2lookup, classnos, block, classno;
  S := Range(c1);
  if S <> Range(c2) then
    ErrorNoReturn("Semigroups: MeetSemigroupCongruences: usage,\n",
                  "congruences must be defined over the same semigroup,");
  fi;

  # Calculate the intersection of the kernels
  # TODO: can we do this without enumerating the whole kernel?
  kernel := InverseSemigroup(Intersection(c1!.kernel, c2!.kernel));
  kernel := InverseSemigroup(SmallInverseSemigroupGeneratingSet(kernel));

  # Calculate the intersection of the traces
  traceBlocks := [];
  ids := IdempotentGeneratedSubsemigroup(S);
  c2lookup := c2!.traceLookup;
  for block in c1!.traceBlocks do
    classnos := c2lookup{List(block, x -> Position(ids, x))};
    for classno in DuplicateFreeList(classnos) do
      Add(traceBlocks, block{Positions(classnos, classno)});
    od;
  od;

  return InverseSemigroupCongruenceByKernelTrace(S, kernel, traceBlocks);
end);

SEMIGROUPS.KernelTraceClosure := function(S, kernel, traceBlocks, pairstoapply)
  local idsmgp, idslist, slist, kernelgenstoapply, gen, nrk, nr,
        traceUF, i, pos1, j, pos, hashlen, ht, right, genstoapply,
        NormalClosureInverseSemigroup, enumerate_trace, enforce_conditions,
        compute_kernel, oldLookup, oldKernel, trace_unchanged, kernel_unchanged;

  # This function takes an inverse semigroup S, a subsemigroup ker, an
  # equivalence traceBlocks on the idempotents, and a list of pairs in S.
  # It returns the minimal congruence containing "kernel" in its kernel and
  # "traceBlocks" in its trace, and containing all the given pairs
  # TODO Review this JDM for use of Elements, AsList etc. Could iterators work
  # better?

  idsmgp  := IdempotentGeneratedSubsemigroup(S);
  idslist := AsListCanonical(idsmgp);
  slist   := AsListCanonical(S);

  # Retrieve the initial information
  kernel := InverseSubsemigroup(S, kernel);
  kernelgenstoapply := Set(pairstoapply, x -> x[1] * x[2] ^ -1);
  # kernel might not be normal, so make sure to check its generators too
  for gen in GeneratorsOfInverseSemigroup(kernel) do
    AddSet(kernelgenstoapply, gen);
  od;
  nrk := Length(kernelgenstoapply);
  Enumerate(kernel);
  pairstoapply := List(pairstoapply,
                       x -> [PositionCanonical(idsmgp, RightOne(x[1])),
                             PositionCanonical(idsmgp, RightOne(x[2]))]);
  nr := Length(pairstoapply);

  # Calculate traceUF from traceBlocks
  traceUF := UF_NEW(Length(idslist));
  for i in [1 .. Length(traceBlocks)] do
    pos1 := PositionCanonical(idsmgp, traceBlocks[i][1]);
    for j in [2 .. Length(traceBlocks[i])] do
      UF_UNION(traceUF, [pos1, PositionCanonical(idsmgp, traceBlocks[i][j])]);
    od;
  od;
  UF_FLATTEN(traceUF);

  # Setup some useful information
  pos := 0;
  hashlen := SEMIGROUPS.OptionsRec(S).hashlen;
  ht := HTCreate([1, 1], rec(forflatplainlists := true,
                             treehashsize := hashlen));
  right := RightCayleyGraphSemigroup(idsmgp);
  genstoapply := [1 .. Length(right[1])];

  #
  # The functions that do the work:
  #
  NormalClosureInverseSemigroup := function(S, K, coll)
    local T, opts, x, list;
    # This takes an inv smgp S, an inv subsemigroup K, and some elms coll,
    # then creates the *normal closure* of K with coll inside S.
    # It assumes K is already normal.
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
        list := AsList(Enumerate(Orb(GeneratorsOfSemigroup(S), x, POW, opts)));
        T := ClosureInverseSemigroup(T, list);
      od;
    od;
    return K;
  end;

  enumerate_trace := function()
    local a, j, x, y, z;
    if pos = 0 then
      # Add the generating pairs themselves
      for x in pairstoapply do
        if x[1] <> x[2] and HTValue(ht, x) = fail then
          HTAdd(ht, x, true);
          UF_UNION(traceUF, x);
          # Add each pair's "conjugate" pairs
          for a in GeneratorsOfSemigroup(S) do
            z := [PositionCanonical(idsmgp, a ^ -1 * idslist[x[1]] * a),
                  PositionCanonical(idsmgp, a ^ -1 * idslist[x[2]] * a)];
            if z[1] <> z[2] and HTValue(ht, z) = fail then
              HTAdd(ht, z, true);
              nr := nr + 1;
              pairstoapply[nr] := z;
              UF_UNION(traceUF, z);
            fi;
          od;
        fi;
      od;
    fi;

    while pos < nr do
      pos := pos + 1;
      x := pairstoapply[pos];
      for j in genstoapply do
        # Add the pair's right-multiples (idsmgp is commutative)
        y := [right[x[1]][j], right[x[2]][j]];
        if y[1] <> y[2] and HTValue(ht, y) = fail then
          HTAdd(ht, y, true);
          nr := nr + 1;
          pairstoapply[nr] := y;
          UF_UNION(traceUF, y);
          # Add the pair's "conjugate" pairs
          for a in GeneratorsOfSemigroup(S) do
            z := [PositionCanonical(idsmgp, a ^ -1 * idslist[x[1]] * a),
                  PositionCanonical(idsmgp, a ^ -1 * idslist[x[2]] * a)];
            if z[1] <> z[2] and HTValue(ht, z) = fail then
              HTAdd(ht, z, true);
              nr := nr + 1;
              pairstoapply[nr] := z;
              UF_UNION(traceUF, z);
            fi;
          od;
        fi;
      od;
    od;
    UF_FLATTEN(traceUF);
  end;

  enforce_conditions := function()
    local traceTable, traceBlocks, a, e, f, classno;
    traceTable := UF_TABLE(traceUF);
    traceBlocks := UF_BLOCKS(traceUF);
    for a in slist do
      if a in kernel then
        e := PositionCanonical(idsmgp, LeftOne(a));
        f := PositionCanonical(idsmgp, RightOne(a));
        if traceTable[e] <> traceTable[f] then
          nr := nr + 1;
          pairstoapply[nr] := [e, f];
        fi;
      else
        classno := traceTable[PositionCanonical(idsmgp, RightOne(a))];
        for e in traceBlocks[classno] do
          if a * idslist[e] in kernel then
            nrk := nrk + 1;
            AddSet(kernelgenstoapply, a);
            break;
            # JDM is this correct? Why repeatedly add the same a to
            # kernelgenstoapply?
          fi;
        od;
      fi;
    od;
  end;

  compute_kernel := function()
    # Take the normal closure inverse semigroup containing the new elements
    if kernelgenstoapply <> [] then
      kernel := NormalClosureInverseSemigroup(S, kernel,
                                              kernelgenstoapply);
      Enumerate(kernel);
      kernelgenstoapply := [];
      nrk := 0;
    fi;
  end;

  # Keep applying the method until no new info is found
  repeat
    oldLookup := StructuralCopy(UF_TABLE(traceUF));
    oldKernel := kernel;
    compute_kernel();
    enforce_conditions();
    enumerate_trace();
    trace_unchanged := (oldLookup = UF_TABLE(traceUF));
    kernel_unchanged := (oldKernel = kernel);
    Info(InfoSemigroups, 2, "lookup: ", trace_unchanged);
    Info(InfoSemigroups, 2, "kernel: ", kernel_unchanged);
    Info(InfoSemigroups, 2, "nrk = 0: ", nrk = 0);
  until trace_unchanged and kernel_unchanged and (nrk = 0);

  # Convert traceLookup to traceBlocks
  traceBlocks := List(Compacted(UF_BLOCKS(traceUF)),
                      b -> List(b, i -> idslist[i]));

  return InverseSemigroupCongruenceByKernelTraceNC(S, kernel, traceBlocks);
end;

InstallMethod(MinimumGroupCongruence,
"for an inverse semigroup with inverse op",
[IsInverseSemigroup and IsGeneratorsOfInverseSemigroup],
# This is the maximum congruence whose quotient is a group
function(S)
  local ker, leq, n, x, traceBlocks;

  # Kernel should be the majorant closure of the idempotents
  ker := IdempotentGeneratedSubsemigroup(S);
  leq := NaturalLeqInverseSemigroup(S);
  for n in ker do
    for x in S do
      if leq(n, x) and not x in ker then
        ker := ClosureInverseSemigroup(ker, x);
      fi;
    od;
  od;
  ker := InverseSemigroup(SmallInverseSemigroupGeneratingSet(ker));

  # Trace should be the universal congruence
  traceBlocks := [Idempotents(S)];

  return InverseSemigroupCongruenceByKernelTraceNC(S, ker, traceBlocks);
end);
