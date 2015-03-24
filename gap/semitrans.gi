#############################################################################
##
#W  semitrans.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(IsTransformationSemigroupGreensClass, "for a Green's class",
[IsGreensClass], x -> IsTransformationSemigroup(Parent(x)));

#

InstallMethod(IteratorSorted, "for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
  if HasAsSSortedList(S) then
    return IteratorList(AsSSortedList(S));
  fi;
  return CallFuncList(IteratorSortedOp, List(RClasses(S), IteratorSorted));
end);

#

InstallMethod(IteratorSorted, "for an R-class",
[IsGreensRClass],
function(R)
  local o, m, rep, n, scc, base, S, out, x, image, basei, iter, i;

  o := LambdaOrb(R);
  m := LambdaOrbSCCIndex(R);
  rep := Representative(R);
  n := DegreeOfTransformationSemigroup(Parent(R));

  scc := OrbSCC(o)[m];
  base := DuplicateFreeList(ImageListOfTransformation(rep, n));
  S := StabChainOp(LambdaOrbSchutzGp(o, m), rec(base := base));
  out := [IteratorByIterator(
    IteratorSortedConjugateStabChain(S, ()), p -> rep * p,
    [IsIteratorSorted])];

  for i in [2 .. Length(scc)] do
    x := rep * EvaluateWord(o!.gens,
     TraceSchreierTreeOfSCCForward(o, m, scc[i]));
    image := ImageListOfTransformation(x, n);
    basei := DuplicateFreeList(image);
    iter := IteratorSortedConjugateStabChain(S,
     MappingPermListList(base, basei));
    out[i] := IteratorByIterator(iter,
      function(iter, p)
        return iter!.rep * p;
      end,
      [IsIteratorSorted], ReturnTrue,
      rec(rep := Transformation(image)));
  od;
  return CallFuncList(IteratorSortedOp, out);
end);

#

InstallMethod(\<, "for transformation semigroups",
[IsTransformationSemigroup, IsTransformationSemigroup],
function(S, T)
  local des, det, SS, TT, s, t;

  des := DegreeOfTransformationSemigroup(S);
  det := DegreeOfTransformationSemigroup(T);

  if des <> det then
    return des < det;
  fi;

  SS := IteratorSorted(S);
  TT := IteratorSorted(T);

  while not (IsDoneIterator(SS) or IsDoneIterator(TT)) do
    s := NextIterator(SS);
    t := NextIterator(TT);
    if s <> t then
      return s < t;
    fi;
  od;

  if IsDoneIterator(SS) and IsDoneIterator(TT) then
    return true;
  else
    return IsDoneIterator(SS);
  fi;
end);

#

InstallMethod(GeneratorsSmallest, "for a semigroup",
[IsSemigroup],
function(S)
  local iter, T, x;

  iter := IteratorSorted(S);
  T := Semigroup(NextIterator(iter));

  for x in iter do
    if not x in T then
      T := ClosureSemigroup(T, x);
      if T = S then
        break;
      fi;
    fi;
  od;

  return GeneratorsOfSemigroup(T);
end);

#

BindGlobal("SEMIGROUPS_ElementRClass",
function(R, largest)
  local o, m, rep, n, base, S, max, scc, y, basei, p, x, i;

  if Size(R) = 1 then
    return Representative(R);
  fi;

  o := LambdaOrb(R);
  m := LambdaOrbSCCIndex(R);
  rep := Representative(R);

  n := DegreeOfTransformationSemigroup(Parent(R));
  base := DuplicateFreeList(ImageListOfTransformation(rep, n));

  if not largest then
    base := Reversed(base);
  fi;

  S := StabChainOp(LambdaOrbSchutzGp(o, m), rec(base := base));
  max := rep * LargestElementStabChain(S, ());

  scc := OrbSCC(o)[m];

  for i in [2 .. Length(scc)] do
    y := EvaluateWord(o!.gens, TraceSchreierTreeOfSCCForward(o, m, scc[i]));
    basei := DuplicateFreeList(ImageListOfTransformation(rep * y, n));
    p := MappingPermListList(base, basei);
    x := rep * y * LargestElementStabChain(S, (), p);
    if x > max then
      max := x;
    fi;
  od;

  return max;
end);

#

InstallMethod(SmallestElementRClass, "for an R-class",
[IsGreensRClass],
function(R)
  return SEMIGROUPS_ElementRClass(R, false);
end);

InstallMethod(LargestElementRClass, "for an R-class",
[IsGreensRClass],
function(R)
  return SEMIGROUPS_ElementRClass(R, true);
end);

#

InstallMethod(SmallestElementSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
  local n;

  n := DegreeOfTransformationSemigroup(S);

  if ConstantTransformation(n, 1) in MinimalIdeal(S) then
    return ConstantTransformation(n, 1);
  fi;

  return Minimum(List(RClasses(S), SmallestElementRClass));
end);

InstallMethod(LargestElementSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
  local n;

  n := DegreeOfTransformationSemigroup(S);

  if ConstantTransformation(n, n) in MinimalIdeal(S) then
    return ConstantTransformation(n, n);
  fi;

  return Maximum(List(RClasses(S), LargestElementRClass));
end);

# different method required (but not yet given!! JDM) for ideals

InstallMethod(IsTransitive,
"for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(S)
  return IsTransitive(S, DegreeOfTransformationSemigroup(S));
end);

InstallMethod(IsTransitive,
"for a transformation semigroup with generators and a positive int",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsPosInt],
function(S, n)
  return IsTransitive(GeneratorsOfSemigroup(S), n);
end);

InstallMethod(IsTransitive,
"for a transformation semigroup with generators and a set of pos ints",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsList],
function(S, set)
  return IsTransitive(GeneratorsOfSemigroup(S), set);
end);

# JDM this could be done without creating the graph first and then running
# IsStronglyConnectedDigraph, but just using the method of
# IsStronglyConnectedDigraph with the generators themselves.

InstallMethod(IsTransitive,
"for a transformation collection and a positive int",
[IsTransformationCollection, IsPosInt],
function(coll, n)
  local nrgens, graph, i, x;

  nrgens := Length(coll);
  graph := EmptyPlist(n);

  for i in [1 .. n] do
    graph[i] := EmptyPlist(nrgens);
    for x in coll do
      Add(graph[i], i ^ x);
    od;
  od;

  return Length(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(graph)) = 1;
end);

InstallMethod(IsTransitive,
"for a transformation collection and set of positive integers",
[IsTransformationCollection, IsList],
function(coll, set)
  local n, nrgens, graph, lookup, j, i, x;

  if not (IsSSortedList(set) and IsHomogeneousList(set) and IsPosInt(set[1]))
   then
    Error("Semigroups: IsTransitive: usage,\n",
          "the second argument <set> must be a set of positive ",
          "integers");
    return;
  fi;

  n := Length(set);
  nrgens := Length(coll);
  graph := EmptyPlist(n);
  lookup := [];

  for i in [1 .. n] do
    lookup[set[i]] := i;
  od;

  for i in [1 .. n] do
    graph[i] := EmptyPlist(nrgens);
    for x in coll do
      j := set[i] ^ x;
      if IsBound(lookup[j]) then # <j> is in <set>!
        Add(graph[i], lookup[set[i] ^ x]);
      fi;
    od;
  od;

  return Length(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(graph)) = 1;
end);

# same method for ideals

InstallMethod(IsSynchronizingSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
  local deg;

  deg := DegreeOfTransformationSemigroup(S);
  if deg = 0 then
    return false;
  fi;

  return IsSynchronizingSemigroup(S, deg);
end);

# same method for ideals

InstallMethod(IsSynchronizingSemigroup,
"for a transformation semigroup and positive integer",
[IsTransformationSemigroup, IsPosInt],
function(S, n)
  local gens;

  if HasGeneratorsOfSemigroup(S) then
    gens := GeneratorsOfSemigroup(S);
  else
    gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));
  fi;

  return IsSynchronizingTransformationCollection(gens, n);
end);

# this method comes from PJC's slides from the Lisbon Workshop in July 2014
# not applicable to ideals

InstallMethod(IsSynchronizingTransformationCollection,
"for a transformation collection and positive integer",
[IsTransformationCollection, IsPosInt],
function(gens, n)
  local all_perms, r, NumberPair, PairNumber, genstoapply, act, graph, x, adj,
  y, num, marked, squashed, i, j;

  if n = 1 then
    return true;
  fi;

  all_perms := true;
  for x in gens do
    r := RankOfTransformation(x, n);
    if r = 1 then
      return true;
    elif r < n then
      all_perms := false;
    fi;
  od;
  if all_perms then
    return false; # S = <gens> is a group of transformations
  fi;

  #graph := SEMIGROUPS_RightActionGraphOnPairs2(gens, n);

  NumberPair := function(x)
    if x[2] > x[1] then
      return n * (x[1] - 1) + x[2] - x[1];
    else
      return (n - 1) * (x[1] - 1) + x[2];
    fi;
  end;

  PairNumber := function(x)
    local q, r;
    q := QuoInt(x - 1, n - 1);
    r := (x - 1) - q * (n - 1);
    if q > r then
      return [q + 1, r + 1];
    else
      return [q + 1, r + 2];
    fi;
  end;

  genstoapply := [1 .. Length(gens)];

  act := function(set, f)
    return OnPosIntSetsTrans(set, f, n);
  end;

  graph := List([1 .. n ^ 2], x -> []);

  # add edges for pairs
  for i in [1 .. n ^ 2 - n] do
    x := PairNumber(i);
    adj := [];
    for j in genstoapply do
      y := act(x, gens[j]);
      if Length(y) = 2 then
        num := NumberPair(act(x, gens[j]));
        AddSet(graph[num], i);
        AddSet(adj, num);
      else
        AddSet(graph[n ^ 2 - n + y[1]], i);
        AddSet(adj, n ^ 2 - n + y[1]);
      fi;
    od;
    if Length(adj) = 1 and adj[1] = i then
      # can't get anywhere by applying things to this pair
      return false;
    fi;
  od;

  marked := BlistList([1 .. n ^ 2], [n ^ 2 - n + 1 .. n ^ 2]);
  squashed := [n ^ 2 - n + 1 .. n ^ 2];
  for i in squashed do
    for j in graph[i] do
      if not marked[j] then
        marked[j] := true;
        squashed[Length(squashed) + 1] := j;
      fi;
    od;
  od;

  return Length(squashed) = n ^ 2;
end);

# WW: UNFINISHED!

SEMIGROUPS_RightActionGraphOnPairs2 := function(gens, n)
  local nrgens, nrpairs, PairNumber, NumberPair, in_nbs, labels, pair, act,
  range, i, j;

  nrgens     := Length(gens);
  nrpairs    := Binomial(n, 2);
  PairNumber := Concatenation([1 .. n], Combinations([1 .. n], 2));

  # Currently assume <x> is sorted and is a valid combination of [1 .. n]
  NumberPair := function(n, x)
    if Length(x) = 1 then
      return x[1];
    fi;
    return n + Binomial(n, 2) - Binomial(n + 1 - x[1], 2) + x[2] - x[1];
  end;

  in_nbs := List([1 .. n + nrpairs], x -> []);
  labels := List([1 .. n + nrpairs], x -> []);
  for i in [(n + 1) .. (n + nrpairs)] do
    pair := PairNumber[i];
    #isolated_pair := true;
    for j in [1 .. nrgens] do
      act := OnSets(pair, gens[j]);
      range := NumberPair(n, act);
      Add(in_nbs[range], i);
      Add(labels[range], j);
      #if range <> pair and isolated_pair then
        #isolated_pair := false;
      #fi;
    od;
    #if isolated_pair then
      # S is not synchronizing
    #fi;
  od;

  return rec(degree := n,
             in_nbs := in_nbs,
             labels := labels,
             PairNumber := PairNumber,
             NumberPair := NumberPair);

end;

SEMIGROUPS_RightActionGraphOnPairs := function(S) 
  local n;

  n := DegreeOfTransformationSemigroup(S);
  if n = 0 then
    return fail;
  fi;

  return SEMIGROUPS_RightActionGraphOnPairs2(GeneratorsOfSemigroup(S), n);
end;

# WW: UNFINISHED!

InstallMethod(RepresentativeOfMinimalIdeal, "for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
  local gens, nrgens, n, min_rank, rank, min_rank_index, graph, recursion,
  nrpairs, elts, seen_pairs, p, t, im, y, i, a, x;

  if HasMultiplicativeZero(S) and MultiplicativeZero(S) <> fail then
    return MultiplicativeZero(S);
  fi;

  gens := GeneratorsOfSemigroup(S);

  # This also catches T_1 and known trivial semigroups
  if HasIsSimpleSemigroup(S) and IsSimpleSemigroup(S) then
    return gens[1];
  fi;

  nrgens := Length(gens);
  n := DegreeOfTransformationSemigroup(S); # Smallest n such that S <= T_n
                                           # We must have n >= 2.

  # Find the minimum rank of a generator
  min_rank := n;
  for i in [1 .. nrgens] do
    rank := RankOfTransformation(gens[i], n);
    if rank = 1 then
      # SetIsSynchronizingSemigroup(S, true);
      # WW IsSynchronizingSemigroup is not a property. Shouldn't it be?
      return gens[i];
    elif rank < min_rank then
      min_rank := rank;
      min_rank_index := i;
    fi;
  od;

  if min_rank = n then
    SetIsGroupAsSemigroup(S, true);
    return gens[1];
  fi;

  graph := SEMIGROUPS_RightActionGraphOnPairs(S);

  # find a word describing a path from each collapsible pair to a singleton
  recursion := function(a)
    local b, k;

    for k in [1 .. Length(graph.in_nbs[a])] do
      b := graph.in_nbs[a][k];
      if not seen_pairs[b] then
        seen_pairs[b] := true;
        if a <= n then
          elts[b] := [graph.labels[a][k]];
        else
          elts[b] := Concatenation([graph.labels[a][k]], elts[a]);
        fi;
        recursion(b);
      fi;
    od;
    return;
  end;

  nrpairs := Binomial(n, 2);
  elts := EmptyPlist(n + nrpairs);
  seen_pairs := BlistList([1 .. n + nrpairs], []);
  for a in [1 .. n] do
    recursion(a);
  od;

  # Count the number of pairs which have been seen
  # p > 0 since p = 0 iff every generator has rank n. This is already checked
  p := Number(seen_pairs, x -> x);

  t := gens[min_rank_index];

  while true do
    im := ImageSetOfTransformation(t);

    if Length(im) = 1 then
      break;
    fi;

    for x in Combinations(im, 2) do
      y := graph.NumberPair(n, x);
      if seen_pairs[y] then
        t := t * EvaluateWord(gens, elts[y]);
        continue;
      fi;
    od;

    break;
  od;

  return t;
end);

#

InstallMethod(AsTransformationSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  return Range(IsomorphismTransformationSemigroup(S));
end);

#

InstallMethod(ViewString,
"for a transformation semigroup ideal with ideal generators",
[IsTransformationSemigroup and IsSemigroupIdeal and
HasGeneratorsOfSemigroupIdeal],
function(I)
  local str, nrgens;

  str := "\><";

  if HasIsTrivial(I) and IsTrivial(I) then
    Append(str, "\>trivial\< ");
  else
    if HasIsCommutative(I) and IsCommutative(I) then
      Append(str, "\>commutative\< ");
    fi;
  fi;

  if HasIsTrivial(I) and IsTrivial(I) then
  elif HasIsZeroSimpleSemigroup(I) and IsZeroSimpleSemigroup(I) then
    Append(str, "\>0-simple\< ");
  elif HasIsSimpleSemigroup(I) and IsSimpleSemigroup(I) then
    Append(str, "\>simple\< ");
  fi;

  if HasIsInverseSemigroup(I) and IsInverseSemigroup(I) then
    Append(str, "\>inverse\< ");
  elif HasIsRegularSemigroup(I)
   and not (HasIsSimpleSemigroup(I) and IsSimpleSemigroup(I)) then
    if IsRegularSemigroup(I) then
      Append(str, "\>regular\< ");
    else
      Append(str, "\>non-regular\< ");
    fi;
  fi;

  Append(str, "\>transformation\< \>semigroup\< \>ideal\< ");
  Append(str, "\>on \>");
  Append(str, ViewString(DegreeOfTransformationSemigroup(I)));
  Append(str, "\< pts with\> ");

  nrgens := Length(GeneratorsOfSemigroupIdeal(I));
  Append(str, ViewString(nrgens));
  Append(str, "\< generator");

  if nrgens > 1 or nrgens = 0 then
    Append(str, "s\<");
  else
    Append(str, "\<");
  fi;
  Append(str, ">\<");

  return str;
end);

#

InstallMethod(DegreeOfTransformationSemigroup,
"for a transformation semigroup ideal",
[IsTransformationSemigroup and IsSemigroupIdeal],
function(I)
  return DegreeOfTransformationSemigroup(SupersemigroupOfIdeal(I));
end);

#

InstallMethod(ComponentRepsOfTransformationSemigroup,
"for a transformation semigroup", [IsTransformationSemigroup],
function(S)
  local pts, reps, next, opts, gens, o, out, i;

  pts := [1 .. DegreeOfTransformationSemigroup(S)];
  reps := BlistList(pts, []);
  # true=its a rep, false=not seen it, fail=its not a rep
  next := 1;
  opts := rec(lookingfor := function(o, x)
    return reps[x] = true or reps[x] = fail;
  end);

  if IsSemigroupIdeal(S) then
    gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));
  else
    gens := GeneratorsOfSemigroup(S);
  fi;

  repeat
    o := Orb(gens, next, OnPoints, opts);
    Enumerate(o);
    if PositionOfFound(o) <> false and reps[o[PositionOfFound(o)]] = true then
      reps[o[PositionOfFound(o)]] := fail;
    fi;
    reps[next] := true;
    for i in [2 .. Length(o)] do
      reps[o[i]] := fail;
    od;
    next := Position(reps, false, next);
  until next = fail;

  out := [];
  for i in pts do
    if reps[i] = true then
      Add(out, i);
    fi;
  od;

  return out;
end);

#

InstallMethod(ComponentsOfTransformationSemigroup,
"for a transformation semigroup", [IsTransformationSemigroup],
function(S)
  local pts, comp, next, nr, opts, gens, o, out, i;

  pts := [1 .. DegreeOfTransformationSemigroup(S)];
  comp := BlistList(pts, []);
  # integer=its component index, false=not seen it
  next := 1;
  nr := 0;
  opts := rec(lookingfor := function(o, x)
    return IsPosInt(comp[x]);
  end);

  if IsSemigroupIdeal(S) then
    gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));
  else
    gens := GeneratorsOfSemigroup(S);
  fi;

  repeat
    o := Orb(gens, next, OnPoints, opts);
    Enumerate(o);
    if PositionOfFound(o) <> false then
      for i in o do
        comp[i] := comp[o[PositionOfFound(o)]];
      od;
    else
      nr := nr + 1;
      for i in o do
        comp[i] := nr;
      od;
    fi;
    next := Position(comp, false, next);
  until next = fail;

  out := [];
  for i in pts do
    if not IsBound(out[comp[i]]) then
      out[comp[i]] := [];
    fi;
    Add(out[comp[i]], i);
  od;

  return out;
end);

#

InstallMethod(CyclesOfTransformationSemigroup,
"for a transformation semigroup", [IsTransformationSemigroup],
function(S)
  local pts, comp, next, nr, cycles, opts, gens, o, scc, i;

  pts := [1 .. DegreeOfTransformationSemigroup(S)];
  comp := BlistList(pts, []);
  # integer=its component index, false=not seen it
  next := 1;
  nr := 0;
  cycles := [];
  opts := rec(lookingfor := function(o, x)
    return IsPosInt(comp[x]);
  end);

  if IsSemigroupIdeal(S) then
    gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));
  else
    gens := GeneratorsOfSemigroup(S);
  fi;

  repeat
    o := Orb(gens, next, OnPoints, opts);
    Enumerate(o);
    if PositionOfFound(o) <> false then
      for i in o do
        comp[i] := comp[o[PositionOfFound(o)]];
      od;
    else
      nr := nr + 1;
      for i in o do
        comp[i] := nr;
      od;
      scc := First(OrbSCC(o), x -> Length(x) > 1);
      if scc = fail then
        Add(cycles, [o[Length(o)]]);
      else
        Add(cycles, o{scc});
      fi;
    fi;
    next := Position(comp, false, next);
  until next = fail;

  return cycles;
end);

#EOF
