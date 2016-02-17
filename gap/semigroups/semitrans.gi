#############################################################################
##
#W  semitrans.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for every operation/attribute/property that is
# specific to transformation semigroups.

InstallMethod(IsConnectedTransformationSemigroup,
"for a transformation semigroup with generators",
[IsTransformationSemigroup],
function(S)
  return IsConnectedDigraph(DigraphOfActionOnPoints(S));
end);

InstallMethod(FixedPoints, "for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local n, gens, out, fixed, i, x;

  n    := DegreeOfTransformationSemigroup(S);
  gens := GeneratorsOfSemigroup(S);
  out  := [];

  for i in [1 .. n] do
    fixed := true;
    for x in gens do
      if i ^ x <> i then
        fixed := false;
        break;
      fi;
    od;
    if fixed then
      Add(out, i);
    fi;
  od;

  return out;
end);

InstallMethod(MovedPoints, "for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(S)
  return Difference([1 .. DegreeOfTransformationSemigroup(S)], FixedPoints(S));
end);

InstallMethod(\^, "for a transformation semigroup with generators and perm",
[IsTransformationCollection, IsPerm],
function(coll, p)
  return List(coll, x -> x ^ p);
end);

InstallMethod(\^, "for a transformation semigroup with generators and perm",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsPerm],
function(S, p)
  return Semigroup(GeneratorsOfSemigroup(S) ^ p);
end);

InstallMethod(DigraphOfActionOnPoints,
"for a transformation semigroup with known generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(S)
  return DigraphOfActionOnPoints(S, DegreeOfTransformationSemigroup(S));
end);

InstallMethod(DigraphOfActionOnPoints,
"for a transformation semigroup with known generators and pos int",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsPosInt],
function(S, n)
  local out, gens, x, k, i, j;

  out := List([1 .. n], x -> []);
  gens := GeneratorsOfSemigroup(S);
  for i in [1 .. Length(gens)] do
    x := gens[i];
    for j in [1 .. n] do
      k := j ^ x;
      AddSet(out[j], k);
    od;
  od;
  return DigraphNC(out);
end);

SEMIGROUPS.ElementRClass := function(R, largest)
  local o, m, rep, n, base, S, out, scc, y, basei, p, x, i;

  if Size(R) = 1 then
    return Representative(R);
  fi;

  o := LambdaOrb(R);
  m := LambdaOrbSCCIndex(R);
  rep := Representative(R);

  n := DegreeOfTransformationSemigroup(Parent(R));
  base := DuplicateFreeList(ImageListOfTransformation(rep, n));

  S := StabChainOp(LambdaOrbSchutzGp(o, m), rec(base := base));
  if largest then
    out := rep * LargestElementStabChain(S, ());
  else
    out := rep * SmallestElementConjugateStabChain(S, (), ());
  fi;

  scc := OrbSCC(o)[m];

  for i in [2 .. Length(scc)] do
    y := EvaluateWord(o!.gens, TraceSchreierTreeOfSCCForward(o, m, scc[i]));
    basei := DuplicateFreeList(ImageListOfTransformation(rep * y, n));
    p := MappingPermListList(base, basei);
    if largest then
      x := rep * y * LargestElementConjugateStabChain(S, (), p);
      if x > out then
        out := x;
      fi;
    else
      x := rep * y * SmallestElementConjugateStabChain(S, (), p);
      if x < out then
        out := x;
      fi;
    fi;
  od;

  return out;
end;

SEMIGROUPS.SmallestElementRClass := function(R)
  return SEMIGROUPS.ElementRClass(R, false);
end;

SEMIGROUPS.LargestElementRClass := function(R)
  return SEMIGROUPS.ElementRClass(R, true);
end;

# stop_on_isolated_pair:
#   if true, this function returns false if there is an isolated pair-vertex
# TODO should this be a Digraph??
# FIXME make this a digraph
SEMIGROUPS.GraphOfRightActionOnPairs :=
function(gens, n, stop_on_isolated_pair)
  local nrgens, nrpairs, PairNumber, NumberPair, in_nbs, labels, pair,
  isolated_pair, act, range, i, j;
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
    isolated_pair := true;
    for j in [1 .. nrgens] do
      act := OnSets(pair, gens[j]);
      range := NumberPair(n, act);
      Add(in_nbs[range], i);
      Add(labels[range], j);
      if range <> i and isolated_pair then
        isolated_pair := false;
      fi;
    od;
    if stop_on_isolated_pair and isolated_pair then
      return false;
    fi;
  od;

  return rec(degree := n,
             in_nbs := in_nbs,
             labels := labels,
             PairNumber := PairNumber,
             NumberPair := NumberPair);
end;

# FIXME can probably do better than this

InstallMethod(Idempotents, "for a transformation semigroup and pos int",
[IsTransformationSemigroup, IsPosInt],
function(S, rank)
  local deg;
  deg := DegreeOfTransformationSemigroup(S);
  if rank > deg then
    return [];
  fi;
  return Filtered(Idempotents(S),
                              x -> RankOfTransformation(x, deg) = rank);
end);

# different method required (but not yet given!!) for ideals

InstallMethod(IsTransformationSemigroupGreensClass, "for a Green's class",
[IsGreensClass], x -> IsTransformationSemigroup(Parent(x)));

#

InstallMethod(IteratorSorted, "for an acting transformation semigroup",
[IsTransformationSemigroup and IsActingSemigroup],
function(S)
  if HasAsSSortedList(S) then
    return IteratorList(AsSSortedList(S));
  fi;
  return CallFuncList(IteratorSortedOp, List(RClasses(S), IteratorSorted));
end);

#

InstallMethod(IteratorSorted, "for an R-class",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(R)
  local o, m, rep, n, scc, base, S, out, x, image, basei, iter, i;

  o := LambdaOrb(R);
  m := LambdaOrbSCCIndex(R);
  rep := Representative(R);
  n := DegreeOfTransformationSemigroup(Parent(R));

  scc := OrbSCC(o)[m];
  base := DuplicateFreeList(ImageListOfTransformation(rep, n));
  S := StabChainOp(LambdaOrbSchutzGp(o, m), rec(base := base));
  out := [IteratorByIterator(IteratorSortedConjugateStabChain(S, ()),
                             p -> rep * p, [IsIteratorSorted])];

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

InstallMethod(SmallestElementSemigroup, 
"for an acting transformation semigroup",
[IsTransformationSemigroup and IsActingSemigroup],
function(S)
  local n;

  n := DegreeOfTransformationSemigroup(S);

  if ConstantTransformation(n, 1) in MinimalIdeal(S) then
    return ConstantTransformation(n, 1);
  fi;

  return Minimum(List(RClasses(S), SEMIGROUPS.SmallestElementRClass));
end);

InstallMethod(LargestElementSemigroup, "for an acting transformation semigroup",
[IsTransformationSemigroup and IsActingSemigroup],
function(S)
  local n;

  n := DegreeOfTransformationSemigroup(S);

  if ConstantTransformation(n, n) in MinimalIdeal(S) then
    return ConstantTransformation(n, n);
  fi;

  return Maximum(List(RClasses(S), SEMIGROUPS.LargestElementRClass));
end);

# different method required (but not yet given!! JDM) for ideals

InstallMethod(IsTransitive,
"for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(S)
  return IsStronglyConnectedDigraph(DigraphOfActionOnPoints(S));
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

  return IsStronglyConnectedDigraph(Digraph(graph));
end);

InstallMethod(IsTransitive,
"for a transformation collection and set of positive integers",
[IsTransformationCollection, IsList],
function(coll, set)
  local n, nrgens, graph, lookup, j, i, x;

  if not (IsSSortedList(set) and IsHomogeneousList(set)
          and IsPosInt(set[1])) then
    ErrorNoReturn("Semigroups: IsTransitive: usage,\n",
                  "the second argument <set> must be a set of positive ",
                  "integers");
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

  return IsStronglyConnectedDigraph(Digraph(graph));
end);

# not relevant for ideals

InstallMethod(Size, "for a monogenic transformation semigroup",
[IsTransformationSemigroup and IsMonogenicSemigroup],
function(S)
  local gen, ind;
  gen := IrredundantGeneratingSubset(S)[1];
  ind := IndexPeriodOfTransformation(gen);
  if ind[1] > 0 then
    return Sum(ind) - 1;
  fi;
  return Sum(ind);
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

  if HasMultiplicativeZero(S) and MultiplicativeZero(S) <> fail then
    return RankOfTransformation(MultiplicativeZero(S), deg) = 1;
  fi;

  if HasRepresentativeOfMinimalIdeal(S) then
    return RankOfTransformation(RepresentativeOfMinimalIdeal(S), deg) = 1;
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

InstallMethod(IsSynchronizingTransformationCollection,
"for a transformation collection and positive integer",
[IsTransformationCollection, IsPosInt],
function(gens, n)
  local all_perms, r, graph, marked, squashed, x, i, j;

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

  graph := SEMIGROUPS.GraphOfRightActionOnPairs(gens, n, true);

  if graph = false then
    return false;
  fi;

  marked := BlistList([1 .. n + Binomial(n, 2)], []);
  squashed := [1 .. n];
  for i in squashed do
    for j in graph.in_nbs[i] do
      if not marked[j] then
        marked[j] := true;
        Add(squashed, j);
      fi;
    od;
  od;

  return Length(squashed) = n + Binomial(n, 2);
end);

#

InstallMethod(RepresentativeOfMinimalIdeal, "for a transformation semigroup",
[IsTransformationSemigroup], RankFilter(IsActingSemigroup),
# to beat the default method for acting semigroups
function(S)
  local gens, nrgens, n, min_rank, rank, min_rank_index, graph, nrpairs, elts,
  marked, squashed, j, t, im, reduced, y, i, k, x;

  if IsSemigroupIdeal(S) and
      (HasRepresentativeOfMinimalIdeal(SupersemigroupOfIdeal(S))
       or not HasGeneratorsOfSemigroup(S)) then
    return RepresentativeOfMinimalIdeal(SupersemigroupOfIdeal(S));
  fi;

  gens := GeneratorsOfSemigroup(S);

  # This catches T_1. This also catches known trivial semigroups.
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

  graph := SEMIGROUPS.GraphOfRightActionOnPairs(gens, n, false);

  # find a word describing a path from each collapsible pair to a singleton
  nrpairs := Binomial(n, 2);
  elts := EmptyPlist(n + nrpairs);
  marked := BlistList([1 .. n + nrpairs], []);
  squashed := [1 .. n];
  for i in squashed do
    for k in [1 .. Length(graph.in_nbs[i])] do
      j := graph.in_nbs[i][k];
      if not marked[j] then
        marked[j] := true;
        if i <= n then
          elts[j] := [graph.labels[i][k]];
        else
          elts[j] := Concatenation([graph.labels[i][k]], elts[i]);
        fi;
        Add(squashed, j);
      fi;
    od;
  od;

  t := gens[min_rank_index];
  im := ImageSetOfTransformation(t, n);

  # find a word in S of minimal rank by repeatedly collapsing pairs in im(t)
  while true do
    reduced := false;
    for x in IteratorOfCombinations(im, 2) do
      y := graph.NumberPair(n, x);
      if marked[y] then
        t := t * EvaluateWord(gens, elts[y]);
        im := ImageSetOfTransformation(t, n);
        reduced := true;
        break;
      fi;
    od;
    if not reduced then
      break;
    fi;
  od;

  return t;
end);

#

InstallMethod(IsomorphismTransformationMonoid,
"for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local id, dom, gens, inv;

  if IsMonoid(s) then
    return MappingByFunction(s, s, IdFunc, IdFunc);
  fi;

  if MultiplicativeNeutralElement(s) = fail then
    ErrorNoReturn("Semigroups: IsomorphismTransformationMonoid: usage,\n",
                  "the argument <s> must have a multiplicative neutral element",
                  ",");
  fi;

  id := MultiplicativeNeutralElement(s);
  dom := ImageSetOfTransformation(id);

  gens := List(Generators(s), x -> TransformationOp(x, dom));

  inv := function(f)
    local out, i;

    out := [1 .. DegreeOfTransformationSemigroup(s)];
    for i in [1 .. Length(dom)] do
      out[dom[i]] := dom[i ^ f];
    od;
    return id * Transformation(out);
  end;

  return MappingByFunction(s,
                           Monoid(gens),
                           f -> TransformationOp(f, dom),
                           inv);
end);

# FIXME get rid of this -> AsSemigroup

InstallMethod(AsTransformationSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  return Range(IsomorphismTransformationSemigroup(S));
end);

# same method for ideals

InstallMethod(IsomorphismPermGroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)

  if not IsGroupAsSemigroup(s) then
    ErrorNoReturn("Semigroups: IsomorphismPermGroup: usage,\n",
                  "the argument <s> must be a transformation semigroup ",
                  "satisfying IsGroupAsSemigroup,");
  fi;
  # gaplint: ignore 4
  return MagmaIsomorphismByFunctionsNC(s,
           Group(List(GeneratorsOfSemigroup(s), PermutationOfImage)),
           PermutationOfImage,
           x -> AsTransformation(x, DegreeOfTransformationSemigroup(s)));
end);

# same method for ideals

InstallMethod(GroupOfUnits, "for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
  local H, map, G, U;

  if MultiplicativeNeutralElement(S) = fail then
    return fail;
  fi;

  H := GreensHClassOfElementNC(S, MultiplicativeNeutralElement(S));
  map := InverseGeneralMapping(IsomorphismPermGroup(H));
  G := Source(map);
  U := Semigroup(List(GeneratorsOfGroup(G), x -> x ^ map));

  SetIsomorphismPermGroup(U, MappingByFunction(U, G, PermutationOfImage,
                                               x -> x ^ map));
  SetIsGroupAsSemigroup(U, true);
  UseIsomorphismRelation(U, G);

  return U;
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
  return DigraphConnectedComponents(DigraphOfActionOnPoints(S)).comps;
end);

#

InstallMethod(CyclesOfTransformationSemigroup,
"for a transformation semigroup", [IsTransformationSemigroup],
function(S)
  return DigraphAllSimpleCircuits(DigraphOfActionOnPoints(S));
end);

InstallMethod(EndomorphismMonoid, "for a digraph",
[IsDigraph],
function(digraph)
  local hook, S;

  if HasGeneratorsOfEndomorphismMonoidAttr(digraph) then
    return Semigroup(GeneratorsOfEndomorphismMonoidAttr(digraph),
                     rec(small := true));
  fi;

  hook := function(S, f)
    S[1] := ClosureSemigroup(S[1], f);
  end;

  S := [AsTransformationSemigroup(AutomorphismGroup(digraph))];

  return HomomorphismDigraphsFinder(digraph, digraph, hook, S, infinity,
                                    fail, false, DigraphVertices(digraph), [],
                                    fail, fail)[1];
end);

InstallMethod(EndomorphismMonoid, "for a digraph and a homogeneous list",
[IsDigraph, IsHomogeneousList],
function(digraph, colors)
  local hook, S;

  hook := function(S, f)
    S[1] := ClosureSemigroup(S[1], f);
  end;

  S := [AsTransformationSemigroup(AutomorphismGroup(digraph, colors))];

  return HomomorphismDigraphsFinder(digraph, digraph, hook, S, infinity,
                                    fail, false, DigraphVertices(digraph), [],
                                    colors, colors)[1];

end);
