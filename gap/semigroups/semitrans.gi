#############################################################################
##
##  semitrans.gi
##  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for every operation/attribute/property that is
# specific to transformation semigroups.

#############################################################################
## Random
#############################################################################

InstallMethod(SEMIGROUPS_ProcessRandomArgsCons,
[IsTransformationSemigroup, IsList],
function(filt, params)
  return SEMIGROUPS_ProcessRandomArgsCons(IsSemigroup, params);
end);

InstallMethod(SEMIGROUPS_ProcessRandomArgsCons,
[IsTransformationMonoid, IsList],
function(filt, params)
  return SEMIGROUPS_ProcessRandomArgsCons(IsSemigroup, params);
end);

InstallMethod(RandomSemigroupCons, "for IsTransformationSemigroup and a list",
[IsTransformationSemigroup, IsList],
function(filt, params)
  return Semigroup(List([1 .. params[1]], i ->
                   RandomTransformation(params[2])));
end);

InstallMethod(RandomMonoidCons, "for IsTransformationMonoid and a list",
[IsTransformationMonoid, IsList],
function(filt, params)
  return Monoid(List([1 .. params[1]], i -> RandomTransformation(params[2])));
end);

InstallMethod(RandomInverseSemigroupCons,
"for IsTransformationSemigroup and a list",
[IsTransformationSemigroup, IsList],
SEMIGROUPS.DefaultRandomInverseSemigroup);

InstallMethod(RandomInverseMonoidCons,
"for IsTransformationMonoid and a list",
[IsTransformationMonoid, IsList],
SEMIGROUPS.DefaultRandomInverseMonoid);

#############################################################################
## Constructions
#############################################################################

InstallMethod(IsConnectedTransformationSemigroup,
"for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
  return IsConnectedDigraph(DigraphOfActionOnPoints(S));
end);

InstallMethod(FixedPointsOfTransformationSemigroup,
"for a transformation semigroup with generators",
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
  return Difference([1 .. DegreeOfTransformationSemigroup(S)],
                    FixedPointsOfTransformationSemigroup(S));
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
S -> DigraphOfActionOnPoints(S, DegreeOfTransformationSemigroup(S)));

InstallMethod(DigraphOfActionOnPoints,
"for a transformation semigroup with known generators and int",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsInt],
function(S, n)
  local gens, out, range, i, j;

  if n < 0 then
    ErrorNoReturn("Semigroups: DigraphOfActionOnPoints: usage,\n",
                  "the second argument <n> must be non-negative,");
  elif n = 0 then
    return EmptyDigraph(0);
  elif HasDigraphOfActionOnPoints(S)
      and n = DegreeOfTransformationSemigroup(S) then
    return DigraphOfActionOnPoints(S);
  fi;

  gens := GeneratorsOfSemigroup(S);
  out := List([1 .. n], x -> []);
  for i in [1 .. n] do
    for j in [1 .. Length(gens)] do
      range := i ^ gens[j];
      if range > n then
        return fail;
      fi;
      AddSet(out[i], range);
    od;
  od;
  return DigraphNC(out);
end);

SEMIGROUPS.ElementRClass := function(R, largest)
  local o, m, rep, n, base1, S, GroupFunc, out, scc, y, base2, p, x, i;

  if Size(R) = 1 then
    return Representative(R);
  fi;

  o := LambdaOrb(R);
  m := LambdaOrbSCCIndex(R);
  rep := Representative(R);

  n := DegreeOfTransformationSemigroup(Parent(R));
  base1 := DuplicateFreeList(ImageListOfTransformation(rep, n));

  S := StabChainOp(LambdaOrbSchutzGp(o, m), rec(base := base1));
  if largest then
    GroupFunc := SEMIGROUPS.LargestElementConjugateStabChain;
    out := rep * LargestElementStabChain(S, ());
  else
    GroupFunc := SEMIGROUPS.SmallestElementConjugateStabChain;
    out := rep * GroupFunc(S, (), ());
  fi;

  scc := OrbSCC(o)[m];

  for i in [2 .. Length(scc)] do
    y := rep * EvaluateWord(o!.gens,
                            TraceSchreierTreeOfSCCForward(o, m, scc[i]));
    base2 := DuplicateFreeList(ImageListOfTransformation(y, n));
    p := MappingPermListList(base1, base2);

    if largest then
      x := y * GroupFunc(S, (), p);
      if x > out then
        out := x;
      fi;
    else
      x := y * GroupFunc(S, (), p);
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

InstallMethod(DigraphOfActionOnPairs,
"for a transformation semigroup",
[IsTransformationSemigroup],
S -> DigraphOfActionOnPairs(S, DegreeOfTransformationSemigroup(S)));

InstallMethod(DigraphOfActionOnPairs,
"for a transformation semigroup and int",
[IsTransformationSemigroup, IsInt],
function(S, n)
  local gens, PairNumber, NumberPair, nr, nrpairs, out_nbs, inn_nbs, label,
  pair, range, gr, i, j;

  if n < 0 then
    ErrorNoReturn("Semigroups: DigraphOfActionOnPairs: usage,\n",
                  "the second argument <n> must be non-negative,");
  fi;

  gens := GeneratorsOfSemigroup(S);

  if ForAny(gens, x -> ForAny([1 .. n], i -> i ^ x > n)) then
    return fail;
  elif n <= 1 then
    return EmptyDigraph(n);
  elif HasDigraphOfActionOnPairs(S)
      and DegreeOfTransformationSemigroup(S) = n then
    return DigraphOfActionOnPairs(S);
  fi;

  PairNumber := Concatenation([1 .. n], Combinations([1 .. n], 2));
  NumberPair := function(n, x)
    # This function assumes that <x> is a sorted subset of [1 .. n]
    if Length(x) = 1 then
      return x[1];
    fi;
    return n + Binomial(n, 2) - Binomial(n + 1 - x[1], 2) + x[2] - x[1];
  end;

  nr := Length(gens);
  nrpairs := Binomial(n, 2);
  out_nbs := List([1 .. n + nrpairs], x -> []);
  inn_nbs := List([1 .. n + nrpairs], x -> []);
  label   := List([1 .. n + nrpairs], x -> []);

  for i in [n + 1 .. n + nrpairs] do
    pair := PairNumber[i];
    for j in [1 .. nr] do
      range := NumberPair(n, OnSets(pair, gens[j]));
      if not i in inn_nbs[range] then
        Add(inn_nbs[range], i);
        Add(out_nbs[i], range);
        Add(label[i], j);
      fi;
    od;
  od;

  gr := DigraphNC(out_nbs);
  SetInNeighbours(gr, inn_nbs);
  SetDigraphEdgeLabels(gr, label);
  SetDigraphVertexLabels(gr, PairNumber);
  return gr;
end);

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

InstallMethod(IteratorSorted, "for an acting transformation semigroup",
[IsTransformationSemigroup and IsActingSemigroup],
function(S)
  if HasAsSSortedList(S) then
    return IteratorList(AsSSortedList(S));
  fi;
  return CallFuncList(IteratorSortedOp, List(RClasses(S), IteratorSorted));
end);

InstallMethod(IteratorSorted, "for an R-class",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(R)
  local IterFunc, o, m, rep, n, scc, base, S, out, x, image, basei, iter, i;

  IterFunc := SEMIGROUPS.IteratorSortedConjugateStabChain;

  o := LambdaOrb(R);
  m := LambdaOrbSCCIndex(R);
  rep := Representative(R);
  n := DegreeOfTransformationSemigroup(Parent(R));

  scc := OrbSCC(o)[m];
  base := DuplicateFreeList(ImageListOfTransformation(rep, n));
  S := StabChainOp(LambdaOrbSchutzGp(o, m), rec(base := base));
  out := [IteratorByIterator(IterFunc(S, ()),
                             p -> rep * p, [IsIteratorSorted])];

  for i in [2 .. Length(scc)] do
    x := rep * EvaluateWord(o!.gens,
                            TraceSchreierTreeOfSCCForward(o, m, scc[i]));
    image := ImageListOfTransformation(x, n);
    basei := DuplicateFreeList(image);
    iter := IterFunc(S, MappingPermListList(base, basei));
    out[i] := IteratorByIterator(iter,
                                 function(iter, p)
                                   return iter!.rep * p;
                                 end,
                                 [IsIteratorSorted], ReturnTrue,
                                 rec(rep := Transformation(image)));
  od;
  return CallFuncList(IteratorSortedOp, out);
end);

InstallMethod(\<, "for transformation semigroups",
[IsTransformationSemigroup, IsTransformationSemigroup],
function(S, T)
  if DegreeOfTransformationSemigroup(S)
      <> DegreeOfTransformationSemigroup(T) then
    return DegreeOfTransformationSemigroup(S)
      < DegreeOfTransformationSemigroup(T);
  fi;
  TryNextMethod();
end);

InstallMethod(SmallestElementSemigroup,
"for an acting transformation semigroup",
[IsTransformationSemigroup and IsActingSemigroup],
function(S)
  local n;

  n := DegreeOfTransformationSemigroup(S);

  if n = 0 then
    return IdentityTransformation;
  elif ConstantTransformation(n, 1) in MinimalIdeal(S) then
    return ConstantTransformation(n, 1);
  fi;

  return Minimum(List(RClasses(S), SEMIGROUPS.SmallestElementRClass));
end);

InstallMethod(LargestElementSemigroup, "for an acting transformation semigroup",
[IsTransformationSemigroup and IsActingSemigroup],
function(S)
  local n;

  n := DegreeOfTransformationSemigroup(S);

  if n = 0 then
    return IdentityTransformation;
  elif ConstantTransformation(n, n) in MinimalIdeal(S) then
    return ConstantTransformation(n, n);
  fi;

  return Maximum(List(RClasses(S), SEMIGROUPS.LargestElementRClass));
end);

# different method required (but not yet given!! JDM FIXME) for ideals

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
  local graph, i, x;

  graph := EmptyPlist(n);

  for i in [1 .. n] do
    graph[i] := [];
    for x in coll do
      if i ^ x <= n then
        AddSet(graph[i], i ^ x);
      fi;
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
      if IsBound(lookup[j]) then  # <j> is in <set>!
        Add(graph[i], lookup[set[i] ^ x]);
      fi;
    od;
  od;

  return IsStronglyConnectedDigraph(Digraph(graph));
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

# this method comes from PJC's slides from the Lisbon Workshop in July 2014

# same method for ideals

InstallMethod(IsSynchronizingSemigroup,
"for a transformation semigroup and positive integer",
[IsTransformationSemigroup, IsPosInt],
function(S, n)
  local deg, gens, all_perms, r, gr, verts, offset, inn, marked, squashed, x, i,
  j;

  deg := DegreeOfTransformationSemigroup(S);
  if n = 1 then
    return true;
  elif HasIsSynchronizingSemigroup(S) and n <= deg then
    return IsSynchronizingSemigroup(S);
  fi;

  if HasGeneratorsOfSemigroup(S) then
    gens := GeneratorsOfSemigroup(S);
  else
    gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));
  fi;

  all_perms := true;
  for x in gens do
    r := RankOfTransformation(x, n);
    if r = 1 then
      return true;
    elif r < deg then
      all_perms := false;
    fi;
  od;
  if all_perms then
    return false;  # S = <gens> is a group of transformations
  fi;

  gr := DigraphOfActionOnPairs(S);
  # Create <verts> = vertices corresponding to pairs in [1 .. n]
  # These are the vertices that we still need to be collapsible in <gr>
  if n = deg then
    verts := BlistList(DigraphVertices(gr), [n + 1 .. DigraphNrVertices(gr)]);
  else
    verts := BlistList(DigraphVertices(gr), []);
    offset := 0;
    for i in [1 .. n - 1] do
      offset := offset + deg - i + 1;
      for j in [1 .. n - i] do
        verts[offset + j] := true;
      od;
    od;
  fi;

  # Check if any pairs are isolated
  if ForAny([deg + 1 .. DigraphNrVertices(gr)],
            v -> verts[v] and OutNeighboursOfVertex(gr, v) = [v]) then
    return false;
  fi;

  inn := InNeighbours(gr);

  marked := BlistList(DigraphVertices(gr), []);
  # <marked[i]> iff <i> has been visited in the search of <gr>
  squashed := [1 .. n];
  for i in squashed do
    for j in inn[i] do
      if not marked[j] then
        marked[j] := true;
        verts[j] := false;
        Add(squashed, j);
      fi;
    od;
  od;

  return SizeBlist(verts) = 0;
end);

InstallMethod(RepresentativeOfMinimalIdealNC, "for a transformation semigroup",
[IsTransformationSemigroup], RankFilter(IsActingSemigroup),
# to beat the default method for acting semigroups
function(S)
  local n, gens, nrgens, min_rank, rank, min_rank_index, gr, inn, elts, marked,
  squashed, j, t, im, NumberPair, reduced, y, i, k, x;

  n := DegreeOfTransformationSemigroup(S);  # Smallest n such that S <= T_n
                                            # We must have n >= 2.
  gens := GeneratorsOfSemigroup(S);
  nrgens := Length(gens);
  # Find the minimum rank of a generator
  min_rank := n;
  for i in [1 .. nrgens] do
    rank := RankOfTransformation(gens[i], n);
    if rank = 1 then
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

  if (HasSize(S) and Size(S) < Binomial(n, 2))
      or n > 10000 then
    TryNextMethod();
  fi;

  gr := DigraphOfActionOnPairs(S);
  inn := InNeighbours(gr);

  # find a word describing a path from each collapsible pair to a singleton
  elts := EmptyPlist(DigraphNrVertices(gr));
  marked := BlistList(DigraphVertices(gr), []);
  squashed := [1 .. n];
  for i in squashed do
    for k in [1 .. Length(inn[i])] do
      j := inn[i][k];
      if not marked[j] then
        marked[j] := true;
        if i <= n then
          elts[j] := [DigraphEdgeLabel(gr, j, i)];
        else
          elts[j] := Concatenation([DigraphEdgeLabel(gr, j, i)], elts[i]);
        fi;
        Add(squashed, j);
      fi;
    od;
  od;

  t := gens[min_rank_index];
  im := ImageSetOfTransformation(t, n);

  NumberPair := function(n, x)
    # This function assumes that <x> is a sorted 2-subset of [1 .. n]
    return n + Binomial(n, 2) - Binomial(n + 1 - x[1], 2) + x[2] - x[1];
  end;

  # find a word in S of minimal rank by repeatedly collapsing pairs in im(t)
  while true do
    reduced := false;
    for x in IteratorOfCombinations(im, 2) do
      y := NumberPair(n, x);
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

InstallMethod(WreathProduct,
"for a transformation monoid and a permutation group",
[IsTransformationMonoid, IsPermGroup],
function(M, G)
  return WreathProduct(M, AsMonoid(IsTransformationMonoid, G));
end);

InstallMethod(WreathProduct,
"for a permutation group and a transformation semigroup",
[IsPermGroup, IsTransformationSemigroup],
function(G, S)
  return WreathProduct(AsMonoid(IsTransformationMonoid, G), S);
end);

InstallMethod(WreathProduct,
"for a transformation monoid and a transformation semigroup",
[IsTransformationMonoid, IsTransformationSemigroup],
function(M, S)
  local m, gensM, gensS, orbs, n, rimage, maps, next, gen1, newmap, x, y, s, i;

  if not IsMonoidAsSemigroup(S) then
    ErrorNoReturn("Semigroups: WreathProduct: usage,\n",
                  "the second argument <S> should be a monoid (as semigroup),");
  fi;

  m := DegreeOfTransformationCollection(M);

  gensM := List(GeneratorsOfMonoid(M), x -> ImageListOfTransformation(x, m));
  gensS := GeneratorsOfSemigroup(S);

  orbs := List(ComponentsOfTransformationSemigroup(S), Minimum);
  n := DegreeOfTransformationCollection(S);
  rimage := [1 .. n];

  for x in orbs do
    for y in gensS do
      RemoveSet(rimage, x ^ y);
    od;
  od;

  maps := [];  # final generating set for the wreath product

  # move copies of M as by the action induced by S
  next := [1 .. m * n];
  for s in gensS do
    for i in [1 .. n] do
      next{[1 .. m] + (i - 1) * m} := [1 .. m] + (i ^ s - 1) * m;
    od;
    Add(maps, Transformation(next));
  od;

  gen1 := gensS[1];
  for i in orbs do
    newmap := ShallowCopy(ImageListOfTransformation(maps[1], m * n));
    for x in gensM do
      newmap{[1 .. m] + (i - 1) * m} := x + (i ^ gen1 - 1) * m;
      Add(maps, Transformation(newmap));
    od;
  od;

  for i in rimage do
    newmap := OnTuples([1 .. m * n], maps[1]);
    for x in gensM do
      newmap{[1 .. m] + (i - 1) * m} := x + (i ^ gen1 - 1) * m;
      Add(maps, Transformation(newmap));
    od;
  od;

  return Semigroup(maps);
end);

#############################################################################
# ?. Isomorphisms
#############################################################################

InstallMethod(IsomorphismSemigroup,
"for IsTransformationSemigroup and a semigroup",
[IsTransformationSemigroup, IsSemigroup],
function(filt, S)
  return IsomorphismTransformationSemigroup(S);
end);

InstallMethod(IsomorphismMonoid,
"for IsTransformationMonoid and a semigroup",
[IsTransformationMonoid, IsSemigroup],
function(filt, S)
  return IsomorphismTransformationMonoid(S);
end);

# there could be an even faster C/C++ version of this
# TODO AntiIsomorphismTransformationSemigroup using LeftCayleyGraph

InstallMethod(IsomorphismTransformationSemigroup,
"for an enumerable semigroup",
[IsEnumerableSemigroupRep], 2,
# to beat the method in the library (which has "and HasGeneratorsOfSemigroup")
function(S)
  local cay, deg, gen, next, T, iso, inv, i;

  if not IsFinite(S) then
    # This is unreachable in tests, since there is no other method that
    # terminates
    TryNextMethod();
  fi;

  cay := OutNeighbours(RightCayleyDigraph(S));
  deg := Size(S);
  gen := [];

  for i in [1 .. Length(cay[1])] do
    next := List([1 .. deg], j -> cay[j][i]);
    if MultiplicativeNeutralElement(S) = fail then
      Add(next, i);
    fi;
    Add(gen, Transformation(next));
  od;

  T := Semigroup(gen);
  UseIsomorphismRelation(S, T);

  iso := function(x)
    return EvaluateWord(gen, MinimalFactorization(S, x));
  end;

  inv := function(x)
    return EvaluateWord(GeneratorsOfSemigroup(S), Factorization(T, x));
  end;

  # TODO replace this with SemigroupIsomorphismByImagesOfGenerators
  return MagmaIsomorphismByFunctionsNC(S, T, iso, inv);
end);

InstallMethod(IsomorphismTransformationSemigroup,
"for an fp monoid",
[IsFpMonoid],
function(S)
  local cay, deg, gen, i, next, T, iso, inv;

  if not IsFinite(S) then
    # This is unreachable in tests, since there is not other method that
    # terminates
    TryNextMethod();
  fi;

  cay := OutNeighbours(RightCayleyDigraph(S));
  deg := Size(S);
  gen := EmptyPlist(Length(cay[1]));

  for i in [1 .. Length(cay[1])] do
    next := List([1 .. deg], j -> cay[j][i]);
    gen[i] := Transformation(next);
  od;

  T := Semigroup(gen);
  UseIsomorphismRelation(S, T);

  iso := function(x)
    return EvaluateWord(gen, MinimalFactorization(S, x));
  end;

  inv := function(x)
    return EvaluateWord(GeneratorsOfSemigroup(S), Factorization(T, x));
  end;

  # TODO replace this with SemigroupIsomorphismByImagesOfGenerators
  return MagmaIsomorphismByFunctionsNC(S, T, iso, inv);
end);

InstallMethod(IsomorphismTransformationSemigroup,
"for a boolean matrix semigroup with generators",
[IsBooleanMatSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local T, map, inv, n, pts, o, pos, i;
  n := Length(Representative(S)![1]);
  if ForAll(GeneratorsOfSemigroup(S), IsTransformationBooleanMat) then
    T := Semigroup(List(GeneratorsOfSemigroup(S), AsTransformation));
    map := AsTransformation;
    inv := x -> AsBooleanMat(x, n);
  else
    pts := [];
    for i in [1 .. n] do
      o := Enumerate(Orb(S, BlistList([1 .. n], [i]), OnBlist));
      pts := Union(pts, AsList(o));
    od;
    pos := List([1 .. n], x -> Position(pts, BlistList([1 .. n], [x])));
    T := Semigroup(List(GeneratorsOfSemigroup(S),
                        x -> TransformationOpNC(x, pts, OnBlist)));
    map := x -> TransformationOpNC(x, pts, OnBlist);
    inv := x -> BooleanMat(List([1 .. n], i -> pts[pos[i] ^ x]));
  fi;
  UseIsomorphismRelation(S, T);

  return MagmaIsomorphismByFunctionsNC(S, T, map, inv);
end);

InstallMethod(IsomorphismTransformationSemigroup,
"for a bipartition semigroup with generators",
[IsBipartitionSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local T, n;

  if not ForAll(GeneratorsOfSemigroup(S), IsTransBipartition) then
    TryNextMethod();
  fi;

  T := Semigroup(List(GeneratorsOfSemigroup(S), AsTransformation));
  n := DegreeOfBipartitionSemigroup(S);
  UseIsomorphismRelation(S, T);

  return MagmaIsomorphismByFunctionsNC(S,
                                       T,
                                       AsTransformation,
                                       x -> AsBipartition(x, n));
end);

InstallMethod(IsomorphismTransformationSemigroup,
"for semigroup of binary relations with generators",
[IsSemigroup and IsGeneralMappingCollection and HasGeneratorsOfSemigroup],
function(S)
  local n, pts, o, pos, T, i;

  if not IsBinaryRelationOnPointsRep(Representative(S)) then
    TryNextMethod();
  fi;
  n := DegreeOfBinaryRelation(GeneratorsOfSemigroup(S)[1]);
  pts := EmptyPlist(2 ^ n);

  for i in [1 .. n] do
    o := Orb(S, [i], OnPoints);
    Enumerate(o);
    pts := Union(pts, AsList(o));
  od;
  ShrinkAllocationPlist(pts);
  pos := List([1 .. n], x -> Position(pts, [x]));
  T := Semigroup(List(GeneratorsOfSemigroup(S),
                 x -> TransformationOpNC(x, pts, OnPoints)));

  return MappingByFunction(S,
                           T,
                           x -> TransformationOpNC(x, pts, OnPoints),
                           x -> BinaryRelationOnPoints(List([1 .. n], i ->
                                                            pts[pos[i] ^ x])));
end);

InstallMethod(IsomorphismTransformationSemigroup,
"for a semigroup ideal",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(I)
  local iso, inv, J;

  iso := IsomorphismTransformationSemigroup(SupersemigroupOfIdeal(I));
  inv := InverseGeneralMapping(iso);
  J := SemigroupIdeal(Range(iso), Images(iso, GeneratorsOfSemigroupIdeal(I)));
  UseIsomorphismRelation(I, J);

  return MagmaIsomorphismByFunctionsNC(I, J, x -> x ^ iso, x -> x ^ inv);
end);

#############################################################################
# ?. Attributes
#############################################################################

# same method for ideals

InstallMethod(GroupOfUnits, "for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
  local H, map, G, U, iso;

  if MultiplicativeNeutralElement(S) = fail then
    return fail;
  fi;

  H := GreensHClassOfElementNC(S, MultiplicativeNeutralElement(S));
  map := InverseGeneralMapping(IsomorphismPermGroup(H));
  G := Source(map);
  U := Semigroup(List(GeneratorsOfGroup(G), x -> x ^ map));
  SetIsGroupAsSemigroup(U, true);
  UseIsomorphismRelation(U, G);

  iso := MagmaIsomorphismByFunctionsNC(U,
                                       G,
                                       PermutationOfImage,
                                       x -> x ^ map);
  SetIsomorphismPermGroup(U, iso);

  return U;
end);

InstallMethod(DegreeOfTransformationCollection,
"for a transformation semigroup",
[IsTransformationSemigroup], DegreeOfTransformationSemigroup);

InstallMethod(DegreeOfTransformationSemigroup,
"for a transformation semigroup ideal",
[IsTransformationSemigroup and IsSemigroupIdeal],
function(I)
  return DegreeOfTransformationSemigroup(SupersemigroupOfIdeal(I));
end);

# TODO replace this method with one using Digraphs

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

InstallMethod(ComponentsOfTransformationSemigroup,
"for a transformation semigroup", [IsTransformationSemigroup],
function(S)
  return DigraphConnectedComponents(DigraphOfActionOnPoints(S)).comps;
end);

InstallMethod(CyclesOfTransformationSemigroup,
"for a transformation semigroup", [IsTransformationSemigroup],
function(S)
  return DigraphAllSimpleCircuits(DigraphOfActionOnPoints(S));
end);

InstallMethod(EndomorphismMonoid, "for a digraph",
[IsDigraph],
function(digraph)
  local hook, S;

  if HasGeneratorsOfEndomorphismMonoidAttr(digraph)
      or SEMIGROUPS.DefaultOptionsRec.acting = false then
    return Monoid(GeneratorsOfEndomorphismMonoidAttr(digraph),
                  rec(small := true));
  fi;

  S := [AsMonoid(IsTransformationMonoid, AutomorphismGroup(digraph))];

  hook := function(S, f)
    S[1] := ClosureMonoid(S[1], f);
  end;

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

  S := [AsMonoid(IsTransformationMonoid,
                 AutomorphismGroup(digraph, colors))];

  return HomomorphismDigraphsFinder(digraph, digraph, hook, S, infinity,
                                    fail, false, DigraphVertices(digraph), [],
                                    colors, colors)[1];

end);

InstallMethod(DigraphCore,
"for a digraph with generators of endomorphism monoid",
[IsDigraph and HasGeneratorsOfEndomorphismMonoidAttr],
function(D)
  local x;
  x := RepresentativeOfMinimalIdeal(EndomorphismMonoid(D));
  return ImageSetOfTransformation(x, DigraphNrVertices(D));
end);
