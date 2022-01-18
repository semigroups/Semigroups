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
## Operators
#############################################################################

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

#############################################################################
## Isomorphisms
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

# TODO(later) AntiIsomorphismTransformationSemigroup using LeftCayleyGraph

InstallMethod(IsomorphismTransformationSemigroup,
"for a semigroup with CanComputeFroidurePin",
[CanComputeFroidurePin], 2,
# to beat the method in the library (which has "and HasGeneratorsOfSemigroup")
function(S)
  local cay, deg, gen, next, T, iso, inv, i;
  if IsTransformationSemigroup(S) then
    TryNextMethod();
  elif not IsFinite(S) then
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

  # TODO(later) replace this with SemigroupIsomorphismByImagesOfGenerators
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

  # TODO(later) replace this with SemigroupIsomorphismByImagesOfGenerators
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
## Algebraic attributes
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

# can probably do better than this

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

#############################################################################
## Iterators
#############################################################################

# TODO remove
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

#############################################################################
## Degree
#############################################################################

InstallMethod(DegreeOfTransformationCollection,
"for a transformation semigroup",
[IsTransformationSemigroup], DegreeOfTransformationSemigroup);

InstallMethod(DegreeOfTransformationSemigroup,
"for a transformation semigroup ideal",
[IsTransformationSemigroup and IsSemigroupIdeal],
function(I)
  return DegreeOfTransformationSemigroup(SupersemigroupOfIdeal(I));
end);

#############################################################################
## Action on points and pairs
#############################################################################

InstallMethod(DigraphOfAction,
"for a transformation collection, list, and action",
[IsTransformationCollection, IsList, IsFunction],
function(coll, list, act)
  local n, map, out, in_, labels, genstoapply, i, y, index, D, j;

  # TODO arg checks

  n := Length(list);
  if n = 0 then
    return EmptyDigraph(n);
  fi;

  map := HashMap(Length(list));
  for i in [1 .. Length(list)] do
    map[list[i]] := i;
  od;

  # We track the out-neighbours and in-neighbours because at least one
  # application (finding the representative of the minimal ideal) requires
  # this.
  out         := List([1 .. n], x -> []);
  in_         := List([1 .. n], x -> []);
  labels      := List([1 .. n], x -> []);
  genstoapply := [1 .. Length(coll)];

  i := 1;
  repeat
    for j in genstoapply do
      y := act(list[i], coll[j]);
      if y <> fail then
        index := map[y];
        if index = fail then
          n := n + 1;
          list[n] := y;
          map[y] := n;
          Add(out, []);
          Add(in_, []);
          Add(labels, []);
          index := n;
        fi;
        if not i in in_[index] then
          Add(out[i], index);
          Add(in_[index], i);
          Add(labels[i], j);
        fi;
      fi;
    od;
    i := i + 1;
  until i > n;

  D := DigraphNC(out);
  SetDigraphVertexLabels(D, list);
  SetDigraphEdgeLabelsNC(D, labels);
  SetInNeighbours(D, in_);
  return D;
end);

InstallMethod(DigraphOfAction,
"for a transformation semigroup with generators, list, and action",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsList, IsFunction],
function(S, list, act)
  return DigraphOfAction(GeneratorsOfSemigroup(S), list, act);
end);

InstallMethod(DigraphOfActionOnPoints,
"for a transformation collection and int",
[IsTransformationCollection, IsInt],
function(coll, n)
  local act;

  if n < 0 then
    ErrorNoReturn("the 2nd argument (an integer) must be non-negative");
  fi;

  act := function(pt, x)
    local y;
    y := OnPoints(pt, x);
    if y >= 1 and y <= n then
      return y;
    fi;
    return fail;
  end;

  return DigraphOfAction(coll, [1 .. n], act);
end);

InstallMethod(DigraphOfActionOnPoints,
"for a transformation semigroup with known generators and int",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsInt],
function(S, n)
  return DigraphOfActionOnPoints(GeneratorsOfSemigroup(S), n);
end);

InstallMethod(DigraphOfActionOnPoints,
"for a transformation semigroup with known generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
S -> DigraphOfActionOnPoints(S, DegreeOfTransformationSemigroup(S)));

InstallMethod(FixedPointsOfTransformationSemigroup,
"for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local n, gens;
  n    := DegreeOfTransformationSemigroup(S);
  gens := GeneratorsOfSemigroup(S);
  return Filtered([1 .. n], i -> ForAll(gens, x -> i ^ x = i));
end);

InstallMethod(MovedPoints, "for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(S)
  return Difference([1 .. DegreeOfTransformationSemigroup(S)],
                    FixedPointsOfTransformationSemigroup(S));
end);

InstallMethod(IsConnectedTransformationSemigroup,
"for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
  return IsConnectedDigraph(DigraphOfActionOnPoints(S));
end);

InstallMethod(IsTransitive,
"for a transformation collection and a positive int",
[IsTransformationCollection, IsPosInt],
function(coll, n)
  return IsStronglyConnectedDigraph(DigraphOfActionOnPoints(coll, n));
end);

InstallMethod(IsTransitive,
"for a transformation collection and homog. list",
[IsTransformationCollection, IsHomogeneousList],
function(coll, set)
  local n, p;
  # The check for IsSSortedList is here because <set> might be strictly sorted
  # but not know it.
  if not (IsPosInt(set[1]) and IsSSortedList(set)) then
    ErrorNoReturn("the 2nd argument (a list) must be a set of positive ",
                  "integers");
  fi;

  n := Length(set);
  p := MappingPermListList(set, [1 .. n]);
  return IsTransitive(coll ^ p, n);
end);

InstallMethod(IsTransitive,
"for a transformation semigroup with generators and a pos. int.",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsPosInt],
function(S, n)
  return IsTransitive(GeneratorsOfSemigroup(S), n);
end);

InstallMethod(IsTransitive,
"for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(S)
  return IsTransitive(S, DegreeOfTransformationSemigroup(S));
end);

InstallMethod(IsTransitive,
"for a transformation semigroup with generators and a list",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsList],
function(S, set)
  return IsTransitive(GeneratorsOfSemigroup(S), set);
end);

InstallMethod(ComponentRepsOfTransformationSemigroup,
"for a transformation semigroup", [IsTransformationSemigroup],
function(S)
  local D, C;
  D := DigraphMutableCopy(DigraphOfActionOnPoints(S));
  C := DigraphStronglyConnectedComponents(D).comps;
  DigraphRemoveLoops(QuotientDigraph(D, C));
  return List(DigraphSources(D), x -> DigraphVertexLabel(D, x)[1]);
end);

InstallMethod(ComponentsOfTransformationSemigroup,
"for a transformation semigroup", [IsTransformationSemigroup],
function(S)
  return DigraphConnectedComponents(DigraphOfActionOnPoints(S)).comps;
end);

InstallMethod(CyclesOfTransformationSemigroup,
"for a transformation semigroup", [IsTransformationSemigroup],
function(S)
  return DigraphStronglyConnectedComponents(DigraphOfActionOnPoints(S)).comps;
end);

InstallMethod(RepresentativeOfMinimalIdealNC,
"for a transformation semigroup with known generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
RankFilter(IsActingSemigroup),
# to beat the default method for acting semigroups
function(S)
  local n, result, rank, image, labels, D, squashable, paths, seen, neighbours,
  map, nothing_squashed, pos, x, y, i, pair;

  n      := DegreeOfTransformationSemigroup(S);
  result := Product(GeneratorsOfSemigroup(S));
  rank   := RankOfTransformation(result, n);

  # Take the product of the generators is an arbitrary choice, the aim is to
  # try to find a smallish set to use to generate the nodes in the digraph
  # below, and also we might get lucky and find that the product of the
  # generators is of rank 1 or n, in which case we can stop right away.

  if rank = n then
    SetIsGroupAsSemigroup(S, true);
    return result;
  elif rank = 1 then
    return result;
  elif HasSize(S) and Size(S) < Binomial(n, 2) then
    TryNextMethod();
  elif HasLambdaOrb(S) and IsClosedOrbit(LambdaOrb(S)) then
    TryNextMethod();
  fi;

  # Form the digraph of action on points and pairs of points inside the image
  # set of the product of the generators. If n is very large, then it isn't
  # feasible to create a digraph with O(n ^ 2) vertices, so we use the image
  # set of the product of the generators in the hope that this is smaller.
  image := ImageSetOfTransformation(result, n);
  labels := List(ImageSetOfTransformation(result, n), x -> [x]);
  Append(labels, Combinations(ImageSetOfTransformation(result, n), 2));
  D := DigraphOfAction(S, labels, OnSets);

  # Perform a bfs through the reverse of the digraph D to find all those "pair"
  # nodes from which a "point" node is reachable, and to find the edge labels
  # of the path from the "pair" node to the "point" node. When such paths are
  # known we can squash all possible pairs of points in the image of <result>
  # into single points.

  # Sets of size 1 are squashable!
  squashable        := PositionsProperty(DigraphVertexLabels(D),
                                         x -> Size(x) = 1);
  paths             := [];
  paths{squashable} := List([1 .. Length(squashable)], x -> []);
  seen              := BlistList(DigraphVertices(D), squashable);

  for x in squashable do
    neighbours := InNeighboursOfVertex(D, x);
    for y in neighbours do
      if not seen[y] then
        seen[y] := true;
        Add(squashable, y);
        paths[y] := Concatenation([DigraphEdgeLabel(D, y, x)], paths[x]);
      fi;
    od;
  od;

  # The labels may have changed because ActionDigraph may generate additional
  # nodes, so we must renew the labels.
  labels := Filtered(DigraphVertices(D),
                     i -> Size(DigraphVertexLabel(D, i)) = 2
                          and IsBound(paths[i]));
  if IsEmpty(labels) then
    # This means there are no paths from any pair of points in the image of
    # result that can be collapsed.
    return result;
  fi;
  paths  := paths{labels};
  labels := DigraphVertexLabels(D){labels};
  map    := HashMap(Length(labels));
  for i in [1 .. Length(labels)] do
    map[labels[i]] := i;
  od;

  # Form an element of the semigroup that squashes every possible pair of
  # points in the image of <result>.
  repeat
    nothing_squashed := true;
    for pair in IteratorOfCombinations(image, 2) do
      # To ensure that every pair is eventually squashed, we have to see where
      # the pair has ended up under result.
      pair := OnSets(pair, result);
      if Size(pair) = 1 then
        result := result * result;
        image := ImageSetOfTransformation(result, n);
        nothing_squashed := false;
        break;
      fi;
      pos := map[pair];
      if pos <> fail then
        result := result * EvaluateWord(GeneratorsOfSemigroup(S), paths[pos]);
        image := ImageSetOfTransformation(result, n);
        nothing_squashed := false;
        break;
      fi;
    od;
  until nothing_squashed or Size(image) = 1;
  return result;
end);

# same method for ideals

InstallMethod(IsSynchronizingSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
  local N;
  N := DegreeOfTransformationSemigroup(S);
  if N = 0 then
    return false;
  elif HasMultiplicativeZero(S) and MultiplicativeZero(S) <> fail then
    return RankOfTransformation(MultiplicativeZero(S), N) = 1;
  else
    return RankOfTransformation(RepresentativeOfMinimalIdeal(S), N) = 1;
  fi;
end);

#############################################################################
## Smallest, largest element
#############################################################################

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

InstallMethod(SmallestElementSemigroup,
"for an acting transformation semigroup",
[IsTransformationSemigroup and IsActingSemigroup],
function(S)
  local n, SmallestElementRClass;

  n := DegreeOfTransformationSemigroup(S);
  if n = 0 then
    return IdentityTransformation;
  elif HasAsSSortedList(S) then
    return AsSSortedList(S)[1];
  elif HasEnumeratorSorted(S) or HasEnumeratorCanonical(S) then
    return EnumeratorSorted(S)[1];
  elif ConstantTransformation(1, 1) in MinimalIdeal(S) then
    return ConstantTransformation(1, 1);
  fi;

  SmallestElementRClass := R -> SEMIGROUPS.ElementRClass(R, false);

  return Minimum(List(RClasses(S), SmallestElementRClass));
end);

InstallMethod(LargestElementSemigroup, "for an acting transformation semigroup",
[IsTransformationSemigroup and IsActingSemigroup],
function(S)
  local n, LargestElementRClass;

  n := DegreeOfTransformationSemigroup(S);
  if n = 0 then
    return IdentityTransformation;
  elif HasAsSSortedList(S) then
    return AsSSortedList(S)[Size(S)];
  elif HasEnumeratorSorted(S) or HasEnumeratorCanonical(S) then
    return EnumeratorSorted(S)[Size(S)];
  elif ConstantTransformation(n, n) in MinimalIdeal(S) then
    return ConstantTransformation(n, n);
  fi;

  LargestElementRClass := R -> SEMIGROUPS.ElementRClass(R, true);

  return Maximum(List(RClasses(S), LargestElementRClass));
end);

#############################################################################
# Constructions (e.g. wreath product)
#############################################################################

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
    ErrorNoReturn("the 2nd argument (a transformation semigroup) ",
                  "should be a monoid (as semigroup)");
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
# Endomorphisms of digraphs
#############################################################################

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
