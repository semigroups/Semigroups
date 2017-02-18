#############################################################################
##
#W  grpperm.gi
#Y  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# In this file there are some methods for perm groups that were not found in
# the library.

# Returns an iterator of the sorted elements of the stab chain S ^ conj.

SEMIGROUPS.IteratorSortedConjugateStabChain := function(S, conj)
  local SortedStabChain, record, indices, T, iter;

  # finds the element of the group with stab chain S corresponding to the tuple
  # <indices>.
  SortedStabChain := function(S, rep, indices, level)
    local pnt, x, next, gen;

    if Length(S.generators) = 0  then
      return rep;
    fi;

    pnt := S.orbit[1];
    x := conj * rep;
    next := AsSet(OnTuples(S.orbit, x))[indices[level]] / x;

    while next <> pnt do
      gen := S.transversal[next];
      rep := LeftQuotient(gen ^ conj, rep);
      next := next ^ gen;
    od;

    return SortedStabChain(S.stabilizer, rep, indices, level + 1);
  end;

  record := rec();

  # find the lengths of the orbits in the chain
  indices := [];
  T := S;

  while Length(T.generators) <> 0 do
    Add(indices, [1 .. Length(T.orbit)]);
    T := T.stabilizer;
  od;

  record.indices := IteratorOfCartesianProduct(indices);
  record.stabchain := S;

  record.NextIterator := function(iter)
    if IsDoneIterator(iter!.indices) then
      return fail;
    fi;
    return SortedStabChain(iter!.stabchain, (), NextIterator(iter!.indices),
                           1);
  end;

  record.ShallowCopy := function(iter)
    return rec(indices := ShallowCopy(iter!.indices),
               stabchain := iter!.stabchain);
  end;

  iter := IteratorByNextIterator(record);
  SetFilterObj(iter, IsIteratorSorted);
  return iter;
end;

# Finds the element p of the group G ^ conj with stab chain S ^ conj such that
# the OnTuples(BaseOfStabChain(S) ^ conj, p) is lexicographically maximum. I.e.
# this function returns the same value as:
#
# LargestElementStabChain(StabChainOp(G ^ conj,
#                                     rec(base := BaseOfStabChain(S) ^ conj)));

SEMIGROUPS.LargestElementConjugateStabChain := function(S, rep, conj)
  local pnt, max, val, gen, i, lrep;

  if Length(S.generators) = 0  then
    return rep ^ conj;
  fi;

  pnt := S.orbit[1];
  max := 0;
  val := 0;
  lrep := rep ^ conj;

  for i in S.orbit  do
    if (i ^ conj) ^ lrep > val  then
      max := i;
      val := (i ^ conj) ^ lrep;
    fi;
  od;

  while pnt <> max  do
    gen := S.transversal[max];
    rep := LeftQuotient(gen, rep);
    max := max ^ gen;
  od;
  return SEMIGROUPS.LargestElementConjugateStabChain(S.stabilizer,
                                                     rep,
                                                     conj);
end;

# Finds the element p of the group G ^ conj with stab chain S ^ conj such that
# the OnTuples(BaseOfStabChain(S) ^ conj, p) is lexicographically minimum.
# Note that since the base of the stab chain can be anything, the return value
# of this function is not always the identity perm.

SEMIGROUPS.SmallestElementConjugateStabChain := function(S, rep, conj)
  local pnt, min, val, lrep, gen, i;

  if Length(S.generators) = 0  then
    return rep ^ conj;
  fi;

  pnt := S.orbit[1];
  min := 0;
  val := infinity;
  lrep := rep ^ conj;

  for i in S.orbit  do
    if (i ^ conj) ^ lrep < val  then
      min := i;
      val := (i ^ conj) ^ lrep;
    fi;
  od;

  while pnt <> min  do
    gen := S.transversal[min];
    rep := LeftQuotient(gen, rep);
    min := min ^ gen;
  od;

  return SEMIGROUPS.SmallestElementConjugateStabChain(S.stabilizer,
                                                      rep,
                                                      conj);
end;

# fall back method, same method for ideals

InstallMethod(IsomorphismPermGroup, "for a semigroup",
[IsSemigroup],
function(S)
  local cay, deg, gen, next, G, iso, inv, i;

  if not IsFinite(S) then
    TryNextMethod();
  elif not IsGroupAsSemigroup(S) then
    ErrorNoReturn("Semigroups: IsomorphismPermGroup: usage,\n",
                  "the argument must be a semigroup satisfying ",
                  "IsGroupAsSemigroup,");
  fi;

  cay := RightCayleyGraphSemigroup(S);
  deg := Size(S);
  gen := [];

  for i in [1 .. Length(cay[1])] do
    next := List([1 .. deg], j -> cay[j][i]);
    Add(gen, PermList(next));
  od;

  G := Semigroup(gen);
  UseIsomorphismRelation(S, G);

  iso := function(x)
    return EvaluateWord(gen, Factorization(S, x));
  end;

  inv := function(x)
    return EvaluateWord(GeneratorsOfSemigroup(S),
                        MinimalFactorization(G, x));
  end;

  #TODO replace this with SemigroupIsomorphismByImagesOfGenerators
  return MagmaIsomorphismByFunctionsNC(S, G, iso, inv);
end);

InstallMethod(IsomorphismPermGroup,
"for a partial perm semigroup",
[IsPartialPermSemigroup],
function(S)
  local G, dom;

  if not IsGroupAsSemigroup(S) then
    ErrorNoReturn("Semigroups: IsomorphismPermGroup: usage,\n",
                  "the argument <S> must be a partial perm semigroup ",
                  "satisfying IsGroupAsSemigroup,");
  fi;

  G := Group(List(GeneratorsOfSemigroup(S), AsPermutation));
  UseIsomorphismRelation(S, G);

  dom := DomainOfPartialPermCollection(S);

  return MagmaIsomorphismByFunctionsNC(S,
                                       G,
                                       AsPermutation,
                                       x -> AsPartialPerm(x, dom));
end);

InstallMethod(IsomorphismPermGroup,
"for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
  local G, id;

  if not IsGroupAsSemigroup(S) then
    ErrorNoReturn("Semigroups: IsomorphismPermGroup: usage,\n",
                  "the argument <S> must satisfy IsGroupAsSemigroup,");
  fi;

  G := Group(List(GeneratorsOfSemigroup(S), PermutationOfImage));
  UseIsomorphismRelation(S, G);
  id := MultiplicativeNeutralElement(S);

  return MagmaIsomorphismByFunctionsNC(S,
                                       G,
                                       PermutationOfImage,
                                       x -> id * x);
end);

InstallMethod(IsomorphismPermGroup,
"for a perm bipartition group",
[IsPermBipartitionGroup],
1, # to beat the method for IsBlockBijectionSemigroup
function(S)
  local G, deg;

  G := Group(List(GeneratorsOfSemigroup(S), AsPermutation));
  UseIsomorphismRelation(S, G);
  deg := DegreeOfBipartitionSemigroup(S);

  return MagmaIsomorphismByFunctionsNC(S,
                                       G,
                                       AsPermutation,
                                       x -> AsBipartition(x, deg));
end);

InstallMethod(IsomorphismPermGroup,
"for a bipartition semigroup",
[IsBipartitionSemigroup],
function(S)
  local iso, inv;

  if not IsBlockBijectionSemigroup(S) then
    TryNextMethod();
  elif not IsGroupAsSemigroup(S) then
    ErrorNoReturn("Semigroups: IsomorphismPermGroup: usage,\n",
                  "the argument must be a semigroup satisfying ",
                  "IsGroupAsSemigroup,");
  fi;

  iso := IsomorphismPermGroup(GroupHClass(DClass(S, Representative(S))));
  inv := InverseGeneralMapping(iso);

  return MagmaIsomorphismByFunctionsNC(S,
                                       Range(iso),
                                       x -> x ^ iso,
                                       x -> x ^ inv);
end);
