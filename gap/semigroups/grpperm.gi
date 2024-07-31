#############################################################################
##
##  semigroups/grpperm.gi
##  Copyright (C) 2014-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# In this file there are some methods for perm groups that were not found in
# the GAP library.

# Finds the element p of the group G ^ conj with stab chain S ^ conj such that
# the OnTuples(BaseOfStabChain(S) ^ conj, p) is lexicographically maximum. I.e.
# this function returns the same value as:
#
# LargestElementStabChain(StabChainOp(G ^ conj,
#                                     rec(base := BaseOfStabChain(S) ^ conj)));

# TODO(later) doc
InstallMethod(LargestElementConjugateStabChain,
"for a stabilizer chain record and perm", [IsRecord, IsPerm],
function(S, conj)
  local Recurse;

  Recurse := function(S, rep, conj)
    local pnt, max, val, lrep, gen, i;
    if IsEmpty(S.generators) then
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
    return Recurse(S.stabilizer, rep, conj);
  end;
  return Recurse(S, (), conj);
end);

# fall back method, same method for ideals

InstallMethod(IsomorphismPermGroup, "for a semigroup", [CanUseFroidurePin],
function(S)
  local cay, deg, G, gen1, gen2, next, pos, iso, inv, i;
  if not IsFinite(S) then
    TryNextMethod();
  elif not IsGroupAsSemigroup(S) then
    ErrorNoReturn("the argument (a semigroup) does not satisfy ",
                  "IsGroupAsSemigroup");
  fi;

  cay := OutNeighbours(RightCayleyDigraph(S));
  deg := Size(S);
  G   := Group(());
  gen1 := [];
  gen2 := [];
  for i in [1 .. Length(cay[1])] do
    next := PermList(List([1 .. deg], j -> cay[j][i]));
    Add(gen1, next);
    G   := ClosureGroup(G, next);
    pos := Position(GeneratorsOfGroup(G), next);
    if pos <> fail then
      gen2[pos] := i;
    fi;
  od;

  UseIsomorphismRelation(S, G);

  iso := x -> EvaluateWord(gen1, Factorization(S, x));

  inv := function(x)
    local w, i;
    w := ExtRepOfObj(Factorization(G, x));
    if IsEmpty(w) then
      return MultiplicativeNeutralElement(S);
    fi;
    for i in [2, 4 .. Length(w)] do
      if w[i] < 0 then
        w[i] := Order(GeneratorsOfGroup(G)[w[i - 1]]) + w[i];
      fi;
    od;
    w := SEMIGROUPS.ExtRepObjToWord(w);
    return EvaluateWord(GeneratorsOfSemigroup(S),
                        List(w, x -> gen2[x]));
  end;

  return SemigroupIsomorphismByFunctionNC(S, G, iso, inv);
end);

InstallMethod(IsomorphismPermGroup, "for a partial perm semigroup",
[IsPartialPermSemigroup],
function(S)
  local G, dom;

  if not IsGroupAsSemigroup(S) then
    ErrorNoReturn("the argument (a partial perm semigroup) ",
                  "does not satisfy IsGroupAsSemigroup");
  fi;

  G := Group(List(GeneratorsOfSemigroup(S), AsPermutation));
  UseIsomorphismRelation(S, G);

  dom := DomainOfPartialPermCollection(S);

  return SemigroupIsomorphismByFunctionNC(S,
                                          G,
                                          AsPermutation,
                                          x -> AsPartialPerm(x, dom));
end);

InstallMethod(IsomorphismPermGroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
  local G, id;

  if not IsGroupAsSemigroup(S) then
    ErrorNoReturn("the argument (a transformation semigroup) does ",
                  "not satisfy IsGroupAsSemigroup");
  fi;

  G := Group(List(GeneratorsOfSemigroup(S), PermutationOfImage));
  UseIsomorphismRelation(S, G);
  id := MultiplicativeNeutralElement(S);

  return SemigroupIsomorphismByFunctionNC(S,
                                       G,
                                       PermutationOfImage,
                                       x -> id * x);
end);

InstallMethod(IsomorphismPermGroup, "for a perm bipartition group",
[IsPermBipartitionGroup],
1,  # to beat the method for IsBlockBijectionSemigroup
function(S)
  local G, deg;

  G := Group(List(GeneratorsOfSemigroup(S), AsPermutation));
  UseIsomorphismRelation(S, G);
  deg := DegreeOfBipartitionSemigroup(S);

  return SemigroupIsomorphismByFunctionNC(S,
                                       G,
                                       AsPermutation,
                                       x -> AsBipartition(x, deg));
end);

InstallMethod(IsomorphismPermGroup, "for a bipartition semigroup",
[IsBipartitionSemigroup],
function(S)
  local iso, inv;
  if not IsBlockBijectionSemigroup(S) then
    TryNextMethod();
  elif not IsGroupAsSemigroup(S) then
    ErrorNoReturn("the argument (a bipartition semigroup) does ",
                  "not satisfy IsGroupAsSemigroup");
  fi;
  iso := IsomorphismPermGroup(GroupHClass(DClass(S, Representative(S))));
  inv := InverseGeneralMapping(iso);
  return SemigroupIsomorphismByFunctionNC(S,
                                       Range(iso),
                                       x -> x ^ iso,
                                       x -> x ^ inv);
end);
