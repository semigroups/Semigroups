#############################################################################
##
#W  attract.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for finding various attributes of acting
# semigroups, sometimes there is no better method than that given in
# attributes.gi.

# same method for ideals

InstallMethod(IsMultiplicativeZero,
"for an acting semigroup and element",
[IsActingSemigroup, IsMultiplicativeElement],
function(S, x)
  return MultiplicativeZero(S) <> fail and x = MultiplicativeZero(S);
end);

# same method for ideals
# TODO take the transitive closure of the partial order and use that instead of
# comp_index

InstallMethod(IsGreensDGreaterThanFunc, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local gr, data;

  gr := Digraph(PartialOrderOfDClasses(S));
  gr := DigraphReflexiveTransitiveClosure(gr);
  data := SemigroupData(S);

  return
  function(x, y)
    local u, v;
    if x = y then
      return false;
    fi;
    u := OrbSCCLookup(data)[Position(data, x)] - 1;
    v := OrbSCCLookup(data)[Position(data, y)] - 1;
    return u <> v and IsReachable(gr, u, v);
  end;
end);

# different method for ideals/regular/inverse, although this method also works

InstallMethod(MaximalDClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local gens, partial, data, pos, i, out, classes, x;

  gens := GeneratorsOfSemigroup(s);
  partial := PartialOrderOfDClasses(s);
  data := SemigroupData(s);
  pos := [];
  for x in gens do
    i := OrbSCCLookup(data)[Position(data, x)] - 1;
    #index of the D-class containing x
    AddSet(pos, i);
  od;

  out := [];
  classes := GreensDClasses(s);
  for i in pos do
    if not ForAny([1 .. Length(partial)], j -> j <> i and i in partial[j]) then
      Add(out, classes[i]);
    fi;
  od;

  return out;
end);

# same method for inverse, different method for inverse ideals

InstallMethod(MaximalDClasses, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(S)
  local gens, partial, pos, o, scc, out, classes, x, i;

  gens := GeneratorsOfSemigroup(S);
  partial := PartialOrderOfDClasses(S);
  pos := [];
  o := LambdaOrb(S);
  scc := OrbSCCLookup(o);

  for x in gens do
    #index of the D-class containing x
    AddSet(pos, scc[Position(o, LambdaFunc(S)(x))] - 1);
  od;

  out := [];
  classes := GreensDClasses(S);
  for i in pos do
    if not ForAny([1 .. Length(partial)], j -> j <> i and i in partial[j]) then
      Add(out, classes[i]);
    fi;
  od;

  return out;
end);

# same method for ideals

InstallMethod(StructureDescriptionSchutzenbergerGroups,
"for an acting semigroup", [IsActingSemigroup],
function(S)
  local o, scc, out, m;

  o := LambdaOrb(S);
  Enumerate(o, infinity);
  scc := OrbSCC(o);
  out := [];

  for m in [2 .. Length(scc)] do
    AddSet(out, StructureDescription(LambdaOrbSchutzGp(o, m)));
  od;

  return out;
end);

# different method for ideals

InstallMethod(InversesOfSemigroupElementNC,
"for an acting semigroup and multiplicative element",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsMultiplicativeElement],
function(S, x)
  local regular, lambda, rank, rhorank, tester, j, o, rhos, opts,
    grades, rho_x, lambdarank, creator, inv, out, k, y, i, name, rho;

  regular := IsRegularSemigroup(S);
  if not (regular or IsRegularSemigroupElementNC(S, x)) then
    return [];
  fi;

  lambda := LambdaFunc(S)(x);
  rank := LambdaRank(S)(LambdaFunc(S)(x));
  rhorank := RhoRank(S);
  tester := IdempotentTester(S);
  j := 0;

  # can't use GradedRhoOrb here since there may be inverses not D-related to f
  if HasRhoOrb(S) and IsClosedOrbit(RhoOrb(S)) then
    o := RhoOrb(S);
    rhos := EmptyPlist(Length(o));
    for i in [2 .. Length(o)] do
      if rhorank(o[i]) = rank and tester(lambda, o[i]) then
        j := j + 1;
        rhos[j] := o[i];
      fi;
    od;
  else
    opts := rec(treehashsize := SEMIGROUPS.OptionsRec(S).hashlen,
                gradingfunc := function(o, x) return rhorank(x); end,
                onlygrades := function(x, y) return x >= rank; end,
                onlygradesdata := fail);

    for name in RecNames(LambdaOrbOpts(S)) do
      opts.(name) := LambdaOrbOpts(S).(name);
    od;

    o := Orb(GeneratorsOfSemigroup(S), RhoOrbSeed(S), RhoAct(S), opts);
    Enumerate(o, infinity);

    grades := Grades(o);
    rhos := EmptyPlist(Length(o));
    for i in [2 .. Length(o)] do
      if grades[i] = rank and tester(lambda, o[i]) then
        j := j + 1;
        rhos[j] := o[i];
      fi;
    od;
  fi;
  ShrinkAllocationPlist(rhos);

  rho_x := RhoFunc(S)(x);
  lambdarank := LambdaRank(S);
  creator := IdempotentCreator(S);
  inv := LambdaInverse(S);

  out := [];
  k := 0;

  #if HasLambdaOrb(S) and IsClosedOrbit(LambdaOrb(S)) then
  # Notes: it seems that LambdaOrb(S) is always closed at this point
  o := LambdaOrb(S);
  Enumerate(o); # just in case
  for i in [2 .. Length(o)] do
    if lambdarank(o[i]) = rank and tester(o[i], rho_x) then
      for rho in rhos do
        y := creator(lambda, rho) * inv(o[i], x);
        if regular or y in S then
          k := k + 1;
          out[k] := y;
        fi;
      od;
    fi;
  od;
  #else
  #   opts := rec(treehashsize := s!.opts.hashlen,
  #               gradingfunc := function(o, x) return lambdarank(x); end,
  #               onlygrades := function(x, y) return x >= rank; end,
  #               onlygradesdata := fail);

  #  for name in RecNames(LambdaOrbOpts(s)) do
  #    opts.(name) := LambdaOrbOpts(s).(name);
  #  od;

  #  o := Orb(s, LambdaOrbSeed(s), LambdaAct(s), opts);
  #  Enumerate(o);
  #  grades := Grades(o);

  #  for i in [2 .. Length(o)] do
  #    if grades[i] = rank and tester(o[i], rho_x) then
  #      for rho in rhos do
  #        g := creator(lambda, rho) * inv(o[i], f);
  #        if regular or g in s then
  #          k := k + 1;
  #          out[k] := g;
  #        fi;
  #      od;
  #    fi;
  #  od;
  #fi;

  return out;
end);

# same method for ideals

InstallMethod(MultiplicativeNeutralElement, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local gens, rank, lambda, max, rep, r, e, lo, ro, lact, ract;

  gens := Generators(S);
  rank := LambdaRank(S);
  lambda := LambdaFunc(S);
  max := 0;
  rep := gens[1];

  for e in gens do
    r := rank(lambda(e));
    if r > max then
      max := r;
      rep := e;
    fi;
  od;

  if max = ActionDegree(S) and IsMultiplicativeElementWithOneCollection(S) then
    return One(S);
  fi;

  r := GreensRClassOfElementNC(S, rep);

  if NrIdempotents(r) <> 1 or NrHClasses(r) <> 1 or
      NrHClasses(GreensLClassOfElementNC(S, rep)) <> 1 then
    Info(InfoSemigroups, 2, "the D-class of the first maximum rank generator ",
                            "is not a group");
    return fail;
  fi;

  e := Idempotents(r)[1];

  if HasGeneratorsOfSemigroup(S) then
    if ForAll(GeneratorsOfSemigroup(S), x -> x * e = x and e * x = x) then
      return e;
    fi;
    return fail;
  fi;

  lo := LambdaOrb(S);
  ro := RhoOrb(S);
  lact := LambdaAct(S);
  ract := RhoAct(S);

  # S is an ideal without GeneratorsOfSemigroup
  if ForAll(gens, x -> x * e = x and e * x = x)
      and ForAll([2 .. Length(Enumerate(lo))], i -> lact(lo[i], e) = lo[i])
      and ForAll([2 .. Length(Enumerate(ro))], i -> ract(ro[i], e) = ro[i]) then
    return e;
  fi;
  return fail;
end);

InstallMethod(RepresentativeOfMinimalIdealNC,
"for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local rank, o, pos, min, len, m, i;

  rank := LambdaRank(S);
  o := LambdaOrb(S);

  pos := LookForInOrb(o, function(o, x)
                           return rank(x) = MinActionRank(S);
                         end, 2);

  if pos = false then
    min := rank(o[2]);
    pos := 2;
    len := Length(o);
    for i in [3 .. len] do
      m := rank(o[i]);
      if m < min then
        pos := i;
        min := m;
      fi;
    od;
  fi;

  return EvaluateWord(o, TraceSchreierTreeForward(o, pos));
end);
