#############################################################################
##
##  attributes/acting.gi
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for finding various attributes of acting
# semigroups, sometimes there is no better method than that given in
# attr.gi.

# same method for ideals

InstallMethod(IsMultiplicativeZero,
"for an acting semigroup and element",
[IsActingSemigroup, IsMultiplicativeElement],
{S, x} -> MultiplicativeZero(S) <> fail and x = MultiplicativeZero(S));

# same method for ideals

InstallMethod(IsGreensDGreaterThanFunc, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local D, data;

  D    := PartialOrderOfDClasses(S);
  data := SemigroupData(S);

  return
  function(x, y)
    local u, v;
    if x = y then
      return false;
    fi;
    x := ConvertToInternalElement(S, x);
    y := ConvertToInternalElement(S, y);
    u := OrbSCCLookup(data)[Position(data, x)] - 1;
    v := OrbSCCLookup(data)[Position(data, y)] - 1;
    return u <> v and IsReachable(D, u, v);
  end;
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
  local regular, lambda, rank, rhorank, tester, j, o, rhos, opts, gens, grades,
  rho_x, lambdarank, creator, inv, out, k, y, i, name, rho;

  regular := IsRegularSemigroup(S);
  if not (regular or IsRegularSemigroupElementNC(S, x)) then
    return [];
  fi;

  x := ConvertToInternalElement(S, x);
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
    opts := rec(treehashsize   := SEMIGROUPS.OptionsRec(S).hashlen,
                gradingfunc    := {o, x} -> rhorank(x),
                onlygrades     := {x, y} -> x >= rank,
                onlygradesdata := fail);

    for name in RecNames(LambdaOrbOpts(S)) do
      opts.(name) := LambdaOrbOpts(S).(name);
    od;
    gens := List(GeneratorsOfSemigroup(S),
                 x -> ConvertToInternalElement(S, x));

    o := Enumerate(Orb(gens, RhoOrbSeed(S), RhoAct(S), opts));

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

  # Notes: it seems that LambdaOrb(S) is always closed at this point
  o := LambdaOrb(S);
  Enumerate(o);  # just in case
  for i in [2 .. Length(o)] do
    if lambdarank(o[i]) = rank and tester(o[i], rho_x) then
      for rho in rhos do
        y := creator(lambda, rho) * inv(o[i], x);
        if regular or y in S then
          k := k + 1;
          out[k] := ConvertToExternalElement(S, y);
        fi;
      od;
    fi;
  od;
  return out;
end);

# same method for ideals

InstallMethod(MultiplicativeNeutralElement, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local gens, rank, lambda, max, rep, r, e, lo, ro, lact, ract, ie;

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

  if max = ActionDegree(S)
      and (IsMultiplicativeElementWithOneCollection(S)
           or IsFFECollCollColl(S)) then
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
  ie := ConvertToInternalElement(S, e);

  # S is an ideal without GeneratorsOfSemigroup
  if ForAll(gens, x -> x * e = x and e * x = x)
      and ForAll([2 .. Length(Enumerate(lo))], i -> lact(lo[i], ie) = lo[i])
      and ForAll([2 .. Length(Enumerate(ro))], i -> ract(ro[i], ie) = ro[i])
      then
    return e;
  fi;
  return fail;
end);

InstallMethod(RepresentativeOfMinimalIdealNC,
"for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local rank, o, pos, min, len, m, result, i;

  rank := LambdaRank(S);
  o := LambdaOrb(S);
  pos := LookForInOrb(o, {o, x} -> rank(x) = MinActionRank(S), 2);

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

  result := EvaluateWord(o, TraceSchreierTreeForward(o, pos));
  return ConvertToExternalElement(S, result);
end);

InstallMethod(RightIdentity,
"for an acting semigroup with generators + mult. elt.",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsMultiplicativeElement],
function(S, x)
  local o, l, m, scc, f, p;

  if not x in S then
    ErrorNoReturn("the 2nd argument (a mult. elt.) does not belong to ",
                  "the 1st argument (a semigroup)");
  elif IsMonoid(S) then
    return One(S);
  elif IsMonoidAsSemigroup(S) then
    return MultiplicativeNeutralElement(S);
  elif IsIdempotent(x) then
    return x;
  fi;

  x := ConvertToInternalElement(S, x);
  o := Enumerate(LambdaOrb(S));
  l := Position(o, LambdaFunc(S)(x));
  m := OrbSCCLookup(o)[l];
  scc := OrbSCC(o)[m];

  if l <> scc[1] then
    f := LambdaOrbMult(o, m, l);
    return ConvertToExternalElement(S, f[2] * f[1]);
  else
    p := Factorization(o, m, LambdaIdentity(S)(true));
    if p = fail then
      return fail;
    else
      return ConvertToExternalElement(S, EvaluateWord(o!.gens, p));
    fi;
  fi;
end);

InstallMethod(LeftIdentity,
"for an acting semigroup with generators + mult. elt.",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsMultiplicativeElement],
function(S, x)
  local l, D, p, result;

  if not x in S then
    ErrorNoReturn("the 2nd argument (a mult. elt.) does not belong to ",
                  "the 1st argument (a semigroup)");
  elif IsMonoid(S) then
    return One(S);
  elif IsMonoidAsSemigroup(S) then
    return MultiplicativeNeutralElement(S);
  elif IsIdempotent(x) then
    return x;
  fi;

  x := ConvertToInternalElement(S, x);
  l := Position(Enumerate(RhoOrb(S)), RhoFunc(S)(x));
  D := Digraph(OrbitGraph(RhoOrb(S)));
  p := DigraphPath(D, l, l);
  if p = fail then
    return fail;
  fi;
  result := EvaluateWord(RhoOrb(S)!.gens, Reversed(p[2]));
  result := ConvertToExternalElement(S, result);
  return result ^ SmallestIdempotentPower(result);
end);
