#############################################################################
##
#W  attributes-acting.gi
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
[IsActingSemigroup, IsAssociativeElement],
function(S, x)
  return MultiplicativeZero(S) <> fail and x = MultiplicativeZero(S);
end);

# same method for ideals
# TODO take the transitive closure of the partial order and use that instead of
# comp_index

InstallMethod(IsGreensDLeq, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local partial, data, comp_index;

  partial := PartialOrderOfDClasses(S);
  data := SemigroupData(S);

  comp_index := function(x, y)
    if y in partial[x] then
      return true;
    elif Length(partial[x]) = 1 and partial[partial[x][1]] = partial[x] then
      return false;
    fi;
    return ForAny(partial[x], z -> z <> x and comp_index(z, y));
  end;

  return function(x, y)
    return comp_index(OrbSCCLookup(data)[Position(data, x)] - 1,
                      OrbSCCLookup(data)[Position(data, y)] - 1);
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

# same method for ideals

InstallMethod(IdempotentGeneratedSubsemigroup, "for an acting semigroup",
[IsActingSemigroup], S -> Semigroup(Idempotents(S), rec(small := true)));

# same method for ideals

InstallMethod(IdempotentGeneratedSubsemigroup,
"for an inverse op acting semigroup", 
[IsSemigroupWithInverseOp and IsActingSemigroup],
S -> InverseSemigroup(Idempotents(S), rec(small := true)));

# same method for ideals

InstallMethod(InjectionPrincipalFactor, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local g, rep, rreps, lreps, mat, inv_l, inv_r, lambdaperm, leftact, rightact,
  f, rms, iso, inv, hom, i, j;

  if not IsRegularDClass(d) then
    Error("Semigroups: InjectionPrincipalFactor: usage,\n",
          "the argument <d> must be a regular D-class,");
    return;
  fi;

  g := GroupHClass(d);
  rep := Representative(g);
  g := Range(IsomorphismPermGroup(g));

  rreps := HClassReps(LClass(d, rep));
  lreps := HClassReps(RClass(d, rep));
  mat := [];

  inv_l := EmptyPlist(Length(lreps));
  inv_r := EmptyPlist(Length(rreps));

  lambdaperm := LambdaPerm(Parent(d));
  if IsTransformationSemigroupGreensClass(d)
      or IsPartialPermSemigroupGreensClass(d)
      or IsBipartitionSemigroupGreensClass(d) then
    leftact := PROD;
  elif IsReesZeroMatrixSubsemigroup(Parent(d)) then
    leftact := function(x, y)
      return Objectify(TypeObj(y),
                       [y![1], y![4][rep![3]][rep![1]] ^ -1 * x * rep![2] ^ -1
                        * y![2], y![3], y![4]]);
    end;
  fi;

  rightact := StabilizerAction(Parent(d));

  for i in [1 .. Length(lreps)] do
    mat[i] := [];
    for j in [1 .. Length(rreps)] do
      f := lreps[i] * rreps[j];
      if f in d then
        mat[i][j] := lambdaperm(rep, f);
        if not IsBound(inv_r[j]) then
          # could use lreps[i]*rreps[j]^-1*lreps[i] instead if there was a
          # method for ^-1...
          inv_r[j] := leftact(mat[i][j] ^ -1, lreps[i]);
        fi;
        if not IsBound(inv_l[i]) then
          inv_l[i] := rightact(rreps[j], mat[i][j] ^ -1);
        fi;
      else
        mat[i][j] := 0;
      fi;
    od;
  od;

  if NrIdempotents(d) = NrHClasses(d) then
    rms := ReesMatrixSemigroup(g, mat);
  else
    rms := ReesZeroMatrixSemigroup(g, mat);
  fi;

  iso := function(x)
    local o, m, i, j;
    o := LambdaOrb(d);
    m := LambdaOrbSCCIndex(d);
    i := Position(o, LambdaFunc(Parent(d))(x));

    if i = fail or OrbSCCLookup(o)[i] <> m then
      return fail;
    fi;
    i := Position(OrbSCC(o)[OrbSCCLookup(o)[i]], i);
    if not IsInverseOpClass(d) then
      o := RhoOrb(d);
      m := RhoOrbSCCIndex(d);
    fi;
    j := Position(o, RhoFunc(Parent(d))(x));
    if j = fail or OrbSCCLookup(o)[j] <> m then
      return fail;
    fi;
    j := Position(OrbSCC(o)[OrbSCCLookup(o)[j]], j);
    return Objectify(TypeReesMatrixSemigroupElements(rms),
                     [j, lambdaperm(rep, rep * inv_r[j] * x * inv_l[i]), i,
                      mat]);
  end;

  inv := function(x)
    if x![1] = 0 then
      return fail;
    fi;
    return rightact(rreps[x![1]], x![2]) * lreps[x![3]];
  end;

  hom := MappingByFunction(d, rms, iso, inv);
  SetIsInjective(hom, true);
  SetIsTotal(hom, true);
  return hom;
end);

# same method for ideals

InstallMethod(InversesOfSemigroupElement,
"for an acting semigroup and associative element",
[IsActingSemigroup, IsAssociativeElement],
function(S, x)

  if x in S then
    return InversesOfSemigroupElementNC(S, x);
  fi;

  return fail;
end);

# different method for ideals

InstallMethod(InversesOfSemigroupElementNC,
"for an acting semigroup and associative element",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(s, f)
  local regular, lambda, rank, rhorank, tester, j, o, rhos, opts, grades,
   rho_f, lambdarank, creator, inv, out, k, g, i, name, rho;

  regular := IsRegularSemigroup(s);

  if not (regular or IsRegularSemigroupElementNC(s, f)) then
    return [];
  fi;

  lambda := LambdaFunc(s)(f);
  rank := LambdaRank(s)(LambdaFunc(s)(f));
  rhorank := RhoRank(s);
  tester := IdempotentTester(s);
  j := 0;

  # can't use GradedRhoOrb here since there may be inverses not D-related to f
  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then
    o := RhoOrb(s);
    rhos := EmptyPlist(Length(o));
    for i in [2 .. Length(o)] do
      if rhorank(o[i]) = rank and tester(lambda, o[i]) then
        j := j + 1;
        rhos[j] := o[i];
      fi;
    od;
  else

    opts := rec(treehashsize := SEMIGROUPS_OptionsRec(s).hashlen.M,
                gradingfunc := function(o, x) return rhorank(x); end,
                onlygrades := function(x, y) return x >= rank; end,
                onlygradesdata := fail);

    for name in RecNames(LambdaOrbOpts(s)) do
      opts.(name) := LambdaOrbOpts(s).(name);
    od;

    o := Orb(s, RhoOrbSeed(s), RhoAct(s), opts);
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

  rho_f := RhoFunc(s)(f);
  lambdarank := LambdaRank(s);
  creator := IdempotentCreator(s);
  inv := LambdaInverse(s);

  out := [];
  k := 0;
  
  #if HasLambdaOrb(s) and IsClosed(LambdaOrb(s)) then
  # Notes: it seems that LambdaOrb(S) is always closed at this point
  o := LambdaOrb(s);
  Enumerate(o); # just in case
  for i in [2 .. Length(o)] do
    if lambdarank(o[i]) = rank and tester(o[i], rho_f) then
      for rho in rhos do
        g := creator(lambda, rho) * inv(o[i], f);
        if regular or g in s then
          k := k + 1;
          out[k] := g;
        fi;
      od;
    fi;
  od;
  #else
  #   opts := rec(treehashsize := s!.opts.hashlen.M,
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
  #    if grades[i] = rank and tester(o[i], rho_f) then
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
  local gens, rank, lambda, max, rep, r, e;

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

  if not NrIdempotents(r) = 1 then
    Info(InfoSemigroups, 2, "the number of idempotents in the R-class of the",
         " first maximum rank");
    Info(InfoSemigroups, 2, " generator is not 1");
    return fail;
  fi;

  e := Idempotents(r)[1];

  if ForAll(gens, x -> x * e = x and e * x = x) then
    return e;
  fi;

  Info(InfoSemigroups, 2, "the unique idempotent in the R-class of the first",
       " maximum rank");
  Info(InfoSemigroups, 2, " generator is not the identity");
  return fail;
end);

# same method for inverse/ideals

InstallMethod(RepresentativeOfMinimalIdeal, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local rank, o, pos, min, len, m, i;

  if IsSemigroupIdeal(S)
      and HasRepresentativeOfMinimalIdeal(SupersemigroupOfIdeal(S)) then
    return RepresentativeOfMinimalIdeal(SupersemigroupOfIdeal(S));
  fi;

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
