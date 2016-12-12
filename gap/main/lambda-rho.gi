############################################################################
##
#W  lambda-rho.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(LambdaOrb, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local record, o;

  record := ShallowCopy(LambdaOrbOpts(S));
  record.scc_reps := [FakeOne(GeneratorsOfSemigroup(S))];

  record.schreier     := true;
  record.orbitgraph   := true;
  record.storenumbers := true;
  record.log          := true;
  record.parent       := S;
  record.treehashsize := SEMIGROUPS.OptionsRec(S).hashlen.M;

  o := Orb(GeneratorsOfSemigroup(S), LambdaOrbSeed(S), LambdaAct(S), record);

  SetFilterObj(o, IsLambdaOrb);

  if IsInverseActingSemigroupRep(S) then
    SetFilterObj(o, IsInverseOrb);
  fi;

  return o;
end);

InstallGlobalFunction(LambdaOrbMults,
function(o, m)
  local scc, gens, one, mults, genpos, inv, trace, i;

  scc := OrbSCC(o);

  if IsBound(o!.hasmults) then
    if IsBound(o!.hasmults[m]) and o!.hasmults[m] then
      return o!.mults;
    fi;
  else
    if not IsBound(o!.mults) then
      o!.mults := EmptyPlist(Length(o));
    fi;
    o!.hasmults := BlistList([1 .. Length(scc)], []);
  fi;

  o!.hasmults[m] := true;
  scc := OrbSCC(o)[m];
  gens := o!.gens;
  one := FakeOne(gens);
  mults := o!.mults;

  # it would be better to use the SchreierTree here not the ReverseSchreierTree
  # and shouldn't there be a second case of inverse orbits here??
  genpos := ReverseSchreierTreeOfSCC(o, m);
  inv := function(lambda, x)
           return LambdaInverse(o!.parent)(lambda, x);
         end;

  trace := function(i)
    local x;
    if IsBound(mults[i]) then
      return mults[i][2];
    elif i = scc[1] then
      mults[i] := [one, one];
      return one;
    fi;
    x := gens[genpos[1][i]] * trace(genpos[2][i]);
    mults[i] := [inv(o[i], x), x];
    return x;
  end;

  for i in scc do
    trace(i);
  od;
  return o!.mults;
end);

# f takes o[i] to o[scc[1]] and inv(o[i], f) takes o[scc[1]] to o[i]
# JDM: this should be the other way around like in RhoOrbMult, and using
# SchreierTreeOfSCC instead of its reverse.

InstallGlobalFunction(LambdaOrbMult,
function(o, m, i)
  local scc, mults, gens, one, genpos, inv, trace;

  if IsBound(o!.mults) then
    if IsBound(o!.mults[i]) then
      return o!.mults[i];
    fi;
  else
    o!.mults := EmptyPlist(Length(o));
  fi;

  scc := OrbSCC(o)[m];
  gens := o!.gens;
  one := FakeOne(gens);
  mults := o!.mults;

  if not IsInverseActingSemigroupRep(o!.parent) then
    #FIXME it would be better to use the SchreierTree here not the
    #ReverseSchreierTree
    genpos := ReverseSchreierTreeOfSCC(o, m);
    inv := function(lambda, x)
             return LambdaInverse(o!.parent)(lambda, x);
           end;

    trace := function(i)
      local x;
      if IsBound(mults[i]) then
        return mults[i][2];
      elif i = scc[1] then
        mults[i] := [one, one];
        return one;
      fi;
      x := gens[genpos[1][i]] * trace(genpos[2][i]);
      mults[i] := [inv(o[i], x), x];
      return x;
    end;
  else
    genpos := SchreierTreeOfSCC(o, m);

    trace := function(i)
      local x;
      if IsBound(mults[i]) then
        return mults[i][2];
      elif i = scc[1] then
        mults[i] := [one, one];
        return one;
      fi;
      x := INV(gens[genpos[1][i]]) * trace(genpos[2][i]);
      mults[i] := [INV(x), x];
      return x;
    end;
  fi;

  trace(i);
  return o!.mults[i];
end);

# JDM this is really slow (due to EvaluateWord) for large degree

InstallGlobalFunction(LambdaOrbRep,
function(o, m)
  local w;

  if IsBound(o!.scc_reps[m]) then
    return o!.scc_reps[m];
  fi;

  w := TraceSchreierTreeForward(o, OrbSCC(o)[m][1]);
  o!.scc_reps[m] := EvaluateWord(o, w);

  if not IsIdealOrb(o) then
    o!.scc_reps[m] := o!.scc_reps[1] * o!.scc_reps[m];
  fi;

  return o!.scc_reps[m];
end);

InstallGlobalFunction(RhoOrbRep,
function(o, m)
  local w;

  if IsBound(o!.scc_reps[m]) then
    return o!.scc_reps[m];
  fi;

  w := TraceSchreierTreeForward(o, OrbSCC(o)[m][1]);
  o!.scc_reps[m] := EvaluateWord(o, Reversed(w));

  if not IsIdealOrb(o) then
    o!.scc_reps[m] := o!.scc_reps[m] * o!.scc_reps[1];
  fi;

  return o!.scc_reps[m];
end);

InstallGlobalFunction(LambdaOrbSchutzGp,
function(o, m)
  local s, gens, scc, lookup, orbitgraph, genstoapply, lambdaperm, rep, rank,
  one, bound, g, stop, forward, f, k, l;

  if IsBound(o!.schutz) then
    if IsBound(o!.schutz[m]) then
      return o!.schutz[m];
    fi;
  else
    o!.schutz := EmptyPlist(Length(OrbSCC(o)));
    o!.schutzstab := EmptyPlist(Length(OrbSCC(o)));
  fi;

  s := o!.parent;
  gens := o!.gens;
  scc := OrbSCC(o)[m];
  lookup := o!.scc_lookup;
  orbitgraph := OrbitGraph(o);
  genstoapply := [1 .. Length(gens)];
  lambdaperm := LambdaPerm(s);
  rep := LambdaOrbRep(o, m);
  rank := LambdaRank(s)(o[scc[1]]);
  one := LambdaIdentity(s)(rank);
  bound := LambdaBound(s)(rank);

  if rank = 0 then
    # The group is the symmetric group
    # if rank = 0
    # mpf: Is this the correct fix? if we put false
    #      here, PartialOrderOfDClasses fails because
    #      mults are not defined
    o!.schutzstab[m] := true;
    o!.schutz[m] := Group(one);
    return o!.schutz[m];
  fi;

  g := Group(one);

  stop := false;

  for k in scc do
    forward := LambdaOrbMult(o, m, k)[1];
    for l in genstoapply do
      if IsBound(orbitgraph[k][l]) and lookup[orbitgraph[k][l]] = m then
        f := lambdaperm(rep, rep * forward * gens[l]
                             * LambdaOrbMult(o, m, orbitgraph[k][l])[2]);
        g := ClosureGroup(g, f);
      fi;

      if Size(g) >= bound then
        stop := true;
        break;
      fi;
    od;
    if stop then
      break;
    fi;
  od;

  o!.schutz[m] := g;

  if stop then
    o!.schutzstab[m] := true;
  elif Size(g) = 1 then
    o!.schutzstab[m] := false;
  elif IsPermGroup(g) then
    o!.schutzstab[m] := StabChainImmutable(g);
  else # if IsMatrixGroup(g)
    o!.schutzstab[m] := g;
  fi;

  return g;
end);

InstallMethod(RhoOrbStabChain, "for a rho orb and scc index",
[IsOrbit, IsPosInt],
function(o, m)

  if IsBound(o!.schutzstab) then
    if IsBound(o!.schutzstab[m]) then
      return o!.schutzstab[m];
    fi;
  fi;

  RhoOrbSchutzGp(o, m);
  return o!.schutzstab[m];
end);

InstallGlobalFunction(LambdaOrbStabChain,
function(o, m)

  if IsBound(o!.schutzstab) then
    if IsBound(o!.schutzstab[m]) then
      return o!.schutzstab[m];
    fi;
  fi;

  LambdaOrbSchutzGp(o, m);
  return o!.schutzstab[m];
end);

InstallMethod(RhoOrb, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local record, o;

  record := ShallowCopy(RhoOrbOpts(s));
  record.schreier := true;
  record.orbitgraph := true;
  record.storenumbers := true;
  record.log := true;
  record.parent := s;
  record.treehashsize := SEMIGROUPS.OptionsRec(s).hashlen.M;
  record.scc_reps := [FakeOne(GeneratorsOfSemigroup(s))];

  o := Orb(GeneratorsOfSemigroup(s), RhoOrbSeed(s), RhoAct(s), record);

  SetFilterObj(o, IsRhoOrb);
  if IsInverseActingSemigroupRep(s) then
    SetFilterObj(o, IsInverseOrb);
  fi;
  return o;
end);

# f takes o[scc[1]] to o[i] and inv(o[scc[1]],f) takes o[i] to o[scc[1]]

InstallGlobalFunction(RhoOrbMult,
function(o, m, i)
  local scc, gens, one, mults, genpos, inv, trace;

  if IsBound(o!.mults) then
    if IsBound(o!.mults[i]) then
      return o!.mults[i];
    fi;
  else
    o!.mults := EmptyPlist(Length(o));
  fi;

  scc := OrbSCC(o)[m];
  gens := o!.gens;
  one := FakeOne(gens);

  mults := o!.mults;

  genpos := SchreierTreeOfSCC(o, m);
  inv := x -> RhoInverse(o!.parent)(o[scc[1]], x);

  trace := function(i)
    local x;
    if IsBound(mults[i]) then
      return mults[i][1];
    elif i = scc[1] then
      mults[i] := [one, one];
      return one;
    fi;
    x := gens[genpos[1][i]] * trace(genpos[2][i]);
    mults[i] := [x, inv(x)];
    return x;
  end;

  trace(i);
  return o!.mults[i];
end);

# f takes o[scc[1]] to o[i] and inv(o[i], f) takes o[i] to o[scc[1]]

InstallGlobalFunction(RhoOrbMults,
function(o, m)
  local scc, gens, one, mults, genpos, inv, trace, i;

  scc := OrbSCC(o);
  if IsBound(o!.hasmults) then
    if IsBound(o!.hasmults[m]) and o!.hasmults[m] then
      return o!.mults;
    fi;
  else
    if not IsBound(o!.mults) then
      o!.mults := EmptyPlist(Length(o));
    fi;
    o!.hasmults := BlistList([1 .. Length(scc)], []);
  fi;

  o!.hasmults[m] := true;
  scc := OrbSCC(o)[m];
  gens := o!.gens;
  one := FakeOne(gens);
  mults := o!.mults;

  genpos := SchreierTreeOfSCC(o, m);
  inv := x -> RhoInverse(o!.parent)(o[scc[1]], x);

  trace := function(i)
    local x;
    if IsBound(mults[i]) then
      return mults[i][1];
    elif i = scc[1] then
      mults[i] := [one, one];
      return one;
    fi;
    x := gens[genpos[1][i]] * trace(genpos[2][i]);
    mults[i] := [x, inv(x)];
    return x;
  end;

  for i in scc do
    trace(i);
  od;
  return o!.mults;
end);

InstallGlobalFunction(RhoOrbSchutzGp,
function(o, m)
  local S, rank, bound, one, G, gens, nrgens, scc, lookup, orbitgraph,
   lambdaperm, rep, mults, stop, i, j;

  if IsBound(o!.schutz) then
    if IsBound(o!.schutz[m]) then
      return o!.schutz[m];
    fi;
  else
    o!.schutz := EmptyPlist(Length(OrbSCC(o)));
    o!.schutzstab := EmptyPlist(Length(OrbSCC(o)));
  fi;

  S := o!.parent;
  scc := OrbSCC(o)[m];
  rank := RhoRank(S)(o[scc[1]]);
  bound := RhoBound(S)(rank);
  one := RhoIdentity(S)(rank);
  G := Group(one);

  if bound = 1 or rank = 0 then
    o!.schutz[m] := G;
    o!.schutzstab[m] := false;
    return G;
  fi;

  gens := o!.gens;
  nrgens := Length(gens);
  lookup := o!.scc_lookup;
  orbitgraph := OrbitGraph(o);
  lambdaperm := LambdaPerm(S);
  rep := RhoOrbRep(o, m);
  mults := RhoOrbMults(o, m);

  stop := false;

  for i in scc do
    for j in [1 .. nrgens] do
      if IsBound(orbitgraph[i][j]) and lookup[orbitgraph[i][j]] = m then
        G := ClosureGroup(G, lambdaperm(rep,
                                        mults[orbitgraph[i][j]][2] * gens[j] *
                                        mults[i][1] * rep));
        if Size(G) >= bound then
          stop := true;
          break;
        fi;
      fi;
    od;
    if stop then
        break;
    fi;
  od;
  o!.schutz[m] := G;

  if stop then
    o!.schutzstab[m] := true;
  elif Size(G) = 1 then
    o!.schutzstab[m] := false;
  elif IsPermGroup(G) then
    o!.schutzstab[m] := StabChainImmutable(G);
  else # if IsMatrixGroup(g)
    o!.schutzstab[m] := G;
  fi;
  return G;
end);
