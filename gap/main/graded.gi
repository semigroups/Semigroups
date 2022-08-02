############################################################################
##
##  main/graded.gi
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Not attempting to get high code coverage here since this file will have to be
# completely rewritten.

# things for graded orbits

InstallMethod(GradedLambdaHT, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local record;
  record := ShallowCopy(LambdaOrbOpts(S));
  record.treehashsize := SEMIGROUPS.OptionsRec(S).hashlen;
  return HTCreate(LambdaFunc(S)(Representative(S)), record);
end);

InstallMethod(GradedRhoHT, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local record;
  record := ShallowCopy(RhoOrbOpts(S));
  record.treehashsize := SEMIGROUPS.OptionsRec(S).hashlen;
  return HTCreate(RhoFunc(S)(Representative(S)), record);
end);

InstallMethod(\in, "for a lambda value and graded lambda orbs",
[IsObject, IsGradedLambdaOrbs],
function(lamf, o)
  return not HTValue(GradedLambdaHT(o!.parent), lamf) = fail;
end);

InstallMethod(\in, "for a rho value and graded rho orbs",
[IsObject, IsGradedRhoOrbs],
function(rho, o)
  return not HTValue(GradedRhoHT(o!.parent), rho) = fail;
end);

InstallMethod(ELM_LIST, "for graded lambda orbs, and pos int",
[IsGradedLambdaOrbs, IsPosInt],
function(o, j)
  return o!.orbits[j];
end);

InstallMethod(ELM_LIST, "for graded rho orbs, and pos int",
[IsGradedRhoOrbs, IsPosInt],
function(o, j)
  return o!.orbits[j];
end);

InstallGlobalFunction(GradedLambdaOrb,
function(arg)
  local S, x, global, obj, lambda, graded, pos, gradingfunc, onlygrades,
  onlygradesdata, orb, gens, o, j, k, l;

  if Length(arg) < 3 then
    ErrorNoReturn("there must be at least 3 arguments");
  fi;

  S := arg[1];
  x := arg[2];
  global := arg[3];

  if Length(arg) > 3 then
    obj := arg[4];
  fi;

  if not IsActingSemigroup(S) then
    ErrorNoReturn("the first argument <S> must be an acting semigroup");
  elif not IsMultiplicativeElement(x) then
    ErrorNoReturn("the second argument <x> must be a multiplicative element");
  elif not IsBool(global) then
    ErrorNoReturn("the third argument <global> must be a boolean");
  fi;

  x := ConvertToInternalElement(S, x);
  lambda := LambdaFunc(S)(x);

  if global then
    graded := GradedLambdaOrbs(S);
    pos := HTValue(GradedLambdaHT(S), lambda);

    if pos <> fail then
      if IsBound(obj) then
        obj!.LambdaPos := pos[3];
      fi;
      return graded[pos[1]][pos[2]];
    fi;

    gradingfunc := function(o, x)
                     return [LambdaRank(S)(x), x];
                   end;

    onlygrades := function(x, data_ht)
                    return x[1] = LambdaRank(S)(lambda)
                     and HTValue(data_ht, x[2]) = fail;
                  end;

    onlygradesdata := GradedLambdaHT(S);

  else  # local
    gradingfunc := function(o, x)
                     return LambdaRank(S)(x);
                   end;
    onlygrades := function(x, data_ht)
                    return x = LambdaRank(S)(lambda);
                  end;
    onlygradesdata := fail;
  fi;

  orb := ShallowCopy(LambdaOrbOpts(S));
  orb.parent := S;
  orb.treehashsize := SEMIGROUPS.OptionsRec(S).hashlen;
  orb.schreier := true;
  orb.orbitgraph := true;
  orb.storenumbers := true;
  orb.log := true;
  orb.onlygrades := onlygrades;
  orb.gradingfunc := gradingfunc;
  orb.scc_reps := [x];
  orb.onlygradesdata := onlygradesdata;

  if IsSemigroupIdeal(S) then
    gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));
  else
    gens := GeneratorsOfSemigroup(S);
  fi;

  gens := List(gens, x -> ConvertToInternalElement(S, x));

  o := Orb(gens, lambda, LambdaAct(S), orb);
  SetFilterObj(o, IsGradedLambdaOrb);

  if global then  # store o
    j := LambdaRank(S)(lambda) + 1;
    # the +1 is essential as the rank can be 0
    k := graded!.lens[j] + 1;
    graded[j][k] := o;
    Enumerate(o);
    for l in [1 .. Length(o)] do
      HTAdd(onlygradesdata, o[l], [j, k, l]);
    od;
    # remove this it is only used in one place in this file TODO(later)
    o!.position_in_graded := [j, k];
    graded!.lens[j] := k;
  fi;
  if IsBound(obj) then
    obj!.LambdaPos := 1;
  fi;
  return o;
end);

InstallGlobalFunction(GradedRhoOrb,
function(arg)
  local S, x, global, obj, rho, graded, pos, gradingfunc, onlygrades,
  onlygradesdata, orb, gens, o, j, k, l;

  if Length(arg) < 3 then
    ErrorNoReturn("there must be at least 3 arguments");
  fi;

  S := arg[1];
  x := arg[2];
  global := arg[3];

  if Length(arg) > 3 then
    obj := arg[4];
  fi;

  if not IsActingSemigroup(S) then
    ErrorNoReturn("the first argument <S> must be an acting semigroup");
  elif not IsMultiplicativeElement(x) then
    ErrorNoReturn("the second argument <f> must be a multiplicative element");
  elif not IsBool(global) then
    ErrorNoReturn("the third argument <opt> must be a boolean");
  fi;

  x := ConvertToInternalElement(S, x);
  rho := RhoFunc(S)(x);

  if global then
    graded := GradedRhoOrbs(S);
    pos := HTValue(GradedRhoHT(S), rho);

    if pos <> fail then
      if IsBound(obj) then
        obj!.RhoPos := pos[3];
      fi;
      return graded[pos[1]][pos[2]];
    fi;

    gradingfunc := {o, x} -> [RhoRank(S)(x), x];

    onlygrades := {x, data_ht} -> x[1]
                  = RhoRank(S)(rho) and HTValue(data_ht, x[2]) = fail;

    onlygradesdata := GradedRhoHT(S);
  else  # local
    gradingfunc := function(o, x)
                     return RhoRank(S)(x);
                   end;
    onlygrades := function(x, data_ht)
                    return x = RhoRank(S)(rho);
                  end;
    onlygradesdata := fail;
  fi;

  orb := ShallowCopy(RhoOrbOpts(S));
  orb.parent := S;
  orb.treehashsize := SEMIGROUPS.OptionsRec(S).hashlen;
  orb.schreier := true;
  orb.orbitgraph := true;
  orb.storenumbers := true;
  orb.log := true;
  orb.onlygrades := onlygrades;
  orb.gradingfunc := gradingfunc;
  orb.scc_reps := [x];
  orb.onlygradesdata := onlygradesdata;

  if IsSemigroupIdeal(S) then
    gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));
  else
    gens := GeneratorsOfSemigroup(S);
  fi;

  gens := List(gens, x -> ConvertToInternalElement(S, x));

  o := Orb(gens, rho, RhoAct(S), orb);
  SetFilterObj(o, IsGradedRhoOrb);

  if global then  # store o
    j := RhoRank(S)(rho) + 1;
    # the +1 is essential as the rank can be 0
    k := graded!.lens[j] + 1;
    graded[j][k] := o;
    Enumerate(o);
    for l in [1 .. Length(o)] do
      HTAdd(onlygradesdata, o[l], [j, k, l]);
    od;
    # store the position of RhoFunc(S)(x) in o
    graded!.lens[j] := k;
  fi;
  if IsBound(obj) then
    obj!.RhoPos := 1;
  fi;
  return o;
end);

# stores so far calculated GradedLambdaOrbs

InstallMethod(GradedLambdaOrbs, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local degree, fam;

  degree := ActionDegree(S) + 1;
  if IsMatrixOverFiniteFieldSemigroup(S) then
    degree := degree + 1;
  fi;
  fam := CollectionsFamily(FamilyObj(LambdaFunc(S)(Representative(S))));
  return Objectify(NewType(fam, IsGradedLambdaOrbs),
                   rec(orbits := List([1 .. degree], x -> []),
                       lens := [1 .. degree] * 0,
                       parent := S));
end);

# stores so far calculated GradedRhoOrbs

InstallMethod(GradedRhoOrbs, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local degree;

  # TODO(later): Why is this function not the direct analogue of
  # GradedLambdaOrbs? Where's fam here?
  degree := ActionDegree(S) + 1;
  if IsMatrixOverFiniteFieldSemigroup(S) then
    degree := degree + 1;
  fi;
  return Objectify(NewType(FamilyObj(S), IsGradedRhoOrbs),
                   rec(orbits := List([1 .. degree], x -> []),
                       lens := [1 .. degree] * 0,
                       parent := S));
end);

InstallMethod(IsBound\[\], "for graded lambda orbs and pos int",
[IsGradedLambdaOrbs, IsPosInt],
function(o, j)
  return IsBound(o!.orbits[j]);
end);

InstallMethod(IsBound\[\], "for graded rho orbs and pos int",
[IsGradedRhoOrbs, IsPosInt],
function(o, j)
  return IsBound(o!.orbits[j]);
end);

InstallMethod(Position, "for graded lambda orbs and lambda value",
[IsGradedLambdaOrbs, IsObject, IsZeroCyc],
function(o, lamf, n)
  return HTValue(GradedLambdaHT(o!.parent), lamf);
end);

InstallMethod(Position, "for graded rho orbs and rho value",
[IsGradedRhoOrbs, IsObject, IsZeroCyc],
function(o, rho, n)
  return HTValue(GradedRhoHT(o!.parent), rho);
end);

InstallMethod(PrintObj, [IsGradedLambdaOrbs],
function(o)
  Print("<graded lambda orbs: ");
  View(o!.orbits);
  Print(" >");
  return;
end);

InstallMethod(PrintObj, [IsGradedRhoOrbs],
function(o)
  Print("<graded rho orbs: ");
  View(o!.orbits);
  Print(" >");
  return;
end);

# Maybe move to iterators...

InstallGlobalFunction(IteratorOfGradedLambdaOrbs,
function(s)
  local record;

  Enumerate(LambdaOrb(s), 2);

  record := rec(seen := [], l := 2);

  record.NextIterator := function(iter)
    local seen, lambda_o, pos, val, o, word;

    seen := iter!.seen;
    lambda_o := LambdaOrb(s);
    pos := LookForInOrb(lambda_o,
                        function(o, x)
                          local val;
                          val := Position(GradedLambdaOrbs(s), x);
                          return val = fail
                            or not IsBound(seen[val[1]])
                            or (IsBound(seen[val[1]]) and not
                                IsBound(seen[val[1]][val[2]]));
                        end,
                        iter!.l);

    if pos = false then
      return fail;
    fi;

    # where to start looking in lambda_o next time
    iter!.l := pos + 1;

    val := Position(GradedLambdaOrbs(s), lambda_o[pos]);
    if val <> fail then  # previously calculated graded orbit
      o := GradedLambdaOrbs(s)[val[1]][val[2]];
    else  # new graded orbit
      word := TraceSchreierTreeForward(lambda_o, pos);
      o := GradedLambdaOrb(s, EvaluateWord(lambda_o, word), true);
      val := o!.position_in_graded;
    fi;

    if not IsBound(seen[val[1]]) then
        seen[val[1]] := [];
    fi;
    seen[val[1]][val[2]] := true;
    return o;
  end;

  record.ShallowCopy := iter -> rec(seen := [], l := 2);

  return IteratorByNextIterator(record);
end);
