############################################################################
##
#W  graded.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# things for graded orbits

InstallMethod(GradedLambdaHT, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local record;

  record := ShallowCopy(LambdaOrbOpts(s));
  record.treehashsize := s!.opts.hashlen.S;
  return HTCreate(LambdaFunc(s)(Representative(s)), record);
end);

#

InstallMethod(GradedRhoHT, "for an acting semigroup", [IsActingSemigroup],
function(s)
  local record;

  record := ShallowCopy(RhoOrbOpts(s));
  record.treehashsize := s!.opts.hashlen.S;
  return HTCreate(RhoFunc(s)(Representative(s)), record);
end);

#

InstallMethod(\in, "for a lambda value and graded lambda orbs",
[IsObject, IsGradedLambdaOrbs],
function(lamf, o)
  return not HTValue(GradedLambdaHT(o!.parent), lamf) = fail;
end);

#

InstallMethod(\in, "for a rho value and graded rho orbs",
[IsObject, IsGradedRhoOrbs],
function(rho, o)
  return not HTValue(GradedRhoHT(o!.parent), rho) = fail;
end);

#

InstallMethod(ELM_LIST, "for graded lambda orbs, and pos int",
[IsGradedLambdaOrbs, IsPosInt],
function(o, j)
  return o!.orbits[j];
end);

#

InstallMethod(ELM_LIST, "for graded rho orbs, and pos int",
[IsGradedRhoOrbs, IsPosInt],
function(o, j)
  return o!.orbits[j];
end);

#

InstallGlobalFunction(GradedLambdaOrb,
function(s, f, opt)
  local lambda, graded, pos, gradingfunc, onlygrades, onlygradesdata, record,
  gens, o, j, k, l;

  if not IsActingSemigroup(s) then
    Error("Semigroups: GradedLambdaOrb: usage,\n",
          "the first argument <S> must be an acting semigroup,");
    return;
  elif not IsAssociativeElement(f) then
    Error("Semigroups: GradedLambdaOrb: usage,\n",
          "the second argument <f> must be an associative element,");
    return;
  elif not IsBool(opt) then
    Error("Semigroups: GradedLambdaOrb: usage,\n",
          "the third argument <opt> must be a boolean,");
    return;
  fi;

  lambda := LambdaFunc(s)(f);

  if opt then   #global
    graded := GradedLambdaOrbs(s);
    pos := HTValue(GradedLambdaHT(s), lambda);

    if pos <> fail then
      return [graded[pos[1]][pos[2]], pos[3]];
    fi;

    gradingfunc := function(o,x)
                     return [LambdaRank(s)(x), x];
                   end;
    onlygrades := function(x, data_ht)
      return x[1] = LambdaRank(s)(lambda)
       and HTValue(data_ht, x[2]) = fail;
    end;
    onlygradesdata := GradedLambdaHT(s);
  else          #local
    gradingfunc := function(o,x)
                     return LambdaRank(s)(x);
                   end;
    onlygrades := function(x,data_ht)
                    return x = LambdaRank(s)(lambda);
                  end;
    onlygradesdata := fail;
  fi;

  record := ShallowCopy(LambdaOrbOpts(s));

  record.parent := s;
  record.treehashsize := s!.opts.hashlen.M;
  record.schreier := true;
  record.orbitgraph := true;
  record.storenumbers := true;
  record.log := true;
  record.onlygrades := onlygrades;
  record.gradingfunc := gradingfunc;
  record.scc_reps := [f];
  record.onlygradesdata := onlygradesdata;

  if IsSemigroupIdeal(s) then
    gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(s));
  else
    gens := GeneratorsOfSemigroup(s);
  fi;

  o := Orb(gens, lambda, LambdaAct(s), record);
  SetFilterObj(o, IsGradedLambdaOrb);

  if opt then # store o
    j := LambdaRank(s)(lambda) + 1;
    # the +1 is essential as the rank can be 0
    k := graded!.lens[j] + 1;
    graded[j][k] := o;
    Enumerate(o);
    for l in [1 .. Length(o)] do
      HTAdd(onlygradesdata, o[l], [j,k,l]);
    od;
    o!.position_in_graded := [j,k];
    graded!.lens[j] := k;
  fi;

  return [o, 1];
end);

#

InstallGlobalFunction(GradedRhoOrb,
function(s, f, opt)
  local rho, graded, pos, gradingfunc, onlygrades, onlygradesdata, record, gens,
  o, j, k, l;

  if not IsActingSemigroup(s) then
    Error("Semigroups: GradedRhoOrb: usage,\n",
          "the first argument <S> must be an acting semigroup,");
    return;
  elif not IsAssociativeElement(f) then
    Error("Semigroups: GradedRhoOrb: usage,\n",
          "the second argument <f> must be an associative element,");
    return;
  elif not IsBool(opt) then
    Error("Semigroups: GradedRhpOrb: usage,\n",
          "the third argument <opt> must be a boolean,");
    return;
  fi;

  rho := RhoFunc(s)(f);

  if opt then   #global
    graded := GradedRhoOrbs(s);
    pos := HTValue(GradedRhoHT(s), rho);

    if pos <> fail then
      return [graded[pos[1]][pos[2]], pos[3]];
    fi;

    gradingfunc := function(o,x)
                     return [RhoRank(s)(x), x];
                   end;

    onlygrades := function(x, data_ht)
                    return x[1] = RhoRank(s)(rho)
                    and HTValue(data_ht, x[2]) = fail;
                  end;

    onlygradesdata := GradedRhoHT(s);
  else          #local
    gradingfunc := function(o,x)
                     return RhoRank(s)(x);
                   end;
                   onlygrades := function(x, data_ht)
                     return x = RhoRank(s)(RhoFunc(s)(f));
                   end;
    onlygradesdata := fail;
  fi;

  record := ShallowCopy(RhoOrbOpts(s));

  record.parent := s;
  record.treehashsize := s!.opts.hashlen.M;
  record.schreier := true;
  record.orbitgraph := true;
  record.storenumbers := true;
  record.log := true;
  record.onlygrades := onlygrades;
  record.gradingfunc := gradingfunc;
  record.scc_reps := [f];
  record.onlygradesdata := onlygradesdata;

  if IsSemigroupIdeal(s) then
    gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(s));
  else
    gens := GeneratorsOfSemigroup(s);
  fi;

  o := Orb(gens, rho, RhoAct(s), record);
  SetFilterObj(o, IsGradedRhoOrb);

  if opt then # store o
    j := RhoRank(s)(RhoFunc(s)(f)) + 1;
    # the +1 is essential as the rank can be 0
    k := graded!.lens[j] + 1;
    graded[j][k] := o;
    Enumerate(o);
    for l in [1 .. Length(o)] do
      HTAdd(onlygradesdata, o[l], [j,k,l]);
    od;

    # store the position of RhoFunc(s)(f) in o
    graded!.lens[j] := k;
  fi;

  return [o, 1];
end);

# stores so far calculated GradedLambdaOrbs

InstallMethod(GradedLambdaOrbs, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local fam;

  fam := CollectionsFamily(FamilyObj(LambdaFunc(s)(Representative(s))));
  return Objectify(NewType(fam, IsGradedLambdaOrbs),
   rec( orbits := List([1 .. ActionDegree(s) + 1], x -> []),
     lens := [1 .. ActionDegree(s) + 1] * 0, parent := s));
end);

# stores so far calculated GradedRhoOrbs

InstallMethod(GradedRhoOrbs, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  return Objectify(NewType(FamilyObj(s), IsGradedRhoOrbs), rec(
    orbits := List([1 .. ActionDegree(s) + 1], x -> []),
    lens := [1 .. ActionDegree(s) + 1] * 0, parent := s));
end);

#

InstallMethod(IsBound\[\], "for graded lambda orbs and pos int",
[IsGradedLambdaOrbs, IsPosInt],
function(o, j)
  return IsBound(o!.orbits[j]);
end);

#

InstallMethod(IsBound\[\], "for graded rho orbs and pos int",
[IsGradedRhoOrbs, IsPosInt],
function(o, j)
  return IsBound(o!.orbits[j]);
end);

#

InstallMethod(Position, "for graded lambda orbs and lambda value",
[IsGradedLambdaOrbs, IsObject, IsZeroCyc],
function(o, lamf, n)
  return HTValue(GradedLambdaHT(o!.parent), lamf);
end);

#

InstallMethod(Position, "for graded rho orbs and rho value",
[IsGradedRhoOrbs, IsObject, IsZeroCyc],
function(o, rho, n)
  return HTValue(GradedRhoHT(o!.parent), rho);
end);

#

InstallMethod(PrintObj, [IsGradedLambdaOrbs],
function(o)
  Print("<graded lambda orbs: ");
  View(o!.orbits);
  Print(" >");
  return;
end);

#

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
    local seen, pos, val, o, lambda_o;

    seen := iter!.seen;
    lambda_o := LambdaOrb(s);
    pos := LookForInOrb(lambda_o,
      function(o, x)
        local val;
        val := Position(GradedLambdaOrbs(s), x);
        return val = fail
          or not IsBound(seen[val[1]])
          or (IsBound(seen[val[1]]) and not IsBound(seen[val[1]][val[2]]));
      end, iter!.l);

    if pos = false then
      return fail;
    fi;

    #where to start looking in lambda_o next time
    iter!.l := pos + 1;

    val := Position(GradedLambdaOrbs(s), lambda_o[pos]);
    if val <> fail then # previously calculated graded orbit
      o := GradedLambdaOrbs(s)[val[1]][val[2]];
    else # new graded orbit
      o := GradedLambdaOrb(s, EvaluateWord(lambda_o,
        TraceSchreierTreeForward(lambda_o, pos)), true)[1];
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

#EOF
