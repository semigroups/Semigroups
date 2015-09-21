#############################################################################
##
#W  semigroups-acting.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

## Methods for some standard things for acting semigroups.

# different method for inverse/regular, same for ideals

InstallMethod(Random, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local data, gens, i, w, x, n, m, o, rep, g;

  data := SemigroupData(s);

  if not IsClosedData(data) then
    if HasGeneratorsOfSemigroup(s) then
      gens := GeneratorsOfSemigroup(s);
      i := Random([1 .. 2 * Length(gens)]);
      w := List([1 .. i], x -> Random([1 .. Length(gens)]));
      return EvaluateWord(gens, w);
    elif IsSemigroupIdeal(s) and HasGeneratorsOfSemigroupIdeal(s) then
      x := Random([1 .. Length(GeneratorsOfSemigroupIdeal(s))]);
      gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(s));

      i := Random([1 .. Length(gens)]);
      w := List([1 .. i], x -> Random([1 .. Length(gens)]));

      x := x * EvaluateWord(gens, w);

      i := Random([1 .. Length(gens)]);
      w := List([1 .. i], x -> Random([1 .. Length(gens)]));
      return EvaluateWord(gens, w) * x;
    fi;
  fi;

  n := Random([2 .. Length(data!.orbit)]);
  m := data[n][2];
  o := data[n][3];
  rep := data[n][4];

  g := Random(LambdaOrbSchutzGp(o, m));
  i := Random(OrbSCC(o)[m]);
  return StabilizerAction(s)(rep, g) * LambdaOrbMult(o, m, i)[1];
end);

# different method for inverse, same method for ideals

InstallMethod(Random, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(S)
  local gens, i, w, x, o, m;

  if not IsClosed(LambdaOrb(S)) or not IsClosed(RhoOrb(S)) then
    if HasGeneratorsOfSemigroup(S) then
      gens := GeneratorsOfSemigroup(S);
      i := Random([1 .. 2 * Int(Length(gens))]);
      w := List([1 .. i], x -> Random([1 .. Length(gens)]));
      return EvaluateWord(gens, w);
    else
      x := Random(GeneratorsOfSemigroupIdeal(S));
      gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));

      i := Random([1 .. Length(gens)]);
      w := List([1 .. i], x -> Random([1 .. Length(gens)]));

      x := x * EvaluateWord(gens, w);

      i := Random([1 .. Length(gens)]);
      w := List([1 .. i], x -> Random([1 .. Length(gens)]));
      return EvaluateWord(gens, w) * x;
    fi;
  fi;

  o := LambdaOrb(S);
  i := Random([2 .. Length(o)]);
  m := OrbSCCLookup(o)[i];
  x := LambdaOrbRep(o, m) * Random(LambdaOrbSchutzGp(o, m))
   * LambdaOrbMult(o, m, i)[1];

  o := RhoOrb(S);
  m := OrbSCCLookup(o)[Position(o, RhoFunc(S)(x))];
  i := Random(OrbSCC(o)[m]);

  return RhoOrbMult(o, m, i)[1] * x;
end);

# same method for inverse ideals

InstallMethod(Random, "for an acting semigroup with inverse op and generators",
[IsActingSemigroupWithInverseOp],
function(S)
  local gens, i, w, x, o, m;

  if not IsClosed(LambdaOrb(S)) then
    if HasGeneratorsOfSemigroup(S) then
      gens := GeneratorsOfSemigroup(S);
      i := Random([1 .. 2 * Int(Length(gens))]);
      w := List([1 .. i], x -> Random([1 .. Length(gens)]));
      return EvaluateWord(gens, w);
    else
      x := Random(GeneratorsOfSemigroupIdeal(S));
      gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));

      i := Random([1 .. Length(gens) / 2]);
      w := List([1 .. i], x -> Random([1 .. Length(gens)]));

      x := x * EvaluateWord(gens, w);

      i := Random([1 .. Length(gens) / 2]);
      w := List([1 .. i], x -> Random([1 .. Length(gens)]));
      return EvaluateWord(gens, w) * x;
    fi;
  fi;

  o := LambdaOrb(S);
  i := Random([2 .. Length(o)]);
  m := OrbSCCLookup(o)[i];
  x := LambdaOrbRep(o, m) * Random(LambdaOrbSchutzGp(o, m))
   * LambdaOrbMult(o, m, i)[1];

  i := Random(OrbSCC(o)[m]);

  return LambdaOrbMult(o, m, i)[2] * x;
end);

#############################################################################

# different method for inverse, same method for ideals

InstallMethod(\in, "for an associative element and regular acting semigroup",
[IsAssociativeElement, IsActingSemigroup and IsRegularSemigroup],
function(x, S)
  local pos_lambda, pos_rho, m, schutz, n, rep;

  if ElementsFamily(FamilyObj(S)) <> FamilyObj(x)
      or (IsActingSemigroupWithFixedDegreeMultiplication(S)
          and ActionDegree(x) <> ActionDegree(S))
      or ActionDegree(x) > ActionDegree(S) then
    return false;
  fi;

  if not (IsMonoid(S) and IsOne(x)) then
    if Length(Generators(S)) > 0
        and ActionRank(S)(x) >
        MaximumList(List(Generators(S), y -> ActionRank(S)(y))) then
      Info(InfoSemigroups, 2, "element has larger rank than any element of ",
           "semigroup");
      return false;
    fi;
  fi;

  if HasMinimalIdeal(S) then
    if ActionRank(S)(x) < ActionRank(S)(Representative(MinimalIdeal(S))) then
      Info(InfoSemigroups, 2, "element has smaller rank than any element of ",
           "semigroup.");
      return false;
    fi;
  fi;

  if HasAsSSortedList(S) then
    return x in AsSSortedList(S);
  fi;

  pos_lambda := Position(Enumerate(LambdaOrb(S)), LambdaFunc(S)(x));

  if pos_lambda = fail then
    return false;
  fi;

  pos_rho := EnumeratePosition(RhoOrb(S), RhoFunc(S)(x), false);
  # this is worth it in the case that schutz=true below! For example, in the
  # full transformation monoid on 12 points (see Issue 22 in testinstall.tst)

  if pos_rho = fail then
    return false;
  fi;

  m := OrbSCCLookup(LambdaOrb(S))[pos_lambda];
  schutz := LambdaOrbStabChain(LambdaOrb(S), m);

  if schutz = true then
    return true;
  elif pos_lambda <> OrbSCC(LambdaOrb(S))[m][1] then
    x := x * LambdaOrbMult(LambdaOrb(S), m, pos_lambda)[2];
  fi;

  n := OrbSCCLookup(Enumerate(RhoOrb(S)))[pos_rho];

  if pos_rho <> OrbSCC(RhoOrb(S))[n][1] then
    x := RhoOrbMult(RhoOrb(S), n, pos_rho)[2] * x;
  fi;

  if IsIdempotent(x) then
    return true;
  fi;

  rep := LambdaOrbRep(LambdaOrb(S), m);
  pos_rho := Position(RhoOrb(S), RhoFunc(S)(rep));

  if OrbSCCLookup(RhoOrb(S))[pos_rho] <> n then
    return false;
  elif pos_rho <> OrbSCC(RhoOrb(S))[n][1] then
    rep := RhoOrbMult(RhoOrb(S), n, pos_rho)[2] * rep;
  fi;

  if rep = x then
    return true;
  elif schutz = false then
    return false;
  fi;

  return SchutzGpMembership(S)(schutz, LambdaPerm(S)(rep, x));
end);

# same method for inverse ideals
# TODO clean this up

InstallMethod(\in,
"for an associative element and acting semigroup with inversion",
[IsAssociativeElement, IsActingSemigroupWithInverseOp],
function(f, s)
  local o, lambda, lambda_l, rho, rho_l, m, schutz, scc, rep;

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f)
      or (IsActingSemigroupWithFixedDegreeMultiplication(s)
          and ActionDegree(f) <> ActionDegree(s))
      or ActionDegree(f) > ActionDegree(s) then
    return false;
  fi;

  if HasAsSSortedList(s) then
    return f in AsSSortedList(s);
  fi;

  if not (IsMonoid(s) and IsOne(f)) then
    if Length(Generators(s)) > 0
        and ActionRank(s)(f) >
        MaximumList(List(Generators(s), f -> ActionRank(s)(f))) then
      Info(InfoSemigroups, 2, "element has larger rank than any element of ",
           "semigroup.");
      return false;
    fi;
  fi;

  if HasMinimalIdeal(s) then
    if ActionRank(s)(f) < ActionRank(s)(Representative(MinimalIdeal(s))) then
      Info(InfoSemigroups, 2, "element has smaller rank than any element of ",
           "semigroup.");
      return false;
    fi;
  fi;

  o := LambdaOrb(s);
  Enumerate(o);
  lambda := LambdaFunc(s)(f);
  lambda_l := Position(o, lambda);

  if lambda_l = fail then
    return false;
  fi;

  rho := RhoFunc(s)(f);
  rho_l := Position(o, rho);

  if rho_l = fail then
    return false;
  fi;

  # must use LambdaOrb(s) and not a graded lambda orb as LambdaOrbRep(o, m)
  # when o is graded, is just f and hence \in will always return true!!
  m := OrbSCCLookup(o)[lambda_l];

  if OrbSCCLookup(o)[rho_l] <> m then
    return false;
  fi;

  schutz := LambdaOrbStabChain(o, m);

  if schutz = true then
    return true;
  fi;

  scc := OrbSCC(o)[m];

  if lambda_l <> scc[1] then
    f := f * LambdaOrbMult(o, m, lambda_l)[2];
  fi;

  if rho_l <> scc[1] then
    f := LambdaOrbMult(o, m, rho_l)[1] * f;
  fi;

  if IsIdempotent(f) then
    return true;
  elif schutz = false then
    return false;
  fi;

  rep := LambdaOrbRep(LambdaOrb(s), m);
  rho_l := Position(LambdaOrb(s), RhoFunc(s)(rep));

  if rho_l <> OrbSCC(LambdaOrb(s))[m][1] then
    # the D-class rep corresponding to lambda_o and scc.
    rep := LambdaOrbMult(LambdaOrb(s), m, rho_l)[1] * rep;
  fi;
  return SchutzGpMembership(s)(schutz, LambdaPerm(s)(rep, f));
end);

#############################################################################

# different method for inverse semigroups

InstallMethod(Size, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(s)
  local lambda_o, rho_o, nr, lambda_scc, rho_scc, r, rhofunc, lookup,
  rho, m;

  lambda_o := Enumerate(LambdaOrb(s), infinity);
  rho_o := Enumerate(RhoOrb(s), infinity);

  nr := 0;
  lambda_scc := OrbSCC(lambda_o);
  rho_scc := OrbSCC(rho_o);
  r := Length(lambda_scc);
  rhofunc := RhoFunc(s);
  lookup := OrbSCCLookup(rho_o);

  for m in [2 .. r] do
    rho := rhofunc(LambdaOrbRep(lambda_o, m));
    nr := nr + Length(lambda_scc[m]) * Size(LambdaOrbSchutzGp(lambda_o, m)) *
     Length(rho_scc[lookup[Position(rho_o, rho)]]);
  od;

  return nr;
end);

# same method for inverse ideals

InstallMethod(Size, "for an acting semigroup with inversion",
[IsActingSemigroupWithInverseOp], 10,
function(s)
  local o, scc, r, nr, m;

  o := LambdaOrb(s);
  Enumerate(o, infinity);
  scc := OrbSCC(o);
  r := Length(scc);
  nr := 0;

  for m in [2 .. r] do
    nr := nr + Length(scc[m]) ^ 2 * Size(LambdaOrbSchutzGp(o, m));
  od;
  return nr;
end);
