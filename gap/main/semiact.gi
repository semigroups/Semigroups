#############################################################################
##
##  main/semiact.gi
##  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# The code coverage for this file is not as good as it could be.

SEMIGROUPS.ChangeDegreeOfTransformationSemigroup := function(o, old_deg, t)
  local deg, extra, ht, max, i, orb;
  deg := DegreeOfTransformationSemigroup(t);
  orb := o!.orbit;
  if IsLambdaOrb(o) then
    # rehash the orbit values
    extra := [old_deg + 1 .. deg];
    ht := HTCreate(o[1], rec(treehashsize := o!.treehashsize));
    # JDM: could make the treehashsize bigger if needed here!
    HTAdd(ht, o[1], 1);
    for i in [2 .. Length(o)] do
      orb[i] := ShallowCopy(o[i]);
      Append(o[i], extra);
      HTAdd(ht, o[i], i);
    od;
    Unbind(o!.ht);
    o!.ht := ht;

    # change the action of <o> to that of <t>
    o!.op := LambdaAct(t);
  elif IsRhoOrb(o) then
    ht := HTCreate(o[1], rec(treehashsize := o!.treehashsize));
    # JDM: could make the treehashsize bigger if needed here!
    HTAdd(ht, o[1], 1);
    for i in [2 .. Length(o)] do
      orb[i] := ShallowCopy(o[i]);
      if not IsEmpty(o[i]) then
        max := MaximumList(o[i]);  # nr kernel classes
      else
        max := 0;
      fi;
      Append(o[i], [max + 1 .. max + deg - old_deg]);
      HTAdd(ht, o[i], i);
    od;
    Unbind(o!.ht);
    o!.ht := ht;

    # change the action of <o> to that of <t>
    o!.op := RhoAct(t);
  fi;
  return o;
end;

## Methods for some standard things for acting semigroups.

InstallMethod(ClosureSemigroupOrMonoidNC,
"for a function, acting semigroup, finite list of mult. elts, and record",
[IsFunction,
 IsActingSemigroup,
 IsMultiplicativeElementCollection and IsList and IsFinite,
 IsRecord],
function(Constructor, S, coll, opts)
  local t, old_o, o, rho_o, old_deg, scc, old_scc, lookup, old_lookup, rho_ht,
  new_data, old_data, max_rank, ht, new_orb, old_orb, new_nr, old_nr, graph,
  old_graph, reps, lambdarhoht, rholookup, repslookup, orblookup1, orblookup2,
  repslens, lenreps, new_schreierpos, old_schreierpos, new_schreiergen,
  old_schreiergen, new_schreiermult, old_schreiermult, gens, nr_new_gens,
  nr_old_gens, lambdaperm, rho, old_to_new, htadd, htvalue, i, x, pos, m, rank,
  rhox, l, ind, pt, schutz, data_val, old, n, j;

  # opts must be copied and processed before calling this function
  # coll must be copied before calling this function

  # TODO(later) split this into two methods, one for collections and the other
  # for single elements
  if Size(coll) > 1 then
    coll := Shuffle(Set(coll));
    n := ActionDegree(coll);
    Sort(coll, {x, y} -> ActionRank(x, n) > ActionRank(y, n));
    for x in coll do
      S := ClosureSemigroupOrMonoidNC(Constructor, S, [x], opts);
    od;
    return S;
  elif coll[1] in S then
    return S;
  fi;
  # Size(coll) = 1 . . .

  # init the semigroup or monoid
  t := Constructor(S, coll, opts);

  # if nothing is known about s, then return t
  if not HasLambdaOrb(S) or IsSemigroupIdeal(S) then
    return t;
  fi;

  # set up lambda orb for t
  old_o := LambdaOrb(S);
  o     := StructuralCopy(old_o);
  rho_o := StructuralCopy(RhoOrb(S));

  if IsTransformationSemigroup(S) then
    old_deg := DegreeOfTransformationSemigroup(S);
    if old_deg < DegreeOfTransformationSemigroup(t) then
      SEMIGROUPS.ChangeDegreeOfTransformationSemigroup(o, old_deg, t);
      SEMIGROUPS.ChangeDegreeOfTransformationSemigroup(rho_o, old_deg, t);
    fi;
  fi;

  coll[1] := ConvertToInternalElement(S, coll[1]);

  AddGeneratorsToOrbit(o, coll);

  # unbind everything related to strongly connected components, since
  # even if the orbit length doesn't change the strongly connected components
  # might
  Unbind(o!.scc);
  Unbind(o!.trees);
  Unbind(o!.scc_lookup);
  Unbind(o!.mults);
  Unbind(o!.schutz);
  Unbind(o!.reverse);
  Unbind(o!.rev);
  Unbind(o!.schutzstab);
  Unbind(o!.factorgroups);
  Unbind(o!.factors);

  o!.parent := t;
  o!.scc_reps := [FakeOne(GeneratorsOfSemigroup(t))];

  SetLambdaOrb(t, o);

  if not HasSemigroupData(S) or SemigroupData(S)!.pos = 0 then
    return t;
  fi;

  scc := OrbSCC(o);
  old_scc := OrbSCC(old_o);
  lookup := o!.scc_lookup;

  old_lookup := old_o!.scc_lookup;

  # we don't do AddGeneratorsToOrbit of rho_o here because this is handled by
  # Enumerate(SemigroupData.. later
  rho_ht := rho_o!.ht;

  # unbind everything related to strongly connected components, since
  # even if the orbit length doesn't change the strongly connected components
  # might
  Unbind(rho_o!.scc);
  Unbind(rho_o!.trees);
  Unbind(rho_o!.scc_lookup);
  Unbind(rho_o!.mults);
  Unbind(rho_o!.schutz);
  Unbind(rho_o!.reverse);
  Unbind(rho_o!.rev);
  Unbind(rho_o!.schutzstab);

  rho_o!.parent := t;
  rho_o!.scc_reps := [FakeOne(GeneratorsOfSemigroup(t))];
  Append(rho_o!.gens, coll);
  ResetFilterObj(rho_o, IsClosedOrbit);
  SetRhoOrb(t, rho_o);

  # get new and old R-rep orbit data
  new_data := SemigroupData(t);
  old_data := SemigroupData(S);
  max_rank := MaximumList(List(coll, x -> ActionRank(t)(x)));

  Assert(1, rho_o!.gens = new_data!.gens);
  Assert(1, o!.gens = new_data!.gens);

  ht := new_data!.ht;
  # so far found R-reps

  new_orb := new_data!.orbit;
  old_orb := old_data!.orbit;
  # the so far found R-reps data

  new_nr := Length(new_orb);
  old_nr := Length(old_orb);
  # points in orb in position at most i have descendants

  graph := new_data!.graph;
  old_graph := old_data!.graph;
  graph[1] := ShallowCopy(old_graph[1]);
  # orbit graph of orbit of R-classes under left mult

  reps := new_data!.reps;
  # reps grouped by equal lambda and rho value
  # HTValue(lambdarhoht, Concatenation(lambda(x), rho(x)))

  lambdarhoht := new_data!.lambdarhoht;
  rholookup := new_data!.rholookup;

  repslookup := new_data!.repslookup;
  # Position(orb, reps[i][j])=repslookup[i][j] = HTValue(ht, reps[i][j])

  orblookup1 := new_data!.orblookup1;
  # orblookup1[i] position in reps containing orb[i][4] (the R-rep)

  orblookup2 := new_data!.orblookup2;
  # orblookup2[i] position in reps[orblookup1[i]]
  # containing orb[i][4] (the R-rep)

  repslens := new_data!.repslens;
  # Length(reps[i])=repslens[i]

  lenreps := new_data!.lenreps;
  # lenreps=Length(reps)

  # schreier
  new_schreierpos := new_data!.schreierpos;
  old_schreierpos := old_data!.schreierpos;
  new_schreiergen := new_data!.schreiergen;
  old_schreiergen := old_data!.schreiergen;
  new_schreiermult := new_data!.schreiermult;
  old_schreiermult := old_data!.schreiermult;

  # generators
  gens := new_data!.gens;
  nr_new_gens := Length(gens);
  nr_old_gens := Length(old_data!.gens);

  # lambda/rho
  lambdaperm := LambdaPerm(t);
  rho := RhoFunc(t);

  # look up for old_to_new[i]:=Position(new_orb, old_orb[i]);
  # i.e. position of old R-rep in new_orb
  # TODO(later): this is mainly used to update the orbit graph of the R-rep
  # orbit, but I think this could also be done during the main loop below.

  old_to_new := EmptyPlist(old_nr);
  old_to_new[1] := 1;

  # initialise <reps>, <repslookup>, <repslens>, <lenreps>...
  for i in [2 .. Length(scc)] do
    reps[i] := [];
    repslookup[i] := [];
    repslens[i] := [];
    lenreps[i] := 0;
  od;

  if IsBoundGlobal("ORBC") then
    htadd := HTAdd_TreeHash_C;
    htvalue := HTValue_TreeHash_C;
  else
    htadd := HTAdd;
    htvalue := HTValue;
  fi;

  i := 1;

  # install old R-class reps in new_orb
  while new_nr <= old_nr and i < old_nr do
    i := i + 1;

    x := old_orb[i][4];

    pos := old_schreiermult[i];  # lambda - index for x
    m := lookup[pos];
    rank := ActionRank(t)(x);

    if rank > max_rank or scc[m][1] = old_scc[old_lookup[pos]][1] then
      # in either case x is an old R-rep and so has rectified lambda value.
    elif pos = old_scc[old_lookup[pos]][1] then
      x := x * LambdaOrbMult(o, m, pos)[2];
    else
      # x has rectified lambda value but pos refers to the unrectified value
      x := x * LambdaOrbMult(old_o, old_lookup[pos], pos)[1]
       * LambdaOrbMult(o, m, pos)[2];
    fi;

    rhox := rho(x);
    l := htvalue(rho_ht, rhox);
    # l <> fail since we have copied the old rho values

    if not IsBound(lambdarhoht[l]) then
      # old rho-value, but new lambda-rho-combination

      new_nr := new_nr + 1;
      lenreps[m] := lenreps[m] + 1;
      ind := lenreps[m];
      lambdarhoht[l] := [];
      lambdarhoht[l][m] := ind;

      reps[m][ind] := [x];
      repslookup[m][ind] := [new_nr];
      repslens[m][ind] := 1;

      orblookup1[new_nr] := ind;
      orblookup2[new_nr] := 1;

      pt := [t, m, o, x, false, new_nr];
    elif not IsBound(lambdarhoht[l][m]) then
      # old rho-value, but new lambda-rho-combination

      new_nr := new_nr + 1;
      lenreps[m] := lenreps[m] + 1;
      ind := lenreps[m];
      lambdarhoht[l][m] := ind;

      reps[m][ind] := [x];
      repslookup[m][ind] := [new_nr];
      repslens[m][ind] := 1;

      orblookup1[new_nr] := ind;
      orblookup2[new_nr] := 1;

      pt := [t, m, o, x, false, new_nr];
    else
      # old rho value, and maybe we already have a rep of y's R-class...
      ind := lambdarhoht[l][m];
      pt := [t, m, o, x, false, new_nr + 1];
      if not rank > max_rank then
        # this is maybe a new R-reps and so tests are required...

        # check membership in Schutzenberger group via stabiliser chain
        schutz := LambdaOrbStabChain(o, m);

        if schutz = true then
          # the Schutzenberger group is the symmetric group
          old_to_new[i] := repslookup[m][ind][1];
          continue;
        else
          if schutz = false then
            # the Schutzenberger group is trivial
            data_val := htvalue(ht, x);
            if data_val <> fail then
              old_to_new[i] := data_val;
              continue;
            fi;
          else
            # the Schutzenberger group is neither trivial nor symmetric group
            old := false;
            for n in [1 .. repslens[m][ind]] do
              if SchutzGpMembership(S)(schutz, lambdaperm(reps[m][ind][n], x))
                  then
                old := true;
                old_to_new[i] := repslookup[m][ind][n];
                break;
              fi;
            od;
            if old then
              continue;
            fi;
          fi;
        fi;
      fi;
      # if rank>max_rank, then <y> is an old R-rep and hence a new one too
      new_nr := new_nr + 1;
      repslens[m][ind] := repslens[m][ind] + 1;
      reps[m][ind][repslens[m][ind]] := x;
      repslookup[m][ind][repslens[m][ind]] := new_nr;
      orblookup1[new_nr] := ind;
      orblookup2[new_nr] := repslens[m][ind];
    fi;
    rholookup[new_nr] := l;
    new_orb[new_nr] := pt;
    graph[new_nr] := ShallowCopy(old_graph[i]);
    new_schreierpos[new_nr] := old_to_new[old_schreierpos[i]];
    # orb[nr] is obtained from orb[i]
    new_schreiergen[new_nr] := old_schreiergen[i];
    # by multiplying by gens[j]
    new_schreiermult[new_nr] := pos;  # and ends up in position <pos> of
                                      # its lambda orb
    htadd(ht, x, new_nr);
    old_to_new[i] := new_nr;
  od;

  # process the orbit graph
  for i in [1 .. new_nr] do
    for j in [1 .. Length(graph[i])] do
      graph[i][j] := old_to_new[graph[i][j]];
    od;
  od;

  # apply new generators to old R-reps
  new_data!.genstoapply := [nr_old_gens + 1 .. nr_new_gens];
  new_data!.pos := 0;
  new_data!.stopper := old_to_new[old_data!.pos];
  new_data!.init := true;
  Enumerate(new_data, infinity, ReturnFalse);

  new_data!.pos := old_to_new[old_data!.pos];
  new_data!.stopper := false;
  new_data!.genstoapply := [1 .. nr_new_gens];

  return t;
end);

InstallMethod(ClosureInverseSemigroupOrMonoidNC,
"for a function, inverse acting semigroup, finite list of mult. elts, and rec",
[IsFunction,
 IsInverseActingSemigroupRep,
 IsMultiplicativeElementCollection and IsList and IsFinite,
 IsRecord],
function(Constructor, S, coll, opts)
  local gens, T, o, n, x;

  # opts must be copied and processed before calling this function
  # coll must be copied before calling this function

  if IsSemigroupIdeal(S) then
    TryNextMethod();
  fi;

  # TODO(later) split this into two methods
  if Size(coll) > 1 then
    coll := Shuffle(Set(coll));
    n := ActionDegree(coll);
    Sort(coll, {x, y} -> ActionRank(x, n) > ActionRank(y, n));

    for x in coll do
      S := ClosureInverseSemigroupOrMonoidNC(Constructor, S, [x], opts);
    od;
    return S;
  elif coll[1] in S then
    return S;
  fi;

  gens := GeneratorsOfInverseSemigroup(S);
  T := Constructor(Concatenation(gens, coll), opts);

  if not IsIdempotent(coll[1]) then
    Add(coll, coll[1] ^ -1);
  fi;

  if Constructor = InverseMonoid and One(T) <> One(S) then
    Add(coll, One(T));
  fi;

  o := StructuralCopy(LambdaOrb(S));
  coll[1] := ConvertToInternalElement(S, coll[1]);
  AddGeneratorsToOrbit(o, coll);

  # Remove everything related to strongly connected components
  Unbind(o!.scc);
  Unbind(o!.trees);
  Unbind(o!.scc_lookup);
  Unbind(o!.mults);
  Unbind(o!.schutz);
  Unbind(o!.reverse);
  Unbind(o!.rev);
  Unbind(o!.schutzstab);
  Unbind(o!.factorgroups);
  Unbind(o!.factors);

  o!.parent := T;
  o!.scc_reps := [FakeOne(o!.gens)];

  SetLambdaOrb(T, o);
  return T;
end);

# different method for inverse/regular, same for ideals

InstallMethodWithRandomSource(Random,
"for a random source and an acting semigroup",
[IsRandomSource, IsActingSemigroup],
function(rs, s)
  local data, gens, i, w, x, n, m, o, rep, g;

  data := SemigroupData(s);

  if not IsClosedData(data) then
    if HasGeneratorsOfSemigroup(s) then
      gens := GeneratorsOfSemigroup(s);
      i := Random(rs, 1, 2 * Length(gens));
      w := List([1 .. i], x -> Random(rs, 1, Length(gens)));
      return EvaluateWord(gens, w);
    elif IsSemigroupIdeal(s) and HasGeneratorsOfSemigroupIdeal(s) then
      # This clause is currently unreachable
      x := Random(rs, 1, Length(GeneratorsOfSemigroupIdeal(s)));
      gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(s));

      i := Random(rs, 1, Length(gens));
      w := List([1 .. i], x -> Random(rs, 1, Length(gens)));

      x := x * EvaluateWord(gens, w);

      i := Random(rs, 1, Length(gens));
      w := List([1 .. i], x -> Random(rs, 1, Length(gens)));
      return EvaluateWord(gens, w) * x;
    fi;
  fi;

  n := Random(rs, 2, Length(data!.orbit));
  m := data[n][2];
  o := data[n][3];
  rep := data[n][4];

  g := Random(rs, LambdaOrbSchutzGp(o, m));
  i := Random(rs, OrbSCC(o)[m]);
  return ConvertToExternalElement(s,
                                  StabilizerAction(s)(rep, g)
                                  * LambdaOrbMult(o, m, i)[1]);
end);

# different method for inverse, same method for ideals

InstallMethodWithRandomSource(Random,
"for a random source and a regular acting semigroup rep",
[IsRandomSource, IsRegularActingSemigroupRep],
function(rs, S)
  local gens, i, w, x, o, m;

  if not IsClosedOrbit(LambdaOrb(S)) or not IsClosedOrbit(RhoOrb(S)) then
    if HasGeneratorsOfSemigroup(S) then
      gens := GeneratorsOfSemigroup(S);
      i := Random(rs, 1, 2 * Int(Length(gens)));
      w := List([1 .. i], x -> Random(rs, 1, Length(gens)));
      return EvaluateWord(gens, w);
    else
      x := Random(rs, GeneratorsOfSemigroupIdeal(S));
      gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));

      i := Random(rs, 1, Length(gens));
      w := List([1 .. i], x -> Random(rs, 1, Length(gens)));

      x := x * EvaluateWord(gens, w);

      i := Random(rs, 1, Length(gens));
      w := List([1 .. i], x -> Random(rs, 1, Length(gens)));
      return EvaluateWord(gens, w) * x;
    fi;
  fi;

  o := LambdaOrb(S);
  i := Random(rs, 2, Length(o));
  m := OrbSCCLookup(o)[i];
  x := LambdaOrbRep(o, m)
       * Random(rs, LambdaOrbSchutzGp(o, m))
       * LambdaOrbMult(o, m, i)[1];

  o := RhoOrb(S);
  m := OrbSCCLookup(o)[Position(o, RhoFunc(S)(x))];
  i := Random(rs, OrbSCC(o)[m]);

  return ConvertToExternalElement(S, RhoOrbMult(o, m, i)[1] * x);
end);

# same method for inverse ideals

InstallMethodWithRandomSource(Random,
"for a random source and and an acting inverse semigroup rep and generators",
[IsRandomSource, IsInverseActingSemigroupRep],
function(rs, S)
  local gens, i, w, x, o, m;

  if not IsClosedOrbit(LambdaOrb(S)) then
    if HasGeneratorsOfSemigroup(S) then
      gens := GeneratorsOfSemigroup(S);
      i := Random(rs, 1, 2 * Int(Length(gens)));
      w := List([1 .. i], x -> Random(rs, 1, Length(gens)));
      return EvaluateWord(gens, w);
    else
      x := Random(rs, GeneratorsOfSemigroupIdeal(S));
      gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));

      i := Random(rs, 1, Length(gens) / 2);
      w := List([1 .. i], x -> Random(rs, 1, Length(gens)));

      x := x * EvaluateWord(gens, w);

      i := Random(rs, 1, Length(gens) / 2);
      w := List([1 .. i], x -> Random(rs, 1, Length(gens)));
      return EvaluateWord(gens, w) * x;
    fi;
  fi;

  o := LambdaOrb(S);
  i := Random(rs, 2, Length(o));
  m := OrbSCCLookup(o)[i];
  x := LambdaOrbRep(o, m)
       * Random(rs, LambdaOrbSchutzGp(o, m))
       * LambdaOrbMult(o, m, i)[1];

  i := Random(rs, OrbSCC(o)[m]);

  return ConvertToExternalElement(S, LambdaOrbMult(o, m, i)[2] * x);
end);

#############################################################################

# different method for inverse, same method for ideals

InstallMethod(\in,
"for a multiplicative element and regular acting semigroup rep",
[IsMultiplicativeElement, IsRegularActingSemigroupRep],
function(x, S)
  local pos_lambda, pos_rho, m, schutz, n, rep;

  if ElementsFamily(FamilyObj(S)) <> FamilyObj(x)
      or (IsActingSemigroupWithFixedDegreeMultiplication(S)
          and ActionDegree(x) <> ActionDegree(S))
      or ActionDegree(x) > ActionDegree(S) then
    return false;
  elif Position(S, x) <> fail then  # check if x is already known to be in S
    return true;
  elif IsEnumerated(S) then
    return false;
  elif HasAsSSortedList(S) then
    # This is currently unreachable
    return x in AsSSortedList(S);
  elif not (IsMonoid(S) and IsOne(x)) then
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

  x := ConvertToInternalElement(S, x);
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
    # Cannot currently find an example that enters here.
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
# TODO(later) clean this up

InstallMethod(\in,
"for a multiplicative element and inverse acting semigroup rep",
[IsMultiplicativeElement, IsInverseActingSemigroupRep],
function(x, S)
  local o, lambda, lambda_l, rho, rho_l, m, schutz, scc, rep;

  if ElementsFamily(FamilyObj(S)) <> FamilyObj(x)
      or (IsActingSemigroupWithFixedDegreeMultiplication(S)
          and ActionDegree(x) <> ActionDegree(S))
      or ActionDegree(x) > ActionDegree(S) then
    return false;
  elif HasFroidurePin(S) and Position(S, x) <> fail then
    # check if x is already known to be in S
    return true;
  elif HasFroidurePin(S) and IsEnumerated(S) then
    return false;
  elif HasAsSSortedList(S) then
    return x in AsSSortedList(S);
  elif not (IsMonoid(S) and IsOne(x)) then
    if Length(Generators(S)) > 0
        and ActionRank(S)(x) >
        MaximumList(List(Generators(S), x -> ActionRank(S)(x))) then
      Info(InfoSemigroups, 2, "element has larger rank than any element of ",
           "semigroup.");
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

  x := ConvertToInternalElement(S, x);

  o := LambdaOrb(S);
  Enumerate(o);
  lambda := LambdaFunc(S)(x);
  lambda_l := Position(o, lambda);

  if lambda_l = fail then
    return false;
  fi;

  rho := RhoFunc(S)(x);
  rho_l := Position(o, rho);

  if rho_l = fail then
    return false;
  fi;

  # must use LambdaOrb(S) and not a graded lambda orb as LambdaOrbRep(o, m)
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
    x := x * LambdaOrbMult(o, m, lambda_l)[2];
  fi;

  if rho_l <> scc[1] then
    x := LambdaOrbMult(o, m, rho_l)[1] * x;
  fi;

  if IsIdempotent(x) then
    return true;
  elif schutz = false then
    return false;
  fi;

  rep := LambdaOrbRep(LambdaOrb(S), m);
  rho_l := Position(LambdaOrb(S), RhoFunc(S)(rep));

  if rho_l <> OrbSCC(LambdaOrb(S))[m][1] then
    # the D-class rep corresponding to lambda_o and scc.
    rep := LambdaOrbMult(LambdaOrb(S), m, rho_l)[1] * rep;
  fi;
  return SchutzGpMembership(S)(schutz, LambdaPerm(S)(rep, x));
end);

# different method for inverse semigroups

InstallMethod(Size, "for a regular acting semigroup rep",
[IsRegularActingSemigroupRep],
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

InstallMethod(Size, "for an acting inverse semigroup rep",
[IsInverseActingSemigroupRep], 10,
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
