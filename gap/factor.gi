#############################################################################
##
#W  factor.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# factorisation of Schutzenberger group element, the same method works for
# ideals

InstallMethod(Factorization, "for a lambda orbit, scc index, and perm",
[IsLambdaOrb, IsPosInt, IsPerm],
function(o, m, elt)
  local pos, word, factors, out, s, gens, scc, lookup, orbitgraph, genstoapply,
  lambdaperm, rep, bound, nrgens, uword, u, vword, v, f, ex, stop, i, k, l;

  if IsBound(o!.factors) then
    if IsBound(o!.factors[m]) then
      pos := Position(o!.exhaust[m], elt);
      if pos = fail then
        Error("Semigroups: Factorization: usage,\n",
              "the third arg <perm> does not belong to the ",
              "Schutzenberger group,");
        return;
      fi;

      # express <elt> as a word in the generators of the Schutzenberger group
      word := TraceSchreierTreeForward(o!.exhaust[m], pos);
      factors := o!.factors[m];

      # convert group generators to semigroup generators
      out := [];
      for i in word do
        Append(out, factors[i]);
      od;
      return out;
    fi;
  else
    o!.factors := EmptyPlist(Length(OrbSCC(o)));
    o!.exhaust := EmptyPlist(Length(OrbSCC(o)));
  fi;

  s := o!.parent;
  gens := o!.gens;
  scc := OrbSCC(o)[m];
  lookup := o!.scc_lookup;
  orbitgraph := OrbitGraph(o);
  genstoapply := [1 .. Length(gens)];
  lambdaperm := LambdaPerm(s);
  rep := LambdaOrbRep(o, m);
  factors := [];
  bound := Size(LambdaOrbSchutzGp(o, m));
  nrgens := 0;
  stop := false;

  for k in scc do
    uword := TraceSchreierTreeOfSCCForward(o, m, k);
    u := EvaluateWord(o, uword);
    for l in genstoapply do
      if IsBound(orbitgraph[k][l]) and lookup[orbitgraph[k][l]] = m then
        vword := TraceSchreierTreeOfSCCBack(o, m, orbitgraph[k][l]);
        v := EvaluateWord(o, vword);
        f := lambdaperm(rep, rep * u * gens[l] * v);
        if not IsBound(ex) then
          nrgens := nrgens + 1;
          factors[nrgens] := Concatenation(uword, [l], vword);
          ex := Orb([f], (), PROD, rec(hashlen := 2 * bound, schreier := true,
                log := true));
          Enumerate(ex);
        elif not f in ex then
          nrgens := nrgens + 1;
          factors[nrgens] := Concatenation(uword, [l], vword);
          AddGeneratorsToOrbit(ex, [f]);
        fi;
        if Length(ex) = bound then
          stop := true;
          break;
        fi;
      fi;
    od;
    if stop then
      break;
    fi;
  od;

  o!.factors[m] := factors;
  o!.exhaust[m] := ex;

  return Factorization(o, m, elt);
end);

# returns a word in the generators of the parent of <data> equal to the R-class
# representative stored in <data!.orbit[pos]>.

# Notes: the code is more complicated than you might think since the R-class
# reps are obtained from earlier reps by left multiplication but the orbit
# multipliers correspond to right multiplication.

# This method does not work for ideals!

InstallMethod(TraceSchreierTreeForward, "for semigroup data and pos int",
[IsSemigroupData, IsPosInt],
function(data, pos)
  local word1, word2, schreiergen, schreierpos, schreiermult, orb, o, m;

  word1 := []; # the word obtained by tracing schreierpos and schreiergen
             # (left multiplication)
  word2 := []; # the word corresponding to multipliers applied (if any)
             # (right multiplication)

  schreiergen := data!.schreiergen;
  schreierpos := data!.schreierpos;
  schreiermult := data!.schreiermult;

  orb := data!.orbit;

  while pos > 1 do
    Add(word1, schreiergen[pos]);
    Append(word2, Reversed(TraceSchreierTreeOfSCCBack(orb[pos][3], orb[pos][2],
     schreiermult[pos])));
    pos := schreierpos[pos];
  od;

  Append(word1, Reversed(word2));
  return word1;
end);

#

InstallMethod(Factorization,
"for an acting semigroup with generators and element",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(s, f)
  local o, l, m, scc, data, pos, rep, word1, word2, p;

  if not f in s then
    Error("Semigroups: Factorization: usage,\n",
          "the second arg <f> is not an element of the first arg <S>,");
    return;
  fi;

  o := LambdaOrb(s);
  l := Position(o, LambdaFunc(s)(f));
  m := OrbSCCLookup(o)[l];
  scc := OrbSCC(o)[m];

  data := SemigroupData(s);
  pos := Position(data, f);                     #not <fail> since <f> in <s>
  rep := data[pos][4];                          #rep of R-class of <f>

  word1 := TraceSchreierTreeForward(data, pos); #a word equal to <rep>

  #compensate for the action of the multipliers, if necessary
  if l <> scc[1] then
    word2 := TraceSchreierTreeOfSCCForward(o, m, l);
    p := LambdaPerm(s)(rep, f * LambdaInverse(s)(o[scc[1]],
     EvaluateWord(o!.gens, word2)));
  else
    word2 := [];
    p := LambdaPerm(s)(rep, f);
  fi;

  if IsOne(p) then #no need to factorise <p>
    Append(word1, word2);
    return word1;
  fi;

  # factorize the group element <p> over the generators of <s>
  Append(word1, Factorization(o, m, p));
  Append(word1, word2);

  return word1;
end);

#

InstallMethod(Factorization,
"for an acting semigroup with inverse op with generators and element",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup,
IsAssociativeElement],
function(s, f)
  local o, gens, l, m, scc, word1, k, rep, word2, p;

  if not f in s then
    Error("Semigroups: Factorization: usage,\n",
          "the second arg <f> is not an element of the first arg <S>,");
    return;
  fi;

  o := LambdaOrb(s);
  gens := o!.gens;
  l := Position(o, LambdaFunc(s)(f));
  m := OrbSCCLookup(o)[l];
  scc := OrbSCC(o)[m];

  # find the R-class rep
  word1 := TraceSchreierTreeForward(o, scc[1]);
  # lambda value is ok, but rho value is wrong
  k := Position(o, RhoFunc(s)(EvaluateWord(gens, word1)));
  # take rho value of <word1> back to 1st position in scc
  word1 := Concatenation(TraceSchreierTreeOfSCCForward(o, m, k), word1);

  # take rho value of <word1> forwards to rho value of <f>
  k := Position(o, RhoFunc(s)(f));
  word1 := Concatenation(TraceSchreierTreeOfSCCBack(o, m, k), word1);
  # <rep> is an R-class rep for the R-class of <f>
  rep := EvaluateWord(gens, word1);

  # compensate for the action of the multipliers
  if l <> scc[1] then
    word2 := TraceSchreierTreeOfSCCForward(o, m, l);
    p := LambdaPerm(s)(rep, f
         * LambdaInverse(s)(o[scc[1]], EvaluateWord(gens, word2)));
  else
    word2 := [];
    p := LambdaPerm(s)(rep, f);
  fi;

  if IsOne(p) then
    Append(word1, word2);
    return word1;
  fi;

  # factorize the group element <p> over the generators of <s>
  Append(word1, Factorization(o, m, p));
  Append(word1, word2);

  return word1;
end);

#

InstallMethod(Factorization,
"for a regular acting semigroup with generators and element",
[IsActingSemigroup and IsRegularSemigroup and HasGeneratorsOfSemigroup,
IsAssociativeElement],
function(s, f)
  local o, gens, l, m, scc, word1, k, rep, p, word2;

  if not f in s then
    Error("Semigroups: Factorization: usage,\n",
          "the second arg <f> is not an element of the first arg <S>,");
    return;
  fi;

  o := RhoOrb(s);
  Enumerate(o);
  gens := o!.gens;
  l := Position(o, RhoFunc(s)(f));

  # find the R-class rep
  word1 := TraceSchreierTreeBack(o, l);    #rho value is ok
  #trace back to get forward since this is a left orbit
  rep := EvaluateWord(gens, word1);        #but lambda value is wrong

  o := LambdaOrb(s);
  Enumerate(o);
  l := Position(o, LambdaFunc(s)(f));
  m := OrbSCCLookup(o)[l];
  scc := OrbSCC(o)[m];

  k := Position(o, LambdaFunc(s)(rep));
  word2 := TraceSchreierTreeOfSCCBack(o, m, k);
  rep := rep * EvaluateWord(gens, word2); #the R-class rep of the R-class of <f>
  Append(word1, word2);               #and this word equals <rep>

  #compensate for the action of the multipliers
  #JDM: update this as above
  if l <> scc[1] then
    word2 := TraceSchreierTreeOfSCCForward(o, m, l);
    p := LambdaPerm(s)(rep,
         f * LambdaInverse(s)(o[scc[1]], EvaluateWord(gens, word2)));
  else
    word2 := [];
    p := LambdaPerm(s)(rep, f);
  fi;

  if IsOne(p) then
    Append(word1, word2);
    return word1;
  fi;

  # factorize the group element <p> over the generators of <s>
  Append(word1, Factorization(o, m, p));
  Append(word1, word2);

  return word1;
end);

#EOF
