#############################################################################
##
#W  factor.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods relating to factorising elements of a semigroup
# over its generators.

# same method for ideals

# this is declared in the library, but there is no method for semigroups in the
# library.

InstallMethod(Factorization,
"for an enumerable semigroup and multiplicative element",
[IsEnumerableSemigroupRep, IsMultiplicativeElement],
function(S, x)
  return MinimalFactorization(S, x);
end);

# factorisation of Schutzenberger group element, the same method works for
# ideals

InstallMethod(Factorization, "for a lambda orbit, scc index, and perm",
[IsLambdaOrb, IsPosInt, IsPerm],
function(o, m, p)
  local gens, scc, lookup, orbitgraph, genstoapply, lambdaperm, rep, bound, G,
  factors, nr, stop, uword, u, adj, vword, v, x, iso, word, epi, out, k, l, i;

  if not IsBound(o!.factors) then
    o!.factors      := [];
    o!.factorgroups := [];
  fi;

  if not IsBound(o!.factors[m]) then
    gens        := o!.gens;
    scc         := OrbSCC(o)[m];
    lookup      := o!.scc_lookup;
    orbitgraph  := OrbitGraph(o);
    genstoapply := [1 .. Length(gens)];
    lambdaperm  := LambdaPerm(o!.parent);
    rep         := LambdaOrbRep(o, m);
    bound       := Size(LambdaOrbSchutzGp(o, m));

    G           := Group(());
    factors     := [];
    nr          := 0;
    stop        := false;

    for k in scc do
      uword := TraceSchreierTreeOfSCCForward(o, m, k);
      u     := EvaluateWord(o, uword);
      adj   := orbitgraph[k];

      for l in genstoapply do
        if IsBound(adj[l]) and lookup[adj[l]] = m then
          vword := TraceSchreierTreeOfSCCBack(o, m, adj[l]);
          v     := EvaluateWord(o, vword);
          x     := lambdaperm(rep, rep * u * gens[l] * v);
          if not x in G then
            G := ClosureGroup(G, x);
            nr := nr + 1;
            factors[nr] := Concatenation(uword, [l], vword);
            if Size(G) = bound then
              stop := true;
              break;
            fi;
          fi;
        fi;
      od;
      if stop then
        break;
      fi;
    od;
    o!.factors[m]      := factors;
    o!.factorgroups[m] := G;
  else
    factors      := o!.factors[m];
    G            := o!.factorgroups[m];
  fi;

  if not p in G then
    ErrorNoReturn("Semigroups: Factorization: usage,\n",
                  "the third argument <p> does not belong to the ",
                  "Schutzenberger group,");
  fi;

  # express <elt> as a word in the generators of the Schutzenberger group
  if (not IsInverseActingSemigroupRep(o!.parent)) and Size(G) <= 1024 then
    iso := IsomorphismTransformationSemigroup(G);
    word := MinimalFactorization(Range(iso), p ^ iso);
  else
    # Note that in this case the word potentially contains inverses of
    # generators and we do not have a good way, in general, of finding words in
    # the original generators of the semigroup that equal the inverse of a
    # generator of the Schutzenberger group.
    epi := EpimorphismFromFreeGroup(G);
    word := LetterRepAssocWord(PreImagesRepresentative(epi, p));
  fi;

  # convert group generators to semigroup generators
  out := [];
  for i in word do
    if i > 0 then
      Append(out, factors[i]);
    elif IsInverseActingSemigroupRep(o!.parent) then
      Append(out, Reversed(factors[-i]) * -1);
    else
      Append(out, Concatenation(List([1 .. Order(G.(-i)) - 1],
                                x -> factors[-i])));
    fi;
  od;
  return out;
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
  local word1, word2, schreiergen, schreierpos, schreiermult, orb;

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
    Append(word2,
           Reversed(TraceSchreierTreeOfSCCBack(orb[pos][3],
                                               orb[pos][2],
                                               schreiermult[pos])));
    pos := schreierpos[pos];
  od;

  Append(word1, Reversed(word2));
  return word1;
end);

InstallMethod(Factorization,
"for an acting semigroup with generators and element",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsMultiplicativeElement],
function(S, x)
  local pos, o, l, m, scc, data, rep, word1, word2, p;

  if not x in S then
    ErrorNoReturn("Semigroups: Factorization: usage,\n",
                  "the second argument <x> is not an element ",
                  "of the first argument <S>,");
  else
    pos := Position(S, x); # position in the current data structure if any
    if pos <> fail then
      # avoid re-hashing x
      return EN_SEMI_FACTORIZATION(S, pos);
    fi;
  fi;

  o := LambdaOrb(S);
  l := Position(o, LambdaFunc(S)(x));
  m := OrbSCCLookup(o)[l];
  scc := OrbSCC(o)[m];

  data := SemigroupData(S);
  pos := Position(data, x);                     #not <fail> since <f> in <s>
  rep := data[pos][4];                          #rep of R-class of <f>

  word1 := TraceSchreierTreeForward(data, pos); #a word equal to <rep>

  # compensate for the action of the multipliers, if necessary
  if l <> scc[1] then
    word2 := TraceSchreierTreeOfSCCForward(o, m, l);
    p := LambdaPerm(S)(rep, x *
                       LambdaInverse(S)(o[scc[1]],
                                        EvaluateWord(o!.gens, word2)));
  else
    word2 := [];
    p := LambdaPerm(S)(rep, x);
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

InstallMethod(Factorization,
"for an acting inverse semigroup rep with generators and element",
[IsInverseActingSemigroupRep and HasGeneratorsOfSemigroup,
 IsMultiplicativeElement],
function(S, x)
  local pos, o, gens, l, m, scc, word1, k, rep, word2, p;

  if not x in S then
    ErrorNoReturn("Semigroups: Factorization: usage,\n",
                  "the second argument <x> is not an element ",
                  "of the first argument <S>,");
  else
    pos := Position(S, x); # position in the current data structure if any
    if pos <> fail then
      return EN_SEMI_FACTORIZATION(S, pos);
    fi;
  fi;

  o := LambdaOrb(S);
  gens := o!.gens;
  l := Position(o, LambdaFunc(S)(x));
  m := OrbSCCLookup(o)[l];
  scc := OrbSCC(o)[m];

  # find the R-class rep
  word1 := TraceSchreierTreeForward(o, scc[1]);
  # lambda value is ok, but rho value is wrong
  k := Position(o, RhoFunc(S)(EvaluateWord(gens, word1)));
  # take rho value of <word1> back to 1st position in scc
  word1 := Concatenation(TraceSchreierTreeOfSCCForward(o, m, k), word1);

  # take rho value of <word1> forwards to rho value of <f>
  k := Position(o, RhoFunc(S)(x));
  word1 := Concatenation(TraceSchreierTreeOfSCCBack(o, m, k), word1);
  # <rep> is an R-class rep for the R-class of <f>
  rep := EvaluateWord(gens, word1);

  # compensate for the action of the multipliers
  if l <> scc[1] then
    word2 := TraceSchreierTreeOfSCCForward(o, m, l);
    p := LambdaPerm(S)(rep, x *
                       LambdaInverse(S)(o[scc[1]], EvaluateWord(gens, word2)));
  else
    word2 := [];
    p := LambdaPerm(S)(rep, x);
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

InstallMethod(Factorization,
"for a regular acting semigroup with generators and element",
[IsActingSemigroup and IsRegularSemigroup and HasGeneratorsOfSemigroup,
 IsMultiplicativeElement],
function(S, x)
  local pos, o, gens, l, word1, rep, m, scc, k, word2, p;

  if not x in S then
    ErrorNoReturn("Semigroups: Factorization: usage,\n",
                  "the second argument <x> is not an element ",
                  "of the first argument <S>,");
  else
    pos := Position(S, x); # position in the current data structure if any
    if pos <> fail then
      return EN_SEMI_FACTORIZATION(S, pos);
    fi;
  fi;

  o := RhoOrb(S);
  Enumerate(o);
  gens := o!.gens;
  l := Position(o, RhoFunc(S)(x));

  # find the R-class rep
  word1 := TraceSchreierTreeBack(o, l);    #rho value is ok
  #trace back to get forward since this is a left orbit
  rep := EvaluateWord(gens, word1);        #but lambda value is wrong

  o := LambdaOrb(S);
  Enumerate(o);
  l := Position(o, LambdaFunc(S)(x));
  m := OrbSCCLookup(o)[l];
  scc := OrbSCC(o)[m];

  k := Position(o, LambdaFunc(S)(rep));
  word2 := TraceSchreierTreeOfSCCBack(o, m, k);
  rep := rep * EvaluateWord(gens, word2);
  #the R-class rep of the R-class of <f>
  Append(word1, word2);               #and this word equals <rep>

  #compensate for the action of the multipliers
  if l <> scc[1] then
    word2 := TraceSchreierTreeOfSCCForward(o, m, l);
    p := LambdaPerm(S)(rep, x *
                       LambdaInverse(S)(o[scc[1]], EvaluateWord(gens, word2)));
  else
    word2 := [];
    p := LambdaPerm(S)(rep, x);
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
