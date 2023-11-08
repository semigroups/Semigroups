#############################################################################
##
##  attributes/factor.gi
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods relating to factorising elements of a semigroup
# over its generators.

# this is declared in the library, but there is no method for semigroups in the
# library.

InstallMethod(Factorization,
"for semigroup with CanUseFroidurePin and multiplicative element",
[IsSemigroup and CanUseFroidurePin, IsMultiplicativeElement],
MinimalFactorization);

InstallMethod(NonTrivialFactorization,
"for semigroup with CanUseFroidurePin and multiplicative element",
[IsSemigroup and CanUseFroidurePin, IsMultiplicativeElement],
function(S, x)
  local gens, pos, gr, verts, i, j, y;

  if not x in S then
    ErrorNoReturn("the 2nd argument (a mult. elt.) must belong to the ",
                  "1st argument (a semigroup)");
  elif HasIndecomposableElements(S)
      and x in IndecomposableElements(S) then
    return fail;
  fi;

  # if <x> is not a generator of <S>, then any factorization is non-trivial
  gens := GeneratorsOfSemigroup(S);
  pos := Position(gens, x);
  if pos = fail then
    return Factorization(S, x);
  elif IsIdempotent(x) then
    return [pos, pos];
  fi;

  pos := PositionCanonical(S, x);
  gr  := RightCayleyDigraph(S);
  verts := InNeighboursOfVertex(gr, pos);
  if IsEmpty(verts) then
    return fail;
  fi;
  i := verts[1];
  j := Position(OutNeighboursOfVertex(gr, i), pos);
  y := EnumeratorCanonical(S)[i];
  return Concatenation(MinimalFactorization(S, y), [j]);
end);

InstallMethod(NonTrivialFactorization,
"for an acting semigroup with generators and multiplicative element",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsMultiplicativeElement],
function(S, x)
  local factorization, id, gens, data, pos, graph, j, i;

  if not x in S then
    ErrorNoReturn("the 2nd argument (a mult. elt.) must belong to the ",
                  "1st argument (a semigroup)");
  elif HasIndecomposableElements(S)
      and x in IndecomposableElements(S) then
    return fail;
  fi;

  factorization := Factorization(S, x);
  if not IsTrivial(factorization) then
    return factorization;
  fi;

  # Attempt to find a right identity for x.
  id := RightIdentity(S, x);
  if id <> fail then
    return Concatenation(factorization, Factorization(S, id));
  fi;

  # {x} is a non-reg trivial R-class.
  Assert(1, IsTrivial(RClass(S, x)) and not IsRegularGreensClass(RClass(S, x)));

  # Attempt to find a left identity for x.
  id := LeftIdentity(S, x);
  if id <> fail then
    return Concatenation(Factorization(S, id), factorization);
  fi;

  # {x} is a non-reg trivial D-class. Either {x} is maximal or <x> is redundant.
  Assert(1, IsTrivial(DClass(S, x)) and not IsRegularDClass(DClass(S, x)));

  # If <x> is redundant, we can decompose <x> as a left multiple of some other
  # R-class rep of the semigroup
  gens := GeneratorsOfSemigroup(S);
  data := SemigroupData(S);
  Enumerate(data);
  pos := Position(data, x);
  graph := OrbitGraph(data);
  for j in [2 .. Length(data)] do
    for i in [1 .. Length(gens)] do
      if graph[j][i] = pos then
        # x = gens[i] * RClassReps(S)[j - 1].
        return Concatenation([i], Factorization(S, data[j][4]));
      fi;
    od;
  od;

  # {x} is a maximal D-class and is therefore an indecomposable element.
  return fail;
end);

InstallMethod(NonTrivialFactorization,
"for an acting inverse semigroup rep with generators and element",
[IsInverseActingSemigroupRep and HasGeneratorsOfSemigroup,
 IsMultiplicativeElement],
function(S, x)
  local pos;

  if not x in S then
    ErrorNoReturn("the 2nd argument (a mult. elt.) must belong to the ",
                  "1st argument (a semigroup)");
  fi;
  pos := Position(GeneratorsOfSemigroup(S), x);
  if pos = fail then
    return Factorization(S, x);
  fi;
  return [pos, -pos, pos];
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
          if bound = 1 or not x in G then
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
    factors := o!.factors[m];
    G       := o!.factorgroups[m];
  fi;

  if not p in G then
    ErrorNoReturn("the 3rd argument <p> does not belong to the ",
                  "Schutzenberger group");
  elif IsEmpty(factors) then
    # No elt of the semigroup stabilizes the relevant lambda value.  Therefore
    # the Schutzenberger group is trivial, and corresponds to the action of an
    # adjoined identity. No element of the semigroup acts like <p> = ().
    return fail;
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
    if p = () then
      # When p = (), LetterRepAssocWord gives the empty word. The case p = () is
      # only used by NonTrivialFactorization, which requires a non-empty word.
      # Return [k, -k] (i.e. kk^-1), where k is gen with smallest order in G.
      v := infinity;
      for i in [1 .. Length(GeneratorsOfGroup(G))] do
        x := Order(G.(i));
        if x < v then
          k := i;
          v := x;
        fi;
      od;
      word := [k, -k];
    else
      epi := EpimorphismFromFreeGroup(G);
      word := LetterRepAssocWord(PreImagesRepresentativeNC(epi, p));
    fi;
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

  word1 := [];  # the word obtained by tracing schreierpos and schreiergen
                # (left multiplication)
  word2 := [];  # the word corresponding to multipliers applied (if any)
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
    ErrorNoReturn("the 2nd argument (a mult. elt.) must belong to the ",
                  "1st argument (a semigroup)");
  else
    pos := Position(S, x);  # position in the current data structure if any
    if pos <> fail then
      return MinimalFactorization(S, x);
    fi;
  fi;

  o := Enumerate(LambdaOrb(S));
  l := Position(o, LambdaFunc(S)(x));
  m := OrbSCCLookup(o)[l];
  scc := OrbSCC(o)[m];

  data := SemigroupData(S);
  pos := Position(data, x);                     # Not <fail> since <f> in <s>
  rep := data[pos][4];                          # rep of R-class of <f>

  word1 := TraceSchreierTreeForward(data, pos);  # A word equal to <rep>

  # Compensate for the action of the multipliers, if necessary
  if l <> scc[1] then
    word2 := TraceSchreierTreeOfSCCForward(o, m, l);
    p := LambdaPerm(S)(rep, x *
                       LambdaInverse(S)(o[scc[1]],
                                        EvaluateWord(o!.gens, word2)));
  else
    word2 := [];
    p := LambdaPerm(S)(rep, x);
  fi;

  if IsOne(p) then
    # No need to factorise <p>
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
IsCollsElms,
[IsInverseActingSemigroupRep and HasGeneratorsOfSemigroup,
 IsMultiplicativeElement],
function(S, x)
  local pos, o, gens, l, m, scc, word1, k, rep, word2, p;

  if not x in S then
    ErrorNoReturn("the 2nd argument (a mult. elt.) must belong to the ",
                  "1st argument (a semigroup)");
  else
    pos := Position(S, x);  # position in the current data structure if any
    if pos <> fail then
      return MinimalFactorization(S, x);
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
    ErrorNoReturn("the 2nd argument (a mult. elt.) must belong to the ",
                  "1st argument (a semigroup)");
  else
    pos := Position(S, x);  # position in the current data structure if any
    if pos <> fail then
      return MinimalFactorization(S, x);
    fi;
  fi;

  o := RhoOrb(S);
  Enumerate(o);
  gens := o!.gens;
  l := Position(o, RhoFunc(S)(x));

  # find the R-class rep
  word1 := TraceSchreierTreeBack(o, l);
  # rho value is ok but lambda value is wrong
  # trace back to get forward since this is a left orbit
  rep := EvaluateWord(gens, word1);

  o := LambdaOrb(S);
  Enumerate(o);
  l := Position(o, LambdaFunc(S)(x));
  m := OrbSCCLookup(o)[l];
  scc := OrbSCC(o)[m];

  k := Position(o, LambdaFunc(S)(rep));
  word2 := TraceSchreierTreeOfSCCBack(o, m, k);
  rep := rep * EvaluateWord(gens, word2);  # the R-class rep of the R-class of f
  Append(word1, word2);                    # and this word equals rep

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
