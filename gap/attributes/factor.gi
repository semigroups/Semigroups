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

# this works for infinite semigroups if x is really in there.

InstallMethod(MinimalFactorization,
"for a semigroup and an associative element",
[IsSemigroup, IsMultiplicativeElement],
function(S, x)
  local data, pos;
  if not x in S then
    ErrorNoReturn("Semigroups: MinimalFactorization:\n",
                  "the second argument <x> is not an element ",
                  "of the first argument <S>,");
  fi;
  data := GenericSemigroupData(S);
  pos := Position(data, x);
  return SEMIGROUP_FACTORIZATION(data, pos);
end);

# same method for ideals

# this is declared in the library, but there is no method for semigroups in the
# library.

InstallMethod(Factorization,
"for a semigroup and associative element",
[IsSemigroup, IsMultiplicativeElement],
function(S, x)
  return MinimalFactorization(S, x);
end);

# factorisation of Schutzenberger group element, the same method works for
# ideals

InstallMethod(Factorization, "for a lambda orbit, scc index, and perm",
[IsLambdaOrb, IsPosInt, IsPerm],
function(o, m, p)
  local gens, scc, lookup, orbitgraph, genstoapply, lambdaperm, rep, bound, G,
  factors, inversefacts, nr, stop, uword, u, adj, vword, v, x, wword, w, path,
  y, len, xyword, epi, word, out, k, l, i;
  
  if not IsBound(o!.factors) then 
    o!.factors      := [];
    o!.inversefacts := [];
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

    G            := Group(()); 
    factors      := [];
    inversefacts := [];
    nr           := 0;
    stop         := false;

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
            
            # try to find a word in the generators equal to the inverse of <x>
            if IsSemigroupWithInverseOp(o!.parent) then 
              inversefacts[nr] := Reversed(factors[nr]) * -1;
            elif Order(x) = 2 then 
              #Print("Case 1!\n");
              inversefacts[nr] := factors[nr];
            else 
              vword := TraceSchreierTreeOfSCCForward(o, m, adj[l]);
              v     := EvaluateWord(o, vword);
              wword := TraceSchreierTreeOfSCCBack(o, m, k);
              w     := EvaluateWord(o, wword);
              path  := DIGRAPH_PATH(OrbitGraph(o), adj[l], k)[2];
              y     := lambdaperm(rep, rep * v * EvaluateWord(o, path) * w);
              len   := Length(vword) + Length(path) + Length(wword);
              if len <= Length(factors[nr]) * (Order(x) - 1) and y = x ^ -1 then
                #Print("Case 2!\n");
                inversefacts[nr] := Concatenation(vword, path, wword);
              elif Order(x * y) * (len + Length(factors[nr])) - Length(factors[nr])
                   < Length(factors[nr]) * (Order(x) - 1) then 
                #Print("Case 3!\n");
                inversefacts[nr] := [Concatenation(vword, path, wword)];
                xyword := Concatenation(factors[nr], inversefacts[nr]);
                Append(inversefacts[nr], List([1 .. Order(x * y) - 1], 
                                              i -> xyword));
              else
                #Print("Case 4!\n");
                inversefacts[nr] := Concatenation(List([1 .. Order(x) - 1], 
                                                       i -> factors[nr]));
              fi;
            fi;
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

    o!.factorgroups[m] := G;
    o!.inversefacts[m] := inversefacts;
    o!.factors[m]      := factors;
  else 
    G            := o!.factorgroups[m];
    inversefacts := o!.inversefacts[m];
    factors      := o!.factors[m];
  fi;

  if not p in G then
    ErrorNoReturn("Semigroups: Factorization: usage,\n",
                  "the third argument <p> does not belong to the ",
                  "Schutzenberger group,");
  fi;

  # express <elt> as a word in the generators of the Schutzenberger group
  epi := EpimorphismFromFreeGroup(G);
  word := LetterRepAssocWord(PreImagesRepresentative(epi, p));

  # convert group generators to semigroup generators
  out := [];
  for i in word do
    if i > 0 then 
      Append(out, factors[i]);
    else 
      Append(out, inversefacts[-i]);
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

#

InstallMethod(Factorization,
"for an acting semigroup with generators and element",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsMultiplicativeElement],
function(S, x)
  local o, l, m, scc, data, pos, rep, word1, word2, p;

  if not x in S then
    ErrorNoReturn("Semigroups: Factorization: usage,\n",
                  "the second argument <x> is not an element ",
                  "of the first argument <S>,");
  elif HasGenericSemigroupData(S) and x in GenericSemigroupData(S) then 
    return MinimalFactorization(S, x);
  fi;

  o := LambdaOrb(S);
  l := Position(o, LambdaFunc(S)(x));
  m := OrbSCCLookup(o)[l];
  scc := OrbSCC(o)[m];

  data := SemigroupData(S);
  pos := Position(data, x);                     #not <fail> since <f> in <s>
  rep := data[pos][4];                          #rep of R-class of <f>

  word1 := TraceSchreierTreeForward(data, pos); #a word equal to <rep>

  #compensate for the action of the multipliers, if necessary
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

#

InstallMethod(Factorization,
"for an acting semigroup with inverse op with generators and element",
[IsSemigroupWithInverseOp and IsActingSemigroup and HasGeneratorsOfSemigroup,
 IsMultiplicativeElement],
function(S, x)
  local o, gens, l, m, scc, word1, k, rep, word2, p;

  if not x in S then
    ErrorNoReturn("Semigroups: Factorization: usage,\n",
                  "the second argument <x> is not an element ",
                  "of the first argument <S>,");
  elif HasGenericSemigroupData(S) and x in GenericSemigroupData(S) then 
    return MinimalFactorization(S, x);
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

#

InstallMethod(Factorization,
"for a regular acting semigroup with generators and element",
[IsActingSemigroup and IsRegularSemigroup and HasGeneratorsOfSemigroup,
 IsMultiplicativeElement],
function(S, x)
  local o, gens, l, m, scc, word1, k, rep, p, word2;

  if not x in S then
    ErrorNoReturn("Semigroups: Factorization: usage,\n",
                  "the second argument <x> is not an element ",
                  "of the first argument <S>,");
  elif HasGenericSemigroupData(S) and x in GenericSemigroupData(S) then 
    return MinimalFactorization(S, x);
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
