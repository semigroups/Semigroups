#############################################################################
##
#W  slp.gi
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# express generators of LambdaOrbSchutzGp(o, m) as products in generators of
# semigroup. Note that this is not a true factorization of these generators
# since they are not created using TraceSchreierTreeOfSCCForward but using the
# inverse of the element TraceSchreierTreeOfSCCBack. 

InstallGlobalFunction(LambdaOrbWords,
function(o, m)
  local slp, nr, graph, i;

  if IsBound(o!.schutzgens) then 
    if IsBound(o!.schutzgens[m]) then 
      return o!.schutzgens[m];
    fi;
  else 
    o!.schutzgens:=EmptyPlist(Length(OrbSCC(o)));
  fi;
  slp:=o!.slp[m];
  nr:=Length(GeneratorsOfGroup(LambdaOrbSchutzGp(o, m)));
  o!.schutzgens[m]:=EmptyPlist(nr);
  graph:=OrbitGraph(o);
  
  for i in [1..nr] do
    o!.schutzgens[m][i]:=Concatenation(
     TraceSchreierTreeOfSCCForward(o, m, slp[i][1]),
      [slp[i][2]],
       TraceSchreierTreeOfSCCBack(o, m, graph[slp[i][1]][slp[i][2]]));
  od;
  
  return o!.schutzgens[m];
end);

# the exhaustive orbit to find min. length factorisation of schutz gp element
# in terms of the group generators

InstallGlobalFunction(LambdaOrbSchutzSchreier,
function(o, m)
  local g, schreier;

  if IsBound(o!.exhaust) then 
    if IsBound(o!.exhaust[m]) then 
      return o!.exhaust[m];
    fi;
  else 
    o!.exhaust:=EmptyPlist(Length(OrbSCC(o)));
  fi;

  g:=LambdaOrbSchutzGp(o, m);
  o!.exhaust[m]:=Orb(g, One(g), PROD, rec(hashlen:=2*Size(g), schreier:=true));
  Enumerate(o!.exhaust[m]);
  return o!.exhaust[m];
end);

# min. length factorisation of schutz gp element

InstallMethod(Factorization, "for a lambda orbit, scc index, and perm",
[IsLambdaOrb, IsPosInt, IsPerm],
function(o, m, elt)
  local schreier, word, words;

  schreier:=LambdaOrbSchutzSchreier(o, m);
  #express <elt> as a word in the generators of the Schutz gp
  word:=TraceSchreierTreeForward(schreier, Position(schreier, elt));
  
  #<words> gives a pseudo-factorization for the generators of the Schutz gp in
  #terms of the generators of the semigroup
  words:=LambdaOrbWords(o, m);
  
  #convert group generators to semigroup generators
  return Concatenation(List(word, i-> words[i]));
end);

#

InstallMethod(Factorization, 
"for an acting semigroup with generators and element", 
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement], 
function(s, f)
  local o, gens, l, m, data, pos, rep, word1, p, word2;
 
  if not f in s then 
    Error("usage: <f> is not an element of the semigroup <s>,");
    return;
  fi;
 
  o:=LambdaOrb(s);
  gens:=o!.gens;
  l:=Position(o, LambdaFunc(s)(f));
  m:=OrbSCCLookup(o)[l];
  data:=SemigroupData(s);
  pos:=Position(data, f);                     #not <fail> since <f> in <s>
  rep:=data[pos][4];                          #rep of R-class of <f>
  word1:=TraceSchreierTreeForward(data, pos); #a word equal to <rep>
  
  #compensate for the action of the multipliers
  if l<>OrbSCC(o)[m][1] then 
    p:=LambdaPerm(s)(rep, f*LambdaOrbMult(o, m, l)[2]);
    word2:=TraceSchreierTreeOfSCCForward(o, m, l);
    p:=p*LambdaPerm(s)(
     rep*EvaluateWord(gens, word2)*LambdaOrbMult(o, m, l)[2], rep); 
  else 
    p:=LambdaPerm(s)(rep, f);
    word2:=[];
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
"for an acting semigroup with inverse op with generators and element", 
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup, IsAssociativeElement], 
function(s, f)
  local o, gens, l, m, scc, word1, k, rep, p, word2;
 
  if not f in s then 
    Error("usage: <f> is not an element of the semigroup <s>,");
    return;
  fi;
 
  o:=LambdaOrb(s);
  gens:=o!.gens;
  l:=Position(o, LambdaFunc(s)(f));
  m:=OrbSCCLookup(o)[l];
  scc:=OrbSCC(o)[m];

  # find the R-class rep 
  word1:=TraceSchreierTreeForward(o, scc[1]); #lambda value is ok
  #but rho value is wrong
  k:=Position(o, RhoFunc(s)(EvaluateWord(gens, word1)));
  word1:=Concatenation(TraceSchreierTreeOfSCCForward(o, m, k), word1);
  k:=Position(o, RhoFunc(s)(f));
  # <word> is an R-class rep for the R-class of <f>
  word1:=Concatenation(TraceSchreierTreeOfSCCBack(o, m, k), word1);
  rep:=EvaluateWord(gens, word1);

  #compensate for the action of the multipliers
  if l<>scc[1] then 
    p:=LambdaPerm(s)(rep, f*LambdaOrbMult(o, m, l)[2]);
    word2:=TraceSchreierTreeOfSCCForward(o, m, l);
    p:=p*LambdaPerm(s)(
     rep*EvaluateWord(gens, word2)*LambdaOrbMult(o, m, l)[2], rep); 
  else 
    p:=LambdaPerm(s)(rep, f);
    word2:=[];
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
"for an acting semigroup with inverse op with generators and element", 
[IsActingSemigroup and IsRegularSemigroup and HasGeneratorsOfSemigroup,
IsAssociativeElement], 
function(s, f)
  local o, gens, l, m, scc, word1, k, rep, p, word2;
  
  # a semigroup that was not created regular already knows how to factorize its
  # elements
  if HasSemigroupData(s) and IsClosed(SemigroupData(s)) then 
    TryNextMethod();
  fi;

  if not f in s then 
    Error("usage: <f> is not an element of the semigroup <s>,");
    return;
  fi;
 
  o:=RhoOrb(s); Enumerate(o);
  gens:=o!.gens;
  l:=Position(o, RhoFunc(s)(f));

  # find the R-class rep 
  word1:=TraceSchreierTreeBack(o, l);    #rho value is ok
  #trace back to get forward since this is a left orbit
  rep:=EvaluateWord(gens, word1);        #but lambda value is wrong
 
  o:=LambdaOrb(s); Enumerate(o);
  l:=Position(o, LambdaFunc(s)(f));
  m:=OrbSCCLookup(o)[l];
  scc:=OrbSCC(o)[m];
  
  k:=Position(o, LambdaFunc(s)(rep));
  word2:=TraceSchreierTreeOfSCCBack(o, m, k);
  rep:=rep*EvaluateWord(gens, word2); #the R-class rep of the R-class of <f>
  Append(word1, word2);               #and this word equals <rep>

  #compensate for the action of the multipliers
  if l<>scc[1] then 
    p:=LambdaPerm(s)(rep, f*LambdaOrbMult(o, m, l)[2]);
    word2:=TraceSchreierTreeOfSCCForward(o, m, l);
    p:=p*LambdaPerm(s)(
     rep*EvaluateWord(gens, word2)*LambdaOrbMult(o, m, l)[2], rep); 
  else 
    p:=LambdaPerm(s)(rep, f);
    word2:=[];
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

# returns a word in the generators of the parent of <data> equal to the R-class
# representative store in <data!.orbit[pos]>.

# Notes: the code is more complicated than you might think since the R-class
# reps are obtained from earlier reps by left multiplication but the orbit
# multipliers correspond to right multiplication.

InstallMethod(TraceSchreierTreeForward, "for semigp data and pos int",
[IsSemigroupData, IsPosInt], 100,
function(data, pos)
  local word, word2, schreiergen, schreierpos, schreiermult, orb, o, m;
  
  word:=[];  # the word obtained by tracing schreierpos and schreiergen
             # (left multiplication)
  word2:=[]; # the word corresponding to multipliers applied (if any)
             # (right multiplication)

  schreiergen:=data!.schreiergen;
  schreierpos:=data!.schreierpos;
  schreiermult:=data!.schreiermult;

  orb:=data!.orbit;

  while pos > 1 do
    Add(word, schreiergen[pos]);
    #if schreiermult[pos]<>fail then # a multiplier was applied!
    #  o:=orb[pos][3];               # the lambda orb
    #  m:=orb[pos][2];               # the scc
    #  Append(word2,
    #   Reversed(TraceSchreierTreeOfSCCBack(o, m, schreiermult[pos])));
    #fi;
    Append(word2, Reversed(TraceSchreierTreeOfSCCBack(orb[pos][3], orb[pos][2],
     schreiermult[pos])));
    pos:=schreierpos[pos];
  od;

  Append(word, Reversed(word2));
  return word;
end);

#

InstallMethod(SemigroupElementSLP, 
"for an acting semigroup with inverse op & gens, and associative element",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfInverseSemigroup,
IsAssociativeElement],
function(s, x)
  local zip, o, scc, gens, lookup, k, m, word_a, tmp, rep, l, a, slp, word_c, c;

  if not x in s then 
    Error("the element does not belong to the semigroup,");
    return;
  fi;
  
  #

  zip:=function(list)
    local len, out, i;

    len:=Length(list);
    out:=EmptyPlist(2*len);
    for i in [1..len] do
      out[2*i-1]:=list[i];
      out[2*i]:=1;
    od;
    return out;
  end;

  #
 
  o:=LambdaOrb(s);
  scc:=OrbSCC(o);
  gens:=o!.gens;
  lookup:=OrbSCCLookup(o);
  
  # find <a>
  k:=Position(o, RhoFunc(s)(x));
  m:=lookup[k];
  word_a:=TraceSchreierTreeOfSCCBack(o, m, k);
  tmp:=TraceSchreierTreeForward(o, scc[m][1]);
  rep:=EvaluateWord(gens, tmp);
  l:=Position(o, RhoFunc(s)(rep));
  Append(word_a, TraceSchreierTreeOfSCCForward(o, m, l));
  Append(word_a, tmp);
  a:=EvaluateWord(gens, word_a);

  slp:=StraightLineProgram([zip(word_a)], Length(gens));

  # find <c>
  l:=Position(o, LambdaFunc(s)(x));
  word_c:=TraceSchreierTreeOfSCCForward(o, m, l);
  c:=EvaluateWord(gens, word_c);

  # find <b>
  rep:=x*LambdaOrbMult(o, m, l)[2]; #the rep of the R-class of <x>
  tmp:=LambdaPerm(s)(rep, a)^-1;

  if tmp<>() then
    # slp for schutz gp generators in terms of semigp generators
    slp:=ProductOfStraightLinePrograms(slp, 
     CompositionOfStraightLinePrograms(
      SiftShorterSLP(LambdaOrbSchutzGp(o, m), tmp), LambdaOrbSLP(o, m)));
  fi;

  tmp:=LambdaPerm(s)(rep, x*c^-1);
  if tmp<>() then 
    slp:=ProductOfStraightLinePrograms(slp, 
     CompositionOfStraightLinePrograms(
     SiftShorterSLP(LambdaOrbSchutzGp(o, m), tmp), LambdaOrbSLP(o, m)));
  fi;

  #compose slp with <c>

  if not word_c=[] then 
    slp:=ProductOfStraightLinePrograms(slp, 
      StraightLineProgram([zip(word_c)], Length(gens)));
  fi;

  return slp;
end);

#JDm rough!

InstallMethod(SemigroupElementSLP, "for an acting semigroup and acting elt",
[IsActingSemigroup, IsAssociativeElement],
function(s, x)
  local data, nr, gens, zip, o, m, scc, l, word, v, y, p, slp;

  if not x in s then 
    Error("the element does not belong to the semigroup,");
    return;
  fi;
  
  data:=SemigroupData(s);
  nr:=Position(data, x);
  gens:=GeneratorsOfSemigroup(s);

  zip:=function(list)
    local len, out, i;

    len:=Length(list);
    out:=EmptyPlist(2*len);
    for i in [1..len] do
      out[2*i-1]:=list[i];
      out[2*i]:=1;
    od;
    return out;
  end;

  # find group element
  o:=data[nr][3];
  m:=data[nr][2];
  scc:=OrbSCC(o);
  l:=Position(o, LambdaFunc(s)(x));
  word:=TraceSchreierTreeOfSCCForward(o, m, l);
  v:=EvaluateWord(gens, word);

  #JDM this line should be changed
  y:=x*MappingPermListList(OnTuples(o[scc[m][1]], v),
   o[scc[m][1]]); 

  # LambdaMult(o[scc[m][1]], v)^-1

  p:=LambdaPerm(s)(data[nr][4], y);
  # x=rep*p*v

  if p<>() then
    slp:=SiftShorterSLP(LambdaOrbSchutzGp(o, m), p);
    # slp for schutz gp generators in terms of semigp generators
    slp:=CompositionOfStraightLinePrograms(slp, LambdaOrbSLP(o, m));
  fi;

  # find word equaling rep of R-class of x
  if IsBound(slp) then
    slp:=ProductOfStraightLinePrograms(
     StraightLineProgram([zip(TraceSchreierTreeForward(data, nr))],
      Length(gens)), slp);
  else
    slp:=StraightLineProgram(
     [zip(TraceSchreierTreeForward(data, nr))], Length(gens));
  fi;

  # slp for multiplier
  if word<>[] then
    return ProductOfStraightLinePrograms(slp,
     StraightLineProgram([zip(word)], Length(gens)));
  fi;
  return slp;
end);


#

InstallMethod(ShorterSLPStabChain, "for a perm group",
[IsPermGroup],
function(g)
  local S, l, SS, h, o, SSS, stab, short;

  S:=StabilizerChain(g);
  l:=[];

  SS:=S; h:=g;

  while SS!.stab<>false do 
    o:=Orb(h, SS!.orb[1], SS!.orb!.op, rec(schreier:=true));
    Enumerate(o);
    Add(l,o);
    SSS:=SS!.stab;
    stab:=Group(S!.stronggens{SSS!.layergens});
    short:=FindShortGeneratorsOfSubgroup(h, stab);
    Add(l, short);
    SS:=SSS;
    h:=Group(short.gens);
  od;
  o:=Orb(h, SS!.orb[1], SS!.orb!.op, rec(schreier:=true));
  Enumerate(o);
  Add(l,o); 
  return l;
end);

#

InstallMethod(SiftShorterSLP, "for a perm group and perm",
[IsPermGroup, IsPerm],
function(g, x)
  local l, i, fakegens, realgens, y, z, o, pos, word;

  l:=ShorterSLPStabChain(g);

  i:=1;
  fakegens:=GeneratorsWithMemory(List(GeneratorsOfGroup(g), x->()));
  realgens:=GeneratorsOfGroup(g);

  y:=fakegens[1]^0;
  z:=x;

  while i<Length(l) do
    o:=l[i];
    pos:=Position(o, o!.op(o[1],z));
    word:=TraceSchreierTreeForward(o, pos);
    y:=ORB_ApplyWord(y^0,word,fakegens,List(fakegens, x-> x^-1), OnRight)*y;
    z:=ORB_ApplyWord(z, Reversed(word),List(realgens, x-> x^-1),realgens,
    OnRight);
    i:=i+1;
    realgens:=l[i].gens;
    fakegens:=ResultOfStraightLineProgram(l[i].slp,fakegens);
    i:=i+1;
  od;
  
  o:=l[i];
  pos:=Position(o, o!.op(o[1],z));
  word:=TraceSchreierTreeForward(o, pos);    
  y:=ORB_ApplyWord(y^0,word,fakegens,List(fakegens, x-> x^-1),
  OnRight)*y;      

  return SLPOfElm(y);
end);

# returns an slp for the generators of LambdaOrbSchutzGp(o, m) in the
# generators of the semigroup.

# JDM 2 methods required (inverse and non-inverse)

InstallGlobalFunction(LambdaOrbSLP,
function(o, m)
  local g, slp, nr, r, graph, slp_lines, word, i, j;

  if IsBound(o!.slp) then
    if IsBound(o!.slp[m]) and IsStraightLineProgram(o!.slp[m]) then
      return o!.slp[m];
    fi;
  fi;

  g:=LambdaOrbSchutzGp(o, m);
  slp:=o!.slp[m];
  nr:=Length(slp);
  slp_lines:=EmptyPlist(nr);
  r:=Length(o!.gens);

  if nr<>0 then
    graph:=OrbitGraph(o);
    for i in [1..nr] do
      word:=Concatenation(TraceSchreierTreeOfSCCForward(o, m, slp[i][1]),
       [slp[i][2]],
        TraceSchreierTreeOfSCCBack(o, m, graph[slp[i][1]][slp[i][2]]));
      slp_lines[i]:=[];
      for j in [1..Length(word)] do
        slp_lines[i][2*j-1]:=word[j];
        slp_lines[i][2*j]:=1;
      od;
    od;
  fi;

  return StraightLineProgram([slp_lines], r);
end);
#EOF
