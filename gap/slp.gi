#############################################################################
##
#W  r.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

ShorterSLPStabChain:=function(g)
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
end;

SiftShorterSLP:=function(g, x)
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
end;

# returns an slp for the generators of LambdaOrbSchutzGp(o, m) in the
# generators of the semigroup.

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
  r:=Length(Generators(o!.semi));

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

#JDM we don't correct for the group elt,
#and so the answer is out by a multiple of a group elt.

# an element <x> of an inverse semigroup <s> is given as the product <abc>,
# where: 
#
# • <a> is an element of the H-class containing the representative of the
#   R-class <R> containing <x>, which is equal to: 
#   TraceSchreierTreeOfSCCBack(o, m, k)
#   *TraceSchreierTreeOfSCCForward(o, m, l)
#   *TraceSchreierTreeForward(o, scc[m][1])
#   where l=Position(o, RhoFunc(s)(rep)), k=Position(o, RhoFunc(s)(x)),
#   rep=TraceSchreierTreeForward(o, scc[m][1])
#
# • <b> is the element of the Schutz group of <R> corresponding to <xc^-1>
#   i.e. b=LambdaPerm(s)(rep,a)^-1*LambdaPerm(s)(rep, xc^-1) where
#   rep=x*LambdaOrbMult(o, m, Position(o, LambdaFunc(s)(x)))[2]
# 
# • <c> is TraceSchreierTreeOfSCCForward(o, m, l); (i.e. that takes
#  o[scc[m][1]] to o[l]), where l=Position(o, LambdaFunc(s)(x))

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
  tmp:=LambdaPerm(s)(rep, a);

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
  gens:=Generators(s);

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


#TTT

# new for 1.0! - TraceSchreierTreeForward - "for semi data and pos int"
##############################################################################
# returns a word in the generators of the parent of <data> equal to the R-class
# representative store in <data!.orbit[pos]>.

# Notes: the code is more complicated than you might think since the R-class
# reps are obtained from earlier reps by left multiplication but the orbit
# multipliers correspond to right multiplication.

InstallOtherMethod(TraceSchreierTreeForward, "for semigp data and pos int",
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

#EOF
