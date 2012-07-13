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

# new for 1.0! - LambdaOrbSLP - "for a lambda orb and scc index"
##############################################################################
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

# new for 1.0! - SemigroupEltSLP - "for an acting semigroup and acting elt"
##############################################################################
#JDM we don't correct for the group elt,
#and so the answer is out by a multiple of a group elt.

#JDm rough!

InstallMethod(SemigroupEltSLP, "for an acting semigroup and acting elt",
[IsActingSemigroup, IsActingElt],
function(s, x)
  local data, nr, gens, zip, o, m, scc, l, word, v, y, p, slp;

  # suppose that the data is fully enumerated...
  data:=SemigroupData(s);
  #Enumerate(s, lookingfor:=??)

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

  y:=x*MappingPermListList(OnIntegerTuplesWithT(o[scc[m][1]], v),
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
    if schreiermult[pos]<>fail then # a multiplier was applied!
      o:=orb[pos][3];               # the lambda orb
      m:=orb[pos][2];            # the scc
      Append(word2,
       Reversed(TraceSchreierTreeOfSCCBack(o, m, schreiermult[pos])));
    fi;
    pos:=schreierpos[pos];
  od;

  Append(word, Reversed(word2));
  return word;
end);


# old

#FFF

# mod for 0.5! - Factorization - "for a trans. semigp. and trans."
#############################################################################

#InstallOtherMethod(Factorization, "for a trans. semigroup and trans.", 
#[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsTransformation], 
#function(s, f)
#  local data, l, o, rep, p, w, g, q;
# 
#  if not f in s then 
#    Error("transformation is not an element of the semigroup,");
#    return;
#  fi;
# 
#  if not s!.opts!.schreier then 
#    Error("it is not possible to factorize elements of this semigroup,");
#    return;
#  fi;
#
#  #data:=PreInOrbitsOfImages(s, f, false)[2];
#
#  #l:=data[3]; o:=ImageOrbitFromData(s, data);
#  #data[3]:=ImageOrbitSCCFromData(s, data)[1]; #JDM hack rectify!
#  #rep:=RClassRepFromData(s, data); p:=data[8];
#
#  if p=fail then 
#    p:=PermLeftQuoTransformationNC(rep![1], data[7]);
#  fi;
#
#  if l=data[3] and p=() then # f is an R-class rep!
#    Info(InfoCitrus, 2, "transformation is an R-class representative.");
#   # return TraceRClassRepsTree(s, RClassIndexFromData(s, data));
#  fi;
#  
#  if not l=data[3] then 
#    w:=TraceSchreierTreeOfSCCForward(o, data[4], l);
#    g:=EvaluateWord(Generators(s), w);
#    q:=PermLeftQuoTransformationNC(rep*g*ImageOrbitPermsFromData(s, data)[l],
#     rep); # would be good to remove this step!
#  else
#    w:=[]; q:=();
#  fi;
#
#  if p*q=() then 
#    return Concatenation(TraceRClassRepsTree(s, RClassIndexFromData(s,
#     data)), w);
#  fi;
#  
#  # f= rep*p*q*g. 
#  
#  return Concatenation(TraceRClassRepsTree(s, RClassIndexFromData(s, data)),
#   Factorization(s, data, p*q), w);
#end);

## new for 0.4! - Factorization - "for a trans. semi., img data, and perm" 
##############################################################################
## Usage: s = trans. semigroup, data = image data, f = permutation
#
## Returns: a word in the generators of s that acts on the image of
## the representative of the R-class with data <data> in the same way that f
## acts on this image.
#
## Notes: this is rather slow! Require some MN assistance with this one. 
#
#InstallOtherMethod(Factorization, "for a trans. semi., img data, and perm",
#[IsTransformationSemigroup, IsList, IsPerm],
#function(s, data, f)
#  local g, w, out, orders, power, gen, o, word, graph, m, u, i;
#  
#  g:=ImageOrbitSchutzGpFromData(s, data);
#  w:=String(Factorization(g, f));
#  
#  if w="<identity ...>" then
#    return [];
#  fi;
# 
#  w:=List(SplitString(w, "*"), x-> SplitString(x, "^"));
#  out:=[]; orders:=List(GeneratorsOfGroup(g), Order);
#  
#  for u in w do 
#    if IsBound(u[2]) then 
#      power:=Int(u[2]); gen:=Int(u[1]{[2..Length(u[1])]});
#      if IsNegInt(power) then 
#        power:=power+orders[gen];
#      fi;
#      for i in [1..power] do 
#        Add(out, gen);
#      od;
#    else
#      Add(out, Int(u[1]{[2..Length(u[1])]}));
#    fi;
#  od;
#  o:=ImageOrbitFromData(s, data);
#  word:=o!.schutz[data[4]][3];
#  graph:=OrbitGraph(o); m:=data[4];
#
#  return Concatenation(List(out, x->
#  Concatenation([TraceSchreierTreeOfSCCForward(o, m,
#  word[x][1]), [word[x][2]], TraceSchreierTreeOfSCCBack(o, m,
#  graph[word[x][1]][word[x][2]])])));
#end);
#
#TTT

# new for 0.4! - TraceRClassRepsTree - not a user function!
#############################################################################
# Usage: s = trans. semigroup; i = index of R-class rep 

# Returns: a word in the generators of s equal to GreensRClassReps(s)[i]. 

#InstallGlobalFunction(TraceRClassRepsTree, 
#function(s, i)
#  local o, gen1, pos1, gen2, pos2, word_1, word_2, j, orb, m, l;
#
#  Info(InfoCitrus, 4, "TraceRClassRepsTree");
#
#  if not s!.opts!.schreier then 
#    Error("it is not possible to factorize elements of this semigroup,");
#    return;
#  fi;
#
#  o:=OrbitsOfImages(s);
#  gen1:=o!.gen1; pos1:=o!.pos1; gen2:=o!.gen2; pos2:=o!.pos2; o:=o!.orbits;
#
#  word_1:=[]; word_2:=[]; j:=i;
#
#  while not gen1[pos2[j]]=fail do
#    Add(word_1, gen1[pos2[j]]);
#    if not ForAny(gen2[j], x-> x=fail) then 
#      orb:=o[gen2[j][1]][gen2[j][2]]; m:=gen2[j][4]; l:=gen2[j][3]; 
#      word_2:= Concatenation(word_2, 
#       Reversed(TraceSchreierTreeOfSCCBack(orb, m, l)));
#    fi;
#    j:=pos1[pos2[j]];
#  od;
#  
#  if not pos2[j]=1 then  
#    Add(word_1, pos2[j]-1);
#  fi;
#  
#  if not ForAny(gen2[j], x-> x=fail) then 
#    orb:=o[gen2[j][1]][gen2[j][2]]; m:=gen2[j][4]; l:=gen2[j][3];
#    word_2:=Concatenation(word_2, 
#     Reversed(TraceSchreierTreeOfSCCBack(orb, m, l)));
#  fi;
#  
#  return Concatenation(word_1, Reversed(word_2));
#end);
#
#EOF
