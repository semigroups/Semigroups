############################################################################
##
#W  ideals-lambda-rho.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(LambdaOrb, "for an acting semigroup ideal with generators",
[IsActingSemigroup and IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(I)
  local gens, record, lambdafunc, seeds, pt, seedslookup, genslookup, nr, lambda, pos, o, i;
 
  #JDM: if Parent(I) already knows its LambdaOrb, then probably this should
  #either just copy this, or otherwise create the LambdaOrb(I) directly from
  #LambdaOrb(Parent(I)) without recomputing anything...

  gens:=GeneratorsOfSemigroupIdeal(I);
  
  record:=ShallowCopy(LambdaOrbOpts(I));
  record.scc_reps:=[gens[1]];
  
  record.schreier:=true;        record.orbitgraph:=true;
  record.storenumbers:=true;    record.log:=true;
  record.parent:=I;             record.treehashsize:=I!.opts.hashlen.M;
  
  lambdafunc:=LambdaFunc(I);    
  seeds:=[];                    pt:=lambdafunc(gens[1]); 
  seedslookup:=[1];   #Position(o, lambdafunc(gens[i]))=seedslookup[i]
  genslookup:=[1];    #o[i] equals lambdafunc(gens[genslookup[i]])
  nr:=1;

  for i in [2..Length(gens)] do 
    lambda:=lambdafunc(gens[i]);
    pos:=Position(seeds, lambda);
    if pos<>fail then
      seedslookup[i]:=pos;
    else
      seeds[nr]:=lambda;
      nr:=nr+1;
      seedslookup[i]:=nr;
      genslookup[nr]:=i;
    fi;
  od;

  record.seeds:=seeds;           record.seedslookup:=seedslookup;
  record.genslookup:=genslookup;
  
  o:=Orb(GeneratorsOfSemigroup(Parent(I)), pt, LambdaAct(I), record);

  SetFilterObj(o, IsLambdaOrb);
  SetFilterObj(o, IsIdealOrb);

  if IsActingSemigroupWithInverseOp(I) then 
    SetFilterObj(o, IsInvLambdaOrb);
  fi;
  
  return o;
end);

#

InstallMethod(RhoOrb, "for an acting semigroup ideal with generators",
[IsActingSemigroup and IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(I)
  local gens, record, rhofunc, seeds, pt, seedslookup, genslookup, nr, rho, pos, o, i;
 
  #JDM: if Parent(I) already knows its RhoOrb, then probably this should
  #either just copy this, or otherwise create the RhoOrb(I) directly from
  #RhoOrb(Parent(I)) without recomputing anything...

  gens:=GeneratorsOfSemigroupIdeal(I);
  
  record:=ShallowCopy(RhoOrbOpts(I));
  record.scc_reps:=[gens[1]];
  
  record.schreier:=true;        record.orbitgraph:=true;
  record.storenumbers:=true;    record.log:=true;
  record.parent:=I;             record.treehashsize:=I!.opts.hashlen.M;
  
  rhofunc:=RhoFunc(I);    
  seeds:=[];                    pt:=rhofunc(gens[1]); 
  seedslookup:=[1];   #Position(o, rhofunc(gens[i]))=seedslookup[i]
  genslookup:=[1];    #o[i] equals rhofunc(gens[genslookup[i]])
  nr:=1;

  for i in [2..Length(gens)] do 
    rho:=rhofunc(gens[i]);
    pos:=Position(seeds, rho);
    if pos<>fail then
      seedslookup[i]:=pos;
    else
      seeds[nr]:=rho;
      nr:=nr+1;
      seedslookup[i]:=nr;
      genslookup[nr]:=i;
    fi;
  od;

  record.seeds:=seeds;           record.seedslookup:=seedslookup;
  record.genslookup:=genslookup;
  
  o:=Orb(GeneratorsOfSemigroup(Parent(I)), pt, RhoAct(I), record);

  SetFilterObj(o, IsRhoOrb);
  SetFilterObj(o, IsIdealOrb);

  if IsActingSemigroupWithInverseOp(I) then 
    SetFilterObj(o, IsInvRhoOrb);
  fi;
  
  return o;
end);

# the first position of the returned word refers to the generators of the ideal
# corresponding to the position in the orbit of the point from which the <o[pos]>
# is obtained. For example, [1,2,3] means I.1*S.2*S.3.

InstallMethod( TraceSchreierTreeForward, 
"for an ideal orb and a position (Semigroups)",
  [ IsIdealOrb, IsPosInt ],
  function( o, pos )
    local word;
    word := [];
    while o!.schreierpos[pos] <> fail do
        Add(word,o!.schreiergen[pos]);
        pos := o!.schreierpos[pos];
    od;
    Add(word, o!.genslookup[pos]);
    return Reversed(word);
  end );

#

InstallMethod( EvaluateWord, 
"for a semigroup or ideal, and a word (Semigroups)",
  [ IsSemigroup, IsList ],
  function(S, w)
    local res, gens, i;

    if IsMagmaIdeal(S) then 
      res:=GeneratorsOfSemigroupIdeal(S)[w[1]];
      gens:=GeneratorsOfSemigroup(Parent(S));
    else
      gens:=GeneratorsOfSemigroup(S);
      res:=gens[w[1]];
    fi;
    for i in [2..Length(w)] do
        res := res * gens[w[i]];
    od;
    return res;
  end );

#Usage: o = orbit of images; i = index of scc; j = element of scc[i].

# Notes: returns a word in the generators that takes o!.scc[i][1] to o[j] 
# assuming that j in scc[i]

InstallMethod(TraceSchreierTreeOfSCCForward,
"for an ideal orbit and two positive integers",
[IsIdealOrb, IsPosInt, IsPosInt],
function(o, i, j)
  local tree, scc, word, parent;

  tree:=SchreierTreeOfSCC(o, i);
  scc:=OrbSCC(o)[i];

  word := [];
  parent := tree[2][j];
  while parent  <> fail do
    Add(word, tree[1][j]);
    j := parent;
    parent := tree[2][j];
  od;
  
  Add(word, o!.genslookup[j]);
  return Reversed(word);
end);


InstallMethod(TraceSchreierTreeOfSCCBack,
"for an ideal orbit and two positive integers",
[IsIdealOrb, IsPosInt, IsPosInt],
function(o, i, j)
  local tree, mult, scc, word, parent;

  if not IsInvLambdaOrb(o) then
    tree:=ReverseSchreierTreeOfSCC(o, i);
    mult:=1;
  else
    tree:=SchreierTreeOfSCC(o, i);
    mult:=-1;
  fi;

  scc:=OrbSCC(o)[i];

  word := [];
  parent := tree[2][j];
  while parent <> fail do
    Add(word, tree[1][j]);
    j := parent;
    parent := tree[2][j];
  od;

  return word*mult;
end);
