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
  local gens, record, lambdafunc, pt, seeds, seedslookup, lambda, pos, o, i;
 
  #JDM: if Parent(I) already knows its LambdaOrb, then probably this should
  #either just copy this, or otherwise create the LambdaOrb(I) directly from
  #LambdaOrb(Parent(I)) without recomputing anything...

  gens:=GeneratorsOfSemigroupIdeal(I);
  
  record:=ShallowCopy(LambdaOrbOpts(I));
  record.scc_reps:=[gens[1]];
  
  record.schreier:=true;        record.orbitgraph:=true;
  record.storenumbers:=true;    record.log:=true;
  record.parent:=I;             record.treehashsize:=I!.opts.hashlen.M;
  
  lambdafunc:=LambdaFunc(I);    pt:=lambdafunc(gens[1]);
  seeds:=[];                    seedslookup:=[fail];    
  #Position(o, lambdafunc(gens[i]))=seedslookup[i]

  for i in [2..Length(gens)] do 
    lambda:=lambdafunc(gens[i]);
    pos:=Position(seeds, lambda);
    if pos<>fail then
      seedslookup[i]:=pos;
    else
      Add(seeds, lambda);
    fi;
  od;

  record.seeds:=seeds;         record.seedslookup:=seedslookup;

  o:=Orb(GeneratorsOfSemigroup(Parent(I)), pt, LambdaAct(I), record);

  SetFilterObj(o, IsLambdaOrb);

  if IsActingSemigroupWithInverseOp(I) then 
    SetFilterObj(o, IsInvLambdaOrb);
  fi;
  
  return o;
end);

#

InstallMethod(RhoOrb, "for an acting semigroup ideal with generators",
[IsActingSemigroup and IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(I)
  local gens, record, rhofunc, pt, seeds, seedslookup, rho, pos, o, i;
 
  #JDM: if Parent(I) already knows its RhoOrb, then probably this should
  #either just copy this, or otherwise create the RhoOrb(I) directly from
  #RhoOrb(Parent(I)) without recomputing anything...

  gens:=GeneratorsOfSemigroupIdeal(I);
  
  record:=ShallowCopy(RhoOrbOpts(I));
  record.scc_reps:=[FakeOne(gens)];
  
  record.schreier:=true;        record.orbitgraph:=true;
  record.storenumbers:=true;    record.log:=true;
  record.parent:=I;             record.treehashsize:=I!.opts.hashlen.M;
  
  rhofunc:=RhoFunc(I);          pt:=rhofunc(gens[1]);
  seeds:=[];                    seedslookup:=[fail];    
  #Position(o, rhofunc(gens[i]))=seedslookup[i]

  for i in [2..Length(gens)] do 
    rho:=rhofunc(gens[i]);
    pos:=Position(seeds, rho);
    if pos<>fail then
      seedslookup[i]:=pos;
    else
      Add(seeds, rho);
    fi;
  od;

  record.seeds:=seeds;         record.seedslookup:=seedslookup;

  o:=Orb(GeneratorsOfSemigroup(Parent(I)), pt, RhoAct(I), record);

  SetFilterObj(o, IsRhoOrb);

  if IsActingSemigroupWithInverseOp(I) then 
    SetFilterObj(o, IsInvRhoOrb);
  fi;
  
  return o;
end);

#

