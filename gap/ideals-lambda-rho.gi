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
  local record, o, lambdafunc, gens, nrgens, orb, ht, nr, schreiergen,
    schreierpos, orbitgraph, lambda, pos, x;
 
  #JDM: if Parent(I) already knows its LambdaOrb, then probably this should
  #either just copy this, or otherwise create the LambdaOrb(I) directly from
  #LambdaOrb(Parent(I)) without recomputing anything...

  # same as for a non-ideal
  record:=ShallowCopy(LambdaOrbOpts(I));
  record.scc_reps:=[FakeOne(GeneratorsOfSemigroupIdeal(I))];
  
  record.schreier:=true;        record.orbitgraph:=true;
  record.storenumbers:=true;    record.log:=true;
  record.parent:=I;             record.treehashsize:=I!.opts.hashlen.M;

  o:=Orb(GeneratorsOfSemigroup(Parent(I)), LambdaOrbSeed(I), LambdaAct(I), record);

  o!.pos:=2;  # don't apply the generators of the parent to the
              # lambda-seed...
  
  SetFilterObj(o, IsLambdaOrb);
  
  if IsActingSemigroupWithInverseOp(I) then 
    SetFilterObj(o, IsInvLambdaOrb);
  fi;
  
  #next add the ideal generators, which is a special case of enumerate
  lambdafunc:=LambdaFunc(I);
  gens := GeneratorsOfSemigroupIdeal(I);
  nrgens:= Length(o!.gens); 

  orb := o!.orbit;
  ht := o!.ht;
  nr := Length(orb);
  
  schreiergen := o!.schreiergen;
  schreierpos := o!.schreierpos;
  orbitgraph := o!.orbitgraph;

  for x in gens do 
    lambda:=lambdafunc(x);
    pos:=HTValue(ht, lambda);
    if pos = fail then
      nr:=nr+1;
      orb[nr]:=lambda;
      HTAdd(ht, lambda, nr);
        
      schreiergen[nr] := fail;
      schreierpos[nr] := fail;
      orbitgraph[nr] := EmptyPlist(nrgens);
      #also we must update the log...
    fi;
  od;    
  
  return o;
end);
