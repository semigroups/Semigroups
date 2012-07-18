

#GGG

# new for 1.0! - GreensHClasses - for an acting semigroup with inversion
############################################################################

#JDM RhoOrb of h is not defined here, maybe it should be!?

InstallOtherMethod(GreensHClasses, "for an acting semigroup with inversion",
[IsActingSemigroupWithInversion],
function(s)
  local lambda_o, lambda_scc, lambda_len, out, type, hrel, l, n, lambda_m, lambda_mults, f, h, i, j, k;
  
  lambda_o:=Enumerate(LambdaOrb(s), infinity);
  lambda_scc:=OrbSCC(lambda_o);
  lambda_len:=Length(lambda_scc);
  
  out:=EmptyPlist(NrHClasses(s));
  type:=HClassType(s);
  hrel:=GreensHRelation(s);
  l:=ActingSemigroupModifier(s);
  n:=0; 
    
  for i in [1..lambda_l-l] do
    lambda_m:=i+l;
    lambda_mults:=LambdaOrbMults(lambda_o, lambda_m);
    f:=RightOne(LambdaOrbRep(lambda_o, lambda_m));
    for j in lambda_scc[lambda_m] do
      f:=f*lambda_mults[j][1];
      for k in lambda_scc[lambda_m] do
        n:=n+1;
        h:=Objectify(type, rec());
        SetParentSemigroup(h, s);
        SetLambdaOrb(h, lambda_o);
        SetLambdaOrbSCCIndex(h, lambda_m);
        SetRepresentative(h, lambda_mults[k][1]*f);
        SetEquivalenceClassRelation(h, hrel);
        SetIsGreensClassNC(h, false);
        out[n]:=h;
      od;
    od;
  od;
  return out;
end);

#HHH

# new for 1.0! - HClassReps - for an acting semigroup with inversion
############################################################################

InstallOtherMethod(HClassReps, "for an acting semigroup with inversion",
[IsActingSemigroupWithInversion],
function(s)
  local lambda_o, lambda_scc, lambda_len, out, type, hrel, l, n, lambda_m, lambda_mults, f, h, i, j, k;
  
  lambda_o:=Enumerate(LambdaOrb(s), infinity);
  lambda_scc:=OrbSCC(lambda_o);
  lambda_len:=Length(lambda_scc);
  
  out:=EmptyPlist(NrHClasses(s));
  l:=ActingSemigroupModifier(s);
  n:=0; 
    
  for i in [1..lambda_l-l] do
    lambda_m:=i+l;
    lambda_mults:=LambdaOrbMults(lambda_o, lambda_m);
    f:=RightOne(LambdaOrbRep(lambda_o, lambda_m));
    for j in lambda_scc[lambda_m] do
      f:=f*lambda_mults[j][1];
      for k in lambda_scc[lambda_m] do
        n:=n+1;
        out[n]:=lambda_mults[k][1]*f;
      od;
    od;
  od;
  return out;
end);

#NNN

# new for 0.7! - NaturalPartialOrder - "for an inverse semigroup"
##############################################################################
# C function for me!

InstallMethod(NaturalPartialOrder, "for an inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  local elts, n, out, i, j;

  elts:=Elements(s);  n:=Length(elts);
  out:=List([1..n], x-> EmptyPlist(n));
  for i in [n, n-1..1] do
    for j in [i-1,i-2 ..1] do
      if NaturalLeqPP(elts[j], elts[i]) then
        AddSet(out[i], j);
      fi;
    od;
  od;
  Perform(out, ShrinkAllocationPlist);
  return out;
end);

# new for 1.0! - NrIdempotents - for an acting semigroup with inversion
##############################################################################

InstallOtherMethod(NrIdempotents, "for an acting semigroup with inversion",
[IsActingSemigroupWithInversion], 
function(s)
  return Length(Enumerate(LambdaOrb(s), infinity))-
   ActingSemigroupModifier(s);     
end);

# mod for 1.0! - NrRClasses - for an acting semigroup with inversion
##############################################################################

InstallOtherMethod(NrRClasses, "for an acting semigroup with inversion",
[IsActingSemigroupWithInversion], NrLClasses);

# mod for 1.0! - NrHClasses - for an acting semigroup with inversion
##############################################################################

InstallOtherMethod(NrHClasses, "for an acting semigroup with inversion",
[IsActingSemigroupWithInversion],
function(s)
  local o, scc;
  o:=Enumerate(LambdaOrb(s), infinity);
  scc:=OrbSCC(o);

  return Sum(List(scc, m-> Length(m)^2))-ActingSemigroupModifier(s);
end);


