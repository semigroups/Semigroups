############################################################################
##
#W  normalizer.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(Normalizer, 
"for a permutation group and a semigroup", 
[IsPermGroup, IsSemigroup],
function(G, S)
  return Normalizer(G, S, true); #i.e. deterministic
end);

#

InstallMethod(Normalizer, 
"for a permutation group, a transformation semigroup, a boolean", 
[IsPermGroup, IsTransformationSemigroup, IsBool],
function(G, S, bool)
  local o, U, gens, nrgens, P, pruner, out;

  if (not IsBound(GAPInfo.PackagesLoaded.genss)) or bool then # deterministic
    o:=LambdaOrb(S);   Enumerate(o, infinity);  
    o:=ShallowCopy(o); Remove(o, 1);      
    Sort(o);
    
    Info(InfoSemigroups, 2, "finding the stabilizer of the images...");
    U:=Stabilizer(G, o, OnSetsSets);
    Info(InfoSemigroups, 2, Size(U), " found");

    if Size(U)<>1 then 
      o:=RhoOrb(S);      Enumerate(o, infinity);  
      o:=ShallowCopy(o); Remove(o, 1);      
      Sort(o);
     
      Info(InfoSemigroups, 2, "finding the stabilizer of the kernels...");
      U:=Stabilizer(U, o, 
        function(pt, x) return Set(pt, y-> POW_KER_PERM(y, x)); end);
      Info(InfoSemigroups, 2, Size(U), " found");
    fi;
    
    if Size(U)=1 then 
      return U;
    fi;

    gens:=Generators(S);
    nrgens:=Length(gens);

    P:=function(x)
      local i, pt;
      i:=0; 
      repeat
        i:=i+1;
        pt:=gens[i];
      until i=nrgens or (not pt^x in S);
      return i=nrgens and pt^x in S;
    end;

    return SubgroupProperty(U, P);
  
  else #non-deterministic
    Info(InfoWarning, 1, 
      "This function uses random methods and so there is some chance that");
    Info(InfoWarning, 1, 
      "it will return an incorrect result. Call the function with the third");
    Info(InfoWarning, 1, 
      "argument <true> for a deterministic",
      " (but slower) answer.");
    o:=LambdaOrb(S);   Enumerate(o, infinity);  
    o:=ShallowCopy(o); Remove(o, 1);
    Sort(o, function(x, y) return Length(x)<Length(y); end);
    
    Info(InfoSemigroups, 2, "finding the stabilizer of the images...");
    U:=SetwiseStabilizer(G, OnSets, o).setstab;
    Info(InfoSemigroups, 2, Size(U), " found");

    if Size(U)<>1 then 
      o:=RhoOrb(S);       Enumerate(o, infinity);  
      o:=ShallowCopy(o);  Remove(o, 1); 
      Sort(o, function(x, y) return Maximum(x)<Maximum(y); end);
      
      Info(InfoSemigroups, 2, "finding the stabilizer of the kernels...");
      U:=SetwiseStabilizer(U, POW_KER_PERM, o).setstab;
      Info(InfoSemigroups, 2, Size(U), " found");
    fi;
    
    if Size(U)=1 then 
      return U;
    fi;

    gens:=Generators(S);
    nrgens:=Length(gens);
    
    # recalculate the stabilizer chain using the generators of <S> as the base
    # points
    U:=StabilizerChain(U, rec( Cand := rec( points := gens, 
     ops := ListWithIdenticalEntries(nrgens, OnPoints), used := 0),
     StrictlyUseCandidates := true));

    P:=function(x)
    local i, pt;
      i:=0; 
      repeat
        i:=i+1;
        pt:=gens[i];
      until i=nrgens or (not pt^x in S);
      return i=nrgens and pt^x in S;
    end;

    pruner:=function(stabchain, index, tg, t, word)
      return stabchain!.orb[1]^tg in S;
    end;

    U:=BacktrackSearchStabilizerChainSubgroup(U, P, pruner);
    out:=Group(U!.orb!.gens);
    SetStabilizerChain(out, U);
    return out;
  fi;
end);

#JDM: redo this as above

InstallMethod(Normalizer, 
"for a permutation group and a partial perm semigroup", 
[IsPermGroup, IsPartialPermSemigroup],
function(G, S)
  local o, U, gens, nrgens, used, P, pruner, out, nr, i;

  o:=LambdaOrb(S);   Enumerate(o, infinity);  o:=o{[2..Length(o)]};
  Sort(o, function(x, y) return Length(x)<Length(y); end);
  
  if IsEmpty(o[1]) then 
    Remove(o, 1);
  fi;
  
  nr:=2;
  while Length(o[nr])=1 do
    nr:=nr+1;
  od;
  
  if Union(o{[2..nr]})=MovedPoints(G) then 
    for i in [2..nr] do 
      Remove(o, 1);
    od;
  fi;

  Info(InfoSemigroups, 2, "finding the stabilizer of the images...");
  U:=SetwiseStabilizer(G, OnSets, o).setstab;
  Info(InfoSemigroups, 2, Size(U), " found");

  if Size(U)<>1 then 
    o:=RhoOrb(S);   Enumerate(o, infinity);  o:=o{[2..Length(o)]};
    Sort(o, function(x, y) return Length(x)<Length(y); end);

    Info(InfoSemigroups, 2, "finding the stabilizer of the domains...");
    U:=SetwiseStabilizer(U, RhoAct(S), o).setstab;
    Info(InfoSemigroups, 2, Size(U), " found");
  else 
    return U;
  fi;
  
  if Size(U)=1 then 
    return U;
  fi;

  gens:=Generators(S);
  nrgens:=Length(gens);
  
  # recalculate the stabilizer chain using the generators of <S> as the base
  # points
  U:=StabilizerChain(U, rec( Cand := rec( points := gens, 
   ops := ListWithIdenticalEntries(nrgens, OnPoints), used := 0),
   StrictlyUseCandidates := true));

  P:=function(x)
  local i, pt;
    i:=0; 
    repeat
      i:=i+1;
      pt:=gens[i];
    until i=nrgens or (not pt^x in S);
    return i=nrgens and pt^x in S;
  end;

  pruner:=function(stabchain, index, tg, t, word)
    return stabchain!.orb[1]^tg in S;
  end;

  U:=BacktrackSearchStabilizerChainSubgroup(U, P, pruner);
  out:=Group(U!.orb!.gens);
  SetStabilizerChain(out, U);
  return out;
end);

#JDM: redo this as above

InstallMethod(Normalizer, 
"for a permutation group and a partial perm inverse semigroup", 
[IsPermGroup, IsPartialPermSemigroup and IsInverseSemigroup],
function(G, S)
  local o, U, gens, nrgens, used, P, pruner, out, nr, i;

  o:=LambdaOrb(S);   Enumerate(o, infinity);  o:=o{[2..Length(o)]};
  Sort(o, function(x, y) return Length(x)<Length(y); end);
  
  if IsEmpty(o[1]) then 
    Remove(o, 1);
  fi;
  
  nr:=2;
  while Length(o[nr])=1 do
    nr:=nr+1;
  od;
  
  if Union(o{[2..nr]})=MovedPoints(G) then 
    for i in [2..nr] do 
      Remove(o, 1);
    od;
  fi;

  Info(InfoSemigroups, 2, "finding the stabilizer of the images...");
  U:=SetwiseStabilizer(G, OnSets, o).setstab;
  Info(InfoSemigroups, 2, Size(U), " found");

  if Size(U)=1 then 
    return U;
  fi;

  gens:=Generators(S);
  nrgens:=Length(gens);
  
  # recalculate the stabilizer chain using the generators of <S> as the base
  # points
  U:=StabilizerChain(U, rec( Cand := rec( points := gens, 
   ops := ListWithIdenticalEntries(nrgens, OnPoints), used := 0),
   StrictlyUseCandidates := true));

  P:=function(x)
  local i, pt;
    i:=0; 
    repeat
      i:=i+1;
      pt:=gens[i];
    until i=nrgens or (not pt^x in S);
    return i=nrgens and pt^x in S;
  end;

  pruner:=function(stabchain, index, tg, t, word)
    return stabchain!.orb[1]^tg in S;
  end;

  U:=BacktrackSearchStabilizerChainSubgroup(U, P, pruner);
  out:=Group(U!.orb!.gens);
  SetStabilizerChain(out, U);
  return out;
end);

#JDM the function below is not fully written...

InstallMethod(Normalizer, 
"for a permutation group and a bipartition regular *-semigroup", 
[IsPermGroup, IsBipartitionSemigroup and IsRegularStarSemigroup],
function(G, S)
  local o, U, gens, nrgens, used, P, pruner, out, nr, i;

  o:=LambdaOrb(S);   Enumerate(o, infinity);  o:=o{[2..Length(o)]};
  Sort(o, function(x, y) return Length(x)<Length(y); end);
  
  if IsEmpty(o[1]) then 
    Remove(o, 1);
  fi;
  
  nr:=2;
  while Length(o[nr])=1 do
    nr:=nr+1;
  od;
  
  if Union(o{[2..nr]})=MovedPoints(G) then 
    for i in [2..nr] do 
      Remove(o, 1);
    od;
  fi;

  Info(InfoSemigroups, 2, "finding the stabilizer of the images...");
  U:=SetwiseStabilizer(G, OnSets, o).setstab;
  Info(InfoSemigroups, 2, Size(U), " found");

  if Size(U)=1 then 
    return U;
  fi;

  gens:=Generators(S);
  nrgens:=Length(gens);
  
  # recalculate the stabilizer chain using the generators of <S> as the base
  # points
  U:=StabilizerChain(U, rec( Cand := rec( points := gens, 
   ops := ListWithIdenticalEntries(nrgens, OnPoints), used := 0),
   StrictlyUseCandidates := true));

  P:=function(x)
  local i, pt;
    i:=0; 
    repeat
      i:=i+1;
      pt:=gens[i];
    until i=nrgens or (not pt^x in S);
    return i=nrgens and pt^x in S;
  end;

  pruner:=function(stabchain, index, tg, t, word)
    return stabchain!.orb[1]^tg in S;
  end;

  U:=BacktrackSearchStabilizerChainSubgroup(U, P, pruner);
  out:=Group(U!.orb!.gens);
  SetStabilizerChain(out, U);
  return out;
end);

