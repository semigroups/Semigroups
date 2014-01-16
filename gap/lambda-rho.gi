############################################################################
##
#W  lambda-rho.gi
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# returns the element <f> premultiplied by RhoOrbMult so that the resulting 
# element has its RhoValue in the first position of its scc.

# s, o, f, l, m

InstallGlobalFunction(RectifyRho,
function(arg)
  local f, l, m;

  if not IsClosed(arg[2]) then 
    Enumerate(arg[2], infinity);
  fi;

  f:=arg[3];
  if not IsBound(arg[4]) or arg[4]=fail then 
    l:=Position(arg[2], RhoFunc(arg[1])(f));
  else
    l:=arg[4];
  fi;

  if not IsBound(arg[5]) or arg[5]=fail then 
    m:=OrbSCCLookup(arg[2])[l];
  else
    m:=arg[5];
  fi;

  if l<>OrbSCC(arg[2])[m][1] then
    f:=RhoOrbMult(arg[2], m, l)[2]*f;
  fi;
  return rec(l:=l, m:=m, rep:=f);
end);

#

InstallGlobalFunction(RectifyLambda,
function(arg)
  local f, l, m;
  if not IsClosed(arg[2]) then 
    Enumerate(arg[2], infinity);
  fi;

  f:=arg[3];
  if not IsBound(arg[4]) or arg[4]=fail then 
    l:=Position(arg[2], LambdaFunc(arg[1])(f));
  else
    l:=arg[4];
  fi;
  if not IsBound(arg[5]) or arg[5]=fail then 
    m:=OrbSCCLookup(arg[2])[l];
  else
    m:=arg[5];
  fi;

  if l<>OrbSCC(arg[2])[m][1] then
    f:=f*LambdaOrbMult(arg[2], m, l)[2];
  fi;
  return rec(l:=l, m:=m, rep:=f);
end);
#

InstallMethod(LambdaOrb, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local record, o;
  
  record:=ShallowCopy(LambdaOrbOpts(s));
  record.scc_reps:=[FakeOne(GeneratorsOfSemigroup(s))];
  
  record.schreier:=true;        record.orbitgraph:=true;
  record.storenumbers:=true;    record.log:=true;
  record.parent:=s;             record.treehashsize:=s!.opts.hashlen.M;

  o:=Orb(GeneratorsOfSemigroup(s), LambdaOrbSeed(s), LambdaAct(s), record);
  
  SetFilterObj(o, IsLambdaOrb);
  if IsActingSemigroupWithInverseOp(s) then 
    SetFilterObj(o, IsInvLambdaOrb);
  fi;
  return o;
end);

#

InstallGlobalFunction(LambdaOrbMults,
function(o, m)
  local scc, mults, one, gens, genpos, inv, trace, x, i;

  scc:=OrbSCC(o);

  if IsBound(o!.hasmults) then
    if o!.hasmults[m] then 
      return o!.mults;
    fi;
  else 
    if not IsBound(o!.mults) then 
      mults:=EmptyPlist(Length(o));
      one:=[FakeOne(o!.gens), FakeOne(o!.gens)];
      for x in OrbSCC(o) do 
        mults[x[1]]:=one;
      od;
      o!.mults:=mults;
    fi;
    o!.hasmults:=BlistList([1..Length(scc)], []);
  fi;

  o!.hasmults[m]:=true;
  scc:=OrbSCC(o)[m];
  gens:=o!.gens;
  mults:=o!.mults;
  
  if not IsBound(mults[scc[1]]) then 
    mults[scc[1]]:=[FakeOne(gens), FakeOne(gens)];
  fi; 
 
  genpos:=ReverseSchreierTreeOfSCC(o, m);
  inv:=function(im, f) return LambdaInverse(o!.parent)(im, f); end;

  trace:=function(i)
    local f;

    if IsBound(mults[i]) then 
      return mults[i][2];
    fi;
    f:=gens[genpos[1][i]]*trace(genpos[2][i]);
    mults[i]:=[inv(o[i], f), f];
    return f;
  end;

  for i in scc do 
    trace(i);  
  od;
  return o!.mults;
end);

# f takes o[i] to o[scc[1]] and inv(o[i], f) takes o[scc[1]] to o[i]

InstallGlobalFunction(LambdaOrbMult,
function(o, m, i)
  local mults, one, scc, gens, genpos, inv, trace, x;

  if IsBound(o!.mults) then
    if IsBound(o!.mults[i]) then
      return o!.mults[i];
    fi;
  else
    mults:=EmptyPlist(Length(o));
    one:=[FakeOne(o!.gens), FakeOne(o!.gens)];
    for x in OrbSCC(o) do 
      mults[x[1]]:=one;
    od;
    o!.mults:=mults;
  fi;

  scc:=OrbSCC(o)[m];
  mults:=o!.mults;
  gens:=o!.gens;
  if not IsInvLambdaOrb(o) then
#JDM it would be better to use the SchreierTree here not the ReverseSchreierTree
    genpos:=ReverseSchreierTreeOfSCC(o, m);
    inv:=function(im, f) return LambdaInverse(o!.parent)(im, f); end;

    trace:=function(i)
      local f;
      if IsBound(mults[i]) then 
        return mults[i][2];
      fi;
      f:=gens[genpos[1][i]]*trace(genpos[2][i]);
      mults[i]:=[inv(o[i], f), f];
      return f;
    end;
  else
    genpos:=SchreierTreeOfSCC(o, m);

    trace:=function(i)
      local f;

      if IsBound(mults[i]) then 
        return mults[i][2];
      fi;
      f:=INV(gens[genpos[1][i]])*trace(genpos[2][i]);
      mults[i]:=[INV(f), f];
      return f;
    end;
  fi;

  trace(i);
  return o!.mults[i];
end);

# JDM this is really slow (due to EvaluateWord) for large degree 

InstallGlobalFunction(LambdaOrbRep,
function(o, m)
  local w;

  if IsBound(o!.scc_reps[m]) then
    return o!.scc_reps[m];
  fi;
  w:=TraceSchreierTreeForward(o, OrbSCC(o)[m][1]);
  o!.scc_reps[m]:=o!.scc_reps[1]*EvaluateWord(o!.gens, w);
  return o!.scc_reps[m];
end);

#

InstallGlobalFunction(LambdaOrbSchutzGp, 
function(o, m)
  local s, gens, nrgens, scc, lookup, orbitgraph, lambdaperm, rep, slp, lenslp, len, bound, g, is_sym, vor, f, h, k, l;
  
  if IsBound(o!.schutz) then 
    if IsBound(o!.schutz[m]) then 
      return o!.schutz[m];
    fi;
  else
    o!.schutz:=EmptyPlist(Length(OrbSCC(o))); 
    o!.schutzstab:=EmptyPlist(Length(OrbSCC(o)));
    o!.slp:=EmptyPlist(Length(OrbSCC(o)));
  fi;

  s:=o!.parent;
  gens:=o!.gens; 
  nrgens:=Length(gens);
  scc:=OrbSCC(o)[m];      
  lookup:=o!.scc_lookup;
  orbitgraph:=OrbitGraph(o);
  lambdaperm:=LambdaPerm(s);
  rep:=LambdaOrbRep(o, m);
  slp:=[]; lenslp:=0;

  len:=LambdaRank(s)(o[scc[1]]);

  if len<1000 then
    bound:=Factorial(len);
  else
    bound:=infinity;
  fi;

  g:=Group(()); is_sym:=false;

  for k in scc do
    vor:=EvaluateWord(gens, TraceSchreierTreeOfSCCForward(o, m, k));
    for l in [1..nrgens] do
      if IsBound(orbitgraph[k][l]) and lookup[orbitgraph[k][l]]=m then
        f:=lambdaperm(rep, 
         rep*vor*gens[l]*LambdaOrbMult(o, m, orbitgraph[k][l])[2]);
        #f:=lambdaperm(rep, rep*LambdaOrbMult(o, m, k)[1]*gens[l]
        #  *LambdaOrbMult(o, m, orbitgraph[k][l])[2]);
        h:=ClosureGroup(g, f);
        if Size(h)>Size(g) then 
          g:=h; 
          lenslp:=lenslp+1;
          slp[lenslp]:=[k,l];
          if Size(g)>=bound then
            is_sym:=true;
            break;
          fi;
        fi;
      fi;
    od;
    if is_sym then
      break;
    fi;
  od;

  o!.schutz[m]:=g;
  o!.slp[m]:=slp;

  if is_sym then
    o!.schutzstab[m]:=true;
  elif Size(g)=1 then
    o!.schutzstab[m]:=false;
  else
    o!.schutzstab[m]:=StabChainImmutable(g);
  fi;

  return g;
end);

#

InstallMethod(RhoOrbStabChain, "for a rho orb and scc index",
[IsOrbit, IsPosInt],
function(o, m)
  
  if IsBound(o!.schutzstab) then 
    if IsBound(o!.schutzstab[m]) then 
      return o!.schutzstab[m];
    fi;
  fi;
 
  RhoOrbSchutzGp(o, m, infinity);
  return o!.schutzstab[m];
end);

#

InstallGlobalFunction(LambdaOrbStabChain, 
function(o, m)
  
  if IsBound(o!.schutzstab) then 
    if IsBound(o!.schutzstab[m]) then 
      return o!.schutzstab[m];
    fi;
  fi;
 
  LambdaOrbSchutzGp(o, m);
  return o!.schutzstab[m];
end);

#

InstallMethod(RhoOrb, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local record, o;
  
  # it might be better in the case of having IsClosed(SemigroupData)
  # to just fake the orbit below (we have all the info already).
  # But it seems to be so fast to calculate the 
  # in most cases that there is no point. 

  record:=ShallowCopy(RhoOrbOpts(s));
  record.schreier:=true;        record.orbitgraph:=true;
  record.storenumbers:=true;    record.log:=true;
  record.parent:=s;             record.treehashsize:=s!.opts.hashlen.M;
  record.scc_reps:=[FakeOne(GeneratorsOfSemigroup(s))];

  o:=Orb(GeneratorsOfSemigroup(s), RhoOrbSeed(s), RhoAct(s), record);
  
  SetFilterObj(o, IsRhoOrb);
  if IsActingSemigroupWithInverseOp(s) then
    SetFilterObj(o, IsInvRhoOrb);
  fi;
  return o;
end);

# f takes o[scc[1]] to o[i] and inv(o[scc[1]],f) takes o[i] to o[scc[1]]

InstallGlobalFunction(RhoOrbMult,
function(o, m, i)
  local mults, one, scc, gens, genpos, inv, trace, x;

  if IsBound(o!.mults) then
    if IsBound(o!.mults[i]) then
      return o!.mults[i];
    fi;
  else
    mults:=EmptyPlist(Length(o));
    one:=[FakeOne(o!.gens), FakeOne(o!.gens)];
    for x in OrbSCC(o) do 
      mults[x[1]]:=one;
    od;
    o!.mults:=mults;
  fi;

  scc:=OrbSCC(o)[m];
  mults:=o!.mults;
  gens:=o!.gens;
  genpos:=SchreierTreeOfSCC(o, m);
  inv:=f-> RhoInverse(o!.parent)(o[scc[1]], f);
  
  trace:=function(i)
    local f;

    if IsBound(mults[i]) then 
      return mults[i][1];
    fi;
    f:=gens[genpos[1][i]]*trace(genpos[2][i]);
    mults[i]:=[f, inv(f)];
    return f;
  end;

  trace(i);
  return o!.mults[i];
end);

# f takes o[scc[1]] to o[i] and inv(o[i], f) takes o[i] to o[scc[1]]

InstallGlobalFunction(RhoOrbMults,
function(o, m)
  local scc, mults, one, gens, genpos, inv, trace, x, i;

  scc:=OrbSCC(o);

  if IsBound(o!.hasmults) then
    if o!.hasmults[m] then 
      return o!.mults;
    fi;
  else 
    if not IsBound(o!.mults) then 
      mults:=EmptyPlist(Length(o));
      one:=[FakeOne(o!.gens), FakeOne(o!.gens)];
      for x in OrbSCC(o) do 
        mults[x[1]]:=one;
      od;
      o!.mults:=mults;
    fi;
    o!.hasmults:=BlistList([1..Length(scc)], []);
  fi;

  o!.hasmults[m]:=true;
  scc:=OrbSCC(o)[m];
  gens:=o!.gens;
  mults:=o!.mults;
  
  if not IsBound(mults[scc[1]]) then 
    mults[scc[1]]:=[FakeOne(gens), FakeOne(gens)];
  fi; 

  genpos:=SchreierTreeOfSCC(o, m);
  inv:=f-> RhoInverse(o!.parent)(o[scc[1]], f);

  trace:=function(i)
    local f;

    if IsBound(mults[i]) then 
      return mults[i][1];
    fi;
    f:=gens[genpos[1][i]]*trace(genpos[2][i]);
    mults[i]:=[f, inv(f)];
    return f;
  end;

  for i in scc do 
    trace(i);  
  od;
  return o!.mults;
end);

#

InstallGlobalFunction(RhoOrbRep, 
function(o, m)
  local w;

  if IsBound(o!.scc_reps[m]) then 
    return o!.scc_reps[m];
  fi;

  w:=Reversed(TraceSchreierTreeForward(o, OrbSCC(o)[m][1]));
  o!.scc_reps[m]:=EvaluateWord(o!.gens, w)*o!.scc_reps[1];
  return o!.scc_reps[m];
end);

# JDM could use IsRegular here to speed up?

InstallGlobalFunction(RhoOrbSchutzGp, 
function(o, m, bound)
  local g, s, gens, nrgens, scc, lookup, orbitgraph, lambdaperm, rep, mults, rho_rank, i, j;
  
  if IsBound(o!.schutz) then 
    if IsBound(o!.schutz[m]) then 
      return o!.schutz[m];
    fi;
  else
    o!.schutz:=EmptyPlist(Length(OrbSCC(o)));
    o!.schutzstab:=EmptyPlist(Length(OrbSCC(o)));
  fi;
  
  g:=Group(());

  if bound=1 then 
    o!.schutz[m]:=g;
    o!.schutzstab[m]:=false;
    return g;
  fi;

  s:=o!.parent;
  gens:=o!.gens;
  nrgens:=Length(gens);
  scc:=OrbSCC(o)[m];
  lookup:=o!.scc_lookup;
  orbitgraph:=OrbitGraph(o);
  lambdaperm:=LambdaPerm(s);
  rep:=RhoOrbRep(o, m);
  mults:=RhoOrbMults(o, m);
  
  i:=RhoRank(s)(o[scc[1]]);

  if i<1000 then
    j:=Factorial(i);
    if bound>j then 
      bound:=j;
    fi;
  else
    bound:=infinity;
  fi;
  for i in scc do 
    for j in [1..nrgens] do 
      if IsBound(orbitgraph[i][j]) and lookup[orbitgraph[i][j]]=m then 
        g:=ClosureGroup(g, 
         lambdaperm(rep, mults[orbitgraph[i][j]][2]*gens[j]*mults[i][1]*rep));
        if Size(g)>=bound then 
          break;
        fi;
      fi;
    od;
    if Size(g)>=bound then 
      break;
    fi;
  od;
  
  o!.schutz[m]:=g;
  rho_rank:=RhoRank(s)(o[scc[1]]);

  if rho_rank<1000 and Size(g)=Factorial(rho_rank) then 
    o!.schutzstab[m]:=true;
  elif Size(g)=1 then 
    o!.schutzstab[m]:=false;
  else
    o!.schutzstab[m]:=StabChainImmutable(g);
  fi;

  return g;
end);
