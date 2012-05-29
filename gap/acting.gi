


# old

#JDM this should be mod to remove ImageOrbit
InstallOtherMethod(LambdaOrb, "for a D-class of a trans. semi",
[IsGreensDClass and IsGreensClassOfTransSemigp], ImageOrbit);

InstallOtherMethod(LambdaOrb, "for a D-class of a part perm semi",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup], d-> d!.o);

#JDM this should be mod
InstallOtherMethod(RhoOrb, "for a D-class of a trans. semi",
[IsGreensDClass and IsGreensClassOfTransSemigp], KernelOrbit);

InstallOtherMethod(RhoOrb, "for a D-class of a part perm semi",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup], d-> d!.o);




########

InstallMethod(LambdaAct, "for a transformation semi",
[IsTransformationSemigroup], x-> OnSets);

if IsBound(OnIntegerSetsWithPP) then 
  InstallMethod(LambdaAct, "for a partial perm semi",
  [IsPartialPermSemigroup], x-> OnIntegerSetsWithPP);
fi;

InstallMethod(LambdaDomain, "for a transformation semi",
[IsTransformationSemigroup], s-> [1..Degree(s)]*1);

InstallMethod(LambdaDomain, "for a transformation semi",
[IsPartialPermSemigroup], s-> Points(s));

InstallMethod(LambdaFunc, "for a trans",
[IsTransformationSemigroup], x-> ImageSetOfTransformation);

if IsBound(RanSetPP) then
  InstallMethod(LambdaFunc, "for a partial perm",
    [IsPartialPermSemigroup], x-> RanSetPP);
fi;

InstallMethod(RhoFunc, "for a trans",
[IsTransformationSemigroup], x-> CanonicalTransSameKernel);

if IsBound(DomPP) then
  InstallMethod(RhoFunc, "for a partial perm",
   [IsPartialPermSemigroup], x-> DomPP);
fi;

InstallMethod(LambdaRank, "for a transformation semigroup", 
[IsTransformationSemigroup], x-> Length);

InstallMethod(LambdaRank, "for a semigroup of partial perms", 
[IsPartialPermSemigroup], x-> Length);

InstallMethod(LambdaDegree, "for an acting semigroup", 
[IsActingSemigroup], s-> Length(LambdaDomain(s)));

InstallMethod(LambdaHT, "for a transformation semi",
[IsTransformationSemigroup],
function(s)
return HTCreate(LambdaFunc(s)(Generators(s)[1]), rec(forflatplainlists:=true,
     hashlen:=s!.opts.hashlen.S));
end);

InstallMethod(LambdaHT, "for a partial perm semi",
[IsPartialPermSemigroup],
function(s)
return HTCreate(LambdaFunc(s)(Generators(s)[1]), rec(forflatplainlists:=true,
     hashlen:=s!.opts.hashlen.S));
end);

InstallMethod(LambdaPerm, "for a transformation semi",
[IsTransformationSemigroup], s-> PermLeftQuoTransformationNC);

InstallMethod(LambdaPerm, "for a partial perm semi",
[IsPartialPermSemigroup], s-> function(f,g)
  local h;
  h:=f^-1*g;
  return MappingPermListList(DomPP(h), RanPP(h)); end);

InstallMethod(LambdaMult, "for a transformation semi",
[IsTransformationSemigroup], s-> function(pt, f)
  return MappingPermListList(f![1]{pt}, pt);
end);

InstallMethod(LambdaMult, "for a partial perm semi",
[IsPartialPermSemigroup], s-> function(pt, f) 
  return MappingPermListList(OnIntegerSetsWithPP(pt, f), pt);
end);

#CCC

# new for 1.0! - CreateLambdaOrbMults -
##############################################################################
# from o[i] to o[scc[m][1]]

# lambda_perm = lambda perm of s, gens = generators of semigroup, o = lambda
# orb, m = scc index, scc = the actual scc, f = scc rep.

InstallGlobalFunction(CreateLambdaOrbMults, 
function(lambda_mult, gens, o, m, scc)
  local mults, f, i;

  mults:=o!.mults;

  for i in scc do 
    f:=EvaluateWord(gens, TraceSchreierTreeOfSCCForward(o, m, i));
    mults[i]:=lambda_mult(o[scc[1]], f);
  od;    
  o!.mults:=mults;
  return;
end);

# new for 1.0! - CreateLambdaOrbGS -
##############################################################################
# 

InstallGlobalFunction(CreateLambdaOrbGS, 
function(o, m, scc, gens, nrgens, rep, lookup, orbitgraph, mults, schutz,
schutzstab, lambda_perm)
  local g, is_sym, bound, f, i, j;
 
 g:=Group(()); is_sym:=false;
  
  if Length(o[scc[1]])<1000 then
    bound:=Factorial(Length(o[scc[1]]));
  else
    bound:=infinity;
  fi; 

  for i in scc do 
    for j in [1..nrgens] do 
      if IsBound(orbitgraph[i][j]) and lookup[orbitgraph[i][j]]=m then 
        f:=lambda_perm(rep, rep/mults[i]*(gens[j]*mults[orbitgraph[i][j]]));
        if not f=() then 
          g:=ClosureGroup(g, f);
        fi;

        if Size(g)>=bound then 
          is_sym:=true;
          break;
        fi;
      fi;
    od;
  od;

  schutz[m]:=g;  
  
  if is_sym then 
    schutzstab[m]:=true;
  elif Size(g)=1 then 
    schutzstab[m]:=false;
  else
    schutzstab[m]:=StabChainImmutable(g);
  fi;
  return;
end);

#GGG

# new for 1.0! - GradedLambdaOrb - "for an acting semigroup and elt"
##############################################################################

InstallGlobalFunction(GradedLambdaOrb,
function(s, f, opt)
  local o, onlygrades, onlygradesdata, gradingfunc;

  if opt then   #global
    gradingfunc := function(o,x) return [LambdaRank(s)(x), x]; end;
    onlygrades:=function(x, data_ht)
      return x[1]=LambdaRank(s)(LambdaFunc(s)(f))
       and HTValue(data_ht, x[2])=fail; 
    end;
    onlygradesdata:=LambdaHT(s);
    #JDM check if GradedLambdaOrb of f is already in GradedLambdaOrbs?
  else          #local
    gradingfunc:=function(o,x) return LambdaRank(s)(x); end;
    onlygrades:=function(x,data_ht) 
      return x=LambdaRank(s)(LambdaFunc(s)(f));
    end;
    onlygradesdata:=fail;
  fi;  
 
 o:=Orb(s, LambdaFunc(s)(f), LambdaAct(s),
      rec(
        semigroup:=s,
        forflatplainlists:=true, #JDM probably don't want to assume this..
        hashlen:=CitrusOptionsRec.hashlen.M,
        schreier:=true,
        gradingfunc:=gradingfunc,
        orbitgraph:=true,
        onlygrades:=onlygrades,
        onlygradesdata:=onlygradesdata,
        storenumbers:=true,
        log:=true));
  
  o!.scc_reps:=[f];

  SetIsGradedLambdaOrb(o, true);
  return o;
end);

# new for 1.0! - GradedLambdaOrbs - "for an acting semigroup" 
##############################################################################
# stores so far calculated GradedLambdaOrbs

InstallMethod(GradedLambdaOrbs, "for an acting semigroup", 
[IsActingSemigroup],
function(s)
  
  return Objectify(NewType(FamilyObj(s), IsGradedLambdaOrbs), rec(
    finished:=false,
    orbits:=EmptyPlist(LambdaDegree(s)),
    lens:=[1..LambdaDegree(s)]*0));
end);

#III

# new for 1.0! - InGradedLambdaOrbs - "for an acting semigroup"
##############################################################################

InstallGlobalFunction(InGradedLambdaOrbs, 
[IsActingSemigroup],
function(s, f)
  local o, x, j, k, l, m;
  
  o:=GradedLambdaOrbs(s);
  x:=LambdaFunc(s)(f);
  j:=LambdaRank(s)(x);
  
  if not IsBound(o[j]) then 
    return [false, j, fail, fail, fail, fail];
  fi;

  k:=HTValue(LambdaHT(s), x);

  if k=fail then 
    return [false, j, fail, fail, fail, fail];
  fi;

  l:=Position(o[j][k], x);
  m:=OrbSCCLookup(o[j][k])[l];
  return [true, j, k, m, OrbSCC(o[j][k])[m][1], l];
end);


#LLL

# new for 1.0! - LambdaOrbMults - "for a lambda orb and scc index"
##############################################################################

InstallGlobalFunction(LambdaOrbMults, 
function(o, m) 
  local scc;
 
  scc:=OrbSCC(o)[m];

  if IsBound(o!.mults) then  
    if IsBound(o!.mults[scc[1]]) then 
      return o!.mults;
    fi;
  else
    o!.mults:=EmptyPlist(Length(o)); 
  fi; 
   
  CreateLambdaOrbMults(LambdaMult(o!.semigroup), o!.gens, o, m, scc);
  return o!.mults;
end);


# new for 1.0! - LambdaOrbRep - "for an orbit and pos int"
#############################################################################

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

# new for 1.0! - LambdaOrbSchutzGp - "for a lambda orb and scc index"
##############################################################################

InstallGlobalFunction(LambdaOrbSchutzGp, 
function(o, m)
  local gens;
  
  if IsBound(o!.schutz) then 
    if IsBound(o!.schutz[m]) then 
      return o!.schutz[m];
    fi;
  else
    o!.schutz:=EmptyPlist(Length(OrbSCC(o))); 
    o!.schutzstab:=EmptyPlist(Length(OrbSCC(o)));
  fi;

  gens:=o!.gens;
  
  CreateLambdaOrbGS(o, m, o!.scc[m], gens, Length(gens), 
   LambdaOrbRep(o, m), o!.scc_lookup, OrbitGraph(o), LambdaOrbMults(o, m),
   o!.schutz, o!.schutzstab, LambdaPerm(o!.semigroup)); 
  
  return o!.schutz[m];
end);

# new for 1.0! - LambdaOrbStabChain - "for a lambda orb and scc index"
##############################################################################

InstallGlobalFunction(LambdaOrbStabChain, 
function(o, m)
  local gens;
  
  if IsBound(o!.schutzstab) then 
    if IsBound(o!.schutzstab[m]) then 
      return o!.schutzstab[m];
    fi;
  else
    o!.schutz:=EmptyPlist(Length(OrbSCC(o))); 
    o!.schutzstab:=EmptyPlist(Length(OrbSCC(o)));
  fi;

  gens:=o!.gens;
  
  CreateLambdaOrbGS(o, m, o!.scc[m], gens, Length(gens), 
   LambdaOrbRep(o, m), o!.scc_lookup, OrbitGraph(o), 
   LambdaOrbMults(o, m), o!.schutz,
   o!.schutzstab, LambdaPerm(o!.semigroup)); 
  
  return o!.schutzstab[m];
end);
  
# new for 1.0! - LambdaOrb - "for an acting semigroup"
##############################################################################

InstallMethod(LambdaOrb, "for an acting semigroup",
[IsActingSemigroup],
function(s)

  return Orb(s, LambdaDomain(s), LambdaAct(s),
        rec(forflatplainlists:=true, schreier:=true, orbitgraph:=true,
        storenumbers:=true, log:=true, hashlen:=CitrusOptionsRec.hashlen.M,
        finished:=false, scc_reps:=[()], semigroup:=s));
end);

#PPP

# new for 1.0! - PrintObj - "for graded lambda orbs"
##############################################################################

InstallMethod(PrintObj, [IsGradedLambdaOrbs],
function(o)
  Print(o!.orbits);
  return;
end);

