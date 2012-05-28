


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





InstallMethod(LambdaAct, "for a transformation semi",
[IsTransformationSemigroup], x-> OnSets);

if IsBound(OnIntegerSetsWithPP) then 
  InstallMethod(LambdaAct, "for a partial perm semi",
  [IsPartialPermSemigroup], x-> OnIntegerSetsWithPP);
fi;

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

InstallMethod(LambdaGrading, "for a transformation semigroup", 
[IsTransformationSemigroup], x-> Length);

InstallMethod(LambdaGrading, "for a semigroup of partial perms", 
[IsPartialPermSemigroup], x-> Length);

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

# probably don't do this!

InstallMethod(LambdaOrbSCCMultCreator, "for a transformation semi",
[IsTransformationSemigroup],
function(s)
  return 
  function(gens, o, m, i)
  local f;
    f:=CitrusEvalWord(gens, TraceSchreierTreeOfSCCBack(o, m, i));
    return MappingPermListList(o[i], f{o[i]});
  end;
end);

InstallMethod(LambdaOrbSCCMultCreator, "for a partial perm semi",
[IsPartialPermSemigroup],
function(s)
  return 
  function(gens, o, m, i)
  local f;
    f:=TraceSchreierTreeOfSCCForward(o, m, i);
    if f=[] then 
      return PartialPermNC(o[i], o[i]);
    fi;
    return EvaluateWord(gens, f)^-1;
  end;
end);

# new for 1.0! - CreateLambdaOrbSCCMults
##############################################################################

InstallGlobalFunction(CreateLambdaOrbSCCMults, 
function(gens, o, m, scc)
  
  out:=EmptyPlist(Length(scc));

  for i in scc do 
    out[i]:=LambdaOrbSCCMultCreator(s)(gens, o, m, i);
  od;    
  return out;
end);

# new for 1.0! - GradedLambdaOrb - "for an acting semigroup and elt"
##############################################################################

InstallGlobalFunction(GradedLambdaOrb,
function(s, f, opt)
  local o, onlygrades, onlygradesdata, gradingfunc;

  if opt then #global
    gradingfunc := function(o,x) return [LambdaGrading(s)(x), x]; end;
    onlygrades:=function(x, data_ht)
      return x[1]=LambdaGrading(s)(LambdaFunc(s)(f))
       and HTValue(data_ht, x[2])=fail; 
    end;
    onlygradesdata:=LambdaHT(s);
  else #local
    gradingfunc:=function(o,x) return LambdaGrading(s)(x); end;
    onlygrades:=function(x,data_ht) 
      return x=LambdaGrading(s)(LambdaFunc(s)(f));
    end;
    onlygradesdata:=fail;
  fi;  
 
 o:=Orb(s, LambdaFunc(s)(f), LambdaAct(s),
      rec(forflatplainlists:=true, #JDM probably don't want to assume this..
        hashlen:=CitrusOptionsRec.hashlen.M,
        schreier:=true,
        gradingfunc:=gradingfunc,
        orbitgraph:=true,
        onlygrades:=onlygrades,
        onlygradesdata:=onlygradesdata,
        storenumbers:=true,
        log:=true));

  SetIsGradedLambdaOrb(o, true);
  return o;
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
  fi;

  gens:=o!.gens;
  
  o!.schutz[m]:=CreateLambdaOrbSchutzGp(gens, o, OrbSCCRep(o, m), 
   o!.scc[m], o!.lookup[m], OrbitGraph(o), Length(gens), 
    LambdaOrbSCCMults(o, m));
  return o!.schutz[m];
end);

# new for 1.0! - LambdaOrbSCCMults - "for a lambda orb and scc index"
##############################################################################

InstallGlobalFunction(LambdaOrbSCCMults, 
function(o, m) 
 
  scc:=OrbSCC(o)[m];

  if IsBound(o!.mults) then  
    if IsBound(o!.mults[scc[1]]) then 
      return o!.mults;
    fi;
  else
    if not IsClosed(o) then  
      Enumerate(o, infinity); 
    fi; 
    o!.mults:=EmptyPlist(Length(o)); 
  fi; 
 
  o!.mults:=o!.mults+CreateLambdaOrbSCCMults(o!.gens, o, m, scc);
  return o!.mults;
end);

  




