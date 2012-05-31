


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

# setup

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
[IsTransformationSemigroup], x-> SSortedList);

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

InstallMethod(LambdaHT, "for an acting semi",
[IsActingSemigroup],
function(s)
return HTCreate(LambdaFunc(s)(SemigroupData(s).gens[1]), 
rec(forflatplainlists:=true,
     hashlen:=s!.opts.hashlen.S));
end);

InstallMethod(RhoHT, "for an acting semi",
[IsActingSemigroup],
function(s)
  local x;
  x:=SemigroupData(s).gens[1]; 
  return HTCreate(Concatenation(LambdaFunc(s)(x), RhoFunc(s)(x)),
  rec(forflatplainlists:=true,
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

# where the work is done..

InstallMethod(\in, "for acting semi elt and graded lambda orbs",
[IsObject, IsGradedLambdaOrbs],
function(lambda_f, o)
  local s;
  
  # created from the same type of obj?
  #if not FamilyObj(lambda_f)=ElementsFamily(FamilyObj(o)) then 
  #  return fail;
  #fi;

  s:=o!.semigroup;
  return not HTValue(LambdaHT(s), lambda_f)=fail;
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

#EEE

# new for 1.0! - ELM_LIST - for graded lambda orbs 
##############################################################################

InstallOtherMethod(ELM_LIST, "for graded lambda orbs, and pos int",
[IsGradedLambdaOrbs, IsPosInt], 
function(o, j)
  return o!.orbits[j];
end);

# new for 1.0! - EnumerateSemigroupData - for an acting semigroup and limit
##############################################################################

InstallGlobalFunction(EnumerateSemigroupData, 
function(s, limit)
  local o, data, act, ht, i, orb, graph, nr, gens, nrgens, genstoapply, graded, reps, lambda, rho, rank, lens, rhoht, nrrepsets, x, pos, lam_x, l, lambda_o, m, val, y, new, schutz, subreps, n, p, j, z;

  data:=SemigroupData(s);
  left_act:=data.left_act; 
  right_act:=data.right_act;
  ht:=data.ht; 
  i:=data.pos; 
  orb:=data.orbit;
  graph:=data.graph;
  nr:=Length(orb);
  gens:=data.gens; 
  nrgens:=Length(gens); 
  genstoapply:=[1..nrgens];
  graded:=GradedLambdaOrbs(s);
  reps:=data.reps;
  lambda:=LambdaFunc(s);
  rho:=RhoFunc(s);
  rank:=LambdaRank(s);
  lens:=graded!.lens;
  rhoht:=RhoHT(s);
  nrrepsets:=data.nrrepsets;

  while i<=limit and i<=nr do 
    i:=i+1;
    for j in genstoapply do 
      x:=left_act(orb[i][4], gens[j]);
      #check if x is already in ht
      pos:=HTValue(ht, x);
      if pos<>fail then 
        graph[i][j]:=pos;
        continue; 
      fi;
      #check if lambda orb of x is already known
      lam_x:=lambda(x);
      #expand
      pos:=Position(graded, lam_x);
      if pos=fail then #new lambda orbit, new R-class
        #expand
        o:=GradedLambdaOrb(s, x, true);
        nrrepsets:=nrrepsets+1;
        reps[nrrepsets]:=[x];
        HTAdd(rhoht, Concatenation(lam_x, rho(x)), nrrepsets);
        x:=[s,[1,1,1],o,x];
      else #old lambda orbit
        o:=graded[pos[1]][pos[2]];
        #expand by keeping lookups in graded or o?!
        m:=OrbSCCLookup(o)[pos[3]];
        #use Create... instead
        y:=right_act(x, LambdaOrbMults(o, m)[pos[3]]);
        val:=HTValue(rhoht, Concatenation(o[o!.scc[m][1]],rho(y)));

        if val=fail then #new rho value
          new:=true;
          val:=Length(reps[pos[1]][pos[2]][m])+1;
          reps[pos[1]][pos[2]][m][val]:=y;
          HTAdd(rhoht[pos[1]][pos[2]][m], rho(x));
        else  # old rho value
          #use Create... instead
          schutz:=LambdaOrbStabChain(graded[pos[1]][pos[2]], m);
          if schutz=true then 
            graph[i][j]:=HTValue(ht, reps[pos[1]][pos[2]][m][val][1]);
          else
            subreps:=reps[pos[1]][pos[2]][m][val];
            if schutz=false then 
              for z in reps do 
                if z=y then 
                  continue;
                fi;
              od;
            else
              n:=0; 
              while n<Length(reps) and new do 
                n:=n+1;
                p:=LambdaPerm(s)(reps[n], y);
                if SiftedPermutation(schutz, p)=() then 
                  graph[i][j]:=HTValue(ht, reps[n]);
                  continue;
                fi;
              od;
            fi;
          fi;
        fi;
      fi;
      nr:=nr+1;
      orb[nr]:=x;
      HTAdd(ht, x[4], nr);
      graph[nr]:=EmptyPlist(nrgens);
      graph[i][j]:= nr;
    od;
  od;
  return true;
end);

#GGG

# new for 1.0! - GradedLambdaOrb - "for an acting semigroup and elt"
##############################################################################

InstallGlobalFunction(GradedLambdaOrb,
function(s, f, opt)
  local graded, data, gradingfunc, onlygrades, onlygradesdata, o, j, k, l;

  if IsTransformation(f) then 
    f:=f![1];
  fi;

  if opt then   #global
    graded:=GradedLambdaOrbs(s);
    data:=Position(graded, f);
  
    if not data=fail then 
      return graded[data[1]][data[2]];
    fi;
    
    gradingfunc := function(o,x) return [LambdaRank(s)(x), x]; end;
    onlygrades:=function(x, data_ht)
      return x[1]=LambdaRank(s)(LambdaFunc(s)(f))
       and HTValue(data_ht, x[2])=fail; 
    end;
    onlygradesdata:=LambdaHT(s);
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

  if opt then # store o
    j:=LambdaRank(s)(LambdaFunc(s)(f));
    k:=graded!.lens[j]+1;
    graded[j][k]:=o;
    Enumerate(o);
    for l in [1..Length(o)] do
      HTAdd(onlygradesdata, o[l], [j,k,l]);
    od;
    o!.data:=[j,k,1,1,1];
    graded!.lens[j]:=k;
  fi;

  return o;
end);

# new for 1.0! - GradedLambdaOrbs - "for an acting semigroup" 
##############################################################################
# stores so far calculated GradedLambdaOrbs

InstallMethod(GradedLambdaOrbs, "for an acting semigroup", 
[IsActingSemigroup],
function(s)
  
  return Objectify(NewType(FamilyObj(s), IsGradedLambdaOrbs), rec(
    semigroup:=s,
    finished:=false,
    orbits:=List([1..LambdaDegree(s)], x-> []),
    lens:=[1..LambdaDegree(s)]*0));
end);

#III

# new for 1.0! - IsBound - for graded lambda orbs and pos int
##############################################################################

InstallMethod(IsBound\[\], "for graded lambda orbs and pos int",
[IsGradedLambdaOrbs, IsPosInt], 
function(o, j)
  return IsBound(o!.orbits[j]);
end);

# new for 1.0! - InGradedLambdaOrbs - "for an acting semigroup"
##############################################################################

InstallGlobalFunction(InGradedLambdaOrbs, 
[IsGradedLambdaOrbs, IsActingSemigroupElt],
function(o, f)
  local s, x, j, k, l, m;
 
  s:=o!.semigroup;
  x:=LambdaFunc(s)(f);
  j:=LambdaRank(s)(x);
  
  if not IsBound(o[j]) then 
    return [false, j, 1, 1, 1, 1];
  fi;

  k:=HTValue(LambdaHT(s), x);

  if k=fail then 
    return [false, j, 1, o!.lens[j]+1, 1, 1];
  fi;

  return ;
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

# new for 1.0! - Position - "for graded lambda orbs and acting semi elt"
##############################################################################

InstallOtherMethod(Position, "for graded lambda orbs and acting semi elt",
[IsGradedLambdaOrbs, IsObject, IsZeroCyc],
function(o, lambda_f, n)
  local s;
  
  #if not FamilyObj(lambda_f)=ElementsFamily(FamilyObj(o)) then
  #  return fail;
  #fi;
  s:=o!.semigroup; 
  return HTValue(LambdaHT(s), lambda_f);
end);

# new for 1.0! - PrintObj - "for graded lambda orbs"
##############################################################################

InstallMethod(PrintObj, [IsGradedLambdaOrbs],
function(o)
  Print("<graded lambda orbs: ");
  View(o!.orbits);
  Print(" >");
  return;
end);

#SSS

# new for 1.0! - SemigroupData - "for an acting semigroup"
##############################################################################

InstallMethod(SemigroupData, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local gens, pt, left_act, right_act, forflatplainlists, data, pos, lam_pt, o,
  m, scc1;
  
  gens:=GeneratorsOfSemigroup(s);

  #JDM remove this case split when transformations are improved
  if IsTransformationSemigroup(s) then 
    gens:=List(gens, x-> x![1]);
    pt:=[1..Length(gens[1])]*1;
    left_act:=function(x,y) return x{y}; end;
    right_act:=function(x,y) return OnTuples(x,y); end;
    forflatplainlists:=true;
  elif IsPartialPermSemigroup(s) then 
    pt:=PartialPermNC(Points(gens), Points(gens));
    left_act:=function(x,y) return ProdPP(y,x); end;
    right_act:=function(x,y) return ProdPP(x,y); end;
    forflatplainlists:=false;
  else
    return fail;
  fi;

  data:=rec(ht:=HTCreate(pt, rec(hashlen:=s!.opts.hashlen.L,
    forflatplainlists:=forflatplainlists)), 
     pos:=0, left_act:=left_act, right_act:=right_act,
     graph:=[EmptyPlist(Length(gens))], 
     reps:=[], gens:=gens, nrrepsets:=0, orbit:=[[,,,pt]]);
  #remove gens when p trans are properly implemented.

  #the following 6 lines can be removed when gens weirdness is resolved
  SetLambdaHT(s, HTCreate(LambdaFunc(s)(pt),
  rec(forflatplainlists:=true,
       hashlen:=s!.opts.hashlen.S)));
  SetRhoHT(s, HTCreate(Concatenation(LambdaFunc(s)(pt), RhoFunc(s)(pt)),
    rec(forflatplainlists:=true,
         hashlen:=s!.opts.hashlen.S)));

  if pt in gens then #install its orbit etc.. #make this InitSemigroupData
    lam_pt:=LambdaFunc(s)(pt);
    pos:=Position(GradedLambdaOrbs(s), lam_pt);
    #expand above and below
    if pos=fail then 
      o:=GradedLambdaOrb(s, pt, true);
      pos:=[1,1,1]; #[scc index, scc[1], pos of LambdaFunc(x) in o]
    else
      o:=GradedLambdaOrbs(s)[pos[1]][pos[2]];
      m:=OrbSCCLookup(o)[pos[3]];
      scc1:=o!.scc[m][1];
      pos:=[m, scc1, pos[3]];
      if not pos[3]=scc1 then 
        pt:=pt*LambdaOrbMults(o, m)[pos[3]];
        lam_pt:=o[scc1];
      fi;
    fi;  
 
    HTAdd(data.ht, pt, 1);
    data.orbit:=[[s, pos, o, pt]];
    data.nrrepsets:=data.nrrepsets+1;
    data.reps[data.nrrepsets]:=[pt];
    HTAdd(RhoHT(s), Concatenation(lam_pt, RhoFunc(s)(pt)), data.nrrepsets);
  fi;

  return data;
end);

