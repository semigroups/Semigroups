#############################################################################
###
##W  acting.gi
##Y  Copyright (C) 2011-12                                James D. Mitchell
###
###  Licensing information can be found in the README file of this package.
###
##############################################################################
###


##############################################################################
# Notes                                                                      #
##############################################################################

# - this is all written from the perspective that transformations are
#   well-implemented.

##############################################################################

# old

InstallOtherMethod(LambdaOrb, "for a D-class of a trans. semi",
[IsGreensDClass and IsGreensClassOfTransSemigp], ImageOrbit);

InstallOtherMethod(LambdaOrb, "for a D-class of a part perm semi",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup], d-> d!.o);

InstallOtherMethod(RhoOrb, "for a D-class of a trans. semi",
[IsGreensDClass and IsGreensClassOfTransSemigp], KernelOrbit);

InstallOtherMethod(RhoOrb, "for a D-class of a part perm semi",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup], d-> d!.o);

###############################################################################
# Setup - install the basic things required for specific acting semigroups    #
###############################################################################

# new for 1.0 - LambdaAct
###############################################################################

InstallMethod(LambdaAct, "for a transformation semi",
[IsTransformationSemigroup], x-> OnSets);

if IsBound(OnIntegerSetsWithPP) then 
  InstallMethod(LambdaAct, "for a partial perm semi",
  [IsPartialPermSemigroup], x-> OnIntegerSetsWithPP);
fi;

# new for 1.0! - LambdaDegree
###############################################################################

InstallMethod(LambdaDegree, "for an acting semigroup", 
[IsActingSemigroup], s-> Length(LambdaDomain(s)));

# new for 1.0! - LambdaDomain
###############################################################################

InstallMethod(LambdaDomain, "for a transformation semi",
[IsTransformationSemigroup], s-> [1..Degree(s)]*1);

InstallMethod(LambdaDomain, "for a partial perm semi",
[IsPartialPermSemigroup], s-> Points(s));

# new for 1.0! - LambdaHT
###############################################################################

InstallMethod(LambdaHT, "for an acting semi",
[IsActingSemigroup],
function(s)
return HTCreate(LambdaFunc(s)(GeneratorsOfSemigroup(s)[1]), 
rec(forflatplainlists:=true,
     hashlen:=s!.opts.hashlen.S));
end);

# new for 1.0! - LambdaFunc
###############################################################################

InstallMethod(LambdaFunc, "for a trans semi",
[IsTransformationSemigroup], x-> y-> SSortedList(y![1]));

if IsBound(RanSetPP) then
  InstallMethod(LambdaFunc, "for a partial perm",
    [IsPartialPermSemigroup], x-> RanSetPP);
fi;

# new for 1.0! - LambdaMult
###############################################################################

InstallMethod(LambdaMult, "for a transformation semi",
[IsTransformationSemigroup], s-> function(pt, f)
  return MappingPermListList(f![1]{pt}, pt);
end);

InstallMethod(LambdaMult, "for a partial perm semi",
[IsPartialPermSemigroup], s-> function(pt, f) 
  return MappingPermListList(OnIntegerSetsWithPP(pt, f), pt);
end);

# new for 1.0! - LambdaPerm
###############################################################################

InstallMethod(LambdaPerm, "for a transformation semi",
[IsTransformationSemigroup], s-> PermLeftQuoTransformationNC);

InstallMethod(LambdaPerm, "for a partial perm semi",
[IsPartialPermSemigroup], s-> function(f,g)
  local h;
  h:=f^-1*g;
  return MappingPermListList(DomPP(h), RanPP(h)); 
end);

# new for 1.0! - LambdaRank
###############################################################################

InstallMethod(LambdaRank, "for a transformation semigroup", 
[IsTransformationSemigroup], x-> Length);

InstallMethod(LambdaRank, "for a semigroup of partial perms", 
[IsPartialPermSemigroup], x-> Length);

# new for 1.0! - RhoFunc
###############################################################################

InstallMethod(RhoFunc, "for a trans semi",
[IsTransformationSemigroup], x-> CanonicalTransSameKernel);

if IsBound(DomPP) then
  InstallMethod(RhoFunc, "for a partial perm semi",
   [IsPartialPermSemigroup], x-> DomPP);
fi;

# new for 1.0! - RhoHT
###############################################################################

InstallMethod(RhoHT, "for an acting semi",
[IsActingSemigroup],
function(s)
  local x;
  x:=GeneratorsOfSemigroup(s)[1]; 
  return HTCreate(Concatenation(LambdaFunc(s)(x), RhoFunc(s)(x)),
  rec(forflatplainlists:=true,
     hashlen:=s!.opts.hashlen.S));
end);

############################################################################### 
###############################################################################

# new for 1.0! - \in - for lambda value of acting semi elt & graded lamda orbs
##############################################################################

InstallMethod(\in, "for lambda value of acting semi elt and graded lambda orbs",
[IsObject, IsGradedLambdaOrbs],
function(lamf, o)
  return not HTValue(LambdaHT(ParentAttr(o)), lamf)=fail;
end);

#CCC

# new for 1.0! - CreateLambdaOrbGS -
##############################################################################
# Usage: o = lambda orb, m = scc index, scc = a strongly connected component,
# gens = generators of semigroup, nrgens = the number of gens, 
# rep = LambdaOrbRep(o, m), lookup = OrbSCCLookup(o)[m], 
# orbitgraph = OrbitGraph(o), mults = LambdaOrbMults(o, m), 
# schutz = 

# I'm not sure this is worth it...

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

# new for 1.0! - CreateLambdaOrbMults
##############################################################################
# Usage: lambda_perm = LambdaPerm(s), gens = generators of semigroup, 
# o = lambda orb, m = scc index, scc = the actual scc.
#
# Notes: from o[i] to o[scc[m][1]]

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
  local data, ht, i, orb, graph, nr, gens, nrgens, genstoapply, graded, gradedlens, reps, reps_lookup, lambda, rho, lens, lambdaht, lambdaact, rhoht, nrrepsets, lambdaperm, lambdamult, rank, hashlen, gradingfunc, x, pos, lamx, rankx, o, scc, r, lookup, m, mults, y, rhoy, val, schutzstab, schutz, g, is_sym, len, bound, orbitgraph, f, old, p, j, k, l, n;

  data:=SemigroupData(s);
  ht:=data.ht;      # ht and orb contain existing R-class reps
  orb:=data.orbit;  
  nr:=Length(orb);
  i:=data.pos;       # points in orb in position at most i have descendants
  graph:=data.graph; # orbit graph of orbit of R-classes under left mult 
  reps:=data.reps;              # reps grouped by equal lambda and rho value
                                # HTValue(lambdarhoht, Concatenation(lambda(x),
                                # rho(x))

  reps_lookup:=data.reps_lookup;# Position(orb, reps[i][j])=reps_lookup[i][j]
                                # = HTValue(ht, reps[i][j])
  lens:=data.lens;            # Length(reps[i])=lens[i] 
  nrrepsets:=data.nrrepsets;  # nrrepsets=Length(reps)
  
  graded:=GradedLambdaOrbs(s);  # existing graded lambda orbs
  gradedlens:=graded!.lens;     # gradedlens[j]=Length(graded[j]);
  
  # generators
  gens:=GeneratorsOfSemigroup(s); 
  nrgens:=Length(gens); 
  genstoapply:=[1..nrgens];
  
  # lambda/rho
  lambda:=LambdaFunc(s);
  lambdaht:=LambdaHT(s);
  lambdaact:=LambdaAct(s);  
  lambdaperm:=LambdaPerm(s);
  lambdamult:=LambdaMult(s);
  rank:=LambdaRank(s);
  rho:=RhoFunc(s);
  rhoht:=RhoHT(s);
  
  # options for graded lambda orbs
  hashlen:=CitrusOptionsRec.hashlen.M;  
  gradingfunc := function(o,x) return [rank(x), x]; end;

  while i<=limit and i<nr do 
    i:=i+1;
    
    for j in genstoapply do 
      x:=gens[j]*orb[i][4];
      
      #check if x is already an R-class rep
      pos:=HTValue(ht, x);
      if pos<>fail then 
        graph[i][j]:=pos;
        continue; 
      fi;
      #check if lambda orb of x is already known
      lamx:=lambda(x);
      pos:=HTValue(lambdaht, lamx);
      
      if pos=fail then #new lambda orbit, new R-class
        
        #setup graded lambda orb
        rankx:=rank(lamx);
        gradedlens[rankx]:=gradedlens[rankx]+1;
        
        o:=Orb(gens, lamx, lambdaact,
          rec(
            forflatplainlists:=true,
            hashlen:=hashlen,
            schreier:=true,
            gradingfunc:=gradingfunc,
            orbitgraph:=true,
            onlygrades:=function(y, onlygradesdata)
              return y[1]=rankx and 
                HTValue(onlygradesdata, y[2])=fail;
              end,
            onlygradesdata:=lambdaht,
            storenumbers:=true,
            log:=true, 
            scc_reps:=[x], 
            data:=[rankx, gradedlens[rankx]]));
        
        SetIsGradedLambdaOrb(o, true);

        #store graded lambda orb
        graded[rankx][gradedlens[rankx]]:=o;
        
        #install points in lambdaht
        Enumerate(o, infinity);
        for y in [1..Length(o)] do 
          HTAdd(lambdaht, o[y], [rankx, gradedlens[rankx], y]);
        od;

        #     
        nrrepsets:=nrrepsets+1;
        nr:=nr+1;
        reps[nrrepsets]:=[x];
        reps_lookup[nrrepsets]:=[nr];
        lens[nrrepsets]:=1;
        HTAdd(rhoht, Concatenation(lamx, rho(x)), nrrepsets);
        x:=[s, [1, 1, 1], o, x];

      else #old lambda orbit
        o:=graded[pos[1]][pos[2]];
        
        #find the scc
        scc:=OrbSCC(o); r:=Length(scc);
        lookup:=o!.scc_lookup;
        m:=lookup[pos[3]];
        scc:=scc[m]; 

        #get the multipliers
        if not IsBound(o!.mults) then 
          o!.mults:=EmptyPlist(Length(o));
        fi;
        mults:=o!.mults;
        if not IsBound(mults[scc[1]]) then 
          CreateLambdaOrbMults(lambdamult, gens, o, m, scc);
        fi;
        
        #put lambda x in the first position in its scc
        if not pos[3]=scc[1] then 
          y:=x*mults[pos[3]];
        else
          y:=x;
        fi;

        #check if we've seen rho(y) before
        rhoy:=ShallowCopy(o[scc[1]]);
        Append(rhoy, rho(y));
        val:=HTValue(rhoht, rhoy);

        # this is what we keep if it is new
        x:=[s, [m, scc[1], pos[3]], o, y];

        if val=fail then  #new rho value
          nrrepsets:=nrrepsets+1;
          HTAdd(rhoht, rhoy, nrrepsets);
          nr:=nr+1;
          reps[nrrepsets]:=[y];
          reps_lookup[nrrepsets]:=[nr];
          lens[nrrepsets]:=1;
        else              # old rho value
          
          #get schutz gp stab chain
          if not IsBound(o!.schutzstab) then 
            o!.schutzstab:=EmptyPlist(r);
            o!.schutz:=EmptyPlist(r);
          fi;
          schutzstab:=o!.schutzstab;
          
          #create the schutz gp and stab chain if necessary
          if not IsBound(schutzstab[m]) then 
            
            #doing LambdaOrbStabChain(o, m) should do exactly the same!
            schutz:=o!.schutz;
            g:=Group(()); is_sym:=false;
            len:=rank(o[scc[1]]);

            if len<1000 then
             bound:=Factorial(len);
            else
              bound:=infinity;
            fi;

            orbitgraph:=OrbitGraph(o);
            for k in scc do
              for l in [1..nrgens] do
                if IsBound(orbitgraph[k][l]) and lookup[orbitgraph[k][l]]=m then
                  f:=lambdaperm(reps[val][1], reps[val][1]/mults[k]*
                   (gens[l]*mults[orbitgraph[k][l]]));
                  g:=ClosureGroup(g, f);
                  if Size(g)>=bound then
                    is_sym:=true;
                    break;
                  fi;
                fi;
              od;
              if is_sym then 
                break;
              fi;
            od;

            schutz[m]:=g; 

            if is_sym then
              schutzstab[m]:=true;
            elif Size(g)=1 then
              schutzstab[m]:=false;
            else
              schutzstab[m]:=StabChainImmutable(g);
            fi; 
          fi;

          #check membership in schutz gp via stab chain
          if schutzstab[m]=true then 
            graph[i][j]:=reps_lookup[val][1];
            continue;
          else
            if schutzstab[m]=false then 
              old:=false;
              for n in [1..lens[val]] do 
                if reps[val][n]=y then 
                  old:=true;
                  graph[i][j]:=reps_lookup[val][n];
                  break;
                fi;
              od;
              if old then 
                continue;
              fi;
            else
              old:=false; 
              for n in [1..lens[val]] do 
                p:=lambdaperm(reps[val][n], y);
                if SiftedPermutation(schutzstab[m], p)=() then 
                  old:=true;
                  graph[i][j]:=reps_lookup[val][n]; 
                  break;
                fi;
              od;
              if old then 
                continue;
              fi;
            fi;
            nr:=nr+1;
            reps[val][n+1]:=y;
            reps_lookup[val][n+1]:=nr;
            lens[val]:=lens[val]+1;
          fi;
        fi;
      fi;
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

  if opt then   #global
    graded:=GradedLambdaOrbs(s);
    data:=Position(graded, LambdaFunc(s)(f));
  
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
        forflatplainlists:=true, #JDM probably don't want to assume this..
        hashlen:=CitrusOptionsRec.hashlen.M,
        schreier:=true,
        gradingfunc:=gradingfunc,
        orbitgraph:=true,
        onlygrades:=onlygrades,
        onlygradesdata:=onlygradesdata,
        storenumbers:=true,
        log:=true,
        scc_reps:=[f]));
  
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
    orbits:=List([1..LambdaDegree(s)], x-> []),
    lens:=[1..LambdaDegree(s)]*0));
end);

#III

# new for 1.01! - InitSemigroupData - "for acting semi, data, and element"
#############################################################################

InstallGlobalFunction(InitSemigroupData, 
function(s, data, x)
  local lamx, pos, o, m, scc;

  lamx:=LambdaFunc(s)(x);
  pos:=HTValue(LambdaHT(s), lamx);

  if pos=fail then 
    o:=GradedLambdaOrb(s, x, true);
    pos:=[1,1,1]; #[scc index, scc[1], pos of LambdaFunc(x) in o]
  else
    o:=GradedLambdaOrbs(s)[pos[1]][pos[2]];
    m:=OrbSCCLookup(o)[pos[3]];
    scc:=o!.scc[m];
    pos:=[m, scc[1], pos[3]];
    if not pos[3]=scc[1] then 
      x:=x*LambdaOrbMults(o, m)[pos[3]];
      lamx:=o[scc[1]];
    fi;
  fi;  

  HTAdd(data.ht, x, 1);
  data.orbit:=[[s, pos, o, x]];
  data.lens[1]:=1;
  data.nrrepsets:=data.nrrepsets+1;
  data.reps[data.nrrepsets]:=[x];
  HTAdd(RhoHT(s), Concatenation(lamx, RhoFunc(s)(x)), data.nrrepsets);

  return data;
end);

# new for 1.0! - IsBound - for graded lambda orbs and pos int
##############################################################################

InstallMethod(IsBound\[\], "for graded lambda orbs and pos int",
[IsGradedLambdaOrbs, IsPosInt], 
function(o, j)
  return IsBound(o!.orbits[j]);
end);

#LLL

# new for 1.0! - LambdaOrb - "for an acting semigroup"
##############################################################################

InstallMethod(LambdaOrb, "for an acting semigroup",
[IsActingSemigroup],
function(s)

  return Orb(s, LambdaDomain(s), LambdaAct(s),
        rec(forflatplainlists:=true, schreier:=true, orbitgraph:=true,
        storenumbers:=true, log:=true, hashlen:=CitrusOptionsRec.hashlen.M,
        finished:=false, scc_reps:=[()]));
end);


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
   
  CreateLambdaOrbMults(LambdaMult(ParentAttr(o)), o!.gens, o, m, scc);
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
   o!.schutz, o!.schutzstab, LambdaPerm(ParentAttr(o))); 
  
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
   o!.schutzstab, LambdaPerm(ParentAttr(o))); 
  
  return o!.schutzstab[m];
end);
  
#PPP

# new for 1.0! - Position - "for graded lambda orbs and acting semi elt"
##############################################################################

InstallOtherMethod(Position, "for graded lambda orbs and acting semi elt",
[IsGradedLambdaOrbs, IsObject, IsZeroCyc],
function(o, lamf, n)
  return HTValue(LambdaHT(ParentAttr(o)), lamf);
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
  local gens, x, data;
  
  gens:=GeneratorsOfSemigroup(s);

  if IsTransformationSemigroup(s) then 
    x:=One(gens[1]);
  elif IsPartialPermSemigroup(s) then 
    x:=PartialPermNC(Points(gens), Points(gens));
  else
    return fail;
  fi;

  data:=rec(ht:=HTCreate(x, rec(hashlen:=s!.opts.hashlen.L)), 
     pos:=0, graph:=[EmptyPlist(Length(gens))], 
     reps:=[], reps_lookup:=[], nrrepsets:=0, orbit:=[[,,,x]], lens:=[]);

  if x in gens then 
    InitSemigroupData(s, data, x);
    if not IsMonoid(s) then 
      SetIsMonoidAsSemigroup(s, true);
    fi;
  fi;

  return data;
end);

