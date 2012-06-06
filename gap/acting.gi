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
# LambdaMult(s)(pt, f) returns a permutation taking acting in the same way as
# f^-1 on pt. This is required to produce the lambda orb mults
# (LambdaOrbMults). 

InstallMethod(LambdaMult, "for a transformation semi",
[IsTransformationSemigroup], s-> function(pt, f)
  return MappingPermListList(pt, f![1]{pt});
end);

InstallMethod(LambdaMult, "for a partial perm semi",
[IsPartialPermSemigroup], s-> function(pt, f) 
  return MappingPermListList(pt, OnIntegerTuplesWithPP(pt, f));
end);

# new for 1.0! - LambdaPerm
###############################################################################
# LambdaPerm(s) returns a permutation from two acting semigroup elements with
# equal LambdaFunc and RhoFunc. This is required to check if one of the two
# elements belongs to the schutz gp of a lambda orb.

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

# new for 1.0! - LambdaRhoHT
###############################################################################

InstallMethod(LambdaRhoHT, "for an acting semi",
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
  return not HTValue(LambdaHT(o!.semi), lamf)=fail;
end);

#CCC

#EEE

# new for 1.0! - ELM_LIST - for graded lambda orbs 
##############################################################################

InstallOtherMethod(ELM_LIST, "for graded lambda orbs, and pos int",
[IsGradedLambdaOrbs, IsPosInt], 
function(o, j)
  return o!.orbits[j];
end);

# new for 1.0! - ELM_LIST - for graded lambda orbs 
##############################################################################

InstallOtherMethod(ELM_LIST, "for acting semigp data, and pos int",
[IsSemigroupData, IsPosInt], 
function(o, nr)
  return o!.orbit[nr];
end);

# new for 1.0! - EnumerateSemigroupData - for an acting semigroup and limit
##############################################################################

#lookingfor? 

InstallGlobalFunction(EnumerateSemigroupData, 
function(arg)
  local s, limit, lookfunc, looking, data, ht, orb, nr, i, graph, reps, repslookup, repslens, lenreps, schreierpos, schreiergen, schreiermult, gens, nrgens, genstoapply, lambda, lambdaht, lambdaact, lambdaperm, lambdamult, rank, rho, lambdarhoht, o, scc, r, lookup, x, pos, lamx, m, mults, y, rhoy, val, schutz, old, p, graded, gradedlens, hashlen, gradingfunc, rankx, f, schutzstab, g, is_sym, len, bound, orbitgraph, j, n, k, l;

  s:=arg[1]; limit:=arg[2];

  # are we looking for something?
  if IsBound(arg[3]) then 
    lookfunc:=arg[3];
    looking:=true;
  else
    looking:=false;
  fi;

  data:=SemigroupData(s);
  ht:=data!.ht;       # ht and orb contain existing R-class reps
  orb:=data!.orbit;  
  nr:=Length(orb);
  i:=data!.pos;       # points in orb in position at most i have descendants
  graph:=data!.graph; # orbit graph of orbit of R-classes under left mult 
  reps:=data!.reps;   # reps grouped by equal lambda and rho value
                      # HTValue(lambdarhoht, Concatenation(lambda(x),
                      # rho(x))

  repslookup:=data!.repslookup; # Position(orb, reps[i][j])=repslookup[i][j]
                                # = HTValue(ht, reps[i][j])
  repslens:=data!.repslens;     # Length(reps[i])=repslens[i] 
  lenreps:=data!.lenreps;       # lenreps=Length(reps)

  # schreier

  schreierpos:=data!.schreierpos;
  schreiergen:=data!.schreiergen;
  schreiermult:=data!.schreiermult;

  # generators
  gens:=Generators(s); 
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
  lambdarhoht:=LambdaRhoHT(s);

  if data!.graded=false then # decided when data is created in InitSemigroupData
    
    o:=LambdaOrb(s);
    Enumerate(o, infinity);
    scc:=OrbSCC(o); r:=Length(scc);
    lookup:=o!.scc_lookup;
    
    while nr<=limit and i<nr do 
      i:=i+1;
      
      for j in genstoapply do #JDM
        x:=gens[j]*orb[i][4];
       
        #check if x is already an R-class rep
        pos:=HTValue(ht, x);
        if pos<>fail then 
          graph[i][j]:=pos;
          continue; 
        fi;
        
        lamx:=lambda(x);
        pos:=Position(o, lamx);
        #find the scc
        m:=lookup[pos];

        #get the multipliers
        mults:=LambdaOrbMults(o, m);         

        #put lambda x in the first position in its scc
        if not pos=scc[m][1] then 
          y:=x*mults[pos];
        else
          y:=x;
          pos:=fail;
        fi;

        #check if we've seen rho(y) before
        rhoy:=ShallowCopy(o[scc[m][1]]);
        Append(rhoy, rho(y));
        val:=HTValue(lambdarhoht, rhoy);

        # this is what we keep if it is new
        x:=[s, [m, scc[m][1]], o, y, nr+1];
        # semigroup, lambda orb data, lambda orb, rep, index

        if val=fail then  #new rho value, and hence new R-rep
          lenreps:=lenreps+1;
          HTAdd(lambdarhoht, rhoy, lenreps);
          nr:=nr+1;
          reps[lenreps]:=[y];
          repslookup[lenreps]:=[nr];
          repslens[lenreps]:=1;
        else              # old rho value
          schutz:=LambdaOrbStabChain(o, m);
          
          #check membership in schutz gp via stab chain
          
          if schutz=true then # schutz gp is symmetric group
            graph[i][j]:=repslookup[val][1];
            continue;
          else
            if schutz=false then # schutz gp is trivial
              old:=false;
              for n in [1..repslens[val]] do 
                if reps[val][n]=y then 
                  old:=true;
                  graph[i][j]:=repslookup[val][n];
                  break;
                fi;
              od;
              if old then 
                continue;
              fi;
            else # schutz gp neither trivial nor symmetric group
              old:=false; 
              for n in [1..repslens[val]] do 
                p:=lambdaperm(reps[val][n], y);
                if SiftedPermutation(schutz, p)=() then 
                  old:=true;
                  graph[i][j]:=repslookup[val][n]; 
                  break;
                fi;
              od;
              if old then 
                continue;
              fi;
            fi;
            nr:=nr+1;
            reps[val][n+1]:=y;
            repslookup[val][n+1]:=nr;
            repslens[val]:=repslens[val]+1;
          fi;
        fi;
        orb[nr]:=x;
        schreierpos[nr]:=i; # orb[nr] is obtained from orb[i]
        schreiergen[nr]:=j; # by multiplying by gens[j]
        schreiermult[nr]:=pos; # and ends up in position <pos> of 
                               # its lambda orb
        HTAdd(ht, y, nr);
        graph[nr]:=EmptyPlist(nrgens);
        graph[i][j]:= nr;
        if looking then 
          if lookfunc(data, y) then 
            data!.pos:=i;
            data!.found:=nr;
            data!.lenreps:=lenreps;
            return data;
          fi;
        fi;
      od;
    od;
  else #JDM
    
    graded:=GradedLambdaOrbs(s);  # existing graded lambda orbs
    gradedlens:=graded!.lens;     # gradedlens[j]=Length(graded[j]);
       
    # options for graded lambda orbs
    hashlen:=CitrusOptionsRec.hashlen.M;  
    gradingfunc := function(o,x) return [rank(x)+1, x]; end;

    while nr<=limit and i<nr do 
      i:=i+1;
      
      for j in genstoapply do #JDM
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
          rankx:=rank(lamx)+1;
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
          
          #install points from new lambda orb in lambdaht
          Enumerate(o, infinity);
          for y in [1..Length(o)] do 
            HTAdd(lambdaht, o[y], [rankx, gradedlens[rankx], y]);
          od;

          #     
          lenreps:=lenreps+1;
          nr:=nr+1;
          reps[lenreps]:=[x];
          repslookup[lenreps]:=[nr];
          repslens[lenreps]:=1;
          HTAdd(lambdarhoht, Concatenation(lamx, rho(x)), lenreps);
          x:=[s, [1, 1], o, x, nr];
          pos:=true;
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
          if not IsBound(mults[scc[1]]) then ;
            for k in scc do
              f:=EvaluateWord(gens, TraceSchreierTreeOfSCCBack(o, m, k));
              mults[k]:=lambdamult(o[k], f);
            od; 
          fi;
          
          #put lambda x in the first position in its scc
          if not pos[3]=scc[1] then 
            y:=x*mults[pos[3]];
            pos:=pos[3];
          else
            y:=x; 
          fi;

          #check if we've seen rho(y) before
          rhoy:=ShallowCopy(o[scc[1]]);
          Append(rhoy, rho(y));
          val:=HTValue(lambdarhoht, rhoy);

          # this is what we keep if it is new
          x:=[s, [m, scc[1]], o, y, nr+1];

          if val=fail then  #new rho value
            lenreps:=lenreps+1;
            HTAdd(lambdarhoht, rhoy, lenreps);
            nr:=nr+1;
            reps[lenreps]:=[y];
            repslookup[lenreps]:=[nr];
            repslens[lenreps]:=1;
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
                  if IsBound(orbitgraph[k][l]) and lookup[orbitgraph[k][l]]=m
                    then
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
            
            if schutzstab[m]=true then # schutz gp is symmetric group
              graph[i][j]:=repslookup[val][1];
              continue;
            else
              if schutzstab[m]=false then # schutz gp is trivial
                old:=false;
                for n in [1..repslens[val]] do 
                  if reps[val][n]=y then 
                    old:=true;
                    graph[i][j]:=repslookup[val][n];
                    break;
                  fi;
                od;
                if old then 
                  continue;
                fi;
              else # schutz gp neither trivial nor symmetric group
                old:=false; 
                for n in [1..repslens[val]] do 
                  p:=lambdaperm(reps[val][n], y);
                  if SiftedPermutation(schutzstab[m], p)=() then 
                    old:=true;
                    graph[i][j]:=repslookup[val][n]; 
                    break;
                  fi;
                od;
                if old then 
                  continue;
                fi;
              fi;
              nr:=nr+1;
              reps[val][n+1]:=y;
              repslookup[val][n+1]:=nr;
              repslens[val]:=repslens[val]+1;
            fi;
          fi;
        fi;
        orb[nr]:=x;
        schreierpos[nr]:=i; # orb[nr] is obtained from orb[i]
        schreiergen[nr]:=j; # by multiplying by gens[j]
        schreiermult[nr]:=pos; # and then by multiplying by o!.mults[pos]
                               # pos = fail if gens[j]*orb[nr] is the R-rep
                               # (its lambda value is in scc[1])
        HTAdd(ht, x[4], nr);
        graph[nr]:=EmptyPlist(nrgens);
        graph[i][j]:= nr;
      od;
    od;
  fi;
  data!.pos:=i;
  data!.lenreps:=lenreps;

  return true;
end);

#JDM

#FFF

#GGG

# new for 1.0! - GradedLambdaOrb - "for an acting semigroup and elt"
##############################################################################

InstallGlobalFunction(GradedLambdaOrb,
function(s, f, opt)
  local graded, pos, gradingfunc, onlygrades, onlygradesdata, o, j, k, l;

  if opt then   #global
    graded:=GradedLambdaOrbs(s);
    pos:=HTValue(LambdaHT(s), LambdaFunc(s)(f));
  
    if pos<>fail then 
      return graded[pos[1]][pos[2]];
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
        semi:=s,
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
    j:=LambdaRank(s)(LambdaFunc(s)(f))+1;
    # the +1 is essential as the rank can be 0
    k:=graded!.lens[j]+1;
    graded[j][k]:=o;
    Enumerate(o);
    for l in [1..Length(o)] do
      HTAdd(onlygradesdata, o[l], [j,k,l]);
    od;
    o!.data:=[j,k]; 
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
    orbits:=List([1..LambdaDegree(s)+1], x-> []), 
    lens:=[1..LambdaDegree(s)+1]*0, semi:=s));
end);

#III

# new for 1.0! - InitSemigroupData - "for acting semi, data, and element"
#############################################################################
# - maybe rewrite so that the generators are really added here, rather than one.

InstallGlobalFunction(InitSemigroupData, 
function(s, data, x)
  local lamx, pos, o, m, scc;

  # decide if we are using graded orbits or not.
  if (not HasGradedLambdaOrbs(s)) #or (HasLambdaHT(s) and LambdaHT(s)!.nr<20) 
   or (HasLambdaOrb(s) and HasGradedLambdaOrbs(s) and
    Length(LambdaOrb(s))>=LambdaHT(s)!.nr) then 
    data!.graded:=false;
  else
    data!.graded:=true;
  fi;
  
  # install first point if we are in a monoid
  if not x=false then 
    
    # find the orbit containing LambdaFunc(s)(x)...
    lamx:=LambdaFunc(s)(x);
    if not data!.graded then 
      o:=LambdaOrb(s);
      pos:=[1, 1, 1];
    else
      pos:=HTValue(LambdaHT(s), lamx);
      if pos=fail then 
        o:=GradedLambdaOrb(s, x, true);
        pos:=[1, 1, 1]; #[scc index, scc[1], pos of LambdaFunc(x) in o]
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
    fi;
    
    # install the info about x in data
    HTAdd(data!.ht, x, 1);
    data!.orbit:=[[s, pos, o, x]];
    data!.repslens[1]:=1;
    data!.lenreps:=data!.lenreps+1;
    data!.reps[data!.lenreps]:=[x];
    data!.repslookup[1]:=[1];
    HTAdd(LambdaRhoHT(s), Concatenation(lamx, RhoFunc(s)(x)), data!.lenreps);
  fi;

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
  local x;
  
  if IsTransformationSemigroup(s) then
    x:=One(Generators(s)[1]);
  elif IsPartialPermSemigroup(s) then 
    x:=PartialPermNC(Points(Generators(s)), Points(Generators(s)));
  fi;     

  return Orb(s, LambdaDomain(s), LambdaAct(s),
        rec(forflatplainlists:=true, schreier:=true, orbitgraph:=true,
        storenumbers:=true, log:=true, hashlen:=CitrusOptionsRec.hashlen.M,
        finished:=false, scc_reps:=[x], semi:=s));
end);

# new for 1.0! - LambdaOrbMults - "for a lambda orb and scc index"
##############################################################################

InstallGlobalFunction(LambdaOrbMults, 
    function(o, m) 
    local scc, s, mults, gens, lambdamult, f, i;
 
  scc:=OrbSCC(o)[m];

  if IsBound(o!.mults) then  
    if IsBound(o!.mults[scc[1]]) then 
      return o!.mults;
    fi;
  else
    o!.mults:=EmptyPlist(Length(o)); 
  fi; 
  
  s:=o!.semi;
  mults:=o!.mults;
  gens:=Generators(s);  
  lambdamult:=LambdaMult(s);

  for i in scc do
    f:=EvaluateWord(gens, TraceSchreierTreeOfSCCBack(o, m, i));
    mults[i]:=lambdamult(o[i], f);
  od;
 
  return mults;
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
  local s, gens, nrgens, scc, lookup, orbitgraph, lambdaperm, rep, mults, slp,
  lenslp, len, bound, g, is_sym, f, h, k, l;
  
  if IsBound(o!.schutz) then 
    if IsBound(o!.schutz[m]) then 
      return o!.schutz[m];
    fi;
  else
    o!.schutz:=EmptyPlist(Length(OrbSCC(o))); 
    o!.schutzstab:=EmptyPlist(Length(OrbSCC(o)));
    o!.slp:=EmptyPlist(Length(OrbSCC(o)));
  fi;

  s:=o!.semi;
  gens:=Generators(s); 
  nrgens:=Length(gens);
  scc:=OrbSCC(o)[m];      
  lookup:=o!.scc_lookup;
  orbitgraph:=OrbitGraph(o);
  lambdaperm:=LambdaPerm(s);
  rep:=LambdaOrbRep(o, m);
  mults:=LambdaOrbMults(o, m);
  slp:=[]; lenslp:=0;

  len:=LambdaRank(s)(o[scc[1]]);

  if len<1000 then
    bound:=Factorial(len);
  else
    bound:=infinity;
  fi;

  g:=Group(()); is_sym:=false;
  
  for k in scc do
    for l in [1..nrgens] do
      if IsBound(orbitgraph[k][l]) and lookup[orbitgraph[k][l]]=m then
        f:=lambdaperm(rep, rep/mults[k]*(gens[l]*mults[orbitgraph[k][l]]));
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

  return o!.schutz[m];
end);

# new for 1.0! - LambdaOrbSLP - "for a lambda orb and scc index"
##############################################################################
# returns an slp for the generators of LambdaOrbSchutzGp(o, m) in the
# generators of the semigroup.

InstallGlobalFunction(LambdaOrbSLP, 
function(o, m)
  local g, slp, nr, r, graph, slp_lines, word, i, j;

  if IsBound(o!.slp) then 
    if IsBound(o!.slp[m]) and IsStraightLineProgram(o!.slp[m]) then 
      return o!.slp[m];
    fi;
  fi;
 
  g:=LambdaOrbSchutzGp(o, m);
  slp:=o!.slp[m];
  nr:=Length(slp);
  slp_lines:=EmptyPlist(nr);
  r:=Length(Generators(o!.semi));
  
  if nr<>0 then
    graph:=OrbitGraph(o);
    for i in [1..nr] do 
      word:=Concatenation(TraceSchreierTreeOfSCCForward(o, m, slp[i][1]),
       [slp[i][2]], 
        TraceSchreierTreeOfSCCBack(o, m, graph[slp[i][1]][slp[i][2]]));
      slp_lines[i]:=[];
      for j in [1..Length(word)] do 
        slp_lines[i][2*j-1]:=word[j];
        slp_lines[i][2*j]:=1;
      od;
    od;
  fi;

  return StraightLineProgram([slp_lines], r);
end);

# new for 1.0! - LambdaOrbStabChain - "for a lambda orb and scc index"
##############################################################################

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

# new for 1.0! - Length - for semigroup data of acting semigroup
##############################################################################

InstallOtherMethod(Length, "for semigroup data of acting semigroup",
[IsSemigroupData], x-> Length(x!.orbit));

#PPP

# new for 1.0! - Position - "for graded lambda orbs and acting semi elt"
##############################################################################

InstallOtherMethod(Position, "for graded lambda orbs and acting semi elt",
[IsGradedLambdaOrbs, IsObject, IsZeroCyc],
function(o, lamf, n)
  return HTValue(LambdaHT(o!.semi), lamf);
end);

# new for 1.0! - Position - "for acting semigroup data and acting elt"
##############################################################################
# returns the index of the representative of the R-class containing x in the
# parent of data. 

InstallOtherMethod(Position, "for acting semigroup data and acting elt",
[IsSemigroupData, IsObject, IsZeroCyc],
function(data, x, n)
  local val, s, o, l, m, scc, schutz, repslookup, mults, y, reps, repslens, lambdaperm;

  val:=HTValue(data!.ht, x);

  if val<>fail then 
    return val;
  fi;

  s:=data!.semi;

  if IsBound(data!.graded) and data!.graded then 
    o:=GradedLambdaOrb(s, x, true);
  else
    o:=LambdaOrb(s);
  fi; 

  Enumerate(o);
  l:=Position(o, LambdaFunc(s)(x));
  m:=OrbSCCLookup(o)[l];
  scc:=OrbSCC(o);

  val:=HTValue(LambdaRhoHT(s), Concatenation(o[scc[m][1]], RhoFunc(s)(x)));
  if val=fail then 
    return fail;
  fi;

  schutz:=LambdaOrbStabChain(o, m);
  repslookup:=data!.repslookup;

  if schutz=true then 
    return repslookup[val][1];
  fi;
 
  if not l=scc[m][1] then 
    mults:=LambdaOrbMults(o, m);
    y:=x*mults[l];
  else
    y:=x;
  fi; 

  reps:=data!.reps; repslens:=data!.repslens;

  if schutz=false then 
    for n in [1..repslens[val]] do 
      if reps[val][n]=y then 
        return repslookup[val][n];
      fi;
    od;
  else
    lambdaperm:=LambdaPerm(s);
    for n in [1..repslens[val]] do 
      if SiftedPermutation(schutz, lambdaperm(reps[val][n], y))=() then 
        return repslookup[val][n];
      fi;
    od;
  fi; 
  return fail;
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
  
  gens:=Generators(s);

  if IsTransformationSemigroup(s) then 
    x:=One(gens[1]);
  elif IsPartialPermSemigroup(s) then 
    x:=PartialPermNC(Points(gens), Points(gens));
  else
    return fail;
  fi;

  data:=rec(ht:=HTCreate(x, rec(hashlen:=s!.opts.hashlen.L)), 
     pos:=0, graph:=[EmptyPlist(Length(gens))], 
     reps:=[], repslookup:=[], lenreps:=0, orbit:=[[,,,x]], repslens:=[], 
     schreierpos:=[fail], schreiergen:=[fail], schreiermult:=[fail], 
     semi:=s);
  
  Objectify(NewType(FamilyObj(s), IsSemigroupData), data);

  if IsMonoid(s) or x in gens then 
    InitSemigroupData(s, data, x);
    if not IsMonoid(s) then 
      SetIsMonoidAsSemigroup(s, true);
    fi;
  else
    InitSemigroupData(s, data, false);
    SetIsMonoidAsSemigroup(s, false);
  fi;

  return data;
end);

# new for 1.0! - SemigroupEltSLP - "for an acting semigroup and acting elt"
##############################################################################
#JDM this is not really working due to the slps for group elements containing
#inverses.

InstallMethod(SemigroupEltSLP, "for an acting semigroup and acting elt",
[IsActingSemigroup, IsActingElt],
function(s, x)
  local data, nr, gens, zip, o, m, scc, l, mult, y, p, schutz, stab, slpstrong,
   slp;

  Info(InfoWarning, 1, "this does not always return the correct answer!");

  # suppose that the data is fully enumerated...
  data:=SemigroupData(s);
  #EnumerateSemigroupData(s, lookingfor:=??)
  
  nr:=Position(data, x); 
  gens:=Generators(s);
  
  zip:=function(list)
    local len, out, i;

    len:=Length(list);
    out:=EmptyPlist(2*len);
    for i in [1..len] do 
      out[2*i-1]:=list[i];
      out[2*i]:=1;
    od;
    return out;
  end;

  # find group element
  o:=data[nr][3]; 
  m:=data[nr][2][1];
  scc:=OrbSCC(o);
  l:=Position(o, LambdaFunc(s)(x)); 
  mult:=TraceSchreierTreeOfSCCForward(o, m, l);

  y:=x*MappingPermListList(o[scc[m][1]], OnTuples(o[scc[m][1]], 
   EvaluateWord(gens, mult)))^-1;

  p:=LambdaPerm(s)(data[nr][4], y);
  
  if p<>() then
    schutz:=GroupWithMemory(LambdaOrbSchutzGp(o, m));
    stab:=StabilizerChain(schutz); 
    slpstrong := SLPOfElms(StrongGenerators(stab));
    ForgetMemory(stab);
    
    # slp for p in terms of strong generators
    slp:=SiftGroupElementSLP(stab, p).slp;
    
    # slp for p in terms original schutz gp generators
    slp:=CompositionOfStraightLinePrograms(slp, slpstrong);
    
    # slp for schutz gp generators in terms of semigp generators
    slp:=CompositionOfStraightLinePrograms(slp, LambdaOrbSLP(o, m));
  fi;

  # find word equaling rep of R-class of x
  if IsBound(slp) then 
    slp:=ProductOfStraightLinePrograms(
     StraightLineProgram([zip(TraceSchreierTreeForward(data, nr))],
      Length(gens)), slp);
  else
    slp:=StraightLineProgram(
     [zip(TraceSchreierTreeForward(data, nr))], Length(gens));
  fi;

  # slp for multiplier
  if mult<>[] then 
    return ProductOfStraightLinePrograms(slp, 
     StraightLineProgram([zip(mult)], Length(gens)));
  fi;
  return slp;
end);

#TTT

# new for 1.0! - TraceSchreierTreeForward - "for semi data and pos int"
##############################################################################
# returns a word in the generators of the parent of <data> equal to the R-class
# representative store in <data!.orbit[pos]>.

# Notes: the code is more complicated than you might think since the R-class
# reps are obtained from earlier reps by left multiplication but the orbit
# multipliers correspond to right multiplication.

InstallOtherMethod(TraceSchreierTreeForward, "for semigp data and pos int",
[IsSemigroupData, IsPosInt],
function(data, pos)
  local word, word2, schreiergen, schreierpos, schreiermult, orb, o, m;
  
  word:=[];  # the word obtained by tracing schreierpos and schreiergen
             # (left multiplication)
  word2:=[]; # the word corresponding to multipliers applied (if any)
             # (right multiplication)
  
  schreiergen:=data!.schreiergen; 
  schreierpos:=data!.schreierpos;
  schreiermult:=data!.schreiermult;

  orb:=data!.orbit;

  while pos > 1 do 
    Add(word, schreiergen[pos]);
    if schreiermult[pos]<>fail then # a multiplier was applied!
      o:=orb[pos][3];               # the lambda orb
      m:=orb[pos][2][1];            # the scc
      Append(word2, 
       Reversed(TraceSchreierTreeOfSCCBack(o, m, schreiermult[pos])));
    fi;
    pos:=schreierpos[pos];
  od;

  Append(word, Reversed(word2));
  return word;
end);

#VVV

# new for 1.0! - ViewObj - "for semigroup data"
##############################################################################

InstallMethod(ViewObj, [IsSemigroupData],
function(data)
  Print("<semigroup data: ", Length(data!.orbit), " reps, ",
  Length(data!.reps), " lambda-rho values>");
  return;
end);

#EOF
