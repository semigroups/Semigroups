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

# for convenience

InstallOtherMethod(LambdaOrb, "for a Green's class of an acting semi",
[IsActingSemigroupGreensClass], x-> x!.o);

###############################################################################
# Setup - install the basic things required for specific acting semigroups    #
###############################################################################

# new for 1.0 - LambdaAct and RhoAct
###############################################################################

InstallMethod(LambdaAct, "for a transformation semi",
[IsTransformationSemigroup], x-> OnIntegerSetsWithT);

InstallMethod(RhoAct, "for a transformation semi",
[IsTransformationSemigroup], x-> OnKerT);

if IsBound(OnIntegerSetsWithPP) then 
  InstallMethod(LambdaAct, "for a partial perm semi",
  [IsPartialPermSemigroup], x-> OnIntegerSetsWithPP);
  
  InstallMethod(RhoAct, "for a partial perm semi",
  [IsPartialPermSemigroup], 
  function(s)
    return 
      function(set, f) 
        return OnIntegerSetsWithPP(set, f^-1);
      end;
  end);
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
[IsPartialPermSemigroup], Points);

InstallMethod(RhoDomain, "for a transformation semi",
[IsTransformationSemigroup], s-> [1..Degree(s)]*1);

InstallMethod(RhoDomain, "for a partial perm semi",
[IsPartialPermSemigroup], Points);

# new for 1.0! - LambdaHT
###############################################################################

InstallMethod(LambdaHT, "for an acting semi",
[IsActingSemigroup],
function(s)
return HTCreate(LambdaFunc(s)(GeneratorsOfSemigroup(s)[1]), 
 rec(forflatplainlists:=true, hashlen:=s!.opts.hashlen.S));
end);

# new for 1.0! - RhoHT 
###############################################################################

InstallMethod(RhoHT, "for an acting semi",
[IsActingSemigroup],
function(s)
return HTCreate(RhoFunc(s)(GeneratorsOfSemigroup(s)[1]), 
 rec(forflatplainlists:=true, hashlen:=s!.opts.hashlen.S));
end);


# new for 1.0! - LambdaFunc
###############################################################################

InstallMethod(LambdaFunc, "for a trans semi",
[IsTransformationSemigroup], x-> RanSetT);

if IsBound(RanSetPP) then
  InstallMethod(LambdaFunc, "for a partial perm",
    [IsPartialPermSemigroup], x-> RanSetPP);
fi;

# new for 1.0! - LambdaMult
###############################################################################
# LambdaMult(s)(pt, f) returns a permutation acting in the same way as
# f^-1 on pt. This is required to produce the lambda orb mults
# (LambdaOrbMults). 

InstallMethod(LambdaMult, "for a transformation semi",
[IsTransformationSemigroup], s-> function(pt, f)
  return MappingPermListList(pt, OnIntegerTuplesWithT(pt, f));
end);

InstallMethod(LambdaMult, "for a partial perm semi",
[IsPartialPermSemigroup], s-> function(pt, f) 
  return MappingPermListList(pt, OnIntegerTuplesWithPP(pt, f));
end);

# new for 1.0! - RhoMult 
###############################################################################
#JDM c method for this!

InstallMethod(RhoMult, "for a transformation semi",
[IsTransformationSemigroup], s-> 
  function(ker, f)
    local g, n, m, lookup, i, j;
  
    g:=ker{RanT(f)};
    n:=f[1]; m:=MaximumList(ker);
    lookup:=EmptyPlist(n);
    
    i:=0; j:=0;
    repeat 
      i:=i+1;
      if not IsBound(lookup[g[i]]) then 
        lookup[g[i]]:=i;
        j:=j+1;
      fi;
    until j=m;
    return TransformationNC(List([1..n], i-> lookup[ker[i]]));
  end);

InstallMethod(RhoMult, "for a partial perm semi",
[IsPartialPermSemigroup], s-> f-> f^-1); 

# new for 1.0! - LambdaPerm
###############################################################################
# LambdaPerm(s) returns a permutation from two acting semigroup elements with
# equal LambdaFunc and RhoFunc. This is required to check if one of the two
# elements belongs to the schutz gp of a lambda orb.

InstallMethod(LambdaPerm, "for a transformation semi",
[IsTransformationSemigroup], s-> PermLeftQuoTransformationNC);

#JDM c method for this!
InstallMethod(LambdaPerm, "for a partial perm semi",
[IsPartialPermSemigroup], s-> function(f,g)
  local h;
  h:=f^-1*g;
  return MappingPermListList(DomPP(h), RanPP(h)); 
end);

# new for 1.0! - LambdaRank and RhoRank
###############################################################################

InstallMethod(LambdaRank, "for a transformation semigroup", 
[IsTransformationSemigroup], x-> Length);

InstallMethod(RhoRank, "for a transformation semigroup", 
[IsTransformationSemigroup], x-> MaximumList);

InstallMethod(LambdaRank, "for a semigroup of partial perms", 
[IsPartialPermSemigroup], x-> Length);

InstallMethod(RhoRank, "for a semigroup of partial perms", 
[IsPartialPermSemigroup], x-> Length);

# new for 1.0! - RhoFunc
###############################################################################

InstallMethod(RhoFunc, "for a trans semi",
[IsTransformationSemigroup], x-> KerT);

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

# new for 1.0! - IdempotentLambdaRhoTester - "for a trans semigp"
##############################################################################
#JDM this should be revised.

InstallMethod(IdempotentLambdaRhoTester, "for a trans semigp", 
[IsTransformationSemigroup], s-> function(x, y) 
return IsInjectiveTransOnList(y, x); end);

# new for 1.0! - IdempotentLambdaRhoTester - "for a partial perm semigp"
##############################################################################

InstallMethod(IdempotentLambdaRhoTester, "for a partial perm semigp", 
[IsPartialPermSemigroup], s-> EQ);

# new for 1.0! - IdempotentLambdaRhoCreator - "for a trans semigp"
##############################################################################
#JDM we should update/replace IdempotentNC.

InstallMethod(IdempotentLambdaRhoCreator, "for a trans semigp",
[IsTransformationSemigroup], s-> 
function(x,y)
return IdempotentNC(y,x); end);

# new for 1.0! - IdempotentLambdaRhoCreator - "for a partial perm semigp"
##############################################################################

InstallMethod(IdempotentLambdaRhoCreator, "for a partial perm semigp",
[IsPartialPermSemigroup], s-> PartialPermNC);

############################################################################### 
###############################################################################

# new for 1.0! - \in - for lambda value of acting semi elt & graded lamda orbs
##############################################################################

InstallMethod(\in, "for lambda value of acting semi elt and graded lambda orbs",
[IsObject, IsGradedLambdaOrbs],
function(lamf, o)
  return not HTValue(LambdaHT(o!.semi), lamf)=fail;
end);

# new for 1.0! - \in - for rho value of acting semi elt & graded rho orbs
##############################################################################

InstallMethod(\in, "for rho value of acting semi elt and graded rho orbs",
[IsObject, IsGradedLambdaOrbs],
function(rho, o)
  return not HTValue(RhoHT(o!.semi), rho)=fail;
end);

# new for 1.0! - \in - for acting semi elt and semigroup data
##############################################################################
# expand?

InstallMethod(\in, "for acting semi elt and semigroup data",
[IsActingElt, IsSemigroupData],
function(f, data)
  return not Position(data, f)=fail;
end);

# new for 1.0! - \in - for an acting elt and acting semigroup
##############################################################################
# JDM remove the 100 from below when the \in method for transformation
# semigroup is removed. Insert 100 below to use this method in preference to
# the other \in method, doing this messes everything up in the old set up. 

InstallMethod(\in, "for an acting elt and acting semigroup",
[IsActingElt, IsActingSemigroup], 100,
function(f, s)
  local data, len, ht, val, lambda, o, l, lookfunc, m, scc, lambdarho, schutz, g, reps, repslens, lambdaperm, n, max, found;
  
  if not ElementsFamily(FamilyObj(s))=FamilyObj(f) then 
    Error("the element and semigroup are not of the same type,");
    return;
  fi;

  if HasAsSSortedList(s) then 
    return f in AsSSortedList(s); 
  fi;

  #JDM this doesn't work for semigroups of partial perms...
  #if Degree(f)<>Degree(s) then 
  #  Info(InfoCitrus, 2, "element and semigroup have different degrees.");
  #  return false;       
  #fi;

  if not (IsMonoid(s) and IsOne(f)) and 
   Rank(f) > MaximumList(List(Generators(s), Rank)) then
    Info(InfoCitrus, 2, "element has larger rank than any element of ",
     "semigroup.");
    return false;
  fi;

  if HasMinimalIdeal(s) and 
   Rank(f) < Rank(Representative(MinimalIdeal(s))) then
    Info(InfoCitrus, 2, "element has smaller rank than any element of ",
     "semigroup.");
    return false;
  fi;  

  data:=SemigroupData(s);
  len:=Length(data!.orbit);  
  ht:=data!.ht;

  # check if f is an existing R-rep
  val:=HTValue(ht, f);

  if val<>fail then 
    return true;
  fi;

  lambda:=LambdaFunc(s)(f);

  # look for lambda!
  if not data!.graded then 
    o:=LambdaOrb(s);
    l:=Position(o, lambda);
    
    if l=fail then 
      if IsClosed(o) then 
        return false;
      fi;
      o!.looking:=true; o!.lookingfor:=function(o, x) return x=lambda; end;
      o!.lookfunc:=o!.lookingfor;
      Enumerate(o);
      l:=PositionOfFound(o);
      o!.found:=false; o!.looking:=false; 
      Unbind(o!.lookingfor); Unbind(o!.lookfunc);
      if l=false then 
        return false;
      fi;
    fi;
  else 
    o:=GradedLambdaOrbs(s);
    l:=HTValue(LambdaHT(s), lambda);
    
    if l=fail then 
      if IsClosed(data) then 
        return false;
      fi;
      lookfunc:=function(data, x) return LambdaFunc(s)(x)=lambda; end;
      # lookahead?
      data:=Enumerate(data, infinity, lookfunc);
      l:=data!.found;
      if l=false then 
        return false;
      fi;
    fi;
  fi;
  
  # the only case when l is found but f not in s.
  if l=1 and not IsMonoidAsSemigroup(s) then 
    return false;
  fi;

  # strongly connected component of lambda, the disadvantage in the case that
  # data!.graded=false is that the next line enumerates the whole of o.
  m:=OrbSCCLookup(o)[l];
  scc:=OrbSCC(o);

  # check if lambdarho is already known
  lambdarho:=ShallowCopy(o[scc[m][1]]);
  Append(lambdarho, RhoFunc(s)(f));
  val:=HTValue(LambdaRhoHT(s), lambdarho);

  lookfunc:=function(data, x) 
    return Concatenation(LambdaFunc(s)(x), RhoFunc(s)(x))=lambdarho;
  end;
  
  # if lambdarho is not already known, then look for it
  if val=fail then 
    data:=Enumerate(data, infinity, lookfunc);
    val:=data!.found; # position in data!.orbit 

    # lambdarho not found, so f not in s
    if val=false then 
      return false;
    fi;
    val:=data!.orblookup1[val]; 
    # the index of the list of reps with same
    # lambdarho value as f. 
    # = HTValue(LambdaRhoHT(s), lambdarho);
  fi;

  schutz:=LambdaOrbStabChain(o, m);

  # if the schutz gp is the symmetric group, then f in s!
  if schutz=true then 
    return true;
  fi;

  # make sure lambda of f is in the first place of its scc
  if l<>scc[m][1] then 
    g:=f*LambdaOrbMults(o, m)[l];
  else
    g:=f;
  fi;

  # check if anything changed
  if len<Length(data!.orbit) or l<>scc[m][1] then 

    # check again if g is an R-class rep.
    if HTValue(ht, g)<>fail then
      return true;
    fi;
  fi;

  reps:=data!.reps; repslens:=data!.repslens;
  
  # if schutz is false, then g has to be an R-rep which it is not...
  if schutz<>false then 

    # check if f already corresponds to an element of reps[val]
    lambdaperm:=LambdaPerm(s);
    for n in [1..repslens[val]] do 
      if SiftedPermutation(schutz, lambdaperm(reps[val][n], g))=() then
        return true;
      fi;
    od;
  fi; 
  
  # enumerate until we find f or the number of elts in reps[val] exceeds max
  max:=Factorial(LambdaRank(s)(lambda))/Size(LambdaOrbSchutzGp(o, m));

  if repslens[val]<max then 
    if schutz=false then 
      repeat 

        # look for more R-reps with same lambda-rho value
        data:=Enumerate(data, infinity, lookfunc);
        found:=data!.found;
        if found<>false then 
          val:=HTValue(ht, g);
          if val<>fail then 
            return true;
          fi;
        fi;
      until found=false or repslens[val]>=max;
    else 
      repeat
        
        # look for more R-reps with same lambda-rho value
        data:=Enumerate(data, infinity, lookfunc);
        found:=data!.found;
        if found<>false then 
          reps:=data!.reps; repslens:=data!.repslens;
          for m in [n+1..repslens[val]] do 
            if SiftedPermutation(schutz, lambdaperm(reps[val][m], g))=() then 
              return true;
            fi;
          od;
          n:=repslens[val];
        fi;
      until found=false or repslens[val]>=max;
    fi;
  fi;

  return false;
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

# new for 1.0! - ELM_LIST - for graded rho orbs 
##############################################################################

InstallOtherMethod(ELM_LIST, "for graded rho orbs, and pos int",
[IsGradedRhoOrbs, IsPosInt], 
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

# new for 1.0! - Enumerate - for an acting semigroup data
##############################################################################

InstallOtherMethod(Enumerate, "for an acting semigroup data", 
[IsSemigroupData],
function(data)
  return Enumerate(data, infinity, ReturnFalse);
end);

# new for 1.0! - Enumerate - for an acting semigroup data and limit
##############################################################################

InstallOtherMethod(Enumerate, "for an acting semi data and limit", 
[IsSemigroupData, IsCyclotomic],
function(data, limit)
  return Enumerate(data, limit, ReturnFalse);
end);

# new for 1.0! - Enumerate - for an acting semigroup data, limit, func
##############################################################################

InstallOtherMethod(Enumerate, 
"for an acting semi data, limit, and func",
[IsSemigroupData, IsCyclotomic, IsFunction],
function(data, limit, lookfunc)
  local looking, ht, orb, nr, i, graph, reps, repslookup, orblookup1, orblookup2, repslens, lenreps, schreierpos, schreiergen, schreiermult, s, gens, nrgens, genstoapply, lambda, lambdaht, lambdaact, lambdaperm, lambdamult, rank, rho, lambdarhoht, o, scc, r, lookup, x, lamx, pos, m, mults, y, rhoy, val, schutz, tmp, old, p, graded, gradedlens, hashlen, gradingfunc, rankx, schutzstab, j, n;

  if IsClosed(data) then 
    return data;
  fi;
 
 if lookfunc<>ReturnFalse then 
    looking:=true;
  else
    looking:=false;
  fi;

  ht:=data!.ht;
  orb:=data!.orbit;   # the so far found R-reps data 
  nr:=Length(orb);
  i:=data!.pos;       # points in orb in position at most i have descendants
  graph:=data!.graph; # orbit graph of orbit of R-classes under left mult 
  reps:=data!.reps;   # reps grouped by equal lambda and rho value
                      # HTValue(lambdarhoht, Concatenation(lambda(x),
                      # rho(x))
  
  repslookup:=data!.repslookup; # Position(orb, reps[i][j])=repslookup[i][j]
                                # = HTValue(ht, reps[i][j])
  
  orblookup1:=data!.orblookup1; # orblookup1[i] position in reps containing 
                                # orb[i][4] (the R-rep)

  orblookup2:=data!.orblookup2; # orblookup2[i] position in reps[orblookup1[i]] 
                                # containing orb[i][4] (the R-rep)

 repslens:=data!.repslens;     # Length(reps[i])=repslens[i] 
  lenreps:=data!.lenreps;       # lenreps=Length(reps)

  # schreier

  schreierpos:=data!.schreierpos;
  schreiergen:=data!.schreiergen;
  schreiermult:=data!.schreiermult;

  # generators
  s:=ParentAttr(data);
  gens:=Generators(s); 
  nrgens:=Length(gens); 
  genstoapply:=[1..nrgens];
  
  # lambda/rho
  lambda:=LambdaFunc(s);
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
        
        lamx:=lambda(x);
        pos:=Position(o, lamx);
        
        #find the scc
        m:=lookup[pos];

        #put lambda x in the first position in its scc
        if not pos=scc[m][1] then 
          
          #get the multipliers
          #JDM expand!
          mults:=LambdaOrbMults(o, m);         
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
        # x:=[s, [m, scc[m][1]], o, y, nr+1, val];

        if val=fail then  #new rho value, and hence new R-rep
          lenreps:=lenreps+1;
          HTAdd(lambdarhoht, rhoy, lenreps);
          nr:=nr+1;
          reps[lenreps]:=[y];
          repslookup[lenreps]:=[nr];
          orblookup1[nr]:=lenreps;
          orblookup2[nr]:=1;
          repslens[lenreps]:=1;
          x:=[s, m, o, y, nr];
          # semigroup, lambda orb data, lambda orb, rep, index in orbit,
          # position of reps with equal lambda-rho value

        else              # old rho value
          x:=[s, m, o, y, nr+1];
          
          # JDM expand!
          schutz:=LambdaOrbStabChain(o, m);
          
          #check membership in schutz gp via stab chain
          
          if schutz=true then # schutz gp is symmetric group
            graph[i][j]:=repslookup[val][1];
            continue;
          else
            if schutz=false then # schutz gp is trivial
              tmp:=HTValue(ht, y);
              if tmp<>fail then 
                graph[i][j]:=tmp;
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
            repslens[val]:=repslens[val]+1;
            reps[val][repslens[val]]:=y;
            repslookup[val][repslens[val]]:=nr;
            orblookup1[nr]:=val;
            orblookup2[nr]:=repslens[val];
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
        
        # are we looking for something?
        if looking then 
          
          # did we find it?
          if lookfunc(data, y) then 
            data!.pos:=i-1;
            data!.found:=nr;
            data!.lenreps:=lenreps;
            return data;
          fi;
        fi;
      od;
    od;
  else #JDM graded- this should be updated as per the first part of this func
    
    lambdaht:=LambdaHT(s);
    graded:=GradedLambdaOrbs(s);  # existing graded lambda orbs
    gradedlens:=graded!.lens;     # gradedlens[j]=Length(graded[j]);
       
    # options for graded lambda orbs
    hashlen:=CitrusOptionsRec.hashlen.M;  
    gradingfunc := function(o,x) return [rank(x)+1, x]; end;

    while nr<=limit and i<nr do 
      i:=i+1;
      
      for j in genstoapply do #JDM
        x:=gens[j]*orb[i][4];
        
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
              data:=[rankx, gradedlens[rankx]],
              semi:=s));
          
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
          x:=[s, 1, o, x, nr];
          pos:=true;
        else #old lambda orbit
          o:=graded[pos[1]][pos[2]];
          
          #find the scc
          scc:=OrbSCC(o); r:=Length(scc);
          lookup:=o!.scc_lookup;
          m:=lookup[pos[3]];
          scc:=scc[m]; 

          #get the multipliers
          #JDM expand
          mults:=LambdaOrbMults(o, m);
          
          #if not IsBound(o!.mults) then 
          #  o!.mults:=EmptyPlist(Length(o));
          #fi;
          #mults:=o!.mults;
          #if not IsBound(mults[scc[1]]) then ;
          #  for k in scc do
          #    f:=EvaluateWord(gens, TraceSchreierTreeOfSCCBack(o, m, k));
          #    mults[k]:=lambdamult(o[k], f);
          #  od; 
          #fi;
          
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
            #JDM expand!
            schutzstab:=LambdaOrbStabChain(o, m);

            #check membership in schutz gp via stab chain
            
            if schutzstab=true then # schutz gp is symmetric group
              graph[i][j]:=repslookup[val][1];
              continue;
            else
              if schutzstab=false then # schutz gp is trivial
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
                  if SiftedPermutation(schutzstab, p)=() then 
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
        graph[nr]:=EmptyPlist(nrgens);
        graph[i][j]:= nr;
        
        # are we looking for something?
        if looking then

          # did we find it?
          if lookfunc(data, y) then
            data!.pos:=i-1;
            data!.found:=nr;
            data!.lenreps:=lenreps;
            return data;
          fi;
        fi;
      od;
    od;
  fi;
  data!.pos:=i;
  data!.lenreps:=lenreps;
  if looking then 
    data!.found:=false;
  fi;
  if nr=i then 
    SetFilterObj(data, IsClosed);
  fi;
  return data;
end);

#JDM

# new for 1.0! - Enumerate - "for a regular acting semi"
##############################################################################
# JDM how do we do this for SemigroupData?!

InstallOtherMethod(Enumerate, "for a regular acting semigroup data",
[IsActingSemigroup and IsRegularSemigroup],
function(s)
  local o, scc, r, m;

  o:=LambdaOrb(s);

  if not o!.enumerated then 
    scc:=OrbSCC(o);
    r:=Length(scc);
    for m in [1..r] do
      #JDM expand!
      LambdaOrbSchutzGp(o, m);
    od;
  fi;
  
  return o;
end);

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

# new for 1.0! - GradedRhoOrb - "for an acting semigroup and elt"
##############################################################################

InstallGlobalFunction(GradedRhoOrb,
function(s, f, opt)
  local graded, pos, gradingfunc, onlygrades, onlygradesdata, o, j, k, l;

  if opt then   #global
    graded:=GradedRhoOrbs(s);
    pos:=HTValue(RhoHT(s), RhoFunc(s)(f));
  
    if pos<>fail then 
      return graded[pos[1]][pos[2]];
    fi;
    
    gradingfunc := function(o,x) return [RhoRank(s)(x), x]; end;
    onlygrades:=function(x, data_ht)
      return x[1]=RhoRank(s)(RhoFunc(s)(f))
       and HTValue(data_ht, x[2])=fail; 
    end;
    onlygradesdata:=RhoHT(s);
  else          #local
    gradingfunc:=function(o,x) return RhoRank(s)(x); end;
    onlygrades:=function(x,data_ht) 
      return x=RhoRank(s)(RhoFunc(s)(f));
    end;
    onlygradesdata:=fail;
  fi;  
 
  o:=Orb(s, RhoFunc(s)(f), RhoAct(s),
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

  SetIsGradedRhoOrb(o, true);

  if opt then # store o
    j:=RhoRank(s)(RhoFunc(s)(f))+1;
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

# new for 1.0! - GradedRhoOrbs - "for an acting semigroup" 
##############################################################################
# stores so far calculated GradedRhoOrbs

InstallMethod(GradedRhoOrbs, "for an acting semigroup", 
[IsActingSemigroup],
function(s)
  return Objectify(NewType(FamilyObj(s), IsGradedRhoOrbs), rec(
    orbits:=List([1..RhoDegree(s)+1], x-> []), 
    lens:=[1..RhoDegree(s)+1]*0, semi:=s));
end);

#III

# new for 1.0! - InitSemigroupData - "for acting semi, data, and element"
#############################################################################
# - maybe rewrite so that the generators are really added here, rather than one.

InstallGlobalFunction(InitSemigroupData, 
function(s, data, x)
  local lamx, pos, o, m, scc;

  # decide if we are using graded orbits or not.
  if (not HasGradedLambdaOrbs(s)) or (HasLambdaOrb(s) and
   HasGradedLambdaOrbs(s) and Length(LambdaOrb(s))>=LambdaHT(s)!.nr) then 
    data!.graded:=false;
  else
    data!.graded:=true;
  fi;
  
  # install first point if we are in a monoid
  if x<>false then 
    
    # find the orbit containing LambdaFunc(s)(x)...
    lamx:=LambdaFunc(s)(x);
    if not data!.graded then 
      o:=LambdaOrb(s);
      m:=1;
    else
      pos:=HTValue(LambdaHT(s), lamx); #scc index, scc[1], pos of lamx in o
      if pos=fail then 
        o:=GradedLambdaOrb(s, x, true);
        m:=1; #scc index
      else
        o:=GradedLambdaOrbs(s)[pos[1]][pos[2]];
        m:=OrbSCCLookup(o)[pos[3]];
        # LambdaFunc(x) must be in 1st place of scc since scc has length 1!
      fi;  
    fi;
    
    # install the info about x in data
    HTAdd(data!.ht, x, 1);
    data!.orbit:=[[s, m, o, x, 1]];
    data!.repslens[1]:=1;
    data!.lenreps:=data!.lenreps+1;
    data!.reps[data!.lenreps]:=[x];
    data!.repslookup[1]:=[1];
    data!.orblookup1[1]:=1;
    data!.orblookup2[1]:=1;

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

  # the component enumerated is only used for regular semigroups
  return Orb(s, LambdaDomain(s), LambdaAct(s),
        rec(forflatplainlists:=true, schreier:=true, orbitgraph:=true,
        storenumbers:=true, log:=true, hashlen:=CitrusOptionsRec.hashlen.M,
        enumerated:=false, scc_reps:=[x], semi:=s));
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

  return g;
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

# new for 1.0! - RhoOrbStabChain - "for a rho orb and scc index"
##############################################################################

InstallGlobalFunction(RhoOrbStabChain, 
function(o, m)
  
  if IsBound(o!.schutzstab) then 
    if IsBound(o!.schutzstab[m]) then 
      return o!.schutzstab[m];
    fi;
  fi;
 
  RhoOrbSchutzGp(o, m, infinity);
  return o!.schutzstab[m];
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

# new for 1.0! - LambdaRhoLookup - "for a D-class of an acting semigroup"
##############################################################################

InstallMethod(LambdaRhoLookup, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass], 
function(d)
  local data, orb_scc, orblookup1, orblookup2, out, i;

  data:=SemigroupData(ParentAttr(d));
  
  # scc of R-reps corresponding to d 
  orb_scc:=SemigroupDataSCC(d);

  # positions in reps containing R-reps in d 
  orblookup1:=data!.orblookup1;
  orblookup2:=data!.orblookup2;

  out:=[]; 
  for i in orb_scc do 
    if not IsBound(out[orblookup1[i]]) then 
      out[orblookup1[i]]:=[];
    fi;
    Add(out[orblookup1[i]], orblookup2[i]);
  od;

  return out;
end);

# new for 1.0! - Length - for semigroup data of acting semigroup
##############################################################################

InstallOtherMethod(Length, "for semigroup data of acting semigroup",
[IsSemigroupData], x-> Length(x!.orbit));

#OOO

# new for 1.0! - OrbitGraphAsSets - for semigroup data of acting semigroup
##############################################################################

InstallOtherMethod(OrbitGraphAsSets, "for semigroup data of acting semigroup",  
[IsSemigroupData], 99,
function(data)
  return List(data!.graph, Set);
end);

#PPP

# new for 1.0! - Position - "for graded lambda orbs and lambda value"
##############################################################################

InstallOtherMethod(Position, "for graded lambda orbs and lambda value",
[IsGradedLambdaOrbs, IsObject, IsZeroCyc],
function(o, lamf, n)
  return HTValue(LambdaHT(o!.semi), lamf);
end);

# new for 1.0! - Position - "for graded rho orbs and rho value"
##############################################################################

InstallOtherMethod(Position, "for graded rho orbs and rho value",
[IsGradedRhoOrbs, IsObject, IsZeroCyc],
function(o, rho, n)
  return HTValue(RhoHT(o!.semi), rho);
end);

# new for 1.0! - Position - "for acting semigroup data and acting elt"
##############################################################################
# returns the index of the representative of the R-class containing x in the
# parent of data. 

InstallOtherMethod(Position, "for acting semigroup data and acting elt",
[IsSemigroupData, IsObject, IsZeroCyc], 100,
function(data, x, n)
  local val, s, o, l, m, scc, schutz, repslookup, mults, y, reps, repslens, lambdaperm;

  val:=HTValue(data!.ht, x);

  if val<>fail then 
    return val;
  fi;

  s:=ParentAttr(data);

  if data!.graded then 
    # JDM this assumes that x is an element of s, probably shouldn't do this!
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
 
  if l<>scc[m][1] then 
    mults:=LambdaOrbMults(o, m);
    y:=x*mults[l];
  else
    y:=x;
  fi; 

  reps:=data!.reps; repslens:=data!.repslens;

  if schutz=false then #JDM change this so that it just returns HTValue(data!.ht);
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

# new for 1.0! - PrintObj - "for graded rho orbs"
##############################################################################

InstallMethod(PrintObj, [IsGradedRhoOrbs],
function(o)
  Print("<graded rho orbs: ");
  View(o!.orbits);
  Print(" >");
  return;
end);

#RRR

# new for 1.0! - RhoOrb - "for an acting semigroup"
##############################################################################

InstallMethod(RhoOrb, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local x;

  # it might be better in the case of having IsClosed(SemigroupData)
  # to just fake the orbit below (we have all the info already).
  # But it seems to be so fast to calculate the 
  # in most cases that there is no point. 
  
  #JDM it would be much better to just do One(s); 
  if IsTransformationSemigroup(s) then
    x:=One(Generators(s)[1]);
  elif IsPartialPermSemigroup(s) then
    x:=PartialPermNC(Points(Generators(s)), Points(Generators(s)));
  fi;    

  # the component enumerated is only used for regular semigroups
  return Orb(s, RhoDomain(s), RhoAct(s),
        rec(forflatplainlists:=true, schreier:=true, orbitgraph:=true,
        storenumbers:=true, log:=true, hashlen:=CitrusOptionsRec.hashlen.M,
        enumerated:=false, scc_reps:=[x], semi:=s));
end);

# new for 1.0! - RhoOrbMults - "for a rho orb and scc index"
##############################################################################
# f takes o[scc[1]] to o[i] and rhomult(o[i], f) takes o[i] to o[scc[1]]

InstallGlobalFunction(RhoOrbMults,
function(o, m)
  local scc, s, mults, gens, rhomult, f, i;

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
  rhomult:=RhoMult(s);

  for i in scc do
    f:=EvaluateWord(gens, Reversed(TraceSchreierTreeOfSCCForward(o, m, i)));
    mults[i]:=[f, rhomult(o[scc[1]], f)];
  od;

  return mults;
end);

# new for 1.0! - RhoOrbRep - "for a rho orb and scc index"
##############################################################################

InstallGlobalFunction(RhoOrbRep, 
function(o, m)
  local w;

  if IsBound(o!.scc_reps[m]) then 
    return o!.scc_reps[m];
  fi;

  w:=Reversed(TraceSchreierTreeForward(o, OrbSCC(o)[m][1]));
  o!.scc_reps[m]:=o!.scc_reps[1]*EvaluateWord(o!.gens, w);
  return o!.scc_reps[m];
end);

# new for 1.0! - RhoOrbSchutzGp - "for a rho orb, scc index, and bound"
##############################################################################
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

  s:=o!.semi;
  gens:=Generators(s);
  nrgens:=Length(gens);
  scc:=OrbSCC(o)[m];
  lookup:=o!.scc_lookup;
  orbitgraph:=OrbitGraph(o);
  lambdaperm:=LambdaPerm(s);
  rep:=RhoOrbRep(o, m);
  mults:=RhoOrbMults(o, m);
  
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

  data:=rec( ht:=HTCreate(x, rec(hashlen:=s!.opts.hashlen.L)),
     pos:=0, graph:=[EmptyPlist(Length(gens))], 
     reps:=[], repslookup:=[], orblookup1:=[], orblookup2:=[],
     lenreps:=0, orbit:=[[,,,x]], repslens:=[], 
     schreierpos:=[fail], schreiergen:=[fail], schreiermult:=[fail]);
  
  Objectify(NewType(FamilyObj(s), IsSemigroupData and IsAttributeStoringRep),
   data);

  if IsMonoid(s) or x in gens then 
    InitSemigroupData(s, data, x);
    if not IsMonoid(s) then 
      SetIsMonoidAsSemigroup(s, true);
    fi;
    data!.modifier:=0;
  else
    InitSemigroupData(s, data, false);
    SetIsMonoidAsSemigroup(s, false);
    data!.modifier:=1;
  fi;
  SetParentAttr(data, s);
  return data;
end);

# new for 1.0! - SemigroupEltSLP - "for an acting semigroup and acting elt"
##############################################################################
#JDM this is not really working due to the slps for group elements containing
#inverses. Also even if they don't, then we don't correct for the group elt,
#and so the answer is out by a multiple of a group elt.

#JDm rough!

InstallMethod(SemigroupEltSLP, "for an acting semigroup and acting elt",
[IsActingSemigroup, IsActingElt],
function(s, x)
  local data, nr, gens, zip, o, m, scc, l, mult, y, p, schutz, stab, slpstrong,
   slp;

  Info(InfoWarning, 1, "this does not always return the correct answer!");

  # suppose that the data is fully enumerated...
  data:=SemigroupData(s);
  #Enumerate(s, lookingfor:=??)
  
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
  m:=data[nr][2];
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

# new for 1.0! - Size - "for an acting semigroup data"
##############################################################################

InstallOtherMethod(Size, "for semigroup data",
[IsSemigroupData],
function(data)
  local reps, nr, repslookup, orbit, i, j;
   
  reps:=data!.reps;
  nr:=Length(reps);
  repslookup:=data!.repslookup;
  orbit:=data!.orbit;
  i:=0;

  for j in [1..nr] do 
    data:=orbit[repslookup[j][1]];
    i:=i+Length(reps[j])*Size(LambdaOrbSchutzGp(data[3], data[2]))*Length(OrbSCC(data[3])[data[2]]);
  od;
  return i; 
end);

# new for 1.0! - Size - "for an acting semigroup"
##############################################################################

InstallMethod(Size, "for an acting semigroup",
[IsActingSemigroup], 100,
function(s)
  local data, reps, nr, repslookup, orbit, i, j;
   
  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  reps:=data!.reps;
  nr:=Length(reps);
  repslookup:=data!.repslookup;
  orbit:=data!.orbit;
  i:=0;

  for j in [1..nr] do 
    data:=orbit[repslookup[j][1]];
    i:=i+Length(reps[j])*Size(LambdaOrbSchutzGp(data[3], data[2]))*Length(OrbSCC(data[3])[data[2]]);
  od;
  return i; 
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
      m:=orb[pos][2];            # the scc
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

InstallMethod(ViewObj, [IsSemigroupData], 999,
function(data)
  Print("<semigroup data: ", Length(data!.orbit), " reps, ",
  Length(data!.reps), " lambda-rho values>");
  return;
end);

#EOF
