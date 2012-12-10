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

InstallOtherMethod(IsActingSemigroup, "for an object", [IsObject], ReturnFalse);

##############################################################################

InstallGlobalFunction(RhoPos, 
function(o)
  local i;
  if not IsBound(o!.rho_l) then 
    return fail;
  fi;

  i:=o!.rho_l;
  Unbind(o!.rho_l);
  return i;
end);

InstallGlobalFunction(LambdaPos, 
function(o)
  local i;
  if not IsBound(o!.lambda_l) then 
    return fail;
  fi;

  i:=o!.lambda_l;
  Unbind(o!.lambda_l);
  return i;
end);

# new for 1.0! - GradedLambdaHT
###############################################################################

InstallMethod(GradedLambdaHT, "for an acting semi",
[IsActingSemigroup],
function(s)
return HTCreate(LambdaFunc(s)(GeneratorsOfSemigroup(s)[1]),
 rec(forflatplainlists:=true, treehashsize:=s!.opts.hashlen.S));
end);

# new for 1.0! - GradedRhoHT 
###############################################################################

InstallMethod(GradedRhoHT, "for an acting semi",
[IsActingSemigroup],
function(s)
return HTCreate(RhoFunc(s)(GeneratorsOfSemigroup(s)[1]),
 rec(forflatplainlists:=true, treehashsize:=s!.opts.hashlen.S));
end);

# new for 1.0! - RectifyRho - "for a rho orb and an acting element"
##############################################################################
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

InstallGlobalFunction(RectifyInverseRho,
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
    f:=LambdaOrbMult(arg[2], m, l)[1]*f;
  fi;
  return rec(l:=l, m:=m, rep:=f);
end);

# new for 1.0! - LambdaRhoHT
###############################################################################

InstallMethod(LambdaRhoHT, "for an acting semi",
[IsActingSemigroup],
function(s)
  local x;
  x:=GeneratorsOfSemigroup(s)[1]; 
  return HTCreate(Concatenation([1], RhoFunc(s)(x)),
  rec(forflatplainlists:=true,
     treehashsize:=s!.opts.hashlen.S));
end);

############################################################################### 
###############################################################################

# new for 1.0! - \in - for lambda value of acting semi elt & graded lamda orbs
##############################################################################

InstallMethod(\in, "for lambda value of acting semi elt and graded lambda orbs",
[IsObject, IsGradedLambdaOrbs],
function(lamf, o)
  return not HTValue(GradedLambdaHT(o!.semi), lamf)=fail;
end);

# new for 1.0! - \in - for rho value of acting semi elt & graded rho orbs
##############################################################################

InstallMethod(\in, "for rho value of acting semi elt and graded rho orbs",
[IsObject, IsGradedRhoOrbs],
function(rho, o)
  return not HTValue(GradedRhoHT(o!.semi), rho)=fail;
end);

# new for 1.0! - \in - for acting semi elt and semigroup data
##############################################################################
# expand?

InstallMethod(\in, "for acting semi elt and semigroup data",
[IsAssociativeElement, IsSemigroupData],
function(f, data)
  return not Position(data, f)=fail;
end);

# new for 1.0! - \in - for an acting elt and acting semigroup
##############################################################################

InstallMethod(\in, "for an acting elt and acting semigroup",
[IsAssociativeElement, IsActingSemigroup], 
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
  #  Info(InfoSemigroups, 2, "element and semigroup have different degrees.");
  #  return false;       
  #fi;

  if not (IsMonoid(s) and IsOne(f)) and 
   ActionRank(f) > MaximumList(List(Generators(s), ActionRank)) then
    Info(InfoSemigroups, 2, "element has larger rank than any element of ",
     "semigroup.");
    return false;
  fi;

  if HasMinimalIdeal(s) and 
   ActionRank(f) < ActionRank(Representative(MinimalIdeal(s))) then
    Info(InfoSemigroups, 2, "element has smaller rank than any element of ",
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
  o:=LambdaOrb(s);
 
  # JDM this is a disadvantage of the approach of enumerating the entire
  # LambdaOrb when applied to a single \in test.
  if not IsClosed(o) then 
    Enumerate(o, infinity);
  fi;
  
  l:=Position(o, lambda);
    
  if l=fail then 
    return false;
  fi;
  
  # strongly connected component of lambda orb
  m:=OrbSCCLookup(o)[l];
  scc:=OrbSCC(o);

  # check if lambdarho is already known
  lambdarho:=[m];
  Append(lambdarho, RhoFunc(s)(f));
  val:=HTValue(LambdaRhoHT(s), lambdarho);

  lookfunc:=function(data, x) 
    return Concatenation([x[2]], RhoFunc(s)(x[4]))=lambdarho;
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
    #g:=f*LambdaOrbMults(o, m)[l][2];
    g:=f*LambdaOrbMult(o, m, l)[2];
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
      #if SiftGroupElement(schutz, lambdaperm(reps[val][n], g)).isone then 
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
          n:=HTValue(ht, g);
          if n<>fail then 
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
            #if SiftGroupElement(schutz, lambdaperm(reps[val][m], g)).isone then 
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

#AAA

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
  local looking, ht, orb, nr, i, graph, reps, repslookup, orblookup1, orblookup2, repslens, lenreps, schreierpos, schreiergen, schreiermult, gens, nrgens, genstoapply, s, lambda, lambdaact, lambdaperm, rho, lambdarhoht, o, oht, scc, r, lookup, htvalue, htadd, x, lamx, pos, m, y, rhoy, val, schutz, tmp, old, j, n;

  if IsClosed(data) then 
    return data;
  fi;
 
 if lookfunc<>ReturnFalse then 
    looking:=true;
  else
    looking:=false;
  fi;
  data!.looking:=looking;

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
  gens:=data!.gens; 
  nrgens:=Length(gens); 
  genstoapply:=[1..nrgens];
  
  # lambda/rho
  s:=ParentSemigroup(data);
  lambda:=LambdaFunc(s);
  lambdaact:=LambdaAct(s);  
  lambdaperm:=LambdaPerm(s);
  rho:=RhoFunc(s);
  lambdarhoht:=LambdaRhoHT(s);

  o:=LambdaOrb(s);
  oht:=o!.ht;
  Enumerate(o, infinity);
  scc:=OrbSCC(o); r:=Length(scc);
  lookup:=o!.scc_lookup;
 
  if IsBound(HTAdd_TreeHash_C) then 
    htadd:=HTAdd_TreeHash_C;
    htvalue:=HTValue_TreeHash_C;
  else
    htadd:=HTAdd;
    htvalue:=HTValue;
  fi;

  while nr<=limit and i<nr do 
    i:=i+1;
    
    for j in genstoapply do #JDM
      x:=gens[j]*orb[i][4];
      lamx:=lambda(x);
      #pos:=Position(o, lamx);
      pos:=htvalue(oht, lamx); 

      #find the scc
      m:=lookup[pos];

      #put lambda x in the first position in its scc
      if not pos=scc[m][1] then 
        
        #JDM expand!
        y:=x*LambdaOrbMult(o, m, pos)[2];
      else
        y:=x;
        pos:=fail;
      fi;

      rhoy:=[m];
      Append(rhoy, rho(y));
      val:=htvalue(lambdarhoht, rhoy);

      # this is what we keep if it is new
      # x:=[s, m, o, y, false, nr+1];

      if val=fail then  #new rho value, and hence new R-rep
        lenreps:=lenreps+1;
        htadd(lambdarhoht, rhoy, lenreps);
        nr:=nr+1;
        reps[lenreps]:=[y];
        repslookup[lenreps]:=[nr];
        orblookup1[nr]:=lenreps;
        orblookup2[nr]:=1;
        repslens[lenreps]:=1;
        x:=[s, m, o, y, false, nr];
        # semigroup, lambda orb data, lambda orb, rep, index in orbit,
        # position of reps with equal lambda-rho value

      else              # old rho value
        x:=[s, m, o, y, false, nr+1];
        
        # JDM expand!
        schutz:=LambdaOrbStabChain(o, m);
        
        #check membership in schutz gp via stab chain
        
        if schutz=true then # schutz gp is symmetric group
          graph[i][j]:=repslookup[val][1];
          continue;
        else
          if schutz=false then # schutz gp is trivial
            tmp:=htvalue(ht, y);
            if tmp<>fail then 
              graph[i][j]:=tmp;
              continue;
            fi;
          else # schutz gp neither trivial nor symmetric group
            old:=false; 
            for n in [1..repslens[val]] do 
              if SiftedPermutation(schutz, lambdaperm(reps[val][n], y))=() then 
              #if SiftGroupElement(schutz,lambdaperm(reps[val][n],y)).isone then
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
      htadd(ht, y, nr);
      graph[nr]:=EmptyPlist(nrgens);
      graph[i][j]:= nr;
      
      # are we looking for something?
      if looking then 
        # did we find it?
        if lookfunc(data, x) then 
          data!.pos:=i-1;
          data!.found:=nr;
          data!.lenreps:=lenreps;
          return data;
        fi;
      fi;
    od;
  od;
  
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

#FFF

#GGG

# new for 1.0! - GradedLambdaOrb - "for an acting semigroup and elt"
##############################################################################

# if GradedLambdaOrb(s, f, true) is called, then the returned orbit o has 
# the position in o of lambda val of f stored in o!.data.

InstallGlobalFunction(GradedLambdaOrb,
function(s, f, opt)
  local lambda, graded, pos, gradingfunc, onlygrades, onlygradesdata, o, j, k, l;

  if IsAssociativeElement(f) then 
    lambda:=LambdaFunc(s)(f);
  else
    lambda:=f;
  fi;

  if opt then   #global
    graded:=GradedLambdaOrbs(s);
    pos:=HTValue(GradedLambdaHT(s), lambda);
  
    if pos<>fail then 
      graded[pos[1]][pos[2]]!.lambda_l:=pos[3];
      return graded[pos[1]][pos[2]];
    fi;
    
    gradingfunc := function(o,x) return [LambdaRank(s)(x), x]; end;
    onlygrades:=function(x, data_ht)
      return x[1]=LambdaRank(s)(lambda)
       and HTValue(data_ht, x[2])=fail; 
    end;
    onlygradesdata:=GradedLambdaHT(s);
  else          #local
    gradingfunc:=function(o,x) return LambdaRank(s)(x); end;
    onlygrades:=function(x,data_ht) 
      return x=LambdaRank(s)(lambda);
    end;
    onlygradesdata:=fail;
  fi;  
 
  o:=Orb(s, lambda, LambdaAct(s),
      rec(
        semi:=s,
        forflatplainlists:=true, #JDM probably don't want to assume this..
        treehashsize:=SemigroupsOptionsRec.hashlen.M,
        schreier:=true,
        gradingfunc:=gradingfunc,
        orbitgraph:=true,
        onlygrades:=onlygrades,
        onlygradesdata:=onlygradesdata,
        storenumbers:=true,
        log:=true,
        scc_reps:=[f])); # note that this component shouldn't be used if f is a
                         # lambda value and not an acting elt.

  SetIsGradedLambdaOrb(o, true);
  o!.lambda_l:=1;
  
  if opt then # store o
    j:=LambdaRank(s)(lambda)+1;
    # the +1 is essential as the rank can be 0
    k:=graded!.lens[j]+1;
    graded[j][k]:=o;
    Enumerate(o);
    for l in [1..Length(o)] do
      HTAdd(onlygradesdata, o[l], [j,k,l]);
    od;
    o!.val:=[j,k,1]; 
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
    pos:=HTValue(GradedRhoHT(s), RhoFunc(s)(f));
  
    if pos<>fail then 
      
      # store the position of RhoFunc(s)(f) in o 
      graded[pos[1]][pos[2]]!.rho_l:=pos[3];
      return graded[pos[1]][pos[2]];
    fi;
    
    gradingfunc := function(o,x) return [RhoRank(s)(x), x]; end;
    onlygrades:=function(x, data_ht)
      return x[1]=RhoRank(s)(RhoFunc(s)(f))
       and HTValue(data_ht, x[2])=fail; 
    end;
    onlygradesdata:=GradedRhoHT(s);
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
        treehashsize:=SemigroupsOptionsRec.hashlen.M,
        schreier:=true,
        gradingfunc:=gradingfunc,
        orbitgraph:=true,
        onlygrades:=onlygrades,
        onlygradesdata:=onlygradesdata,
        storenumbers:=true,
        log:=true,
        scc_reps:=[f]));

  SetIsGradedRhoOrb(o, true);
  o!.rho_l:=1; 
  
  if opt then # store o
    j:=RhoRank(s)(RhoFunc(s)(f))+1;
    # the +1 is essential as the rank can be 0
    k:=graded!.lens[j]+1;
    graded[j][k]:=o;
    Enumerate(o);
    for l in [1..Length(o)] do
      HTAdd(onlygradesdata, o[l], [j,k,l]);
    od;
    
    # store the position of RhoFunc(s)(f) in o 
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
  local fam;
 
  fam:=CollectionsFamily(FamilyObj(LambdaFunc(s)(Representative(s))));
  return Objectify(NewType(fam, IsGradedLambdaOrbs), 
   rec( orbits:=List([1..LambdaDegree(s)+1], x-> []), 
     lens:=[1..LambdaDegree(s)+1]*0, semi:=s));
end);

# new for 1.0! - GradedRhoOrbs - "for an acting semigroup" 
##############################################################################
# stores so far calculated GradedRhoOrbs

InstallMethod(GradedRhoOrbs, "for an acting semigroup", 
[IsActingSemigroup],
function(s)
  return Objectify(NewType(FamilyObj(s), IsGradedRhoOrbs), rec(
    orbits:=List([1..LambdaDegree(s)+1], x-> []), 
    lens:=[1..LambdaDegree(s)+1]*0, semi:=s));
end);

#III

# new for 1.0! - IsBound - for graded lambda orbs and pos int
##############################################################################

InstallMethod(IsBound\[\], "for graded lambda orbs and pos int",
[IsGradedLambdaOrbs, IsPosInt], 
function(o, j)
  return IsBound(o!.orbits[j]);
end);

# new for 1.0! - IsBound - for graded rho orbs and pos int
##############################################################################

InstallMethod(IsBound\[\], "for graded rho orbs and pos int",
[IsGradedRhoOrbs, IsPosInt], 
function(o, j)
  return IsBound(o!.orbits[j]);
end);

#LLL

# new for 1.0! - LambdaOrb - "for an acting semigroup"
##############################################################################

InstallMethod(LambdaOrb, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local opts, semi, name;

  opts:= rec(schreier:=true, orbitgraph:=true,
          storenumbers:=true, log:=true, 
          treehashsize:=SemigroupsOptionsRec.hashlen.M,
          scc_reps:=[One(Generators(s))], semi:=s);
  
  for name in RecNames(LambdaOrbOpts(s)) do 
    opts.(name):=LambdaOrbOpts(s).(name);
  od;

  return Orb(GeneratorsOfSemigroup(s), LambdaDomain(s), LambdaAct(s), opts);
end);

# new for 1.0! - LambdaOrbMults - "for a lambda orb and scc index"
##############################################################################

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
      one:=[One(o!.gens), One(o!.gens)];
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
    mults[scc[1]]:=[One(gens), One(gens)];
  fi; 
 
  genpos:=ReverseSchreierTreeOfSCC(o, m);
  inv:=function(i, f) return LambdaInverse(o!.semi)(f); end;

  trace:=function(i)
    local f;

    if IsBound(mults[i]) then 
      return mults[i][2];
    fi;
    f:=gens[genpos[1][i]]*trace(genpos[2][i]);
    mults[i]:=[inv(i, f), f];
    return f;
  end;

  for i in scc do 
    trace(i);  
  od;
  return o!.mults;
end);

# new for 1.0! - LambdaOrbMult - "for a lambda orb, scc index, and index"
##############################################################################
# f takes o[scc[1]] to o[i] and inv(o[i], f) takes o[i] to o[scc[1]]

InstallGlobalFunction(LambdaOrbMult,
function(o, m, i)
  local mults, one, scc, gens, genpos, inv, trace, x;

  if IsBound(o!.mults) then
    if IsBound(o!.mults[i]) then
      return o!.mults[i];
    fi;
  else
    mults:=EmptyPlist(Length(o));
    one:=[One(o!.gens), One(o!.gens)];
    for x in OrbSCC(o) do 
      mults[x[1]]:=one;
    od;
    o!.mults:=mults;
  fi;

  scc:=OrbSCC(o)[m];
  mults:=o!.mults;
  gens:=o!.gens;
  genpos:=ReverseSchreierTreeOfSCC(o, m);
  inv:=function(i, f) return LambdaInverse(o!.semi)(f); end;

  trace:=function(i)
    local f;

    if IsBound(mults[i]) then 
      return mults[i][2];
    fi;
    f:=gens[genpos[1][i]]*trace(genpos[2][i]);
    mults[i]:=[inv(i, f), f];
    return f;
  end;

  trace(i);
  return o!.mults[i];
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
  local s, gens, nrgens, scc, lookup, orbitgraph, lambdaperm, rep, slp, lenslp, len, bound, g, is_sym, f, h, k, l;
  
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
    for l in [1..nrgens] do
      if IsBound(orbitgraph[k][l]) and lookup[orbitgraph[k][l]]=m then
        # JDM maybe keep TraceSchreierTreeOfSCCForward(o, m, k) in o?
        f:=lambdaperm(rep, rep*EvaluateWord(gens,
         TraceSchreierTreeOfSCCForward(o, m, k))
          *gens[l]*LambdaOrbMult(o, m, orbitgraph[k][l])[2]);
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
    #o!.schutzstab[m]:=StabilizerChain(g);
  fi;

  return g;
end);

# new for 1.0! - RhoOrbStabChain - "for a rho orb and scc index"
##############################################################################

InstallOtherMethod(RhoOrbStabChain, "for a rho orb and scc index",
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

  data:=SemigroupData(ParentSemigroup(d));
  
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
  return HTValue(GradedLambdaHT(o!.semi), lamf);
end);

# new for 1.0! - Position - "for graded rho orbs and rho value"
##############################################################################

InstallOtherMethod(Position, "for graded rho orbs and rho value",
[IsGradedRhoOrbs, IsObject, IsZeroCyc],
function(o, rho, n)
  return HTValue(GradedRhoHT(o!.semi), rho);
end);

# new for 1.0! - Position - "for acting semigroup data and acting elt"
##############################################################################
# returns the index of the representative of the R-class containing x in the
# parent of data. 

InstallOtherMethod(Position, "for acting semigroup data and acting elt",
[IsSemigroupData, IsObject, IsZeroCyc], 100,
function(data, x, n)
  local val, s, o, l, m, scc, schutz, repslookup, y, reps, repslens, lambdaperm;

  val:=HTValue(data!.ht, x);

  if val<>fail then 
    return val;
  fi;

  s:=ParentSemigroup(data);
  o:=LambdaOrb(s);

  if not IsClosed(o) then 
    Enumerate(o, infinity);
  fi;
  
  l:=Position(o, LambdaFunc(s)(x));
  m:=OrbSCCLookup(o)[l];
  scc:=OrbSCC(o);

  val:=HTValue(LambdaRhoHT(s), Concatenation([m], RhoFunc(s)(x)));
  if val=fail then 
    return fail;
  fi;

  schutz:=LambdaOrbStabChain(o, m);
  repslookup:=data!.repslookup;

  if schutz=true then 
    return repslookup[val][1];
  fi;
 
  if l<>scc[m][1] then 
    y:=x*LambdaOrbMult(o, m, l)[2];
  else
    y:=x;
  fi; 

  reps:=data!.reps; repslens:=data!.repslens;

  if schutz=false then 
    return HTValue(data!.ht, y);
  else
    lambdaperm:=LambdaPerm(s);
    for n in [1..repslens[val]] do 
      if SiftedPermutation(schutz, lambdaperm(reps[val][n], y))=() then 
      #if SiftGroupElement(schutz, lambdaperm(reps[val][n], y)).isone then
        return repslookup[val][n];
      fi;
    od;
  fi; 
  return fail;
end);

# new for 1.0! - PositionOfFound - "for semigroup data"
##############################################################################

InstallOtherMethod( PositionOfFound,"for semigroup data",
[IsSemigroupData],
function( data )
  if not(data!.looking) then
    Error("not looking for anything,");
    return fail;
  fi;
  return data!.found;
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

  return Orb(GeneratorsOfSemigroup(s), RhoDomain(s), RhoAct(s),
        rec(forflatplainlists:=true, schreier:=true, orbitgraph:=true,
        storenumbers:=true, log:=true,
        treehashsize:=SemigroupsOptionsRec.hashlen.M,
        scc_reps:=[One(Generators(s))], semi:=s));
end);

# new for 1.0! - RhoOrbMult - "for a rho orb and index"
##############################################################################
# f takes o[scc[1]] to o[i] and inv(o[i], f) takes o[i] to o[scc[1]]

InstallGlobalFunction(RhoOrbMult,
function(o, m, i)
  local mults, one, scc, gens, genpos, inv, trace, x;

  if IsBound(o!.mults) then
    if IsBound(o!.mults[i]) then
      return o!.mults[i];
    fi;
  else
    mults:=EmptyPlist(Length(o));
    one:=[One(o!.gens), One(o!.gens)];
    for x in OrbSCC(o) do 
      mults[x[1]]:=one;
    od;
    o!.mults:=mults;
  fi;

  scc:=OrbSCC(o)[m];
  mults:=o!.mults;
  gens:=o!.gens;
  genpos:=SchreierTreeOfSCC(o, m);
  inv:=f-> RhoInverse(o!.semi)(o[scc[1]], f);
  
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

# new for 1.0! - RhoOrbMults - "for a rho orb and index"
##############################################################################
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
      one:=[One(o!.gens), One(o!.gens)];
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
    mults[scc[1]]:=[One(gens), One(gens)];
  fi; 

  genpos:=SchreierTreeOfSCC(o, m);
  inv:=f-> RhoInverse(o!.semi)(o[scc[1]], f);

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

# new for 1.0! - RhoOrbRep - "for a rho orb and scc index"
##############################################################################

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
    #o!.schutzstab[m]:=StabilizerChain(g);
  fi;

  return g;
end);

#SSS

# new for 1.0! - SemigroupData - "for an acting semigroup"
##############################################################################

InstallMethod(SemigroupData, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local gens, one, data;
 
  #if IsMonoid(s) then 
  #  gens:=GeneratorsOfMonoid(s);
  #else
    gens:=GeneratorsOfSemigroup(s);
  #fi;

  one:=One(gens);

  data:=rec( gens:=gens, 
     ht:=HTCreate(one, rec(treehashsize:=s!.opts.hashlen.L)),
     pos:=0, graph:=[EmptyPlist(Length(gens))], 
     reps:=[], repslookup:=[], orblookup1:=[], orblookup2:=[],
     lenreps:=0, orbit:=[[,,,one]], repslens:=[], 
     schreierpos:=[fail], schreiergen:=[fail], schreiermult:=[fail]);
  
  Objectify(NewType(FamilyObj(s), IsSemigroupData and IsAttributeStoringRep),
   data);
  
  SetParentSemigroup(data, s);
  return data;
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
    i:=i+Length(reps[j])*Size(LambdaOrbSchutzGp(data[3], data[2]))
     *Length(OrbSCC(data[3])[data[2]]);
  od;
  return i; 
end);

# new for 1.0! - Size - "for an acting semigroup"
##############################################################################

InstallMethod(Size, "for an acting semigroup",
[IsActingSemigroup], 
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
