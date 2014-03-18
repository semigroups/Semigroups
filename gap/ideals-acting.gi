############################################################################# 
## 
#W  ideals-acting.gi
#Y  Copyright (C) 2013-14                                 James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

# JDM: currently almost a straight copy from acting.gi

InstallMethod(SemigroupData, "for an acting semigroup ideal",
[IsActingSemigroup and IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(s)
  local gens, data, opts;
 
  gens:=GeneratorsOfSemigroup(Parent(s));

  data:=rec(gens:=gens, 
     ht:=HTCreate(gens[1], rec(treehashsize:=s!.opts.hashlen.L)),
     pos:=0, graph:=[EmptyPlist(Length(gens))], init:=false,
     reps:=[], repslookup:=[], orblookup1:=[], orblookup2:=[], rholookup:=[fail],
     lenreps:=[0], orbit:=[[,,,FakeOne(gens)]], dorbit:=[], repslens:=[],
     lambdarhoht:=[], schreierpos:=[fail], schreiergen:=[fail],
     schreiermult:=[fail], genstoapply:=[1..Length(gens)], stopper:=false);
  
  Objectify(NewType(FamilyObj(s), IsSemigroupIdealData and IsAttributeStoringRep),
   data);
  
  SetParent(data, s);
  return data;
end);

# We concentrate on the case when nothing is known about the parent of the
# ideal.

# we make the R-class centered data structure as in SemigroupData but at the
# same time have an additional "orbit" consisting of D-class reps. 

InstallMethod(Enumerate, 
"for semigroup ideal data, limit, and func",
[IsSemigroupIdealData, IsCyclotomic, IsFunction],
function(data, limit, lookfunc)
  local looking, ht, orb, nr_r, d, nr_d, graph, reps, repslens, lenreps, lambdarhoht, repslookup, orblookup1, orblookup2, rholookup, stopper, schreierpos, schreiergen, schreiermult, gens, nrgens, genstoapply, I, lambda, lambdaact, lambdaperm, o, oht, scc, lookup, rho_o, rho, act, htadd, htvalue, add_to_data, i, start, x, k, j;
 
  if lookfunc<>ReturnFalse then 
    looking:=true;
  else
    looking:=false;
  fi;
  
  if IsClosed(data) then 
    if looking then 
      data!.found:=false;
    fi;
    return data;
  fi;
  
  data!.looking:=looking;

  ht:=data!.ht;       # so far found R-reps
  orb:=data!.orbit;   # the so far found R-reps data 
  nr_r:=Length(orb);
  d:=data!.dorbit;    # the so far found D-classes
  nr_d:=Length(d);
  graph:=data!.graph; # orbit graph of orbit of R-classes under left mult 
  reps:=data!.reps;   # reps grouped by equal lambda-scc-index and rho-value-index
 
  repslens:=data!.repslens;       # Length(reps[m][i])=repslens[m][i] 
  lenreps:=data!.lenreps;         # lenreps[m]=Length(reps[m])
  
  lambdarhoht:=data!.lambdarhoht; # HTValue(lambdarhoht, [m,l])=position in reps[m] 
                                  # of R-reps with lambda-scc-index=m and
                                  # rho-value-index=l
                      
  repslookup:=data!.repslookup; # Position(orb, reps[m][i][j])
                                # = repslookup[m][i][j]
                                # = HTValue(ht, reps[m][i][j])
  
  orblookup1:=data!.orblookup1; # orblookup1[i] position in reps[m] containing 
                                # orb[i][4] (the R-rep)

  orblookup2:=data!.orblookup2; # orblookup2[i] position in 
                                # reps[m][orblookup1[i]] 
                                # containing orb[i][4] (the R-rep)

  rholookup:=data!.rholookup;   #rholookup[i]=rho-value-index of orb[i][4]
  
  stopper:=data!.stopper;       # stop at this place in the orbit

  # schreier
  schreierpos:=data!.schreierpos;
  schreiergen:=data!.schreiergen;
  schreiermult:=data!.schreiermult;

  # generators
  gens:=data!.gens; # generators of the parent semigroup
  nrgens:=Length(gens); 
  genstoapply:=data!.genstoapply;
  
  # lambda
  I:=Parent(data);
  lambda:=LambdaFunc(I);
  lambdaact:=LambdaAct(I);  
  lambdaperm:=LambdaPerm(I);

  o:=LambdaOrb(I);
  oht:=o!.ht;
  scc:=OrbSCC(o); 
  lookup:=o!.scc_lookup;
 
  # rho
  rho_o:=RhoOrb(I); #??JDM better use graded here
  Enumerate(rho_o);
  rho:=RhoFunc(I);

  #
  act:=StabilizerAction(I);
 
  if IsBoundGlobal("ORBC") then 
    htadd:=HTAdd_TreeHash_C;
    htvalue:=HTValue_TreeHash_C;
  else
    htadd:=HTAdd;
    htvalue:=HTValue;
  fi;

  # the function which checks if x is already R/D-related to something in the
  # data and if not adds it in the appropriate place
  add_to_data:=function(x)
    local l, m, schutz, ind, n, rectify, mults, cosets, y, z;

    # the following is similar to Position(data, x);
    l:=Position(o, lambda(x));
    m:=lookup[l];
    if l<>scc[m][1] then 
      x:=x*LambdaOrbMult(o, m, l)[2];
    fi;

    schutz:=LambdaOrbStabChain(o, m);

    if HTValue(ht, x)<>fail then 
      return;
    fi;

    if schutz<>false then 
      l:=Position(rho_o, rho(x));
      if IsBound(lambdarhoht[l]) and IsBound(lambdarhoht[l][m]) then 
        
        ind:=lambdarhoht[l][m];
        
        if schutz=true then 
          return;
        fi;
        
        for n in [1..repslens[m][ind]] do
          if SiftedPermutation(schutz, lambdaperm(reps[m][ind][n], x))=() then 
            return;
          fi;
        od;
      fi;
    fi;

    nr_d:=nr_d+1;
    rectify:=RectifyRho(I, rho_o, x);
    d[nr_d]:=CreateDClassNC(I, m, o, rectify.m, rho_o, rectify.rep, false);
    x:=rectify.rep; 
    mults:=RhoOrbMults(rho_o, RhoOrbSCCIndex(d[nr_d]));
    cosets:=RhoCosets(d[nr_d]);

    for l in RhoOrbSCC(d[nr_d]) do #install the R-class reps
      if not IsBound(lambdarhoht[l]) then 
        lambdarhoht[l]:=[];
      fi;
      if not IsBound(lambdarhoht[l][m]) then 
        lenreps[m]:=lenreps[m]+1;
        ind:=lenreps[m];
        lambdarhoht[l][m]:=ind;
        repslens[m][ind]:=0;
        reps[m][ind]:=[];
        repslookup[m][ind]:=[];
      else
        ind:=lambdarhoht[l][m];
      fi;
      y:=mults[l][1]*x;

      for z in cosets do 
        nr_r:=nr_r+1;
        
        repslens[m][ind]:=repslens[m][ind]+1;
        reps[m][ind][repslens[m][ind]]:=act(y, z^-1);
        repslookup[m][ind][repslens[m][ind]]:=nr_r;
        
        orblookup1[nr_r]:=ind;
        orblookup2[nr_r]:=repslens[m][ind];

        rholookup[nr_r]:=l; # orb[nr] has rho-value in position l of the rho-orb
        
        orb[nr_r]:=[ I, m, o, reps[m][ind][repslens[m][ind]], false, nr_r ];
        
        htadd(ht, reps[m][ind][repslens[m][ind]], nr_r);

      od;
    od;
  end;

  # initialise the data if necessary
  if data!.init=false then 
    # init the list of reps
    for i in [1..Length(scc)] do 
      reps[i]:=[];
      repslookup[i]:=[];
      repslens[i]:=[];
      lenreps[i]:=0;
    od;
    # add the generators of the ideal...
    for x in GeneratorsOfSemigroupIdeal(I) do 
      add_to_data(x);
    od;

    data!.init:=true;
    if looking then 
      # did we find it?
      for k in [1..nr_r] do 
        if lookfunc(data, orb[k]) then 
          data!.found:=k;
          return data;
        fi;
      od;
    fi;
  fi;
  
  i:=data!.pos;       # points in orb in position at most i have descendants

  while nr_d<=limit and i<nr_d and i<>stopper do 
     
    i:=i+1; # advance in the dorb
    start:=nr_r;
    
    # left/right multiply the R/L-class reps by the generators
    for j in genstoapply do
      for x in RClassReps(d[i]) do
        add_to_data(gens[j]*x);
      od;
      for x in LClassReps(d[i]) do 
        add_to_data(x*gens[j]);
      od;
    od;
    
    if looking then 
      # did we find it?
      for k in [start+1..nr_r] do #check the newly added R-reps
        if lookfunc(data, orb[k]) then 
          data!.found:=k;
          return data;
        fi;
      od;
    fi;

  od;
  
  # for the data-orbit
  data!.pos:=i;
  
  if looking then 
    data!.found:=false;
  fi;
  
  if nr_d=i then 
    SetFilterObj(data, IsClosed);
  fi;

  return data;
end);

