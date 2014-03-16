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
     reps:=[], repslookup:=[], orblookup1:=[], orblookup2:=[], rholookup:=[1],
     lenreps:=[0], orbit:=[[,,,FakeOne(gens)]], dorbit:=[], repslens:=[],
     lambdarhoht:=[], schreierpos:=[fail], schreiergen:=[fail],
     schreiermult:=[fail], genstoapply:=[1..Length(gens)], stopper:=false);
  
  Objectify(NewType(FamilyObj(s), IsSemigroupData and IsAttributeStoringRep),
   data);
  
  SetParent(data, s);
  return data;
end);

# We concentrate on the case when nothing is known about the parent of the
# ideal.

# we make the R-class centered data structure as in SemigroupData but at the
# same time have an additional "orbit" consisting of D-class reps. 

InstallMethod(Enumerate, 
"for an semigroup data, limit, and func",
[IsSemigroupData, IsCyclotomic, IsFunction],
function(data, limit, lookfunc)
  local looking, ht, orb, nr, i, graph, reps, repslens, lenreps, lambdarhoht, repslookup, orblookup1, orblookup2, rholookup, stopper, schreierpos, schreiergen, schreiermult, gens, nrgens, genstoapply, s, lambda, lambdaact, lambdaperm, o, oht, scc, lookup, rho, rho_o, rho_orb, rho_nr, rho_ht, rho_schreiergen, rho_schreierpos, rho_log, rho_logind, rho_logpos, rho_depth, rho_depthmarks, rho_orbitgraph, htadd, htvalue, suc, x, pos, m, rhox, l, pt, ind, schutz, data_val, old, j, n;
 
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
  d:=data!.dorbit;    # the so far found D-classes
  nr:=Length(d);
  i:=data!.pos;       # points in orb in position at most i have descendants
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
  s:=Parent(data);
  lambda:=LambdaFunc(s);
  lambdaact:=LambdaAct(s);  
  lambdaperm:=LambdaPerm(s);

  o:=LambdaOrb(s);
  oht:=o!.ht;
  scc:=OrbSCC(o); 
  lookup:=o!.scc_lookup;
 
  # rho
  rho_ht:=GradedRhoHT(s);
 
  # initialise the data if necessary
  if data!.init=false then 
    #add the generators of the ideal here!! 
    for i in [2..Length(scc)] do 
      reps[i]:=[];
      repslookup[i]:=[];
      repslens[i]:=[];
      lenreps[i]:=0;
    od;
    data!.init:=true;
    i:=data!.pos;
  fi;
 
  if IsBoundGlobal("ORBC") then 
    htadd:=HTAdd_TreeHash_C;
    htvalue:=HTValue_TreeHash_C;
  else
    htadd:=HTAdd;
    htvalue:=HTValue;
  fi;
  
  while nr<=limit and i<nr and i<>stopper do 
     
    i:=i+1; # advance in the dorb
    
    # left multiply the R-class reps by the generators
    for x in RClassReps(d[i]) do
      for j in genstoapply do 
        x:=gens[j]*x;
        pos:=Position(data, x);
        if pos=fail then # new D-class
          # add the new D-class to the dorb
          # install the RClassReps of the new D-class in the orb
          # update everything for all of these...
        else             # old D-class
          # do everything expected in SemigroupData for non-ideals

        fi;

      od;
 
    od;
    
    # right multiply the L-class reps by the generators
    for x in LClassReps(d[i]) do 
      for j in genstoapply do 
        x:=x*gens[j];
        pos:=Position(data, x);
        if pos=fail then # new D-class
          # add the new D-class to the dorb
          # install the RClassReps of the new D-class in the orb
          # update everything for all of these...
        else             # old D-class
          # do everything expected in SemigroupData for non-ideals

        fi;
      od;
    od;

    for j in genstoapply do #JDM
      x:=gens[j]*orb[i][4];
      pos:=htvalue(oht, lambda(x)); 
      m:=lookup[pos];   #lambda-value-scc-index

      #put lambda(x) in the first position in its scc
      if pos<>scc[m][1] then 
        x:=x*LambdaOrbMult(o, m, pos)[2];
      fi;
      
      rhox:=rho(x); 
      l:=htvalue(rho_ht, rhox);

      if l=fail then #new rho-value, new R-rep
      
        #                                              #
    
        nr:=nr+1;
        lenreps[m]:=lenreps[m]+1;
        ind:=lenreps[m];
        lambdarhoht[l]:=[];
        lambdarhoht[l][m]:=ind;
        
        reps[m][ind]:=[x];
        repslookup[m][ind]:=[nr];
        repslens[m][ind]:=1;
        
        orblookup1[nr]:=ind;
        orblookup2[nr]:=1;

        pt:=[s, m, o, x, false, nr];
        # semigroup, lambda orb scc index, lambda orb, rep,
        # IsGreensClassNC, index in orbit

      elif not IsBound(lambdarhoht[l]) then 
        # old rho-value, but new lambda-rho-combination

        # update rho orbit graph
        rho_orbitgraph[rholookup[i]][j]:=l;
        
        nr:=nr+1;
        lenreps[m]:=lenreps[m]+1;
        ind:=lenreps[m];
        lambdarhoht[l]:=[];
        lambdarhoht[l][m]:=ind;
        
        reps[m][ind]:=[x];
        repslookup[m][ind]:=[nr];
        repslens[m][ind]:=1;
        
        orblookup1[nr]:=ind;
        orblookup2[nr]:=1;

        pt:=[s, m, o, x, false, nr];
      elif not IsBound(lambdarhoht[l][m]) then 
        # old rho-value, but new lambda-rho-combination

        # update rho orbit graph
        rho_orbitgraph[rholookup[i]][j]:=l;
        
        nr:=nr+1;
        lenreps[m]:=lenreps[m]+1;
        ind:=lenreps[m];
        lambdarhoht[l][m]:=ind;
        
        reps[m][ind]:=[x];
        repslookup[m][ind]:=[nr];
        repslens[m][ind]:=1;
        
        orblookup1[nr]:=ind;
        orblookup2[nr]:=1;

        pt:=[s, m, o, x, false, nr];
      else 
      # old lambda-rho combination
        ind:=lambdarhoht[l][m];
        pt:=[s, m, o, x, false, nr+1];
        
        #check membership in Schutzenberger group via stabiliser chain
        schutz:=LambdaOrbStabChain(o, m);

        if schutz=true then 
        # the Schutzenberger group is the symmetric group
          graph[i][j]:=repslookup[m][ind][1];
          rho_orbitgraph[rholookup[i]][j]:=l;
          continue;
        else
          if schutz=false then 
          # the Schutzenberger group is trivial
            data_val:=htvalue(ht, x);
            if data_val<>fail then 
              graph[i][j]:=data_val;
              rho_orbitgraph[rholookup[i]][j]:=l;
              continue;
            fi;
          else 
          # the Schutzenberger group is neither trivial nor symmetric group
            old:=false; 
            for n in [1..repslens[m][ind]] do 
              if SiftedPermutation(schutz, lambdaperm(reps[m][ind][n], x))=() then
                old:=true;
                graph[i][j]:=repslookup[m][ind][n]; 
                rho_orbitgraph[rholookup[i]][j]:=l;
                break;
              fi;
            od;
            if old then 
              continue;
            fi;
          fi;
          nr:=nr+1;
          repslens[m][ind]:=repslens[m][ind]+1;
          reps[m][ind][repslens[m][ind]]:=x;
          repslookup[m][ind][repslens[m][ind]]:=nr;
          orblookup1[nr]:=ind;
          orblookup2[nr]:=repslens[m][ind];
          
          # update rho orbit graph and rholookup
          rho_orbitgraph[rholookup[i]][j]:=l;
        fi;
      fi;
      rholookup[nr]:=l; # orb[nr] has rho-value in position l of the rho-orb
      
      orb[nr]:=pt;
      schreierpos[nr]:=i; # orb[nr] is obtained from orb[i]
      schreiergen[nr]:=j; # by multiplying by gens[j]
      schreiermult[nr]:=pos; # and ends up in position <pos> of 
                             # its lambda orb
      htadd(ht, x, nr);
      graph[nr]:=EmptyPlist(nrgens);
      graph[i][j]:= nr;
      
      # are we looking for something?
      if looking then 
        # did we find it?
        if lookfunc(data, pt) then 
          data!.pos:=i-1;
          data!.found:=nr;
          data!.lenreps:=lenreps;
          rho_o!.depth:=rho_depth;
          return data;
        fi;
      fi;
    od;

    # for the rho-orbit
    if suc then 
      rho_log[rho_logpos-2]:=-rho_log[rho_logpos-2];
    else
      rho_logind[rholookup[i]]:=0;
    fi;

  od;
  
  # for the data-orbit
  data!.pos:=i;
  data!.lenreps:=lenreps;
  if looking then 
    data!.found:=false;
  fi;
  if nr=i then 
    SetFilterObj(data, IsClosed);
    SetFilterObj(rho_o, IsClosed);
    rho_o!.orbind:=[1..rho_nr];
  fi;

  #for the rho-orbit
  if i<>0 then 
    rho_o!.pos:=rholookup[i];
    rho_o!.depth:=rho_depth;
  fi;
  
  return data;
end);

