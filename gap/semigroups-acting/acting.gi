############################################################################
##
#W  acting.gi
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# acting semigroups...

InstallMethod(SemigroupData, 
"for an acting semigroup with inverse op and generators",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup],
function(S)
  return fail;
end);

#

InstallMethod(SemigroupData, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, data, opts;
 
  gens:=GeneratorsOfSemigroup(s);

  data:=rec(gens:=gens, 
     ht:=HTCreate(gens[1], rec(treehashsize:=s!.opts.hashlen.L)),
     pos:=0, graph:=[EmptyPlist(Length(gens))], init:=false,
     reps:=[], repslookup:=[], orblookup1:=[], orblookup2:=[], rho_lookup:=[1],
     lenreps:=[], orbit:=[[,,,FakeOne(gens)]],
     schreierpos:=[fail], schreiergen:=[fail], schreiermult:=[fail],
     genstoapply:=[1..Length(gens)], stopper:=false);
  
  # hash table of all valid rho values found so far, HTValue[m] of
  # LambdaRhoHT points to where the existing R-class reps with same lambda-rho
  # value are in SemigroupData(s).reps[m].  
  
  opts:=ShallowCopy(RhoOrbOpts(s));
  opts.treehashsize:=s!.opts.hashlen.M;
  data.lambdarhoht:=HTCreate(RhoFunc(s)(Representative(s)), opts);

  Objectify(NewType(FamilyObj(s), IsSemigroupData and IsAttributeStoringRep),
   data);
  
  SetParent(data, s);
  return data;
end);

#

InstallMethod(\in, 
"for an associative element and acting semigroup with generators",
[IsAssociativeElement, IsActingSemigroup and HasGeneratorsOfSemigroup], 
function(f, s)
  local data, len, ht, lambda, o, l, m, scc, rho, oo, val, reps, lookfunc, schutz, lenreps, max, lambdaperm, found, n, i;
  
  if ElementsFamily(FamilyObj(s))<>FamilyObj(f) 
    or (IsActingSemigroupWithFixedDegreeMultiplication(s) 
     and ActionDegree(f)<>ActionDegree(s)) 
    or (ActionDegree(f)>ActionDegree(s)) then 
    return false;
  fi;

  if not (IsMonoid(s) and IsOne(f)) then 
    if ActionRank(s)(f)>MaximumList(List(Generators(s), f-> ActionRank(s)(f)))
     then
      Info(InfoSemigroups, 2, "element has larger rank than any element of ",
       "semigroup.");
      return false;
    fi;
  fi;

  if HasMinimalIdeal(s) then 
    if ActionRank(s)(f)<ActionRank(s)(Representative(MinimalIdeal(s))) then
      Info(InfoSemigroups, 2, "element has smaller rank than any element of ",
       "semigroup.");
      return false;
    fi;
  fi;  

  data:=SemigroupData(s);
  len:=Length(data!.orbit);  
  ht:=data!.ht;

  # check if f is an existing R-rep
  if HTValue(ht, f)<>fail then 
    return true;
  fi;

  lambda:=LambdaFunc(s)(f);

  # look for lambda!
  o:=LambdaOrb(s); 
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

  # check if rho is already known
  
  rho:=RhoFunc(s)(f); 
  oo:=RhoOrb(s);
  val:=Position(oo, rho);
  reps:=data!.reps;

  # if rho is not already known, then look for it
  if val=fail then 
    if IsClosed(oo) then 
      return false;
    fi;

    lookfunc:=function(data, x) 
      val:=Position(oo, rho);
      return val<>fail;
    end;
  
    data:=Enumerate(data, infinity, lookfunc);

    # rho not found, so f not in s
    if val=fail then 
      return false;
    fi;
  elif not IsBound(reps[m][val]) then 
    if IsClosed(data) then 
      return false;
    fi;

    lookfunc:=function(data, x)
      return IsBound(reps[m][val]);
    end;

    data:=Enumerate(data, infinity, lookfunc);
    if data!.found=false then 
      return false;
    fi;
  fi;

  schutz:=LambdaOrbStabChain(o, m);
  
  # if the schutz gp is the symmetric group, then f in s!
  if schutz=true then 
    return true;
  fi;

  reps:=data!.reps; lenreps:=data!.lenreps;

  max:=Factorial(LambdaRank(s)(lambda))/Size(LambdaOrbSchutzGp(o, m));
 
  if lenreps[m][val]=max then 
    return true;
  fi;

  # make sure lambda of f is in the first place of its scc
  if l<>scc[m][1] then 
    f:=f*LambdaOrbMult(o, m, l)[2];
  fi;

  # check if anything changed
  if len<Length(data!.orbit) or l<>scc[m][1] then 

    # check again if f is an R-class rep.
    if HTValue(ht, f)<>fail then
      return true;
    fi;
  fi;
  
  # if schutz is false, then f has to be an R-rep which it is not...
  if schutz<>false then 
    
    # check if f already corresponds to an element of reps[val]
    lambdaperm:=LambdaPerm(s);
    for n in [1..lenreps[m][val]] do 
      if SiftedPermutation(schutz, lambdaperm(reps[m][val][n], f))=() then
        return true;
      fi;
    od;
  fi; 

  if IsClosed(data) then 
    return false;
  fi;
  
  len:=lenreps[m][val];

  lookfunc:=function(o, x)
    return lenreps[m][val]>len;
  end;

  # enumerate until we find f or the number of elts in reps[m][val] exceeds max
  if lenreps[m][val]<max then# doesn't this have to be try JDM  
    if schutz=false then 
      repeat 
        # look for more R-reps with same lambda-rho value
        data:=Enumerate(data, infinity, lookfunc);
        len:=lenreps[m][val];
        found:=data!.found;
        if found<>false then 
          if f=data[found][4] then 
            return true;
          fi;
        fi;
        
      until found=false or lenreps[m][val]>=max;
    else 
      repeat
        
        # look for more R-reps with same lambda-rho value
        data:=Enumerate(data, infinity, lookfunc);
        found:=data!.found;
        if found<>false then 
          for i in [n+1..lenreps[m][val]] do 
            if SiftedPermutation(schutz, lambdaperm(reps[m][val][i], f))=()
             then 
              return true;
            fi;
          od;
          n:=lenreps[m][val];
        fi;
      until found=false or lenreps[m][val]>=max;
    fi;
  fi;
  return false;
end);

#

InstallMethod(Size, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local data, lenreps, o, scc, r, n, m, i;
   
  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  lenreps:=data!.lenreps;
  o:=LambdaOrb(s);
  scc:=OrbSCC(o);
  
  r:=0;
  for m in [2..Length(scc)] do 
    n:=Size(LambdaOrbSchutzGp(o, m))*Length(scc[m]);
    for i in lenreps[m] do 
      r:=r+i*n;
    od;
  od;

  return r; 
end);

# data...

InstallMethod(\in, "for associative element and semigroup data",
[IsAssociativeElement, IsSemigroupData],
function(f, data)
  return not Position(data, f)=fail;
end);
#

InstallMethod(ELM_LIST, "for semigroup data, and pos int",
[IsSemigroupData, IsPosInt], 
function(o, nr)
  return o!.orbit[nr];
end);

#

InstallMethod(Enumerate, "for semigroup data", [IsSemigroupData],
function(data)
  return Enumerate(data, infinity, ReturnFalse);
end);

#

InstallMethod(Enumerate, "for semigroup data and limit", 
[IsSemigroupData, IsCyclotomic],
function(data, limit)
  return Enumerate(data, limit, ReturnFalse);
end);

#

InstallMethod(Enumerate, 
"for an semigroup data, limit, and func",
[IsSemigroupData, IsCyclotomic, IsFunction],
function(data, limit, lookfunc)
  local looking, ht, orb, nr, i, graph, reps, repslookup, orblookup1, orblookup2,  lenreps,  stopper, schreierpos, schreiergen, schreiermult, gens, nrgens, genstoapply, s, lambda, lambdaact, lambdaperm, o, oht, scc, lookup, rho, rho_o, rho_orb, rho_nr, rho_ht, rho_schreiergen, rho_schreierpos, rho_log, rho_logind, rho_logpos, rho_depth, rho_depthmarks, rho_orbitgraph, rho_lookup, htadd, htvalue, suc, x, pos, m, rhox, val, pt, schutz, data_val, old, j, n;
 
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
  nr:=Length(orb);
  i:=data!.pos;       # points in orb in position at most i have descendants
  graph:=data!.graph; # orbit graph of orbit of R-classes under left mult 
  reps:=data!.reps;   # reps grouped by equal lambda scc index and rho value
  
  repslookup:=data!.repslookup; # Position(orb, reps[m][i][j])
                                # = repslookup[m][i][j]
                                # = HTValue(ht, reps[m][i][j])
  
  orblookup1:=data!.orblookup1; # orblookup1[i] position in reps[m] containing 
                                # orb[i][4] (the R-rep)

  orblookup2:=data!.orblookup2; # orblookup2[i] position in 
                                # reps[m][orblookup1[i]] 
                                # containing orb[i][4] (the R-rep)

  lenreps:=data!.lenreps;       # lenreps[m]=Length(reps[m])

  stopper:=data!.stopper;       # stop at this place in the orbit

  # schreier
  schreierpos:=data!.schreierpos;
  schreiergen:=data!.schreiergen;
  schreiermult:=data!.schreiermult;

  # generators
  gens:=data!.gens; 
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
  
  #rho
  rho:=RhoFunc(s);
  rho_o:=RhoOrb(s);
  rho_orb:=rho_o!.orbit;
  rho_nr:=Length(rho_orb);
  rho_ht:=rho_o!.ht;
  rho_schreiergen:=rho_o!.schreiergen;
  rho_schreierpos:=rho_o!.schreierpos;
  
  rho_log:=rho_o!.log;
  rho_logind:=rho_o!.logind;
  rho_logpos:=rho_o!.logpos;
  rho_depth:=rho_o!.depth;
  rho_depthmarks:=rho_o!.depthmarks;

  rho_orbitgraph:=rho_o!.orbitgraph;

  rho_lookup:=data!.rho_lookup; # rho_lookup[i]=Position(rho_o, rho(o[i][4]))

  # initialise the data if necessary
  if data!.init=false then 
    for i in [2..Length(scc)] do 
      reps[i]:=[];
      repslookup[i]:=[];
      lenreps[i]:=[];
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
     
    i:=i+1;
    
    # for the rho-orbit
    if rho_lookup[i]>=rho_depthmarks[rho_depth+1] then
      rho_depth:=rho_depth+1;
      rho_depthmarks[rho_depth+1]:=rho_nr+1;
    fi;
    rho_logind[rho_lookup[i]]:=rho_logpos; suc:=false;
    #
    
    for j in genstoapply do #JDM
      x:=gens[j]*orb[i][4];
      pos:=htvalue(oht, lambda(x)); 

      #find the scc
      m:=lookup[pos];

      #put lambda(x) in the first position in its scc
      if pos<>scc[m][1] then 
        x:=x*LambdaOrbMult(o, m, pos)[2];
      fi;
      
      rhox:=rho(x); 
      val:=htvalue(rho_ht, rhox);

      if val=fail then #new rho-value, new R-rep
      
        # deal with rho
        rho_nr:=rho_nr+1;
        val:=rho_nr;
        rho_orb[rho_nr]:=rhox;
        htadd(rho_ht, rhox, rho_nr);

        rho_orbitgraph[rho_nr]:=EmptyPlist(nrgens);
        rho_orbitgraph[rho_lookup[i]][j]:=rho_nr;

        rho_schreiergen[rho_nr]:=j;
        rho_schreierpos[rho_nr]:=rho_lookup[i];
        
        suc:=true;
        rho_log[rho_logpos]:=j;
        rho_log[rho_logpos+1]:=rho_nr;
        rho_logpos:=rho_logpos+2;
        rho_o!.logpos:=rho_logpos;
        
        # deal with R-class reps
        nr:=nr+1;
        lenreps[m][val]:=1;
        reps[m][val]:=[x];
        repslookup[m][val]:=[nr];
        orblookup1[nr]:=lenreps[m];
        orblookup2[nr]:=1;
        pt:=[s, m, o, x, false, nr];
        # semigroup, lambda orb scc index, lambda orb, rep,
        # IsGreensClassNC, index in orbit

      elif not IsBound(reps[m][val]) then 
      # old rho-value, but new rho-lambda-combination
       
        nr:=nr+1;
        lenreps[m][val]:=1;
        reps[m][val]:=[x];
        repslookup[m][val]:=[nr];
        orblookup1[nr]:=lenreps[m];
        orblookup2[nr]:=1;
        pt:=[s, m, o, x, false, nr];
        
        # update rho orbit graph
        rho_orbitgraph[rho_lookup[i]][j]:=val;
      
      else              
      # old rho value
        
        pt:=[s, m, o, x, false, nr+1];
        
        #check membership in Schutzenberger group via stabiliser chain
        schutz:=LambdaOrbStabChain(o, m);

        if schutz=true then 
        # the Schutzenberger group is the symmetric group
          graph[i][j]:=repslookup[m][val][1];
          rho_orbitgraph[rho_lookup[i]][j]:=val;
          continue;
        else
          if schutz=false then 
          # the Schutzenberger group is trivial
            data_val:=htvalue(ht, x);
            if data_val<>fail then 
              graph[i][j]:=data_val;
              rho_orbitgraph[rho_lookup[i]][j]:=val;
              continue;
            fi;
          else 
          # the Schutzenberger group is neither trivial nor symmetric group
            old:=false; 
            for n in [1..lenreps[m][val]] do 
              if SiftedPermutation(schutz, lambdaperm(reps[m][val][n], x))=()
                then
                old:=true;
                graph[i][j]:=repslookup[m][val][n]; 
                rho_orbitgraph[rho_lookup[i]][j]:=val;
                break;
              fi;
            od;
            if old then 
              continue;
            fi;
          fi;
          nr:=nr+1;
          lenreps[m][val]:=lenreps[m][val]+1;
          reps[m][val][lenreps[m][val]]:=x;
          repslookup[m][val][lenreps[m][val]]:=nr;
          orblookup1[nr]:=val;
          orblookup2[nr]:=lenreps[m][val];
          
          # update rho orbit graph and rho_lookup
          rho_orbitgraph[rho_lookup[i]][j]:=val;
        fi;
      fi;
      # orb[nr] has rho-value in rho_o[rho_nr]
      rho_lookup[nr]:=val;
      
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
        if lookfunc(data, x) then 
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
      rho_logind[rho_lookup[i]]:=0;
    fi;

  od;
  
  # for the data-orbit
  data!.pos:=i;
  if looking then 
    data!.found:=false;
  fi;
  if nr=i then 
    SetFilterObj(data, IsClosed);
    SetFilterObj(rho_o, IsClosed);
    rho_o!.orbind:=[1..rho_nr];
  fi;

  #for the rho-orbit
  rho_o!.pos:=rho_lookup[i];
  rho_o!.depth:=rho_depth;
  
  return data;
end);

# JDM this is stored as an attribute, unfortunately..

InstallMethod(Length, "for semigroup data", [IsSemigroupData], 
x-> Length(x!.orbit));

#

InstallMethod(OrbitGraphAsSets, "for semigroup data",  
[IsSemigroupData], 99,
function(data)
  return List(data!.graph, Set);
end);

# returns the index of the representative of the R-class containing x in the
# parent of data. 

InstallMethod(Position, "for semigroup data and an associative element",
[IsSemigroupData, IsAssociativeElement, IsZeroCyc], 100,
function(data, x, n)
  local val, s, o, l, m, scc, schutz, repslookup, y, reps, repslens, lambdaperm;

  val:=HTValue(data!.ht, x);

  if val<>fail then 
    return val;
  fi;

  s:=Parent(data);
  o:=LambdaOrb(s);

  if not IsClosed(o) then 
    Enumerate(o, infinity);
  fi;
  
  l:=Position(o, LambdaFunc(s)(x));
  
  if l=fail then 
    return fail;
  fi;
  
  m:=OrbSCCLookup(o)[l];
  scc:=OrbSCC(o);

  val:=HTValue(data!.lambdarhoht, RhoFunc(s)(x));
  if val=fail or not IsBound(val[m]) then 
    return fail;
  fi;

  schutz:=LambdaOrbStabChain(o, m);
  repslookup:=data!.repslookup[m][val[m]];

  if schutz=true then 
    return repslookup[1];
  fi;
 
  if l<>scc[m][1] then 
    y:=x*LambdaOrbMult(o, m, l)[2];
  else
    y:=x;
  fi; 

  reps:=data!.reps[m][val[m]]; repslens:=data!.repslens[m][val[m]];

  if schutz=false then 
    return HTValue(data!.ht, y);
  else
    lambdaperm:=LambdaPerm(s);
    for n in [1..repslens] do 
      if SiftedPermutation(schutz, lambdaperm(reps[n], y))=() then 
        return repslookup[n];
      fi;
    od;
  fi; 
  return fail;
end);

#

InstallMethod(PositionOfFound,"for semigroup data",
[IsSemigroupData],
function( data )
  if not(data!.looking) then
    Error("not looking for anything,");
    return fail;
  fi;
  return data!.found;
end);

#

InstallGlobalFunction(SizeOfSemigroupData,
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

#

InstallMethod(ViewObj, [IsSemigroupData], 999,
function(data)
  Print("<");

  if IsClosed(data) then 
    Print("closed ");
  else 
    Print("open ");
  fi;
  Print("semigroup data with ", Length(data!.orbit)-1, " reps>");
  return;
end);

# we require a fake one in the case that the objects we are dealing with don't
# have one.

InstallMethod(String, "for the universal fake one",
[IsUniversalFakeOne], 
function(obj)
  return "<universal fake one>";
end);

InstallMethod(\*, 
"for the universal fake one and an associative element",
[IsUniversalFakeOne, IsAssociativeElement],
function(x, y)
  return y;
end);

InstallMethod(\*, 
"for an associative element and the universal fake one",
[IsAssociativeElement, IsUniversalFakeOne],
function(x, y)
  return x;
end);

InstallMethod(\<, "for the universal fake one and an associative element",
[IsUniversalFakeOne, IsAssociativeElement], ReturnTrue);

InstallMethod(\<, "for an associative element and the universal fake one",
[IsAssociativeElement, IsUniversalFakeOne], ReturnFalse);

#EOF
