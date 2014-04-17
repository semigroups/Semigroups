############################################################################
##
#W  acting.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# acting semigroups...

# same method for ideals

InstallMethod(SemigroupData, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp], ReturnFail);

# different method for ideals

InstallMethod(SemigroupData, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local gens, data, opts;
 
  gens:=GeneratorsOfSemigroup(s);

  data:=rec(gens:=gens, parent:=s, 
     ht:=HTCreate(gens[1], rec(treehashsize:=s!.opts.hashlen.L)),
     pos:=0, graph:=[EmptyPlist(Length(gens))], init:=false,
     reps:=[], repslookup:=[], orblookup1:=[], orblookup2:=[], rholookup:=[1],
     lenreps:=[0], orbit:=[[,,,FakeOne(gens)]], repslens:=[], lambdarhoht:=[],
     schreierpos:=[fail], schreiergen:=[fail], schreiermult:=[fail],
     genstoapply:=[1..Length(gens)], stopper:=false);
  
  Objectify(NewType(FamilyObj(s), IsSemigroupData), data);
  
  return data;
end);

# different method for regular ideals, regular/inverse semigroups, same method
# for non-regular ideals

InstallMethod(\in, 
"for an associative element and acting semigroup",  
[IsAssociativeElement, IsActingSemigroup], 
function(f, s)
  local data, ht, lambda, lambdao, l, m, rho, rhoo, lambdarhoht, rholookup, lookfunc, new, schutz, ind, reps, repslens, max, lambdaperm, oldrepslens, found, n, i;
  
  if ElementsFamily(FamilyObj(s))<>FamilyObj(f) 
    or (IsActingSemigroupWithFixedDegreeMultiplication(s) 
     and ActionDegree(f)<>ActionDegree(s)) 
    or (ActionDegree(f)>ActionDegree(s)) then 
    return false;
  fi;

  if not (IsMonoid(s) and IsOne(f)) then 
    if Length(Generators(s))>0 
      and ActionRank(s)(f)>MaximumList(List(Generators(s), f-> ActionRank(s)(f)))
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
  ht:=data!.ht;

  # look for lambda!
  lambda:=LambdaFunc(s)(f);
  lambdao:=LambdaOrb(s); 
  if not IsClosed(lambdao) then 
    Enumerate(lambdao, infinity);
  fi;
  
  l:=Position(lambdao, lambda);
    
  if l=fail then 
    return false;
  fi;
  
  # strongly connected component of lambda orb
  m:=OrbSCCLookup(lambdao)[l];

  # make sure lambda of f is in the first place of its scc
  if l<>OrbSCC(lambdao)[m][1] then 
    f:=f*LambdaOrbMult(lambdao, m, l)[2];
  fi;
  
  # check if f is an existing R-rep
  if HTValue(ht, f)<>fail then 
    return true;
  fi;
  
  # check if rho is already known
  rho:=RhoFunc(s)(f); 
  rhoo:=RhoOrb(s);
  l:=Position(rhoo, rho);
  lambdarhoht:=data!.lambdarhoht;
  rholookup:=data!.rholookup;
 
  new:=false; 

  if l=fail then 
  # rho is not already known, so we look for it
    if IsClosed(rhoo) then 
      return false;
    fi;

    lookfunc:=function(data, x) 
      return rhoo[rholookup[x[6]]]=rho;
    end;
  
    data:=Enumerate(data, infinity, lookfunc);
    l:=PositionOfFound(data);

    # rho is not found, so f not in s
    if l=false then 
      return false;
    fi;
    l:=rholookup[l];
    new:=true;
  fi;
  
  if not IsBound(lambdarhoht[l]) or not IsBound(lambdarhoht[l][m]) then 
  # lambda-rho-combination not yet seen
    if IsClosedData(data) then 
      return false;
    fi;
    
    lookfunc:=function(data, x)
      return IsBound(lambdarhoht[l]) and IsBound(lambdarhoht[l][m]);
    end;
    data:=Enumerate(data, infinity, lookfunc);
    if not IsBound(lambdarhoht[l]) or not IsBound(lambdarhoht[l][m]) then 
      return false;
    fi;
    new:=true;
  fi;
 
  schutz:=LambdaOrbStabChain(lambdao, m);
  ind:=lambdarhoht[l][m];
  # the index of the list of reps with same lambda-rho value as f. 

  # if the Schutzenberger group is the symmetric group, then f in s!
  if schutz=true then 
    return true;
  fi;

  reps:=data!.reps;       repslens:=data!.repslens;

  max:=Factorial(LambdaRank(s)(lambda))/Size(LambdaOrbSchutzGp(lambdao, m));
  
  if repslens[m][ind]=max then 
    return true;
  fi;
  
  # if schutz is false, then f has to be an R-rep which it is not...
  if schutz<>false then 
    
    # check if f already corresponds to an element of reps[m][ind]
    lambdaperm:=LambdaPerm(s);
    for n in [1..repslens[m][ind]] do 
      if SiftedPermutation(schutz, lambdaperm(reps[m][ind][n], f))=() then
        return true;
      fi;
    od;
  elif new and HTValue(ht, f)<>fail then 
    return true; 
  fi; 

  if IsClosedData(data) then 
    return false;
  fi;

  # enumerate until we find f or finish 
  if repslens[m][ind]<max then 
    oldrepslens:=repslens[m][ind];
    lookfunc:=function(data, x)
      return repslens[m][ind]>oldrepslens;
    end; 
    if schutz=false then 
      repeat 
        # look for more R-reps with same lambda-rho value
        data:=Enumerate(data, infinity, lookfunc);
        oldrepslens:=repslens[m][ind];
        found:=data!.found;
        if found<>false then 
          if oldrepslens=max or f=data[found][4] then 
            return true;
          fi;
        fi;
      until found=false;
    else 
      repeat
        # look for more R-reps with same lambda-rho value
        data:=Enumerate(data, infinity, lookfunc);
        oldrepslens:=repslens[m][ind];
        found:=data!.found;
        if found<>false then
          if oldrepslens=max then 
            return true;
          fi;
          for i in [n+1..repslens[m][ind]] do 
            if SiftedPermutation(schutz, lambdaperm(reps[m][ind][i], f))=()
             then 
              return true;
            fi;
          od;
          n:=repslens[m][ind];
        fi;
      until found=false;
    fi;
  fi;
  return false;
end);

# different for regular/inverse/ideals

InstallMethod(Size, "for an acting semigroup",
[IsActingSemigroup], 2, #to beat the method for a Rees 0-matrix semigroup
function(s)
  local data, lenreps, repslens, o, scc, size, n, m, i;
   
  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  lenreps:=data!.lenreps;
  repslens:=data!.repslens;
  o:=LambdaOrb(s);
  scc:=OrbSCC(o);
  
  size:=0;
  
  for m in [2..Length(scc)] do 
    n:=Size(LambdaOrbSchutzGp(o, m))*Length(scc[m]);
    for i in [1..lenreps[m]] do 
      size:=size+n*repslens[m][i];
    od;
  od;

  return size; 
end);

# data...

# same method for ideals

InstallMethod(\in, "for associative element and semigroup data",
[IsAssociativeElement, IsSemigroupData],
function(f, data)
  return not Position(data, f)=fail;
end);

# same method for ideals

InstallMethod(ELM_LIST, "for semigroup data, and pos int",
[IsSemigroupData, IsPosInt], 
function(o, nr)
  return o!.orbit[nr];
end);

# same method for ideals

InstallMethod(Length, "for semigroup data", [IsSemigroupData], 
function(data)
  return Length(data!.orbit);
end);

# same method for ideals

InstallMethod(Enumerate, "for semigroup data", [IsSemigroupData],
function(data)
  return Enumerate(data, infinity, ReturnFalse);
end);

# same method for ideals

InstallMethod(Enumerate, "for semigroup data and limit", 
[IsSemigroupData, IsCyclotomic],
function(data, limit)
  return Enumerate(data, limit, ReturnFalse);
end);

# different method for ideals...

#JDM: this has the same problem as orb in Issue #4, the log is not properly
#formed if the enumeration stops early.

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
  
  if IsClosedData(data) then 
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
  gens:=data!.gens; 
  nrgens:=Length(gens); 
  genstoapply:=data!.genstoapply;
  
  # lambda
  s:=data!.parent;
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

  # initialise the data if necessary
  if data!.init=false then 
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
     
    i:=i+1;
    
    #               for the rho-orbit               #
    if rholookup[i]>=rho_depthmarks[rho_depth+1] then
      rho_depth:=rho_depth+1;
      rho_depthmarks[rho_depth+1]:=rho_nr+1;
    fi;
    rho_logind[rholookup[i]]:=rho_logpos; suc:=false;
    #                                               #
    
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
      
        #                update rho-orbit             #
        rho_nr:=rho_nr+1;
        l:=rho_nr;
        rho_orb[rho_nr]:=rhox;
        htadd(rho_ht, rhox, rho_nr);
        
        rho_orbitgraph[rho_nr]:=EmptyPlist(nrgens);
        rho_orbitgraph[rholookup[i]][j]:=rho_nr;

        rho_schreiergen[rho_nr]:=j;
        rho_schreierpos[rho_nr]:=rholookup[i];
        
        suc:=true;
        rho_log[rho_logpos]:=j;
        rho_log[rho_logpos+1]:=rho_nr;
        rho_logpos:=rho_logpos+2;
        rho_o!.logpos:=rho_logpos;
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
  
  if looking then 
    data!.found:=false;
  fi;
  if nr=i then 
    SetFilterObj(data, IsClosedData);
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

# not currently applicable for ideals

InstallMethod(OrbitGraph, "for semigroup data",  
[IsSemigroupData],
function(data)
  return data!.graph;
end);

# not currently applicable for ideals

InstallMethod(OrbitGraphAsSets, "for semigroup data",  
[IsSemigroupData],
function(data)
  return List(data!.graph, Set);
end);

# returns the index of the representative of the R-class containing x in the
# parent of data. Note that this depends on the state of the data, it only tells
# you if it is there already, it doesn't try to find it. 

# same method for ideals

InstallMethod(Position, "for semigroup data and an associative element",
[IsSemigroupData, IsAssociativeElement, IsZeroCyc], 
function(data, x, n)
  local s, o, l, m, val, schutz, lambdarhoht, ind, repslookup, reps, repslens,
  lambdaperm;

  s:=data!.parent;
  o:=LambdaOrb(s);
  l:=Position(o, LambdaFunc(s)(x));
  
  if l=fail then 
    return fail;
  fi;
  
  m:=OrbSCCLookup(o)[l];

  if l<>OrbSCC(o)[m][1] then 
    x:=x*LambdaOrbMult(o, m, l)[2];
  fi; 
  
  val:=HTValue(data!.ht, x);
  schutz:=LambdaOrbStabChain(o, m);
  
  if val<>fail then 
    return val;
  elif schutz=false then 
    return fail;
  fi;
  
  l:=Position(RhoOrb(s), RhoFunc(s)(x));
  
  if l=fail then 
    return fail;
  fi;
  
  lambdarhoht:=data!.lambdarhoht;

  if not IsBound(lambdarhoht[l]) or not IsBound(lambdarhoht[l][m]) then 
    return fail;
  fi;

  ind:=lambdarhoht[l][m];
  repslookup:=data!.repslookup[m][ind];

  if schutz=true then 
    return repslookup[1];
  fi;

  reps:=data!.reps[m][ind]; repslens:=data!.repslens[m][ind];

  lambdaperm:=LambdaPerm(s);
  for n in [1..repslens] do 
    if SiftedPermutation(schutz, lambdaperm(reps[n], x))=() then 
      return repslookup[n];
    fi;
  od;

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

# same method for ideals

InstallMethod(SizeOfSemigroupData, "for semigroup data", [IsSemigroupData],
function(data)
  local lenreps, repslens, o, scc, size, n, m, i;
  
  if not data!.init then 
    return 0;
  fi;
  
  lenreps:=data!.lenreps;      repslens:=data!.repslens;
  o:=LambdaOrb(data!.parent);  scc:=OrbSCC(o);
  
  size:=0;
  for m in [2..Length(scc)] do 
    n:=Size(LambdaOrbSchutzGp(o, m))*Length(scc[m]);
    for i in [1..lenreps[m]] do 
      size:=size+n*repslens[m][i];
    od;
  od;
  return size; 
end);

# different method for ideals

InstallMethod(ViewObj, [IsSemigroupData], 
function(data)
  Print("<");

  if IsClosedData(data) then 
    Print("closed ");
  else 
    Print("open ");
  fi;
  Print("semigroup ");

  Print("data with ", Length(data!.orbit)-1, " reps, ",
   Length(LambdaOrb(data!.parent))-1, " lambda-values, ", 
   Length(RhoOrb(data!.parent))-1, " rho-values>"); 
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
