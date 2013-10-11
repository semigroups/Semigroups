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
     reps:=[], repslookup:=[], orblookup1:=[], orblookup2:=[],
     lenreps:=[], orbit:=[[,,,FakeOne(gens)]], repslens:=[], 
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
  local data, len, ht, lambda, o, l, m, scc, rho, val, lookfunc, schutz, g, reps, repslens, lambdaperm, max, found, n, i;
  
  if ElementsFamily(FamilyObj(s))<>FamilyObj(f) 
    or (IsActingSemigroupWithFixedDegreeMultiplication(s) 
     and ActionDegree(f)<>ActionDegree(s)) 
    or (ActionDegree(f)>ActionDegree(s)) then 
    return false;
  fi;

  #if HasAsSSortedList(s) then 
  #  return f in AsSSortedList(s); 
  #fi;
  
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
  val:=HTValue(data!.lambdarhoht, rho);

  lookfunc:=function(data, x) 
    return x[2]=m and RhoFunc(s)(x[4])=rho;
  end;
  
  # if lambdarho is not already known, then look for it
  if val=fail or not IsBound(val[m]) then 
    if IsClosed(data) then 
      return false;
    fi;
  
    data:=Enumerate(data, infinity, lookfunc);
    val:=data!.found; # position in data!.orbit 

    # lambdarho not found, so f not in s
    if val=false then 
      return false;
    fi;
    val:=data!.orblookup1[val]; 
    # the index of the list of reps with same lambdarho value as f. 
    # = HTValue(LambdaRhoHT(s), lambdarho);
  else 
    val:=val[m];
  fi;

  schutz:=LambdaOrbStabChain(o, m);

  # if the schutz gp is the symmetric group, then f in s!
  if schutz=true then 
    return true;
  fi;

  # make sure lambda of f is in the first place of its scc
  if l<>scc[m][1] then 
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
    for n in [1..repslens[m][val]] do 
      if SiftedPermutation(schutz, lambdaperm(reps[m][val][n], g))=() then
        return true;
      fi;
    od;
  fi; 
 
  if IsClosed(data) then 
    return false;
  fi;

  # enumerate until we find f or the number of elts in reps[m][val] exceeds max
  max:=Factorial(LambdaRank(s)(lambda))/Size(LambdaOrbSchutzGp(o, m));

  if repslens[m][val]<max then 
    if schutz=false then 
      repeat 
        # look for more R-reps with same lambda-rho value
        data:=Enumerate(data, infinity, lookfunc);
        found:=data!.found;
        if found<>false then 
          if g=data[found][4] then 
            return true;
          fi;
        fi;
      until found=false or repslens[m][val]>=max;
    else 
      repeat
        
        # look for more R-reps with same lambda-rho value
        data:=Enumerate(data, infinity, lookfunc);
        found:=data!.found;
        if found<>false then 
          reps:=data!.reps; repslens:=data!.repslens;
          for i in [n+1..repslens[m][val]] do 
            if SiftedPermutation(schutz, lambdaperm(reps[m][val][i], g))=()
             then 
              return true;
            fi;
          od;
          n:=repslens[m][val];
        fi;
      until found=false or repslens[m][val]>=max;
    fi;
  fi;
  return false;
end);

#

InstallMethod(Size, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local data, lenreps, repslens, o, scc, r, n, m, i, j;
   
  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  lenreps:=data!.lenreps;
  repslens:=data!.repslens;
  o:=LambdaOrb(s);
  scc:=OrbSCC(o);
  
  r:=0;

  for m in [2..Length(scc)] do 
    n:=Size(LambdaOrbSchutzGp(o, m))*Length(scc[m]);
    for i in [1..lenreps[m]] do 
      for j in [1..repslens[m][i]] do 
        r:=r+n;
      od;
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
  local looking, ht, orb, nr, i, graph, reps, lambdarhoht, repslookup, orblookup1, orblookup2, repslens, lenreps, stopper, schreierpos, schreiergen, schreiermult, gens, nrgens, genstoapply, s, lambda, lambdaact, lambdaperm, rho, o, oht, scc, lookup, htadd, htvalue, x, pos, m, y, rhoy, val, tmp, schutz, old, j, n;
 
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
                      # HTValue(lambdarhoht, rho(x))[m]
  lambdarhoht:=data!.lambdarhoht;
  
  repslookup:=data!.repslookup; # Position(orb, reps[m][i][j])
                                # = repslookup[m][i][j]
                                # = HTValue(ht, reps[m][i][j])
  
  orblookup1:=data!.orblookup1; # orblookup1[i] position in reps[m] containing 
                                # orb[i][4] (the R-rep)

  orblookup2:=data!.orblookup2; # orblookup2[i] position in 
                                # reps[m][orblookup1[i]] 
                                # containing orb[i][4] (the R-rep)

  repslens:=data!.repslens;     # Length(reps[m][i])=repslens[m][i] 
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
  
  # lambda/rho
  s:=Parent(data);
  lambda:=LambdaFunc(s);
  lambdaact:=LambdaAct(s);  
  lambdaperm:=LambdaPerm(s);
  rho:=RhoFunc(s);

  o:=LambdaOrb(s);
  oht:=o!.ht;
  scc:=OrbSCC(o); 
  lookup:=o!.scc_lookup;
  
  if data!.init=false then #init these lists
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
    for j in genstoapply do #JDM
      x:=gens[j]*orb[i][4];
      pos:=htvalue(oht, lambda(x)); 

      #find the scc
      m:=lookup[pos];

      #put lambda x in the first position in its scc
      if pos<>scc[m][1] then 
        y:=x*LambdaOrbMult(o, m, pos)[2];
      else
        y:=x;
      fi;
      rhoy:=rho(y); 
      val:=htvalue(lambdarhoht, rhoy);

      # this is what we keep if it is new
      # x:=[s, m, o, y, nr+1];

      if val=fail or not IsBound(val[m]) then  
        #new rho value, and hence new R-rep
        lenreps[m]:=lenreps[m]+1;
        if val=fail then 
          val:=[];
          val[m]:=lenreps[m];
          htadd(lambdarhoht, rhoy, val);
        else 
          val[m]:=lenreps[m];
        fi;
        nr:=nr+1;
        reps[m][lenreps[m]]:=[y];
        repslookup[m][lenreps[m]]:=[nr];
        repslens[m][lenreps[m]]:=1;
        orblookup1[nr]:=lenreps[m];
        orblookup2[nr]:=1;
        x:=[s, m, o, y, false, nr];
        # semigroup, lambda orb scc index, lambda orb, rep,
        # IsGreensClassNC, index in orbit,
    

      else              # old rho value
        val:=val[m];
        x:=[s, m, o, y, false, nr+1];
        # JDM expand!
        schutz:=LambdaOrbStabChain(o, m);
        
        #check membership in schutz gp via stab chain
        
        if schutz=true then # schutz gp is symmetric group
          graph[i][j]:=repslookup[m][val][1];
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
            for n in [1..repslens[m][val]] do 
              if SiftedPermutation(schutz, lambdaperm(reps[m][val][n], y))=()
                then
                old:=true;
                graph[i][j]:=repslookup[m][val][n]; 
                break;
              fi;
            od;
            if old then 
              continue;
            fi;
          fi;
          nr:=nr+1;
          repslens[m][val]:=repslens[m][val]+1;
          reps[m][val][repslens[m][val]]:=y;
          repslookup[m][val][repslens[m][val]]:=nr;
          orblookup1[nr]:=val;
          orblookup2[nr]:=repslens[m][val];
        fi;
      fi;
      # add reporting here!!
      #Print("found ", nr, "             R-class reps\r");
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
  #Length(data!.reps), " lambda-rho values>");
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
