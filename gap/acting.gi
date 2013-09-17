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

InstallMethod(SemigroupData, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, one, data, val;
 
  gens:=GeneratorsOfSemigroup(s);
  one:=One(gens);

  data:=rec(gens:=gens, 
     ht:=HTCreate(one, rec(treehashsize:=s!.opts.hashlen.L)),
     pos:=0, graph:=[EmptyPlist(Length(gens))], 
     reps:=[], repslookup:=[], orblookup1:=[], orblookup2:=[],
     lenreps:=0, orbit:=[[,,,one]], repslens:=[], 
     schreierpos:=[fail], schreiergen:=[fail], schreiermult:=[fail],
     genstoapply:=[1..Length(gens)], stopper:=false);
  
  # hash table of all valid lambda-rho values found so far, HTValue of
  # LambdaRhoHT points to where the existing R-class reps with same lambda-rho
  # value are in SemigroupData(s).reps.  
  
  val:=Concatenation([1], RhoFunc(s)(Representative(s)));
  data.lambdarhoht:=HTCreate(val, 
   rec(treehashsize:=s!.opts.hashlen.M, forflatplainlists:=true));

  #

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
  local data, len, ht, val, lambda, o, l, lookfunc, m, scc, lambdarho, schutz,
  g, reps, repslens, lambdaperm, n, max, found;
  
  if ElementsFamily(FamilyObj(s))<>FamilyObj(f) then 
    Error("the element and semigroup are not of the same type,");
    return;
  fi;

  if HasAsSSortedList(s) then 
    return f in AsSSortedList(s); 
  fi;

  if IsActingSemigroupWithFixedDegreeMultiplication(s) 
    and ActionDegree(f)<>ActionDegree(s) then 
    return false;
  #elif IsTransformation(f) and ActionDegree(f)<>ActionDegree(s) then 
  #  f:=AsTransformation(f, ActionDegree(s));
  #  # if AsTransformation doesn't work, then <f> is unchanged.
  #  if ActionDegree(f)<>ActionDegree(s) then 
  #    return false;
  #  fi;
  fi;

  
  # add degree here, since that applies to both partial perms and
  # transformations JDM
  if not (IsMonoid(s) and IsOne(f)) and 
   ActionRank(f) > MaximumList(List(Generators(s), ActionRank)) then
    Info(InfoSemigroups, 2, "element has larger rank than any element of ",
     "semigroup.");
    return false;
  fi;

  # add degree here, since that applies to both partial perms and
  # transformations JDM
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
  if HTValue(ht, f)<>fail then 
    return true;
  fi;

  lambda:=LambdaFunc(s)(f);

  # look for lambda!
  o:=LambdaOrb(s);
  l:=EnumeratePosition(o, lambda, false);
    
  if l=fail then 
    return false;
  fi;
  
  # strongly connected component of lambda orb
  m:=OrbSCCLookup(o)[l];
  scc:=OrbSCC(o);

  # check if lambdarho is already known
  lambdarho:=[m];
  Append(lambdarho, RhoFunc(s)(f));
  val:=HTValue(data!.lambdarhoht, lambdarho);

  lookfunc:=function(data, x) 
    return Concatenation([x[2]], RhoFunc(s)(x[4]))=lambdarho;
  end;
  
  # if lambdarho is not already known, then look for it
  if val=fail then 
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
    for n in [1..repslens[val]] do 
      if SiftedPermutation(schutz, lambdaperm(reps[val][n], g))=() then
        return true;
      fi;
    od;
  fi; 
 
  if IsClosed(data) then 
    return false;
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

#

InstallMethod(Size, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
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
  local looking, ht, orb, nr, i, graph, reps, repslookup, orblookup1, orblookup2, repslens, lenreps, stopper, schreierpos, schreiergen, schreiermult, gens, nrgens, genstoapply, s, lambda, lambdaact, lambdaperm, rho, lambdarhoht, o, oht, scc, lookup, htadd, htvalue, x, lamx, pos, m, y, rhoy, val, schutz, tmp, old, j, n;
 
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
  reps:=data!.reps;   # reps grouped by equal lambda and rho value
                      # HTValue(lambdarhoht, [lambda(x), rho(x)])
  lambdarhoht:=data!.lambdarhoht;
  
  repslookup:=data!.repslookup; # Position(orb, reps[i][j])=repslookup[i][j]
                                # = HTValue(ht, reps[i][j])
  
  orblookup1:=data!.orblookup1; # orblookup1[i] position in reps containing 
                                # orb[i][4] (the R-rep)

  orblookup2:=data!.orblookup2; # orblookup2[i] position in reps[orblookup1[i]] 
                                # containing orb[i][4] (the R-rep)

  repslens:=data!.repslens;     # Length(reps[i])=repslens[i] 
  lenreps:=data!.lenreps;       # lenreps=Length(reps)

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
      lamx:=lambda(x);
      pos:=htvalue(oht, lamx); 

      #find the scc
      m:=lookup[pos];

      #put lambda x in the first position in its scc
      if pos<>scc[m][1] then 
        y:=x*LambdaOrbMult(o, m, pos)[2];
      else
        y:=x;
      fi;
      #rhoy:=[m, rho(y)];
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
  m:=OrbSCCLookup(o)[l];
  scc:=OrbSCC(o);

  val:=HTValue(data!.lambdarhoht, Concatenation([m], RhoFunc(s)(x)));
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
  Print("<semigroup data: ", Length(data!.orbit)-1, " reps, ",
  Length(data!.reps), " lambda-rho values>");
  return;
end);

#EOF
