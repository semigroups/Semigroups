############################################################################# 
## 
#W  ideals.gi
#Y  Copyright (C) 2013                                    James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

#

InstallMethod(MagmaIdealByGenerators,
"for an acting semigroup and collection of its elements", 
IsIdenticalObj, 
[IsActingSemigroup, IsAssociativeElementWithActionCollection],
SemigroupIdealByGenerators);

#

InstallMethod(SemigroupIdealByGenerators,
"for an acting semigroup and collection of its elements", 
IsIdenticalObj, 
[IsActingSemigroup, IsAssociativeElementWithActionCollection],
function( M, gens )
local S;
    
    S:= Objectify( NewType( FamilyObj( gens ), IsMagmaIdeal and
     IsAttributeStoringRep and IsActingSemigroup ),
     rec(opts:=SemigroupsOptionsRec));
    
    SetGeneratorsOfMagmaIdeal( S, AsList( gens ) );
    SetIsSemigroupIdeal(S, true);
    SetParent(S, M);

    return S;
end );

# JDM move to lib

InstallMethod(\=, "for semigroup ideals", 
[IsSemigroupIdeal and HasGeneratorsOfMagmaIdeal, 
 IsSemigroupIdeal and HasGeneratorsOfMagmaIdeal],
function(I, J)
  
  if Parent(I)=Parent(J) then 
    return ForAll(GeneratorsOfMagmaIdeal(I), x-> x in J) and
    ForAll(GeneratorsOfMagmaIdeal(J), x-> x in I);
  elif HasGeneratorsOfSemigroup(I) and HasGeneratorsOfSemigroup(J) then 
    return ForAll(GeneratorsOfSemigroup(I), x-> x in J) and
     ForAll(GeneratorsOfSemigroup(J), x-> x in I); 
  else
    return AsSSortedList(I)=AsSSortedList(J);
  fi;

end);

#

InstallTrueMethod(IsSemigroupIdeal, IsMagmaIdeal and IsActingSemigroup);

#

InstallMethod(Representative, "for a semigroup ideal", 
[IsSemigroupIdeal and HasGeneratorsOfMagmaIdeal],
function(I)
return Representative(GeneratorsOfMagmaIdeal(I));
end);

#

InstallMethod(SemigroupData, "for an acting semigroup ideal",
[IsActingSemigroup and IsSemigroupIdeal],
function(I)
  local gens, nrgens, rep, data, ht, orb, nr, i, graph, reps, repslookup, orblookup1, orblookup2, repslens, lenreps, schreierpos, schreiergen, schreiermult, lambda, lambdaact, lambdaperm, rho, lambdarhoht, o, oht, scc, lookup, htadd, htvalue, lamx, pos, m, y, rhoy, val, x, schutz, tmp, old, stopper, n;

  gens:=GeneratorsOfSemigroup(ParentAttr(I));
  nrgens:=Length(gens);
  rep:=Representative(I);

  data:=rec();

  ht:=HTCreate(rep, rec(treehashsize:=I!.opts.hashlen.L));
  orb:=[EmptyPlist(0)];
  nr:=1;
  graph:=[EmptyPlist(Length(gens))];
  reps:=[];
  repslookup:=[];
  orblookup1:=[];
  orblookup2:=[];
  repslens:=[];
  lenreps:=0;
  schreierpos:=[fail];
  schreiergen:=[fail];
  schreiermult:=[fail];

  lambda:=LambdaFunc(I);
  lambdaact:=LambdaAct(I);
  lambdaperm:=LambdaPerm(I);
  rho:=RhoFunc(I);
  lambdarhoht:=LambdaRhoHT(I);

  o:=LambdaOrb(I);
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
  
  for x in GeneratorsOfMagmaIdeal(I) do 
    lamx:=lambda(x);
    pos:=htvalue(oht, lamx);
    m:=lookup[pos];
    if pos<>scc[m][1] then
      y:=x*LambdaOrbMult(o, m, pos)[2];
    else
      y:=x;
    fi;
    rhoy:=[m];
    Append(rhoy, rho(y));;
    val:=htvalue(lambdarhoht, rhoy);
    if val=fail then  #new rho value, and hence new R-rep
      lenreps:=lenreps+1;
      htadd(lambdarhoht, rhoy, lenreps);
      nr:=nr+1;
      reps[lenreps]:=[y];
      repslookup[lenreps]:=[nr];
      orblookup1[nr]:=lenreps;
      orblookup2[nr]:=1;
      repslens[lenreps]:=1;
      x:=[I, m, o, y, false, nr];
      # semigroup, lambda orb data, lambda orb, rep, index in orbit,
      # position of reps with equal lambda-rho value

    else              # old rho value
      x:=[I, m, o, y, false, nr+1];

      # JDM expand!
      schutz:=LambdaOrbStabChain(o, m);

      #check membership in schutz gp via stab chain

      if schutz=true then # schutz gp is symmetric group
        continue;
      else
        if schutz=false then # schutz gp is trivial
          tmp:=htvalue(ht, y);
          if tmp<>fail then
            continue;
          fi;
        else # schutz gp neither trivial nor symmetric group
          old:=false;
          for n in [1..repslens[val]] do
            if SiftedPermutation(schutz, lambdaperm(reps[val][n], y))=() then
              old:=true;
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
    schreierpos[nr]:=fail; 
    schreiergen[nr]:=fail; # by multiplying by gens[j]
    schreiermult[nr]:=fail; # and ends up in position <pos> of 
                           # its lambda orb
    htadd(ht, y, nr);
    graph[nr]:=EmptyPlist(nrgens);
  od;

  data:=rec(gens:=gens,
     ht:=ht, pos:=1, graph:=graph,
     reps:=reps, repslookup:=repslookup, orblookup1:=orblookup1, 
     orblookup2:=orblookup2,
     lenreps:=lenreps, orbit:=orb, repslens:=repslens,
     schreierpos:=schreierpos, schreiergen:=schreiergen, 
     schreiermult:=schreiermult,
     genstoapply:=[1..nrgens], stopper:=false);

  Objectify(NewType(FamilyObj(I), IsSemigroupData and IsAttributeStoringRep),
   data);

  SetParent(data, I);
  return data;
end);
