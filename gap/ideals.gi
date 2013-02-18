
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

  if IsBound(HTAdd_TreeHash_C) then
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

  SetParentSemigroup(data, I);
  return data;
end);
