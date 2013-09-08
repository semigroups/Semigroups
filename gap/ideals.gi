############################################################################# 
## 
#W  ideals.gi
#Y  Copyright (C) 2013                                    James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

InstallMethod(ViewObj, "for a semigroup ideal with generators",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal], 1,
function(S)
  Print(ViewString(S));
end);

#

InstallMethod(ViewString, "for a semigroup ideal with generators",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(S)
  local str;
  
  str:="<ideal of ";
  Append(str, ViewString(Parent(S)));
  Append(str, " with ");
  Append(str, String(Length(GeneratorsOfSemigroupIdeal(S))));
  Append(str, " generator");
  if Length(GeneratorsOfSemigroupIdeal(S))>1 then 
    Append(str, "s");
  fi;
  Append(str, ">");
  return str;
end);

#

InstallMethod(\., "for a semigroup ideal with generators and pos int",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal, IsPosInt],
function(S, n)
  S:=GeneratorsOfSemigroupIdeal(S);
  n:=NameRNam(n);
  n:=Int(n);
  if n=fail or Length(S)<n then
    Error("usage: the second argument <n> should be a positive integer\n",
     "not greater than the number of generators of the semigroup <S> in\n", 
     "the first argument,");
    return;
  fi;
  return S[n];
end);

# a convenience, similar to the function <Semigroup>...

InstallGlobalFunction(SemigroupIdeal, 
function( arg )
  local out, i;

  if not IsSemigroup(arg[1]) then 
    Error("usage: the first argument should be a semigroup,");
    return;
  fi;

  if Length(arg)=1 then 
    Error("usage: there must be a second argument, which specifies\n",
    "the ideal you are trying to create,");
    return;
  fi;

  # special case for matrices, because they may look like lists
  if Length( arg ) = 2 and IsMatrix( arg[2] )  then
    return SemigroupIdealByGenerators(arg[1],  [arg[2]]);

  # list of generators
  elif Length(arg)=2 and IsList(arg[2]) and 0 < Length(arg[2]) then
    return SemigroupIdealByGenerators(arg[1], arg[2]);
  
  # generators and collections of generators
  elif IsAssociativeElement(arg[2]) 
   or IsAssociativeElementCollection(arg[2]) then
    out:=[];
    for i in [2..Length(arg)] do
      if IsAssociativeElement(arg[i]) then
        Add(out, arg[i]);
      elif IsAssociativeElementCollection(arg[i]) then
        if HasGeneratorsOfSemigroup(arg[i]) then
          Append(out, GeneratorsOfSemigroup(arg[i]));
        elif HasGeneratorsOfSemigroupIdeal(arg[i]) then 
          Append(out, GeneratorsOfSemigroupIdeal(arg[i]));
        elif IsList(arg[i]) then 
          Append(out, arg[i]);
        else 
          Append(out, AsList(arg[1])); #JDM should use this in Semigroup too
        fi;
      #so that we can pass the options record in the Semigroups package 
      #elif i=Length(arg[2]) and IsRecord(arg[2][i]) then
      #  return SemigroupIdealByGenerators(out, arg[2][i]);
      else
        Error( "usage: the second argument should be some\n",
        "combination of generators, lists of generators, or semigroups,");
        return;
      fi;
    od;
    return SemigroupIdealByGenerators(arg[1], out);
  # no argument given, error
  else
    Error( "usage: the second argument should be some\n",
    "combination of generators, lists of generators, or semigroups,");
    return;
  fi;
end);
#

InstallMethod(MagmaIdealByGenerators,
"for an acting semigroup and collection of its elements", 
IsIdenticalObj, 
[IsActingSemigroup, IsAssociativeElementCollection],
SemigroupIdealByGenerators);

#

InstallMethod(SemigroupIdealByGenerators,
"for an acting semigroup and associative element collection", 
IsIdenticalObj, 
[IsActingSemigroup, IsAssociativeElementCollection],
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

#JDM here

InstallMethod(SemigroupData, "for an acting semigroup ideal",
[IsActingSemigroup and IsSemigroupIdeal],
function(I)
  local gens, nrgens, rep, data, ht, orb, nr, graph, reps, repslookup, orblookup1, orblookup2, repslens, lenreps, schreierpos, schreiergen, schreiermult, lambda, lambdaact, lambdaperm, rho, lambdarhoht, o, oht, scc, lookup, htadd, htvalue, lamx, pos, m, y, rhoy, val, x, schutz, tmp, old, parent, genstoapply, n, j;

  nrgens:=Length(GeneratorsOfSemigroup(ParentAttr(I)));
  rep:=Representative(I);

  data:=rec();

  ht:=HTCreate(rep, rec(treehashsize:=I!.opts.hashlen.L));
  orb:=[EmptyPlist(0)];
  nr:=1;
  graph:=[EmptyPlist(nrgens)];
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
  # install the generators of the ideal, this is more complicated since 
  # some of them maybe R-related, we must also install descendants of generators
  # of the parent semigroup
  gens:=GeneratorsOfMagmaIdeal(I);
  for x in gens do 
    lamx:=lambda(x);
    pos:=htvalue(oht, lamx);
    m:=lookup[pos];
    if pos<>scc[m][1] then
      y:=x*LambdaOrbMult(o, m, pos)[2];
    else
      y:=x;
    fi;
    rhoy:=[m, rho(y)];
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

  parent:=GeneratorsOfSemigroup(ParentAttr(I));
  genstoapply:=[1..Length(gens)];
  #install every descendant of  
  for x in parent do 
    for j in genstoapply do
      x:=gens[j]*x;
      lamx:=lambda(x);
      pos:=htvalue(oht, lamx);
      m:=lookup[pos];
      if pos<>scc[m][1] then
        y:=x*LambdaOrbMult(o, m, pos)[2];
      else
        y:=x;
      fi;
      rhoy:=[m, rho(y)];
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
  od;
  
  data:=rec(gens:=parent,
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
