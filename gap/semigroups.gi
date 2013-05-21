############################################################################# 
## 
#W  semigroups.gi 
#Y  Copyright (C) 2011-12                                 James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

# basic things

InstallMethod(Generators, "for a semigroup with generators",
[IsSemigroup],
function(s)

  if HasGeneratorsOfMagmaIdeal(s) then 
    return GeneratorsOfMagmaIdeal(s);
  elif HasGeneratorsOfInverseMonoid(s) then 
    return GeneratorsOfInverseMonoid(s);
  elif HasGeneratorsOfInverseSemigroup(s) then 
    return GeneratorsOfInverseSemigroup(s);
  elif HasGeneratorsOfMonoid(s) then
    return GeneratorsOfMonoid(s);
  elif HasGeneratorsOfSemigroup(s) then 
    return GeneratorsOfSemigroup(s);
  fi;

  return fail;
end);

#

InstallMethod(ViewString, "for a group of transformations",
[IsTransformationSemigroup and IsGroupAsSemigroup],
function(s)
  local str, nrgens;

  str:="\><";
  if HasIsTrivial(s) and IsTrivial(s) then
    Append(str, "\>trivial\< ");
  fi;

  Append(str, "\>transformation\< \>group\< ");
  if HasIsTrivial(s) and not IsTrivial(s) and HasSize(s) and Size(s)<2^64 then
    Append(str, "\>of size\> ");
    Append(str, String(Size(s)));
    Append(str, ",\<\< ");
  fi;

  nrgens:=Length(Generators(s));
  
  Append(str, "\>on \>");
  Append(str, ViewString(NormalizedDegreeOfTransformationSemigroup(s)));
  Append(str, "\< pts with\> ");
  Append(str, ViewString(nrgens));
  Append(str, "\< generator");

  if nrgens>1 or nrgens=0 then
    Append(str, "s\<");
  else
    Append(str, "\<");
  fi;
  Append(str, ">\<");

  return str;
end);

#

# creating semigroups, monoids, inverse semigroups, etc

InstallMethod(MagmaByGenerators, 
"for an associative element with action collection",
[IsAssociativeElementWithActionCollection], SemigroupByGenerators);

#

InstallMethod(SemigroupByGenerators, 
"for an associative element with action collection",
[IsAssociativeElementWithActionCollection],
function(gens)
   return SemigroupByGenerators(gens, SemigroupsOptionsRec);
end);

#

InstallMethod(SemigroupByGenerators, 
"for an associative element with action collection and record",
[IsAssociativeElementWithActionCollection, IsRecord],
function(gens, opts)
  local deg, n, i, closure_opts, s, filts, pos, f;

  opts:=SemigroupOptions(opts);
  gens:=ShallowCopy(gens);
  
  # all generators must have equal degree!
  if Length(gens)>1 and IsTransformationCollection(gens) then
    deg:=DegreeOfTransformationCollection(gens);
    Apply(gens, x-> AsTransformation(x, deg));
  fi;

  # try to find a smaller generating set
  if opts.small and Length(gens)>1 then 
    gens:=SSortedList(gens); #remove duplicates 
    gens:=Permuted(gens, Random(SymmetricGroup(Length(gens))));;
    Sort(gens, function(x, y) return ActionRank(x)>ActionRank(y); end);;

    n:=ActionDegree(gens);

    #remove the identity
    if IsOne(gens[1]) and IsBound(gens[2]) and ActionRank(gens[2])=n then
      Remove(gens, 1);
    fi;

    i:=0;
    closure_opts:=rec(small:=false, hashlen:=opts.hashlen);
    s:=Semigroup(gens[1], closure_opts);

    if InfoLevel(InfoSemigroups)>1 then
      n:=Length(gens);
      for i in [1..n] do
        if not gens[i] in s then
          s:=ClosureSemigroupNC(s, [gens[i]], closure_opts);
        fi;
        Print("at \t", i, " of \t", n, "; \t", Length(Generators(s)),
        " generators so far\n");
      od;
      Print("\n");
    else
      for f in gens do
        if not f in s then
          s:=ClosureSemigroupNC(s, [f], closure_opts);
        fi;
      od;
    fi;
    return s;
  fi;

  filts:=IsSemigroup and IsAttributeStoringRep;

  if opts.acting then 
    filts:=filts and IsActingSemigroup;
  fi;

  s:=Objectify( NewType( FamilyObj( gens ), filts ), rec(opts:=opts));
  
  if opts.regular then 
    SetIsRegularSemigroup(s, true);
  fi;
 
  SetGeneratorsOfMagma( s, AsList( gens ) );

  if IsMultiplicativeElementWithOneCollection(gens) 
   and CanEasilyCompareElements(gens) then
    pos:=Position(gens, One(gens));
    if pos<>fail then 
      SetFilterObj(s, IsMonoid);
      gens:=ShallowCopy(gens);
      Remove(gens, pos);
      SetGeneratorsOfMonoid(s, gens);
    fi;
  fi; 

  return s;
end);

#

InstallMethod(MonoidByGenerators, 
"for an associative element collection",
[IsAssociativeElementWithActionCollection],
function(gens)
  return MonoidByGenerators(gens, SemigroupsOptionsRec);
end);

#

InstallMethod(MonoidByGenerators, 
"for an asssociative element with action collection and record",
[IsAssociativeElementWithActionCollection, IsRecord],
function(gens, record)
  local deg, n, i, closure_opts, s, filts, f;
  
  record:=SemigroupOptions(record);
  gens:=ShallowCopy(gens);

  # all generators must have equal degree!
  if IsTransformationCollection(gens) then
    deg:=DegreeOfTransformationCollection(gens);
    Apply(gens, x-> AsTransformation(x, deg));
  fi;
  
  if record.small and Length(gens)>1 then #small gen. set
    gens:=SSortedList(gens); #remove duplicates 
    gens:=Permuted(gens, Random(SymmetricGroup(Length(gens))));;
    Sort(gens, function(x, y) return ActionRank(x)>ActionRank(y); end);;

    n:=ActionDegree(gens);
    if IsOne(gens[1]) and IsBound(gens[2]) and ActionRank(gens[2])=n then
      #remove id
      Remove(gens, 1);
    fi;

    i:=0;
    closure_opts:=rec(small:=false, hashlen:=record.hashlen);
    s:=Monoid(gens[1], closure_opts);

    if InfoLevel(InfoSemigroups)>1 then
      n:=Length(gens);
      for i in [1..n] do
        if not gens[i] in s then 
          s:=ClosureSemigroupNC(s, [gens[i]], closure_opts);
        fi;
        Print("at \t", i, " of \t", n, "; \t", Length(Generators(s)),
        " generators so far");
      od;
      Print("\n");
    else
      for f in gens do
        if not f in s then 
          s:=ClosureSemigroupNC(s, [f], closure_opts);
        fi;
      od;
    fi;
    return s;
  fi;    

  filts:=IsMonoid and IsAttributeStoringRep;

  if record.acting then 
    filts:=filts and IsActingSemigroup;
  fi;

  s:=Objectify( NewType( FamilyObj( gens ), filts ), rec(opts:=record));

  if record.regular then 
    SetIsRegularSemigroup(s, true);
  fi;

  SetGeneratorsOfMagmaWithOne( s, gens );
  return s;
end);

#

InstallMethod(InverseMonoidByGenerators, 
"for associative element with semigroup inverse and action collection", 
[IsAssociativeElementWithUniqueSemigroupInverseCollection and
IsAssociativeElementWithActionCollection],
function(gens)
  return InverseMonoidByGenerators(gens, SemigroupsOptionsRec);
end);

#

InstallMethod(InverseSemigroupByGenerators, 
"for associative element with semigroup inverse and action collection", 
[IsAssociativeElementWithUniqueSemigroupInverseCollection and
IsAssociativeElementWithActionCollection],
function(gens)
  return InverseSemigroupByGenerators(gens, SemigroupsOptionsRec);
end);

#

InstallMethod(InverseMonoidByGenerators, 
"for associative element with semigroup inverse and action coll, and record",  
[IsAssociativeElementWithUniqueSemigroupInverseCollection and
IsAssociativeElementWithActionCollection, IsRecord],
function(gens, record)
  local closure_opts, s, filts, one, f;

  record:=SemigroupOptions(record);

  if record.small and Length(gens)>1 then 
    gens:=SSortedList(ShallowCopy(gens));
    gens:=Permuted(gens, Random(SymmetricGroup(Length(gens))));;
    Sort(gens, function(x, y) return ActionRank(x)>ActionRank(y); end);;
    
    closure_opts:=rec(small:=false, hashlen:=record.hashlen);
    s:=InverseMonoid(gens[1], closure_opts);
    
    for f in gens do
      if not f in s then 
        s:=ClosureInverseSemigroupNC(s, [f], closure_opts);
      fi;
    od;
    return s;
  fi;

  filts:=IsMagmaWithOne and IsInverseSemigroup and IsAttributeStoringRep;
  
  if record.acting then 
    filts:=filts and IsActingSemigroupWithInverseOp;
  fi;

  s:=Objectify( NewType (FamilyObj( gens ), filts), rec(opts:=record));

  SetGeneratorsOfInverseMonoid(s, AsList(gens));
  
  one:=One(gens); 
  if not one in gens then 
    Add(gens, one);
  fi;
  
  SetGeneratorsOfInverseSemigroup(s, AsList(gens));
  return s;
end);

#

InstallMethod(InverseSemigroupByGenerators, 
"for partial perm coll, partial perm coll, and record",
[IsAssociativeElementWithUniqueSemigroupInverseCollection and
IsAssociativeElementWithActionCollection, IsRecord],
function(gens, record)
  local closure_opts, s, filts, f;

  record:=SemigroupOptions(record);
  
  if record.small and Length(gens)>1 then 
    gens:=SSortedList(ShallowCopy(gens));
    gens:=Permuted(gens, Random(SymmetricGroup(Length(gens))));;
    Sort(gens, function(x, y) return ActionRank(x)>ActionRank(y); end);;
    
    closure_opts:=rec(small:=false, hashlen:=record.hashlen);
    s:=InverseSemigroup(gens[1], closure_opts);
    for f in gens do
      if not f in s then 
        s:=ClosureInverseSemigroupNC(s, [f], closure_opts);
      fi;
    od;
    return s;
  fi;
  
  filts:=IsMagma and IsInverseSemigroup and IsAttributeStoringRep;
  
  if record.acting then 
    filts:=filts and IsActingSemigroupWithInverseOp;
  fi;

  s:=Objectify( NewType (FamilyObj( gens ), filts), rec(opts:=record));

  SetGeneratorsOfInverseSemigroup(s, gens);
  return s;
end);

# closure

InstallMethod(ClosureInverseSemigroup, 
"for acting semigroup with inverse op and an associative element coll",
[IsActingSemigroupWithInverseOp, IsAssociativeElementWithActionCollection],
function(s, coll) 
  return ClosureInverseSemigroup(s, coll, s!.opts);
end);

#

InstallMethod(ClosureInverseSemigroup, 
"for acting semigroup with inverse op and an associative element coll",
[IsActingSemigroupWithInverseOp, IsAssociativeElementWithAction],
function(s, f) 
  return ClosureInverseSemigroup(s, [f], s!.opts);
end);

#

InstallMethod(ClosureInverseSemigroup, 
"for acting semigroup with inverse op and an associative element coll",
[IsActingSemigroupWithInverseOp, IsAssociativeElementWithAction, IsRecord],
function(s, f, record) 
  return ClosureInverseSemigroup(s, [f], record);
end);

#

InstallMethod(ClosureInverseSemigroup, 
"for an acting semigroup with inverse op, ass. elt. coll, and record",
[IsActingSemigroupWithInverseOp, IsAssociativeElementWithActionCollection, IsRecord],
function(s, coll, record)
  local n;

  if not ElementsFamily(FamilyObj(s))=FamilyObj(Representative(coll)) then 
    Error("the semigroup and collection of elements are not of the same type,");
    return;
  fi;

  if IsSemigroup(coll) then 
    coll:=GeneratorsOfSemigroup(coll);
  fi;

  return ClosureInverseSemigroupNC(s, Filtered(coll, x-> not x in s),
   SemigroupOptions(record));
end);

#

InstallGlobalFunction(ClosureInverseSemigroupNC,
function(s, coll, record)
  local t, coll_copy, o, f;
 
  if coll=[] then
    Info(InfoSemigroups, 2, "the elements in the collection belong to the ",
    " semigroup,");
    return s;
  fi;

  coll_copy:=Set(ShallowCopy(coll));
  for f in coll do
    if not f^-1 in coll then
      Add(coll_copy, f^-1);
    fi;
  od;

  o:=StructuralCopy(LambdaOrb(s));
  AddGeneratorsToOrbit(o, coll_copy);

  #should be a case split here for semigroups and monoids JDM
  t:=InverseSemigroupByGenerators(
   Concatenation(GeneratorsOfInverseSemigroup(s), coll), record);

  #remove everything related to strongly connected components
  Unbind(o!.scc); Unbind(o!.trees); Unbind(o!.scc_lookup);
  Unbind(o!.mults); Unbind(o!.schutz); Unbind(o!.reverse);
  Unbind(o!.rev); Unbind(o!.truth); Unbind(o!.schutzstab); Unbind(o!.slp);

  o!.parent:=t;
  o!.scc_reps:=[One(Generators(t))];
  
  SetLambdaOrb(t, o);
  return t;
end);

#

InstallMethod(ClosureSemigroup, 
"for an acting semigroup and associative element with action coll",
[IsActingSemigroup, IsAssociativeElementWithActionCollection],
function(s, coll)
  return ClosureSemigroup(s, coll, s!.opts);
end);

#

InstallMethod(ClosureSemigroup, 
"for an acting semigroup and associative element with action coll",
[IsActingSemigroup, IsList and IsEmpty],
function(s, coll)
  return s;
end);

#

InstallMethod(ClosureSemigroup, 
"for an acting semigroup and associative element with action",
[IsActingSemigroup, IsAssociativeElementWithAction],
function(s, f)
  return ClosureSemigroup(s, [f], s!.opts);
end);

#

InstallMethod(ClosureSemigroup, 
"for an acting semigroup and associative element with action",
[IsActingSemigroup, IsAssociativeElementWithAction, IsRecord],
function(s, f, record)
  return ClosureSemigroup(s, [f], SemigroupOptions(record));
end);

#

InstallMethod(ClosureSemigroup, 
"for an acting semigroup, associative element with action coll, and record",
[IsActingSemigroup, IsAssociativeElementWithActionCollection, IsRecord],
function(s, coll, record)
  
  if not ElementsFamily(FamilyObj(s))=FamilyObj(Representative(coll)) then 
    Error("the semigroup and collection of elements are not of the same type,");
    return;
  fi;

  record.small:=false;

  if IsActingSemigroup(coll) then 
    coll:=Generators(coll);
  fi;

  if IsActingSemigroupWithFixedDegreeMultiplication(s) and
    ActionDegree(s)<>ActionDegree(Representative(coll)) then 
    Error("usage: the degree of the semigroup and collection must be equal,");
    return;
  fi;

  return ClosureSemigroupNC(s, Filtered(coll, x-> not x in s),
   SemigroupOptions(record));
end);

# coll should consist of elements not in s

InstallGlobalFunction(ClosureSemigroupNC,
function(s, coll, opts)
  local t, old_o, o, new_data, old_data, max_rank, ht, new_orb, old_orb, new_nr, old_nr, graph, old_graph, reps, repslookup, orblookup1, orblookup2, repslens, lenreps, new_schreierpos, old_schreierpos, new_schreiergen, old_schreiergen, new_schreiermult, old_schreiermult, gens, nr_new_gens, nr_old_gens, lambda, lambdaact, lambdaperm, rho, lambdarhoht, oht, scc, old_scc, lookup, old_lookup, old_to_new, htadd, htvalue, i, x, pos, m, rank, y, rhoy, val, schutz, tmp, old, j, n;
 
  if coll=[] then 
    Info(InfoSemigroups, 2, "all the elements in the collection belong to the ",
    " semigroup,");
    return s;
  fi;
  

  # init the semigroup or monoid
  if IsMonoid(s) then 
    t:=Monoid(s, coll, opts);
  else
    t:=Semigroup(s, coll, opts);
  fi;
  
  # if nothing is known about s, then return t
  if not HasLambdaOrb(s) then 
    return t;
  fi;
  
  # set up lambda orb for t
  old_o:=LambdaOrb(s);
  o:=StructuralCopy(old_o);
  AddGeneratorsToOrbit(o, coll);

  # unbind everything related to strongly connected components, since 
  # even if the orbit length doesn't change the strongly connected components
  # might
  Unbind(o!.scc); Unbind(o!.trees); Unbind(o!.scc_lookup);
  Unbind(o!.mults); Unbind(o!.schutz); Unbind(o!.reverse); 
  Unbind(o!.rev); Unbind(o!.truth); Unbind(o!.schutzstab); Unbind(o!.slp); 
  
  o!.parent:=t;
  o!.scc_reps:=[One(Generators(t))];

  SetLambdaOrb(t, o); 
  
  if not HasSemigroupData(s) or SemigroupData(s)!.pos=0 then 
    return t;
  fi;
  
  # get new and old R-rep orbit data
  new_data:=SemigroupData(t);
  old_data:=SemigroupData(s);
  max_rank:=MaximumList(List(coll, ActionRank)); 

  ht:=new_data!.ht;       
  # so far found R-reps
  
  new_orb:=new_data!.orbit;   
  old_orb:=old_data!.orbit;   
  # the so far found R-reps data 
  
  new_nr:=Length(new_orb);
  old_nr:=Length(old_orb);
  # points in orb in position at most i have descendants
  
  graph:=new_data!.graph; 
  old_graph:=old_data!.graph;
  graph[1]:=ShallowCopy(old_graph[1]);
  # orbit graph of orbit of R-classes under left mult 

  reps:=new_data!.reps;   
  # reps grouped by equal lambda and rho value 
  # HTValue(lambdarhoht, Concatenation(lambda(x), rho(x))
  
  repslookup:=new_data!.repslookup; 
  # Position(orb, reps[i][j])=repslookup[i][j] = HTValue(ht, reps[i][j])
 
  orblookup1:=new_data!.orblookup1; 
  # orblookup1[i] position in reps containing orb[i][4] (the R-rep)
  
  orblookup2:=new_data!.orblookup2;
  # orblookup2[i] position in reps[orblookup1[i]] 
  # containing orb[i][4] (the R-rep)

  repslens:=new_data!.repslens;
  # Length(reps[i])=repslens[i] 

  lenreps:=new_data!.lenreps;       
  # lenreps=Length(reps)
  
  # schreier
  new_schreierpos:=new_data!.schreierpos;
  old_schreierpos:=old_data!.schreierpos;
  new_schreiergen:=new_data!.schreiergen;
  old_schreiergen:=old_data!.schreiergen;
  new_schreiermult:=new_data!.schreiermult;
  old_schreiermult:=old_data!.schreiermult;

  # generators
  gens:=new_data!.gens;
  nr_new_gens:=Length(gens);
  nr_old_gens:=Length(old_data!.gens);

  # lambda/rho
  lambda:=LambdaFunc(s);
  lambdaact:=LambdaAct(s);
  lambdaperm:=LambdaPerm(s);
  rho:=RhoFunc(s);
  lambdarhoht:=LambdaRhoHT(t);

  oht:=o!.ht;
  scc:=OrbSCC(o);
  old_scc:=OrbSCC(old_o);
  lookup:=o!.scc_lookup; 
  old_lookup:=old_o!.scc_lookup;
  
  # look up for old_to_new[i]:=Position(new_orb, old_orb[i]);
  # i.e. position of old R-rep in new_orb
  
  old_to_new:=EmptyPlist(old_nr);
  old_to_new[1]:=1;

  if IsBound(HTAdd_TreeHash_C) then
    htadd:=HTAdd_TreeHash_C;
    htvalue:=HTValue_TreeHash_C;
  else
    htadd:=HTAdd;
    htvalue:=HTValue;
  fi;

  i:=1;   
  
  # install old R-class reps in new_orb
  while new_nr<=old_nr and i<old_nr do
    i:=i+1;
    x:=old_orb[i][4];
    pos:=old_schreiermult[i];
    m:=lookup[pos];
    rank:=ActionRank(x);
    
    if rank>max_rank or scc[m][1]=old_scc[old_lookup[pos]][1] then 
      y:=x;
    elif pos=old_scc[old_lookup[pos]][1] then 
      y:=x*LambdaOrbMult(o, m, pos)[2];
    else
      # x has rectified lambda value but pos refers to the unrectified value
      y:=x*LambdaOrbMult(old_o, old_lookup[pos], pos)[1]
       *LambdaOrbMult(o, m, pos)[2];
    fi;
   
    rhoy:=[m];
    Append(rhoy, rho(y));
    val:=htvalue(lambdarhoht, rhoy);

    if val=fail or rank>max_rank then #new rho value, and hence new R-rep
      lenreps:=lenreps+1;
      htadd(lambdarhoht, rhoy, lenreps);
      new_nr:=new_nr+1;
      reps[lenreps]:=[y];
      repslookup[lenreps]:=[new_nr];
      orblookup1[new_nr]:=lenreps;
      orblookup2[new_nr]:=1;
      repslens[lenreps]:=1;
      x:=[t, m, o, y, false, new_nr];
    else              # old rho value
      x:=[t, m, o, y, false, new_nr+1];
      #check membership in schutz gp via stab chain
      schutz:=LambdaOrbStabChain(o, m);

      if schutz=true then # schutz gp is symmetric group
        old_to_new[i]:=repslookup[val][1];
        continue;
      else
        if schutz=false then # schutz gp is trivial
          tmp:=htvalue(ht, y);
          if tmp<>fail then
            old_to_new[i]:=tmp;
            continue;
          fi;
        else # schutz gp neither trivial nor symmetric group
          old:=false;
          for n in [1..repslens[val]] do
            if SiftedPermutation(schutz, lambdaperm(reps[val][n], y))=()
              then
              old:=true;
              old_to_new[i]:=repslookup[val][n];
              break;
            fi;
          od;
          if old then
            continue;
          fi;
        fi;
        new_nr:=new_nr+1;
        repslens[val]:=repslens[val]+1;
        reps[val][repslens[val]]:=y;
        repslookup[val][repslens[val]]:=new_nr;
        orblookup1[new_nr]:=val;
        orblookup2[new_nr]:=repslens[val];
      fi;
    fi;
    new_orb[new_nr]:=x;
    graph[new_nr]:=ShallowCopy(old_graph[i]);
    new_schreierpos[new_nr]:=old_to_new[old_schreierpos[i]];
    # orb[nr] is obtained from orb[i]
    new_schreiergen[new_nr]:=old_schreiergen[i];     
    # by multiplying by gens[j]
    new_schreiermult[new_nr]:=pos;  # and ends up in position <pos> of 
                                    # its lambda orb
    htadd(ht, y, new_nr);
    old_to_new[i]:=new_nr;
  od;
  
  # process the orbit graph

  for i in [1..new_nr] do 
    for j in [1..Length(graph[i])] do 
      graph[i][j]:=old_to_new[graph[i][j]];
    od;
  od;

  # apply new generators to old R-reps
  new_data!.genstoapply:=[nr_old_gens+1..nr_new_gens];
  new_data!.pos:=0;
  new_data!.stopper:=old_to_new[old_data!.pos];
  new_data!.lenreps:=lenreps;
  Enumerate(new_data, infinity, ReturnFalse);

  new_data!.pos:=old_to_new[old_data!.pos];
  new_data!.stopper:=false;
  new_data!.genstoapply:=[1..nr_new_gens];
  
  return t;
end);

#subsemigroups

# <limit> is the max size of the subsemigroup.

InstallMethod(SubsemigroupByProperty, 
"for an acting semigroup with generators, function, and positive integer",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsFunction, IsPosInt], 
function(S, func, limit)
  local iter, T, f;
 
  iter:=Iterator(S);
  
  repeat 
    f:=NextIterator(iter);
  until func(f) or IsDoneIterator(iter);
  
  if not func(f) then 
    return fail; # JDM should really return the empty semigroup
  fi;

  T:=Semigroup(f);

  while Size(T)<limit and not IsDoneIterator(iter) do 
    f:=NextIterator(iter);
    if func(f) then 
      T:=ClosureSemigroup(T, f);
    fi;
  od;
  SetParent(T, S);
  return T;
end);

# <limit> is the max size of the subsemigroup.

InstallMethod(InverseSubsemigroupByProperty, 
"for acting semigroup with inverse op & generators, function, positive integer",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup, IsFunction, IsPosInt], 
function(S, func, limit)
  local iter, T, f;
 
  iter:=Iterator(S);
  
  repeat 
    f:=NextIterator(iter);
  until func(f) or IsDoneIterator(iter);
  
  if not func(f) then 
    return fail; # JDM should really return the empty semigroup
  fi;

  T:=InverseSemigroup(f);

  while Size(T)<limit and not IsDoneIterator(iter) do 
    f:=NextIterator(iter);
    if func(f) then 
      T:=ClosureInverseSemigroup(T, f);
    fi;
  od;
  SetParent(T, S);
  return T;
end);

#

InstallMethod(SubsemigroupByProperty, 
"for an acting semigroup with generators and function",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsFunction], 
function(S, func)
  return SubsemigroupByProperty(S, func, Size(S));
end);

#

InstallMethod(InverseSubsemigroupByProperty, 
"for acting semigroup with inverse op & generators and function",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup, IsFunction], 
function(S, func)
  return InverseSubsemigroupByProperty(S, func, Size(S));
end);


#miscellaneous

InstallGlobalFunction(RegularSemigroup, 
function(arg)
  if not IsRecord(arg[Length(arg)]) then 
    Add(arg, rec(regular:=true));
  else
    arg[Length(arg)].regular:=true;
  fi;
  return CallFuncList(Semigroup, arg);
end);

#random

InstallMethod(RandomMatrixSemigroup, "for a ring and pos int",
[IsRing, IsPosInt, IsPosInt], 
function(R, m, n)
  return Semigroup(List([1..m], x-> RandomMat(n, n, R)));
end);

#

#InstallMethod(RandomBinaryRelationMonoid, "for pos int and pos int",
#[IsPosInt, IsPosInt],
#function(m,n)
#  local s;

#  s:=Monoid(List([1..m], x-> RandomBinaryRelationOnPoints(n)));
#  SetIsBinaryRelationSemigroup(s, true); 
#  return s;
#end);

#

#InstallMethod(RandomBinaryRelationSemigroup, "for pos int and pos int",
#[IsPosInt, IsPosInt],
#function(m,n)
#  local s;

#  s:=Semigroup(List([1..m], x-> RandomBinaryRelationOnPoints(n)));
#  SetIsBinaryRelationSemigroup(s, true);
#  return s;
#end);

InstallMethod(RandomBlockGroup, "for pos int and pos int",
[IsPosInt, IsPosInt],
function(m,n)
  return Semigroup(Set(List([1..m], x-> RandomPartialPerm(n))));
end);

#

InstallMethod(RandomInverseMonoid, "for pos int and pos int",
[IsPosInt, IsPosInt],
function(m,n)
  return InverseMonoid(Set(List([1..m], x-> RandomPartialPerm(n))));
end);

#

InstallMethod(RandomInverseSemigroup, "for pos int and pos int",
[IsPosInt, IsPosInt],
function(m,n)
  return InverseSemigroup(Set(List([1..m], x-> RandomPartialPerm(n))));
end);

#

InstallMethod(RandomTransformationSemigroup, "for pos int and pos int",
[IsPosInt, IsPosInt], 
function(m,n)
  return Semigroup(Set(List([1..m], x-> RandomTransformation(n))));
end);

#

InstallMethod(RandomTransformationMonoid, "for a pos int and pos int",
[IsPosInt, IsPosInt], 
function(m,n)
  return Monoid(Set(List([1..m], x-> RandomTransformation(n))));
end);

#InstallMethod(ViewObj, "for a semigroup with generators",
#[IsSemigroup and HasGeneratorsOfSemigroup], 10,
#function(s)
#  local type, name, property, n;
# 
#  if not (IsPartialPermSemigroup(s) or IsTransformationSemigroup(s) or 
#    IsBipartitionSemigroup(s) or IsMatrixSemigroup(s) or
#    IsBinaryRelationSemigroup(s)) then 
#    TryNextMethod();
#  fi;
#
#  # type
#  if IsPartialPermSemigroup(s) then 
#    type:="partial perm ";
#  elif IsTransformationSemigroup(s) then 
#    type:="transformation ";
#  elif IsBipartitionSemigroup(s) then 
#    type:="bipartition ";
#  elif IsMatrixSemigroup(s) then 
#    type:="matrix ";
#  elif IsBinaryRelationSemigroup(s) then 
#    type:="binary relation ";
#  fi;
# 
#  # name
#  if HasIsGroupAsSemigroup(s) and IsGroupAsSemigroup(s) then 
#    name:="group ";
#  elif HasIsZeroGroup(s) and IsZeroGroup(s) then 
#    name:="0-group";
#  elif IsMonoid(s) then 
#    name:="monoid ";
#  else 
#    name:="semigroup ";
#  fi;
#  
#  #properties
#  property:="";
#
#  if HasIsTrivial(s) and IsTrivial(s) then 
#    Append(property, "trivial ");
#  else 
#    if HasIsCommutativeSemigroup(s) and IsCommutativeSemigroup(s) then 
#      Append(property, "commutative ");
#    fi;
#
#    if (HasIsHTrivial(s) and IsHTrivial(s)) 
#      and not (HasIsBrandtSemigroup(s) and IsBrandtSemigroup(s)) then 
#      Append(property, "aperiodic ");
#    fi;
#
#  fi;
#
#  if HasIsTrivial(s) and IsTrivial(s) then 
#  elif HasIsGroupAsSemigroup(s) and IsGroupAsSemigroup(s) then 
#  elif HasIsZeroGroup(s) and IsZeroGroup(s) then 
#  elif HasIsBrandtSemigroup(s) and IsBrandtSemigroup(s) then 
#    Append(property, "Brandt ");
#  elif HasIsZeroSimpleSemigroup(s) and IsZeroSimpleSemigroup(s) then 
#    Append(property, "0-simple ");
#  elif HasIsCliffordSemigroup(s) and IsCliffordSemigroup(s) then
#    Append(property, "Clifford ");
#  elif HasIsInverseSemigroup(s) and IsInverseSemigroup(s) then 
#    if HasIsFactorisableSemigroup(s) and IsFactorisableSemigroup(s) then 
#      Append(property, "factorisable ");
#    fi;
#    Append(property, "inverse ");
#  elif HasIsLeftZeroSemigroup(s) and IsLeftZeroSemigroup(s) then 
#    Append(property, "left zero ");
#  elif HasIsRightZeroSemigroup(s) and IsRightZeroSemigroup(s) then 
#    Append(property, "right zero ");
#  elif HasIsLeftSimple(s) and IsLeftSimple(s) then 
#    Append(property, "left simple ");
#  elif HasIsRightSimple(s) and IsRightSimple(s) then 
#    Append(property, "right simple ");
#  elif HasIsSimpleSemigroup(s) and IsSimpleSemigroup(s) then 
#    Append(property, "simple ");
#  #elif HasIsCompletelyRegularSemigroup(s) and IsCompletelyRegularSemigroup(s)
#  # then 
#  #  Append(property, "completely regular ");
#  elif HasIsOrthodoxSemigroup(s) and IsOrthodoxSemigroup(s) then 
#    Append(property, "orthodox ");
#  elif HasIsRegularSemigroup(s) then 
#    if IsRegularSemigroup(s) then 
#      Append(property, "regular ");
#    else
#      Append(property, "non-regular ");
#    fi;
#  elif HasIsAdequateSemigroup(s) and IsAdequateSemigroup(s) then
#    Append(property, "adequate ");
#  fi;
#
#  Print("<", property, type, name);
#
#  if IsMatrixSemigroup(s) then
#    n:=Length(GeneratorsOfSemigroup(s)[1][1]);
#    Print(n, "x", n, " over ", BaseDomain(GeneratorsOfSemigroup(s)[1][1]), " ");
#  fi;
#  
#  if HasSize(s) and Size(s)<2^64 then 
#    if SizeScreen()[1]-Length(property)-Length(type)-Length(name)-40
#      > Length(String(Size(s))) then 
#      Print("of size ", Size(s), ", ");
#    fi;
#  elif not IsMatrixSemigroup(s) then 
#    Print("of ");
#  fi;
#
#  if IsTransformationSemigroup(s) then 
#    Print("degree ", DegreeOfTransformationSemigroup(s), " ");
#  elif IsPartialPermSemigroup(s) then 
#    Print("degree ", DegreeOfPartialPermSemigroup(s), " ");
#  elif IsBipartitionSemigroup(s) then 
#    Print("degree ", DegreeOfBipartitionSemigroup(s)/2, " "); 
#  elif IsBinaryRelationSemigroup(s) then 
#    Print("degree ", Length(Successors(Generators(s)[1])), " ");
#  fi;
#
#  Print("with ", Length(Generators(s)));
#  Print(" generator");
#
#  if Length(Generators(s))>1 then 
#    Print("s");
#  fi;
#  Print(">");
#end);


#EOF
