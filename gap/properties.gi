#############################################################################
##
#W  properties.gi
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Ecom (commuting idempotents), LI (locally trivial), 
# LG (locally group), B1 (dot-depth one), DA (regular D-classes are idempotent)
# R v L (???), IsNilpotentSemigroup, inverses, local submonoid, right ideal, 
# left ideal, kernel!?

# IsLeftCancellative, 
# IsRightCancellative, IsRightGroup, IsLeftGroup, IsUnitarySemigroup, 
# IsRightUnitarySemigp, IsLeftUnitarySemigp, IsCongruenceFree,
# PrimitiveIdempotents, IdempotentOrder, 
# IsLeftNormalBand, IsRightNormalBand, IsNormalBand, IsEUnitarySemigroup
# IsRectangularGroup, IsBandOfGroups, IsFreeBand, IsFreeSemilattice,
# IsFreeNormalBand, , IsFundamentalInverseSemigp, 
# IsFullSubsemigroup (of an inverse semigroup), IsFactorizableInverseMonoid,
# IsFInverseSemigroup, IsSemigroupWithCentralIdempotents, IsLeftUnipotent,
# IsRightUnipotent, IsSemigroupWithClosedIdempotents, .

# a better method for MinimalIdeal of a simple semigroup.

InstallMethod(OneMutable, "for ring element coll coll coll",
[IsRingElementCollCollColl], x-> One(Representative(x)));

InstallMethod(IsGroupAsSemigroup, "for a matrix semigroup",
[IsMatrixSemigroup],
s-> IsGroupAsSemigroup(Range(IsomorphismTransformationSemigroup(s))));

#

#InstallMethod(IsAbundantSemigroup, "for a trans. semigroup",
#[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
#function(s)
#  local iter, n, ht, ht_o, reg, i, data, f, ker, val, o, scc;
#
#  Info(InfoWarning, 1, "this will sometimes return a false positive.");
#
#  if HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then 
#    Info(InfoSemigroups, 2, "semigroup is regular");
#    return true;
#  fi;
#
#  iter:=IteratorOfRClassData(s); n:=ActionDegree(s);
#  ht:=HTCreate([1..n], rec(hashlen:=s!.opts!.hashlen!.S));
#  ht_o:=HTCreate([1,1,1,1], rec(hashlen:=s!.opts!.hashlen!.S));
#  reg:=[]; i:=0; 
#
#  repeat
#    repeat #JDM this should become an method for IteratorOfRStarClasses
#           # and IsAbundantRClass...
#      data:=NextIterator(iter);
#    until HTValue(ht_o, data{[1,2,4,5]})=fail or IsDoneIterator(iter); 
#    if not IsDoneIterator(iter) then 
#      HTAdd(ht_o, data{[1,2,4,5]}, true);
#
#      #f:=RClassRepFromData(s, data); ker:=CanonicalTransSameKernel(f);
#      val:=HTValue(ht, ker);
#
#      if val=fail then #new kernel
#        i:=i+1; HTAdd(ht, ker, i);
#        val:=i; reg[val]:=false;
#      fi;
#        
#      if reg[val]=false then #old kernel
#        #o:=ImageOrbitFromData(s, data); scc:=ImageOrbitSCCFromData(s, data);
#        reg[val]:=ForAny(scc, j-> IsInjectiveListTrans(o[j], ker));
#      fi;
#    fi;
#  until IsDoneIterator(iter);
#
#  return ForAll(reg, x-> x);
#end);

InstallMethod(IsAdequateSemigroup, 
"for acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
s-> IsAbundantSemigroup(s) and IsBlockGroup(s));

#

InstallMethod(IsBand, "for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup], s-> 
 IsCompletelyRegularSemigroup(s) and IsHTrivial(s));

#

InstallMethod(IsBand, "for an inverse semigroup", 
[IsInverseSemigroup], IsSemilatticeAsSemigroup);

#

InstallMethod(IsBlockGroup, 
"for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local iter, d;

  if HasIsInverseSemigroup(s) and IsInverseSemigroup(s) then 
    Info(InfoSemigroups, 2, "inverse semigroup");
    return true;
  elif (HasIsRegularSemigroup(s) and IsRegularSemigroup(s)) and
   (HasIsInverseSemigroup(s) and not IsInverseSemigroup(s)) then 
    Info(InfoSemigroups, 2, "regular but non-inverse semigroup");
    return false;
  fi;

  iter:=IteratorOfDClasses(s); 
    
  for d in iter do
    if IsRegularDClass(d) and (ForAny(RClasses(d), x-> NrIdempotents(x)>1) or 
      NrRClasses(d)<>NrLClasses(d)) then
      return false;
    fi;
  od;
  return true;
end);

#

InstallMethod(IsBrandtSemigroup, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
s-> IsZeroSimpleSemigroup(s) and IsInverseSemigroup(s));

#

InstallMethod(IsBrandtSemigroup, "for an inverse semigroup", 
[IsInverseSemigroup], IsZeroSimpleSemigroup);

#

InstallMethod(IsCliffordSemigroup, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local gens, idem, f, g;

  if HasIsInverseSemigroup(s) and not IsInverseSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is not inverse");
    return false;
  elif not IsCompletelyRegularSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is not completely regular");
    return false;
  elif IsGroupAsSemigroup(s) then
    Info(InfoSemigroups, 2, "the semigroup is a group");
    return true;
  fi;

  if not IsRegularSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is not regular");
    return false;
  fi;

  gens:=GeneratorsOfSemigroup(s);
  idem:=Set(List(gens, x-> 
   IdempotentCreator(s)(LambdaFunc(s)(x), RhoFunc(s)(x))));

  for f in gens do
    for g in idem do
      if not f*g=g*f then 
        Info(InfoSemigroups, 2, "the idempotents are not central:");
        Info(InfoSemigroups, 2, "  ", f, "\n#I  and\n#I    ", g, 
         "\n#I  do not commute,");
        return false;
      fi;
    od;
  od;

  return true;
end);

#

InstallMethod(IsCliffordSemigroup, 
"for an inverse acting semigroup with generators", 
[IsInverseSemigroup and IsActingSemigroup and HasGeneratorsOfSemigroup], 
s-> ForAll(OrbSCC(LambdaOrb(s)), x-> Length(x)=1));

#

InstallMethod(IsCommutativeSemigroup, "for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, n, i, j; 

  gens:=GeneratorsOfSemigroup(s);
  n:=Length(gens);

  for i in [1..n] do
    for j in [i+1..n] do
      if not gens[i]*gens[j]=gens[j]*gens[i] then 
        Info(InfoSemigroups, 2, "generators ", i, " and ",  j, 
         " do not commute");
        return false;
      fi;
    od;
  od;

  return true;
end);

#

InstallMethod(IsCompletelyRegularSemigroup, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local record, o, pos, f, n;

  if HasIsRegularSemigroup(s) and not IsRegularSemigroup(s) then 
    Info(InfoSemigroups, 2, "semigroup is not regular");
    return false;
  fi;

  record:=ShallowCopy(LambdaOrbOpts(s));
  record.treehashsize:=s!.opts.hashlen.M;

  for f in GeneratorsOfSemigroup(s) do
    o:=Orb(s, LambdaFunc(s)(f), LambdaAct(s), record);
    pos:=LookForInOrb(o, function(o, x) 
      return LambdaRank(s)(LambdaAct(s)(x, f))<>LambdaRank(s)(x); end, 1);
    # for transformations we could use IsInjectiveListTrans instead
    # and the performance would be better!
    
    if pos<>false then 
      Info(InfoSemigroups, 2, "at least one H-class is not a subgroup");
      return false;
    fi;
  od;

  return true;
end);

#

InstallMethod(IsCompletelyRegularSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsCliffordSemigroup);

# Notes: this test required to avoid conflict with Smallsemi, DeclareSynonymAttr
# causes problems. 

InstallMethod(IsCompletelySimpleSemigroup, "for a semigroup",
[IsSemigroup and HasGeneratorsOfSemigroup], 
 x-> IsSimpleSemigroup(x) and IsFinite(x));

#

InstallMethod(IsFactorisableSemigroup, "for a partial perm semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup], 
function(s)
  local G, iso, enum, f;
  
  G:=GroupOfUnits(s);
  
  if G=fail then 
    return false;
  elif IsTrivial(G) then 
    return IsSemilatticeAsSemigroup(s);
  fi;
  
  iso:=InverseGeneralMapping(IsomorphismPermGroup(G));
  enum:=Enumerator(Source(iso));

  for f in Generators(s) do 
    if not f in G then 
      if not ForAny(enum, g-> NaturalLeqPartialPerm(f, g^iso)) then 
        return false;
      fi;
    fi;
  od;
  return true;
end);

#

InstallMethod(IsHTrivial, "for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local iter, d;

  iter:=IteratorOfDClasses(s);
  
  for d in iter do  
    if not IsTrivial(SchutzenbergerGroup(d)) then 
      return false;
    fi;
  od;
  return true;
end);

#

InstallMethod(IsHTrivial, 
"for a D-class of an acting semigroup", 
[IsGreensDClass and IsActingSemigroupGreensClass], 
d-> NrHClasses(d)=Size(d));

#

InstallMethod(IsLTrivial, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local iter, o, d;

  iter:=IteratorOfDClasses(s); 

  for d in iter do 
    if not IsTrivial(SchutzenbergerGroup(d)) then 
      return false;
    fi;
    if Length(RhoOrbSCC(d))<>1 then 
      return false;
    fi;
  od;

  return true;
end);

#

InstallMethod(IsLTrivial, "for an inverse acting semigroup with generators",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup],
function(s)
  return ForAll(OrbSCC(LambdaOrb(s)), x-> Length(x)=1);
end);

#

InstallMethod(IsLTrivial, "for a D-class of an acting semigroup", 
[IsGreensDClass and IsActingSemigroupGreensClass], d-> NrLClasses(d)=Size(d));

#

InstallMethod(IsRTrivial, "for D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass], 
d-> NrRClasses(d)=Size(d));

#

InstallMethod(IsRTrivial, "for an inverse semigroup", 
[IsInverseSemigroup], IsLTrivial);

#

InstallMethod(IsRTrivial, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local iter, x;

  if IsClosed(SemigroupData(s)) and IsClosed(RhoOrb(s)) then 
    for x in GreensDClasses(s) do 
      if (not IsTrivial(SchutzenbergerGroup(x))) or Length(LambdaOrbSCC(x))>1 
       then
        return false;
      fi;
    od;
	
    return true;
  fi;

  iter:=IteratorOfRClasses(s); 
  
  for x in iter do
    if (not IsTrivial(SchutzenbergerGroup(x))) or
     Length(LambdaOrbSCC(x))>1 then 
      return false;
    fi;
  od;

  return true;
end);

#

InstallMethod(IsGroupAsSemigroup, "for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, lambdafunc, lambda, rhofunc, rho, tester, lambda_f, rho_f, f;

  gens:=GeneratorsOfSemigroup(s); #not GeneratorsOfMonoid!

  if IsActingSemigroupWithFixedDegreeMultiplication(s) and 
   ForAll(gens, f->ActionRank(s)(f)=ActionDegree(f)) then
    return true;
  fi;

  lambdafunc:=LambdaFunc(s);
  lambda:=lambdafunc(gens[1]);
  rhofunc:=RhoFunc(s);
  rho:=rhofunc(gens[1]);
  tester:=IdempotentTester(s);

  for f in gens do 
    lambda_f:=lambdafunc(f);
    rho_f:=rhofunc(f);
    if lambda_f<>lambda or rho_f<>rho or not tester(lambda_f, rho_f) then 
      return false;
    fi;
  od;

  return true;
end);

#
# IteratorOfIdempotents would be good here.

InstallMethod(IsIdempotentGenerated, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
function(s) 
  local gens, t, min, new;
 
  gens:=Generators(s);

  if ForAll(gens, IsIdempotent) then 
    Info(InfoSemigroups, 2, "all the generators are idempotents");
    return true;
  fi;
  
  if HasIdempotentGeneratedSubsemigroup(s) then 
    t:=IdempotentGeneratedSubsemigroup(s);
  else
    min:=MinimumList(List(gens, x-> ActionRank(s)(x)));
    new:=Filtered(Idempotents(s), x-> ActionRank(s)(x)>=min);
    if new=[] then 
      return false;
    fi;
    t:=Semigroup(new);
  fi;

  # this is not always the idempotent generated subsemigroup!
  return ForAll(gens, f-> f in t);
end);

#

InstallMethod(IsIdempotentGenerated, "for an inverse semigroup",
[IsInverseSemigroup], IsSemilatticeAsSemigroup);

#

InstallMethod(IsInverseSemigroup,
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local lambda, rho, iter, x;

  if HasIsRegularSemigroup(s) and not IsRegularSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is not regular");
    return false;
  elif IsCompletelyRegularSemigroup(s) then
    Info(InfoSemigroups, 2, "the semigroup is completely regular");
    return IsCliffordSemigroup(s);
  fi;

  lambda:=LambdaOrb(s); Enumerate(lambda);
  rho:=RhoOrb(s); Enumerate(rho, Length(lambda));

  if not (IsClosed(rho) and Length(rho)>=Length(lambda)) then 
    Info(InfoSemigroups, 2, "the number of lambda and rho values is not equal");
    return false;
  fi;
  
  if HasGreensDClasses(s) then 
    iter:=GreensDClasses(s);
    for x in iter do
      if not IsRegularClass(x) or NrIdempotents(x)<>NrRClasses(x) then 
        return false;
      fi;
    od;
  else
    iter:=IteratorOfRClasses(s);
    for x in iter do
      if not IsRegularClass(x) or NrIdempotents(x)>1 then 
        return false;
      fi;
    od;
  fi;
  
  return true;
end);

#

InstallMethod(IsInverseSemigroup, 
"for a semigroup of partial perms with generators",
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
s-> ForAll(Generators(s), x-> x^-1 in s));

#

InstallMethod(IsLeftSimple, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local iter;
  
  if IsLeftZeroSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is a left zero semigroup");
    return true;
  elif HasNrLClasses(s) then 
    return NrLClasses(s)=1;
  fi;
  
  iter:=IteratorOfLClassReps(s); 
  NextIterator(iter);
  return IsDoneIterator(iter);
end);

#

InstallMethod(IsLeftSimple, "for an inverse semigroup", 
[IsInverseSemigroup], IsGroupAsSemigroup);

#

InstallMethod(IsLeftZeroSemigroup, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, lambda, val, x;

  gens:=Generators(s);
  lambda:=LambdaFunc(s);
  val:=lambda(gens[1]);
  
  for x in gens do 
    if not lambda(x)=val then 
      return false;
    fi;
  od;

  return ForAll(gens, IsIdempotent);
end);

#

InstallMethod(IsLeftZeroSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsTrivial);

#

InstallImmediateMethod(IsMonogenicSemigroup, IsSemigroup and HasGeneratorsOfSemigroup, 0, 
function(s) 
  if Length(GeneratorsOfSemigroup(s))=1 then 
    return true;
  fi;
  TryNextMethod();
end);

#

InstallMethod(IsMonogenicSemigroup, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local gens, I, f, i;

  gens:=GeneratorsOfSemigroup(s); 
  
  if not IsDuplicateFreeList(gens) then 
    gens:=ShallowCopy(DuplicateFreeList(gens));
    Info(InfoSemigroups, 2, "there are repeated generators");
  fi;
 
  if Length(gens)=1 then
    Info(InfoSemigroups, 2, "the semigroup only has one generator");
    return true;
  fi;
 
  I:=MinimalIdeal(s);
  
  if not IsGroupAsSemigroup(I) then
    Info(InfoSemigroups, 2, "the minimal ideal is not a group.");
    return false;
  elif not IsCyclic(Range(IsomorphismPermGroup(I))) then 
    Info(InfoSemigroups, 2, "the minimal ideal is a non-cyclic group.");
    return false;
  fi;

  for i in [1..Length(gens)] do 
    f:=gens[i];
    if ForAll(gens, x-> x in Semigroup(f)) then
      Info(InfoSemigroups, 2, "the semigroup is generated by generator ", i);
      SetMinimalGeneratingSet(s, [f]);
      return true;
    fi;
  od;
  Info(InfoSemigroups, 2, "at least one generator does not belong to the", 
   " semigroup generated by any ");
  Info(InfoSemigroups, 2, "other generator.");
  return false;
end);

#

InstallMethod(IsMonogenicInverseSemigroup, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  if not IsInverseSemigroup(s) then 
    return false;
  fi;
  return IsMonogenicInverseSemigroup(Range(IsomorphismPartialPermSemigroup(s)));
end);
 
#

InstallMethod(IsMonogenicInverseSemigroup, 
"for an acting semigroup with inverse op and generators", 
[IsActingSemigroupWithInverseOp and HasGeneratorsOfInverseSemigroup],
function(s)
  local gens, I, f, i;

  gens:=GeneratorsOfInverseSemigroup(s); 
  
  if not IsDuplicateFreeList(gens) then 
    gens:=ShallowCopy(DuplicateFreeList(gens));
    Info(InfoSemigroups, 2, "there are repeated generators");
  fi;
 
  if Length(gens)=1 then
    Info(InfoSemigroups, 2, "the semigroup only has one generator");
    return true;
  fi;
 
  I:=MinimalIdeal(s);
  
  if not IsGroupAsSemigroup(I) then
    Info(InfoSemigroups, 2, "the minimal ideal is not a group.");
    return false;
  elif not IsCyclic(Range(IsomorphismPermGroup(I))) then 
    Info(InfoSemigroups, 2, "the minimal ideal is a non-cyclic group.");
    return false;
  fi;

  for i in [1..Length(gens)] do 
    f:=gens[i];
    if ForAll(gens, x-> x in InverseSemigroup(f)) then
      Info(InfoSemigroups, 2, "the semigroup is generated by generator ", i);
      SetMinimalGeneratingSet(s, [f]);
      return true;
    fi;
  od;

  Info(InfoSemigroups, 2, "at least one generator does not belong to the", 
   " inverse semigroup generated by any ");
  Info(InfoSemigroups, 2, "other generator.");
  return false;
end);

#

InstallMethod(IsMonoidAsSemigroup, "for a semigroup",
[IsSemigroup and HasGeneratorsOfSemigroup], 
 x-> not IsMonoid(x) and MultiplicativeNeutralElement(x)<>fail);

# Notes: is there a better method? JDM

InstallMethod(IsOrthodoxSemigroup, 
"for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local e, m, i, j;

  if not IsRegularSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is not regular");
    return false;
  fi;

  e:=Idempotents(s); m:=Length(e);
  
  for i in [1..m] do
    for j in [1..m] do
      
      if not IsIdempotent(e[i]*e[j]) then 
        Info(InfoSemigroups, 2, "the product of idempotents ", i," and ", j, 
        " is not an idempotent");
        return false;
      fi;
    od;
  od;

  return true;
end);

#

InstallMethod(IsRectangularBand, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)

  if not IsSimpleSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is not simple");
    return false;
  elif HasIsBand(s) then
    return IsBand(s);
  fi;

  return IsHTrivial(s);
end);

#

InstallMethod(IsRectangularBand, "for an inverse semigroup",
[IsInverseSemigroup], s-> IsHTrivial(s) and IsSimpleSemigroup(s));

#

InstallMethod(IsRegularSemigroup, "for an acting semigroup", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local tester, n, rhofunc, lookfunc, data, i;

  if IsSimpleSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is simple");
    return true;
  elif HasIsCompletelyRegularSemigroup(s) 
    and IsCompletelyRegularSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is completely regular");
    return true;
  elif HasGreensDClasses(s) then 
    return ForAll(GreensDClasses(s), IsRegularDClass);
  fi;

  tester:=IdempotentTester(s);
  rhofunc:=RhoFunc(s);

  # look for s not being regular
  lookfunc:=function(data, x)
    local rho, scc, i;
    if data!.repslens[x[2]][data!.orblookup1[x[6]]]>1 then
      return true;
    fi;
    
    # data corresponds to the group of units...
    if IsActingSemigroupWithFixedDegreeMultiplication(s) 
     and ActionRank(s)(x[4])=ActionDegree(x[4]) then 
      return false;
    fi;
    
    rho:=rhofunc(x[4]);
    scc:=OrbSCC(x[3])[x[2]];
    for i in scc do 
      if tester(x[3][i], rho) then 
        return false;
      fi;
    od;
    return true;
  end;

  data:=SemigroupData(s);

  for i in [2..Length(data)] do 
    if lookfunc(data, data[i]) then 
      return false;
    fi;
  od;

  if IsClosed(data) then 
    return true;
  fi;

  data:=Enumerate(data, infinity, lookfunc);
  return data!.found=false;
end);

#

InstallMethod(IsRegularSemigroupElement, 
"for an acting semigroup and acting element",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement], 
function(s, f)                                  
  local o, scc, rho, tester, i;
  
  if not f in s then 
    Info(InfoSemigroups, 2, "the element does not belong to the semigroup,");
    return false;
  fi;
  
  if HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then
    Info(InfoSemigroups, 2, "the semigroup is regular,");
    return true;
  fi;
 
  if IsClosed(LambdaOrb(s)) then 
    o:=LambdaOrb(s); 
  else
    o:=GradedLambdaOrb(s, f, true)[1]; 
  fi;
        
  scc:=OrbSCC(o)[OrbSCCLookup(o)[Position(o, LambdaFunc(s)(f))]];
  rho:=RhoFunc(s)(f);
  tester:=IdempotentTester(s);

  for i in scc do 
    if tester(o[i], rho) then
      return true;
    fi;
  od;
  return false;
end);

#

InstallMethod(IsRegularSemigroupElementNC, 
"for an acting semigroup and acting element",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement], 
function(s, f)                                  
  local o, scc, rho, tester, i;
 
  o:=GradedLambdaOrb(s, f, false)[1];
        
  scc:=OrbSCC(o)[OrbSCCLookup(o)[Position(o, LambdaFunc(s)(f))]];
  rho:=RhoFunc(s)(f);
  tester:=IdempotentTester(s);

  for i in scc do 
    if tester(o[i], rho) then
      return true;
    fi;
  od;
  return false;
end);

#

InstallMethod(IsRightSimple, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local iter;

  if IsRightZeroSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is a right zero semigroup");
    return true;
  elif HasNrRClasses(s) then 
    return NrRClasses(s)=1;
  fi;

  iter:=IteratorOfRClassData(s); 
  NextIterator(iter);
  return IsDoneIterator(iter);
end);

#

InstallMethod(IsRightSimple, "for an inverse semigroup", 
[IsInverseSemigroup], IsGroupAsSemigroup);

#

InstallMethod(IsRightZeroSemigroup, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, rho, val, x;

  gens:=GeneratorsOfSemigroup(s);
  rho:=RhoFunc(s);
  val:=rho(gens[1]);
  for x in gens do 
    if rho(x)<>val or not IsIdempotent(x) then 
      return false;
    fi;
  od;

  return true;
end);

#

InstallMethod(IsRightZeroSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsTrivial);

#

InstallMethod(IsSemiband, "for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], IsIdempotentGenerated);

#

InstallMethod(IsSemilatticeAsSemigroup, 
"for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
 s-> IsCommutativeSemigroup(s) and IsBand(s));

#

InstallMethod(IsSemilatticeAsSemigroup, 
"for an inverse semigroup with generators",
[IsInverseSemigroup and HasGeneratorsOfSemigroup], 
s-> ForAll(GeneratorsOfSemigroup(s), IsIdempotent));

#

InstallMethod(IsSimpleSemigroup, "for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local gens, lambdafunc, lambdarank, rank, opts, o, pos, name, f, n;

  if HasIsRegularSemigroup(s) and not IsRegularSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is not regular");
    return false;
  elif HasIsCompletelyRegularSemigroup(s) and not 
   IsCompletelyRegularSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is not completely regular");
    return false;
  elif HasNrDClasses(s) then
    return NrDClasses(s)=1;
  fi;

  gens:=GeneratorsOfSemigroup(s); #not GeneratorsOfMonoid!
  lambdafunc:=LambdaFunc(s);
  lambdarank:=LambdaRank(s);
  rank:=lambdarank(lambdafunc(gens[1]));
  
  if not ForAll([2..Length(gens)], i-> lambdarank(lambdafunc(gens[i]))=rank)
   then 
    return false;
  fi;

  opts:=rec(treehashsize:=s!.opts.hashlen.M);
  
  for name in RecNames(LambdaOrbOpts(s)) do
    opts.(name):=LambdaOrbOpts(s).(name);
  od; 

  for f in gens do
    o:=Orb(s, LambdaFunc(s)(f), LambdaAct(s), opts);
    pos:=LookForInOrb(o, function(o, x) return LambdaRank(s)(x)<rank; end, 1);
      #or LambdaRank(s)(LambdaAct(s)(x, f))<>LambdaRank(s)(x); end, 1);
    if pos<>false then 
      return false;
    fi;
  od;

  return true;
end);

#

InstallMethod(IsSimpleSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsGroupAsSemigroup);

#

InstallMethod(IsSynchronizingSemigroup, 
"for a transformation semigroup with generators and positive integer", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsPosInt],
function(s, n)
  local o;

  o:=Orb(s, [1..n], LambdaAct(s), rec( schreier:=true, 
    lookingfor:=function(o, x) 
      return Length(x)=1; 
    end));
  
  Enumerate(o);

  if PositionOfFound(o)<>false then 
    Info(InfoSemigroups, 2, "the product of the generators: ",
    TraceSchreierTreeForward(o, PositionOfFound(o)));

    Info(InfoSemigroups, 2, "is a constant function with value ", 
     o[PositionOfFound(o)][1]);
    return true;
  fi;

  return false;
end);

#

InstallMethod(IsSynchronizingTransformationCollection, 
"for a transformation collection and positive integer", 
[IsTransformationCollection, IsPosInt],
function(coll, n)
  local o;

  o:=Orb(coll, [1..n], OnSets, rec(
    lookingfor:=function(o, x) 
      return Length(x)=1;
    end));

  Enumerate(o);
  return PositionOfFound(o)<>false;
end);

#

InstallMethod(IsTrivial, "for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local gens;
  if HasSize(s) and Size(s)=1 then 
    return true;
  fi;
  gens:=GeneratorsOfSemigroup(s);
  return ForAll(gens, x-> gens[1]=x) and IsIdempotent(gens[1]);
end); 

#

InstallMethod(IsUnitRegularSemigroup, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local g, perm_g, o, scc, graded, tester, gens, rhofunc, dom, rho, m, j;

  if not IsRegularSemigroup(s) then 
    return false;
  fi;

  g:=GroupOfUnits(s);
  
  if g=fail then 
    return false;
  elif IsTrivial(g) then #JDM is this any better than the below?
    return IsBand(s);
  fi;

  perm_g:=Range(IsomorphismPermGroup(g));
  o:=LambdaOrb(s);
  scc:=OrbSCC(o);
  graded:=GradedLambdaOrbs(g);
  tester:=IdempotentTester(s);
  gens:=o!.gens;
  rhofunc:=RhoFunc(s);

  for m in [2..Length(scc)] do
    dom:=Union(Orbits(perm_g, o[scc[m][1]], OnPoints));
    if not IsSubgroup(Action(perm_g, dom), Action(LambdaOrbSchutzGp(o, m),
     o[scc[m][1]])) then 
      return false;
    elif Length(scc[m])>1 then 
      rho:=rhofunc(EvaluateWord(gens, TraceSchreierTreeForward(o, scc[m][1])));
      for j in scc[m] do 
        if not o[j] in graded then 
          if not ForAny(GradedLambdaOrb(g, o[j], true)[1], x-> tester(x, rho))
           then 
            return false;
          fi;
        fi;
      od;
    fi;
  od;
  return true;
end);

#

InstallMethod(IsZeroGroup, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)

  if MultiplicativeZero(s)=fail then 
    Info(InfoSemigroups, 2, "the semigroup does not have a zero");
    return false;
  fi;

  if NrHClasses(s)=2 then 
    return ForAll(GreensHClasses(s), IsGroupHClass);
  fi;

  Info(InfoSemigroups, 2, "the semigroup has more than two H-classes");
  return false;
end);

#

InstallMethod(IsZeroRectangularBand, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)

  if not IsZeroSimpleSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is not 0-simple");
    return false;
  elif HasIsBand(s) then
    return IsBand(s);
  fi;

  return IsHTrivial(s);
end);

#

InstallMethod(IsZeroSemigroup, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local z, gens, m, i, j;

  z:=MultiplicativeZero(s);

  if z=fail then
    Info(InfoSemigroups, 2, "the semigroup does not have a zero");
    return false;
  fi;

  gens:=GeneratorsOfSemigroup(s);
  m:=Length(gens);
  for i in [1..m] do
    for j in [1..m] do 
      if not gens[i]*gens[j]=z then 
        Info(InfoSemigroups, 2, "the product of generators ", i, " and ", j,
        " is not the multiplicative zero \n", z);
        return false;
      fi;
    od;
  od;

  return true;
end);

#

InstallMethod(IsZeroSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsTrivial);

#

InstallMethod(IsZeroSimpleSemigroup, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local iter, D;
  
  if MultiplicativeZero(S)=fail then 
    return false;
  fi;
  if IsClosed(SemigroupData(S)) then 
    return IsRegularSemigroup(S) and NrDClasses(S)=2;
  fi;
  iter:=IteratorOfDClasses(S);
  D:=NextIterator(iter);
  if IsDoneIterator(iter) or not IsRegularDClass(D) then 
    return false;
  fi;
  D:=NextIterator(iter);
  return IsDoneIterator(iter) and IsRegularDClass(D);
end);

#EOF
