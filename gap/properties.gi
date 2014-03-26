###############################################################################
#W  properties.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
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

#InstallMethod(IsAdequateSemigroup, 
#"for acting semigroup with generators", 
#[IsActingSemigroup and HasGeneratorsOfSemigroup], 
#s-> IsAbundantSemigroup(s) and IsBlockGroup(s));

# same method for ideals

InstallMethod(IsBand, "for an acting semigroup", 
[IsActingSemigroup], S-> IsCompletelyRegularSemigroup(S) and IsHTrivial(S));

# same method for ideals

InstallMethod(IsBand, "for an inverse semigroup", [IsInverseSemigroup],
IsSemilatticeAsSemigroup);

# same method for ideals

InstallMethod(IsBlockGroup, "for an acting semigroup", [IsActingSemigroup], 
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

# same method for ideals

InstallMethod(IsBrandtSemigroup, "for an acting semigroup", [IsActingSemigroup],
S-> IsZeroSimpleSemigroup(S) and IsInverseSemigroup(S));

# same method for inverse ideals

InstallMethod(IsBrandtSemigroup, "for an inverse semigroup", 
[IsInverseSemigroup], IsZeroSimpleSemigroup);

# same method for non-regular ideals

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

# same method for inverse ideals

InstallMethod(IsCliffordSemigroup, "for an inverse acting semigroup", 
[IsInverseSemigroup and IsActingSemigroup], 
s-> ForAll(OrbSCC(LambdaOrb(s)), x-> Length(x)=1));

# same method for regular ideals, or non-regular without a generating set

InstallMethod(IsCliffordSemigroup, "for a semigroup",
[IsSemigroup], S-> IsRegularSemigroup(S) and NrHClasses(S)=NrDClasses(S));

# different method for ideals

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

# same method for non-regular ideals

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

# same method for regular ideals, or non-regular without a generating set

InstallMethod(IsCompletelyRegularSemigroup, "for a semigroup",
[IsSemigroup], S-> NrHClasses(S)=NrIdempotents(S));

# same method for inverse ideals

InstallMethod(IsCompletelyRegularSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsCliffordSemigroup);

# Notes: this test required to avoid conflict with Smallsemi, DeclareSynonymAttr
# causes problems. 

#same method for ideals

InstallMethod(IsCompletelySimpleSemigroup, "for a semigroup",
[IsSemigroup], S-> IsSimpleSemigroup(S) and IsFinite(S));

#different method for ideals

InstallMethod(IsFactorisableSemigroup, "for an inverse op acting semigroup",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup], 
function(S)
  local G, iso, enum, f;
  
  G:=GroupOfUnits(S);
  
  if G=fail then 
    return false;
  elif IsTrivial(G) then 
    return IsSemilatticeAsSemigroup(S);
  fi;
  
  iso:=InverseGeneralMapping(IsomorphismPermGroup(G));
  enum:=Enumerator(Source(iso));

  for f in Generators(S) do 
    if not f in G then 
      if not ForAny(enum, g-> NaturalLeqInverseSemigroup(f, g^iso)) then 
        return false;
      fi;
    fi;
  od;
  return true;
end);

# same method for ideals

InstallMethod(IsFactorisableSemigroup, "for an inverse semigroup with generators",
[IsInverseSemigroup and HasGeneratorsOfSemigroup], 
function(S)
  if IsGeneratorsOfInverseSemigroup(GeneratorsOfSemigroup(S)) then 
    return IsFactorisableSemigroup(Range(IsomorphismPartialPermSemigroup(S)));
  fi;
  return false;
end);

# same method for ideals

InstallMethod(IsHTrivial, "for an acting semigroup", 
[IsActingSemigroup], 
function(S)
  local iter, x;

  if IsTransformationSemigroup(S) and HasGeneratorsOfSemigroup(S) then 
    for x in GeneratorsOfSemigroup(S) do 
      if IndexPeriodOfTransformation(x)[2]<>1 then 
        return false;
      fi;
    od;
  elif IsPartialPermSemigroup(S) and HasGeneratorsOfSemigroup(S) then 
    for x in GeneratorsOfSemigroup(S) do 
      if IndexPeriodOfPartialPerm(x)[2]<>1 then 
        return false;
      fi;
    od;
  fi;

  iter:=IteratorOfDClasses(S);
  
  for x in iter do  
    if not IsTrivial(SchutzenbergerGroup(x)) then 
      return false;
    fi;
  od;
  return true;
end);

InstallMethod(IsHTrivial, "for a semigroup",
[IsSemigroup], S-> NrHClasses(S)=Size(S));

#same method for ideals

InstallMethod(IsHTrivial, "for a D-class of an acting semigroup", 
[IsGreensDClass and IsActingSemigroupGreensClass], d-> NrHClasses(d)=Size(d));

#same method for non-inverse ideals

InstallMethod(IsLTrivial, "for an acting semigroup",
[IsActingSemigroup],
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

# same method for inverse ideals

InstallMethod(IsLTrivial, "for an inverse acting semigroup",
[IsActingSemigroupWithInverseOp],
function(s)
  return ForAll(OrbSCC(LambdaOrb(s)), x-> Length(x)=1);
end);

#same method for ideals

InstallMethod(IsLTrivial, "for a D-class of an acting semigroup", 
[IsGreensDClass and IsActingSemigroupGreensClass], d-> NrLClasses(d)=Size(d));

#same method for ideals

InstallMethod(IsRTrivial, "for D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass], 
d-> NrRClasses(d)=Size(d));

#same method for ideals

InstallMethod(IsRTrivial, "for an inverse semigroup", 
[IsInverseSemigroup], IsLTrivial);

# different method for ideals

InstallMethod(IsRTrivial, "for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(S)
  if ForAny(GeneratorsOfSemigroup(S), x-> 
   ForAny(CyclesOfTransformation(x), y-> Length(y)>1)) then 
    return false;
  else
    return ForAll(CyclesOfTransformationSemigroup(S), x-> Length(x)=1);
  fi;
end);

# different method for ideals

InstallMethod(IsRTrivial, "for a partial perm semigroup with generators",
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup], 
function(S)
  if ForAny(GeneratorsOfSemigroup(S), x-> 
   ForAny(CyclesOfPartialPerm(x), y-> Length(y)>1)) then 
    return false;
  else
    return ForAll(CyclesOfPartialPermSemigroup(S), x-> Length(x)=1);
  fi;
end);

# same method for non-inverse ideals

InstallMethod(IsRTrivial, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local iter, x;

  if IsClosedData(SemigroupData(S)) and IsClosed(RhoOrb(S)) then 
    for x in GreensDClasses(S) do 
      if (not IsTrivial(SchutzenbergerGroup(x))) or Length(LambdaOrbSCC(x))>1 
       then
        return false;
      fi;
    od;
    return true;
  fi;

  iter:=IteratorOfRClasses(S); 
  
  for x in iter do
    if (not IsTrivial(SchutzenbergerGroup(x))) or
     Length(LambdaOrbSCC(x))>1 then 
      return false;
    fi;
  od;

  return true;
end);

InstallMethod(IsRTrivial, "for a semigroup",
[IsSemigroup], S-> Size(S)=NrRClasses(S));

# same method for non-regular ideals

InstallMethod(IsGroupAsSemigroup, "for an acting semigroup", 
[IsActingSemigroup],
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

# different method for inverse, regular ideals (so that it has higher rank)

InstallMethod(IsGroupAsSemigroup, "for a semigroup",
[IsSemigroup], S-> NrRClasses(S)=1 and NrLClasses(S)=1);

#JDM should gens in the following function be GeneratorsOfSemigroup?
# IteratorOfIdempotents would be good here.

# same method for ideals

InstallMethod(IsIdempotentGenerated, "for an acting semigroup", 
[IsActingSemigroup], 
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

# same method for inverse ideals

InstallMethod(IsIdempotentGenerated, "for an inverse semigroup",
[IsInverseSemigroup], IsSemilatticeAsSemigroup);

# same method for ideals

InstallMethod(IsInverseSemigroup, "for an acting semigroup", 
[IsActingSemigroup],
function(S)
  local lambda, rho, iter, x;

  if HasGeneratorsOfSemigroup(S) and
    IsGeneratorsOfInverseSemigroup(GeneratorsOfSemigroup(S)) and 
    ForAll(GeneratorsOfSemigroup(S), x-> x^-1 in S) then 
    return true;
  fi;
  
  if HasIsRegularSemigroup(S) and not IsRegularSemigroup(S) then 
    Info(InfoSemigroups, 2, "the semigroup is not regular");
    return false;
  elif IsCompletelyRegularSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is completely regular");
    return IsCliffordSemigroup(S);
  fi;

  lambda:=LambdaOrb(S);  Enumerate(lambda);
  rho:=RhoOrb(S);        Enumerate(rho, Length(lambda));

  if not (IsClosed(rho) and Length(rho)>=Length(lambda)) then 
    Info(InfoSemigroups, 2, "the numbers of lambda and rho values are not equal");
    return false;
  fi;
  
  if HasGreensDClasses(S) then 
    iter:=GreensDClasses(S);
    for x in iter do
      if not IsRegularClass(x) or NrIdempotents(x)<>NrRClasses(x) then 
        return false;
      fi;
    od;
  else
    iter:=IteratorOfRClasses(S);
    for x in iter do
      if not IsRegularClass(x) or NrIdempotents(x)>1 then 
        return false;
      fi;
    od;
  fi;
  
  return true;
end);

# same method for ideals

InstallMethod(IsLeftSimple, "for an acting semigroup", [IsActingSemigroup],
function(S)
  local iter;
 
  if HasIsRegularSemigroup(S) and not IsRegularSemigroup(S) then 
    return false;
  elif IsLeftZeroSemigroup(S) then 
    Info(InfoSemigroups, 2, "the semigroup is a left zero semigroup");
    return true;
  elif HasNrLClasses(S) then 
    return NrLClasses(S)=1;
  fi;
  
  iter:=IteratorOfLClassReps(S); 
  NextIterator(iter);
  return IsDoneIterator(iter);
end);

# same method for ideals

InstallMethod(IsLeftSimple, "for an inverse semigroup", 
[IsInverseSemigroup], IsGroupAsSemigroup);

# different method for ideals without generators

InstallMethod(IsLeftZeroSemigroup, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, lambda, val, x;

  gens:=GeneratorsOfSemigroup(s);
  lambda:=LambdaFunc(s);
  val:=lambda(gens[1]);
  
  for x in gens do 
    if not lambda(x)=val then 
      return false;
    fi;
  od;

  return ForAll(gens, IsIdempotent);
end);

InstallMethod(IsLeftZeroSemigroup, "for a semigroup",
[IsSemigroup], S-> NrLClasses(S)=1 and Size(S)=NrRClasses(S));

# same method for ideals

InstallMethod(IsLeftZeroSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsTrivial);

# not applicable for ideals

InstallImmediateMethod(IsMonogenicSemigroup, IsSemigroup and HasGeneratorsOfSemigroup, 0, 
function(s) 
  if Length(GeneratorsOfSemigroup(s))=1 then 
    return true;
  fi;
  TryNextMethod();
end);

# same method for ideals

InstallMethod(IsMonogenicSemigroup, "for an acting semigroup",
[IsActingSemigroup], 
function(S)
  local gens, I, f, i;
  
  if HasGeneratorsOfSemigroup(S) then 

    gens:=GeneratorsOfSemigroup(S); 
    
    if not IsDuplicateFreeList(gens) then 
      gens:=ShallowCopy(DuplicateFreeList(gens));
      Info(InfoSemigroups, 2, "there are repeated generators");
    fi;
   
    if Length(gens)=1 then
      Info(InfoSemigroups, 2, "the semigroup only has one generator");
      return true;
    fi;
  fi;
 
  I:=MinimalIdeal(S);
  
  if not IsGroupAsSemigroup(I) then
    Info(InfoSemigroups, 2, "the minimal ideal is not a group.");
    return false;
  elif not IsCyclic(Range(IsomorphismPermGroup(I))) then 
    Info(InfoSemigroups, 2, "the minimal ideal is a non-cyclic group.");
    return false;
  fi;

  gens:=GeneratorsOfSemigroup(S);

  for i in [1..Length(gens)] do 
    f:=gens[i];
    if ForAll(gens, x-> x in Semigroup(f)) then
      Info(InfoSemigroups, 2, "the semigroup is generated by generator ", i);
      SetMinimalGeneratingSet(S, [f]);
      return true;
    fi;
  od;
  Info(InfoSemigroups, 2, "at least one generator does not belong to the", 
   " semigroup generated by any ");
  Info(InfoSemigroups, 2, "other generator.");
  return false;
end);

# same method for ideals

InstallMethod(IsMonogenicInverseSemigroup, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  if not IsInverseSemigroup(s) then 
    return false;
  fi;
  return IsMonogenicInverseSemigroup(Range(IsomorphismPartialPermSemigroup(s)));
end);
 
# same method for ideals

InstallMethod(IsMonogenicInverseSemigroup, 
"for an acting semigroup with inverse op", [IsActingSemigroupWithInverseOp],
function(S)
  local gens, I, f, i;

  if HasGeneratorsOfInverseSemigroup(S) then 
    gens:=GeneratorsOfInverseSemigroup(S); 
  
    if not IsDuplicateFreeList(gens) then 
      gens:=ShallowCopy(DuplicateFreeList(gens));
      Info(InfoSemigroups, 2, "there are repeated generators");
    fi;
   
    if Length(gens)=1 then
      Info(InfoSemigroups, 2, "the semigroup only has one generator");
      return true;
    fi;
  fi;
 
  I:=MinimalIdeal(S);
  
  if not IsGroupAsSemigroup(I) then
    Info(InfoSemigroups, 2, "the minimal ideal is not a group.");
    return false;
  elif not IsCyclic(Range(IsomorphismPermGroup(I))) then 
    Info(InfoSemigroups, 2, "the minimal ideal is a non-cyclic group.");
    return false;
  fi;
  
  gens:=GeneratorsOfInverseSemigroup(S);

  for i in [1..Length(gens)] do 
    f:=gens[i];
    if ForAll(gens, x-> x in InverseSemigroup(f)) then
      Info(InfoSemigroups, 2, "the semigroup is generated by generator ", i);
      SetMinimalGeneratingSet(S, [f]);
      return true;
    fi;
  od;

  Info(InfoSemigroups, 2, "at least one generator does not belong to the", 
   " inverse semigroup generated by any ");
  Info(InfoSemigroups, 2, "other generator.");
  return false;
end);

#same method for ideals

InstallMethod(IsMonoidAsSemigroup, "for a semigroup",
[IsSemigroup], x-> not IsMonoid(x) and MultiplicativeNeutralElement(x)<>fail);

# Notes: is there a better method? JDM

# same method for ideals

InstallMethod(IsOrthodoxSemigroup, "for an acting semigroup", [IsActingSemigroup], 
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

# same method for ideals

InstallMethod(IsRectangularBand, "for an acting semigroup", 
[IsActingSemigroup],
function(s)

  if not IsSimpleSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is not simple");
    return false;
  elif HasIsBand(s) then
    return IsBand(s);
  fi;

  return IsHTrivial(s);
end);

# same method for ideals

InstallMethod(IsRectangularBand, "for an inverse semigroup",
[IsInverseSemigroup], s-> IsHTrivial(s) and IsSimpleSemigroup(s));

# different method for ideals (ideals know at their point of creation if they
# are regular or not)

InstallMethod(IsRegularSemigroup, "for an acting semigroup with generators", 
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
  else #HasGreensDClasses(s) then 
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

  if IsClosedData(data) then 
    return true;
  fi;

  data:=Enumerate(data, infinity, lookfunc);
  return data!.found=false;
end);

# same method for ideals

InstallMethod(IsRegularSemigroupElement, 
"for an acting semigroup and associative element",
[IsActingSemigroup, IsAssociativeElement], 
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

# same method for ideals

InstallMethod(IsRegularSemigroupElementNC, 
"for an acting semigroup and associative element",
[IsActingSemigroup, IsAssociativeElement], 
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

# same method for ideals

InstallMethod(IsRightSimple, "for an acting semigroup", [IsActingSemigroup],
function(S)
  local iter;
 
  if HasIsRegularSemigroup(S) and not IsRegularSemigroup(S) then 
    return false;
  elif IsRightZeroSemigroup(S) then 
    Info(InfoSemigroups, 2, "the semigroup is a left zero semigroup");
    return true;
  elif HasNrRClasses(S) then 
    return NrRClasses(S)=1;
  fi;
  
  iter:=IteratorOfRClassData(S); 
  NextIterator(iter);
  return IsDoneIterator(iter);
end);

# same method for ideals

InstallMethod(IsRightSimple, "for an inverse semigroup", 
[IsInverseSemigroup], IsGroupAsSemigroup);

# different method for ideals

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

InstallMethod(IsRightZeroSemigroup, "for a semigroup",
[IsSemigroup], S-> NrRClasses(S)=1 and Size(S)=NrLClasses(S));

# same method for ideals

InstallMethod(IsRightZeroSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsTrivial);

# same method for ideals, JDM why is this not a synonym?

InstallMethod(IsSemiband, "for a semigroup", [IsSemigroup], IsIdempotentGenerated);

# same method for ideals

InstallMethod(IsSemilatticeAsSemigroup, "for a semigroup", [IsSemigroup], 
 S-> IsCommutativeSemigroup(S) and IsBand(S));

# not applicable to ideals

InstallMethod(IsSemilatticeAsSemigroup, 
"for an inverse semigroup with generators",
[IsInverseSemigroup and HasGeneratorsOfSemigroup], 
s-> ForAll(GeneratorsOfSemigroup(s), IsIdempotent));

# same method for ideals

InstallMethod(IsSemilatticeAsSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], 
function(S)
  if IsSemigroupIdeal(S) and HasIsSemilatticeAsSemigroup(Parent(S)) 
    and IsSemilatticeAsSemigroup(Parent(S)) then 
    return true;
  else
    return ForAll(GreensDClasses(S), IsTrivial);
  fi;
end);

# same method for ideals

InstallMethod(IsSimpleSemigroup, "for an acting semigroup", [IsActingSemigroup], 
function(S)
  local gens, lambdafunc, lambdarank, rank, opts, o, pos, iter, name, f, n;

  if HasIsRegularSemigroup(S) and not IsRegularSemigroup(S) then 
    Info(InfoSemigroups, 2, "the semigroup is not regular");
    return false;
  elif HasIsCompletelyRegularSemigroup(S) 
   and not IsCompletelyRegularSemigroup(S) then 
    Info(InfoSemigroups, 2, "the semigroup is not completely regular");
    return false;
  elif HasNrDClasses(S) then
    return NrDClasses(S)=1;
  elif HasGeneratorsOfSemigroup(S) then 
    gens:=GeneratorsOfSemigroup(S); #not GeneratorsOfMonoid!
    lambdafunc:=LambdaFunc(S);
    lambdarank:=LambdaRank(S);
    rank:=lambdarank(lambdafunc(gens[1]));
    
    if not ForAll([2..Length(gens)], i-> lambdarank(lambdafunc(gens[i]))=rank)
     then 
      return false;
    fi;

    opts:=rec(treehashsize:=S!.opts.hashlen.M);
    
    for name in RecNames(LambdaOrbOpts(S)) do
      opts.(name):=LambdaOrbOpts(S).(name);
    od; 

    for f in gens do
      o:=Orb(S, LambdaFunc(S)(f), LambdaAct(S), opts);
      pos:=LookForInOrb(o, function(o, x) return LambdaRank(S)(x)<rank; end, 1);
        #or LambdaRank(S)(LambdaAct(S)(x, f))<>LambdaRank(S)(x); end, 1);
      if pos<>false then 
        return false;
      fi;
    od;

    return true;
  else #regular ideal case
    iter:=IteratorOfDClasses(S); 
    NextIterator(iter);
    return IsDoneIterator(iter);
  fi;
  
end);

# same method for ideals

InstallMethod(IsSimpleSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsGroupAsSemigroup);

# same method for ideals

InstallMethod(IsSynchronizingSemigroup, 
"for a transformation semigroup and positive integer", 
[IsTransformationSemigroup, IsPosInt],
function(S, n)
  local gens, o;

  if HasGeneratorsOfSemigroup(S) then 
    gens:=GeneratorsOfSemigroup(S);
  else
    gens:=GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));
  fi;

  o:=Orb(gens, [1..n], LambdaAct(S), rec( schreier:=true, 
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

# not applicable to ideals

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

# different method for ideals

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

# same method for ideals

InstallMethod(IsUnitRegularSemigroup, "for an acting semigroup",
[IsActingSemigroup], 
function(S)
  local g, perm_g, o, scc, graded, tester, gens, rhofunc, dom, rho, m, j;

  if not IsRegularSemigroup(S) then 
    return false;
  fi;

  g:=GroupOfUnits(S);
  
  if g=fail then 
    return false;
  elif IsTrivial(g) then #JDM is this any better than the below?
    return IsBand(S);
  elif IsSemigroupIdeal(S) then 
    return IsUnitRegularSemigroup(SupersemigroupOfIdeal(S));
  fi;

  perm_g:=Range(IsomorphismPermGroup(g));
  o:=LambdaOrb(S);
  scc:=OrbSCC(o);
  graded:=GradedLambdaOrbs(g);
  tester:=IdempotentTester(S);
  gens:=o!.gens;
  rhofunc:=RhoFunc(S);

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

# same method for ideals

InstallMethod(IsZeroGroup, "for an acting semigroup",
[IsActingSemigroup],
function(S)

  if MultiplicativeZero(S)=fail then 
    Info(InfoSemigroups, 2, "the semigroup does not have a zero");
    return false;
  fi;

  if NrHClasses(S)=2 then 
    return ForAll(GreensHClasses(S), IsGroupHClass);
  fi;

  Info(InfoSemigroups, 2, "the semigroup has more than two H-classes");
  return false;
end);

# same method for ideals

InstallMethod(IsZeroRectangularBand, "for an acting semigroup", [IsActingSemigroup],
function(S)

  if not IsZeroSimpleSemigroup(S) then 
    Info(InfoSemigroups, 2, "the semigroup is not 0-simple");
    return false;
  fi;

  return IsHTrivial(S);
end);

# different method for ideals

InstallMethod(IsZeroSemigroup, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local z, gens, m, i, j;

  z:=MultiplicativeZero(S);

  if z=fail then
    Info(InfoSemigroups, 2, "the semigroup does not have a zero");
    return false;
  fi;

  gens:=GeneratorsOfSemigroup(S);
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

InstallMethod(IsZeroSemigroup, "for a semigroup", [IsSemigroup],
function(S) 
  return NrRegularDClasses(S)=1 and MultiplicativeZero(S)<>fail;
end);

# same method for ideals

InstallMethod(IsZeroSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsTrivial);

# same method for ideals

InstallMethod(IsZeroSimpleSemigroup, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local iter, D;
  
  if MultiplicativeZero(S)=fail then 
    return false;
  fi;
  if IsClosedData(SemigroupData(S)) then 
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

# same method for ideals

InstallMethod(IsZeroSimpleSemigroup, "for an inverse semigroup", 
[IsInverseSemigroup], 
function(S)
  return MultiplicativeZero(S)<>fail and NrDClasses(S)=2;
end);

#same method for ideals

InstallMethod(IsCongruenceFreeSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  local t, p, rowsDiff;
  
  rowsDiff := function(p)
    local i, j;
    for i in [1..Size(p)-1] do
      for j in [i+1..Size(p)] do
        if p[i] = p[j] then
          return false;
        fi;
      od;
    od;
    return true;
  end;
  
  if Size(S) <= 2 then
    return true;
  fi;
  
  if MultiplicativeZero(S) <> fail then
    # CASE 1: s has zero
    if IsZeroSimpleSemigroup(S) then
      # Find an isomorphic RMS
      t := Range(IsomorphismReesMatrixSemigroup(S));
      if IsTrivial(UnderlyingSemigroup(t)) then
        # Check that no two rows or columns are identical
        p := Matrix(t);
        if rowsDiff(p) and rowsDiff(TransposedMat(p)) then
          return true;
        fi;
      fi;
    fi;
    return false;
  else
    # CASE 2: s has no zero
    return IsGroup(S) and IsSimpleGroup(S);
  fi;
end);

# same method for ideals

InstallMethod(IsEunitaryInverseSemigroup,
"for an inverse semigroup of partial permutations",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(S)
  return IsMajorantlyClosed(S,IdempotentGeneratedSubsemigroup(S));
end);

#EOF
