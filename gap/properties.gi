
#############################################################################
##
#W  properties.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
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

# Notes: this does not work all that well, use SmallGeneratingSet first. 

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
    if NrRClasses(d)<>NrLClasses(d) then 
      return false;
    elif IsRegularDClass(d) 
      and ForAny(RClasses(d), x-> NrIdempotents(x)>1) then 
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

InstallOtherMethod(IsCliffordSemigroup, 
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

  gens:=Generators(s);

  idem:=List(gens, x->IdempotentCreator(LambdaFunc(s)(x), RhoFunc(s)(x)));

  for f in gens do
    for g in idem do
      if not f*g=g*f then 
        Info(InfoSemigroups, 2, "the idempotents are not central");
        Info(InfoSemigroups, 2, f, " and ", g, "do not commute");
        return false;
      fi;
    od;
  od;

  return true;
end);

#

InstallOtherMethod(IsCliffordSemigroup, 
"for an inverse acting semigroup with generators", 
[IsInverseSemigroup and IsActingSemigroup and HasGeneratorsOfSemigroup], 
s-> ForAll(OrbSCC(LambdaOrb(s)), x-> Length(x)=1));

#

InstallOtherMethod(IsCommutativeSemigroup, "for a semigroup with generators",
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

InstallOtherMethod(IsCompletelyRegularSemigroup, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local pos, f, n;

  if HasIsRegularSemigroup(s) and not IsRegularSemigroup(s) then 
    Info(InfoSemigroups, 2, "semigroup is not regular");
    return false;
  fi;

  for f in Generators(s) do
    pos:=LookForInOrb(LambdaOrb(s), function(o, x) 
      return LambdaRank(s)(LambdaAct(s)(x, f))<>LambdaRank(s)(x); end, 2);
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

InstallOtherMethod(IsCompletelyRegularSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsCliffordSemigroup);

# Notes: this test required to avoid conflict with Smallsemi, DeclareSynonymAttr
# causes problems. 

InstallOtherMethod(IsCompletelySimpleSemigroup, "for a semigroup",
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
      if not ForAny(enum, g-> NaturalLeqPP(f, g^iso)) then 
        return false;
      fi;
    fi;
  od;
  return true;
end);

#

InstallMethod(IsHTrivial, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local iter, i, g;

  iter:=IteratorOfDClassData(s);
  i:=0; 
  repeat 
    i:=i+1;
    #g:=DClassSchutzGpFromData(s, NextIterator(iter)[2]);
    if Size(g)>1 then 
      Info(InfoSemigroups, 2, "the D-class with index ", i, " is not H-trivial");
      return false;
    fi;
  until IsDoneIterator(iter);
  return true;
end);

#

InstallMethod(IsHTrivial, "for a partial perm inv semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  EnumerateInverseSemiData(s);
  return ForAll(LongOrb(s)!.schutz, x-> IsTrivial(x[2]));
end);

#

InstallOtherMethod(IsHTrivial, "for a D-class of a trans. semigp", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
  d-> NrHClasses(d)=Size(d));

#

InstallOtherMethod(IsHTrivial, "for a D-class of a part perm semigp",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup],
  d-> NrHClasses(d)=Size(d));

#

InstallMethod(IsLTrivial, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local iter, i, d;

  iter:=IteratorOfDClassData(s); 
  i:=0;

  #JDM here it would be useful to pass OrbitsOfKernels(s)!.orbits to 
  # KernelOrbitSchutzGpFromData...

  for d in iter do 
    i:=i+1;
    #if not (Size(KernelOrbitSchutzGpFromData(s, d[2]))=1 and 
    # Length(KernelOrbitSCCFromData(s, d[2]))=1) then
      Info(InfoSemigroups, 2, "the D-class with index ", i, " is not L-trivial");
      return false;
    #fi;
  od;

  return true;
end);

#

InstallMethod(IsLTrivial, "for an inverse semigroup", 
[IsInverseSemigroup and IsPartialPermSemigroup],
s-> ForAll(OrbSCC(LongOrb(s)), x-> Length(x)=1) and IsHTrivial(s));

#

InstallOtherMethod(IsLTrivial, "for a D-class of a trans. semigp", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
  d-> NrLClasses(d)=Size(d));

#

InstallOtherMethod(IsLTrivial, "for a D-class of a part perm semigp", 
[IsGreensDClass and IsGreensClassOfPartPermSemigroup], 
  d-> NrLClasses(d)=Size(d));

#

InstallMethod(IsRTrivial, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local i, iter, r, d;
  i:=0;

  #if OrbitsOfKernels(s)!.finished then 
    iter:=IteratorOfDClasses(s);
    for d in iter do 
      i:=i+1;
   #   if not (Size(ImageOrbitSchutzGpFromData(s, d!.data[1]))=1 and 
   #    Length(ImageOrbitSCCFromData(s, d!.data[1]))=1) then
        Info(InfoSemigroups, 2, "the D-class with index ", i, " is not R-trivial");
        return false;
    #  fi;
    od;
	
    return true;
  #fi;

  #iter:=IteratorOfRClassData(s); 

  #JDM here it would be useful to pass OrbitsOfImages(s)!.orbits to 
  # RClassSchutzGpFromData...

  for d in iter do
    i:=i+1;
   # if not (Size(ImageOrbitSchutzGpFromData(s, d))=1 and 
   #  Length(ImageOrbitSCCFromData(s, d))=1) then 
      Info(InfoSemigroups, 2, "the R-class with index ", i, " is not trivial");
      return false;
    #fi;
  od;

  return true;
end);

#

InstallMethod(IsRTrivial, "for an inverse semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup], IsLTrivial);

#

InstallOtherMethod(IsRTrivial, "for D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  #JDM maybe better if we had an enumerator of R-classes of d...
  return NrRClasses(d)=Size(d);
end);

#

InstallOtherMethod(IsRTrivial, "for D-class of a part. perm. semigp.",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup], IsLTrivial);

#

InstallOtherMethod(IsGroupAsSemigroup, "for an acting semigroup", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, lambdafunc, lambda, rhofunc, rho, tester, lambda_f, rho_f, f;

  gens:=GeneratorsOfSemigroup(s); #not GeneratorsOfMonoid!

  if IsTransformationSemigroup(s) and 
   ForAll(gens, f->RankOfTransformation(f)=DegreeOfTransformationSemigroup(s))
    then
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

# Notes: should use ClosureSemigroup. JDM

InstallOtherMethod(IsIdempotentGenerated, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s) 
  local gens, r, i, t;
 
  gens:=Generators(s);

  if ForAll(gens, IsIdempotent) then 
    Info(InfoSemigroups, 2, "all the generators are idempotents");
    return true;
  fi;

  r:=List(gens, ActionRank); 
  i:=Concatenation(List([Maximum(r),Maximum(r)-1..Minimum(r)], i-> 
   Idempotents(s, i)));
  t:=Semigroup(i);
  # this is not the idempotent generated subsemigroup!

  return ForAll(gens, f-> f in t);
end);

#

InstallOtherMethod(IsIdempotentGenerated, "for an inverse semigroup",
[IsInverseSemigroup], IsSemilatticeAsSemigroup);

#

InstallOtherMethod(IsInverseMonoid, "for a trans semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
s-> IsMonoid(s) and IsInverseSemigroup(s));

#

InstallOtherMethod(IsInverseMonoid, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> IsMonoid(s) and IsInverseSemigroup(s));

#

InstallOtherMethod(IsInverseSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local n, imgs, kers, iter, D, d;

  if HasIsRegularSemigroup(s) and not IsRegularSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is not regular");
    return false;
  elif IsCompletelyRegularSemigroup(s) then
    Info(InfoSemigroups, 2, "the semigroup is completely regular");
    return IsCliffordSemigroup(s);
  fi;

#  n:=LambdaDegree(s); imgs:=ImagesOfTransSemigroup(s); Enumerate(imgs, 2^n);
#  kers:=KernelsOfTransSemigroup(s); Enumerate(kers, Length(imgs));

  if not (IsClosed(kers) and Length(kers)=Length(imgs)) then 
    Info(InfoSemigroups, 2, "the numbers of kernels and images are not equal");
    return false;
  fi;

  #if OrbitsOfKernels(s)!.finished then 
  #  iter:=IteratorOfDClassData(s); D:=true;
  #else 
  #  iter:=IteratorOfRClassData(s); D:=false;
  #fi;
    
  for d in iter do
    if D then 
      d:=d[1];
    fi;
   # if not NrIdempotentsRClassFromData(s, d)=1 then 
      Info(InfoSemigroups, 2, "at least one R-class contains more than 1", 
      " idempotent");
      return false;
   # fi;
  od;

  return true;
end);

#

InstallOtherMethod(IsInverseSemigroup, "for a semigroup of partial perms",
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
s-> ForAll(Generators(s), x-> x^-1 in s));

#

InstallOtherMethod(IsLeftSimple, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local iter;
  
  if IsLeftZeroSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is a left zero semigroup");
    return true;
  elif HasNrLClasses(s) then 
    return NrLClasses(s)=1;
  fi;
  
  iter:=IteratorOfLClassData(s); NextIterator(iter);
  return IsDoneIterator(iter);
end);

#

InstallMethod(IsLeftSimple, "for an inverse semigroup", 
[IsInverseSemigroup], IsGroupAsSemigroup);

#

InstallOtherMethod(IsLeftZeroSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, imgs;

  gens:=Generators(s);
  imgs:=Set(List(gens, ImageSetOfTransformation));

  if Size(imgs)=1 and ForAll(gens, IsIdempotent) then
    return true;
  fi;
  return false;
end);

#

InstallOtherMethod(IsLeftZeroSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsTrivial);

#

InstallOtherMethod(IsMonogenicSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local gens, m, I, max, index, j, x, pos, f, i, p;

  gens:=ShallowCopy(GeneratorsOfSemigroup(s)); m:=Length(gens);

  if m=1 then
    Info(InfoSemigroups, 2, "the semigroup only has one generator");
    return true;
  fi;
  
  p:=Sortex(gens);
  gens:=Permuted(gens, p); m:=Length(gens);

  if m=1 then 
    Info(InfoSemigroups, 2, "the semigroup only has one generator and there are",
    " repeated generators");
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

  for i in [1..m] do 
    f:=gens[i];
    if ForAll(gens, x-> x in Semigroup(f)) then
      Info(InfoSemigroups, 2, "the semigroup is generated by generator ", i^(p^-1));
      SetMinimalGeneratingSet(s, [f]);
      return true;
    fi;
  od;
  Info(InfoSemigroups, 2, "at least one generator does not belong to the", 
   " semigroup generated by any");
  Info(InfoSemigroups, 2, "other generator.");
  return false;
end);

#JDM remove Other!

InstallOtherMethod(IsMonogenicSemigroup, "for an inverse semigroup", 
[IsInverseSemigroup and IsPartialPermSemigroup], 
function(s)
  if not IsMonogenicInverseSemigroup(s) then 
    return false;
  fi;
  return IsMonogenicSemigroup(Range(IsomorphismTransformationSemigroup(s)));
end);

#

InstallMethod(IsMonogenicInverseSemigroup, "for a trans. semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  if not IsInverseSemigroup(s) then 
    return false;
  fi;
  return IsMonogenicInverseSemigroup(Range(IsomorphismPartialPermSemigroup(s)));
end);
 
#

InstallMethod(IsMonogenicInverseSemigroup, "for an inverse semigroup", 
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local gens, m, I, max, index, j, x, pos, f, i, p;

  gens:=ShallowCopy(Generators(s)); m:=Length(gens);

  if m=1 then
    Info(InfoSemigroups, 2, "the semigroup only has one generator");
    return true;
  fi;
  
  p:=Sortex(gens);
  gens:=Permuted(gens, p); m:=Length(gens);

  if m=1 then 
    Info(InfoSemigroups, 2, "the semigroup only has one generator and there are",
    " repeated generators");
    return true;
  fi;
  
  I:=MinimalIdeal(s);
  if not IsCyclic(Range(IsomorphismPermGroup(I))) then 
    Info(InfoSemigroups, 2, "the minimal ideal is a non-cyclic group.");
    return false;
  fi;

  for i in [1..m] do 
    f:=gens[i];
    if ForAll(gens, x-> x in InverseSemigroup(f)) then
      Info(InfoSemigroups, 2, "the semigroup is generated by generator ", i^(p^-1));
      SetMinimalGeneratingSet(s, [f]);
      return true;
    fi;
  od;
  Info(InfoSemigroups, 2, "at least one generator does not belong to the", 
   " semigroup generated by any");
  Info(InfoSemigroups, 2, "other generator.");
  return false;
end);

#

InstallOtherMethod(IsMonoidAsSemigroup, "for a semigroup",
[IsSemigroup and HasGeneratorsOfSemigroup], 
 x-> not IsMonoid(x) and MultiplicativeNeutralElement(x)<>fail);

# Notes: is there a better method? JDM

InstallOtherMethod(IsOrthodoxSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local e, m, i, j;

  if not IsRegularSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is not regular");
    return false;
  fi;

  e:=Idempotents(s); m:=Length(e);
  
  for i in [1..m] do
    for j in [1..m] do
      if not (e[i]*e[j])^2=e[i]*e[j] then 
        Info(InfoSemigroups, 2, "the product of idempotents ", i," and ", j, 
        " is not an idempotent");
        return false;
      fi;
    od;
  od;

  return true;
end);

#

InstallMethod(IsPartialPermMonoid, "for a partial perm semigroup",
[IsPartialPermSemigroup],
function(s)
  if ForAny(GeneratorsOfSemigroup(s), x->       
   DomPP(x)=DomainOfPartialPermCollection(s)) then 
    SetGeneratorsOfMonoid(s, GeneratorsOfSemigroup(s));
    SetFilterObj(s, IsMonoid);
    return true;
  fi;
  return false;
end);

#

InstallOtherMethod(IsRectangularBand, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)

  if not IsSimpleSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is not simple");
    return false;
  elif HasIsBand(s) then
    return IsBand(s);
  fi;

  return IsHTrivial(s);
end);

InstallOtherMethod(IsRectangularBand, "for an inverse semigroup",
[IsInverseSemigroup], s-> IsHTrivial(s) and IsSimpleSemigroup(s));

#

InstallOtherMethod(IsRegularSemigroup, "for an acting semigroup", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local tester, n, rhofunc, lookfunc, data, i;

#  if IsSimpleSemigroup(s) then 
#    Info(InfoSemigroups, 2, "the semigroup is simple");
#    return true;
#  elif IsCompletelyRegularSemigroup(s) then 
#    Info(InfoSemigroups, 2, "the semigroup is completely regular");
#    return true;
  
  if HasGreensDClasses(s) then 
    return ForAll(GreensDClasses(s), IsRegularDClass);
  fi;

  tester:=IdempotentTester(s);
  rhofunc:=RhoFunc(s);

  # look for s not being regular
  lookfunc:=function(data, x)
    local rho, scc, i;
    if data!.repslens[data!.orblookup1[x[6]]]>1 then
      return true;
    fi;
    
    # data corresponds to the group of units...
    if ActionRank(x[4])=ActionDegree(x[4]) then 
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
  local o, lookingfor;
  
  if not f in s then 
    Info(InfoSemigroups, 2, "the element does not belong to the semigroup,");
    return fail;
  fi;
  
  if HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then
    Info(InfoSemigroups, 2, "the semigroup is regular,");
    return true;
  fi;
 
  if IsClosed(LambdaOrb(s)) then 
    o:=LambdaOrb(s);
  else
    o:=GradedLambdaOrb(s, f, true);
  fi;
        
  lookingfor:=function(o, x)
    return IdempotentTester(s)(x, RhoFunc(s)(f));
  end;
  
  return LookForInOrb(o, lookingfor, 2)<>false;
end);

#

InstallMethod(IsRegularSemigroupElementNC, 
"for an acting semigroup and acting element",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement], 
function(s, f)                                  
  local o, lookingfor;
 
  o:=GradedLambdaOrb(s, f, false);
        
  lookingfor:=function(o, x)
    return IdempotentTester(s)(x, RhoFunc(s)(f));
  end;
  
  return LookForInOrb(o, lookingfor, 2)<>false;
end);

#

InstallMethod(IsRightSimple, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local iter;

  if IsRightZeroSemigroup(s) then 
    Info(InfoSemigroups, 2, "the semigroup is a right zero semigroup");
    return true;
  elif HasNrRClasses(s) then 
    return NrRClasses(s)=1;
  fi;

  iter:=IteratorOfRClassData(s); NextIterator(iter);
  return IsDoneIterator(iter);
end);

InstallMethod(IsRightSimple, "for an inverse semigroup", 
[IsInverseSemigroup], IsGroupAsSemigroup);

#

InstallOtherMethod(IsRightZeroSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, kers;

  gens:=Generators(s);
  kers:=Set(List(gens, CanonicalTransSameKernel));

  if Length(kers)=1 and ForAll(gens, IsIdempotent) then
    return true;
  fi;

  return false;
end);

#

InstallOtherMethod(IsRightZeroSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsTrivial);

#

InstallOtherMethod(IsSemiband, "for a semigroup",
[IsSemigroup and HasGeneratorsOfSemigroup], IsIdempotentGenerated);

#

InstallOtherMethod(IsSemilatticeAsSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
 s-> IsCommutative(s) and IsBand(s));

#

InstallOtherMethod(IsSemilatticeAsSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], s-> ForAll(Generators(s), x-> x^2=x));

#

InstallMethod(IsSimpleSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local gens, r, o, f;

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

  for f in gens do
    r:=RankOfTransformation(f);
    o:=Orb(gens, ImageSetOfTransformation(f), OnSets, 
        rec(lookingfor:=function(o, x) return Length(x)<r or not
         IsInjectiveListTrans(x, f); end));
    Enumerate(o);
    if IsPosInt(PositionOfFound(o)) then 
      return false;
    fi;
  od;

  SetNrDClasses(s, 1);

  return true;
end);

#

InstallMethod(IsSimpleSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsGroupAsSemigroup);

#

InstallMethod(IsSynchronizingSemigroup, "for a trans. semi.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local n, o;

  n:=DegreeOfTransformationSemigroup(s);

  o:=Orb(s, [1..n], OnSets, 
        rec(schreier:=true, 
        lookingfor:=function(o, x) return Length(x)=1; end));

  Enumerate(o);
  if IsPosInt(PositionOfFound(o)) then 
    Info(InfoSemigroups, 2, "the product of the generators: ",
    TraceSchreierTreeForward(o, PositionOfFound(o)));

    Info(InfoSemigroups, 2, "is a constant function with value ", 
     o[PositionOfFound(o)][1]);
    return true;
  fi;

  return false;
end);

#

InstallMethod(IsTrivial, "for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local gens;
  gens:=Generators(s);
  return (Length(gens)>1 and not ForAny(gens, x-> gens[1]<>x)) or 
  gens[1]^2=gens[1];
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
  #elif IsTrivial(g) then #JDM is this any better than the below?
  #  return IsBand(s);
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
          if not ForAny(GradedLambdaOrb(g, o[j], true), x-> tester(x, rho))
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

InstallOtherMethod(IsZeroGroup, "for a semigroup",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local zero;

  zero:=MultiplicativeZero(s);

  if zero=fail then 
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

InstallMethod(IsZeroRectangularBand, "for a semigroup", 
[IsSemigroup and HasGeneratorsOfSemigroup],
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

InstallOtherMethod(IsZeroSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local z, gens, m, i, j;

  z:=MultiplicativeZero(s);
  gens:=GeneratorsOfSemigroup(s);

  if z=fail then
    Info(InfoSemigroups, 2, "the semigroup does not have a zero");
    return false;
  fi;

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

#JDM remove other!

InstallOtherMethod(IsZeroSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsTrivial);

# Notes: this should be tested more!

InstallOtherMethod(IsZeroSimpleSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local n, gens, o, lookingfor, r, i, z, ker, iter;
    
  n:=ActionDegree(s); gens:=GeneratorsOfSemigroup(s);

  #orbit of images, looking for more than two ranks.
  o:=Orb(gens, [1..n], OnSets, rec(schreier:=true,
    gradingfunc:=function(o, x) return Length(x); end,
    onlygrades:=[1..n], ranks:=EmptyPlist(2), first:=true,
     lookingfor:=function(o, x) 
        local ranks, r;
        if o!.first then 
          o!.first:=false;
          return false;
        fi;
        ranks:=o!.ranks; r:=Length(x);
        if not r in ranks then 
          if Length(ranks)=2 then 
            return true;
          else
            AddSet(ranks, r);
          fi;
        fi;
        return false;
      end)); 

  Enumerate(o);

  if IsPosInt(PositionOfFound(o)) or Length(o!.ranks)=1 then # 3 or 1 ranks
    Info(InfoSemigroups, 2, "elements of the semigroup have either 1 or >2", 
    " different ranks.");
    return false;
  fi;

  # find zero
  if not HasMultiplicativeZero(s) then 
    r:=Minimum(Grades(o)); i:=Position(Grades(o), r);
    z:=EvaluateWord(gens, TraceSchreierTreeForward(o, i));

    if not z^2=z then 
      return false;
    fi;

    ker:=CanonicalTransSameKernel(z);
    o:=Orb(gens, ker, OnKernelsAntiAction, 
     rec(lookingfor:=function(o, x) return not x=ker; end));
    Enumerate(o);

    if IsPosInt(PositionOfFound(o)) or 
     not Size(GreensDClassOfElementNC(s, z))=1 then 
     # this could be faster in the case that the D-class > 1
      return false;
    fi;

    SetMultiplicativeZero(s, z);
  elif MultiplicativeZero(s)=fail then 
    Info(InfoSemigroups, 2, "no multiplicative zero.");
    return false;
  else
    z:=MultiplicativeZero(s);
  fi;

  iter:=IteratorOfDClassData(s); i:=0;
  repeat 
    i:=i+1; NextIterator(iter);
  until i>2 or IsDoneIterator(iter);
  
  if i=2 then 
    return true;
  fi;

  Info(InfoSemigroups, 2, "more than two D-classes.");  
  return false;
end);

#

InstallMethod(IsZeroSimpleSemigroup, "for an inverse semigroup",
[IsInverseSemigroup],
function(s)
  return NrDClasses(s)=2 and not MultiplicativeZero(s)=fail;  
end);

#EOF
