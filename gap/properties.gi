
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

#III

# new for 0.1! - IrredundantGeneratingSubset - "for a tranformation coll."
###########################################################################
# Notes: this does not work all that well, use SmallGeneratingSet first. 

InstallMethod(IrredundantGeneratingSubset, "for a transformation collection", 
[IsTransformationCollection],
function(coll)
  local gens, j, out, i, redund, f;
  
  if IsTransformationSemigroup(coll) then 
    coll:=ShallowCopy(Generators(coll));
  fi;
  
  gens:=Set(ShallowCopy(coll)); j:=Length(gens);
  coll:=Permuted(coll, Random(SymmetricGroup(Length(coll))));
  Sort(coll, function(x, y) return ActionRank(x)>ActionRank(y); end);
  
  out:=EmptyPlist(Length(coll));
  redund:=EmptyPlist(Length(coll));
  i:=0;

  repeat 
    i:=i+1; f:=coll[i];
    if InfoLevel(InfoSemigroups)>=3 then 
      Print("at \t", i, " of \t", Length(coll), " with \t", Length(redund), 
      " redundant, \t", Length(out), " non-redundant\r");
    fi;

    if not f in redund and not f in out then 
      if f in Semigroup(Difference(gens, [f])) then 
        AddSet(redund, f); gens:=Difference(gens, [f]);
      else
        AddSet(out, f);
      fi;
    fi;
  until Length(redund)+Length(out)=j;

  if InfoLevel(InfoSemigroups)>1 then 
    Print("\n");
  fi;
  return out;
end);

# new for 0.7! - IrredundantGeneratingSubset - "for a partial perm coll."
###########################################################################
# Notes: this does not work all that well, use SmallGeneratingSet first. 

InstallOtherMethod(IrredundantGeneratingSubset, "for a partial perm collection", 
[IsPartialPermCollection],
function(coll)
  local gens, j, out, i, redund, f;
  
  if IsPartialPermSemigroup(coll) then 
    coll:=ShallowCopy(Generators(coll));
  fi;
  
  gens:=Set(ShallowCopy(coll)); j:=Length(gens);
  coll:=Permuted(coll, Random(SymmetricGroup(Length(coll))));
  Sort(coll, function(x, y) return ActionRank(x)>ActionRank(y); end);
  
  out:=EmptyPlist(Length(coll));
  redund:=EmptyPlist(Length(coll));
  i:=0;

  repeat 
    i:=i+1; f:=coll[i];
    if InfoLevel(InfoSemigroups)>=3 then 
      Print("at \t", i, " of \t", Length(coll), " with \t", Length(redund), 
      " redundant, \t", Length(out), " non-redundant\r");
    fi;

    if not f in redund and not f in out then 
      if f in InverseSemigroup(Difference(gens, [f])) then 
        AddSet(redund, f); gens:=Difference(gens, [f]);
      else
        AddSet(out, f);
      fi;
    fi;
  until Length(redund)+Length(out)=j;

  if InfoLevel(InfoSemigroups)>1 then 
    Print("\n");
  fi;
  return out;
end);

#IIIAAA

# new for 0.4! - IsAbundantSemigroup - "for a trans. semigroup"
###########################################################################

InstallMethod(IsAbundantSemigroup, "for a trans. semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local iter, n, ht, ht_o, reg, i, data, f, ker, val, o, scc;

  Info(InfoWarning, 1, "this will sometimes return a false positive.");

  if HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then 
    Info(InfoSemigroups, 2, "semigroup is regular");
    return true;
  fi;

  iter:=IteratorOfRClassData(s); n:=ActionDegree(s);
  ht:=HTCreate([1..n], rec(hashlen:=s!.opts!.hashlen!.S));
  ht_o:=HTCreate([1,1,1,1], rec(hashlen:=s!.opts!.hashlen!.S));
  reg:=[]; i:=0; 

  repeat
    repeat #JDM this should become an method for IteratorOfRStarClasses
           # and IsAbundantRClass...
      data:=NextIterator(iter);
    until HTValue(ht_o, data{[1,2,4,5]})=fail or IsDoneIterator(iter); 
    if not IsDoneIterator(iter) then 
      HTAdd(ht_o, data{[1,2,4,5]}, true);

      #f:=RClassRepFromData(s, data); ker:=CanonicalTransSameKernel(f);
      val:=HTValue(ht, ker);

      if val=fail then #new kernel
        i:=i+1; HTAdd(ht, ker, i);
        val:=i; reg[val]:=false;
      fi;
        
      if reg[val]=false then #old kernel
        #o:=ImageOrbitFromData(s, data); scc:=ImageOrbitSCCFromData(s, data);
        reg[val]:=ForAny(scc, j-> IsInjectiveListTrans(o[j], ker));
      fi;
    fi;
  until IsDoneIterator(iter);

  return ForAll(reg, x-> x);
end);

# new for 0.4! - IsAdequateSemigroup - "for a trans. semigroup"
###########################################################################

InstallMethod(IsAdequateSemigroup, "for a trans. semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
s-> IsAbundantSemigroup(s) and IsBlockGroup(s));

#IIIBBB

# mod for 0.1! - IsBand - "for a transformation semigroup"
###########################################################################

InstallOtherMethod(IsBand, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], s-> 
 IsCompletelyRegularSemigroup(s) and IsHTrivial(s));

InstallOtherMethod(IsBand, "for an inverse semigroup", 
[IsInverseSemigroup], IsSemilatticeAsSemigroup);
#JDM remove other!

# new for 0.1! - IsBlockGroup - "for a transformation semigroup"
#############################################################################

InstallMethod(IsBlockGroup, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local iter, i, f, o, scc, reg, d;

  if HasIsInverseSemigroup(s) and IsInverseSemigroup(s) then 
    Info(InfoSemigroups, 2, "inverse semigroup");
    return true;
  elif (HasIsRegularSemigroup(s) and IsRegularSemigroup(s)) and
   (HasIsInverseSemigroup(s) and not IsInverseSemigroup(s)) then 
    Info(InfoSemigroups, 2, "regular but non-inverse semigroup");
    return false;
  fi;

  iter:=IteratorOfDClassData(s); 
    
  for d in iter do
    #i:=NrIdempotentsRClassFromData(s, d[1]);
    if i>1 then #this could be better
    # we only need to find 2 transversals to return false.
      Info(InfoSemigroups, 2, "at least one R-class contains more than 1", 
      " idempotent");
      return false;
    fi;
    
    #now check that D-classes are square...
    #f:=AsSet(DClassRepFromData(s, d)![1]);
    #o:=KernelOrbitFromData(s, d);
    #scc:=KernelOrbitSCCFromData(s, d[2]);
    reg:=false;
    for i in scc do 
      if IsInjectiveListTrans(o[i], f) then 
        if reg then 
          Info(InfoSemigroups, 2, "at least one L-class contains more than 1",
          " idempotent");
          return false;
        fi;
        reg:=true;
      fi;
    od;
  od;

  return true;
end);

# new for 0.2! - IsBrandtSemigroup - "for a transformation semigroup"
#############################################################################

InstallOtherMethod(IsBrandtSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
s-> IsZeroSimpleSemigroup(s) and IsInverseSemigroup(s));

InstallOtherMethod(IsBrandtSemigroup, "for an inverse semigroup", 
[IsInverseSemigroup], IsZeroSimpleSemigroup);
#JDM remove other!

#IIICCC

# new for 0.1! - IsCliffordSemigroup - "for a transformation semigroup"
###########################################################################

InstallOtherMethod(IsCliffordSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
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

  idem:=List(gens, x->TRANS_IMG_KER_NC(ImageSetOfTransformation(x),
   CanonicalTransSameKernel(x)));

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

InstallOtherMethod(IsCliffordSemigroup, "for an inverse semigroup", 
[IsInverseSemigroup and IsPartialPermSemigroup], 
s-> ForAll(OrbSCC(LongOrb(s)), x-> Length(x)=1));
#JDM remove other!

# new for 0.1! - IsCommutativeSemigroup - "for a transformation semigroup"
###########################################################################

InstallOtherMethod(IsCommutativeSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, n, i, j; 

  gens:=Generators(s);
  n:=Length(gens);

  for i in [1..n] do
    for j in [i+1..n] do
      if not gens[i]*gens[j]=gens[j]*gens[i] then 
        Info(InfoSemigroups, 2, "generators ", i, " and ",  j, " do not commute");
        return false;
      fi;
    od;
  od;

  return true;
end);

# new for 0.1! - IsCommutativeSemigroup - "for a partial perm semigroup"
###########################################################################

InstallOtherMethod(IsCommutativeSemigroup, "for a partial perm semigroup",
[IsPartialPermSemigroup],
function(s)
  local gens, n, i, j; 

  gens:=Generators(s);
  n:=Length(gens);

  for i in [1..n] do
    for j in [i+1..n] do
      if not gens[i]*gens[j]=gens[j]*gens[i] then 
        Info(InfoSemigroups, 2, "generators ", i, " and ",  j, " do not commute");
        return false;
      fi;
    od;
  od;

  return true;
end);

# new for 0.1! - IsCompletelyRegularSemigroup - "for a trans. semigp."
###########################################################################

InstallOtherMethod(IsCompletelyRegularSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, o, f;

  if HasIsRegularSemigroup(s) and not IsRegularSemigroup(s) then 
    Info(InfoSemigroups, 2, "semigroup is not regular");
    return false;
  fi;

  gens:=Generators(s);

  for f in gens do
    o:=Orb(gens, ImageSetOfTransformation(f), OnSets, 
     rec(lookingfor:=function(o, x) 
     return not IsInjectiveListTrans(x, f); end));
    Enumerate(o);
    if not PositionOfFound(o)=false then 
      Info(InfoSemigroups, 2, "at least one H-class is not a subgroup");
      return false;
    fi;
  od;

  return true;
end);

InstallOtherMethod(IsCompletelyRegularSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsCliffordSemigroup);
# JDM remove other!

# new for 0.1! - IsCompletelySimpleSemigroup - "for a trans. semigroup"
###########################################################################
# Notes: this test required to avoid conflict with Smallsemi, DeclareSynonymAttr
# causes problems. 

InstallOtherMethod(IsCompletelySimpleSemigroup, "for a semi.",
[IsSemigroup and HasGeneratorsOfSemigroup], 
 x-> IsSimpleSemigroup(x) and IsFinite(x));

#IIIFFF

# new for 0.7! - IsFactorisableSemigroup - "for a partial perm semigroup"
###########################################################################
#JDM prove this method is correct!

if IsBound(NaturalLeqPP) then 
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
fi;

#IIIGGG

# new for 0.1! - IsHTrivial - "for a transformation semigroup"
###########################################################################

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

# new for 0.1! - IsHTrivial - "for a partial perm inv semigroup"
###########################################################################

InstallMethod(IsHTrivial, "for a partial perm inv semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  EnumerateInverseSemiData(s);
  return ForAll(LongOrb(s)!.schutz, x-> IsTrivial(x[2]));
end);

# new for 0.1! - IsHTrivial - "for a D-class of a trans. semigp"
###########################################################################

InstallOtherMethod(IsHTrivial, "for a D-class of a trans. semigp", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
  d-> NrHClasses(d)=Size(d));

InstallOtherMethod(IsHTrivial, "for a D-class of a part perm semigp",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup],
  d-> NrHClasses(d)=Size(d));

# new for 0.1! - IsLTrivial - "for a transformation semigroup"
#############################################################################

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

InstallMethod(IsLTrivial, "for an inverse semigroup", 
[IsInverseSemigroup and IsPartialPermSemigroup],
s-> ForAll(OrbSCC(LongOrb(s)), x-> Length(x)=1) and IsHTrivial(s));

# new for 0.1! - IsLTrivial - "for a D-class of a trans. semigp"
#############################################################################

InstallOtherMethod(IsLTrivial, "for a D-class of a trans. semigp", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
  d-> NrLClasses(d)=Size(d));

InstallOtherMethod(IsLTrivial, "for a D-class of a part perm semigp", 
[IsGreensDClass and IsGreensClassOfPartPermSemigroup], 
  d-> NrLClasses(d)=Size(d));

# fix for 0.4! - IsRTrivial - "for a transformation semigroup"
#############################################################################

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

InstallMethod(IsRTrivial, "for an inverse semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup], IsLTrivial);

# new for 0.1! - IsRTrivial -  "for D-class of a trans. semigp."
#############################################################################

InstallOtherMethod(IsRTrivial, "for D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  #JDM maybe better if we had an enumerator of R-classes of d...
  return NrRClasses(d)=Size(d);
end);

InstallOtherMethod(IsRTrivial, "for D-class of a part. perm. semigp.",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup], IsLTrivial);

# new for 1.0! - IsGroupAsSemigroup - "for an acting semigroup"
###########################################################################
 
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

#IIIIII

# mod for 0.8! - IsIdempotentGenerated - "for a trans semi"
###########################################################################
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

# new for 0.7! - IsIdempotentGenerated - "for an inverse semigroup"
###########################################################################

InstallOtherMethod(IsIdempotentGenerated, "for an inverse semigroup",
[IsInverseSemigroup], IsSemilatticeAsSemigroup);

# new for 0.7! - IsInverseMonoid - "for a trans semigroup"
###########################################################################

InstallOtherMethod(IsInverseMonoid, "for a trans semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
s-> IsMonoid(s) and IsInverseSemigroup(s));

# new for 0.7! - IsInverseMonoid - "for a partial perm semigroup"
###########################################################################

InstallOtherMethod(IsInverseMonoid, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> IsMonoid(s) and IsInverseSemigroup(s));

# new for 0.1! - IsInverseSemigroup - "for a transformation semigroup"
###########################################################################

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

InstallOtherMethod(IsInverseSemigroup, "for a semigroup of partial perms",
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
s-> ForAll(Generators(s), x-> x^-1 in s));

#IIILLL

# new for 0.2! - IsLeftSimple - "for a transformation semigroup"
###########################################################################

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

InstallMethod(IsLeftSimple, "for an inverse semigroup", 
[IsInverseSemigroup], IsGroupAsSemigroup);

# new for 0.1! - IsLeftZeroSemigroup - "for a transformation semigroup"
###########################################################################

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

InstallOtherMethod(IsLeftZeroSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsTrivial);

#IIIMMM

# new for 0.2 - IsMonogenicSemigroup - "for a transformation semigroup"
#############################################################################

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

# new for 0.7 - IsMonogenicSemigroup - "for an inverse semigroup"
#############################################################################
#JDM remove Other!

InstallOtherMethod(IsMonogenicSemigroup, "for an inverse semigroup", 
[IsInverseSemigroup and IsPartialPermSemigroup], 
function(s)
  if not IsMonogenicInverseSemigroup(s) then 
    return false;
  fi;
  return IsMonogenicSemigroup(Range(IsomorphismTransformationSemigroup(s)));
end);

# new for 0.7 - IsMonogenicInverseSemigroup - "for a trans. semigroup"
#############################################################################

InstallMethod(IsMonogenicInverseSemigroup, "for a trans. semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  if not IsInverseSemigroup(s) then 
    return false;
  fi;
  return IsMonogenicInverseSemigroup(Range(IsomorphismPartialPermSemigroup(s)));
end);
 
# new for 0.7 - IsMonogenicInverseSemigroup - "for an inverse semigroup"
#############################################################################

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

# new for 0.1! - IsMonoidAsSemigroup - "for a  semigroup"
#############################################################################

InstallOtherMethod(IsMonoidAsSemigroup, "for a semigroup",
[IsSemigroup and HasGeneratorsOfSemigroup], 
 x-> not IsMonoid(x) and MultiplicativeNeutralElement(x)<>fail);

# new for 0.7! - IsomorphismPartialPermMonoid - "for a perm group"
#############################################################################

if Semigroups_C then 
  InstallMethod(IsomorphismPartialPermMonoid, "for a perm group",
  [IsPermGroup],
  function(g)
    local dom;

    dom:=MovedPoints(g);
    return MappingByFunction(g, InverseMonoid(List(GeneratorsOfGroup(g), p-> 
     AsPartialPerm(p, dom))), p-> AsPartialPerm(p, dom), f-> AsPermutation(f));
  end);
else
  InstallMethod(IsomorphismPartialPermMonoid, "for a perm group",
  [IsPermGroup], SemigroupsIsNotCompiled);
fi;

# new for 0.7! - IsomorphismPartialPermSemigroup - "for a perm group"
#############################################################################

if Semigroups_C then 
  InstallMethod(IsomorphismPartialPermSemigroup, "for a perm group",
  [IsPermGroup],
  function(g)
    local dom;

    dom:=MovedPoints(g);
    return MappingByFunction(g, InverseSemigroup(List(GeneratorsOfGroup(g), p-> 
     AsPartialPerm(p, dom))), p-> AsPartialPerm(p, dom), f-> AsPermutation(f));
  end);
else
  InstallMethod(IsomorphismPartialPermSemigroup, "for a perm group",
  [IsPermGroup], SemigroupsIsNotCompiled);
fi;

# new for 0.7! - IsomorphismPartialPermSemigroup - "for trans semi"
#############################################################################

if Semigroups_C then 
  InstallOtherMethod(IsomorphismPartialPermMonoid, "for a part perm semi",
  [IsPartialPermSemigroup],
  function(s)

    if IsMonoid(s) then 
      return MappingByFunction(s, s, x-> x, x-> x);
    elif not IsMonoidAsSemigroup(s) then 
      Error("usage, partial perm. semigroup satisfying IsMonoidAsSemigroup,");
      return;
    fi;

    return MappingByFunction(s, 
     InverseMonoid(Difference(Generators(s), [One(s)])), x-> x, x-> x); 
  end);
fi;

# new for 0.7! - IsomorphismPartialPermSemigroup - "for trans semi"
#############################################################################

if Semigroups_C then 
  InstallOtherMethod(IsomorphismPartialPermMonoid, "for a trans semi",
  [IsTransformationSemigroup and HasGeneratorsOfSemigroup],
  function(s)
    local iso;

    if not IsInverseMonoid(s) then 
      Error("usage: the argument should be an inverse monoid,");
      return;
    fi;
    
    iso:=function(f)
      local dom, ran;
    
      dom:=OnSets([1..ActionDegree(s)], InversesOfSemigroupElementNC(s, f)[1]);
      ran:=List(dom, i-> i^f);
      return PartialPermNC(dom, ran);
    end;

    return MappingByFunction(s, 
     InverseMonoid(List(GeneratorsOfSemigroup(s), iso)), iso, 
      x-> AsTransformation(x, ActionDegree(s)));
  end);
else
  InstallOtherMethod(IsomorphismPartialPermMonoid, "for a trans semi",
  [IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
  SemigroupsIsNotCompiled);
fi;

# new for 0.7! - IsomorphismPartialPermSemigroup - "for trans semi"
#############################################################################

if Semigroups_C then 
  InstallOtherMethod(IsomorphismPartialPermSemigroup, "for a trans semi",
  [IsTransformationSemigroup and HasGeneratorsOfSemigroup],
  function(s)
    local iso;

    if not IsInverseSemigroup(s) then 
      Error("usage: the argument should be an inverse semigroup,");
      return;
    fi;
  
    iso:=function(f)
      local dom, ran;
  
      dom:=OnSets([1..ActionDegree(s)], InversesOfSemigroupElementNC(s, f)[1]);
      ran:=List(dom, i-> i^f);
      return PartialPermNC(dom, ran);
    end;

    return MappingByFunction(s, 
     InverseSemigroup(List(GeneratorsOfSemigroup(s), iso)), iso, 
      x-> AsTransformation(x, ActionDegree(s)));
  end);
else
  InstallOtherMethod(IsomorphismPartialPermSemigroup, "for a trans semi",
  [IsTransformationSemigroup and HasGeneratorsOfSemigroup],
  SemigroupsIsNotCompiled);
fi;

# new for 0.7! - IsomorphismReesMatrixSemigroup - "for a simple inverse semi"
#############################################################################

InstallOtherMethod(IsomorphismReesMatrixSemigroup, "for a simple inverse semi",
[IsPartialPermSemigroup and IsInverseSemigroup and IsSimpleSemigroup],
function(s)
  return IsomorphismReesMatrixSemigroup(DClass(s, Representative(s)));
end);


# new for 0.7! - IsomorphismTransformationSemigroup - "for partial perm semi"
##############################################################################

InstallOtherMethod(IsomorphismTransformationSemigroup, "for partial perm semi",
[IsPartialPermSemigroup],
function(s)
  local n, gens1, m, gens2, iso, u, i;
 
  if DomainOfPartialPermCollection(s)=[] then 
    # semigroup consisting of the empty set
    return MappingByFunction(s, Semigroup(Transformation([1])), 
    x-> Transformation([1]), x-> PartialPermNC([]));
  fi;

  n:=DegreeOfPartialPermCollection(s)+1;
  gens1:=GeneratorsOfSemigroup(s); 
  m:=Length(gens1);
  gens2:=EmptyPlist(m);

  for i in [1..m] do 
    gens2[i]:=AsTransformation(gens1[i], n);
  od;

  return MappingByFunction(s, Semigroup(gens2), x-> AsTransformation(x, n),
   AsPartialPermNC);
end);

# new for 0.7! - IsomorphismTransformationMonoid - "for partial perm semi"
##############################################################################

InstallOtherMethod(IsomorphismTransformationMonoid, "for partial perm semi",
[IsPartialPermSemigroup],
function(s)
  local n, gens1, m, gens2, iso, u, i;
  
  if not IsMonoidAsSemigroup(s) then 
    Error("the argument should be a monoid,");
    return;
  fi;

  n:=LargestMovedPoint(s)+1;
  gens1:=GeneratorsOfMonoid(s); 
  m:=Length(gens1);
  gens2:=EmptyPlist(m);

  for i in [1..m] do 
    gens2[i]:=AsTransformation(gens1[i], n);
  od;

  return MappingByFunction(s, Monoid(gens2), x-> AsTransformation(x, n),
   AsPartialPermNC);
end);

# new for 0.5! - IsomorphismTransformationMonoid - "for a perm group"
#############################################################################

InstallOtherMethod(IsomorphismTransformationMonoid, "for a perm group",
[IsPermGroup], 
function(g)
  local n, p, iso;
  
  n:=NrMovedPoints(g);
  p:=MappingPermListList(MovedPoints(g), [1..n]);
  iso:=x-> AsTransformation(x^p, n);  
  return MappingByFunction(g, Monoid(List(GeneratorsOfGroup(g), iso)), iso);
end);

# new for 0.1! - IsomorphismTransformationMonoid - "for trans semi"
#############################################################################

InstallMethod(IsomorphismTransformationMonoid, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)

  if not IsMonoidAsSemigroup(s) then 
    Error( "Usage: trans. semigroup satisfying IsMonoidAsSemigroup," );
    return;
  fi;

  return MappingByFunction(s, Monoid(Difference(Generators(s),
  [TransformationNC([1..DegreeOfTransformationSemigroup(s)])])), x-> x, x-> x);
end);

# new for 0.1! - IsomorphismPermGroup - "for a transformation semigroup"
#############################################################################

InstallOtherMethod(IsomorphismPermGroup, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
 
   if not IsGroupAsSemigroup(s)  then
     Error( "Usage: trans. semigroup satisfying IsGroupAsSemigroup,");
     return; 
   fi;
 
   return MappingByFunction(s, Group(List(Generators(s), AsPermutation)), 
    AsPermutation, x-> AsTransformation(x, ActionDegree(s)));
 end);

# new for 0.7! - IsomorphismPermGroup - "for a partial perm semigroup"
#############################################################################

InstallOtherMethod(IsomorphismPermGroup, "for a partial perm semigroup", 
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
function(s)

  if not IsGroupAsSemigroup(s)  then
    Error( "Usage: partial perm. semigroup satisfying IsGroupAsSemigroup,");
    return; 
  fi;

  return MappingByFunction(s, Group(List(Generators(s), AsPermutation)), 
   AsPermutation, x-> AsPartialPerm(x, DomainOfPartialPermCollection(s)));
end);

# new for 0.7! - IsomorphismTransformationSemigroup - "for a matrix semigroup"
###########################################################################

#JDM this should be improved: inverse of the function is missing, and a similar
#approach as used in the method for IsomorphismTransformationSemigroup for a
#semigroup of binary relations should be used to reduce the number of points
#acted on. 

InstallOtherMethod(IsomorphismTransformationSemigroup, "for a matrix semigroup",
[IsMatrixSemigroup], 
function(S)        
  local n, F, T;
  n:=Length(GeneratorsOfSemigroup(S)[1][1]);
  F:=BaseDomain(GeneratorsOfSemigroup(S)[1]);        
  T:=Semigroup(TransformationActionNC(S, Elements(F^n), OnRight));        
  return MappingByFunction(S, T,
   x-> TransformationActionNC(x, Elements(F^Size(F)), OnRight));
end);

# new for 1.0! - IsomorphismTransformationSemigroup - "for a semi of bin rel"
###########################################################################

InstallOtherMethod(IsomorphismTransformationSemigroup, "for semigp of bin rels",
[IsBinaryRelationSemigroup], 
function(s)        
  local n, pts, o, t, pos, i;

  n:=DegreeOfBinaryRelation(Generators(s)[1]);
  pts:=EmptyPlist(2^n);

  for i in [1..n] do 
    o:=Orb(s, [i], OnPoints); #JDM multiseed orb
    Enumerate(o);
    pts:=Union(pts, AsList(o));
  od;
  ShrinkAllocationPlist(pts);
  t:=Semigroup(List(Generators(s), x-> TransformationOpNC(x, pts, OnPoints)));
  
  return MappingByFunction(s, t, x-> TransformationOpNC(x, pts, OnPoints),
  x-> BinaryRelationOnPoints(List([1..n], i-> pts[pos[i]^x])));
end);

#IIIOOO

# new for 0.1! - IsOrthodoxSemigroup - "for a transformation semigroup"
###########################################################################
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

#IIIPPP

# mod for 1.0! - IsPartialPermMonoid - "for a partial perm semigroup"
###########################################################################

if IsBound(DomPP) then 
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
fi;

#IIIRRR

# new for 0.1! - IsRectangularBand - "for a transformation semigroup"
###########################################################################

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

# new for 1.0! - IsRegularSemigroupElement - "for acting semigroup and elt"
###########################################################################

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

# new for 1.0! - IsRegularSemigroupElementNC - "for acting semigroup and elt"
###########################################################################

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

# new for 0.2! - IsRightSimple - "for a transformation semigroup"
###########################################################################

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

# new for 0.1! - IsRightZeroSemigroup - "for a transformation semigroup"
###########################################################################

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

InstallOtherMethod(IsRightZeroSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsTrivial);

#IIISSS

# new for 0.1! - IsSemiband - "for a transformation semigroup"
###############################################################################

InstallOtherMethod(IsSemiband, "for a semigroup",
[IsSemigroup and HasGeneratorsOfSemigroup], IsIdempotentGenerated);

# new for 0.1! - IsSemilatticeAsSemigroup - "for a trans. semigroup"
###############################################################################

InstallOtherMethod(IsSemilatticeAsSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
 s-> IsCommutative(s) and IsBand(s));

# new for 0.7! - IsSemilatticeAsSemigroup - "for an inv. semi""
###############################################################################

InstallOtherMethod(IsSemilatticeAsSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], s-> ForAll(Generators(s), x-> x^2=x));

# new for 0.1! - IsSimpleSemigroup - "for a tran. semi."
###########################################################################

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

InstallMethod(IsSimpleSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsGroupAsSemigroup);

# new for 0.1! - IsSynchronizingSemigroup - "for a trans. semi. or coll."
###########################################################################

InstallMethod(IsSynchronizingSemigroup, "for a trans. semi. or coll.", 
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

#IIITTT

# new for 0.7 - IsTrivial - "for a semigroup with generators"
###########################################################################

InstallMethod(IsTrivial, "for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local gens;
  gens:=Generators(s);
  return (Length(gens)>1 and not ForAny(gens, x-> gens[1]<>x)) or 
  gens[1]^2=gens[1];
end); 


#IIIUUU

# new for 1.0! - IsUnitRegularSemigroup - "for an acting semigroup"
###########################################################################

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

#IIIZZZ

# new for 0.1! - IsZeroGroup - "for a semigroup"
###########################################################################

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

# new for 0.2! - IsZeroRectangularBand - "for a semigroup"
###########################################################################

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

# new for 0.1! - IsZeroSemigroup - "for a transformation semigroup"
###########################################################################

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

# new for 0.2! - IsZeroSimpleSemigroup - "for a transformation semigroup"
###########################################################################
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

InstallMethod(IsZeroSimpleSemigroup, "for an inverse semigroup",
[IsInverseSemigroup],
function(s)
  return NrDClasses(s)=2 and not MultiplicativeZero(s)=fail;  
end);

#MMM

# new for 0.1! - MinimalIdeal - "for a transformation semigroup"
###########################################################################

InstallMethod(MinimalIdeal, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local n, gens, max, o, i, bound, f;

  n:=ActionDegree(s);
  gens:=Generators(s);
  max:=Maximum(List(gens, ActionDegree));

  if max=n then 
    bound:=2^n;
  else
    bound:=Sum([1..max], x-> Binomial(n, x));
  fi;

  o:=Orb(gens, [1..n], OnSets, rec( schreier:=true,
   gradingfunc:=function(o, x) return Length(x); end,
    onlygrades:=[1..max],
     lookingfor:=function(o, x) return Length(x)=1; end));
   
  Enumerate(o, bound);

  if IsPosInt(PositionOfFound(o)) then 
    i:=PositionOfFound(o);
  else
    i:=Position(Grades(o), Minimum(Grades(o))); 
  fi;

  f:=EvaluateWord(gens, TraceSchreierTreeForward(o, i));
  i:=Semigroup(Elements(GreensDClassOfElementNC(s, f)));

  #i:=SemigroupIdealByGenerators(s, [f]);
  SetIsSimpleSemigroup(i, true);
  #SetIsMinimalIdeal(i, true);
  #SetUnderlyingDClassOfMinIdeal(i, GreensDClassOfElement(s, f));
  #return i;
  return i;#JDM temp. 
end);

# new for 0.7! - MinimalIdeal - "for a partial perm semi"
###########################################################################

if IsBound(OnIntegerSetsWithPP) then 
  InstallMethod(MinimalIdeal, "for a partial perm semi",
  [IsPartialPermSemigroup],
  function(s)
    local n, gens, max, bound, o, i, f, I;

    n:=ActionDegree(s);
    gens:=Generators(s);
    max:=Maximum(List(gens, ActionDegree));

    if max=n then
      bound:=2^n;
    else
      bound:=Sum([1..max], x-> Binomial(n, x));
    fi;

    o:=Orb(gens, DomainOfPartialPermCollection(s), OnIntegerSetsWithPP, 
      rec( schreier:=true,
           gradingfunc:=function(o, x) return Length(x); end,
           onlygrades:=[0..max],
           lookingfor:=function(o, x) return Length(x)=0; end));
    
    Enumerate(o, bound);

    if IsPosInt(PositionOfFound(o)) then
      i:=PositionOfFound(o);
    else
      i:=Position(Grades(o), Minimum(Grades(o)));
    fi;

    f:=EvaluateWord(gens, TraceSchreierTreeForward(o, i));
    I:=InverseSemigroup(Elements(GreensDClassOfElementNC(s, f)));
    SetIsGroupAsSemigroup(I, true);
    return I;
  end);
fi;

#NNN

# new for 0.5! - NrElementsOfRank - "for a transformation semigroup"
#############################################################################

InstallMethod(NrElementsOfRank, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsPosInt],
function(s, m)
  local iter, tot, r;
  
  if m > ActionDegree(s) then 
    return 0;
  elif m > MaximumList(List(Generators(s), Rank)) then 
    return 0;
  fi;

  iter:=IteratorOfRClasses(s);

  tot:=0;

  for r in iter do 
    if r!.data[1]=m then 
      tot:=tot+Size(r);
    fi;
  od;

  return tot;
end);

# new for 0.7! - NrElementsOfRank - "for a partial perm semigroup"
#############################################################################

InstallOtherMethod(NrElementsOfRank, "for a partial perm semigroup",
[IsPartialPermSemigroup, IsPosInt],
function(s, m)
  local iter, tot, d;
  
  if m > ActionDegree(s) then
    return 0;
  elif m > MaximumList(List(Generators(s), Rank)) then
    return 0;
  fi;

  iter:=IteratorOfDClasses(s);

  tot:=0;

  for d in iter do
    if Rank(Representative(d))=m then
      tot:=tot+Size(d);
    fi;
  od;

  return tot;
end);

#PPP

# new for 0.5! - PosetOfIdempotents - "for a transformation semigroup"
#############################################################################

InstallMethod(PosetOfIdempotents, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], ReturnFail);

# new for 0.7! - PrimitiveIdempotents - "for an inverse semigroup"
#############################################################################

InstallMethod(PrimitiveIdempotents, "for an inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  local zero, rank;
  zero:=MultiplicativeZero(s);
  if zero=fail then 
    rank:=zero[2];
  else
    rank:=Set(List(OrbSCC(LongOrb(s)), x-> Length(LongOrb(s)[x[1]])))[2];
  fi;

  return Idempotents(s, rank);
end);

# new for 0.7! - PrincipalFactor - "for a D-class"
#############################################################################

InstallMethod(PrincipalFactor, "for a D-class", 
[IsGreensDClass], 
d-> Range(InjectionPrincipalFactor(d)));

#SSS

# fix for 0.5! - SmallGeneratingSet - "for a trans. semi."
#############################################################################

InstallOtherMethod(SmallGeneratingSet, "for a trans. semi.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
s -> Generators(Semigroup(Generators(s), rec(small:=true, schreier:=false))));

# new for 0.7! - SmallGeneratingSet - "for an inverse semi"
#############################################################################

InstallOtherMethod(SmallGeneratingSet, "for a trans. semi.", 
[IsPartialPermSemigroup and IsInverseSemigroup],
s -> Generators(InverseSemigroup(Generators(s), rec(small:=true, 
 schreier:=false))));

# new for 0.2! - StructureDescription - "for a Brandt trans. semigroup"
############################################################################

InstallOtherMethod(StructureDescription, "for a Brandt trans. semigroup",
[IsTransformationSemigroup and IsBrandtSemigroup],
function(s)
  local iter, d;
  
  iter:=IteratorOfDClasses(s);
  repeat 
    d:=NextIterator(iter);
  until Size(d)>1;
  
  return Concatenation("B(", StructureDescription(GroupHClass(d)), ", ",
  String(NrRClasses(d)), ")");
end);

# new for 0.7! - StructureDescription - "for a trans. semi. as group"
############################################################################

InstallOtherMethod(StructureDescription, "for a group as semigroup",
[IsTransformationSemigroup and IsGroupAsSemigroup],
s-> StructureDescription(Range(IsomorphismPermGroup(s))));

# new for 0.7! - StructureDescription - "for a part. perm. semi. as group""
############################################################################

InstallOtherMethod(StructureDescription, "for a group as semigroup",
[IsPartialPermSemigroup and IsGroupAsSemigroup],
s-> StructureDescription(Range(IsomorphismPermGroup(s))));

# new for 0.7! - ViewObj - "for a zero group"
############################################################################

InstallMethod(PrintObj, "for a zero group",
[IsZeroGroup],
function(g)
  Print("<zero group");
  if HasSize(g) then 
    Print(" of size ", Size(g));
  fi;
  Print(" with ", Length(GeneratorsOfMonoid(g)), " generators>");
  return;
end);

#EOF
