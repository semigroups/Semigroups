#############################################################################
##
#W  properties.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

# Ecom (commuting idempotents), LI (locally trivial), 
# LG (locally group), B1 (dot-depth one), DA (regular D-classes are idempotent)
# R v L (???), IsNilpotentSemigroup, inverses, local submonoid, right ideal, 
# left ideal, kernel!?

# IsMonogenicSemigroup, IsRightSimple, IsLeftSimple, IsLeftCancellative, 
# IsRightCancellative, IsRightGroup, IsLeftGroup, IsUnitarySemigroup, 
# IsRightUnitarySemigp, IsLeftUnitarySemigp, IsCongruenceFree,
# PrimitiveIdempotents, IdempotentOrder, IsZeroRectangularBand
# IsLeftNormalBand, IsRightNormalBand, IsNormalBand, IsEUnitarySemigroup
# IsRectangularGroup, IsBandOfGroups, IsFreeBand, IsFreeSemilattice,
# IsFreeNormalBand, IsBrandtSemigroup, IsFundamentalInverseSemigp, 
# IsFullSubsemigroup (of an inverse semigroup), IsFactorizableInverseMonoid,
# IsFInverseSemigroup, 



AllPropsOfSemigps:=["IsBand", "IsBlockGroup", "IsCliffordSemigroup", 
"IsCommutativeSemigroup", "IsCompletelyRegularSemigroup", "IsGreensLTrivial", 
"IsGreensRTrivial",
"IsGreensHTrivial", "IsGroupAsSemigroup", "IsInverseSemigroup", 
"IsLeftZeroSemigroup", "IsMonoidAsSemigroup", "IsOrthodoxSemigroup",
"IsRectangularBand", "IsRegularSemigroup", "IsRightZeroSemigroup",
"IsIdempotentGenerated", "IsSemilatticeAsSemigroup", "IsSimpleSemigroup",
"IsZeroSemigroup", "IsZeroGroup"];

###########################################################################

EasyPropsSemigps:=[IsBand, IsCliffordSemigroup, 
IsCommutativeSemigroup, IsCompletelyRegularSemigroup, 
IsCompletelySimpleSemigroup, IsGroupAsSemigroup,  
IsLeftZeroSemigroup, IsMonoidAsSemigroup, IsRightZeroSemigroup,
IsIdempotentGenerated, IsSemilatticeAsSemigroup, IsSimpleSemigroup,
IsZeroSemigroup, IsZeroGroup];

foo:=function(s)
local d, dd;

d:=List(GreensDClasses(s), Size);
dd:=Filtered(d, x-> not x= Maximum(d));

if not ForAll(dd, x-> x=1) then 
	return false;
fi;

return NrGreensRClasses(GreensDClasses(s)[Position(d, Maximum(d))])=
NrGreensLClasses(GreensDClasses(s)[Position(d, Maximum(d))]);
end;

# new method for 4.0! 
###########################################################################
# JDM would be better if this could return a group of transformations...
# test efficiency...

InstallMethod(GroupOfUnits, "for a tranformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local h, m, g;

if not IsMonoidAsSemigroup(s) then 
	return fail;
fi;

h:=GreensHClassOfElement(s, One(s));
m:=Size(h);
g:=Group(());

repeat 
	g:=ClosureGroup(g, AsPermutation(Random(h)));
until Size(g)=m;

return g;
end);

###########################################################################
#

# new method for 4.0! 
###########################################################################
# - must find some reasonable examples to test this on.

InstallMethod(IsBand, "for a transformation semigroup", 
[IsTransformationSemigroup], s-> IsCompletelyRegularSemigroup(s) and 
 IsGreensHTrivial(s));

# JDM new for 4.0!
#############################################################################

InstallMethod(IsBlockGroup, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(s)
  local iter, f, o, scc, reg, n, k, l, d, i, j;

if IsInverseSemigroup(s) then 
   return true;
elif IsRegularSemigroup(s) then 
   return false;
fi;

iter:=IteratorOfRClassRepsData(s);

for d in iter do
	f:=RClassRepFromData(s, d)![1];
  o:=ImageOrbitFromData(s, d);
  scc:=ImageOrbitFromData(s, d);
  reg:=false;
  n:=Length(o[1]);
  
	for i in scc do 
		k:=[]; l:=0;
		for j in o[i] do 
			if not f[j] in k then 
				AddSet(k, f[j]);
				l:=l+1;
			else
				break;
			fi;
		od;
		
    if l=n and reg then
    	return false;
   	elif l=n then 
    	reg:=true;
    fi;
	od;
od;

return true;
end);

###########################################################################

InstallMethod(IsCliffordSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local gens, idem, f, g;

if HasIsInverseSemigroup(s) and not IsInverseSemigroup(s) then 
	return false;
elif not IsCompletelyRegularSemigroup(s) then 
  return false;
elif IsGroupAsSemigroup(s) then
  return true;
fi;

if not IsRegularSemigroup(s) then 
	return false;
fi;

gens:=Generators(s);

idem:=List(gens, x->IdempotentNC(KernelOfTransformation(x), 
 ImageSetOfTransformation(x)));

for f in gens do
	for g in idem do
		if not f*g=g*f then 
			return false;
		fi;
	od;
od;

SetIsInverseSemigroup(s, true);
SetIsBlockGroup(s, true);

return true;
end);

###########################################################################

InstallMethod(IsCommutativeSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
local gens, n, i, j; 

gens:=Generators(s);
n:=Length(gens);

for i in [1..n] do
  for j in [i+1..n] do
    if not gens[i]*gens[j]=gens[j]*gens[i] then 
      return false;
    fi;
  od;
od;

return true;
end);

###########################################################################
#JDM redo the following!
# use Orb and looking for here!

InstallMethod(IsCompletelyRegularSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local foo, f, g, o, gens;

if HasIsRegularSemigroup(s) and not IsRegularSemigroup(s) then 
	return false;
fi;

foo:=function(f, set) #is f injective on set?
local i, j;
j:=[]; 
for i in set do 
	if not f[i] in j then 
		AddSet(j, f[i]);
	else
		return false;
	fi;
od;

return true;
end;

gens:=Generators(s);

for f in gens do
	g:=f![1];
	o:=Orb(gens, ImageSetOfTransformation(f), OnSets, 
	 rec(lookingfor:=function(o, x) return not foo(g, x); end));
	Enumerate(o);
	if IsPosInt(PositionOfFound(o)) then 
		return false;
	fi;
od;

return true;
end);

###########################################################################
# this test required to avoid conflict with Smallsemi

InstallMethod( IsCompletelySimpleSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
return IsSimpleSemigroup(s) and IsFinite(s);
end);

#############################################################################
#JDM new for 4.0!

InstallMethod(IsGreensLTrivial, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
local iter, d;

iter:=IteratorOfGreensDClasses(s); #JDM would be quicker to have 
#IteratorOfGreensDClassesData(s)

#JDM here it would be useful to pass OrbitsOfKernels(s)!.orbits to 
# LClassSchutzGpFromData...

for d in iter do 
	if not (Size(LClassSchutzGpFromData(s, d!.data[2]))=1 and 
	 Length(LClassSCCFromData(s, d!.data[2]))=1) then
		return false;
	fi;
od;

return true;
end);

#############################################################################
#JDM new for 4.0!

InstallMethod(IsGreensRTrivial, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
local iter, r, d;

if OrbitsOfKernels(s)!.finished then 
	iter:=IteratorOfGreensDClasses(s);
	for d in iter do 
		if not (Size(ImageOrbitSchutzGpFromData(s, d!.data[1]))=1 and 
		 Length(ImageOrbitFromData(s, d!.data[1]))=1) then
			return false;
		fi;
	od;
	
	return true;
fi;

iter:=IteratorOfRClassRepsData(s); 

#JDM here it would be useful to pass OrbitsOfImages(s)!.orbits to 
# RClassSchutzGpFromData...

for d in iter do 
	if not (Size(ImageOrbitSchutzGpFromData(s, d))=1 and 
	 Length(ImageOrbitFromData(s, d))=1) then 
		return false;
	fi;
od;

return true;
end);

###########################################################################

InstallOtherMethod(IsGreensHTrivial, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local iter, g;

iter:=IteratorOfGreensDClasses(s);
#JDM again it would be good to do this for iter of data rather than
# D-classes
repeat 
	g:=SchutzenbergerGroup(NextIterator(iter));
	if Size(g)>1 then 
		return false;
	fi;
until IsDoneIterator(iter);
return true;
end);

###########################################################################
 
InstallMethod(IsGroupAsSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local n, foo, gens, ker1, img1, f, img, ker;

n:=Degree(s);
gens:=Generators(s);

if ForAll(gens, f-> Rank(f)=n) then
	return true;
fi;

foo:=function(f, set) #is f injective on set?
local i, lookup;
lookup:=EmptyPlist(Length(f));

for i in set do 
	if not IsBound(lookup[f[i]]) then 
		lookup[f[i]]:=0;
	else
		return false;
	fi;
od;
return true;
end;

ker1:=KernelOfTransformation(gens[1]);
img1:=ImageSetOfTransformation(gens[1]);

for f in gens do 
	img:=ImageSetOfTransformation(f);
	ker:=KernelOfTransformation(f);
	if not (foo(f![1], img) and img=img1 and ker=ker1) then 
		return false;
	fi;
od;

return true;
end);

###########################################################################

InstallMethod(IsSemigroupWithCommutingIdempotents, "for a trans. semigp.", 
[IsTransformationSemigroup],
function(s)
local iter, idem , f;
iter:=IteratorOfIdempotents(s);
idem:=[];

for f in iter do 
	if not ForAll(idem, g-> f*g=g*f) then 
		return false;
	fi;
od;

return true;
end);

###########################################################################
# JDM the following should be tested! 

InstallOtherMethod(IsInverseSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local n, imgs, kers, foo, iter, d, f, o, scc, reg, i;

if HasIsRegularSemigroup(s) and not IsRegularSemigroup(s) then 
   return false;
elif IsCompletelyRegularSemigroup(s) then
   return IsCliffordSemigroup(s);
fi;

n:=Degree(s);
imgs:=ImagesOfTransSemigroup(s);
Enumerate(imgs, 2^n);
kers:=KernelsOfTransSemigroup(s);
Enumerate(kers, Length(imgs));

if not (IsClosed(kers) and Length(kers)=Length(imgs)) then 
	return false;
fi;

foo:=function(f, set) #is f injective on set?
local i, j;
j:=[]; 
for i in set do 
	if not f[i] in j then 
		AddSet(j, f[i]);
	else
		return false;
	fi;
od;
return true;
end;

if OrbitsOfKernels(s)!.finished then 
	#JDM use IteratorOfDClassReps here when implemented
	iter:=IteratorOfGreensDClasses(s);
	for d in iter do 
		f:=RClassRepFromData(s, d!.data[1]);
		o:=ImageOrbit(d);
		scc:=ImageOrbitFromData(s, d!.data[1]);
		reg:=false;
		for i in scc do 
			if foo(f, o[i]) then 
				if reg then 
					return false;
				fi;
				reg:=true;
			fi;
		od;
		
		if not reg then 
			return false;
		fi;
	od;
fi;

iter:=IteratorOfRClassRepsData(s);

for d in iter do 
	f:=RClassRepFromData(s, d)![1];
	#JDM again it would be good to pass OrbitsOfImages!.orbits to RClasRepFromData
	o:=ImageOrbitFromData(s, d);
	scc:=ImageOrbitFromData(s, d);
	reg:=false;
	
	for i in scc do 
		if foo(f, o[i]) then 
			if reg then 
				return false;
			fi;
			reg:=true;
		fi;
	od;
	
	if not reg then 
		return false;
	fi;
od;

return true;
end);

#############################################################################
#JDM new for 4.0!

InstallMethod(IsIrredundantGeneratingSet, 
"for a collection of transformations",
[IsTransformationCollection],
function(gens)
return not ForAny(gens, x-> x in Semigroup(Difference(gens, [x])));
end);

#############################################################################
#JDM new for 4.0!

InstallOtherMethod(IsIrredundantGeneratingSet, 
"for a transformation semigroup and collection of transformations",
[IsTransformationSemigroup, IsTransformationCollection],
function(S, gens)

if S=Semigroup(gens) then 
	return IsIrredundantGeneratingSet(gens);
fi;
end);

###########################################################################

InstallMethod(IsLeftZeroSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local gens, imgs;

gens:=Generators(s);
imgs:=Set(List(gens, ImageSetOfTransformation));

if Size(imgs)=1 and ForAll(gens, IsIdempotent) then
   return true;
fi;
return false;
end);

#############################################################################

InstallOtherMethod(IsMonoidAsSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup], x-> not MultiplicativeNeutralElement(x)=fail);

#############################################################################
#JDM

#InstallMethod(IsomorphismTransformationMonoid, "for a transformation semigroup",
#[IsTransformationSemigroup and IsMonoidAsSemigroup],
#function(s)
#Error("not yet implemented");
#end);

###########################################################################
##  JDM is there a better way? 

InstallMethod(IsOrthodoxSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local idems, e, f;

if not IsRegularSemigroup(s) then 
	return false;
fi;

idems:=Idempotents(s);

for e in idems do
   for f in idems do
      if not (e*f)^2=e*f then 
         return false;
      fi;
   od;
od;

return true;
end);

###########################################################################
# check efficiency JDM

InstallMethod(IsRectangularBand, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local x, y, z, gens;

if not IsSimpleSemigroup(s) then 
   return false;
elif HasIsBand(s) then
   return IsBand(s);
fi;

SetIsBand(s, true);

return IsGreensHTrivial(s);
end);
#gens:=GeneratorsOfSemigroup(M);

#for x in gens do
#	for y in gens do
#		 for z in gens do
#				if not x*y*z=x*z then 
#					 return false;
#				fi;
#		 od;
#	od;
#od;
#SetIsBand(M, true)
#return true;
#fi; 

# new method for 4.0! 
###########################################################################
# JDM check efficiency!

InstallOtherMethod(IsRegularSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local iter, d;

if IsSimpleSemigroup(s) then 
	return true;
elif IsCompletelyRegularSemigroup(s) then 
	return true;
elif HasGreensDClasses(s) then 
	return ForAll(GreensDClasses(s), IsRegularDClass);
fi;

iter:=IteratorOfRClassRepsData(s);

for d in iter do 
	if not IsRegularRClassData(s, d) then 
		return false;
	fi;
od; 

return true;
end);

###########################################################################

InstallMethod(IsRightZeroSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local gens, kers;

gens:=Generators(s);
kers:=Set(List(gens, KernelOfTransformation));

if Length(kers)=1 and ForAll(gens, IsIdempotent) then
   return true;
else
   return false;
fi;

end);

###########################################################################
##  JDM is there a better way?
# should store t as IdempotentsGeneratedSubsemigroup...

InstallMethod(IsIdempotentGenerated, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s) 
local gens, r, i, t;
gens:=Generators(s);
r:=List(gens, Rank); 

i:=Concatenation(List([Maximum(r),Maximum(r)-1..Minimum(r)], i-> 
 Idempotents(s, i)));
t:=Semigroup(i);

return ForAll(gens, f-> f in t);
end);

###############################################################################

InstallMethod(IsSemilatticeAsSemigroup, [IsSemigroup],
function(s)
return IsBand(s) and IsCommutativeSemigroup(s);
end);

###########################################################################

InstallMethod( IsSimpleSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local foo, gens, f, g, r, o;

if HasIsRegularSemigroup(s) and not IsRegularSemigroup(s) then 
	return false;
elif HasIsCompletelyRegularSemigroup(s) and not IsCompletelyRegularSemigroup(s) 
 then 
	return false;
elif HasNrGreensDClasses(s) and NrGreensDClasses(s)=1 then 
	return true;
fi;

foo:=function(f, set) #is f injective on set?
local i, j;
j:=[]; 
for i in set do 
	if not f[i] in j then 
		AddSet(j, f[i]);
	else
		return false;
	fi;
od;

return true;
end;

gens:=GeneratorsOfSemigroup(s);

for f in gens do
	g:=f![1];
	r:=Rank(f);
	o:=Orb(gens, ImageSetOfTransformation(f), OnSets, 
	 rec(lookingfor:=function(o, x) return Length(x)<r or not foo(g, x); end));
	Enumerate(o);
	if IsPosInt(PositionOfFound(o)) then 
		return false;
	fi;
od;

SetIsCompletelyRegularSemigroup(s, true);
SetIsRegularSemigroup(s, true);
SetNrGreensDClasses(s, 1);

return true;
end);

###########################################################################

InstallOtherMethod(IsZeroSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local z, x, y, gens;

z:=MultiplicativeZero(s);
gens:=GeneratorsOfSemigroup(s);

if z=fail then
	return false;
fi;

for x in gens do
	for y in gens do 
		if not x*y=z then 
			return false;
		fi;
	od;
od;

return true;
end);

###########################################################################
#JDM new for 3.1.4!
#used to accept IsSemigroup as filter, changed for semex

InstallOtherMethod(IsZeroGroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
local zero, one;

zero:=MultiplicativeZero(s);

if zero=fail then 
	return false;
fi;

#one:=MultiplicativeNeutralElement(s);

if NrGreensHClasses(s)=2 then 
	return ForAll(GreensHClasses(s), IsGroupHClass);
fi;

return false;
end);

###########################################################################

InstallMethod(MinimalIdeal, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local n, gens, max, o, i, bound, f;

n:=Degree(s);
gens:=Generators(s);
max:=Maximum(List(gens, Degree));

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
i:=SemigroupIdealByGenerators(s, [f]);
SetIsSimpleSemigroup(i, true);
SetIsMinimalIdeal(i, true);
SetUnderlyingDClassOfMinIdeal(i, GreensDClassOfElement(s, f));

return i;
end);

###########################################################################
# JDM what method for the below? 

InstallOtherMethod(MultiplicativeNeutralElement, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local gens, n, f, r;

gens:=Generators(s);
n:=Maximum(List(gens, Rank));

if n=Degree(s) then
	return One(s);
fi;

f:=First(gens, f-> Rank(f)=n);

r:=GreensRClassOfElementNC(s, f); #NC? JDM

if NrIdempotents(r)>1 or NrIdempotents(r)=0 then 
	return fail;
fi;

f:=Idempotents(r)[1];

if ForAll(gens, x-> x*f=x and f*x=x) then 
	return f;
fi;

return fail;
end);

###########################################################################
# JDM test it more!

InstallOtherMethod(MultiplicativeZero, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local n, gens, max, bound, o, i, f;
n:=Degree(s); gens:=Generators(s);

max:=Maximum(List(gens, Degree));
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

if f^2=f and Size(GreensDClassOfElement(s, f))=1 then #JDM NC? D-class?
	return f;
fi;

return fail;
end);

# new for 4.0!
#############################################################################
#

InstallMethod(IrredundantGeneratingSubset, "for a trans. coll.", 
[IsTransformationCollection], 
function(coll)
return IrredundantGeneratingSubset(coll, false);
end);

#############################################################################
#

InstallOtherMethod(IrredundantGeneratingSubset, "for a trans. coll. and bool", 
[IsTransformationCollection, IsBool], 
function(coll, bool)
local n, a, g, s, i, m, j, max, info, k;

n:=DegreeOfTransformation(coll[1]);

Info(InfoMonoidProperties, 3, "checking degrees of transformations in", 
 " collection...");
if not ForAll(coll, f-> Degree(f)=n) then 
	Error("Usage: collection of transformations of equal degree");
fi;

Info(InfoMonoidProperties, 3, "sorting transformations by rank...");
a:=ShallowCopy(coll);
Sort(a, function(f,g) return Rank(f)>Rank(g) and f![1]>g![1]; end);

if Rank(a[1])=n then 
	Info(InfoMonoidProperties, 3, "finding small generating set for unit", 
	" group...");
	g:=Group(List(Filtered(a, f-> Rank(f)=n), AsPermutation));
	s:=Semigroup(List(SmallGeneratingSet(g), f-> AsTransformation(f, n)));
else
	s:=Semigroup(a[1]);
fi;

i:=0;
m:=Length(a);
j:=Length(Generators(s));

Info(InfoMonoidProperties, 3, "looping over elements...");

info:=false;

if InfoLevel(InfoMonoidProperties)>=3 then 
	info:=true;
fi;

k:=Length(coll);
while  i<k do 
	i:=i+1;
	
	if info then Print("at ", i, " of ", m, "; ", j, " generators\r"); fi;
	
	if not a[i] in s then 
		j:=j+1;
		s:=ClosureSemigroupNC(s, [a[i]]);
		
		if j=k-1 then
			if bool then  
				return Semigroup(a);
			fi;
			return a;
		fi;
	fi;
od;

if info then 
	Print("\n");
fi;

if bool then 
	return s;
fi;

return Generators(s);
end);

#############################################################################
#

InstallOtherMethod(IsomorphismPermGroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)

if not IsGroupAsSemigroup(s)  then
   Error( "Usage: trans. semigroup satisfying IsGroupAsSemigroup" );
fi;

return MappingByFunction(s, Group(List(Generators(s), AsPermutation)), 
 AsPermutation);
end);


#############################################################################
#

InstallMethod(PropertiesOfSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local T, F, str, f;

T:=[]; F:=[];

for str in AllPropsOfSemigps do 
	Info(InfoMonoidProperties, 3, "testing... ", str);
	f:=EvalString(str);
	if f(s) then 
		Add(T, str);
	else
		Add(F, str);
	fi;
od;

Print("satisfies: ", T, "\n");
Print("does not satisfy: ", F, "\n");

if not MultiplicativeZero(s)=fail then 
	Print("with zero\n");
else
	Print("no zero\n");
fi;

return true;
end);

#############################################################################

InstallMethod(EasyPropertiesOfSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local T, F, str, f;

T:=[]; F:=[];

for str in AllPropsOfSemigps do 
	if not str="IsIdempotentGenerated" then 
		Info(InfoMonoidProperties, 3, "testing... ", str);
		f:=EvalString(str);
		if f(s) then 
			Add(T, str);
		else
			Add(F, str);
		fi;
	fi;
od;

Print("satisfies: ", T, "\n");
Print("does not satisfy: ", F, "\n");

if not MultiplicativeZero(s)=fail then 
	Print("with zero\n");
else
	Print("no zero\n");
fi;

return true;
end);

#############################################################################

InstallMethod(PrintObj, "for an ideal of a trans. semigp.",
[IsSemigroupIdeal and IsTransformationSemigroup], 
function(i)
Print("<semigroup ideal with ", Length( GeneratorsOfMagmaIdeal( i ) ), 
 " generators>");
return;
end);

#############################################################################

InstallMethod(ViewObj, "for an ideal of a trans. semigp.",
[IsSemigroupIdeal and IsTransformationSemigroup], 
function(i)
Print("<semigroup ideal with ", Length( GeneratorsOfMagmaIdeal( i ) ), 
 " generators>");
return;
end);

#############################################################################
#JDM there must be better methods than the following for special types of S.
#JDM new for 4.0! JDM is there a better way? This doesn't work all that well.

InstallOtherMethod(SmallGeneratingSet, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
local n, gens, j, min, g, t, ss, iter, i, r, f;

n:=Degree(s);

if Length(Generators(s))=2 and not IsCommutativeSemigroup(s) then 
	return Generators(s);
fi;

Info(InfoMonoidProperties, 3, "finding irredundant generating subset...");

ss:=IrredundantGeneratingSubset(Generators(s), true);

gens:=Generators(ss);
j:=Length(gens);

min:=Rank(gens[j]);

if Rank(gens[1])=n then 
	t:=Semigroup(Filtered(gens, x-> Rank(x)=n));
else
	t:=Semigroup(gens[1]);
fi;

iter:=IteratorOfGreensRClasses(ss);
i:=0;

for r in iter do 
	f:=r!.rep;
	if min<=Rank(f) and Rank(f)<n then 
		for f in Iterator(r) do 
			if not f in t then 
				i:=i+1;
				t:=ClosureSemigroupNC(t, [f]);
				if ForAll(gens, x-> x in t) then 
					return Generators(t);
				fi;
				
				if i=j-1 then 
					return Generators(ss);
				fi;
				
			fi;
		od;
	fi;
od;

return Generators(ss);
end);

#####################
