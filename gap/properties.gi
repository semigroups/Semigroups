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

AllPropertiesOfSemigroups:=[IsBand, IsBlockGroup, IsCliffordSemigroup, 
IsCommutativeSemigroup, IsCompletelyRegularSemigroup, 
IsCompletelySimpleSemigroup, IsGreensLTrivial, IsGreensRTrivial,
IsGreensHTrivial, IsGroupAsSemigroup, IsInverseSemigroup, 
IsLeftZeroSemigroup, IsMonoidAsSemigroup, IsOrthodoxSemigroup,
IsRectangularBand, IsRegularSemigroup, IsRightZeroSemigroup,
IsSemiBand, IsSemilatticeAsSemigroup, IsSimpleSemigroup,
IsZeroSemigroup, IsZeroGroup];


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
local iter, r, f, o, scc, reg, n, i, j, k, l,d;

if IsInverseSemigroup(s) then 
   return true;
elif IsRegularSemigroup(s) then 
   return false;
fi;

iter:=IteratorOfRClassRepsData(s);

for d in iter do
	f:=RClassRepFromData(s, d)![1];
  o:=RClassImageOrbitFromData(s, d);
  scc:=RClassSCCFromData(s, d);
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
elif HasIsRegularSemigroup(s) and not IsRegularSemigroup(s) then 
	return false;
elif not IsCompletelyRegularSemigroup(s) then 
  return false;
elif IsGroupAsSemigroup(s) then
  return true;
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
		if not (Size(RClassSchutzGpFromData(s, d!.data[1]))=1 and 
		 Length(RClassSCCFromData(s, d!.data[1]))=1) then
			return false;
		fi;
	od;
	
	return true;
fi;

iter:=IteratorOfRClassRepsData(s); 

#JDM here it would be useful to pass OrbitsOfImages(s)!.orbits to 
# RClassSchutzGpFromData...

for d in iter do 
	if not (Size(RClassSchutzGpFromData(s, d))=1 and 
	 Length(RClassSCCFromData(s, d))=1) then 
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
		o:=DClassImageOrbit(s, d);
		scc:=RClassSCCFromData(s, d!.data[1]);
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
	o:=RClassImageOrbitFromData(s, d);
	scc:=RClassSCCFromData(s, d);
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
[IsTransformationSemigroup], x-> One(x) in x);

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
   return IsBand(s) and IsSimpleSemigroup(s);
fi;

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
return IsBand(s) and IsCommutative(s);
end);

###########################################################################
##  JDM could include if IsCompletelyRegular and HasGreensDClasses etc
##  JDM but this is so fast it might not be worthwhile...

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
local z, x, y;

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
one:=MultiplicativeNeutralElement(s);

if not (zero=fail or one=fail) and NrGreensHClasses(s)=2 then 
	return IsGroupHClass(GreensHClassOfElement(S, one));
fi;

return false;
end);

###########################################################################

InstallOtherMethod(MultiplicativeZero, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(S)

n:=Degree(s); gens:=Generators(s);

max:=Maximum(List(gens, Degree));
if max=n then 
	bound:=2^n;
else
	bound:=Sum([1..max], x-> Binomial(n, x));
fi;

o:=Orb(gens, [1..n], OnSets, rec(storenumbers:=true, 
lookingfor:=function(o, x) Length(x)=1; end));

if IsPosInt(PositionOfFound(o)) then 


return fail;
end);

# new for 4.0!
#############################################################################

InstallOtherMethod(SmallGeneratingSet, "for a trans. coll. and pos. int", 
[IsTransformationCollection, IsPosInt], 
function(coll, bound)
local n, a, g, s, i;

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

Info(InfoMonoidProperties, 3, "looping over elements...");

while  i<Length(coll) and Size(s)<bound do 
	i:=i+1;
	if not a[i] in s then 
		s:=Semigroup(Concatenation(Generators(s), [a[i]]));
	fi;
od;

return s;
end);

# new for 4.0!
#############################################################################
#
# JDM this is broken on # Sierpinski graph S_2 when trying to run with 
# <gens> as an argument...

# should probably be renamed or return a small generating set!? JDM

InstallOtherMethod(SmallGeneratingSet, "for a trans. coll.", 
[IsTransformationCollection], 
function(coll)
local n, a, g, s, i, m, j, max, info;

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
j:=0;
max:=0;
Info(InfoMonoidProperties, 3, "looping over elements...");

info:=false;

if InfoLevel(InfoMonoidProperties)>=3 then 
	info:=true;
fi;

while  i<Length(coll) do 
	i:=i+1;
	
	if info then Print("at ", i, " of ", m, "; ", j, " generators\r"); fi;
	
	if not a[i] in s then 
		j:=j+1;
		s:=ClosureSemigroup(s, [a[i]]);
	fi;
od;

if info then 
	Print("\n");
fi;

return s;
end);

#############################################################################
#JDM there must be better methods than the following for special types of S.
#JDM new for 4.0! JDM is there a better way?

InstallOtherMethod(SmallGeneratingSet, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
local n, gens, min, g, t, iter, r, iter_r, f, info;

n:=Degree(s);
gens:=Generators(s);

Info(InfoMonoidProperties, 3, "sorting generators by rank...");
gens:=ShallowCopy(gens);
Sort(gens, function(f,g) return Rank(f)>Rank(g) and f![1]>g![1]; end);
min:=Rank(gens[Length(gens)]);

if Rank(gens[1])=n then 
	Info(InfoMonoidProperties, 3, "finding small generating set for unit", 
	" group...");
	g:=Group(List(Filtered(gens, f-> Rank(f)=n), AsPermutation));
	t:=Semigroup(List(SmallGeneratingSet(g), f-> AsTransformation(f, n)));
else
	t:=Semigroup(gens[1]); #JDM good idea?
fi;

iter:=IteratorOfRClassRepsData(s);

info:=false;

if InfoLevel(InfoMonoidProperties)>=3 then 
	info:=true;
fi;

iter:=IteratorOfGreensRClasses(s);

while not t=s and not Length(Generators(t))>=Length(gens) do 
	r:=NextIterator(iter); f:=r!.rep;
	if min<=Rank(f) and Rank(f)<n then 
		iter_r:=Iterator(r);
		while not IsDoneIterator(iter_r) and not t=s and not 
		 Length(Generators(t))>=Length(gens) do 
			f:=NextIterator(iter_r);
			t:=ClosureSemigroup(t, [f]);
			if info then Print(Length(Generators(t)), " generators\r"); fi;
		od;
	fi;
od;

if t=s then 
	return Generators(t);
fi;

return Generators(s);
end);

#############################################################################
# JDM this should be moved...

InstallOtherMethod(Size, "for a simple transformation semigroup",
[IsSimpleSemigroup and IsTransformationSemigroup],
function(M)
local gens, ims, kers, H;

gens:=GeneratorsOfSemigroup(M);

ims:=Size(Set(List(gens, ImageSetOfTransformation)));
kers:=Size(Set(List(gens, KernelOfTransformation)));
H:=GreensHClassOfElement(M, gens[1]);
#JDM this could be better if it used the schutz group of the R-class of 
#    any elt.

return Size(H)*ims*kers;
end);

#####################
#JDM why's this commented out? 
#InstallOtherMethod(IsMultiplicativeZero, "for a transformation semigroup", 
#true, [IsTransformationSemigroup, IsTransformation], 0,
#function(S, f)
#
#return f=MultiplicativeZero(S);
#end);