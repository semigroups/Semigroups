#############################################################################
##
#W  orbits.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

## this file contains the functions for forward orbits that do not depend
## on whether orb is loaded or not.

###########################################################################

InstallOtherMethod(GradedOrbit, 
"for a trans. collection, (integer, set of integers, or set of sets of integers), and grading", 
[IsTransformationCollection, IsObject, IsFunction],  
function(s, seed, grading)

if IsPosInt(seed) then
   return GradedOrbit(s, seed, OnPoints, grading);
elif IsCyclotomicCollection(seed) then
   return GradedOrbit(s, seed, OnSets, grading);
elif IsCyclotomicCollColl(seed) then
   return GradedOrbit(s, seed, OnSetsSets, grading);
fi;

return fail;
end);

###########################################################################

InstallOtherMethod(MonoidOrbit, 
"for an integer, set of integers, or set of sets of integers",
 [IsTransformationCollection, IsObject],
function(M, pt)

if IsPosInt(pt) then
   return MonoidOrbit(M, pt, OnPoints);
elif IsCyclotomicCollection(pt) then
   return MonoidOrbit(M, pt, OnSets);
elif IsCyclotomicCollColl(pt) then 
   return MonoidOrbit(M, pt, OnSetsSets);
fi;

return fail;
end);

###########################################################################
#JDM this could probably be improved to avoid duplicate computations

InstallMethod(MonoidOrbits, "for a trans. collection, a list, and action", 
[IsTransformationCollection, IsList, IsFunction],
function(M, objt, action)
return List(objt, x-> MonoidOrbit(M, x, action));
end);

###########################################################################

InstallOtherMethod(MonoidOrbits, "for a list of integers and OnPoints", 
[IsTransformationCollection, IsCyclotomicCollection], 
function(M, list)
return MonoidOrbits(M, list, OnPoints);
end);

###########################################################################
# JDM require C version! and should subsequently be moved to orbits_no_orb.gi

InstallGlobalFunction(OnKernelsAntiAction, [IsList, IsTransformation],
function(ker, s)
local n, pos, new, loc, i, img;

n:= DegreeOfTransformation(s);  
pos:= []; new:= []; loc:= [];
img:=s![1];

# construct transformation 'pos' with kernel 'ker'.
for i in [1..Length(ker)] do
	pos{ker[i]}:= List(ker[i], x-> i);
od;

# apply 's' from the left.
pos:= pos{img};

# determine kernel.
for i in [1..n] do 
	if IsBound(loc[pos[i]]) then
		Add(new[loc[pos[i]]], i);
	else
		Add(new, [i]);
		loc[pos[i]]:= Length(new);
	fi;
od;

# return the kernel.
return new;
end) ;

###########################################################################
# JDM require C version! and should subsequently be moved to orbits_no_orb.gi
 
InstallGlobalFunction(OnTuplesOfSetsAntiAction, [IsObject, IsTransformation], 
function(tup, s)
local res, ker, set, perm, k;

ker:=KernelOfTransformation(s);
res:=[];

for set in tup do
	Unbind(perm);
	for k in ker do
		if k[1]^s in set then
			if IsBound(perm) then
				perm:=Union(perm, k);
			else 
				perm:=ShallowCopy(k);
			fi;
		fi;
	od;
	if IsBound(perm) then  
		Add(res, perm);
	fi;
od;

return res;
end);

###########################################################################
# JDM should be rewritten! and moved into orbits_no_orb and orbits_orb

InstallMethod(ImagesOfTransSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(M)
local gens, orb, imgs, x, y, new, limit, n;
 
if HasGradedImagesOfTransSemigroup(M) then 
	return Union(GradedImagesOfTransSemigroup(M));
elif HasAsSSortedList(M) then #JDM new for 3.1.4
	return Set(List(Elements(M), x-> AsSet(x![1])));
else

	n:=DegreeOfTransformationSemigroup(M);
	if IsTransformationMonoid(M) then 
		gens:=GeneratorsOfMonoid(M);
	else
		gens:=GeneratorsOfSemigroup(M); 
	fi;

	imgs:=SetX(GeneratorsOfSemigroup(M), ImageSetOfTransformation);

	if HasParentAttr(M) and Length(GeneratorsOfSemigroup(ParentAttr(M)))<Length(gens) then 
		limit:=Length(ImagesOfTransSemigroup(ParentAttr(M)));
	else 
		limit:=Sum([1..Maximum(List(gens, DegreeOfTransformation))], x-> 
		Binomial(n, x));
	fi;

	if Length(imgs)=limit then 
		return imgs;
	fi;

	orb:=List(GeneratorsOfSemigroup(M), ImageSetOfTransformation);

	for x in orb do
		for y in gens do 
			new:=OnSets(x,y);
			if not new in imgs then 
				AddSet(imgs, new);
				if Length(imgs)=limit then 
					return imgs;
				fi;
				Add(orb, new);
			fi;
		od;
	od;
	return imgs;
fi;
end );

###########################################################################
# JDM should be rewritten! and moved into orbits_no_orb and orbits_orb

InstallOtherMethod(ImagesOfTransSemigroup, "for  a trans. semigroup and a pos. int.", 
[IsTransformationSemigroup, IsPosInt],
function(S, m)
local n, gens, imgs, limit, orb, i, x, j, y, new, setorb;

#if HasGradedImagesOfTransSemigroup(M) then 
#	return Set(Concatenation(GradedImagesOfTransSemigroup(M)));
#else

n:=DegreeOfTransformationSemigroup(S);

if m>n then 
	return fail;
fi;

if IsTransformationMonoid(S) then 
	gens:=GeneratorsOfMonoid(S);
else
	gens:=GeneratorsOfSemigroup(S); 
fi;

imgs:=List(GeneratorsOfSemigroup(S), x-> AsSet(x![1]));
imgs:=Set(Filtered(imgs, x->Length(x)=m));

limit:=Binomial(n, m);

if not Length(imgs)=limit and ForAny(gens, x-> Length(AsSet(x![1]))>= m) then 
	orb:=List(GeneratorsOfSemigroup(S), x-> AsSet(x![1]));
	setorb:=Set(orb);
	i:=0;
	
	repeat
		i:=i+1;
		x:=orb[i];
		j:=0;
		repeat
			j:=j+1;
			y:=gens[j];
			new:=OnSets(x,y);
			if Length(new)>=m and not new in setorb then 
				AddSet(setorb, new);
				Add(orb, new);
				if Length(new)=m then 
					AddSet(imgs, new);
				fi;
			fi;
		until Length(imgs)=limit or j=Length(gens);
	until Length(imgs)=limit or i=Length(orb);
fi;

return imgs;
end );

###########################################################################
# JDM should be rewritten! and moved into orbits_no_orb and orbits_orb

InstallOtherMethod(KernelsOfTransSemigroup, "for a trans. semigroup", 
[IsTransformationSemigroup, IsPosInt],  
function(S, m)
local n, gens, imgs, limit, orb, i, x, j, y, new, setorb;

#if HasGradedKernelsOfTransSemigroup(M) then 
#	return Set(Concatenation(GradedKernelsOfTransSemigroup(M)));
#elif HasInternalKernels(M) then 
#	return Set(Concatenation(InternalKernels(M))); 
#else

n:=DegreeOfTransformationSemigroup(S);

if m>n then 
	return fail;
fi;

if IsTransformationMonoid(S) then 
	gens:=GeneratorsOfMonoid(S);
else
	gens:=GeneratorsOfSemigroup(S); 
fi;

imgs:=List(GeneratorsOfSemigroup(S), KernelOfTransformation);
imgs:=Set(Filtered(imgs, x->Length(x)=m));

limit:=Stirling2(n, m);

if not Length(imgs)=limit and ForAny(gens, x-> Length(KernelOfTransformation(x))>= m) then 
	orb:=List(GeneratorsOfSemigroup(S), KernelOfTransformation);
	setorb:=Set(orb);
	i:=0;
	
	repeat
		i:=i+1;
		x:=orb[i];
		j:=0;
		repeat
			j:=j+1;
			y:=gens[j];
			new:=OnKernelsAntiAction(x,y);
			if Length(new)>=m and not new in setorb then 
				AddSet(setorb, new);
				Add(orb, new);
				if Length(new)=m then 
					AddSet(imgs, new);
				fi;
			fi;
		until Length(imgs)=limit or j=Length(gens);
	until Length(imgs)=limit or i=Length(orb);
fi;

return imgs;
end );

###########################################################################
# JDM should be rewritten! and moved into orbits_no_orb and orbits_orb

InstallMethod(KernelsOfTransSemigroup, "for a transformation monoid", 
[IsTransformationSemigroup], 
function(M)
local gens, imgs, orb, x, y, new, ker, n, limit;

if HasGradedKernelsOfTransSemigroup(M) then 
	return Union(GradedKernelsOfTransSemigroup(M));
elif HasInternalKernels(M) then 
	return Union(InternalKernels(M)); 
else

	n:=DegreeOfTransformationSemigroup(M);
	ker:=List([1..n], x->[x]);
#KernelOfTransformation(Transformation([1..n]));

	if IsTransformationMonoid(M) then 
		gens:=GeneratorsOfMonoid(M);
	else
		gens:=GeneratorsOfSemigroup(M); 
	fi;

	imgs:=SetX(GeneratorsOfSemigroup(M), KernelOfTransformation);

	if HasParentAttr(M) and Length(GeneratorsOfSemigroup(ParentAttr(M)))<Length(gens) then 
		limit:=Length(KernelsOfTransSemigroup(ParentAttr(M)));
	else 
		limit:=Sum([1..Maximum(List(gens, RankOfTransformation))], x-> 
		Stirling2(n, x));
	fi;

	if Length(imgs)=limit then 
		return imgs;
	fi;

	orb:=SetX(GeneratorsOfSemigroup(M), KernelOfTransformation);

	for x in orb do
		for y in gens do 
			new:=OnKernelsAntiAction(x,y);
			if not new in imgs then 
				AddSet(imgs, new);
				if Length(imgs)=limit then 
					return imgs;
				fi;
				Add(orb, new);
			fi;
		od;
	od;
	return imgs;
fi;
end); 


###########################################################################
#

InstallOtherMethod(ShortOrbit, 
"for a trans. collection, (integer, set of integers, or set of sets of integers), and grading",  
[IsTransformationCollection, IsObject, IsFunction],
function(s, seed, grading)

if IsPosInt(seed) then 
   return ShortOrbit(s, seed, OnPoints, grading);
elif IsCyclotomicCollection(seed) then
   return ShortOrbit(s, seed, OnSets, grading);
elif IsCyclotomicCollColl(seed) then
   return ShortOrbit(s, seed, OnSetsSets, grading);
fi;

return fail;
end);

###########################################################################

InstallOtherMethod(StrongOrbit, 
"for a trans. collection, and (integer, set of integers, or set of sets of integers)",  
[IsTransformationCollection, IsObject],
function(s, seed)

if IsPosInt(seed) then 
   return StrongOrbit(s, seed, OnPoints);
elif IsCyclotomicCollection(seed) then
   return StrongOrbit(s, seed, OnSets);
elif IsCyclotomicCollColl(seed) then
   return StrongOrbit(s, seed, OnSetsSets);
fi;

return fail;
end);

###########################################################################

InstallMethod(StrongOrbits, 
"for a trans. collection, duplicate free list, and action", 
[IsTransformationCollection, IsList, IsFunction],
function(s, seed, action)
local orbits, x;

if not IsDuplicateFreeList(seed) then 
	Info(InfoWarning, 1, 
	 "Usage: trans. collection, duplicate free list, and action");
fi;

orbits:= [];

for x in seed do
	if not ForAny(orbits, y-> x in y) then  
	   Add(orbits, StrongOrbit(s, x, action));
	fi;
od;

# return list of orbits.
return orbits;
end);

###########################################################################

InstallOtherMethod(StrongOrbits, "for a trans. collection, set, and action", 
[IsTransformationCollection, IsSet, IsFunction],   
function(M, set, action)
local orbit, orbits;
 
orbits:= [];

while set <> [] do
   orbit:=StrongOrbit(M, set[1], action);
   Add(orbits, orbit); 
   SubtractSet(set, orbit);
od;

return orbits;
end);

###########################################################################

InstallOtherMethod(StrongOrbits, 
"for a trans. collection and list of pos. ints.",
[IsTransformationCollection, IsCyclotomicCollection],
function(s, seeds)
return StrongOrbits(s, seeds, OnPoints);
end);



