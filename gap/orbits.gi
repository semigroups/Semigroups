#############################################################################
##
#W  orbits.gi                          monoid 
#Y  Copyright (C) 2006-2010            James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id: orbits_orb.gi 21 2010-05-04 17:46:08Z jamesm $
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

InstallOtherMethod(MonoidOrbits, "for a list of integers and OnPoints", [IsTransformationCollection, IsCyclotomicCollection], 
function(M, list)
return MonoidOrbits(M, list, OnPoints);
end);

###########################################################################

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



