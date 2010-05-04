##
## orbits.gi
## Version 3.1.3
## Fri  7 Nov 2008 17:45:12 GMT
##

###########################################################################

InstallMethod(MonoidOrbit, "for an arbitrary object and action",
[IsTransformationSemigroup, IsObject, IsFunction],
function(M, pt, action)
local orbit, gens, x, y, new;
   	
orbit:= [pt];  
gens:= GeneratorsOfSemigroup(M);
	
for x in orbit do
  for y in gens do
    new:= action(x,y);
    if not new in orbit then
      Add(orbit, new);
    fi;
  od;
od;

return orbit;

end);

###########################################################################

InstallOtherMethod(MonoidOrbit, 
"for an integer, set of integers, or set of sets of integers",
 [IsTransformationSemigroup, IsObject],
function(M, pt)

if IsPosInt(pt) then
   return MonoidOrbit(M, pt, OnPoints);
elif IsCyclotomicCollection(pt) then
   return MonoidOrbit(M, pt, OnSets);
elif IsCyclotomicCollColl(pt) then 
   return MonoidOrbit(M, pt, OnSetsSets);
fi;

end);

###########################################################################
#JDM this could probably be improved to avoid duplicate computations

InstallMethod(MonoidOrbits, "for an arbitrary list and action", 
[IsTransformationSemigroup, IsList, IsFunction],
function(M, objt, action)
return List(objt, x-> MonoidOrbit(M, x, action));
end);

###########################################################################

InstallOtherMethod(MonoidOrbits, "for a list of integers and OnPoints", [IsTransformationSemigroup, IsCyclotomicCollection], 
function(M, list)
return MonoidOrbits(M, list, OnPoints);
end);

###########################################################################

InstallMethod(GradedOrbit, 
"for a trans. semigroup, an arbitary object, action, and grading",
[IsTransformationSemigroup, IsObject, IsFunction, IsFunction],
function(M, obj, action, grad)
local orbit, gens, gradorbit, x, y, z, new, g, n;
 
n:=grad(obj);
gens:= GeneratorsOfSemigroup(M);
gradorbit:=List([1..n], x->[]);
Add(gradorbit[n], obj);

for x in [n, n-1..1] do
  for y in gradorbit[x] do
    for z in gens do
      new:= action(y, z);
      g:= grad(new);
      if not new in gradorbit[g] then   
        Add(gradorbit[g], new);
      fi;
    od;
  od;
od;

return gradorbit;

end);

#####################

InstallOtherMethod(GradedOrbit, "for an integer, set of integers, or set of sets of integers", true, [IsTransformationSemigroup, IsObject, IsFunction], 0, 
function(M, obj, grad)

if IsPosInt(obj) then
   return GradedOrbit(M, obj, OnPoints, grad);
elif IsCyclotomicCollection(obj) then
   return GradedOrbit(M, obj, OnSets, grad);
elif IsCyclotomicCollColl(obj) then
   return GradedOrbit(M, obj, OnSetsSets, grad);
fi;

end);

###########################################################################

InstallMethod(ShortOrbit, "for an arbitary object, action, and grading", true, [IsTransformationSemigroup, IsObject, IsFunction, IsFunction], 0, 

function(M, obj, action, grad)
local orbit, n, gens, x, y, new;

orbit:= [obj];  n:= grad(obj);
gens:=GeneratorsOfSemigroup(M);

for x in orbit do
   for y in gens do
      new:= action(x,y);
      if n=grad(new)  and not new in orbit then
         Add(orbit, new);
      fi;
   od;
od;

return orbit;
end );

#####################

InstallOtherMethod(ShortOrbit, "for an integer, set of integers, or set of sets of integers", true, [IsTransformationSemigroup, IsObject, IsFunction], 0, 

function(M, obj, grad)

if IsPosInt(obj) then 
   return ShortOrbit(M, obj, OnPoints, grad);
elif IsCyclotomicCollection(obj) then
   return ShortOrbit(M, obj, OnSets, grad);
elif IsCyclotomicCollColl(obj) then
   return ShortOrbit(M, obj, OnSetsSets, grad);
fi;

end);

###########################################################################

InstallMethod(StrongOrbit, "for a trans. semigroup, object and action", 
[IsTransformationSemigroup, IsObject, IsFunction],
function(s, obj, action)
local orbit, i, j, graph, x, pos, new, gens;

orbit:=[obj];
i:=0;
graph:=[[]];

gens:=GeneratorsOfSemigroup(s);
#JDM include if IsMonoid blah blah..

for x in orbit do
	i:=i+1;
	for j in [1..Length(gens)] do
		new:= action(x, gens[j]);
		pos:=Position(orbit, new);
		if pos=fail then 
			 Add(orbit, new);
			 Add(graph, []);
			 pos:=Length(orbit);
		fi; 
			AddSet(graph[i], pos);
	od;
od;

return orbit{First(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(graph), x-> 1 in x)};
end );

#####################

InstallOtherMethod(StrongOrbit, "for a positive integer", true, [IsTransformationSemigroup, IsPosInt], 0,  
function(M, int)
local cone, gens, cones, x; 

cone:=MonoidOrbit(M, int);
gens:=GeneratorsOfSemigroup(M);

cones:=[];
cones[int]:=cone;

for x in [1..DegreeOfTransformationSemigroup(M)] do 
   if x in cone then 
      cones[x]:=MonoidOrbit(M, x); 
   else 
      cones[x]:=[];
   fi;
od;

#JDM cones:=List(cone, x-> MonoidOrbit(M,x));

#JDM better way?
return First(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(cones), x-> int in x);

end);

###########################################################################

InstallMethod(StrongOrbits, "for arbitrary object and action", true, [IsTransformationSemigroup, IsObject, IsFunction], 0,   

function(M, obj, action)

local orbits, x;
 
orbits:= [];

for x in obj do 
   Add(orbits, StrongOrbit(M, x, action));
od;
#JDM orbits:=List(obj, x-> StrongOrbit(M,x));

# return list of orbits.
return orbits;

end );


InstallOtherMethod(StrongOrbits, "for arbitrary set of sets and action", true, [IsTransformationSemigroup, IsSet, IsFunction], 0,   

function(M, set, action)

local orbit, orbits;
 
orbits:= [];

while set <> [] do
   orbit:=StrongOrbit(M, set[1], action);
   Add(orbits, orbit); 
   SubtractSet(set, orbit);
od;

# return list of orbits.
return orbits;

end );

#####################

InstallOtherMethod(StrongOrbits, "for a list of integers under OnPoints", true, [IsTransformationSemigroup, IsCyclotomicCollection], 0,  
function(M, ints)
local gens, cones, x;

if not ForAll(ints, x-> IsPosInt(x) and x<DegreeOfTransformationSemigroup(M)+1) then 
   return fail;
else
   gens:=GeneratorsOfSemigroup(M);
   cones:=[];

   for x in [1..DegreeOfTransformationSemigroup(M)] do 
      if x in ints then 
         cones[x]:=MonoidOrbit(M, x);
      else 
         cones[x]:=[];
      fi;
   od;

#JDM cones:=List(cone, x-> MonoidOrbit(M,x));

#JDM better way?
return Filtered(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(cones), x-> ForAny(ints, y-> y in x));

fi;

end);

###########################################################################

InstallMethod( GradedStrongOrbit, "for an arbitrary object, action, and grading", true, [IsTransformationSemigroup, IsObject, IsFunction, IsFunction], 0,  
function(M, obj, action, grad)

local orbit;

orbit:=StrongOrbit(M, obj, action);

#JDM better to do it online than to filter
return Filtered(List([1..grad(obj)], x-> Filtered(orbit, y-> grad(y)=x)), x-> not x=[]);
end );

#####################

InstallOtherMethod( GradedStrongOrbit, "for a positive integer, OnPoints, and arbitrary grading", true, [IsTransformationSemigroup, IsPosInt, IsFunction], 0,  
function(M, int, grad)
return GradedStrongOrbit(M, int, OnPoints, grad);
end);

###########################################################################
##  JDM is this valid?? NO!
##

InstallMethod(ShortStrongOrbit,  "for arbitrary object and action", true, [IsTransformationSemigroup, IsObject, IsFunction, IsFunction], 0,  

function(M, obj, action, grad)

return First(GradedStrongOrbit(M, obj, action, grad), x-> obj in x);

end);

InstallOtherMethod(ShortStrongOrbit, "for a positive integer", true, [IsTransformationSemigroup, IsPosInt, IsFunction], 0,  
function(M, int, grad)
local cone, gens, cones, x, n;

cone:=MonoidOrbit(M, int);
gens:=GeneratorsOfSemigroup(M);

cones:=[];
cones[int]:=cone;

for x in [1..DegreeOfTransformationSemigroup(M)] do 
   if x in cone then 
      cones[x]:=MonoidOrbit(M, x);
   else 
      cones[x]:=[];
   fi;
od;

n:=grad(int);

#JDM better way? and advantage?
return Filtered(First(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(cones), x-> int in x), y-> grad(y)=n);

end);