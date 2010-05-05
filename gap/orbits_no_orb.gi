#############################################################################
##
#W  orbits_no_orb.gi                   monoid 
#Y  Copyright (C) 2006-2010            James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

###########################################################################

InstallMethod(GradedOrbit, 
"for a trans. collection, an arbitary object, action, and grading",
[IsTransformationCollection, IsObject, IsFunction, IsFunction],
function(s, seed, action, grading)
local gens, graded, grading_of_seed, x, y, z, new, val;

if IsMonoid(s) then  
	gens:= GeneratorsOfMonoid(s);
elif IsSemigroup(s) then
	gens:=GeneratorsOfSemigroup(s);
else
	gens:=s;
fi;

graded:=[];
grading_of_seed:=grading(seed);
graded[grading_of_seed]:=[];
Add(graded[grading_of_seed], seed);

for x in [grading_of_seed, grading_of_seed-1..1] do
	if IsBound(graded[x]) then 
		x:=graded[x];
		for y in x do
			for z in gens do
				new:= action(y, z);
				val:= grading(new);
				if IsBound(graded[val]) and not new in graded[val] then   
					Add(graded[val], new);
				elif not IsBound(graded[val]) then 
					graded[val]:=[];
					Add(graded[val], new);
				fi;
			od;
		od;
	fi;
od;

return Compacted(graded);
end);

###########################################################################

InstallMethod(MonoidOrbit, "for a trans. collection, object, and action",
[IsTransformationCollection, IsObject, IsFunction],
function(s, seed, action)
local orbit, gens, x, y, new;
   	
if IsMonoid(s) then  
	gens:= GeneratorsOfMonoid(s);
elif IsSemigroup(s) then
	gens:=GeneratorsOfSemigroup(s);
else
	gens:=s;
fi;

orbit:= [seed];  
	
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

InstallMethod(ShortOrbit, 
"for a trans. collection, object, action, and grading", 
[IsTransformationCollection, IsObject, IsFunction, IsFunction], 
function(s, seed, action, grading)
local orbit, n, gens, x, y, new;

if IsMonoid(s) then  
	gens:= GeneratorsOfMonoid(s);
elif IsSemigroup(s) then
	gens:=GeneratorsOfSemigroup(s);
else
	gens:=s;
fi;

orbit:= [seed];  n:= grading(seed);

for x in orbit do
   for y in gens do
      new:= action(x,y);
      if n=grading(new)  and not new in orbit then
         Add(orbit, new);
      fi;
   od;
od;

return orbit;
end );

###########################################################################

InstallMethod(ShortStrongOrbit,  
"for a trans. collection, object, action, and grading", 
[IsTransformationCollection, IsObject, IsFunction, IsFunction],
function(s, seed, action, grading)
local orbit, i, j, graph, x, pos, new, gens, grading_of_seed;

orbit:=[seed];
i:=0;
graph:=[[]];

if IsMonoid(s) then  
	gens:= GeneratorsOfMonoid(s);
elif IsSemigroup(s) then
	gens:=GeneratorsOfSemigroup(s);
else
	gens:=s;
fi;

grading_of_seed:=grading(seed);

for x in orbit do
	i:=i+1;
	for j in [1..Length(gens)] do
		new:= action(x, gens[j]);
		pos:=Position(orbit, new);
		if pos=fail and grading(new)=grading_of_seed then 
			 Add(orbit, new);
			 Add(graph, []);
			 pos:=Length(orbit);
		fi; 
		if not pos=fail then 
			AddSet(graph[i], pos);
		fi;
	od;
od;

return orbit{First(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(graph), x-> 1 in x)};
end);

###########################################################################

InstallMethod(StrongOrbit, "for a trans. collection, object and action", 
[IsTransformationCollection, IsObject, IsFunction],
function(s, seed, action)
local orbit, i, j, graph, x, pos, new, gens;

orbit:=[seed];
i:=0;
graph:=[[]];

if IsMonoid(s) then  
	gens:= GeneratorsOfMonoid(s);
elif IsSemigroup(s) then
	gens:=GeneratorsOfSemigroup(s);
else
	gens:=s;
fi;

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

###########################################################################

InstallGlobalFunction(StrongOrbitsInForwardOrbit, 
function(s, seed, action)
local gens, orbit, i, graph, x, j, new, pos;

if not (IsTransformationCollection(s) and IsObject(seed) 
 and IsFunction(action)) then 
 	Info(InfoWarning, 1, "Usage: trans. collection, object, and action");
 	return fail;
fi;

if IsMonoid(s) then  
	gens:= GeneratorsOfMonoid(s);
elif IsSemigroup(s) then
	gens:=GeneratorsOfSemigroup(s);
else
	gens:=s;
fi;

orbit:=[seed];
i:=0;
graph:=[[]]; 

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

graph:=STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(graph);
return List(graph, x-> orbit{x});
end);

