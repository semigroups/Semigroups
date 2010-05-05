#############################################################################
##
#W  orbits_orb.gi                      monoid 
#Y  Copyright (C) 2006-2010            James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

_HashFunctionForTransformation := function(v,data) 
   return ORB_HashFunctionForIntList(v![1], data); 
end;

#############################################################################

InstallMethod( ChooseHashFunction, "for transformations",
[IsTransformation, IsInt],
function(p,hashlen)
return rec(func := _HashFunctionForTransformation, data := [101, 
NextPrimeInt(hashlen)]);
end);

#############################################################################
# JDM this should become a global function, and return the orbit object.
# JDM it should also be renamed GradedForwardOrbit

InstallMethod(GradedOrbit, 
"for a trans. collection, object, action, and grading",
[IsTransformationCollection, IsObject, IsFunction, IsFunction],
function(s, seed, action, grading)
local gens, ht, orbit, schreier, i, graded, val, x, j, new;

if IsMonoid(s) then  
	gens:= GeneratorsOfMonoid(s);
elif IsSemigroup(s) then
	gens:=GeneratorsOfSemigroup(s);
else
	gens:=s;
fi;

ht:=HTCreate(seed);
HTAdd(ht, seed, true);
orbit:=[seed];
schreier:=[[]];
i:=0;

graded:=[];
val:=grading(seed);
graded[val]:=[];
Add(graded[val], seed);

for x in orbit do
	i:=i+1;
	for j in [1..Length(gens)] do
		new:= action(x, gens[j]);
		if HTValue(ht, new)=fail then 
			Add(orbit, new);
			Add(schreier, Concatenation(schreier[i], [j]));
			HTAdd(ht,  new, true);
			val:=grading(new);
			
			if not IsBound(graded[val]) then 
				graded[val]:=[];
			fi;

			Add(graded[val], new);
		fi;
	od;
od;
 
#o:=rec(ht:=ht, orbit:=orbit, schreier:=schreier, graded:=Compacted(graded));
#Objectify( ForwardOrbitType, o);
#return o;

return graded;
end);

#############################################################################
# JDM this should become a global function, and return the orbit object.

#InstallOtherMethod(ForwardOrbit, 
InstallOtherMethod(MonoidOrbit,
"for a trans. collection, object, and action", 
[IsTransformationCollection, IsObject, IsFunction],
function(s, seed, action)
local gens, ht, orbit, schreier, i, x, j, new;

if IsMonoid(s) then  
	gens:= GeneratorsOfMonoid(s);
elif IsSemigroup(s) then
	gens:=GeneratorsOfSemigroup(s);
else
	gens:=s;
fi;

ht:=HTCreate(seed);
HTAdd(ht, seed, true);

orbit:=[seed];
schreier:=[[]];
i:=0;

for x in orbit do
	i:=i+1;
	for j in [1..Length(gens)] do
		new:= action(x, gens[j]);
		if HTValue(ht, new)=fail then 
			Add(orbit, new);
			Add(schreier, Concatenation(schreier[i], [j])); 
			HTAdd(ht, new, true);
		fi;
	od;
od;
 
#o:=rec(ht:=ht, orbit:=orbit, schreier:=schreier);
#Objectify( ForwardOrbitType, o);
#return o;
return orbit;
end);

###############################################################

InstallOtherMethod(ShortOrbit, 
"for a trans. collection, object, action, and grading", 
[IsTransformationCollection, IsObject, IsFunction, IsFunction], 
function(s, seed, action, grading)
local gens, ht, orbit, schreier, i, val, x, j, new;

if IsMonoid(s) then  
	gens:= GeneratorsOfMonoid(s);
elif IsSemigroup(s) then
	gens:=GeneratorsOfSemigroup(s);
else
	gens:=s;
fi;

ht:=HTCreate(seed);
HTAdd(ht, seed, true);
orbit:=[seed];
schreier:=[[]];
i:=0;

val:=grading(seed);

for x in orbit do
	i:=i+1;
	for j in [1..Length(gens)] do
		new:= grading(x, gens[j]);
		if HTValue(ht, new)=fail and grading(new)=val then 
			Add(orbit, new);
			Add(schreier, Concatenation(schreier[i], [j])); 
			HTAdd(ht,  new, true);
		fi;
	od;
od;
 
#o:=rec(ht:=ht, orbit:=orbit, schreier:=schreier, type:="short");
#Objectify( ForwardOrbitType, o);
#return o;

return orbit;
end);

#############################################################################
# JDM this should become a global function, and return the orbit object.

InstallMethod(ShortStrongOrbit,
"for a trans. collection, object, action, and grading",
[IsTransformationCollection, IsObject, IsFunction, IsFunction],
function(s, seed, action, grading)
local gens, ht, orbit, schreier, graph, i, grading_of_seed, x, j, new, val;
 
if IsMonoid(s) then  
	gens:= GeneratorsOfMonoid(s);
elif IsSemigroup(s) then
	gens:=GeneratorsOfSemigroup(s);
else
	gens:=s;
fi;

ht:=HTCreate(seed);
HTAdd(ht, seed, 1); #the 1 indicates that the element is at position 1 in orbit
orbit:=[seed];
schreier:=[[]];
graph:=[[]]; 
#mult:=[]; JDM currently unused
i:=0;

grading_of_seed:=grading(seed);

for x in orbit do
	i:=i+1;
	for j in [1..Length(gens)] do
		new:= action(x, gens[j]);
		val:=HTValue(ht, new);
		if val=fail and grading(new)=grading_of_seed then 
			 Add(orbit, new);
			 Add(schreier, Concatenation(schreier[i], [j]));
			 HTAdd(ht, new, Length(orbit)); 
			 Add(graph, []);
			 val:=Length(orbit);
			 #if Length(new)=Length(orbit[1]) then #JDM use this later!
				#	mult[Length(orbit)]:=MappingPermListList(orbit[1], new);
			 #fi;
		fi;
		AddSet(graph[i], val);
	od;
od;

graph:=STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(graph);
x:=First(graph, y-> 1 in y);

#return Objectify(ForwardOrbitType, rec(ht:=ht, orbit:=orbit{o}, 
#	 schreier:=schreier{o}, type:="strong"));

return orbit{x};
end);

#############################################################################
# JDM this should become a global function, and return the orbit object.

InstallOtherMethod(StrongOrbit, "for a trans. collection, object, and action", 
[IsTransformationCollection, IsObject, IsFunction],
function(s, seed, action)
local gens, ht, orbit, schreier, i, graph, x, j, val, new;
 
if IsMonoid(s) then  
	gens:= GeneratorsOfMonoid(s);
elif IsSemigroup(s) then
	gens:=GeneratorsOfSemigroup(s);
else
	gens:=s;
fi;

ht:=HTCreate(seed);
HTAdd(ht, seed, 1); #the 1 indicates that the element is at position 1 in orbit
orbit:=[seed];
schreier:=[[]];
graph:=[[]]; 
#mult:=[]; JDM currently unused
i:=0;

for x in orbit do
	i:=i+1;
	for j in [1..Length(gens)] do
		new:= action(x, gens[j]);
		val:=HTValue(ht, new);
		if val=fail then 
			 Add(orbit, new);
			 Add(schreier, Concatenation(schreier[i], [j]));
			 HTAdd(ht, new, Length(orbit)); 
			 Add(graph, []);
			 val:=Length(orbit);
			 #if Length(new)=Length(orbit[1]) then #JDM use this later!
				#	mult[Length(orbit)]:=MappingPermListList(orbit[1], new);
			 #fi;
		fi;
		AddSet(graph[i], val);
	od;
od;

graph:=STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(graph);
x:=First(graph, y-> 1 in y);

#return Objectify(ForwardOrbitType, rec(ht:=ht, orbit:=orbit{o}, 
#	 schreier:=schreier{o}, type:="strong"));

return orbit{x};
end);

#############################################################################

InstallGlobalFunction(StrongOrbitsInForwardOrbit, 
function(s, seed, action)
local gens, ht, orbit, schreier, i, graph, mult, x, j, new, val;

if not (IsTransformationCollection(s) and IsObject(seed) 
 and IsFunction(action)) then 
 	Info(InfoWarning, 1, "Usage: transformation collection, object, and action");
 	return fail;
 fi;

if IsMonoid(s) then  
	gens:= GeneratorsOfMonoid(s);
elif IsSemigroup(s) then
	gens:=GeneratorsOfSemigroup(s);
else
	gens:=s;
fi;

ht:=HTCreate(seed);
orbit:=[seed];
schreier:=[[]];
i:=0;

HTAdd(ht, seed, 1); #the 1 indicates that the element is at position 1 in orbit
graph:=[[]]; 
mult:=[];

for x in orbit do
	i:=i+1;
	for j in [1..Length(gens)] do
		new:= action(x, gens[j]);
		val:=HTValue(ht, new);
		if val=fail then 
			 Add(orbit, new);
			 Add(schreier, Concatenation(schreier[i], [j]));
			 HTAdd(ht, new, Length(orbit)); 
			 Add(graph, []);
			 val:=Length(orbit);
			 #if Length(new)=Length(orbit[1]) then #JDM use this later!
				#	mult[Length(orbit)]:=MappingPermListList(orbit[1], new);
			 #fi;
		fi;
		AddSet(graph[i], val);
	od;
od;

graph:=STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(graph);

#JDM use the following in the ForwardOrbits command!
#for x in graph do 
#JDM not sure this is great, what can the ht be used for here?
#	Add(o, Objectify(ForwardOrbitType, rec(ht:=ht, orbit:=orbit{x}, 
#	 schreier:=schreier{x}, type:="strong")));
#, mult:=mult 
#od;
#return o;

return List(graph, x-> orbit{x});
end);