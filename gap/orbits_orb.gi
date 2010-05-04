#############################################################################
##
#W  orbits_orb.gi                      monoid 
#Y  Copyright (C) 2006-2010            James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id
##

_HashFunctionForTransformation := function(v,data) 
   return ORB_HashFunctionForIntList(v![1], data); 
end;

#############################################################################
#JDM this does not work well. Also the hash function for tuples is poor.

InstallMethod( ChooseHashFunction, "for transformations",
[IsTransformation, IsInt],
function(p,hashlen)
return rec(func := _HashFunctionForTransformation, data := [101,hashlen]);
end);

#############################################################################
# JDM this should become a global function, and return the orbit object.

#InstallOtherMethod(ForwardOrbit, 
InstallOtherMethod(MonoidOrbit,
"for a trans. collection, an arbitary object, and action", 
[IsTransformationCollection, IsObject, IsFunction],
function(s, seed, action)
local gens, ht, orbit, schreier, i, x, j, new;

if IsSemigroup(s) then 
	gens:= GeneratorsOfSemigroup(s);
elif IsTransformationCollection(s) then 
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

#############################################################################
# JDM this should become a global function, and return the orbit object.
# JDM it should also be renamed GradedForwardOrbit

InstallOtherMethod(GradedOrbit, 
"for a trans. collection, an arbitary object, action, and grading",
[IsTransformationCollection, IsObject, IsFunction, IsFunction],
function(s, seed, action, grading)
local gens, ht, orbit, schreier, i, graded, val, x, j, new;

if IsSemigroup(s) then 
	gens:= GeneratorsOfSemigroup(s);
elif IsTransformationCollection(s) then 
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

###############################################################

InstallMethod(PrintObj, "for a forward orbit", 
[IsForwardOrbit],
function(o) 
if IsBound(o!.graded) then 
	Print("<graded forward orbit, ", Length(o!.orbit), " points, ", 
	 Length(o!.graded), " subsets>" );
elif IsBound(o!.type) and o!.type="short" then 
	Print("<short forward orbit, ", Length(o!.orbit), " points>");
elif IsBound(o!.type) and o!.type="strong" then 
	Print("<strong orbit, ", Length(o!.orbit), " points>");
else
	Print("<forward orbit, ", Length(o!.orbit), " points>");
fi;
end);

###############################################################

InstallOtherMethod(ShortOrbit, 
"for a transformation collection, object, action, and grading", 
[IsTransformationCollection, IsObject, IsFunction, IsFunction], 
function(s, seed, action, grading)
local gens, ht, orbit, schreier, i, val, x, j, new;

if IsSemigroup(s) then 
	gens:= GeneratorsOfSemigroup(s);
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

InstallOtherMethod(StrongOrbit, "for a trans. semigroup, object and action", 
[IsTransformationCollection, IsObject, IsFunction],
function(s, seed, action)
local gens, ht, orbit, schreier, i, graph, x, j, val, new;
 
if IsSemigroup(s) then 
	gens:= GeneratorsOfSemigroup(s);
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
# JDM this should become a global function, and return the orbit object.

InstallOtherMethod(StrongOrbits, "for a trans. collection, object and action", 
[IsTransformationCollection, IsObject, IsFunction],
function(s, seed, action)
local gens, ht, orbit, schreier, i, graph, mult, x, j, new, val;

if IsSemigroup(s) then 
	gens:= GeneratorsOfSemigroup(s);
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