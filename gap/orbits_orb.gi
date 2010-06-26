#############################################################################
##
#W  orbits_no_orb.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##


remove_warning:=function()
Info(InfoWarning, 1, "JDM this function should be removed before release");
Info(InfoWarning, 1, " of 3.2. It is only included for testing purposes");
end;

_HashFunctionForTransformation := function(v,data) 
   return ORB_HashFunctionForIntList(v![1], data); 
end;

#JDM new for 3.2!
#############################################################################
# assumes <o> is an orbit object from `orb', that <o> satisfies IsGradedOrbit
# and that the grading function is a pos. int. that decreases as the orbit 
# gets longer.

InstallGlobalFunction( AsPartitionedListNC,
function(o)
local i, out;

Info(InfoMonoidOrbits, 4, "AsPartitionedListNC");

out:=List([1..Grades(o)[1]], x-> []);
Enumerate(o);

for i in [1..Length(o)] do 
  Add(out[Grades(o)[i]], o[i]);
od;

return Filtered(Compacted(out), x-> not x=[]);
end);

#############################################################################

InstallMethod( ChooseHashFunction, "for transformations and pos. int.",
[IsTransformation, IsInt],
function(p, hashlen)
return rec(func := _HashFunctionForTransformation, data := [101, 
NextPrimeInt(hashlen)]);
end);

#############################################################################
# JDM remove GradedOrbit from 3.2!

InstallMethod(GradedOrbit, 
"for a trans. collection, object, action, and grading",
[IsTransformationCollection, IsObject, IsFunction, IsFunction],
function(s, seed, action, grading)
local gens, ht, orbit, schreier, i, graded, val, x, j, new;

remove_warning();

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

return Compacted(graded);
end);

#############################################################################
# delegate to Orb.
# JDM are the options ok, in general? what about using 
# MakeHashFunctionForPlainFlatList?

InstallGlobalFunction(GradedForwardOrbitNC, 
function(s, seed, action, grading)
local o;

Info(InfoMonoidOrbits, 4, "GradedForwardOrbitNC");

o:=Orb(s, seed, action, rec(hashlen:=100003, schreier:=true,
        gradingfunc := function(o,x) return grading(x); end));

return AsPartitionedListNC(o);
end);

#############################################################################

InstallGlobalFunction(GradedForwardOrbit, 
function(s, seed, action, grading)
local o;

Info(InfoMonoidOrbits, 4, "GradedForwardOrbit");

if IsTransformationCollection(s) and IsObject(seed) and IsFunction(action)
 and IsFunction(grading) then 
  o:=GradedForwardOrbitNC(s, seed, action, grading);
  return o;
fi;

Info(InfoWarning, 1, "arguments should be a trans. coll., point, action,",
 " and grading");
return fail;
end);

###########################################################################
# JDM remove GradedImagesOfTransSemigroup from 3.2!

InstallMethod(GradedImagesOfTransSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
local gens, ht, orbit, i, graded, val, x, j, new, seed, grading, action;

if IsMonoid(s) then  
	gens:= GeneratorsOfMonoid(s);
elif IsSemigroup(s) then
	gens:=GeneratorsOfSemigroup(s);
else
	gens:=s;
fi;

seed:=[1..DegreeOfTransformationSemigroup(s)]; grading:=Size; action:=OnSets;

ht:=HTCreate(seed);
HTAdd(ht, seed, true);
orbit:=[seed];
i:=0;

graded:=List([1..DegreeOfTransformationSemigroup(s)], x->[]);
val:=grading(seed);
Add(graded[val], seed);

for x in orbit do
	i:=i+1;
	for j in [1..Length(gens)] do
		new:= action(x, gens[j]);
		if HTValue(ht, new)=fail then 
			Add(orbit, new);
			HTAdd(ht,  new, true);
			val:=grading(new);
			
			Add(graded[val], new);
		fi;
	od;
od;

return graded;
end);

#############################################################################
#JDM new for 3.2!

# the following has its own method rather than using GradedOrbit, as for 
# convenience the output should be a list of lists where the <i>th position
# is the list of images of <s> of size <i> including the empty list. 

InstallGlobalFunction(GradedImagesOfTransSemigroupNC, 
function(s)
local d, o, out, i;

Info(InfoMonoidOrbits, 4, "GradedImagesOfTransSemigroupNC");

d:=DegreeOfTransformationCollNC(s);

o:=Orb(s, [1..d], OnSets, rec(hashlen:=100003, schreier:=true,
        gradingfunc := function(o,x) return Length(x); end, 
        orbitgraph := true));
return o;
end);

#o:=GradedForwardOrbitNC(s, [1..d], OnSets, Size);
#out:=List([1..d], x-> []);
#for i in o do 
#  out[Length(i[1])]:=i;
#od;
#return out; 


#############################################################################
# JDM remove GradedKernelsOfTransSemigroup from 3.2!

InstallMethod(GradedKernelsOfTransSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
local gens, ht, orbit, i, graded, val, x, j, new, seed, grading, action;

if IsMonoid(s) then  
	gens:= GeneratorsOfMonoid(s);
elif IsSemigroup(s) then
	gens:=GeneratorsOfSemigroup(s);
else
	gens:=s;
fi;

seed:=List([1..DegreeOfTransformationSemigroup(s)], x-> [x]); 
grading:=Size; action:=OnKernelsAntiAction;

ht:=HTCreate(seed);
HTAdd(ht, seed, true);
orbit:=[seed];
i:=0;

graded:=List([1..DegreeOfTransformationSemigroup(s)], x->[]);
val:=grading(seed);
Add(graded[val], seed);

for x in orbit do
	i:=i+1;
	for j in [1..Length(gens)] do
		new:= action(x, gens[j]);
		if HTValue(ht, new)=fail then 
			Add(orbit, new);

			HTAdd(ht,  new, true);
			val:=grading(new);
			
			Add(graded[val], new);
		fi;
	od;
od;

return graded;
end);

#############################################################################
# JDM new for 3.2!

# see comment before GradedImagesOfTransSemigroup for explaination of why 
# this method exists.

InstallGlobalFunction(GradedKernelsOfTransformationSemigroupNC, 
function(s)
local d, o, out, i;

Info(InfoMonoidOrbits, 4, "++GradedKernelsOfTransformationSemigroupNC");

d:=DegreeOfTransformationCollNC(s);
o:=AsList(GradedForwardOrbitNC(s, List([1..d], x-> [x]), 
                                OnKernelsAntiAction, Size));
out:=List([1..d], x-> []);

for i in o do 
  out[Length(i[1])]:=i;
od;

Info(InfoMonoidOrbits, 4, "--GradedKernelsOfTransformationSemigroupNC");
return out; #JDM better to just return the orbit... hopefully this can be
            #    improved!
end);


#############################################################################
# JDM remove from 3.2!

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
Error("");

return orbit;
end);

#############################################################################
# JDM new for 3.2!

InstallGlobalFunction(ForwardOrbitNC, 
function(s, seed, action)
local o;
Info(InfoMonoidOrbits, 4, "++ForwardOrbitNC");
o:=Orb(s, seed, action, rec(treehashsize:=100003));#JDM use this as standard!
Info(InfoMonoidOrbits, 4, "--ForwardOrbitNC");

return o;
end);

#############################################################################
# JDM new for 3.2!

InstallGlobalFunction(ForwardOrbit, 
function(s, seed, action)
local o;
Info(InfoMonoidOrbits, 4, "++ForwardOrbit");
if IsTransformationCollection(s) and IsObject(s) and IsFunction(action) then 
  o:=ForwardOrbitNC(s, seed, action);
  Info(InfoMonoidOrbits, 4, "--ForwardOrbit");
  return o;
fi;
Info(InfoWarning, 1, "argument should be trans. coll., a point, and an", 
 " action");
Info(InfoMonoidOrbits, 4, "--ForwardOrbit");
return fail;
end);

###############################################################
# JDM remove from 3.2!

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
		new:= action(x, gens[j]);
		if HTValue(ht, new)=fail and grading(new)=val then 
			Add(orbit, new);
			Add(schreier, Concatenation(schreier[i], [j])); 
			HTAdd(ht,  new, true);
		fi;
	od;
od;

return orbit;
end);

###############################################################
# JDM new for 3.2!

InstallGlobalFunction(ShortForwardOrbitNC, 
function(s, seed, action, grading)
local o;

Info(InfoMonoidOrbits, 4, "++ShortForwardOrbitNC");
o:=Orb(s, seed, action, rec(hashlen:=100003, schreier:=true, 
        gradingfunc:= function(o,x) return grading(x); end, 
        onlygrades:=[grading(seed)]));
Info(InfoMonoidOrbits, 4, "--ShortForwardOrbitNC");

return o;
end); #JDMJDM here!!

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

###############################################################
# JDM new for 3.2!

#InstallGlobalFunction(StrongOrbitNC, 
#function(arg)

#if Length(arg)=4 then 
#  s:=arg[1]; seed:=arg[2]; action:=arg[3]; grading:=arg[4];
  
#elif Length(arg)=3 then 
#  s:=arg[1]; seed:=arg[2]; action:=arg[3];
#else
#  Info(InfoWarning, 1, "there should be 3 or 4 inputs");
#  return fail;
#fi;
#end);


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
		fi;
		AddSet(graph[i], val);
	od;
od;

graph:=STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(graph);

return List(graph, x-> orbit{x});
end);