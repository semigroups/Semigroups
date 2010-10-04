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
hashlen]);
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
# new for 3.2!

InstallGlobalFunction(HashTableForKernels, 
function(ker)
local hf, ht;
hf:=function ( l, hashlen )
local  v, i;
v := 0;
for i  in [ 1 .. Length( l ) ]  do
	v := (v * 101 + ORB_HashFunctionForPlainFlatList( l[i], hashlen )) 
	 mod hashlen;
od;
return v + 1;
end;

ht := HTCreate(ker, rec( hf := hf, hfd := 100003, treehashsize := 100003 ));
HTAdd(ht, ker, 1);

return ht;
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


# new for 4.0!
#############################################################################

InstallGlobalFunction(HashTableForImage, 
function(img)
local ht;
ht := HTCreate(img, rec( hfd := 100003, treehashsize := 100003 ));
HTAdd(ht, img, 1);

return ht;
end);

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
 
#OnTuplesOfSetsAntiAction2:=function(tup, s)
 
#ker_g:=List([1..Length(f![1])], x-> []);

#for i in [1..Length(f![1])] do 
#	Add(ker_g[g![1][i]], i);
#od;


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



