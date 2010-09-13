#############################################################################
##
#W  greens_r_orb.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$

#############################################################################
## Notes

# - this file is alphabetized, keep it that way!

# - this file should only contain functions relating to images/r-classes!

# - consider storing the relevant data for an R-class in that R-class 
#   rather than repeatedly obtaining it from OrbitsOfImages. About 
#   1/3 of the time in finding all the idempotents of all the R-classes
#   is spent in line JDM1. In particular, the scc corresponding to the R-class
#   (the actual images that is).

# - check that wherever we have called d:=InOrbitsOfImages we do not perform
# f*perms[l] afterwards but instead use d[7]!

# - d_schutz should be moved from OrbitsOfImages to OrbitsOfKernels.

#############################################################################
## To do 

# - make test files

# - remove all functions supporting the old methods etc for GreensRClasses/
#  images... 

# - check other functions in other files to see if they still work (or not!)

# - write documentation

# - remove KernelOfTransformationNC everywhere...

# - test against latest orb release...

##
#############################################################################

InstallMethod( \=, "for R-class of trans. semigp. and R-class of trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp, IsGreensRClass and 
IsGreensClassOfTransSemigp],
function(r1, r2)

return r1!.parent=r2!.parent and r1!.rep in r2;
end);

## new for 3.2!
#############################################################################
##  Algorithm E. 

InstallMethod( \in, "for trans. and R-class of trans. semigp.", 
[IsTransformation, IsGreensRClass and IsGreensClassOfTransSemigp],
function(f, r)
local rep, d, o, i, schutz, s;

rep:= r!.rep; 

if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or
 RankOfTransformation(f) <> RankOfTransformation(rep) or
 KernelOfTransformation(f) <> KernelOfTransformation(rep) then
	return false;
fi;

if f=rep then 
	return true;
fi;

d:=r!.data;
s:=r!.parent;
o:=r!.o;

i:= Position(o, ImageSetOfTransformation(f));

if i = fail or not o!.truth[d[4]][i] then #check they are in the same scc
	return false;
fi;

schutz:= RClassStabChainFromData(s, o, d);

return schutz=true or SiftedPermutation(schutz, 
 PermLeftQuoTransformationNC(rep, f*o!.perms[i]))=();
end);


# new for 3.2!
#############################################################################
# not algorithm X.

InstallMethod(\in, "for a transformation semigroup", 
[IsTransformation, IsTransformationSemigroup],
function(f, s)
local gens, g, o, iter, orbits;

#Info(InfoMonoidGreens, 4, "\in: for a trans. semigroup");

if HasAsSSortedList(s) then 
	return f in AsSSortedList(s);
fi;

gens:=GeneratorsOfSemigroup(s);

if not DegreeOfTransformation(f) = DegreeOfTransformation(gens[1]) then 
	return false;
fi;

o:=OrbitsOfImages(s);
orbits:=o!.orbits;
g:=InOrbitsOfImages(s, f, orbits, []);

if g[1] then 
	return true;
elif o!.finished then 
	return false;
fi;

# check what's already known...
iter:=IteratorOfRClassRepsData(s);
#avoid checking what's already known!
iter!.i:=Length(o!.data); 

repeat
	NextIterator(iter);
	g:=InOrbitsOfImages(s, f, orbits, g[2]);

	if g[1] then 
		return true;
	fi;
until IsDoneIterator(iter);

#JDM could also put something in here that returns false if everything,
#from OrbitsOfImages(s)!.at to the end of OrbitsOfImages(s)!.ht!.o 
#has rank less than f. Might be a good idea when degree is very high!

return false;
end);

#############################################################################
#

InstallGlobalFunction(AddToOrbitsOfImages,
function(s, o, f, data)
local j, k, l, m, val, n, g, O, one, gens, d, reps, schutz, img, scc, i, 
 oo, r, ht, y, z, out, deg, bound;

j:=data[1]; 	# img size
k:=data[2]; 	# index of orbit containing img
l:=data[3]; 	# position of img in O[j][k]
m:=data[4]; 	# scc of O[j][k] containing img
val:=data[5]; # position of ker in O[j][k]!.kernels_ht[m]
n:=data[6]; 	# the length of O[j][k]!.reps[m][val]
g:=data[7];		# f*O[j][k]!.perms[l];

O := o!.orbits; one:=o!.one; gens:=o!.gens;
d:=o!.data; ht:=o!.ht; o:=ht!.o;

if k = fail then #new img and l, m, val, n, g=fail

################################################################################

	img:=ImageSetOfTransformation(f);

	deg:=DegreeOfTransformationSemigroup(s);

	if deg<=1000 then 
		bound:=Binomial(DegreeOfTransformationSemigroup(s), j);
	else
		bound:=infinity;
	fi;
	
	oo:=Orb(s, img, OnSets, rec(
					treehashsize:=NextPrimeInt(Minimum(100000, 
					 bound)), 
					schreier:=true,
					gradingfunc := function(o,x) return Length(x); end, 
					orbitgraph := true, 
					onlygrades:=[j], 
					storenumbers:=true));
	
	Enumerate(oo, bound);
	
	#strongly connected components
	scc:=Set(List(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(List(OrbitGraph(oo), 
	Set)), Set));;
	
	if IsBound(O[j]) then 
	#JDM put in the grading function of the orbit if possible
		scc:=scc{Filtered([1..Length(scc)], m-> not ForAny(O[j], x-> 
		 oo[scc[m][1]] in x))};
	fi;
	
	r:=Length(scc);
	oo!.scc:=scc;
	
	#boolean list corresponding to membership in scc[i]
	oo!.truth:=List([1..r], i-> BlistList([1..Length(oo)], scc[i]));
	oo!.trees:=List([1..r], x-> CreateSchreierTreeOfSCC(oo,x));
	
	#representatives of R-classes with image belonging in scc[i] partitioned 
	#according to their kernels
	reps:=List([1..r], m-> 
	 f*EvaluateWord(gens, TraceSchreierTreeForward(oo, scc[m][1])));
	oo!.reps:=List(reps, x->[[x]]); 

	#kernels of representatives of R-classes with image belonging in scc[i]
	oo!.kernels_ht:=List([1..r], m-> 
	 HashTableForKernels(KernelOfTransformation(reps[m])));
	
	#calculate the multipliers for all scc's 
	oo!.perms:=EmptyPlist(Length(oo));
	for i in [1..Length(scc)] do 
		oo!.perms:=oo!.perms+MultipliersOfSCCOfImageOrbit(gens, oo, i);
	od;
	
	#schutzenberger groups
	oo!.schutz:=List([1..r], m-> 
	 SchutzGpOfImageOrbit(gens, oo, reps[m], m));

	#Schutzenberger groups of D-classes and H-classes (only here for convenience
	#when retrieving from the D-classes R-class data! JDM move to 
	# AddToOrbitsOfKernels!
	#oo!.d_schutz:=List([1..r], x-> [[]]);
	
	if IsBound(O[j]) then 
		Add(O[j], oo);
	else
		O[j]:=[oo];
	fi;
	
	out:=[j, Length(O[j]), 1, 1, 1, 1];
	
	for m in [1..r] do 
		d[Length(d)+1]:=[j, Length(O[j]), 1, m, 1, 1];
	od;
	
	
##############################################################################

else #old img
	reps:=O[j][k]!.reps[m];
	#d_schutz:=O[j][k]!.d_schutz[m];
	
	if not val=fail then #old kernel
		reps[val][n+1]:=g;
		out:=[j, k, l, m, val, n+1];
		d[Length(d)+1]:=out;
	else #new kernel
		val:=Length(reps)+1;
		reps[val]:=[g];
		#d_schutz[val]:=[];
		out:=[j, k, l, m, val, 1];
		d[Length(d)+1]:=out;
		HTAdd(O[j][k]!.kernels_ht[m], KernelOfTransformation( g ), val);
	fi;
	reps:=[g]; #JDM should it be g or f?

fi;

#install new pts in the orbit
for f in reps do 
	for y in [1..Length(gens)] do
		z:=gens[y]*f;
		if HTValue(ht, z)=fail then  
			HTAdd(ht, z, true);
			o[Length(o)+1]:=z;
			#schreier words here
		fi;
	od;
od;

return out;
end);

# new for 3.2!
#############################################################################
# Algorithm D.
# JDM check for efficiency!

InstallOtherMethod(AsList, "for an R-class of trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
local f, s, d, h, elts, p, o;

Info(InfoMonoidGreens, 4, "AsList: for an R-class");

f:=r!.rep; #rep should have its image at the first place in the scc
s:=r!.parent;
d:=r!.data;
o:=r!.o;

h:=List(RClassSchutzGpFromData(s, d, o), x-> f*x);

elts:=[];

for p in RClassPermsFromData(s, d, o){RClassSCCFromData(s, d, o)} do 
	elts:=Concatenation(elts, h*p^-1);
od;
return elts;
end);

# new for 3.2!
#############################################################################
# this should be removed after the library method for AsSSortedList 
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)!

InstallOtherMethod(AsSSortedList, "for R-class of trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
Info(InfoMonoidGreens, 4, "AsSSortedList: for an R-class");
return ConstantTimeAccessList(EnumeratorSorted(r));
end);

# new for 3.2!
#############################################################################
# o is the orbit
# i is the index of the scc of o we are trying to create the Schreier tree for!

# this should be moved to greens.gi

InstallGlobalFunction(CreateSchreierTreeOfSCC,
function(o, i)
local gen, pos, seen, oo, graph, j, k, l, scc, t;

#Info(InfoMonoidGreens, 4, "CreateSchreierTreeOfSCC");

if i=1 then 
  return [o!.schreiergen, o!.schreierpos];
fi;

scc:=o!.scc[i];

gen:=List([1..Length(o)], x-> fail);
pos:=List([1..Length(o)], x-> fail);
seen:=BlistList([1..Length(o)], [scc[1]]);
t:=o!.truth[i];
oo:=[scc[1]];
graph:=OrbitGraph(o);
j:=0;

while Length(oo)<Length(scc) do 
  j:=j+1;
	k:=oo[j];
	l:=0;
	while l<Length(graph[k]) and Length(oo)<Length(scc) do  
		l:=l+1;
		if IsBound(graph[k][l]) and not seen[graph[k][l]] and t[graph[k][l]] then 
			Add(oo, graph[k][l]); seen[graph[k][l]]:=true; 
			gen[graph[k][l]]:=l; pos[graph[k][l]]:=k;
		fi;
	od;
od;

return [gen, pos];
end);

# new for 3.2!
#############################################################################
#

# this should be moved to greens.gi

InstallGlobalFunction(CreateReverseSchreierTreeOfSCC,
function(o, i)
local rev, j, k, l, m, graph, scc, gen, pos, seen, t, oo; 

#Info(InfoMonoidGreens, 4, "CreateReverseSchreierTreeOfSCC");

graph:=OrbitGraph(o);
rev:=List([1..Length(graph)], x-> List([1..Length(o!.gens)], x-> []));

for j in [1..Length(graph)] do
  for k in [1..Length(graph[j])] do 
    if IsBound(graph[j][k]) then 
      Add(rev[graph[j][k]][k], j);
      #starting at position j and applying gens[k] we obtain graph[j][k];
    fi;
  od;
od;

scc:=o!.scc[i];

gen:=List([1..Length(o)], x-> fail);
pos:=List([1..Length(o)], x-> fail);
seen:=BlistList([1..Length(o)], [scc[1]]);
t:=o!.truth[i];
oo:=[scc[1]];

j:=0;

while Length(oo)<Length(scc) do 
  j:=j+1;
  k:=oo[j];
  l:=0;
  while l< Length(rev[k]) and Length(oo)<Length(scc) do 
    l:=l+1;
    m:=0;
    while m< Length(rev[k][l]) and Length(oo)<Length(scc) do 
      m:=m+1;
      if not seen[rev[k][l][m]] and t[rev[k][l][m]] then 
        Add(oo, rev[k][l][m]); seen[rev[k][l][m]]:=true;
        gen[rev[k][l][m]]:=l; pos[rev[k][l][m]]:=k;
      fi;
    od;
  od;
od;


return [gen, pos];
end);

# new for 3.2!
#############################################################################
#

InstallOtherMethod(Enumerator, "for R-class of trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
local enum, h;

Info(InfoMonoidGreens, 4, "Enumerator: for an R-class");

h:=List(Elements(RClassSchutzGpFromData(r!.parent, r!.data, r!.o)), x-> 
	r!.rep*x);

enum:=EnumeratorByFunctions(r, rec(
	
	rep:=r!.rep,
	
	h:=h, 
	
	len:=Length(h),
	
	p:=RClassPermsFromData(r!.parent, r!.data, r!.o),
	
	scc:=RClassSCCFromData(r!.parent, r!.data, r!.o),
	
	###########################################################################
	
	ElementNumber:=function(enum, pos)
	local q, n, m;
		if pos>Length(enum) then 
			return fail;
		fi;
		
		if pos<=enum!.len then 
			return enum!.h[pos];
		fi;
		
		n:=pos-1;
		m:=enum!.len;
		
    q := QuoInt(n, m);
    pos:= [ q, n - q * m ]+1;

		return enum!.h[pos[2]]*enum!.p[enum!.scc[pos[1]]]^-1;
	end, 
	
	###########################################################################
	
	NumberElement:=function(enum, f)
		local rep, d, s, o, i, j;
		rep:=enum!.rep;
	
		if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or
		 RankOfTransformation(f) <> RankOfTransformation(rep) or
		 KernelOfTransformation(f) <> KernelOfTransformation(rep) then
			return fail;
		fi;
		
		if f=rep then 
			return 1;
		fi;
		
		d:=r!.data;
		s:=r!.parent;
		
		# check image is in the same weak orbit
		o:=r!.o;#RClassImageOrbitFromData(s, d);
		i:= Position(o, ImageSetOfTransformation(f));
		
		if i = fail or not o!.truth[d[4]][i] then #check they are in the same scc
			return fail;
		fi;
		
		j:= Position(Elements(RClassSchutzGpFromData(s, d, o)),
		 PermLeftQuoTransformationNC(rep, f*o!.perms[i]));
		
		if j = fail then 
			return fail;
		fi;
		
		return Length(enum!.h)*(Position(enum!.scc, i)-1)+j;

	end, 

	###########################################################################
	
	Membership:=function(elm, enum) 
	return elm in UnderlyingCollection(enum); #the R-class itself!
	end,
	
	Length:=enum -> Size(r),

	PrintObj:=function(enum)
	Print( "<enumerator of R-class>");
	return;
	end));

return enum;
end);

#new for 3.2!
#############################################################################

InstallOtherMethod(Enumerator, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local out, iter, i;

Info(InfoMonoidGreens, 4, "Enumerator: for a trans. semigroup");

out:=[];
iter:=Iterator(s);

for i in iter do 
	out[Length(out)+1]:=i;
od;

return Immutable(out);
end);

#new for 3.2!
#############################################################################
# finds all orbits of images!!

InstallGlobalFunction(ExpandOrbitsOfImages, 
function(s)
local o, iter, i;

Info(InfoMonoidGreens, 4, "ExpandOrbitsOfImages");

o:=OrbitsOfImages(s);

if not o!.finished then 
	iter:=IteratorOfRClassRepsData(s);
	iter!.i:=Length(o!.data); 
	# avoids running through those already found.
	for i in iter do od;
fi;

return true;
end);

#new for 3.2!
#############################################################################

InstallGlobalFunction(DisplayOrbitsOfImages, 
function(s)
local o;

o:=OrbitsOfImages(s);

Print("finished: ", o!.finished, "\n");
Print("orbits: "); View(o!.orbits); Print("\n");
Print("at: ", o!.at, "\n");
Print("ht: "); View(o!.ht); Print("\n");
Print("size: ", SizeOrbitsOfImages(s), "\n");
Print("R-classes: ", NrRClassesOrbitsOfImages(s), "\n");
return true;
end);

#new for 3.2!
#############################################################################

InstallGlobalFunction(DisplayOrbitOfImage, 
function(o)

View(o); Print("\n");
Print(o!.scc, "\n");
Print(o!.schutz, "\n");
Print(o!.reps, "\n");

return true;
end);

#new for 4.0!
#############################################################################
# Test efficiency!
# require NC version that stores nothing!


InstallOtherMethod(GreensDClass, "for an R-class of a trans. semigroup", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
return GreensDClassOfElement(r!.parent, r!.rep);
end);

#############################################################################
#

InstallOtherMethod(GreensDClassNC, "for an R-class of a trans. semigroup", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
# not the following here, as we already know the orbit of the image for r
# just need to compute the orbit of the kernel! JDM

return GreensDClassOfElementNC(r!.parent, r!.rep);
end);

# new method in 3.2!
#############################################################################
#

InstallMethod(GreensRClassData, "for a R-class of a trans. semigroup",
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
local rep, d, s, scc, l, o, p, g;

Info(InfoWarning, 1, "this is a legacy from Monoid 3.*");

rep:=r!.rep;
d:=r!.data;
s:=r!.parent;

scc:=RClassSCCFromData(s, d, r!.o);
l:=Position(scc, d[3]);
o:=r!.o{scc}; #RClassImageOrbitFromData(s, d){scc};
p:=RClassPermsFromData(s, d, r!.o){scc};
g:=RClassSchutzGpFromData(s, d, r!.o);

#d[3] is the index of the scc containing rep!
if not l=1 then 
	o:=Concatenation(o{[l..Length(o)]}, o{[1..l-1]});
	p:=List(Concatenation(p{[l..Length(p)]}, 
	p{[1..l-1]}), x-> x*p[l]^-1);
	g:=g^(p[1]^-1);
fi;

return RClassData(rec( rep:=rep, strongorb:=o, 
perms:=p, schutz:=g));;
end);

# new for 3.2!
#############################################################################
# JDM test the efficiency of this function!

InstallMethod(GreensRClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local iter, out, i;

Info(InfoMonoidGreens, 4, "GreensRClasses");

iter:=IteratorOfGreensRClasses(s);
out:=EmptyPlist(Length(OrbitsOfImages(s)!.data));
#JDM is the previous a good idea?

for i in iter do 
	out[Length(out)+1]:=i;
od;

return out;
end);

# new for 3.2!
#############################################################################

InstallOtherMethod(GreensRClassOfElement, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
local d, rep, type, c;

Info(InfoMonoidGreens, 4, "GreensRClassOfElement");

if not f in s then 
	Info(InfoWarning, 1, "transformation is not an element of the semigroup");
	return fail;
fi;

d:=InOrbitsOfImages(s, f)[2];
d[3]:=1;
#rep:=d[7]; not nec. the rep!
rep:=RClassRepFromData(s, d);

type:=NewType( FamilyObj( s ), IsEquivalenceClass and 
	 IsEquivalenceClassDefaultRep and IsGreensRClass and 
	 IsGreensClassOfTransSemigp);

c:=Objectify( type, rec(parent:=s, data:=d{[1..6]}, 
o:=RClassImageOrbitFromData(s, d), rep:=rep));
SetRepresentative(c, rep);
SetEquivalenceClassRelation(c, GreensRRelation(s));
return c;
end);

# new for 3.2!
#############################################################################

InstallOtherMethod(GreensRClassOfElementNC, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
local type, c, o, img, j, scc, reps, d, gens;

Info(InfoMonoidGreens, 4, "GreensRClassOfElementNC");

o:=OrbitsOfImages(s);
d:=InOrbitsOfImages(s, f, o!.orbits, []);

if d[1] then # f in s!
	Info(InfoMonoidGreens, 2, "transformation is an element of semigroup");

	type:=NewType( FamilyObj( s ), IsEquivalenceClass and 
	 IsEquivalenceClassDefaultRep and IsGreensRClass and 
	 IsGreensClassOfTransSemigp);
	
	c:=Objectify( type, rec(parent:=s, data:=d[2]{[1..6]}, 
	o:=RClassImageOrbitFromData(s, d[2]),
	rep:=RClassRepFromData(s, d[2])));
	SetRepresentative(c, c!.rep);
	SetEquivalenceClassRelation(c, GreensRRelation(s));
	return c;

elif o!.finished then #f not in s!
	Info(InfoMonoidGreens, 2, "transformation is not an element of semigroup");
	return fail;
fi;

#don't know if f in s! 
Info(InfoMonoidGreens, 2, "transformation may not be an element of semigroup");

if IsTransformationMonoid( s ) then
	gens := GeneratorsOfMonoid( s );
else
	gens := GeneratorsOfSemigroup( s );
fi;

img:=ImageSetOfTransformation(f);
j:=Length(img);
o:=Orb(s, img, OnSets, rec(
				treehashsize:=NextPrimeInt(Minimum(100000, 
				 3*Binomial(DegreeOfTransformationSemigroup(s), j))), 
				schreier:=true,
				gradingfunc := function(o,x) return Length(x); end, 
				orbitgraph := true, 
				onlygrades:=[j], 
				storenumbers:=true));

Enumerate(o, Binomial(DegreeOfTransformationSemigroup(s), j));
	
#strongly connected components
scc:=First(Set(List(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(List(OrbitGraph(o), Set)), 
 Set)), x-> 1 in x);;
o!.scc:=[scc];
o!.truth:=[BlistList([1..Length(o)], scc)];
o!.trees:=[CreateSchreierTreeOfSCC(o,1)];
reps:=[f];
o!.reps:=List(reps, x->[[x]]); 
o!.kernels_ht:=HashTableForKernels(KernelOfTransformation(f));
o!.perms:=MultipliersOfSCCOfImageOrbit(gens, o, 1);
o!.schutz:=[SchutzGpOfImageOrbit(gens, o, f, 1)];
#JDM schutz_d?

type:=NewType( FamilyObj( s ), IsEquivalenceClass and 
 IsEquivalenceClassDefaultRep and IsGreensRClass and 
 IsGreensClassOfTransSemigp);

c:=Objectify( type, rec(parent:=s, data:=[1,1,1,1,1,1], o:=o, 
rep:=f));
#JDM data ok?

SetRepresentative(c, f);
SetEquivalenceClassRelation(c, GreensRRelation(s));
return c;

end);

# new for 3.2!
#############################################################################

InstallMethod(GreensRClassReps, "for a trans. semigroup", 
[IsTransformationSemigroup], 
function(s)
local iter, i, o;
Info(InfoMonoidGreens, 4, "GreensRClassReps");

ExpandOrbitsOfImages(s);
return List(OrbitsOfImages(s)!.data, x-> 
 RClassRepFromData(s, x));
end);

# new for 3.2!
#############################################################################
# I don't see the need for iterator and enumerator of idempotents, as there
# are just no that many idempotents in general. Or if there are, then 
# we cannot compute the R-class even.... 

InstallOtherMethod( Idempotents, "for a R-class of a trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
local out, ker, rep, n, o, i, img, j;

if HasIsRegularRClass(r) and not IsRegularRClass(r) then 
	return [];
fi;

out:= [];
ker:= KernelOfTransformation(r!.rep);
rep:=r!.rep![1];
n:=Length(Set(rep));
o:=r!.o{RClassSCCFromData(r!.parent, r!.data, r!.o)}; #JDM1

for i in o do
	img:=EmptyPlist(n);
	j:=1;
	while j<=n and POS_LIST_DEFAULT(img, rep[i[j]], 0)=fail do
		img[Length(img)+1]:=rep[i[j]];
		j:=j+1; 
	od;
	
	if j=n+1 then 
		out[Length(out)+1]:=IdempotentNC(ker, i);
	fi;
od;

return out;
end);

#############################################################################

# Usage: s, f, OrbitsOfImages(s)!.orbits, d_img or s, f

InstallGlobalFunction(InOrbitsOfImages, 
function(arg)
local img, j, k, l, m, val,  n, g, schutz, t, reps, s, O, f;

s:=arg[1]; f:=arg[2]; j:=fail;
k:=fail; l:=fail; m:=fail; val:=fail; n:=0; g:=fail;

if Length(arg)=4 then 
	O:=arg[3];
	if not arg[4]=[] then 
		j:=arg[4][1]; 
		k:=arg[4][2]; 
		l:=arg[4][3];
		m:=arg[4][4]; 
		val:=arg[4][5]; 
		n:=arg[4][6];
		g:=arg[4][7];
	fi;

	if k=fail then 
		img:=ImageSetOfTransformation(f);
	fi;
	if j=fail then 
		j:=Length(img);
	fi;
else
	O:=OrbitsOfImages(s)!.orbits;
	img:=ImageSetOfTransformation(f);
	j:=Length(img);
fi;

if not IsBound(O[j]) then
	return [false, [j, fail, fail, fail, fail, 0, fail]];
fi;

if k=fail then #l=fail, m=fail, g=fail
	k:=0;

	repeat
		k:=k+1;
		l:=Position(O[j][k], img);
	until not l=fail or k=Length(O[j]);

	if l = fail then 
		return [false, [j, fail, fail, fail, fail, 0, fail]];
	fi;
	m:=PositionProperty(O[j][k]!.truth, x-> x[l]);
	g:=f*O[j][k]!.perms[l];
	#if Length(O[j][k]!.reps[m])=0 then #this cannot occur, can it? JDM
	#	return [false, [j,k,l,m,fail, 0, g]];
	#fi;
fi;

if val=fail then 
	val:=HTValue(O[j][k]!.kernels_ht[m], KernelOfTransformation(f));
fi;

if val=fail then 
	return [false, [j, k, l, m, fail, 0, g]];
fi;

schutz:=O[j][k]!.schutz[m][1];

if schutz=true then 
	return [true, [j,k,l,m,val,1,g]];
fi;

reps:=O[j][k]!.reps[m][val];
t:=Length(reps);

while n<t do 
	n:=n+1;
	x:=reps[n];
	if SiftedPermutation(schutz, PermLeftQuoTransformationNC(x, g))=() then 
		return [true ,[j,k,l,m,val,n,g]];
	fi;
od;

return [false, [j,k,l,m,val,n,g]];
end);

# new for 3.2!
#############################################################################
# test further for efficiency in comparison to IsRegularTransformation! JDM

InstallMethod(IsRegularRClass, "for an R-class of trans. semigroup",
[IsGreensClassOfTransSemigp], 
function(r)
local f, img, o, i, m;

if not IsGreensRClass(r) then 
	return false;
fi;

if HasIdempotents(r) then 
	return Length(Idempotents(r))>0; 
fi;

return IsRegularRClassData(r!.parent, r!.o, r!.data);
end);

#############################################################################
# the following exists to avoid creating an R-class before check that it is 
# regular!

InstallGlobalFunction(IsRegularRClassData, 
function(s, o, d)
local f, img, m, i;

if HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then 
  return true;
fi;

f:=RClassRepFromData(s, d);
img:= ImageListOfTransformation(f);
m:=Length(ImageSetOfTransformation(f));
o:=o{RClassSCCFromData(s, d, o)};

for i in o do
	if Length(Set(img{i})) = m then
		return true;
	fi;
od;

return false;
end);

# new for 3.2!
#############################################################################
# JDM move to greens.gi when that file is updated!

InstallMethod(Iterator, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "Iterator: for a trans. semigroup");

iter:= IteratorByFunctions( rec( 
	
	R:=IteratorOfGreensRClasses(s), 
	
	r:=fail,
	
	NextIterator:=function(iter)
	
	if IsDoneIterator(iter) then 
		return fail;
	fi;
	
	if iter!.r=fail or IsDoneIterator(iter!.r) then 
		iter!.r:=Iterator(NextIterator(iter!.R));
	fi;
	
	return NextIterator(iter!.r);
	end,
	
	IsDoneIterator:= iter -> IsDoneIterator(iter!.R) and IsDoneIterator(iter!.r),
	
	ShallowCopy:= iter -> rec(R:=IteratorOfGreensRClasses(s), r:=fail)
));

SetIsIteratorOfSemigroup(iter, true);

return iter;
end);

# new for 3.2!
#############################################################################
#

InstallMethod(Iterator, "for a R-class of a trans. semigroup",
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
local iter;

Info(InfoMonoidGreens, 4, "Iterator: for an R-class");

if HasAsSSortedList(r) then 
	iter:=IteratorList(AsSSortedList(r));
else
	iter:=IteratorByFunctions(rec( 
	
	schutz:=List(RClassSchutzGpFromData(r!.parent,  r!.data, r!.o), x-> 
	r!.rep*x),
	
	perms:=RClassPermsFromData(r!.parent, r!.data, r!.o),
	
	scc:=RClassSCCFromData(r!.parent, r!.data, r!.o),
	
	at:=[1,0],
	
	IsDoneIterator:=iter-> iter!.at[1]=Length(iter!.scc) and 
		iter!.at[2]=Length(iter!.schutz),
	
	NextIterator:=function(iter)
	
	if IsDoneIterator(iter) then 
		return fail;
	fi;

	if iter!.at[2]<Length(iter!.schutz) then 
		iter!.at[2]:=iter!.at[2]+1;
	else
		iter!.at[1]:=iter!.at[1]+1; 
		iter!.at[2]:=1;
	fi;
	return iter!.schutz[iter!.at[2]]*iter!.perms[iter!.scc[iter!.at[1]]]^-1;
	end,
	
	ShallowCopy:=iter-> rec(schutz:=iter!.schutz, perms:=iter!.perms, 
	 scc:=iter!.scc, at:=[1,0])
	));
fi;

SetIsIteratorOfRClassElements(iter, true);

return iter;
end);

#############################################################################

InstallOtherMethod(IteratorOfGreensHClasses, "for an R-class", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
Error("not yet implemented!");
end);


# new for 3.2!
#############################################################################

InstallGlobalFunction(IteratorOfGreensRClasses, 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfGreensRClasses");

iter:=IteratorByFunctions( rec(
	
	i:=0,
	
	type:=NewType( FamilyObj( s ), IsEquivalenceClass and 
	 IsEquivalenceClassDefaultRep and IsGreensRClass and 
	 IsGreensClassOfTransSemigp),
	
	s:=s, 
	
	reps:=IteratorOfRClassReps(s),
	
	IsDoneIterator := iter -> IsDoneIterator(iter!.reps), 
	
	NextIterator:= function(iter)
	local c, rep;
	
	rep:=NextIterator(iter!.reps);
	
	if rep=fail then 
		return fail;
	fi;
	
	iter!.i:=iter!.i+1;
	
	#c:=GreensRClassOfElement(iter!.s, rep, type);
	c:=Objectify( iter!.type, rec(parent:=s, 
	 data:=OrbitsOfImages(s)!.data[iter!.i], 
	 o:=RClassImageOrbitFromData(s, OrbitsOfImages(s)!.data[iter!.i]), 
	 rep:=rep));
	SetRepresentative(c, rep);
	SetEquivalenceClassRelation(c, GreensRRelation(s));
	return c; end,
	
	ShallowCopy:=iter-> rec(i:=0, s:=iter!.s, reps:=IteratorOfRClassReps(s))
));

SetIsIteratorOfGreensRClasses(iter, true);
SetUnderlyingSemigroupOfIterator(iter, s);
return iter;
end);

# new for 3.2!
#############################################################################
# not a user function!

InstallGlobalFunction(IteratorOfRClassRepsData, 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfRClassRepsData");

iter:=IteratorByFunctions( rec(
	
	ShallowCopy := iter -> rec( i:=0, s:=iter!.s, 
	last_called := NextIterator, last_value := 0, 
	chooser:=iter!.chooser, next:=iter!.next),
	
	i:=0, # representative index i.e. which representative we are at
	
	s:= s,
	
	next_value := fail,
	
	last_called_by_is_done:=false,
	
	######################################################################
	
	IsDoneIterator:=function(iter)
	local o, ht, gens, i, x, d, y, z, one, O, orbits;
	 
	if iter!.last_called_by_is_done then 
		return iter!.next_value=fail;
	fi;
	
	iter!.last_called_by_is_done:=true;
	
	O:=OrbitsOfImages(s);
	
	iter!.next_value:=fail;
	
	if iter!.i < Length(O!.data) then 
	# we already know this rep
		iter!.i:=iter!.i+1;
		iter!.next_value:=O!.data[iter!.i];
		return false;
	elif O!.finished then  
		return true;
	fi;
	
	ht:=O!.ht;
	o:=ht!.o;
	i:=O!.at;
	
	if i=Length(o) then
	#at the end of the orbit!
		O!.finished:=true;
		return true;
	fi;
	
	gens:=O!.gens;
	orbits:=O!.orbits;
	
	while i<Length(o) do 
		O!.at:=O!.at+1;
		i :=i+1;
		x:=o[i];
		
		d:=InOrbitsOfImages(s, x, orbits, []);

		if not d[1] then #new rep!
			if IsTransformationMonoid(s) or not i = 1 then 
				d:=AddToOrbitsOfImages(s, O, x, d[2]);
				d[3]:=1;
				iter!.i:=iter!.i+1;
				iter!.next_value:=d;
				return false;
			fi;
		fi;
	od;
	
	O!.finished:=true;
	return true;
	end,

	######################################################################

	NextIterator:=function(iter) 
	
	if not iter!.last_called_by_is_done then 
		IsDoneIterator(iter);
	fi;

	iter!.last_called_by_is_done:=false;
	return iter!.next_value;
	end
	
	######################################################################
));

SetIsIteratorOfRClassRepsData(iter, true);

return iter;
end);

#############################################################################
#

#InstallMethod(IteratorOfRClassReps, "for a transformation semigroup", 
#[IsTransformationSemigroup], 

InstallGlobalFunction(IteratorOfRClassReps,
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfRClassReps");

iter:=IteratorByFunctions( rec(

	s:=s,
	
	data:=IteratorOfRClassRepsData(s),
	
	IsDoneIterator := iter-> IsDoneIterator(iter!.data),
	
	NextIterator := function(iter)
	if not IsDoneIterator(iter!.data) then 
		return RClassRepFromData(iter!.s, NextIterator(iter!.data));
	fi;
	return fail; end,
	
	ShallowCopy := iter -> rec( data:=IteratorOfRClassRepsData(
	iter!.s))
));

SetIsIteratorOfRClassReps(iter, true);
SetUnderlyingSemigroupOfIterator(iter, s);

return iter;
end);

# new for 3.2!
#############################################################################
# <j> is the index of the scc we are computing the multipliers for!

InstallGlobalFunction(MultipliersOfSCCOfImageOrbit,
function(gens, o, j)
local i, p, f, scc, schreier;

#p:=o!.perms;
p:=EmptyPlist(Length(o));
scc:=o!.scc[j];

#for i in scc do
#	f:=EvaluateWord(gens, TraceSchreierTreeOfSCCBack(o, j, i));
#	p[i]:=PermList(MappingPermListList(o[i], OnTuples(o[i], f)));
#od; JDM this works also!

for i in scc do
	f:=EvaluateWord(gens, TraceSchreierTreeOfSCCForward(o, j, i)); 
	p[i]:=MappingPermListList(OnTuples(o[scc[1]], f), o[scc[1]]);
od;

return p;
end);

# new for 3.2!
#############################################################################

InstallMethod(NrGreensRClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)

Info(InfoMonoidGreens, 4, "NrGreensRClasses");

ExpandOrbitsOfImages(s);
return NrRClassesOrbitsOfImages(s);
end);


# new for 3.2!
#############################################################################

InstallGlobalFunction(NrRClassesOrbitsOfImages,
function(s)
local i, j, k, l, m, c;

#Info(InfoMonoidGreens, 4, "NrRClassesOrbitsOfImages");

c:=OrbitsOfImages(s);
m:=[];

c:=c!.orbits;

for i in c do
  for j in i do 
    for k in j!.reps do 
      for l in k do 
        Add(m, Length(l));
      od;
    od;
  od;
od;

m:=Sum(m);

if OrbitsOfImages(s)!.finished then 
	SetNrGreensRClasses(s, m);
fi;
return m;
end);

# new for 3.2!
#############################################################################

InstallMethod(OrbitsOfImages, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(s)
local gens, n, one, ht, i;

if IsTransformationMonoid( s ) then
	gens := GeneratorsOfMonoid( s );
else
	gens := GeneratorsOfSemigroup( s );
fi;

n := DegreeOfTransformationSemigroup( s );
one := TransformationNC( [ 1 .. n ] );
ht := HTCreate(one);
HTAdd(ht, one, true);
for i in gens do 
	HTAdd(ht, i, true);
od;

ht!.o := Concatenation([one], gens); 

return rec(
  finished:=false,
  orbits:=EmptyPlist(DegreeOfTransformationSemigroup(s)), 
  at:=0, 
  gens:=gens,
  s:=s,
	deg := n,
	one := one,
	ht:=ht,
	data:=[], 
);
end);

# new for 3.2!
############################################################################

InstallMethod(ParentAttr, "for a R-class of a trans. semigroup", 
[IsGreensRClass and IsGreensClassOfTransSemigp], x-> x!.parent);

# new for 3.2!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassRepsData], 
function(iter)
local O, s;

s:=iter!.s;
O:=OrbitsOfImages(s);

Print( "<iterator of R-class reps data, ", Length(O!.ht!.o), " candidates, ", 
 SizeOrbitsOfImages(s), " elements, ", NrRClassesOrbitsOfImages(s), 
 " R-classes>");
return;
end);

# new for 3.2!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassReps], 
function(iter)
local O, s;

s:=UnderlyingSemigroupOfIterator(iter);
O:=OrbitsOfImages(s);

Print( "<iterator of R-class reps, ", Length(O!.ht!.o), " candidates, ", 
 SizeOrbitsOfImages(s), " elements, ", NrRClassesOrbitsOfImages(s), 
 " R-classes>");
return;
end);

#############################################################################
# keep here

InstallMethod( PrintObj, "for R-class data",
[ IsGreensRClassData and IsGreensRClassDataRep],
function( obj )
Print( "GreensRClassData( ", obj!.rep,  " )" );
end );

# new for 3.2!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfGreensRClasses], 
function(iter)
Print( "<iterator of R-classes>");
return;
end);

# new for 3.2!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfSemigroup], 
function(iter)
Print("<iterator of transformation semigroup>");
return;
end);

# new for 3.2!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassElements], 
function(iter)
Print("<iterator of R-class>");
return;
end);

###########################################################################
#JDM do not recreate the family and type every time here?

InstallGlobalFunction(RClassData, function(list)
return Objectify(NewType(NewFamily("Green's R-class data", IsGreensRClassData), 
IsGreensRClassData and IsGreensRClassDataRep), list);
end);

# new for 3.2!
############################################################################

InstallGlobalFunction(RClassImageOrbitFromData,
function(s, d)
#Info(InfoMonoidGreens, 4, "RClassImageOrbitFromData");

return OrbitsOfImages(s)!.orbits[d[1]][d[2]];
end);

# new for 3.2!
############################################################################

InstallGlobalFunction(RClassRepFromData,
function(s, d)
#Info(InfoMonoidGreens, 4, "RClassRepFromData");
return OrbitsOfImages(s)!.orbits[d[1]][d[2]]!.reps[d[4]][d[5]][d[6]];
end);

# new for 3.2!
############################################################################
# take care using the following, it returns all of the perms for the weak orbit

InstallGlobalFunction(RClassPermsFromData, 
function(arg)
local s, o, d;
#Info(InfoMonoidGreens, 4, "RClassSCCFromData");

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3];
else
	o:=OrbitsOfImages(s)!.orbits[d[1]][d[2]];
fi;

return o!.perms;
end);

# new for 3.2!
############################################################################

InstallGlobalFunction(RClassSCCFromData,
function(arg)
local s, o, d;
#Info(InfoMonoidGreens, 4, "RClassSCCFromData");

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3];
else
	o:=OrbitsOfImages(s)!.orbits[d[1]][d[2]];
fi;

return o!.scc[d[4]];
end);

# new for 3.2!
############################################################################

InstallGlobalFunction(RClassSchutzGpFromData, 
function(arg)
local s, o, d;
#Info(InfoMonoidGreens, 4, "RClassSCCFromData");

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3];
else
	o:=OrbitsOfImages(s)!.orbits[d[1]][d[2]];
fi;

return o!.schutz[d[4]][2];
end);

# new for 3.2!
############################################################################

InstallGlobalFunction(RClassStabChainFromData, 
function(s, o, d)
return o!.schutz[d[4]][1];
end);

# new for 3.2!
############################################################################

InstallOtherMethod(SchutzenbergerGroup, "for a R-class of a trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
local s, d, o;

#Info(InfoMonoidGreens, 4, "SchutzenbergerGroup");

s:=r!.parent;
d:=r!.data;
o:=r!.o; 

return RClassSchutzGpFromData(s, d, o)^(o!.perms[d[3]]^-1);
# replace the above with function calls? No!
end);


# new for 3.2!
#############################################################################
#

# gens are the generators of the semigroup
# o is orbit
# f is a representative of scc with index k
# k is the index of scc containing index of image of f

InstallGlobalFunction(SchutzGpOfImageOrbit,
function(gens, o, f, k) 
local p, t, g, bound, graph, i, j, scc, is_sym;

#Info(InfoMonoidGreens, 4, "SchutzGpOfImageOrbit");

scc:=o!.scc[k];

if Length(o[scc[1]])<1000 then 
	bound:=Factorial(Length(o[scc[1]]));
else
	bound:=infinity;
fi;

g:=Group(());
p:=o!.perms;
t:=o!.truth;
graph:=OrbitGraph(o);
is_sym:=false;

for i in scc do 
	for j in [1..Length(gens)] do 
		if IsBound(graph[i][j]) and t[k][graph[i][j]] then
  		g:=ClosureGroup(g, PermLeftQuoTransformationNC(f, f/p[i] *
  		 (gens[j]*p[graph[i][j]])));
		fi; #keep track of schreier gens here!
		if Size(g)>=bound then 
		  is_sym:=true;
			break;
		fi;
	od;
	if Size(g)>=bound then 
		break;
	fi;
od;

if not is_sym then 
	return [StabChainImmutable(g), g];
else
	return [is_sym, g];
fi;
end);

# new for 3.2!
#############################################################################
##  Algorithm C. 

InstallOtherMethod(Size, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
local s, d, o;

Info(InfoMonoidGreens, 4, "Size: for an R-class");

d:=r!.data;
s:=r!.parent;
o:=r!.o;

return Size(RClassSchutzGpFromData(s, d, o))*Length(RClassSCCFromData(s, d, o));
end);

#JDM really require a RClassSizeFromData command that is used here?

#############################################################################
##  Algorithm V.

InstallMethod(Size, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local iter, o, i;

Info(InfoMonoidGreens, 4, "Size: for a trans. semigroup");

ExpandOrbitsOfImages(s);
return SizeOrbitsOfImages(s);
end);

# new for 3.2!
#############################################################################
# returns the size of the semigroup so far

InstallGlobalFunction(SizeOrbitsOfImages, 
function(s)
local i, o, j, c;
i:=0;

#Info(InfoMonoidGreens, 4, "SizeOrbitsOfImages");

c:=OrbitsOfImages(s)!.orbits;

for o in Concatenation(Compacted(c)) do 
  for j in [1..Length(o!.scc)] do
    if IsBound(o!.schutz[j]) and IsBound(o!.reps[j]) and IsBound(o!.scc[j]) then 
      i:=i+Size(o!.schutz[j][2])*
      Sum(List(o!.reps[j]), Length)*Length(o!.scc[j]);
    fi;
  od;
od;

if OrbitsOfImages(s)!.finished then 
	SetSize(s, i);
fi;

return i;
end);

# new for 3.2!
#############################################################################
# returns a word in generators that takes o!.scc[i][1] to o[j] assuming that
# j in scc[i]

InstallGlobalFunction(TraceSchreierTreeOfSCCForward, 
function( o, i, j)
local word;

#Info(InfoMonoidGreens, 4, "TraceSchreierTreeOfSCCForward");

word := [];
while j > o!.scc[i][1] do
	Add(word, o!.trees[i][1][j]);
	j := o!.trees[i][2][j];
od;
return Reversed(word);
end );

# new for 3.2!
#############################################################################

InstallGlobalFunction(TraceSchreierTreeOfSCCBack,
function( o, i, j)
local word;

#Info(InfoMonoidGreens, 4, "TraceSchreierTreeOfSCCBack");

word := [];
while j > o!.scc[i][1] do
	Add(word, o!.reverse[i][1][j]);
	j := o!.reverse[i][2][j];
od;
return word;
end);

#############################################################################
# keep here

InstallMethod( ViewObj, "for Green's R-class data",
[IsGreensRClassData and IsGreensRClassDataRep],
function( obj )
Print( "GreensRClassData( ", obj!.rep, ", ", obj!.strongorb,", ", obj!.perms,
", ", obj!.schutz, " )" );
end );

#############################################################################
# DELETE!

CheckSchreierWords:=function(o)
local scc, words, reps, i, j, k, f, gens;
gens:=o!.gens;
scc:=o!.scc;
words:=o!.words;
reps:=o!.reps;

for i in [1..Length(scc)] do 
  for j in [1..Length(words[i])] do #words related to scc[i]
    for k in [1..Length(words[i][j])] do #words of reps of scc[i] with a given kernel
			#f:=EvaluateWord(gens, words[i][j][k]);
			#if not KernelOfTransformationNC(f)=KernelOfTransformationNC(reps[i][j][k]) 
			# or not Set(f![1])=Set(reps[i][j][k]![1]) then 
			if not EvaluateWord(gens, words[i][j][k])=reps[i][j][k] then 
				return [i,j,k];
			fi;
    od;
  od;
od;

return true;
end;

#############################################################################

ConvertToOldStyle:=function(c)
local out, i, j, k;

out:=[[],[],[],[]];

for i in c do 
  for j in i do 
    for k in [1..Length(j!.scc)] do 
      if IsBound(j!.reps[k][1]) then
				Add(out[1], j{j!.scc[k]});
				Add(out[2], j!.perms{j!.scc[k]});
				Add(out[3], j!.schutz[k][2]);
				Add(out[4], j!.scc[k]);
			fi;
    od;
  od;
od;

return out;
end;
