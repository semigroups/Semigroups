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

# - it can be that when you are doing iterator of orbits of images that 
# you create an orbit that is later contained in another orbit
# this will lead to the later orbit not have all of its reps, etc defined. 
# What should be done about the above comment?

# - consider storing the relevant data for an R-class in that R-class 
#   rather than repeatedly obtaining it from OrbitsOfImages. About 
#   1/3 of the time in finding all the idempotents of all the R-classes
#   is spent in line JDM1. In particular, the scc corresponding to the R-class
#   (the actual images that is).

# - is it true that there are an equal number of reps with any given kernel
#   in a given scc of an orbit? 

NrReps:=function(o)
return List([1..Length(o!.scc)], m->List(o!.reps[m], Length));
end;

# - is it true that there are an equal number of distinct kernels of 
#   representatives of any two scc in the same orbit? Not always. 

NrKernels:=function(o)
return List(o!.reps, Length);
end;

# - does it make sense to actually have \in etc defined using D-classes rather 
#   than R-classes? Check efficiency etc and see if it does...

# - maybe combine OrbitsOfImages and OrbitsOfKernels...

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

return r1!.rep in r2 and r2!.rep in r1;
end);

## new for 3.2!
#############################################################################
##  Algorithm E. 

InstallMethod( \in, "for trans. and R-class of trans. semigp.", 
[IsTransformation, IsGreensRClass and IsGreensClassOfTransSemigp],
function(f, r)
local rep, d, o, i, schutz, s;

Info(InfoMonoidGreens, 4, "\in: for an R-class");

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

# check image is in the same weak orbit
o:=RClassImageOrbitFromData(s, d);
i:= Position(o, ImageSetOfTransformation(f));

if i = fail or not o!.truth[d[4]][i] then #check they are in the same scc
	return false;
fi;

schutz:= RClassSchutzGpFromData(s, d)[1];

return schutz=true or SiftedPermutation(schutz, 
 PermLeftQuoTransformationNC(rep, f*o!.perms[i]))=();
end);


# new for 3.2!
#############################################################################
# not algorithm X.

InstallMethod(\in, "for a transformation semigroup", 
[IsTransformation, IsTransformationSemigroup],
function(f, s)
local gens, g, o, iter;

Info(InfoMonoidGreens, 4, "\in: for a trans. semigroup");

if HasAsSSortedList(s) then 
	return f in AsSSortedList(s);
fi;

gens:=GeneratorsOfSemigroup(s);

if not DegreeOfTransformation(f) = DegreeOfTransformation(gens[1]) then 
	return false;
fi;

o:=OrbitsOfImages(s);
g:=InOrbitsOfImages(o, f);

if g[1] then 
	return true;
elif o!.finished then 
	return false;
fi;

# check what's already known...
iter:=IteratorOfRClassReps(s);
iter!.i:=Length(o!.data); #avoids checking what's already known!

repeat
	NextIterator(iter);
	g:=InOrbitsOfImages(o, f, g[2]);

	if g[1] then 
		return true;
	fi;
until IsDoneIterator(iter);

return false;
end);


# new for 3.2!
#############################################################################
# Algorithm D.

InstallOtherMethod(AsList, "for R-class of trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
local f, s, d, h, elts, p;

Info(InfoMonoidGreens, 4, "AsList: for an R-class");

f:=r!.rep; #rep should have its image at the first place in the scc
s:=r!.parent;
d:=r!.data;

h:=List(RClassSchutzGpFromData(s, d)[2], x-> f*x);
# <h> contains the H-class of the representative of <r> as a subgroup. 

elts:=[];

for p in RClassPermsFromData(s, d){RClassSCCFromData(s, d)} do 
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

Info(InfoMonoidGreens, 4, "CreateSchreierTreeOfSCC");

#if IsBound(o!.trees[i]) then 
#  Error("Schreier tree already created for this scc");
#fi;

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

Info(InfoMonoidGreens, 4, "CreateReverseSchreierTreeOfSCC");

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

h:=List(Elements(RClassSchutzGpFromData(r!.parent, r!.data)[2]), x-> 
	r!.rep*x);

enum:=EnumeratorByFunctions(r, rec(
	
	rep:=r!.rep,
	
	h:=h, 
	
	len:=Length(h),
	
	p:=RClassPermsFromData(r!.parent, r!.data),
	
	scc:=RClassSCCFromData(r!.parent, r!.data),
	
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
		o:=RClassImageOrbitFromData(s, d);
		i:= Position(o, ImageSetOfTransformation(f));
		
		if i = fail or not o!.truth[d[4]][i] then #check they are in the same scc
			return fail;
		fi;
		
		j:= Position(Elements(RClassSchutzGpFromData(s, d)[2]),
		 PermLeftQuoTransformationNC(rep, f*o!.perms[i]));
		
		if j = fail then 
			return fail;
		fi;
		
		return Length(enum!.h)*(Position(enum!.scc, i)-1)+j;

	end, 

	###########################################################################
	
	Membership:=function(elm, enum)
	return elm in UnderlyingCollection(enum);
	end,
	
	Length:=enum -> Size(r),

	PrintObj:=function(enum)
	Print( "<enumerator of R-class of ", r!.rep ,">");
	return;
	end));

return enum;
end);

#new for 3.2!
#############################################################################
# consider what to do here really! In particular, if 
# OrbitsOfImages(s)!.finished=true, but we don't have GreensRClasses, 
# then is the following really the best?

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
	iter:=IteratorOfRClassReps(s);
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

InstallGlobalFunction(ForwardOrbitOfImage, 
function(s, f)
local img, i, o;

Info(InfoMonoidGreens, 4, "ForwardOrbitOfImage");

#img:=Set(f![1]);
img:=ImageSetOfTransformation(f);
o:=OrbitsOfImages(s)!.orbits;

if IsBound(o[Length(img)]) then 
  i:=Position(o[Length(img)], x-> img in x);
else
  i:=fail;
fi;

if not i=fail then 
  return o[i];
fi;

return ForwardOrbitOfImageNC(s, f);
end);

#new for 3.2!
#############################################################################

InstallGlobalFunction(ForwardOrbitOfImageNC, 
#function(arg)
function(s, f)
local img, o, scc, t, i, gens, g, bound, graph, j, schutz, schreier, O;
Info(InfoWarning, 2, "Warning: calling this function more than once with the ",
" same arguments will repeatedly add the returned value to OrbitsOfImages. ",
"Use ForwardOrbitOfImage instead.");

Info(InfoMonoidGreens, 4, "ForwardOrbitOfImageNC");

#s:=arg[1]; f:=arg[2];

#if Length(arg)=3 then 
#  schreier:=arg[3].schreier;
#else 
#  schreier:=false;
#fi;

if IsMonoid(s) then 
	gens:=GeneratorsOfMonoid(s);
else
	gens:=GeneratorsOfSemigroup(s);
fi;

#img:=Set(f![1]);
img:=ImageSetOfTransformation(f);
o:=Orb(s, img, OnSets, rec(
        treehashsize:=NextPrimeInt(Minimum(100000, 
         3*Binomial(DegreeOfTransformationSemigroup(s), Length(img)))), 
        schreier:=true,
        gradingfunc := function(o,x) return Length(x); end, 
        orbitgraph := true, 
        onlygrades:=[Length(img)], 
        storenumbers:=true));
Enumerate(o);

#strongly connected components
scc:=Set(List(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(List(OrbitGraph(o), Set)), 
 Set));;
o!.scc:=scc;

#boolean list corresponding to membership in scc[i]
t:=List([1..Length(scc)], i-> BlistList([1..Length(o)], scc[i]));
o!.truth:=t;

#Schreier trees for strongly connected components
o!.trees:=EmptyPlist(Length(scc));
o!.trees[1]:=CreateSchreierTreeOfSCC(o,1);

#representatives of R-classes with image belonging in scc[i] partitioned 
#according to their kernels
o!.reps:=List([1..Length(scc)], x-> []);
Add(o!.reps[1], [f]);

#if schreier then 
#	o!.words:=List([1..Length(scc)], x-> []);
#	o!.reverse:=EmptyPlist(Length(scc));
#	o!.reverse[1]:=CreateReverseSchreierTreeOfSCC(o,1);
#fi;

#kernels of representatives of R-classes with image belonging in scc[i]
o!.kernels_ht:=[];
Add(o!.kernels_ht, HashTableForKernels(KernelOfTransformation(f)));

#calculate the multipliers and schutzenberger groups for the scc containing
#img. 
scc:=scc[1];

#multipliers of scc containing the image of f
o!.perms:=EmptyPlist(Length(o));
#o!.perms:=MultipliersOfSCCOfImageOrbit(o, 1, rec(schreier:=schreier));
MultipliersOfSCCOfImageOrbit(gens, o, 1);

#schutzenberger group corresponding to scc[1]
o!.schutz:=EmptyPlist(Length(scc));
o!.schutz[1]:=SchutzenbergerGroupOfSCCOfImageOrbit(gens, o, f, 1);

#OrbitsOfImages is partitioned according to image size of the first element in 
# each component!

O:=OrbitsOfImages(s);

if IsBound(O!.orbits[Length(img)]) then 
  Add(O!.orbits[Length(img)], o);
else
  O!.orbits[Length(img)]:=[o];
fi;

O!.data[Length(O!.data)+1]:=[Length(img), Length(O!.orbits[Length(img)]), 
 1, 1, 1, 1];

return o;
end);


# new method in 3.2!
#############################################################################
#

InstallMethod(GreensRClassData, "for a R-class of a trans. semigroup",
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
local rep, d, s, scc, l, o, p, g;

rep:=r!.rep;
d:=r!.data;
s:=r!.parent;

scc:=RClassSCCFromData(s, d);
l:=Position(scc, d[3]);
o:=RClassImageOrbitFromData(s, d){scc};
p:=RClassPermsFromData(s, d){scc};
g:=RClassSchutzGpFromData(s, d)[2];

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

d:=InOrbitsOfImages(OrbitsOfImages(s), f)[2];
d[3]:=1;
rep:=d[7];

type:=NewType( FamilyObj( s ), IsEquivalenceClass and 
	 IsEquivalenceClassDefaultRep and IsGreensRClass and 
	 IsGreensClassOfTransSemigp);

c:=Objectify( type, rec(parent:=s, data:=d{[1..6]}, rep:=rep));
SetRepresentative(c, rep);
SetEquivalenceClassRelation(c, GreensRRelation(s));
return c;
end);

# new for 3.2!
#############################################################################
# would be better if this did not store anything simply created the object...
# Currently this is not possible as all the functions for dealing with 
# R-classes make use of OrbitsOfImages and if this function does not change 
# OrbitsOfImages, then we would not be able to do anything with it after 
# creating it (such as find its elements and so on...).

InstallOtherMethod(GreensRClassOfElementNC, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
local d, rep, type, c;

Info(InfoMonoidGreens, 4, "GreensRClassOfElementNC");

d:=InOrbitsOfImages(OrbitsOfImages(s), f);

if d[1] then # f in s!
	Info(InfoMonoidGreens, 2, f, " is an element of ", s);
	d:=InOrbitsOfImages(OrbitsOfImages(s), f)[2]{[1..6]};
	rep:=RClassRepFromData(s, d);
elif not OrbitsOfImages(s)!.finished and ForAny(d[2], x-> x=fail) then 
#don't know if f in s!
	Info(InfoMonoidGreens, 2, f, " may be an element of ", s);
	ForwardOrbitOfImageNC(s, f);
	d:=[Length(ImageSetOfTransformation(f)), 1, 1, 1, 1, 1];
	rep:=f;
else #f not in s!
	Info(InfoMonoidGreens, 2, f, " is not an element of ", s);
	Info(InfoWarning, 1, "transformation is not an element of the semigroup");
	return fail;
fi;

type:=NewType( FamilyObj( s ), IsEquivalenceClass and 
	 IsEquivalenceClassDefaultRep and IsGreensRClass and 
	 IsGreensClassOfTransSemigp);

c:=Objectify( type, rec(parent:=s, data:=d, rep:=rep));
SetRepresentative(c, rep);
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
return List(OrbitsOfImages(s)!.data, x-> RClassRepFromData(s, x));
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

Info(InfoMonoidGreens, 4, "Idempotents: for a R-class");

out:= [];
ker:= KernelOfTransformation(r!.rep);
rep:=r!.rep![1];
n:=Length(Set(rep));
o:=RClassImageOrbitFromData(r!.parent, r!.data){RClassSCCFromData(r!.parent,
 r!.data)}; #JDM1

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

# new for 3.2!
#############################################################################

InstallGlobalFunction(InOrbitsOfImages, 
function(arg)
local O, img, j, k, l, m, val, schutz, s, f, n, g, gens;

Info(InfoMonoidGreens, 4, "InOrbitsOfImages");

O:=arg[1]!.orbits; f:=arg[2]; gens:=arg[1]!.gens;
img:=ImageSetOfTransformation(f);

if Length(arg)=3 then 
	j:=arg[3][1]; k:=arg[3][2]; l:=arg[3][3];
	m:=arg[3][4]; val:=arg[3][5]; n:=arg[3][6];
	g:=arg[3][7];
else
	j:=Length(img);
	k:=fail; l:=fail; m:=fail; val:=fail; n:=0; g:=fail;
fi;

if not IsBound(O[j]) then
	return [false, [j, fail, fail, fail, fail, 0, fail]];
fi;

if k=fail then
	k:=0;

	repeat
		k:=k+1;
		l:=Position(O[j][k], img);
	until not l=fail or k=Length(O[j]);

	if l = fail then 
		return [false, [j, fail, fail, fail, fail, 0, fail]];
	fi;
	m:=PositionProperty(O[j][k]!.truth, x-> x[l]);
	if not IsBound(O[j][k]!.perms[l]) then #we never considered this scc before! 
		return [false, [j,k,l,m,fail, 0, fail]];
	fi;
fi;

if g=fail and not l=fail and IsBound(O[j][k]!.perms[l]) then 
	g:=f*O[j][k]!.perms[l];
fi; 

if val=fail then 
	if not IsBound(O[j][k]!.kernels_ht[m]) then 
		return [false, [j, k, l, m, fail, 0, g]];
	fi;
	val:=HTValue(O[j][k]!.kernels_ht[m], KernelOfTransformation(f));
	if val=fail then 
  	return [false, [j, k, l, m, fail, 0, g]];
	fi;
fi;

schutz:=O[j][k]!.schutz[m][1];

if schutz=true then 
	return [true, [j,k,l,m,val,1,g]];
fi;

while n<Length(O[j][k]!.reps[m][val]) do 
	n:=n+1;
	x:=O[j][k]!.reps[m][val][n];
	if SiftedPermutation(schutz, PermLeftQuoTransformationNC(x, g))=() then 
		return [true ,[j,k,l,m,val,n,g]];
	fi;
od;

return [false, [j,k,l,m,val,n,g]];
end);

# new for 3.2!
#############################################################################
# this should move somewhere else JDM

InstallMethod(IsSubsemigroup, "for a trans. semigp and trans. semigp",
[IsTransformationSemigroup, IsTransformationSemigroup], 
function(s, t) #is t a subsemigroup of s?

Info(InfoMonoidGreens, 4, "IsSubsemigroup");
return ForAll(GeneratorsOfSemigroup(t), x-> x in s);
end);

# new for 3.2!
#############################################################################
# this could be moved to greens.gi... 
# it could probably be more efficient (and complicated) if 
# lower level things were used than R-classes.
# should AsSSortedList be set after this is complete?

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
	
	schutz:=List(RClassSchutzGpFromData(r!.parent, r!.data)[2], x-> 
	r!.rep*x),
	
	perms:=RClassPermsFromData(r!.parent, r!.data),
	
	scc:=RClassSCCFromData(r!.parent, r!.data),
	
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

# new for 3.2!
#############################################################################
# if we do this until IsDoneIterator then shouldn't we store all the RClasses?
# I guess not. This will make the iterator more complicated and involve
# storing the already produced R-classes somewhere. Better just set 
# GreensRClasses by making one instance of this iterator and running through it.

# if not, then should we provide an EnumeratorOfGreensRClasses command for the
# situation when OrbitsOfImages(s)!.finished=true but GreensRClasses has not
# been defined? Probably not in the first instance. Storing the RClasses only 
# really requires storing their representatives and 6 small ints per class. 
# Hence if we can't store this, then we probably can't reach the end of the 
# iterator in the first place.

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
	 data:=OrbitsOfImages(s)!.data[iter!.i], rep:=rep));
	SetRepresentative(c, rep);
	SetEquivalenceClassRelation(c, GreensRRelation(s));
	return c; end,
	
	ShallowCopy:=iter-> rec(i:=0, s:=iter!.s, reps:=IteratorOfRClassReps(s))
));

SetIsIteratorOfGreensRClasses(iter, true);

return iter;
end);

# new for 3.2!
#############################################################################
# add some more info statements?

InstallGlobalFunction(IteratorOfRClassReps, 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfRClassReps");

iter:=IteratorByFunctions( rec(
			
			IsDoneIterator := iter-> iter!.chooser(iter, IsDoneIterator)=fail,
			
			NextIterator := iter-> iter!.chooser(iter, NextIterator),
			
			ShallowCopy := iter -> rec( i:=0, s:=iter!.s, 
			last_called := NextIterator, last_value := 0, 
			chooser:=iter!.chooser, next:=iter!.next),
			
			i:=0, # in case one iterator is started but not finished, then 
			      # another iterator is started. 
			
			s:= s,
			
			last_called := NextIterator,
				
			last_value := 0,
			
			######################################################################
			
			chooser := function( iter, called_by )
			local o;
			
			if iter!.last_called = IsDoneIterator then 
				iter!.last_called := called_by;
				return iter!.last_value; 
			fi;

			if iter!.last_called = NextIterator then
				iter!.last_called := called_by;
				if iter!.last_value=fail then 
					return fail;
				fi;
				
				o:=OrbitsOfImages(iter!.s);
				
				if iter!.i < Length(o!.data) then 
					# we already know this rep
					iter!.i:=iter!.i+1;
					iter!.last_value:=RClassRepFromData(iter!.s, 
					 o!.data[iter!.i]);
				elif o!.finished then  
					iter!.last_value:=fail;
				else
					# must find a new rep if it exists
					iter!.i:=o!.at;
					repeat 
						iter!.last_value:=iter!.next(iter);
					until not iter!.last_value=false or iter!.last_value=fail;
				fi;
				return iter!.last_value;
			fi;
			
			end,
			
			######################################################################

			next:=function(iter) #reduce number of local variables as some not used JDM
			local O, o, j, img, k, l, m, x, ht, y, val, reps, schutz, new, kernels_ht, 
			z, schreier, report, last_report, n, gens, s, one, i, data, oo;
			
			s := iter!.s;
			oo:=OrbitsOfImages(s);
			gens := oo!.gens;
			n := oo!.deg;
			one := oo!.one;
			ht:= oo!.ht;
			o := ht!.o;
			O := oo!.orbits;
			data := oo!.data;
			
			if iter!.i=Length(o) then 
				oo!.finished:=true;
				return fail;
			fi;
			
			iter!.i:=iter!.i+1;
			oo!.at:=iter!.i;
			i := iter!.i;

			if iter!.i=1 then 
				HTAdd(ht, one, true);
			fi;

			img:=ImageSetOfTransformation(o[i]);
			j:=Length(img);
			new:=false;
			
			#check if img has been seen before
			if IsBound(O[j]) then
				k:=0;
				repeat
					k:=k+1;
					l:=Position(O[j][k], img);
				until not l=fail or k=Length(O[j]);
				
				if l=fail then k:=fail; fi;
			else 
				k:=fail;
			fi;
		
			if k = fail then #img has not been seen before
				new:=true; x:=o[i]; 
				
				if IsTransformationMonoid( s ) or not o[i] = one then
					ForwardOrbitOfImageNC(s, o[i]);
					#scheier words here!
				fi;
				
			else #img has been seen before
				if IsTransformationMonoid( s ) or not o[i] = one then
					
					m:=PositionProperty(O[j][k]!.truth, x-> x[l]); #the scc containing img
					reps:=O[j][k]!.reps[m];
					
					#scheier words here!
					
					if not IsBound(O[j][k]!.perms[l]) then 
						#we never considered this scc before!
						O[j][k]!.trees[m]:=CreateSchreierTreeOfSCC(O[j][k], m);
						
						#schreier words here
						
						#O[j][k]!.perms:=
						MultipliersOfSCCOfImageOrbit(gens, O[j][k], m);
						x:= o[i] * O[j][k]!.perms[l]; #img(x)=O[j][k][scc[m][1]]
						O[j][k]!.schutz[m]:=SchutzenbergerGroupOfSCCOfImageOrbit(gens, 
						 O[j][k], x, m);;
						O[j][k]!.kernels_ht[m]:=
						 HashTableForKernels(KernelOfTransformation( x ));
						reps[Length(reps)+1]:=[x];
						data[Length(data)+1]:=[j, k, l, m, 1, 1];
						new:=true; 
		
					else #we have considered scc before
						kernels_ht:=O[j][k]!.kernels_ht[m];
						x := o[i] * O[j][k]!.perms[l];
						val:=HTValue(kernels_ht, KernelOfTransformation(x));
						
						if not val=fail then #kernel seen before
							schutz:=O[j][k]!.schutz[m][1];
							if not schutz=true and not ForAny(reps[val], y-> 
							 SiftedPermutation(schutz, 
							  PermLeftQuoTransformationNC(y, x))=()) then 
									reps[val][Length(reps[val])+1]:=x;
									data[Length(data)+1]:=[j, k, l, m, val, Length(reps[val])];
									new:=true;
								#schreier words here
							fi;
						else #new kernel
							reps[Length(reps)+1]:=[x];
							data[Length(data)+1]:=[j, k, l, m, Length(reps), 1];
							#schreier words here
							HTAdd(kernels_ht, KernelOfTransformation( x ), 
							 Length(reps));
							new:=true; 
						fi;
					fi;
				fi;
			fi;
			
			if new then #install new pts in the orbit
				
				for y in [1..Length(gens)] do
					z:=gens[y]*x; 
					if HTValue(ht, z)=fail then  
						HTAdd(ht, z, true);
						o[Length(o)+1]:=z;
						#schreier words here
					fi;
				od;
				
				if IsTransformationMonoid( s ) or not o[i] = one then 
					return x;
				fi;
			fi;
			
			return false;
			end
			######################################################################
));

SetIsIteratorOfRClassReps(iter, true);
SetSemigroupOfIteratorOfRClassReps(iter, s);

return iter;
end);

# new for 3.2!
#############################################################################
# <j> is the index of the scc we are computing the multipliers for!

InstallGlobalFunction(MultipliersOfSCCOfImageOrbit,
#function(arg)
function(gens, o, j)
local i, p, f, scc, schreier;

Info(InfoMonoidGreens, 4, "MultipliersOfSCCOfImageOrbit");

#o:=arg[1]; j:=arg[2]; 
#gens:=o!.gens;

#if Length(arg)=3 then 
#  schreier:=arg[3].schreier;
#else
#  schreier:=false;
#fi;

p:=o!.perms;
scc:=o!.scc[j];

#if schreier then 
#	for i in scc do
#		f:=EvaluateWord(gens, TraceSchreierTreeOfSCCBack(o, j, i));
#		p[i]:=PermList(MappingPermListList(o[i], OnTuples(o[i], f)));
#	od;
#else
	for i in scc do
		f:=EvaluateWord(gens, TraceSchreierTreeOfSCCForward(o, j, i)); 
		p[i]:=MappingPermListList(OnTuples(o[scc[1]], f), o[scc[1]]);
	od;
#fi;

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

Info(InfoMonoidGreens, 4, "NrRClassesOrbitsOfImages");

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
# would it be better to have this stored in the (empty) record used to 
# create <s> in SemigroupByGenerators, to avoid method selection? Since
# this is called so so many times it might be worth it... JDM for MN

InstallMethod(OrbitsOfImages, "for a trans. semigroup",
[IsTransformationSemigroup], 
function(s)
local gens, n, one, ht;

Info(InfoMonoidGreens, 4, "OrbitsOfImages");

if IsTransformationMonoid( s ) then
	gens := GeneratorsOfMonoid( s );
else
	gens := GeneratorsOfSemigroup( s );
fi;

n := DegreeOfTransformationSemigroup( s );
one := TransformationNC( [ 1 .. n ] );
ht := HTCreate(one);
ht!.o := [one];

return rec(
  finished:=false,
  orbits:=EmptyPlist(DegreeOfTransformationSemigroup(s)), 
  at:=0, 
  gens:=gens,
  s:=s,
	deg := n,
	one := one,
	ht:=ht,
	data:=[]
);
end);

# new for 3.2!
############################################################################

InstallMethod(ParentAttr, "for a R-class of a trans. semigroup", 
[IsGreensRClass and IsGreensClassOfTransSemigp], x-> x!.parent);

# new for 3.2!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassReps], 
function(iter)
local O, s;

s:=SemigroupOfIteratorOfRClassReps(iter);
O:=OrbitsOfImages(s);

Print( "<iterator of R-class reps, ", Length(O!.ht!.o), " candidates, ", 
 SizeOrbitsOfImages(s), " elements, ", NrRClassesOrbitsOfImages(s), 
 " R-classes>");
return;
end);

# new for 3.2!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfGreensRClasses], 
function(iter)
Print( "<iterator of Green's R-classes>");
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


# new for 3.2!
############################################################################

InstallGlobalFunction(RClassImageOrbitFromData,
function(s, d)
Info(InfoMonoidGreens, 4, "RClassImageOrbitFromData");

return OrbitsOfImages(s)!.orbits[d[1]][d[2]];
end);

# new for 3.2!
############################################################################
# take care using the following, it returns all of the perms for the weak orbit

InstallGlobalFunction(RClassPermsFromData, 
function(s, d)
Info(InfoMonoidGreens, 4, "RClassPermsFromData");

return OrbitsOfImages(s)!.orbits[d[1]][d[2]]!.perms;
end);

# new for 3.2!
############################################################################

InstallGlobalFunction(RClassRepFromData,
function(s, d)
Info(InfoMonoidGreens, 4, "RClassRepFromData");
return OrbitsOfImages(s)!.orbits[d[1]][d[2]]!.reps[d[4]][d[5]][d[6]];
end);

# new for 3.2!
############################################################################

InstallGlobalFunction(RClassSCCFromData,
function(s,d)
Info(InfoMonoidGreens, 4, "RClassSCCFromData");
return OrbitsOfImages(s)!.orbits[d[1]][d[2]]!.scc[d[4]];
end);

# new for 3.2!
############################################################################

InstallGlobalFunction(RClassSchutzGpFromData, 
function(s, d)
Info(InfoMonoidGreens, 4, "RClassSchutzGpFromData");
return OrbitsOfImages(s)!.orbits[d[1]][d[2]]!.schutz[d[4]];
end);

# new for 3.2!
############################################################################

InstallOtherMethod(SchutzenbergerGroup, "for a R-class of a trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
local s, d, o;

Info(InfoMonoidGreens, 4, "SchutzenbergerGroup");

s:=r!.parent;
d:=r!.data;
o:=RClassImageOrbitFromData(s, d);

return RClassSchutzGpFromData(s, d)[2]^(o!.perms[d[3]]^-1);
# replace the above with function calls? No!
end);


# new for 3.2!
#############################################################################
#

#gens are the generators of the semigroup
#o is orbit
#f is a representative of scc with index k
#k is the index of scc containing index of image of f

InstallGlobalFunction(SchutzenbergerGroupOfSCCOfImageOrbit,
function(gens, o, f, k) 
local p, t, g, bound, graph, i, j, scc, is_sym;

Info(InfoMonoidGreens, 4, "SchutzenbergerGroupOfSCCOfImageOrbit");

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
local s, d;

Info(InfoMonoidGreens, 4, "Size: for an R-class");

d:=r!.data;
s:=r!.parent;

return Size(RClassSchutzGpFromData(s, d)[2])*Length(RClassSCCFromData(s,d));
end);

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

Info(InfoMonoidGreens, 4, "SizeOrbitsOfImages");

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

Info(InfoMonoidGreens, 4, "TraceSchreierTreeOfSCCForward");

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

Info(InfoMonoidGreens, 4, "TraceSchreierTreeOfSCCBack");

word := [];
while j > o!.scc[i][1] do
	Add(word, o!.reverse[i][1][j]);
	j := o!.reverse[i][2][j];
od;
return word;
end);

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
