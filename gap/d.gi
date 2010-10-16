#############################################################################
##
#W  d.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$

#############################################################################
## Notes

# - remove info statements from functions that are called many many times!

# -  called from
#LClassRepFromData( s, d[2] ) called from
#DClassRepFromData( s, [ d_img, d_ker ] ) called from
#GreensDClassOfElement( semi, elm ) called from
#ImagesElm( rel, rep ) called from
#Enumerator( coll ) called from

# when run on a D-class ImagesElm should work!

# - MNMN is IsTransversal a candidate for a C function? Maybe even IdempotentNC

# - is an iterator/aslist method required for D-classes!? maybe not.

##
#############################################################################

#############################################################################

InstallMethod( \=, "for D-class and D-class of trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp, IsGreensDClass and 
IsGreensClassOfTransSemigp],
function(d1, d2)

return d1!.parent=d2!.parent and d1!.rep in d2;
end);

#############################################################################

InstallOtherMethod(\in, "for trans. and D-class of trans. semigp.",
[IsTransformation, IsGreensDClass and IsGreensClassOfTransSemigp],
function(f, d)
local s, rep, o, i, schutz, p, cosets;

s:=d!.parent;
rep:=d!.rep; 

if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or
 RankOfTransformation(f) <> RankOfTransformation(rep) then
	return false;
fi;

if f=rep then 
	return true;
fi;

o:=RClassImageOrbitFromData(s, d!.data[1], d!.o[1]);
i:= Position(o, ImageSetOfTransformation(f));

if i = fail or not o!.truth[d!.data[1][4]][i] then 
	return false;
fi;

f:=f*o!.perms[i]; #adjust image of f so that it is equal o[scc[1]]
o:=LClassKernelOrbitFromData(s, d!.data[2], d!.o[2]);
i:=Position(o, KernelOfTransformation(f));

if i = fail or not o!.truth[d!.data[2][4]][i] then 
	return false;
fi;

f:=o!.rels[i][2]*f; #adjust kernel of f so that it is equal o[scc[1]]

schutz:=LClassStabChainFromData(s, d!.data[2], d!.o[2]);

if schutz=true then
	return true;
fi;

p:=KerRightToImgLeftFromData(s, d!.data, d!.o)^-1;
cosets:=DClassRCosetsFromData(s, d!.data, d!.o);
f:= PermLeftQuoTransformationNC(rep, f);

for i in cosets do 
	if SiftedPermutation(schutz, (f/i)^p)=() then 
		return true;
	fi;
od;

return false;
end);

## new for 4.0!
#############################################################################
# s    - the semigroup
# f    - transformation
# data - [image data, kernel data]
# o    - [OrbitsOfImages(s), OrbitsOfKernels(s)]

InstallGlobalFunction(AddToOrbitsOfKernels,
function(arg)
local j, k, l, m, val, n, g, O, gens, reps, schutz, convert, oo, scc, 
 ker, ht, r, deg, bound, d_schutz, treehashsize, s, f, data, o, d, r_reps; 

s:=arg[1]; f:=arg[2]; data:=arg[3]; 

if Length(arg)=4 then 
	o:=arg[4];
else
	o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];
fi;

j:=data[2][1]; 		# ker length
k:=data[2][2]; 		# index of orbit containing ker
l:=data[2][3]; 		# position of ker in O[j][k]
m:=data[2][4]; 		# scc of O[j][k] containing ker
val:=data[2][5]; 	# position of img in O[j][k]!images_ht[m]
n:=data[2][6]; 		# the length of O[j][k]!.reps[m][val]
g:=data[2][7];		# O[j][k]!.rels[l][2]*f
#i:=data[2][8]		# the index of the coset rep. 

O := o[2]!.orbits; gens:=o[1]!.gens; d:=o[2]!.data;

if k = fail then #new ker and l,m,val,n,g=fail

################################################################################
	# don't enter here when s is a d-class!
	
	oo:=ForwardOrbitOfKernel(s, f, fail, gens);
	
	if IsBound(O[j]) then 
		Add(O[j], oo);
	else 
		O[j]:=[oo];
	fi;
	k:=Length(O[j]);
	data:=[data[1], [j, k, 1, 1, 1, 1]];
	
	Add(oo!.r_reps[1][1][1], data[1]);
	Add(oo!.d_schutz[1], [SchutzGpOfDClass(s, data)]);
	
	d[Length(d)+1]:=data;
	
	return data;

################################################################################
	
else #old ker
	reps:=O[j][k]!.reps[m]; 
	convert:=O[j][k]!.convert[m];
	d_schutz:=O[j][k]!.d_schutz[m];
	r_reps:=O[j][k]!.r_reps[m];

	if not Length(reps)=0 then #considered this scc before
		if not val=fail then #old image
			reps[val][n+1]:=g;
			convert[val][n+1]:=AsPermOfKerImg(f);
			data[2]:=[j, k, l, m, val, n+1];
			d_schutz[val][n+1]:=SchutzGpOfDClass(s, data, o);
			r_reps[val][n+1]:=[data[1]];
			d[Length(d)+1]:=data;
		  return data;
		else #new image
			val:=Length(reps)+1;
			reps[val]:=[g];
			data[2]:=[j,k,l,m,val,1];
			convert[val]:=[AsPermOfKerImg(g)];
			d_schutz[val]:=[SchutzGpOfDClass(s, data, o)];
			r_reps[val]:=[[data[1]]];
			d[Length(d)+1]:=data;
			HTAdd(O[j][k]!.images_ht[m], ImageSetOfTransformation( f ), val);
			return data;
		fi;
	else
		#we never considered this scc before!
		O[j][k]!.trees[m]:=CreateSchreierTreeOfSCC(O[j][k], m);
		O[j][k]!.reverse[m]:=CreateReverseSchreierTreeOfSCC(O[j][k], m);
		O[j][k]!.rels:=O[j][k]!.rels
		 +MultipliersOfSCCOfKernelOrbit(gens, O[j][k], m);
		g:=O[j][k]!.rels[l][2]*f;
		O[j][k]!.schutz[m]:=RightSchutzGpOfKerOrbit(gens, O[j][k], g, m);
		O[j][k]!.images_ht[m]:=HashTableForImage(ImageSetOfTransformation(f));
		
		reps[1]:=[g];
		r_reps[1]:=[[data[1]]];
		convert[1]:=[AsPermOfKerImg(g)];
		data[2]:=[j, k, l, m, 1, 1];
		d[Length(d)+1]:=data;
		d_schutz[1]:=[SchutzGpOfDClass(s, data, o)];
		return data;
	fi;
fi;

return fail;
end);

# new for 4.0!
#############################################################################
# this should be removed after the library method for AsSSortedList 
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)!

InstallOtherMethod(AsSSortedList, "for a D-class of trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
Info(InfoMonoidGreens, 4, "AsSSortedList: for a D-class");
return ConstantTimeAccessList(EnumeratorSorted(d));
end);

# new for 4.0!
#############################################################################
#JDM should be s, rep, data, orbit where orbit is optional and the default
# is OrbitsOfImages(s), OrbitsOfKernels(s)

InstallGlobalFunction(CreateDClass, 
function(s, data, orbit, rep)
local d;

data:=List(data, x-> x{[1..6]});

d:=Objectify(DClassType(s), rec(parent:=s, data:=data, 
o:=orbit, rep:=rep));
SetRepresentative(d, rep);
SetEquivalenceClassRelation(d, GreensDRelation(s));
return d;
end);

###########################################################################
# 

InstallGlobalFunction(DClassData, function(list)
return Objectify(NewType(NewFamily("Green's D Class Data", IsGreensDClassData), 
IsGreensDClassData and IsGreensDClassDataRep), list);
end);

#new for 4.0!
###########################################################################

InstallGlobalFunction(DClassLCosetsFromData, 
function(arg)
local s, d, o;

Error("not yet implemented");

s:=arg[1]; d:=arg[2][2];

if Length(arg)=3 then 
	o:=arg[3][2];
else
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

return o!.d_schutz[d[4]][d[5]][d[6]][4]; #JDM currently nothing stored here!
end);

#############################################################################
#

InstallGlobalFunction(DClassOrbitsFromData, 
function(s, d)
return [RClassImageOrbitFromData(s, d[1]), LClassKernelOrbitFromData(s, d[2])];
end);

#############################################################################
#

InstallGlobalFunction(DClassImageOrbit, 
d ->  RClassImageOrbitFromData(d!.parent, d!.data[1], d!.o[1]));

#############################################################################
#

InstallGlobalFunction(DClassKernelOrbit, 
d -> LClassKernelOrbitFromData(d!.parent, d!.data[2], d!.o[2]));

#############################################################################
# s <- semigroup; d <- d!.data ; o <- d!.o

InstallGlobalFunction(DClassRClassRepsDataFromData,
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2][2];

if Length(arg)=3 then 
	o:=arg[3][2]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

if IsBound(o!.r_reps[d[4]][d[5]][d[6]]) then 
	return o!.r_reps[d[4]][d[5]][d[6]];
fi;
return fail;
end);


#new for 4.0!
###########################################################################

InstallMethod(DClassRCosets, "for a D-class of trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
local D;
D:=d!.data[2];
return d!.o[2]!.orbits[D[1]][D[2]]!.d_schutz[D[4]][D[5]][D[6]][3];
end);

#new for 4.0!
###########################################################################

InstallMethod(DClassLCosets, "for a D-class of trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
local schutz;

schutz:=LClassSchutzGp(d);

if Size(schutz)=1 then 
	return [()];
fi;

return RightTransversal(schutz^KerRightToImgLeft(d),
	SchutzenbergerGroup(d));
end);


#new for 4.0!
###########################################################################

InstallGlobalFunction(DClassRCosetsFromData, 
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2][2];

if Length(arg)=3 then 
	o:=arg[3][2]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

return o!.d_schutz[d[4]][d[5]][d[6]][3];
end);

#############################################################################
# returns a trans. with kernel and img in the first positions of their
# scc's. 

InstallGlobalFunction(DClassRepFromData, 
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2][2];

if Length(arg)=3 then 
	o:=arg[3][2]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

return o!.reps[d[4]][d[5]][d[6]];
end);


#############################################################################
#

InstallGlobalFunction(DClassKernelSCC,
d-> LClassSCCFromData(d!.parent, d!.data[2], d!.o[2]));

#new for 4.0!
#############################################################################

InstallGlobalFunction(DClassSchutzGpFromData, 
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2][2];

if Length(arg)=3 then 
	o:=arg[3][2]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

return o!.d_schutz[d[4]][d[5]][d[6]][2];
end);

#new for 4.0!
#############################################################################
#

InstallGlobalFunction(DClassStabChainFromData, 
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2][2];

if Length(arg)=3 then 
	o:=arg[3][2]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

return o!.d_schutz[d[4]][d[5]][d[6]][1];
end);


#############################################################################
# 

InstallMethod(DClassType, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s);

return NewType( FamilyObj( s ), IsEquivalenceClass and 
	 IsEquivalenceClassDefaultRep and IsGreensDClass and 
	 IsGreensClassOfTransSemigp);
end);

#############################################################################
# JDM maybe insert some more info here?

InstallMethod( Display, "for D-class data'",
[ IsGreensDClassData and IsGreensDClassDataRep],
function( obj )
Print( "GreensDClassData: ", obj!.rep,  " )" );
end );


#new for 4.0!
#############################################################################

InstallGlobalFunction(DisplayOrbitsOfKernels, 
function(s)
local o;

o:=OrbitsOfKernels(s);
Print("finished: ", o!.finished, "\n");
Print("orbits: "); View(o!.orbits); Print("\n");
Print("size: ", SizeOrbitsOfKernels(s), "\n");
Print("D-classes: ", Length(o!.data), "\n");
return true;
end);

# new for 4.0!
#############################################################################
# JDM test this function more!

InstallOtherMethod(Enumerator, "for a D-class of trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local enum, h;

Info(InfoMonoidGreens, 4, "Enumerator: for a D-class");

enum:=EnumeratorByFunctions(d, rec(
	
	m:=Size(GreensRClasses(d)[1]),
	
	###########################################################################
	
	ElementNumber:=function(enum, pos)
	local q, n, m, R;
	
	if pos>Length(enum) then 
		return fail;
	fi;
	R:=GreensRClasses(UnderlyingCollection(enum));
	n:=pos-1;
	m:=enum!.m;
	
	q := QuoInt(n, m);
	pos:= [ q, n - q * m ]+1;

	return Enumerator(R[pos[1]])[pos[2]];
	end, 
	
	###########################################################################
	
	NumberElement:=function(enum, f)
	local R, i, j;
	
	R:=GreensRClasses(UnderlyingCollection(enum));
	for i in [1..Length(R)] do 
		j:=Position(Enumerator(R[i]), f);
		if not j=fail then 
			return enum!.m*(i-1)+j;
		fi;
	od;
	return fail;
	
	end, 

	###########################################################################
	
	Membership:=function(elm, enum) 
	return elm in UnderlyingCollection(enum); #the D-class itself!
	end,
	
	Length:=enum -> Length(GreensRClasses(UnderlyingCollection(enum)))
	 *Size(GreensRClasses(UnderlyingCollection(enum))[1]),

	PrintObj:=function(enum)
	Print( "<enumerator of D-class>");
	return;
	end));

return enum;
end);

#new for 4.0!
#############################################################################

InstallGlobalFunction(ExpandOrbitsOfKernels, 
function(s)
local iter, i;
iter:=IteratorOfNewDClassReps(s);
for i in iter do od;
return true;
end);

#new for 4.0!
#############################################################################
# maybe this should take the image data as input also! and then d_schutz should 
# in this function

InstallGlobalFunction(ForwardOrbitOfKernel, 
function(arg)
local s, f, filt, gens, ker, deg, j, bound, treehashsize, o, scc, r;

s:=arg[1]; f:=arg[2];

if Length(arg)>=3 then 
	filt:=arg[3];
else
	filt:=fail;
fi;

if Length(arg)=4 then 
	gens:=arg[4];
else
	gens:=Generators(s);
fi;

ker:=KernelOfTransformation(f);
deg:=DegreeOfTransformationSemigroup(s);
j:=Length(ker);
	
if deg<1000 then 
	bound:=Stirling2(DegreeOfTransformationSemigroup(s), j);
	treehashsize:=3*bound;
else
	bound:=infinity;
	treehashsize:=100000;
fi;
	
o:=Orb(s, ker, OnKernelsAntiAction, rec(
				treehashsize:=NextPrimeInt(Minimum(100000, treehashsize)), 
				schreier:=true,
				gradingfunc := function(o,x) return Length(x); end, 
				orbitgraph := true, 
				onlygrades:=[j], 
				storenumbers:=true));
SetIsMonoidPkgImgKerOrbit(o, true);
Enumerate(o, bound);

scc:=Set(List(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(List(OrbitGraph(o), 
	Set)), Set));;
#JDM use OrbitGraphAsSets to simplify the previous line!

if not filt=fail then 
	scc:=Filtered(scc, x-> filt(o,x));
fi;

r:=Length(scc);
o!.scc:=scc;

#boolean list corresponding to membership in scc[i]
o!.truth:=List([1..r], i-> BlistList([1..Length(o)], scc[i]));
o!.trees:=EmptyPlist(r);
o!.reverse:=EmptyPlist(r);
o!.trees[1]:=CreateSchreierTreeOfSCC(o,1); 
o!.reverse[1]:=CreateReverseSchreierTreeOfSCC(o,1);

#representatives of L-classes with kernel belonging in scc[i] partitioned 
#according to their kernels
o!.reps:=List([1..r], x-> []);
Add(o!.reps[1], [f]);

o!.r_reps:=List([1..r], x-> []);
Add(o!.r_reps[1], [[]]); 

o!.convert:=List([1..r], x-> []);
Add(o!.convert[1], [AsPermOfKerImg(f)]);

#images of representatives of L-classes with kernel belonging in scc[i]
o!.images_ht:=[HashTableForImage(ImageSetOfTransformation(f))];

#multipliers of scc containing the kernel of f
o!.rels:=EmptyPlist(Length(o));
o!.rels:=o!.rels+MultipliersOfSCCOfKernelOrbit(gens, o, 1);
	
#schutzenberger group
o!.schutz:=EmptyPlist(r);
o!.schutz[1]:=RightSchutzGpOfKerOrbit(gens, o, f, 1);

o!.d_schutz:=List([1..r], x-> []);

return o;
end);

#new for 4.0!
#############################################################################

InstallOtherMethod(GreensDClass, "for an R-class of a trans. semigroup", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
local s, f, d, data, j, o1, o2;

s:=r!.parent; f:=r!.rep;
d:=InOrbitsOfKernels(s, f);

if d[1] or d[2] then # f in s!
	data:=[d[3][1]]; 
	Info(InfoMonoidGreens, 2, "transformation is an element of the semigroup");
	if d[2] then # f in existing D-class
		data[2]:=d[3][2];
	else #f not in existing D-class
		data:=AddToOrbitsOfKernels(s, f, d[3]);
	fi;
	
	return CreateDClass(s, data, [OrbitsOfImages(s), 
	OrbitsOfKernels(s)], DClassRepFromData(s, data));
elif OrbitsOfImages(s)!.finished then #f not in s!
	Info(InfoMonoidGreens, 2, "transformation is not an element of the ",
	 "semigroup");
	return fail;
fi;

Info(InfoMonoidGreens, 2, "transformation may not be an element of the ",
 "semigroup");

j:=Length(ImageSetOfTransformation(f));

o1:=r!.o;

Info(InfoMonoidGreens, 2, "finding orbit of kernel...");
o2:=[];
o2[j]:=[ForwardOrbitOfKernel(s, f, function(o, scc) return scc[1]=1; end)];

d:=[j,1,1,1,1,1];
o2:=rec(gens:=Generators(s), orbits:=o2, data:=[]);

Info(InfoMonoidGreens, 2, "finding the Schutzenberger group...");
Add(o2!.orbits[j][1]!.d_schutz[1], 
 [SchutzGpOfDClass(s, [r!.data, d], [o1, o2])]);

return CreateDClass(s, [r!.data, d], [o1, o2], f);
end);

#new for 4.0!
#############################################################################
# JDM test!

InstallOtherMethod(GreensDClass, "for an L-class of a trans. semigroup", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
local s, d, o, rep;

s:=l!.parent;
d:=l!.data{[1,2]};
o:=l!.o;
rep:=LClassRepFromData(s, Concatenation(d, [[1,1]]), o);

return CreateDClass(s, d, o, rep);
end);

# new for 4.0!
#############################################################################
# JDM test the efficiency of this function!

InstallMethod(GreensDClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local iter, out, i;

iter:=IteratorOfGreensDClasses(s);
out:=EmptyPlist(Length(OrbitsOfKernels(s)!.data));
#JDM is the previous a good idea?

for i in iter do 
	out[Length(out)+1]:=i;
od;

return out;
end);



# new for 4.0!
#############################################################################

InstallOtherMethod(GreensDClassOfElement, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
local d, D;

if not f in s then 
	Info(InfoWarning, 1, "transformation is not an element of the semigroup");
	return fail;
fi;

d:=InOrbitsOfKernels(s, f);

if not d[2] then #orbit of kernel not previously calculated!
	#d[3][1][3]:=RClassSCCFromData(s, d[3][1])[1]; JDM this line required no?
	d:=AddToOrbitsOfKernels(s, d[3][1][7], d[3]); 
	 #d[3][1][7] is f with rectified image!
	D:=OrbitsOfKernels(s)!.data;
	D[Length(D)+1]:=List(d, x-> x{[1..6]});
else
	d:=d[3];
fi;

d:=CreateDClass(s, d, [OrbitsOfImages(s), OrbitsOfKernels(s)], 
 DClassRepFromData(s, d));

return d;
end);

# new for 4.0!
#############################################################################
# the first part below should look very much like GreensDClassOfElement JDM
# for some reason it does not...

InstallOtherMethod(GreensDClassOfElementNC, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
local d, o1, o2, j, data;

Info(InfoMonoidGreens, 4, "GreensDClassOfElementNC");

d:=InOrbitsOfKernels(s, f);

if d[1] then 
	data:=[d[3][1]];
	Info(InfoMonoidGreens, 2, "transformation is an element of the semigroup");
	if d[2] then 
		data[2]:=d[3][2];
	else
		data:=AddToOrbitsOfKernels(s, f, d[3]);
	fi;
	
	return CreateDClass(s, data, [OrbitsOfImages(s), 
	OrbitsOfKernels(s)], DClassRepFromData(s, data));
elif OrbitsOfImages(s)!.finished then #f not in s!
	Info(InfoMonoidGreens, 2, "transformation is not an element of the ",
	 "semigroup");
	return fail;
fi;

Info(InfoMonoidGreens, 2, "transformation may not be an element of the ",
 "semigroup");

j:=Length(ImageSetOfTransformation(f));

Info(InfoMonoidGreens, 2, "finding orbit of image...");
o1:=[];
o1[j]:=[ForwardOrbitOfImage(s, f)[1]];
Info(InfoMonoidGreens, 2, "finding orbit of kernel...");
o2:=[];
o2[j]:=[ForwardOrbitOfKernel(s, f)];

d:=[j,1,1,1,1,1];

o1:=rec( finished:=false, orbits:=o1, gens:=Generators(s), s:=s, 
 deg := DegreeOfTransformationSemigroup(s), data:=[d]);
o2:=rec( orbits:=o2, gens:=Generators(s), data:=[d]);

Info(InfoMonoidGreens, 2, "finding the Schutzenberger group");
Add(o2!.orbits[j][1]!.d_schutz[1], [SchutzGpOfDClass(s, [d,d], [o1, o2])]);

return CreateDClass(s, [d, d], [o1, o2], f);
end);

# new for 4.0!
#############################################################################

InstallMethod(GreensDClassReps, "for a trans. semigroup", 
[IsTransformationSemigroup], 
function(s)

ExpandOrbitsOfKernels(s);
return List(OrbitsOfKernels(s)!.data, x-> DClassRepFromData(s, x));
end);

#############################################################################

InstallOtherMethod(GreensLClassRepsData, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local f, scc, m, out, k, data, i, j;

f:=Representative(d);
scc:=RClassSCC(d);
m:=Length(DClassRCosets(d));

out:=EmptyPlist(Length(scc)*m);
SetNrGreensLClasses(d, Length(scc)*m);

k:=0;
data:=d!.data;

for i in scc do 
	for j in [1..m] do 
		k:=k+1;
		out[k]:=ShallowCopy(data);
		out[k][3]:=[i,j];
	od;
od;

return out;
end);

#############################################################################
# maybe write iterator/enumerator later! JDM

InstallOtherMethod(GreensLClassReps, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local perms, cosets, f, out, i, j, k;

# is the following worth it? JDM 
if HasGreensLClassRepsData(d) then 
	return List(GreensLClassRepsData(d), x-> 
	 LClassRepFromData(d!.parent, x, d!.o));
fi;

perms:=RClassPerms(d){RClassSCC(d)};
cosets:=DClassRCosets(d);
f:=Representative(d);

out:=EmptyPlist(Length(perms)*Length(cosets));
SetNrGreensLClasses(d, Length(perms)*Length(cosets));
k:=0;

for i in perms do 
	for j in cosets do 
		k:=k+1;
		out[k]:=f*(j/i);
	od;
od;

return out;
end);

#############################################################################
# maybe write iterator/enumerator later! JDM

InstallOtherMethod(GreensRClassRepsData, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local f, rels, cosets, j, k, m, out, val, l, a, b, g, data, orbits;

out:=DClassRClassRepsDataFromData(d!.parent, d!.data, d!.o);

if Length(out)=NrGreensRClasses(d) then 
	return out;
fi;

f:=Representative(d);
rels:=LClassRels(d){LClassSCC(d)};;
cosets:=DClassLCosets(d);

j:=d!.data[1][1]; k:=d!.data[1][2]; l:=d!.data[1][3]; m:=d!.data[1][4];
val:=List(rels, x-> HTValue(DClassImageOrbit(d)!.kernels_ht[m], 
 KernelOfTransformation(x[1]*f)));
# maybe the above should be stored or produced in such a way that we don't 
# have to search! 

out:=EmptyPlist(Length(rels)*Length(cosets));
SetNrGreensRClasses(d, Length(rels)*Length(cosets));
orbits:=d!.o[1]!.orbits;

for a in [1..Length(rels)] do
	g:=rels[a][1]*f; 
	for b in [1..Length(cosets)] do 
		g:=g*cosets[b]^-1;
		data:=InOrbitsOfImages(d, g, orbits, [j, k, l, m, val[a], 0]);
		#could do SiftedPermutation directly here, maybe speed things up?
		if not data[1] then 
			data:=AddToOrbitsOfImages(d, g, d!.o[1], data[2]);
		else 
			data:=data[2];
		fi;
		out[Length(out)+1]:=data;
	od;
od;

return out;
end);

#############################################################################

InstallOtherMethod(GreensRClassReps, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local rels, cosets, f, out, i, j, g;

if HasGreensRClassRepsData(d) then 
	return List(GreensRClassRepsData(d), x-> 
	 RClassRepFromData(d!.parent, x, d!.o[1]));
else 
	out:=DClassRClassRepsDataFromData(d!.parent, d!.data, d!.o);
	if not out=fail then
		SetGreensRClassRepsData(d, out);
		return List(GreensRClassRepsData(d), x-> 
		 RClassRepFromData(d!.parent, x, d!.o[1]));
	fi;
fi;

rels:=LClassRels(d){LClassSCC(d)};;
cosets:=DClassLCosets(d);
f:=Representative(d);

out:=EmptyPlist(Length(rels)*Length(cosets));
SetNrGreensRClasses(d, Length(rels)*Length(cosets));

for i in rels do
	g:=i[1]*f;
	for j in cosets do 
		out[Length(out)+1]:=g*j^-1;
	od;
od;

return out;
end);

#############################################################################
#

InstallOtherMethod(GreensLClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local s, o, m, out, i, f, l, data;

s:=d!.parent; o:=d!.o; m:=NrGreensLClasses(d);
out:=EmptyPlist(m); 

for i in [1..m] do 
	data:=GreensLClassRepsData(d)[i]; 
	if HasGreensLClassReps(d) then 
		f:=GreensLClassReps(d)[i];
	else
		f:=LClassRepFromData(s, data, o);
	fi;
	l:=CreateLClass(s, data, o, f);
	SetGreensDClass(l, d);
	out[i]:=l;
od;

return out;
end);

#############################################################################
#

InstallOtherMethod(GreensRClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local s, o, out, f, r, data;

s:=d!.parent; o:=d!.o[1]; 
out:=EmptyPlist(NrGreensRClasses(d)); 

for data in GreensRClassRepsData(d) do 
	f:=RClassRepFromData(s, data, o);
	r:=CreateRClass(s, data, o, f);
	SetGreensDClass(r, d);
	out[Length(out)+1]:=r;#JDM change this line!
od;

return out;
end);

#############################################################################
#

InstallOtherMethod(GreensRClassOfElement, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp, IsTransformation], 
function(d, f)

if not f in d then 
	Info(InfoWarning, 1, "transformation is not an element of the D-class");
	return fail;
fi;

Error("not yet implemented");

end);



#############################################################################
# JDM is this correct? 
# could also try finding the idempotents of one R-class and then multiplying
# them as in GreensRClassReps

InstallOtherMethod( Idempotents, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local ker, n, i, j, k, out, img, m, reps;

if HasIsRegularDClass(d) and not IsRegularDClass(d) then 
	return [];
fi;

ker:=DClassKernelOrbit(d){DClassKernelSCC(d)};
n:=Length(d!.rep![1]);
reps:=[];

for i in [1..Length(ker)] do 
	j:=[1..n];
	for k in [1..Length(ker[i])] do
		j{ker[i][k]}:=List([1..Length(ker[i][k])], x-> k);
	od;
	reps[i]:=j;
od;

out:= [];
img:=RClassImageOrbitFromData(d!.parent, d!.data[1], d!.o[1])
 {RClassSCCFromData(d!.parent, d!.data[1], d!.o[1])};
#replace these as above!

m:=Length(img[1]);

for i in img do 
	for j in [1..Length(ker)] do 
		if Length(Set(reps[j]{i}))=m then 
			out[Length(out)+1]:=IdempotentNC(ker[j], i); #JDM TransformationNC?
		fi;
	od;
od;

return out;
end);

#############################################################################
# Usage: s, f, [d_img, d_ker] or s, f, or  s, f, [d_img, d_ker], 
# [OrbitsOfImages(s), OrbitsOfKernels(s)]

#JDM change the syntax here to that we use OrbitsOfImages!.orbits and 
#OrbitsOfKernels!.orbits as arguments

InstallGlobalFunction(InOrbitsOfKernels, 
function(arg)
local s, f, O, j, k, l, m, val, n, g, d, ker, reps, t, schutz, x, h, cosets, 
 i, p;

s:=arg[1]; f:=arg[2]; j:=fail;
k:=fail; l:=fail; m:=fail; val:=fail; n:=0; g:=fail; i:=fail;

if Length(arg)=4 then 
	O:=arg[4]; 
else 
	O:=[OrbitsOfImages(s), OrbitsOfKernels(s)];
fi;

if Length(arg)>=3 and not arg[3]=[] then 
	j:=arg[3][2][1]; k:=arg[3][2][2]; l:=arg[3][2][3];
	m:=arg[3][2][4]; val:=arg[3][2][5]; n:=arg[3][2][6];
	
	if Length(arg[3])>=7 then 
		g:=arg[4][2][7]; 
	fi;
	
	d:=[true, arg[3][1]];
	
	if k=fail then 
		ker:=KernelOfTransformation(f);
	fi;
	if j=fail then 
		j:=Length(ker);
	fi;
else
	d:=InOrbitsOfImages(s, f, O[1]!.orbits, []); 
	#JDM change the syntax of InOrbitsOfImgs
	if not d[1] then 
		return [d[1], false, [d[2], [j, fail, fail, fail, fail, 0, fail, fail]]];
	fi;
	f:=d[2][7];
	d[2][3]:=RClassSCCFromData(s, d[2], O[1])[1]; #rectify the image!
	ker:=KernelOfTransformation(f);
	j:=Length(ker);
fi;

O:=O[2]!.orbits;

if not IsBound(O[j]) then
	return [d[1], false, [d[2], [j, fail, fail, fail, fail, 0, fail, fail]]];
fi;

if k=fail then #l=fail, m=fail, g=fail
	k:=0;

	repeat
		k:=k+1;
		l:=Position(O[j][k], ker);
	until not l=fail or k=Length(O[j]);

	if l = fail then 
		return [d[1], false, [d[2], [j, fail, fail, fail, fail, 0, fail, fail]]];
	fi;
	m:=PositionProperty(O[j][k]!.truth, x-> x[l]);
	
	if not IsBound(O[j][k]!.rels[l]) then
		return [d[1], false, [d[2], [j,k,l,m,fail, 0, fail, fail]]];
	fi;
	g:=O[j][k]!.rels[l][2]*f;
fi;

if g=fail then 
	g:=O[j][k]!.rels[l][2]*f;
fi;

if val=fail then 
	val:=HTValue(O[j][k]!.images_ht[m], ImageSetOfTransformation(f));
fi;

if val=fail then 
	return [d[1], false, [d[2], [j, k, l, m, fail, 0, g, fail]]];
fi;

reps:=O[j][k]!.reps[m][val];
t:=Length(reps);

schutz:=O[j][k]!.schutz[m][1];
#schutz gp of the L-class

if schutz=true then 
	return [d[1], true, [d[2], [j,k,l,m,val,1,g,1]]]; 
fi;

while n<t do
	n:=n+1;
	p:=O[j][k]!.convert[m][val][n]^-1;
	cosets:=O[j][k]!.d_schutz[m][val][n][3]; #DClassRCosets
	for i in [1..Length(cosets)] do 
		h:=PermLeftQuoTransformationNC(reps[n], g);
		if SiftedPermutation(schutz, (h/cosets[i])^p)=() then 
			return [d[1], true , [d[2], [j,k,l,m,val,n,g,i]]];
		fi;
	od;
od;

return [d[1], false, [d[2], [j,k,l,m,val,n,g,i]]];
end);

#############################################################################
# JDM test further for efficiency in comparison with the old method!

InstallOtherMethod(IsRegularDClass, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)

if HasIdempotents(d) then 
	return not Idempotents(d)=[];
fi;

return IsRegularRClassData(d!.parent, d!.data[1], d!.rep, d!.o[1]);
end);

#############################################################################

InstallGlobalFunction(IteratorOfDClassReps, 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfDClassReps");

iter:=IteratorByFunctions( rec(
	
	ShallowCopy := iter -> rec( i:=0, s:=iter!.s, 
	last_called := NextIterator, last_value := 0, 
	chooser:=iter!.chooser, next:=iter!.next),
	
	i:=0, # representative index i.e. which representative we are at
	
	s:= s,

	next_value:=fail,
	
	last_called_by_is_done:=false,
	
	r:=IteratorOfRClassRepsData(s),
	
	######################################################################

	IsDoneIterator:=function(iter) 
	local s, O, d_img, f, d_ker, d, r_reps;

	if iter!.last_called_by_is_done then 
		return iter!.next_value=fail;
	fi;
	
	iter!.last_called_by_is_done:=true;
	
	s:=iter!.s;
	O:=OrbitsOfKernels(s);

	iter!.next_value:=fail;
	
	if iter!.i < Length(O!.data) then 
		iter!.i:=iter!.i+1;
		iter!.next_value:=DClassRepFromData(s, O!.data[iter!.i]);
		return false;
	elif O!.finished then  
		return true;
	fi;
	
	for d_img in iter!.r do  
		f:=RClassRepFromData(s, d_img);
		d_ker:=InOrbitsOfKernels(s, f, [d_img, 
		 [d_img[1], fail, fail, fail, fail, 0]]);
		if not d_ker[2] then #f not in existing D-class
			d_ker:=AddToOrbitsOfKernels(s, f, d_ker[3]);
			iter!.i:=iter!.i+1;
			iter!.next_value:=DClassRepFromData(s, d_ker);
			return false;
		else #store R-class in kernel orbit/ JDM clean up the following clause
			d:=d_ker[3][2];
			r_reps:=LClassKernelOrbitFromData(s, d, O)!.r_reps[d[4]][d[5]][d[6]];
			r_reps[Length(r_reps)+1]:=d_img;
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

SetIsIteratorOfDClassReps(iter, true);
SetUnderlyingSemigroupOfIterator(iter, s);

return iter;
end);

# new for 4.0!
#############################################################################

InstallGlobalFunction(IteratorOfGreensDClasses, 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfGreensDClasses");

iter:=IteratorByFunctions( rec(
	
	i:=0,
	
	s:=s, 
	
	reps:=IteratorOfDClassReps(s),
	
	IsDoneIterator := iter -> IsDoneIterator(iter!.reps), 
	
	NextIterator:= function(iter)
	local c, rep, d;
	
	rep:=NextIterator(iter!.reps);
	
	if rep=fail then 
		return fail;
	fi;
	
	iter!.i:=iter!.i+1;
	d:=OrbitsOfKernels(iter!.s)!.data[iter!.i];

	return CreateDClass(s, d, [OrbitsOfImages(s), 
	 OrbitsOfKernels(s)], rep);
	end,
	
	ShallowCopy:=iter-> rec(i:=0, s:=iter!.s, reps:=IteratorOfRClassReps(s))
));

SetIsIteratorOfGreensDClasses(iter, true);
SetUnderlyingSemigroupOfIterator(iter, s);
return iter;
end);

# new for 4.0!
###########################################################################
#

InstallGlobalFunction(IteratorOfNewDClassReps, 
function(s)
local o, iter;

o:=OrbitsOfKernels(s);
iter:=IteratorOfDClassReps(s);
iter!.i:=Length(o!.data); 
return iter;
end);

# new for 4.0!
###########################################################################
# permutation converting a perm. of ker. classes to one of img elts

InstallGlobalFunction(KerRightToImgLeftFromData,
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2][2];

if Length(arg)=3 then 
# if IsOrbit(arg[3]) then o:=arg[3]!!! JDM for the sake of convenience!
	o:=arg[3][2]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

return o!.convert[d[4]][d[5]][d[6]];
end);


#############################################################################

InstallMethod(KerRightToImgLeft, "for a D-class of a trans. semigp",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
d-> KerRightToImgLeftFromData(d!.parent, d!.data, d!.o));

#############################################################################
#

InstallGlobalFunction(LeftSchutzGpOfKerOrbit,
function(gens, o, f, k) 
local scc, bound, g, rels, t, graph, is_sym, i, j;

scc:=o!.scc[k];

if Length(o[scc[1]])<1000 then 
	bound:=Factorial(Length(o[scc[1]]));
else
	bound:=infinity;
fi;

g:=Group(());
rels:=o!.rels;
t:=o!.truth;
graph:=OrbitGraph(o);
is_sym:=false;

for i in scc do 
	for j in [1..Length(gens)] do 
		if IsBound(graph[i][j]) and t[k][graph[i][j]] then
  		g:=ClosureGroup(g,  PermLeftQuoTransformationNC(f, 
  		 rels[graph[i][j]][2] * (gens[j] * (rels[i][1] * f)))); 
		fi; 
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

# new for 4.0!
#############################################################################
# JDM check the efficiency of the lines starting that marked with JDM: 
# - could multiply g by some permutation!?
# - take the inverse of f as a binary relation..
# - instead of doing this just find the order of the permutation on o[scc[1]]
#   corresponding to f*g
# - use a while loop...

InstallGlobalFunction(MultipliersOfSCCOfKernelOrbit,
function(gens, o, j)
local rels, scc, i, f, g, k, tup, h;

rels:=EmptyPlist(Length(o));
scc:=o!.scc[j];

for i in scc do
	#reversed used below as we have a left action not right as in R-classes!
	f:=EvaluateWord(gens, Reversed(TraceSchreierTreeOfSCCForward(o, j, i)));
	# OnKernelAntiAction(o[scc[1]], f)=o[i]
	g:=EvaluateWord(gens, Reversed(TraceSchreierTreeOfSCCBack(o, j, i)));
	# OnKernelsAntiAction(o[i], g)=o[scc[1]] 

	tup:=OnTuplesOfSetsAntiAction(OnTuplesOfSetsAntiAction(o[scc[1]], f), g);#JDM
	if not tup=o[scc[1]] then 
		g:=g*(f*g)^(Order(PermListList(tup, o[scc[1]]))-1);
	fi;
	rels[i]:=[f,g];
od;

return rels;
end);

# new for 4.0!
#############################################################################

InstallMethod(NrGreensDClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)

Info(InfoMonoidGreens, 4, "NrGreensDClasses");
ExpandOrbitsOfKernels(s);
return Length(OrbitsOfKernels(s)!.data);
end);


# new for 4.0!
#############################################################################

InstallOtherMethod(NrGreensLClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)

return Length(DClassRCosets(d))*Length(RClassSCCFromData(d!.parent, d!.data[1],
d!.o[1]));
end);

#############################################################################

InstallOtherMethod(NrGreensRClasses, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
local s, f, o, rels, cosets, out, i, j;

if HasGreensRClassReps(d) then 
	return Length(GreensRClassReps(d));
fi;

s:=d!.parent;
f:=d!.rep;
o:=d!.o;
d:=d!.data;

rels:=Length(LClassSCCFromData(s, d[2], o[2]));
cosets:=RightTransversal(LClassSchutzGpFromData(s, d[2], o[2]), 
 DClassSchutzGpFromData(s, d, o)^(KerRightToImgLeftFromData(s, d, o)^-1));
return rels*Length(cosets);
end);

#############################################################################

InstallOtherMethod(NrIdempotents, "for an D-class", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
local ker, n, i, j, k, out, img, m, reps;

if HasIdempotents(d) then 
	return Length(Idempotents(d));
fi;

if HasIsRegularRClass(d) and not IsRegularRClass(d) then 
	return 0;
fi;

ker:=DClassKernelOrbit(d){DClassKernelSCC(d)};
n:=Length(d!.rep![1]);
reps:=[];

for i in [1..Length(ker)] do 
	j:=[1..n];
	for k in [1..Length(ker[i])] do
		j{ker[i][k]}:=List([1..Length(ker[i][k])], x-> k);
	od;
	reps[i]:=j;
od;

out:= 0;
img:=RClassImageOrbitFromData(d!.parent, d!.data[1], d!.o[1])
 {RClassSCCFromData(d!.parent, d!.data[1], d!.o[1])};
#replace these as above! DClassImageOrbit, DClassImageSCC!

m:=Length(img[1]);

for i in img do 
	for j in [1..Length(ker)] do 
		if Length(Set(reps[j]{i}))=m then 
			out:=out+1;
		fi;
	od;
od;

return out;
end);


# new for 4.0!
#############################################################################

InstallMethod(OrbitsOfKernels, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(s)

return Objectify(NewType(FamilyObj(s), IsOrbitsOfKernels), 
	rec(
		s:=s,
	  finished:=false,
	  data:=[],
	  orbits:=EmptyPlist(DegreeOfTransformationSemigroup(s)), 
	  gens:=Generators(s)));
end);

#############################################################################
#

InstallMethod(ParentAttr, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], x-> x!.parent);

#############################################################################
#

InstallMethod(PrintObj, [IsOrbitsOfKernels], 
function(o)
Print("<orbits of kernels; ", 
SizeOrbitsOfKernels(o!.s), " elements; ", Length(o!.data), 
" D-classes>");
end);

#############################################################################
#

InstallMethod(PrintObj, [IsIteratorOfDClassReps], 
function(iter)
local s;

s:=iter!.s;

Print( "<iterator of D-class reps, ", Length(OrbitsOfImages(s)!.data), 
" candidates, ",
 SizeOrbitsOfKernels(s), " elements, ", Length(OrbitsOfKernels(s)!.data), 
 " D-classes>");
return;
end);

# new for 4.0!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfGreensDClasses], 
function(iter)
Print( "<iterator of D-classes>");
return;
end);

# new for 4.0!
#############################################################################
# JDM check this for efficiency!

#gens are the generators of the semigroup
#o is orbit
#f is a representative of scc with index k
#k is the index of scc containing index of image of f

# JDM could make the following shorter by using LeftSchutzGpOfKerOrbit!

InstallGlobalFunction(RightSchutzGpOfKerOrbit,
function(gens, o, f, k) 
local scc, bound, g, rels, t, graph, is_sym, i, j;

scc:=o!.scc[k];

if Length(o[scc[1]])<1000 then 
	bound:=Factorial(Length(o[scc[1]]));
else
	bound:=infinity;
fi;

g:=Group(());
rels:=o!.rels;
t:=o!.truth;
graph:=OrbitGraph(o);
is_sym:=false;

for i in scc do 
	for j in [1..Length(gens)] do 
		if IsBound(graph[i][j]) and t[k][graph[i][j]] then
  		g:=ClosureGroup(g,  PermLeftQuoTransformationNC(f, rels[graph[i][j]][2] * 
  		 (gens[j] * (rels[i][1] * f))));
		fi; 
		if Size(g)>=bound then 
		  is_sym:=true;
			break;
		fi;
	od;
	if Size(g)>=bound then 
		break;
	fi;
od;

g:=g^(AsPermOfKerImg(f)^-1);

if not is_sym then 
	return [StabChainImmutable(g), g];
else
	return [is_sym, g];
fi;
end);

# new for 4.0!
############################################################################

InstallOtherMethod(SchutzenbergerGroup, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local dd;
dd:=d!.data[2];
return LClassKernelOrbitFromData(d!.parent, dd, d!.o[2])!.
 d_schutz[dd[4]][dd[5]][dd[6]][2];
end);


#new for 4.0!
#############################################################################
# JDM MNMN what's the best way of doing this?

InstallGlobalFunction(SchutzGpOfDClass, 
function(arg)
local g, h, p, stab, s, d, o;

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3];
else
	o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];
fi;

g:=RClassSchutzGpFromData(s, d[1], o[1]);

if not Size(g)=1 then 
	h:=LClassStabChainFromData(s, d[2], o[2]);
	p:=KerRightToImgLeftFromData(s, d, o)^-1;
	if not h=true then 
		h:=SubgroupProperty(g, x -> SiftedPermutation(h, x^p)=());
	else
		h:=LClassSchutzGpFromData(s, d[2], o[2]);
		h:=SubgroupProperty(g, x -> x^p in h);
	fi;
else
	h:=g;
fi;

return [StabChainImmutable(h), h , RightTransversal(g, h)];
end);

# new for 4.0!
#############################################################################

InstallOtherMethod(Size, "for a D-class of a trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
local r, l, s, o, D;

s:=d!.parent;
o:=d!.o;
D:=d!.data;

r:=RClassSchutzGpFromData(s, D[1], o[1]);
l:=LClassSchutzGpFromData(s, D[2], o[2]);

return (Size(r)*Length(RClassSCCFromData(s, D[1], o[1]))
*Length(LClassSCCFromData(s, D[2], o[2])))*Size(l)/
Size(SchutzenbergerGroup(d));
end);


#############################################################################
#

InstallGlobalFunction(SizeOrbitsOfKernels, 
function(s)
local data, i, d, l, r, o_r, o_l;

data:=OrbitsOfKernels(s)!.data;
i:=0;

for d in data do
	o_r:=RClassImageOrbitFromData(s, d[1]);
	r:=RClassSchutzGpFromData(s, d[1]);
	o_l:=LClassKernelOrbitFromData(s, d[2]);
	l:=LClassSchutzGpFromData(s, d[2]);
	i:=i+(Size(r)*Length(RClassSCCFromData(s, d[1]))
	 *Length(LClassSCCFromData(s, d[2]))*Size(l)/
	 Size(DClassSchutzGpFromData(s,  d)));
od;

return i;
end);

#############################################################################
# 

InstallMethod( ViewObj, "for D-class data",
[ IsGreensDClassData and IsGreensDClassDataRep],
function( obj )
Print( "GreensDClassData( ", obj!.rep, " )" );
end );
