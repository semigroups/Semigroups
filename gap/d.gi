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

# - consolidate and clean up what's here already and do some more testing!

# - check efficiency of IteratorOfDClassReps and standardise the input to 
#   functions here!

# -  called from
#LClassRepFromData( s, d[2] ) called from
#DClassRepFromData( s, [ d_img, d_ker ] ) called from
#GreensDClassOfElement( semi, elm ) called from
#ImagesElm( rel, rep ) called from
#Enumerator( coll ) called from

# when run on a D-class ImagesElm should work!

# - COMBINE DCLASSREPS AND ORBITSOFKERNELS UNDER THE NAME ORBITS OF KERNELS

##
#############################################################################

#############################################################################

InstallMethod( \=, "for D-class of trans. semigp. and D-class of trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp, IsGreensDClass and 
IsGreensClassOfTransSemigp],
function(d1, d2)

return d1!.rep in d2 and d2!.rep in d1;
end);

#############################################################################

InstallOtherMethod( \in, "for trans. and D-class of trans. semigp.",
[IsTransformation, IsGreensDClass and IsGreensClassOfTransSemigp],
function(f, d)
local s, rep, o, i, r_schutz, l_schutz, cosets, p; 

s:=d!.parent;
rep:=DClassRepFromData(s, d!.data); 

if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or
 RankOfTransformation(f) <> RankOfTransformation(rep) then
	return false;
fi;

o:=RClassImageOrbitFromData(s, d!.data[1]);
i:= Position(o, ImageSetOfTransformation(f));

if i = fail or not o!.truth[d!.data[1][4]][i] then 
	return false;
fi;

f:=f*o!.perms[i]; #adjust image of f so that it is equal o[scc[1]]

o:=LClassKernelOrbitFromData(s, d!.data[2]);
i:=Position(o, KernelOfTransformation(f));

if i = fail or not o!.truth[d!.data[2][4]][i] then 
	return false;
fi;

f:=o!.rels[i][2]*f; #adjust kernel of f so that it is equal o[scc[1]]

l_schutz:=LClassStabChainFromData(s, d!.data[2]);

if l_schutz=true then
	return true;
fi;

p:=KerRightToImgLeft(s, d!.data[2])^-1;
f:= PermLeftQuoTransformationNC(rep, f);

if SiftedPermutation(l_schutz, f^p)=() then 
	return true;
fi;

cosets:= List(RcapLSchutzGpCosetsInRFromData(s, d!.data, d!.o), x-> x^p); 

for i in cosets do
	if SiftedPermutation(l_schutz, (f/i)^p)=() then
		return true;
	fi;
od;

return false;
end);

## new for 4.0!
#############################################################################
# s    - the semigroup
# o    - [OrbitsOfImages(s), OrbitsOfKernels(s)]
# f    - transformation
# data - [image data, kernel data]

InstallGlobalFunction(AddToOrbitsOfKernels,
function(s, o, f, data)
local j, k, l, m, val, n, g, O, gens, reps, schutz, convert, oo, scc, 
ker, ht, r, deg, bound, d_schutz; 

j:=data[2][1]; 	#ker length
k:=data[2][2]; 	#index of orbit containing ker
l:=data[2][3]; 	#position of ker in O[j][k]
m:=data[2][4]; 	#scc of O[j][k] containing ker
val:=data[2][5]; #position of img in O[j][k]!images_ht[m]
n:=data[2][6];
g:=data[2][7];

O := o[2]!.orbits; gens:=o[2]!.gens;

if k = fail then #new ker and l,m,val,n,g=fail

################################################################################

	ker:=KernelOfTransformation(f);
	deg:=DegreeOfTransformationSemigroup(s);
	
	if deg<=1000 then 
		bound:=Stirling2(DegreeOfTransformationSemigroup(s), j);
	else
		bound:=infinity;
	fi;
	
	oo:=Orb(s, ker, OnKernelsAntiAction, rec(
					treehashsize:=NextPrimeInt(Minimum(100000, 
					bound)), 
					schreier:=true,
					gradingfunc := function(o,x) return Length(x); end, 
					orbitgraph := true, 
					onlygrades:=[Length(ker)], 
					storenumbers:=true));
					
	Enumerate(oo, bound);
	
	#strongly connected components
	scc:=Set(List(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(List(OrbitGraph(oo), 
		Set)), Set));;
	r:=Length(scc);
	oo!.scc:=scc;
	
	#boolean list corresponding to membership in scc[i]
	oo!.truth:=List([1..r], i-> BlistList([1..Length(oo)], scc[i]));
	
	#Schreier trees for strongly connected components
	oo!.trees:=EmptyPlist(r);
	oo!.reverse:=EmptyPlist(r);
	oo!.trees[1]:=CreateSchreierTreeOfSCC(oo,1); 
	oo!.reverse[1]:=CreateReverseSchreierTreeOfSCC(oo,1);
	
	#representatives of L-classes with image belonging in scc[i] partitioned 
	#according to their kernels
	oo!.reps:=List([1..r], x-> []);
	Add(oo!.reps[1], [f]);
	
	oo!.convert:=List([1..r], x-> []);
	Add(oo!.convert[1], [AsPermOfKerImg(f)]);
	
	#images of representatives of L-classes with kernel belonging in scc[i]
	oo!.images_ht:=[];
	ht := HashTableForImage(ImageSetOfTransformation(f));
	Add(oo!.images_ht, ht);
	
	#multipliers of scc containing the kernel of f
	oo!.rels:=EmptyPlist(Length(oo));
	oo!.rels:=oo!.rels+MultipliersOfSCCOfKernelOrbit(gens, oo, 1);
	
	#schutzenberger group
	oo!.schutz:=EmptyPlist(r);
	oo!.schutz[1]:=RightSchutzGpOfKerOrbit(gens, oo, f, 1);
	
	if IsBound(O[j]) then 
		Add(O[j], oo);
	else
		O[j]:=[oo];
	fi;
	
	oo!.d_schutz:=List([1..r], x-> []);
	o:=[o[1]!.orbits[data[1][1]][data[1][2]], O[j][1]];
	data:=[data[1], [j, Length(O[j]), 1, 1, 1, 1]];
	Add(oo!.d_schutz[1],[RcapLStabChainSchutzGpAndCosetsInR(s, data, o)]);
	
	return data[2];

################################################################################
	
else #old ker
	reps:=O[j][k]!.reps[m]; 
	convert:=O[j][k]!.convert[m];
	d_schutz:=O[j][k]!.d_schutz[m];
	o:=[o[1]!.orbits[data[1][1]][data[1][2]], O[j][k]];
	
	if not Length(reps)=0 then 
		if not val=fail then #old image
			reps[val][n+1]:=g;
			convert[val][n+1]:=AsPermOfKerImg(f);
			data[2]:=[j, k, l, m, val, n+1];
			d_schutz[val][n+1]:=RcapLStabChainSchutzGpAndCosetsInR(s, data, o);
		  return data[2];
		else #new image
			val:=Length(reps)+1;
			reps[val]:=[g];
			data[2]:=[j,k,l,m,val,1];
			convert[val]:=[AsPermOfKerImg(g)];
			d_schutz[val]:=[RcapLStabChainSchutzGpAndCosetsInR(s, data, o)];
			HTAdd(O[j][k]!.images_ht[m], ImageSetOfTransformation( f ), val);
			return data[2];
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
		convert[1]:=[AsPermOfKerImg(g)];
		data[2]:=[j, k, l, m, 1, 1];
		d_schutz[1]:=[RcapLStabChainSchutzGpAndCosetsInR(s, data, o)];
		return data[2];
	fi;
fi;

return fail;
end);

#AsList

###########################################################################
# 

InstallGlobalFunction(DClassData, function(list)
return Objectify(NewType(NewFamily("Green's D Class Data", IsGreensDClassData), 
IsGreensDClassData and IsGreensDClassDataRep), list);
end);

#############################################################################
#

InstallGlobalFunction(DClassOrbitsFromData, 
function(s, d)
return [RClassImageOrbitFromData(s, d[1]), LClassKernelOrbitFromData(s, d[2])];
end);

#############################################################################
# should return a trans. with kernel and img in the first positions of their
# scc's. 

InstallGlobalFunction(DClassRepFromData, 
function(s, d)
return LClassRepFromData(s, d[2]);
end);

#############################################################################

InstallMethod(DClassRepsData, "for a trans. semigroup",
[IsTransformationSemigroup], 
function(s)

return rec(
  finished:=false,
	data:=[], 
);
end);

#new for 4.0!
#############################################################################

InstallGlobalFunction(DisplayOrbitsOfKernels, 
function(s)
local o;

o:=OrbitsOfKernels(s);
Print("orbits: "); View(o!.orbits); Print("\n");
Print("size: ", SizeOrbitsOfKernels(s), "\n");
return true;
end);

#new for 4.0!
#############################################################################

InstallGlobalFunction(ExpandDClassRepsData, 
function(s)
local o, iter, i;

#Info(InfoMonoidGreens, 4, "ExpandDClassRepsData");

o:=DClassRepsData(s);

if not o!.finished then 
	iter:=IteratorOfDClassReps(s);
	iter!.i:=Length(o!.data); 
	# avoids running through those already found.
	for i in iter do od;
fi;

return true;
end);

#############################################################################
# 

InstallMethod(GreensDClassData,  "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
local rep, s, l, r, h, o;

Info(InfoWarning, 1, "this is a legacy from Monoid 3.*");

rep:=d!.rep;
s:=d!.parent;
o:=d!.o[1];
d:=d!.data;

l:=GreensLClassData(GreensLClassOfElement(s, rep));
r:=GreensRClassData(GreensRClassOfElement(s, rep));
h:=GreensHClassData(GreensHClassOfElement(s, rep));

return DClassData(rec( rep:=rep, R:=r, L:=l, H:=h, 
 cosets:=RcapLSchutzGpCosetsInRFromData(s, d, o), schutz:=SchutzenbergerGroup(h)));;
end);

# new for 4.0!
#############################################################################
# JDM test the efficiency of this function!

InstallMethod(GreensDClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local iter, out, i;

#Info(InfoMonoidGreens, 4, "GreensDClasses");

iter:=IteratorOfGreensDClasses(s);
out:=EmptyPlist(Length(DClassRepsData(s)!.data));
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
local d_img, d_ker, rep, type, c, d, o;

#Info(InfoMonoidGreens, 4, "GreensDClassOfElement");

if not f in s then 
	Info(InfoWarning, 1, "transformation is not an element of the semigroup");
	return fail;
fi;

d_img:=InOrbitsOfImages(s, f)[2];
d_img[3]:=RClassSCCFromData(s, d_img)[1];
rep:=RClassRepFromData(s, d_img);
o:=OrbitsOfKernels(s);

d_ker:=InOrbitsOfKernels(s, rep, o!.orbits, 
[d_img[1], fail, fail, fail, fail, 0, fail], d_img);

if not d_ker[1] then #orbit of kernel not previously calculated!
	d_ker:=AddToOrbitsOfKernels(s, [OrbitsOfImages(s), o], rep, [d_img,
	 d_ker[2]]);
	d:=DClassRepsData(s)!.data;
	d[Length(d)+1]:=[d_img{[1..6]}, d_ker];
else
	d_ker:=d_ker[2]{[1..6]};
fi;

rep:=DClassRepFromData(s, [d_img, d_ker]);

type:=NewType( FamilyObj( s ), IsEquivalenceClass and 
	 IsEquivalenceClassDefaultRep and IsGreensDClass and 
	 IsGreensClassOfTransSemigp);

c:=Objectify( type, rec(parent:=s, data:=[d_img, d_ker], 
o:=DClassOrbitsFromData(s, [d_img, d_ker]), rep:=rep));

SetRepresentative(c, rep);
SetEquivalenceClassRelation(c, GreensDRelation(s));
return c;
end);

# new for 4.0!
#############################################################################

InstallOtherMethod(GreensDClassOfElementNC, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
local d, rep, type, c;

Info(InfoMonoidGreens, 4, "GreensDClassOfElementNC");

d:=[InOrbitsOfImages(s, f), InOrbitsOfKernels(s, f)];

if d[1][1] or d[2][1] then # f in s!
	Info(InfoMonoidGreens, 2, "trans. is an element of ", s);
	if d[2][1] then 
		rep:=DClassRepFromData(s, [d[1][2],d[2][2]]);
	else
		d[2]:=AddToOrbitsOfKernels(s, [OrbitsOfImages(s), OrbitsOfKernels(s)], f, 
		 [d[1][2], d[2][2]]);
		rep:=DClassRepFromData(s, [d[1], d[2]]);
	fi;
elif OrbitsOfImages(s)!.finished then #f not in s!
	Info(InfoMonoidGreens, 2, "transformation is not an element of the semigroup");
	return fail;
else 
	Info(InfoMonoidGreens, 2, "transformation may not be an element of the semigroup");
	Error("Not yet implemented!"); #JDM
	d[1]:=AddToOrbitsOfImages(s, OrbitsOfImages(s), f, d[1][2]); 
	d[2]:=AddToOrbitsOfKernels(s, [OrbitsOfImages(s), OrbitsOfKernels(s)], f, 
	 [d[1][2], d[2][2]]);
	rep:=DClassRepFromData(s, [d[1], d[2]]);
fi;

type:=NewType( FamilyObj( s ), IsEquivalenceClass and 
	 IsEquivalenceClassDefaultRep and IsGreensRClass and 
	 IsGreensClassOfTransSemigp);

c:=Objectify( type, rec(parent:=s, data:=d, rep:=rep));
SetRepresentative(c, rep);
SetEquivalenceClassRelation(c, GreensRRelation(s));
return c;
end);


# new for 4.0!
#############################################################################

InstallMethod(GreensDClassReps, "for a trans. semigroup", 
[IsTransformationSemigroup], 
function(s)

ExpandDClassRepsData(s);
return List(DClassRepsData(s)!.data, x-> DClassRepFromData(s, x));
end);

# new for 4.0!
#############################################################################

InstallMethod(GreensLClassReps, "for a D-class", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(s)
local iter, i, o;
Info(InfoMonoidGreens, 4, "GreensLClassReps");
Error("not yet implemented");
end);

#############################################################################
# JDM store coset reps, and check for efficiency!

InstallOtherMethod(GreensRClassReps, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local s, f, o, rels, cosets, out, i, j;

s:=d!.parent;
f:=d!.rep;
o:=d!.o;
d:=d!.data;

rels:=LClassRelsFromData(s, d[2]){LClassSCCFromData(s, d[2])};
cosets:=RightTransversal(LClassSchutzGpFromData(s, d[2]), 
 DClassSchutzGpFromData(s, o[1], d));
out:=[];

for i in rels do 
	for j in cosets do 
		out[Length(out)+1]:=i[1]*f*j;
	od;
od;

return out;
end);


#############################################################################
# Usage: s, f, OrbitsOfKernels(s)!.orbits, d_ker, d_img or s, f

InstallGlobalFunction(InOrbitsOfKernels, 
function(arg)
local s, f, O, j, k, l, m, val, n, g, d, ker, reps, t, schutz, x, h, cosets, 
i, p;

s:=arg[1]; f:=arg[2];

if Length(arg)=5 then 
	O:=arg[3];
	j:=arg[4][1]; k:=arg[4][2]; l:=arg[4][3];
	m:=arg[4][4]; val:=arg[4][5]; n:=arg[4][6];
	g:=arg[4][7]; 
	d:=arg[5];
	if k=fail then 
		ker:=KernelOfTransformation(f);
	fi;
	if j=fail then 
		j:=Length(ker);
	fi;
else
	ker:=KernelOfTransformation(f);
	j:=Length(ker);
	k:=fail; l:=fail; m:=fail; val:=fail; n:=0; g:=fail;
	O:=OrbitsOfKernels(s)!.orbits;
	d:=InOrbitsOfImages(s, f); 
	if not d[1] then 
		return [false, [j, fail, fail, fail, fail, 0, fail]];
	fi; #JDM correct?
	d:=d[2];
fi;

if not IsBound(O[j]) then
	return [false, [j, fail, fail, fail, fail, 0, fail]];
fi;

if k=fail then #l=fail, m=fail, g=fail
	k:=0;

	repeat
		k:=k+1;
		l:=Position(O[j][k], ker);
	until not l=fail or k=Length(O[j]);

	if l = fail then 
		return [false, [j, fail, fail, fail, fail, 0, fail]];
	fi;
	m:=PositionProperty(O[j][k]!.truth, x-> x[l]);
	
	if not IsBound(O[j][k]!.rels[l]) then
		return [false, [j,k,l,m,fail, 0, fail]];
	fi;
	g:=O[j][k]!.rels[l][2]*f;
fi;

if val=fail then 
	val:=HTValue(O[j][k]!.images_ht[m], ImageSetOfTransformation(f));
fi;

if val=fail then 
	return [false, [j, k, l, m, fail, 0, g]];
fi;

reps:=O[j][k]!.reps[m][val];
t:=Length(reps);

schutz:=O[j][k]!.schutz[m][1];
#schutz gp of the L-class

if schutz=true then 
	return [true, [j,k,l,m,val,1,g]]; 
fi;


while n<t do
	n:=n+1;
	p:=O[j][k]!.convert[m][val][n]^-1;
	cosets:=O[j][k]!.d_schutz[m][val][n][3];
	for i in cosets do 
		h:=PermLeftQuoTransformationNC(reps[n], g);	
		if SiftedPermutation(schutz, (h/i)^p)=() then 
			return [true ,[j,k,l,m,val,n,g]];
		fi;
	od;
od;

return [false, [j,k,l,m,val,n,g]];
end);

#############################################################################
# JDM test further for efficiency in comparison with the old method!

InstallOtherMethod(IsRegularDClass, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
d-> IsRegularRClassData(d!.parent, d!.o[1], d!.data[1]));

#############################################################################
# JDM test the below for efficiency
# JDM modify the below as per r.gi! IteratorOfRClassRepsData!

InstallGlobalFunction(IteratorOfDClassReps, 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfDClassReps");

iter:=IteratorByFunctions( rec(
	
	IsDoneIterator := iter-> iter!.chooser(iter, IsDoneIterator)=fail,
	
	NextIterator := iter-> iter!.chooser(iter, NextIterator),
	
	ShallowCopy := iter -> rec( i:=0, s:=iter!.s, 
	last_called := NextIterator, last_value := 0, 
	chooser:=iter!.chooser, next:=iter!.next),
	
	i:=0, # representative index i.e. which representative we are at
	
	s:= s,
	
	r:=IteratorOfRClassRepsData(s), 
	
	last_called := NextIterator,
		
	last_value := 0,
	
	######################################################################
	# get rid of the chooser!! JDM 
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
		
		o:=DClassRepsData(iter!.s);
		
		if iter!.i < Length(o!.data) then 
			# we already know this rep
			iter!.i:=iter!.i+1;
			iter!.last_value:=DClassRepFromData(iter!.s, 
			 o!.data[iter!.i]);
		elif o!.finished then  
			iter!.last_value:=fail;
		else
			# must find a new rep if it exists
			iter!.i:=iter!.i+1;
			repeat 
				iter!.last_value:=iter!.next(iter);
			until not iter!.last_value=false or iter!.last_value=fail;
		fi;
		return iter!.last_value;
	fi;
	
	end,
	
	######################################################################

	next:=function(iter) 
	local f, o, d_img, d_ker, d, s, rels, cosets, i, j, g;

	d_img:=NextIterator(iter!.r);
	s:=iter!.s;
	
	if d_img=fail then 
		DClassRepsData(s)!.finished:=true;
		return fail;
	fi;
	
	f:=RClassRepFromData(s, d_img);
	d_ker:=InOrbitsOfKernels(s, f, OrbitsOfKernels(s)!.orbits, [d_img[1], 
	fail, fail, fail, fail, 0, fail], d_img); 
	
	# R-class reps always have image in the first position of the 
	# scc containing their image. 
	
	if not d_ker[1] then #this is a new D-class rep!
		d_ker:=AddToOrbitsOfKernels(s, [OrbitsOfImages(s), OrbitsOfKernels(s)], f, 
		 [d_img, d_ker[2]]);
		d:=DClassRepsData(s)!.data;
		d[Length(d)+1]:=[d_img, d_ker];

		return DClassRepFromData(s, [d_img, d_ker]);
	fi;
	
	return false;
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
	
	type:=NewType( FamilyObj( s ), IsEquivalenceClass and 
	 IsEquivalenceClassDefaultRep and IsGreensDClass and 
	 IsGreensClassOfTransSemigp),
	
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
	d:=DClassRepsData(iter!.s)!.data[iter!.i];
	
	#c:=GreensDClassOfElement(iter!.s, rep, type);
	c:=Objectify( iter!.type, rec(parent:=s, 
	 data:=d,
	 o:=[RClassImageOrbitFromData(s, d[1]), 
	  LClassKernelOrbitFromData(s, d[2])],
	 rep:=rep));
	SetRepresentative(c, rep);
	SetEquivalenceClassRelation(c, GreensDRelation(s));
	return c; end,
	
	ShallowCopy:=iter-> rec(i:=0, s:=iter!.s, reps:=IteratorOfRClassReps(s))
));

SetIsIteratorOfGreensDClasses(iter, true);
SetUnderlyingSemigroupOfIterator(iter, s);
return iter;
end);

# new for 4.0!
###########################################################################
# permutation converting a perm. of ker. classes to one of img elts

InstallGlobalFunction(KerRightToImgLeft,
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3];
else
	o:=OrbitsOfKernels(s)!.orbits;
fi;

return o[d[1]][d[2]]!.convert[d[4]][d[5]][d[6]];
end);

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
ExpandDClassRepsData(s);
return Length(DClassRepsData(s)!.data);
end);

# new for 4.0!
#############################################################################
# JDM combine with DClassRepsData!

InstallMethod(OrbitsOfKernels, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(s)
local gens;#, n, one, ht;

if IsTransformationMonoid( s ) then
	gens := GeneratorsOfMonoid( s );
else
	gens := GeneratorsOfSemigroup( s );
fi;

return rec(
  orbits:=EmptyPlist(DegreeOfTransformationSemigroup(s)), 
  gens:=gens
);
end);

#############################################################################
#

InstallMethod(ParentAttr, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], x-> x!.parent);

#############################################################################
#

InstallMethod(PrintObj, [IsIteratorOfDClassReps], 
function(iter)
local s;

s:=iter!.s;

Print( "<iterator of D-class reps, ", Length(OrbitsOfImages(s)!.data), 
" candidates, ",
 SizeDClassRepsData(s), " elements, ", Length(DClassRepsData(s)!.data), 
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

#############################################################################
# JDM maybe insert some more info here?

InstallMethod( Display, "for D-class data'",
[ IsGreensDClassData and IsGreensDClassDataRep],
function( obj )
Print( "GreensDClassData: ", obj!.rep,  " )" );
end );


#new for 4.0!
###########################################################################

InstallGlobalFunction(RcapLSchutzGpCosetsInRFromData, 
function(s, d, o)
return o[2]!.d_schutz[d[2][4]][d[2][5]][d[2][6]][3];
end);

#new for 4.0!
#############################################################################

InstallGlobalFunction(RcapLSchutzGpFromData, 
function(s, d, o)

return o[2]!.d_schutz[d[2][4]][d[2][5]][d[2][6]][2];
end);

#new for 4.0!
#############################################################################
#

InstallGlobalFunction(RcapLStabChainFromData, 
function(s, d, o)

return o[2]!.d_schutz[d[2][4]][d[2][5]][d[2][6]][1];
end);

#new for 4.0!
#############################################################################

InstallGlobalFunction(RcapLStabChainSchutzGpAndCosetsInR, 
function(s, d, o)
local g, h, p, stab;

g:=RClassSchutzGpFromData(s, d[1], o[1]);

if not Size(g)=1 then 
	h:=LClassSchutzGpFromData(s, d[2]);
	if not Size(h)=1 then 
		stab:=LClassStabChainFromData(s, d[2]);
		p:=KerRightToImgLeft(s, d[2])^-1;
		if not stab=true then 
			h:=SubgroupProperty(g, x -> SiftedPermutation(stab, x^p)=());
		else
			h:=SubgroupProperty(g, x -> x^p in h);
		fi;
	fi;
else
	h:=g;
fi;

return [StabChainImmutable(h), h , RightTransversal(g, h)];
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
d-> RcapLSchutzGpFromData(d!.parent, d!.data, d!.o));

#JDM is this correct? Compare it to SchutzenbergerGroup of R-class...

# new for 4.0!
#############################################################################

InstallOtherMethod(Size, "for a D-class of a trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
local r, l, s, o;

s:=d!.parent;
o:=d!.o;
d:=d!.data;

r:=RClassSchutzGpFromData(s, d[1], o[1]);
l:=LClassSchutzGpFromData(s, d[2]);

return (Size(r)*Length(RClassSCCFromData(s, d[1], o[1]))
*Length(LClassSCCFromData(s, d[2])))*Size(l)/
Size(RcapLSchutzGpFromData(s, d, o));
end);


#############################################################################
# JDM some problem here...

InstallGlobalFunction(SizeDClassRepsData, 
function(s)
local data, i, d, l, r, o_r, o_l;

data:=DClassRepsData(s)!.data;
i:=0;

for d in data do
	o_r:=RClassImageOrbitFromData(s, d[1]);
	r:=RClassSchutzGpFromData(s, d[1]);
	o_l:=LClassKernelOrbitFromData(s, d[2]);
	l:=LClassSchutzGpFromData(s, d[2]);
	i:=i+(Size(r)*Length(RClassSCCFromData(s, d[1]))
	 *Length(LClassSCCFromData(s, d[2]))*Size(l)/
	 Size(RcapLSchutzGpFromData(s,  d, [o_r, o_l])));
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
