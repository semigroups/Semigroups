#############################################################################
##
#W  greens.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

# this file (should) contain all the technical and uninteresting functions
# governing Green's relations in MONOID.

#############################################################################

# new for 4.0!
#############################################################################
# keep here 

InstallMethod(IsGreensClassOfTransSemigp, "for a Green's class",
[IsGreensClass], x-> IsTransformationSemigroup(ParentAttr(x)));

InstallOtherMethod(IsGreensClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensRClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensLClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensHClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensDClass, "for an object", [IsObject], ReturnFalse);

#############################################################################
# keep here

InstallOtherMethod(GreensJClassOfElement, "for a trans. semigroup and elt",
[IsTransformationSemigroup and HasIsFinite and IsFinite, IsObject], 
function(s,e)
local ec;

ec := EquivalenceClassOfElementNC( GreensJRelation(s), e );
SetIsGreensClass(ec,true);
SetIsGreensJClass(ec,true);
SetIsGreensClassOfTransSemigp(ec, true);
return ec;
end);

# new for 4.0!
#############################################################################
# JDM this is horribly slow in comparison with Idempotents!

InstallMethod(NrIdempotents, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local i, iter, d, r;
i:=0;

if HasIdempotents(s) then 
	return Length(Idempotents(s));
fi;

if OrbitsOfKernels(s)!.finished then 
	iter:=IteratorOfGreensDClasses(s);
	#JDM would be better to use IteratorOfDClassRepsData
	for d in iter do 
		i:=i+NrIdempotents(d);
	od;
else
	iter:=IteratorOfGreensRClasses(s);
	#JDM would be better to use IteratorOfRClassRepsData but we require
	# NrIdempotentsFromData first!
	for r in iter do 
		i:=i+NrIdempotents(r);
	od;
fi;

return i;
end);

#############################################################################
# 

# the efficiency of the below can be improved by improving 
# NrGreensLClasses and NrGreensRClasses for a D-class JDM

InstallMethod(NrGreensHClasses, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local i, iter, d, r;
i:=0;

iter:=IteratorOfGreensDClasses(s);
#JDM would be better to use IteratorOfDClassRepsData

for d in iter do 
	i:=i+NrGreensHClasses(d);
od;

return i;
end);

#############################################################################
#

InstallMethod(IteratorOfIdempotents, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s);
#JDM here!
Error("not yet implemented");


end);

#############################################################################
#  JDM the following should be reviewed! depends on R-classes

InstallOtherMethod( Idempotents, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local foo, n, bound, out, kers, imgs, min, max, regular, i, ker, f, img, e; 

foo:=function(f, set) #is set a transversal of ker?
local i, j;
j:=[]; 
for i in set do 
	if not f[i] in j then 
		AddSet(j, f[i]);
	else
		return false;
	fi;
od;

return true;
end;

n:=Degree(s);

if HasNrIdempotents(s) then 
	bound:=NrIdempotents(s);
elif (HasSize(s) or OrbitsOfImages(s)!.finished) and HasNrGreensHClasses(s) then 
	bound:=Size(s)/NrGreensHClasses(s);;
elif HasSize(s) or OrbitsOfImages(s)!.finished then 
	bound:=Size(s);
else
	bound:=Sum(List([0..n], k-> Binomial(n,k)*(n-k)^k)); 
fi;

out:=EmptyPlist(bound);

kers:=GradedKernelsOfTransSemigroup(s); 
imgs:=GradedImagesOfTransSemigroup(s);

min:=PositionProperty(imgs, x-> not Length(x)=0); 
max:=First([Length(imgs), Length(imgs)-1..1] , x-> not Length(imgs[x])=0);
regular:=false; 

if HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then 
	regular:=true;
fi;

# loop over all ranks.
for i in [min..max] do
	for ker in kers[i] do
		f:=TABLE_OF_TRANS_KERNEL(ker, n);
		for img in imgs[i] do 
			if foo(f, img) then 
				e:=IdempotentNC(ker, img);
				if regular or e in s then 
					Add(out, e);
				fi;
			fi;
		od;
	od;
od;

return out;
end);

####################################################################################

InstallOtherMethod(Idempotents, "for a trans. semigroup and pos. int.", 
[IsTransformationSemigroup, IsPosInt],
function(s, i)
local foo, n, bound, out, kers, imgs, regular,  ker, f, img, e;

if i>Degree(s) then 
	return fail;
fi;

if HasIdempotents(s) then 
	return Filtered(Idempotents(s), x-> RankOfTransformation(x)=i);
fi; #JDM is this quicker? seems to be.

foo:=function(f, set) #is set a transversal of ker?
local i, j;
j:=[]; 
for i in set do 
	if not f[i] in j then 
		AddSet(j, f[i]);
	else
		return false;
	fi;
od;

return true;
end;

n:=Degree(s);

if HasNrIdempotents(s) then 
	bound:=NrIdempotents(s);
elif (HasSize(s) or OrbitsOfImages(s)!.finished) and HasNrGreensHClasses(s) then 
	bound:=Size(s)/NrGreensHClasses(s);;
elif HasSize(s) or OrbitsOfImages(s)!.finished then 
	bound:=Size(s);
else
	bound:=1; #JDM good idea MN?
fi;

out:=EmptyPlist(bound);

kers:=GradedKernelsOfTransSemigroup(s)[i]; 
imgs:=GradedImagesOfTransSemigroup(s)[i];

regular:=false; 

if HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then 
	regular:=true;
fi;

for ker in kers do
	f:=TABLE_OF_TRANS_KERNEL(ker, n);
	for img in imgs do 
		if foo(f, img) then 
			e:=IdempotentNC(ker, img);
			if regular or e in s then 
				Add(out, e);
			fi;
		fi;
	od;
od;


return out;
end);


#############################################################################

InstallMethod(PartialOrderOfDClasses, "for a semigroup", 
[IsSemigroup], 
function(s)
local d, out, gens, i, a, c, n;

d:= GreensDClasses(s);
n:=Length(d);
out:= List([1..n], x->EmptyPlist(n));
gens:=Generators(s);

for i in [1..Length(d)] do
	AddSet(out[i], i);
	for a in gens do
		for c in GreensRClassReps(d[i]) do
			AddSet(out[i], PositionProperty(d, x-> a * c in x));
		od;
		for c in GreensLClassReps(d[i]) do
			AddSet(out[i], PositionProperty(d, x-> c * a in x));
		od;
	od;
od;

#transitive closure JDM maybe not required??

#for i in [1..Length(class)] do
#	for j in [1..Length(class)] do
#		if j in poset[i] then 
#			poset[i]:=Union(poset[j], poset[i]);
#		fi;
#	od;
#od;

return out;
#return Graph(Group(()), [1..Length(class)], OnPoints, function(x,y) return y in poset[x]; end, true); ;
end);