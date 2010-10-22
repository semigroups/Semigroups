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
# 

InstallMethod(NrIdempotents, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local i, iter, d, r;
i:=0;

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

InstallMethod(GreensHClasses, "for a transformation semigroup",
[IsTransformationSemigroup],
s-> Concatenation(List(GreensDClasses(s), GreensHClasses)));

#############################################################################
## Global functions
#############################################################################

##  JDM the following should be reviewed! depends on R-classes

InstallOtherMethod( Idempotents, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(M)
local idempotent, pt, ker, img, kers, imgs, i, n, idm, one, x;

if not IsCompletelyRegularSemigroup(M) then
	GreensRClasses(M);
fi;

idm:= [];
kers:=GradedKernelsOfTransSemigroup(M);
imgs:=GradedImagesOfTransSemigroup(M);
n:=Size(kers);

# loop over all ranks.
for i in [1..n] do
	# loop over the kernels.
	for ker in kers[i] do
		# loop over the images.
		for img in imgs[i] do
			# check for cross section.
			if IsTransversal(ker, img) then
			
				x:=IdempotentNC(ker, img);
				if IsRegularSemigroup(M) or x in M then 
					Add(idm, x);
				fi;
				## IsRegularSemigroup will calculate the R-classes if it is regular or 
				## return false otherwise requiring the calculation of the R-classes for 
				## the \in test.
			fi;
		od;
	od;
od;

# return the set of idempotents.
return Set(idm);
end);

#####################

##  JDM the following should be reviewed! depends on R-classes

InstallOtherMethod(Idempotents, "for a trans. semigroup and pos. int.", 
[IsTransformationSemigroup, IsPosInt],
function(M, i)
local idempotent, pt, ker, img, kers, imgs, n, idm, one, x;

if i>DegreeOfTransformationSemigroup(M) then 
	return fail;
fi;

if HasIdempotents(M) then 
	return Filtered(Idempotents(M), x-> RankOfTransformation(x)=i);
fi;

if not IsCompletelyRegularSemigroup(M) then
	GreensRClasses(M);
fi;

idm:= [];
kers:=GradedKernelsOfTransSemigroup(M);
imgs:=GradedImagesOfTransSemigroup(M);
n:=Size(kers); #=Size(imgs)

# loop over the kernels.
for ker in kers[i] do

	# loop over the images.
	for img in imgs[i] do
		# check for cross section.
		if IsTransversal(ker, img) then
			x:=IdempotentNC(ker, img);     
			if IsRegularSemigroup(M) or x in M then 
				Add(idm, x);
			fi;
				## IsRegularSemigroup will calculate the R-classes if it is 
				## regular or return false otherwise requiring the calculation
				## of the R-classes for the \in test.
		fi;
	od;
od;

# return the set of idempotents.
return Set(idm);
end);

#############################################################################
##  JDM the following should be reviewed!

InstallMethod(PartialOrderOfDClasses, "for a semigroup", 
[IsSemigroup], 
function(M)
local class, poset, a, i, j, c;

class:= GreensDClasses(M);  
poset:= List([1..Length(class)], x->[]);

for i in [1..Length(class)] do
	AddSet(poset[i], i);
	for a in GeneratorsOfSemigroup(M) do
		for c in GreensRClasses(class[i]) do
			AddSet(poset[i], PositionProperty(class, x-> a * Representative(c) in x));
		od;
		for c in GreensLClasses(class[i]) do
			AddSet(poset[i], PositionProperty(class, x-> Representative(c) * a in x));
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

return poset;
#return Graph(Group(()), [1..Length(class)], OnPoints, function(x,y) return y in poset[x]; end, true); ;

end);