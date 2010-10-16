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






#############################################################################
##
#M GreensHClasses( <L> ) ;
##
## Green's H-classes of a given Green's L-class
##
#JDM D:=GreensDClassData(DClassOfLClass(X)); would be better since 
#JDM GreensDClassData  likely to be known for DClassOfLClass and in 
#JDM unknown in the following line. The only problem is that 
#JDM the D-class returned by the above line does not have the correct
#JDM representative and so things go wrong. 
#JDM I think this is what makes GreensHClasses etc so slow...
#JDM however this can be speeded up by feeding GreensDClassData the
#JDM L-class we start with.

#JDM this should be reviewed

InstallOtherMethod(GreensHClasses, "for Green's L-class", true,
[IsGreensLClass], 0,
function(X)
local M, L, D, x, l, c, d, classes, class, new, grp, sch, cos, rep;

M:=ParentAttr(X); 
x:=Representative(X);
L:=GreensLClassData(X);
classes:= []; 
D:= GreensDClassData(GreensDClassOfElement(M, x), L); 

# determine groups and coset reps.
grp:= D!.L!.schutz; sch:= AsSubgroup(grp, D!.H!.schutz);
cos:= RightCosets(grp, sch);

# this gives a set of representatives of left cosets
Apply(cos, x-> Representative(x)^-1);

# loop over R class reps.
for l in D!.L!.invrelts do
	d:= AsTransformationNC(l * x);

	# loop over cosets.
	for c in cos do
		rep:=d*c;
		class:=GreensHClassOfElement(M, rep);
		SetGreensHClassData(class, HClassData(rec(rep:=rep, schutz:=sch)));
		SetLClassOfHClass(class, X);
		Add(classes, class);
	od;
od;

return classes;
end);



#############################################################################
##
#M  Size( <HClassData> )  
##  
##  Algorithm L.
##   
##  returns the size of <HClassData>
##

InstallOtherMethod(Size,  "size of H-class from data structure", 
[IsGreensHClassData], x-> Size(x!.schutz));

#############################################################################
##
#F  AsSSortedList( <HClassData> ) 
##  
##  Algorithm M.
##   
##  returns the elements of <HClassData>
##

#JDM this should be reviewed

InstallOtherMethod(AsSSortedList,  "elements of H-class from data structure", 
[IsGreensHClassData],
function(H)
local  elts, g, prod;

elts:= [];

for g in H!.schutz do 
	prod:= H!.rep*g;
	if not prod in elts then
		AddSet(elts, prod);
	fi; 
od;

SetSize(H, Length(elts));

return elts;
end);

#############################################################################
##
#M \in
##  
## Algorithm N.
##   
## tests membership in an H-class
##

#JDM this should be reviewed

InstallOtherMethod( \in, "membership test for an H-class", true, 
[IsTransformation, IsGreensHClassData], 0,
function(x,H)
local rep, img, grp;

# check degree, rank, image and kernel.
rep:= H!.rep; 

if DegreeOfTransformation(x) <> DegreeOfTransformation(rep) or RankOfTransformation(x) <> RankOfTransformation(rep) or KernelOfTransformation(x) <> KernelOfTransformation(rep) or ImageSetOfTransformation(x) <> ImageSetOfTransformation(rep) then
	return false;
fi;

# check the group.
return PermLeftQuoTransformation(rep, x) in H!.schutz;  

end) ;

#############################################################################
##
#M  Idempotents( <H> ) 
##
##  returns the idempotent in <H>, if there is one.
## 

#JDM this should be modified
## JDM this could be shortened.
 
InstallOtherMethod( Idempotents, "for H class data", 
[IsGreensHClassData],
function(H)
local idempotent, ker, img, idm;

idm:= [];
img:= ImageSetOfTransformation(H!.rep);
ker:= KernelOfTransformation(H!.rep);

# check for cross section.
if IsTransversal(ker, img) then
	Add(idm, Idempotent(ker, img));
fi;

# return the list of idempotents.
return idm;
end);

#############################################################################
##
#M  IsGroupHClass( <H> ) 
##
 
#JDM this should be modified

InstallOtherMethod(IsGroupHClass, "for a transformation semigroup", true,[IsGreensHClass and IsGreensClassOfTransSemigp], 0, 
x-> ImageSetOfTransformation(Representative(x)^2)=ImageSetOfTransformation(Representative(x)));

#############################################################################
##
#M  RClassOfHClass( <class> ) ; -> GreensRClass
##
##  Green's R-class of Green's H-class
##

#JDM this should be modified

InstallOtherMethod(RClassOfHClass, "for a transformation semigroup", true, [IsGreensHClass and IsGreensClassOfTransSemigp], 0, 
function(hc)

if HasGreensRClasses(ParentAttr(hc)) then 
	return First(GreensRClasses(ParentAttr(hc)), x-> Representative(hc) in x);
else
	return GreensRClassOfElement(ParentAttr(hc), Representative(hc));
fi;
end);

#############################################################################
##
#M LClassOfHClass( <class> ) ; -> GreensLClass
##
## Green's L-class of Green's H-class
##

#JDM this should be modified

InstallOtherMethod(LClassOfHClass, "for a transformation semigroup", true, [IsGreensHClass and IsGreensClassOfTransSemigp], 0, 
function(hc)

if HasGreensLClasses(ParentAttr(hc)) then 
	return First(GreensLClasses(ParentAttr(hc)), x-> Representative(hc) in x);
else
	return GreensLClassOfElement(ParentAttr(hc), Representative(hc));
fi;
end);

#############################################################################
##
#M DClassOfHClass( <class> ) ; -> GreensDClass
##
## Green's D-class of Green's H-class
##

#JDM this should be modified

InstallOtherMethod(DClassOfHClass, "for a transformation semigroup", true, 
[IsGreensHClass and IsGreensClassOfTransSemigp], 0, 
function(hc)

if HasGreensDClasses(ParentAttr(hc)) then 
	return First(GreensDClasses(ParentAttr(hc)), x-> Representative(hc) in x);
else
	return GreensDClassOfElement(ParentAttr(hc), Representative(hc));
fi;
end);

#############################################################################
##
#M GreensHClasses( <transformationsemigroup> ) ;
##
## Green's H-classes of a transformation semigroup
##

#JDM this should be modified

InstallMethod(GreensHClasses, "for a transformation semigroup",
[IsTransformationSemigroup],
x-> Concatenation(List(GreensDClasses(x), GreensHClasses)));

#############################################################################
##
#M	GreensHClasses( <drel> ) 
##	
##	all H-classes of Green's D-class <dclass>.
##

##  JDM the following should be reviewed!

# JDM this will only work if the representatives of D-classes are all
# JDM representatives of R-classes also.
# JDM why L? check which is faster
# JDM this ought also to account for what has already been calculated. I.e. 
# JDM if the GreensHClasses of GreensRClasses are already known then use them!

InstallOtherMethod(GreensHClasses, "for a GreensDClass", [IsGreensDClass],
function(x)
if HasIsCommutative(ParentAttr(x)) and IsCommutative(ParentAttr(x)) then 
	SetIsGreensHClass(x, true);
	return [x];
else 
	return Concatenation(List(GreensRClasses(x), GreensHClasses));
fi;
end);
#x-> Concatenation(List(GreensLClasses(x), GreensHClasses)));
#x-> Concatenation(List(GreensRClasses(x), GreensHClasses)));

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

InstallMethod(PartialOrderOfDClasses, "for a generic semigroup", true, [IsSemigroup], 
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

#transitive closure

for i in [1..Length(class)] do
	for j in [1..Length(class)] do
		if j in poset[i] then 
			poset[i]:=Union(poset[j], poset[i]);
		fi;
	od;
od;


return poset;
#return Graph(Group(()), [1..Length(class)], OnPoints, function(x,y) return y in poset[x]; end, true); ;

end);