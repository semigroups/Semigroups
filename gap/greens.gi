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

# new for 3.2!
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

###########################################################################
# keep here

InstallGlobalFunction(HClassData, function(list)
return Objectify(NewType(NewFamily("Green's H Class Data", IsGreensHClassData), 
IsGreensHClassData and IsGreensHClassDataRep), list);
end);




#############################################################################
# keep here

InstallMethod( ViewObj, "for object in `IsGreensHClassData'",
[ IsGreensHClassData and IsGreensHClassDataRep],
function( obj )
Print( "GreensHClassData( ", obj!.rep, ", ", obj!.schutz, " )" );
end );

## Commands to integrate MONOID functions with those in semirel.g* ##
#####################################################################
# JDM remove?

InstallMethod(GreensData, "for a Green's class of a trans. semigroup", 
[IsGreensClass and IsGreensClassOfTransSemigp],
function(class)

if HasIsGreensRClass(class) and IsGreensRClass(class) then 
	return GreensRClassData(class);
elif HasIsGreensLClass(class) and IsGreensLClass(class) then 
	return GreensLClassData(class);
elif HasIsGreensHClass(class) and IsGreensHClass(class) then 
	return GreensHClassData(class);
elif HasIsGreensDClass(class) and IsGreensDClass(class) then 
	return GreensDClassData(class);
#elif HasIsGreensJClass(class) and IsGreensJClass(class) then 
#return GreensJClassData(class);
fi;

end);

#############################################################################
# JDM remove?

InstallMethod(\<, "for GreensData and GreensData", 
[IsGreensData, IsGreensData],
function(data1, data2)

if not ParentAttr(data1)=ParentAttr(data2) then 
	Error("Green's data do not belong to the same semigroup");
elif IsGreensRClassData(data1) and not IsGreensRClassData(data2) then 
	Error("Green's data are not of the same type"); 
elif IsGreensLClassData(data1) and not IsGreensLClassData(data2) then 
	Error("Green's data are not of the same type"); 
elif IsGreensHClassData(data1) and not IsGreensHClassData(data2) then 
	Error("Green's data are not of the same type"); 
elif IsGreensDClassData(data1) and not IsGreensDClassData(data2) then 
	Error("Green's data are not of the same type"); 
else 
	return data1!.rep<data2!.rep;
fi;
end);

#############################################################################
# JDM remove?

InstallMethod(\=, "for GreenData and GreenData", 
[IsGreensData, IsGreensData],
function(data1, data2)

if not ParentAttr(data1)=ParentAttr(data2) then 
	Error("Green's data do not belong to the same semigroup");
elif IsGreensRClassData(data1) and not IsGreensRClassData(data2) then 
	Error("Green's data are not of the same type"); 
elif IsGreensLClassData(data1) and not IsGreensLClassData(data2) then 
	Error("Green's data are not of the same type"); 
elif IsGreensHClassData(data1) and not IsGreensHClassData(data2) then 
	Error("Green's data are not of the same type"); 
elif IsGreensDClassData(data1) and not IsGreensDClassData(data2) then 
	Error("Green's data are not of the same type"); 
else 
	return data1!.rep in data2;
fi;
end);

#############################################################################
# remove? JDM

InstallOtherMethod(Representative, "for GreensData", 
[IsGreensData], x-> x!.rep);

#############################################################################

InstallOtherMethod(GreensHClasses, "for an R-class", 
[IsGreensRClass], 
function(rc)
local D, x, c, m, classes, class, R;

classes:= []; 
R:=GreensRClassData(rc); 
x:= R!.rep;
D:=GreensDClassData(DClassOfRClass(rc));
#JDM see GreensHClasses of an L-class below

# loop over the cosets.
for c in D!.cosets do 
	# loop over the class representatives.
	for m in R!.perms do
		class:=GreensHClassOfElement(ParentAttr(rc), x * (c/m));
		SetRClassOfHClass(class, rc);
		Add(classes, class);
	od;
od;

return classes;
end );

#############################################################################
# JDM this should be modified...

InstallMethod(GreensLClassData, "for a Green's L-class of trans. semigp", 
true, [IsGreensLClass and IsGreensClassOfTransSemigp], 
function(L)
local ker, orbit, i, j, back, s, pnt, new, a, n, set, sets, z, relts, gens, img, rep, scc, invrelts, newinv, schutz, data;

# determine starting point.
rep:= Representative(L); ker:= KernelOfTransformation(rep);
orbit:=[ker]; sets:=[ker]; n:=Length(ker); i:=0; back:= [[]];

if IsMonoid(ParentAttr(L)) then 
	gens:=GeneratorsOfSemigroup(ParentAttr(L));
else
	gens:= Union(GeneratorsOfSemigroup(ParentAttr(L)),
	[Transformation([1..DegreeOfTransformation(rep)])]); 
fi;	

# form the (weak, but graded) orbit.
for pnt in orbit do

	# keep track of position of 'pnt'.
	i:= i+1;

	# loop over the generators.
	for s in gens do
		new:= OnTuplesOfSetsAntiAction(pnt, s);
		set:= Set(new);

		# discard points of lower grading.
		if not [] in set then
			j:= Position(sets, set);
			# install new point, if necessary.
			if j = fail then
				Add(orbit, new); Add(sets, set); Add(back, []); j:= Length(orbit); 
			fi;
			
			# remember predecessor.
			AddSet(back[i], j);
		fi;
	od;
od;

# form the transitive closure.
scc:=First(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(back), x-> 1 in x);

orbit:= orbit{scc};sets:= sets{scc};

# find multipliers.

relts:=[]; invrelts:=[];
for pnt in orbit do
	new:= []; newinv:=[];
	for j in [1..n] do
		new{ker[j]}:= List(ker[j], x-> pnt[j]);
		newinv{pnt[j]}:=List(pnt[j], x-> ker[j]);
	od;
	Add(relts, BinaryRelationByListOfImages(new)); 
	Add(invrelts, BinaryRelationByListOfImages(newinv));
od;

# determine Schutz grp.

new:= [()];
for i in [1..Length(sets)] do
	pnt:= sets[i]; z:=0;

	repeat #JDM can this loop can be omitted by keeping track earlier?
		z:=z+1; s:=gens[z];
		j:= Position(sets, OnKernelsAntiAction(pnt, s));
		if j <> fail then
			AddSet(new, PermLeftQuoTransformation(rep, AsTransformationNC(relts[j] * (s * AsTransformationNC( invrelts[i] * rep)))));
		fi;
	until z=Length(gens) or Size(new)>=Factorial(n);
od;

data:=LClassData(rec( rep:=rep, strongorb:=sets, relts:=relts, invrelts:=invrelts, schutz:=Group(new)));

return data;

end);

#############################################################################
# JDM this should be modified

InstallMethod(GreensLClasses, "for a trans. semigroup", true, 
[IsTransformationSemigroup], 0, 
function(M)
local n, one, gens, kernels, positions, classespart, reps, orb, x, ker, k, j, i, r, pos, new, s, images, im, classes, class;

classes:=Flat(List(GreensDClasses(M), GreensLClasses));
return classes;

end);

#############################################################################
##
#M  Size( <LClassData> )  
##  
##  Algorithm H.
##   
##  returns the size of <LClassData>
##

InstallOtherMethod(Size,  "size of L-class from data structure", 
true, [IsGreensLClassData], 0,
function(data)
return Size(data!.schutz)*Size(data!.strongorb);
end);

#############################################################################
##
#M  AsSSortedList( <LClassData> ) 
##  
##  Algorithm I.
##   
##  returns the elements of <LClassData>
##

#JDM this should be reviewed

InstallOtherMethod(AsSSortedList,  "elements of L-class from data structure", [IsGreensLClassData],
function(L)
local m, x, elts, grp, g, prod;

grp:= L!.schutz;	#the left schutz. group
x:= L!.rep;				#representative of L-class
elts:= [];

for m in L!.invrelts do
	for g in grp do 
		prod:=AsTransformation(m * x) * g; 
		if not prod in elts then 
			AddSet(elts, prod);
		fi;
	od;
od;

SetSize(L, Size(elts));
return elts;
end) ;

#############################################################################
##
#M  \in
##  
##  Algorithm J.
##   
##  tests membership of L-Class data
##

#JDM this should be reviewed

InstallOtherMethod( \in, "membership test for L-Class data", true, [IsTransformation, IsGreensLClassData], 0,
function(x,L)
local i, rep, grp, ker, n;

rep:= L!.rep; 

if x=rep then 
	return true; 
fi;

# check degree, rank, and kernel.

if DegreeOfTransformation(x) <> DegreeOfTransformation(rep) or RankOfTransformation(x) <> RankOfTransformation(rep) or ImageSetOfTransformation(x) <> ImageSetOfTransformation(rep) then
	return false;
fi;

# check kernel
ker:=KernelOfTransformation(x);

i:= Position(L!.strongorb, ker);

if i = fail then 
	return false;
fi;

# check the group.
return PermLeftQuoTransformation(rep, AsTransformation(L!.relts[i]*x)) in L!.schutz;
#JDM some problem here???
end);

#############################################################################
##
#M  Idempotents( <L> ) 
##
##  gives the idempotents of an L-class <L>
##

#JDM this should be reviewed

InstallOtherMethod( Idempotents, "for Green's L-class data", true, [IsGreensLClassData], 0,
function(L)
local idempotent, ker, img, idm;

idm:= [];
img:= ImageSetOfTransformation(L!.rep);

# loop over the kernels.
for ker in L!.strongorb do
# check for cross section.
	if IsTransversal(ker, img) then
		Add(idm, Idempotent(ker, img));
	fi;
od;

# return the list of idempotents.
return idm;

end );

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
#M DClassOfLClass( <class> ) ;
##
## Green's D-class of Green's L-class
##

#JDM this should be reviewed

InstallOtherMethod(DClassOfLClass, "for a transformation semigroup", true, [IsGreensHClass and IsGreensClassOfTransSemigp], 0, 
function(hc)

if HasGreensDClasses(ParentAttr(hc)) then 
	return First(GreensDClasses(ParentAttr(hc)), x-> Representative(hc) in x);
else
	return GreensDClassOfElement(ParentAttr(hc), Representative(hc));
fi;

end);

#############################################################################
#JDM this should be reviewed

InstallMethod(GreensHClassData, "data structure of H-class of an element", 
true, [IsGreensHClass and IsGreensClassOfTransSemigp], 0,
function(class)
local R, L, data;

R:= GreensRClassData(GreensRClassOfElement(ParentAttr(class), Representative(class))); 
L:= GreensLClassData(GreensLClassOfElement(ParentAttr(class), Representative(class)));

data:=HClassData(rec( rep:=Representative(class),  schutz:=Intersection(R!.schutz, L!.schutz)));

return data;
end) ;

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
#M  RClassOfHClass( <class> ) ;
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
#M LClassOfHClass( <class> ) ;
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
#M DClassOfHClass( <class> ) ;
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

######################
#JDM this should be modified

InstallOtherMethod(GreensDClassData,  "data structure of D-class of an element", 
[IsGreensDClass and IsGreensClassOfTransSemigp, IsGreensLClassData],
function(class, L)
local R, H, cosets, data, schutz, rep;

rep:=Representative(L); # rep has correct kernel
R:=GreensRClassData(GreensRClassOfElement(ParentAttr(class), rep));
H:=GreensHClassData(GreensHClassOfElement(ParentAttr(class), rep));

schutz:=H!.schutz;
cosets:= RightCosets(R!.schutz, AsSubgroup(R!.schutz, H!.schutz));
Apply(cosets, Representative);

data:=DClassData(rec( rep:=rep, R:=R, L:=L, H:=H, cosets:=cosets, schutz:=schutz));

return data;
end);



#############################################################################
##
#M  AsSSortedList( <DClassData> )
##
##  NOT Algorithm Q.
##
##  returns the elements of <DClassData>.
## 

InstallOtherMethod(AsSSortedList, "elements of D-class from data structure", 
[IsGreensDClassData],
function(D)
local c, e, m, elts, cosets, mults, L, new;

elts:= []; # list elements.
cosets:=D!.cosets; # coset representatives
mults:=D!.R!.perms; # multipliers of R-class
L:=D!.L; # L-class

for c in cosets do 
	for m in mults do
		for e in Elements(L) do
			new:=e*c/m;
			if not new in elts then 
				Add(elts, new);
			fi;
		od;
	od;
od;

SetSize(D, elts);
return elts;

end) ;

#############################################################################
##
#M  GreensLClasses( <dclass> ) 
##  
##  all L-classes of Green's D-class <dclass>.
##

InstallOtherMethod(GreensLClasses, "for transformation semigroups",
[IsGreensDClass],
function(dclass)
local M, x, c, m, classes, gens, rep, data, D, class;

classes:= [];  
M:=ParentAttr(dclass);
D:=GreensDClassData(dclass);
x:= D!.rep;  
gens:= GeneratorsOfGroup(D!.L!.schutz);

# loop over the cosets.
for c in D!.cosets do

	# loop over the L class representatives.
	for m in D!.R!.perms do
		rep:= x * (c/m);
		data:=LClassData(rec(  rep:=rep, 
		strongorb:=D!.L!.strongorb, 
		relts:=D!.L!.relts, 
		invrelts:=D!.L!.invrelts, 
		schutz:=Group(List(gens, x-> x^(c/m)), ())));
		class:=GreensLClassOfElement(M, rep);
		SetGreensLClassData(class, data);
		SetDClassOfLClass(class, dclass);
		Add(classes, class);  
	od;
od;

# return the list of L classes.
return classes;
end);

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

InstallMethod(SchutzenbergerGroup, "for GreensData",
[IsGreensData], x-> x!.schutz );

InstallOtherMethod(SchutzenbergerGroup, "for GreensClass",
[IsGreensClass], x-> GreensData(x)!.schutz );


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