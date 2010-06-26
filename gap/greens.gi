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

##  This file contains the `fast' algorithms for computing Green's relations
##  and related notions for transformation semigroups and monoid. 
##  The theory behind these algorithms is developed in 
##  
##  [LPRR1] S. A.   Linton, G.  Pfeiffer, E.  F.  Robertson, and N.   Ruskuc,
##  Groups  and actions in  transformation semigroups, to appear in Math Z.
##  (1998).
##
##  The algorithms themselves are described in
##
##  [LPRR2] S. A.   Linton, G.  Pfeiffer, E.  F.  Robertson, and N.   Ruskuc,
##  Computing transformation semigroups, (1998), in preparation.
##  
##  Another reference is
##
##  [LM]  G.  Lallement and R. McFadden, On the   determination of Green's
##  relations in finite transformation semigroups, J. Symbolic Computation 10
##  (1990), 481--489.
##
#############################################################################
#############################################################################


###########################################################################

InstallMethod(IsAssociatedSemigpTransSemigp, "for a Green's class",
[IsGreensClass], x-> IsTransformationSemigroup(ParentAttr(x)));

#############################################################################

InstallOtherMethod(GreensRClassOfElement, "for a trans. semigroup and elt", 
[IsTransformationSemigroup and HasIsFinite and IsFinite, IsObject],
function(s,e)
local ec;

ec := EquivalenceClassOfElementNC( GreensRRelation(s), e );
SetIsGreensClass(ec,true);
SetIsGreensRClass(ec,true);
SetIsAssociatedSemigpTransSemigp(ec, true);
return ec;
end);

#############################################################################

InstallOtherMethod(GreensLClassOfElement, "for a trans. semigroup and elt", 
[IsTransformationSemigroup and HasIsFinite and IsFinite, IsObject],
function(s,e)
local ec;

ec := EquivalenceClassOfElementNC( GreensLRelation(s), e );
SetIsGreensClass(ec,true);
SetIsGreensLClass(ec,true);
SetIsAssociatedSemigpTransSemigp(ec, true);
return ec;
end);

#############################################################################

InstallOtherMethod(GreensHClassOfElement, "for a trans. semigroup and elt",
[IsTransformationSemigroup and HasIsFinite and IsFinite, IsObject],
function(s,e)
local ec;

ec := EquivalenceClassOfElementNC( GreensHRelation(s), e );
SetIsGreensClass(ec,true);
SetIsGreensHClass(ec,true);
SetIsAssociatedSemigpTransSemigp(ec, true);
return ec;
end);

#############################################################################

InstallOtherMethod(GreensDClassOfElement, "for a trans. semigroup and elt",
[IsTransformationSemigroup and HasIsFinite and IsFinite, IsObject], 
function(s,e)
local ec;

ec := EquivalenceClassOfElementNC( GreensDRelation(s), e );
SetIsGreensClass(ec,true);
SetIsGreensDClass(ec,true);
SetIsAssociatedSemigpTransSemigp(ec, true);
return ec;
end);

#############################################################################

InstallOtherMethod(GreensJClassOfElement, "for a trans. semigroup and elt",
[IsTransformationSemigroup and HasIsFinite and IsFinite, IsObject], 
function(s,e)
local ec;

ec := EquivalenceClassOfElementNC( GreensJRelation(s), e );
SetIsGreensClass(ec,true);
SetIsGreensJClass(ec,true);
SetIsAssociatedSemigpTransSemigp(ec, true);
return ec;
end);

###########################################################################
#JDM do not recreate the family and type every time here...


InstallGlobalFunction(RClassData, function(list)
return Objectify(NewType(NewFamily("Green's R Class Data", IsGreensRClassData), 
IsGreensRClassData and IsGreensRClassDataRep), list);
end);

###########################################################################

InstallGlobalFunction(LClassData, function(list)
return Objectify(NewType(NewFamily("Green's L Class Data", IsGreensLClassData), IsGreensLClassData and IsGreensLClassDataRep), list);
end);

###########################################################################

InstallGlobalFunction(DClassData, function(list)
return Objectify(NewType(NewFamily("Green's D Class Data", IsGreensDClassData), IsGreensDClassData and IsGreensDClassDataRep), list);
end);

###########################################################################

InstallGlobalFunction(HClassData, function(list)
return Objectify(NewType(NewFamily("Green's H Class Data", IsGreensHClassData), IsGreensHClassData and IsGreensHClassDataRep), list);
end);

#############################################################################

InstallMethod( ViewObj, "for Green's R-class data",
[IsGreensRClassData and IsGreensRClassDataRep],
function( obj )
Print( "GreensRClassData( ", obj!.rep, ", ", obj!.strongorb,", ", obj!.perms,", ", obj!.schutz, " )" );
end );

#############################################################################

InstallMethod( PrintObj, "for object in `IsGreensRClassData'",
[ IsGreensRClassData and IsGreensRClassDataRep],
function( obj )
Print( "GreensRClassData( ", obj!.rep,  " )" );
end );

#############################################################################

InstallMethod( ViewObj, "for object in `IsGreensLClassData'",
[ IsGreensLClassData and IsGreensLClassDataRep],
function( obj )
Print( "GreensLClassData( ", obj!.rep, ", ", obj!.strongorb,", ", obj!.relts,", ", obj!.invrelts,", ", obj!.schutz, " )" );
end );

#############################################################################

InstallMethod( PrintObj, "for object in `IsGreensLClassData'",
[ IsGreensLClassData and IsGreensLClassDataRep],
function( obj )
Print( "GreensLClassData( ", obj!.rep,  " )" );
end );

#############################################################################

InstallMethod( ViewObj, "for object in `IsGreensDClassData'",
[ IsGreensDClassData and IsGreensDClassDataRep],
function( obj )
Print( "GreensDClassData( ", obj!.rep, ", ", obj!.R,", ", obj!.L, " )" );
end );

#############################################################################

InstallMethod( PrintObj, "for object in `IsGreensDClassData'",
[ IsGreensDClassData and IsGreensDClassDataRep],
function( obj )
Print( "GreensDClassData( ", obj!.rep,  " )" );
end );

#############################################################################

InstallMethod( ViewObj, "for object in `IsGreensHClassData'",
[ IsGreensHClassData and IsGreensHClassDataRep],
function( obj )
Print( "GreensHClassData( ", obj!.rep, ", ", obj!.schutz, " )" );
end );

## Commands to integrate MONOID functions with those in semirel.g* ##
#####################################################################

InstallMethod(GreensData, "for a Green's class of a transformation semigroup", true, [IsGreensClass and IsAssociatedSemigpTransSemigp], 0, 
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

InstallMethod( \in, "for a Green's Class of a transformation semigroup", true, [IsTransformation, IsGreensClass and IsAssociatedSemigpTransSemigp], 0, 
function (elm, class)
	return elm in GreensData(class);
end); 

#############################################################################

InstallMethod( Size, "for a Green's class of a transformation semigroup", true, [IsGreensClass and IsAssociatedSemigpTransSemigp], 0,
x-> Size(GreensData(x)));

#############################################################################

InstallMethod( AsSSortedList, "for a Green's class of a transformation semigroup", true, [IsGreensClass and IsAssociatedSemigpTransSemigp], 0, 
x-> AsSSortedList(GreensData(x)));

#############################################################################

InstallOtherMethod( Idempotents, "for a Green's class of a transformation semigroup", true, [IsGreensClass and IsAssociatedSemigpTransSemigp], 0, 
x-> Idempotents(GreensData(x)));

#############################################################################

InstallMethod(\<, "for Green's data", true, [IsGreensData, IsGreensData], 0, 
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
#elif IsGreensJClassData(data1) and not IsGreensJClassData(data2) then 
#Error("Green's data are not of the same type"); 
else 
	return data1!.rep<data2!.rep;
fi;

end);

##  returns if the representative of <data1> is in <data2>.
#############################################################################

InstallMethod(\=, "= for GreensData", true, 
[IsGreensData, IsGreensData], 0, 
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
#elif IsGreensJClassData(data1) and not IsGreensJClassData(data2) then 
#Error("Green's data are not of the same type"); 
else 
	return data1!.rep in data2;
fi;
end);

##  user friendly return of data!.rep 
#############################################################################

InstallOtherMethod(Representative, "for GreensData", true, 
[IsGreensData],0,
function(data)
	return data!.rep;
end);

###########################################################################

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
##
##	<#GAPDoc Label="ImagesOfTransSemigroup">
##	<ManSection>
##	<Attr Name="ImagesOfTransSemigroup" Arg="S[,n]"/>
##	<Description>
##	returns the set of all the image sets that elements of <C>S</C> admit. That 
##	is, the union of the orbits of the image sets of the generators of 
##	<C>S</C> under the action <Ref Func="OnSets" BookName="ref"/>. <P/>
##
##	If the 
##	optional second argument <C>n</C> (a positive integer) is present, then the 
##	list of image sets of size <C>n</C> is returned.  If you are only interested 
##	in the images of a given size, then the second version of the function will 
##	likely be faster.
##	<Example>
##  gap&gt;  S:=Semigroup([ Transformation( [ 6, 4, 4, 4, 6, 1 ] ), 
##  &gt; Transformation( [ 6, 5, 1, 6, 2, 2 ] ) ];;
##  gap&gt; ImagesOfTransSemigroup(S, 6);
##  [  ]
##  gap&gt; ImagesOfTransSemigroup(S, 5);
##  [  ]
##  gap&gt; ImagesOfTransSemigroup(S, 4);
##  [ [ 1, 2, 5, 6 ] ]
##  gap&gt; ImagesOfTransSemigroup(S, 3);
##  [ [ 1, 4, 6 ], [ 2, 5, 6 ] ]
##  gap&gt; ImagesOfTransSemigroup(S, 2);
##  [ [ 1, 4 ], [ 2, 5 ], [ 2, 6 ], [ 4, 6 ] ]
##  gap&gt; ImagesOfTransSemigroup(S, 1);
##  [ [ 1 ], [ 2 ], [ 4 ], [ 5 ], [ 6 ] ]
##  gap&gt; ImagesOfTransSemigroup(S);
##  [ [ 1 ], [ 1, 2, 5, 6 ], [ 1, 4 ], [ 1, 4, 6 ], [ 2 ], [ 2, 5 ], [ 2, 5, 6 ], 
##    [ 2, 6 ], [ 4 ], [ 4, 6 ], [ 5 ], [ 6 ] ]
##  </Example> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(ImagesOfTransSemigroup, "for a transformation semigroup", true, [IsTransformationSemigroup], 0, 
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

################

InstallOtherMethod(ImagesOfTransSemigroup, "for  a trans. semigroup and a pos. int.", true, [IsTransformationSemigroup, IsPosInt], 0, 
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

InstallOtherMethod(KernelsOfTransSemigroup, "for a trans. semigroup", true, 
[IsTransformationSemigroup, IsPosInt], 0, 
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

###############

InstallMethod(KernelsOfTransSemigroup, "for a transformation monoid", true, 
[IsTransformationSemigroup], 0, 
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

#############################################################################
##

DeclareGlobalFunction("PermLeftQuoTransformationNC");

#############################################################################
##

PermLeftQuoTransformationNC:= 
function ( t1, t2 )
local  pl, i, deg;

deg := Length( t1![1] );
pl := [ 1 .. deg ];
for i  in [ 1 .. deg ]  do
    pl[t1![1][i]] := t2![1][i];
od;
return PermList( pl );
end;

#############################################################################
##

InstallMethod(\*, "NEW", [IsTransformation and IsTransformationRep, 
IsTransformation and IsTransformationRep],
function(x, y)
local  a, b, c;
a := x![1];
b := y![1];
c := b{a};
MakeImmutable(c);
return Objectify( TypeObj(x), [ c ] );
end);

#############################################################################
##

InstallMethod(\*, [IsTransformation and IsTransformationRep, IsPerm],
function(x, y)
local c;

c:=OnTuples(x![1], y);
MakeImmutable(c);
return Objectify( TypeObj(x), [ c ] );
end);

#############################################################################
##
# assumes <src> and <dst> are sets of pos. ints. of equal length.

MappingPermSetSetNC:=function(src, dst)
local l, d, out, i, j, next, k;

l:=Length(src);
d:=Maximum(src[l], dst[l]);
out:=EmptyPlist(d);

i:=1;
j:=1;
next:=1;   # the next candidate, possibly prevented from being in dst

for k in [1..d] do
 if i<=l and k=src[i] then
   out[k]:=dst[i];
   i:=i+1;
 else
   # Skip things in dst:
   while j<=l and next>=dst[j] do
     if next = dst[j] then next:=next+1; fi;
     j:=j+1;
   od;
   out[k]:=next;
   next:=next+1;
 fi;
od;

return PermList(out);
end;

#############################################################################
##  JDM reduce number of local variable names!

InstallMethod(StrongOrbitOfImage, "for a trans. semigroup and transformation", 
[IsTransformationSemigroup, IsTransformation],
function(M, x)
local sorbit, img, pos, gens, n, i, orb, sets, graph, reps, y, z, new, set, j,
 scc, schutzgens, perms, sccimgs, orbimgs, comp, sorb, rep, schutzgps, k, s;

d:=DegreeOfTransformationSemigroup(s);
img:=Set(f![1]);
o:=Orb(s, img, OnSets, rec(treehashsize:=NextPrimeInt(Minimum(100000, 3*Binomial(d, Length(img)))), 
        schreier:=true,
        gradingfunc := function(o,x) return Length(x); end, 
        orbitgraph := true, onlygrades:=[Length(img)], storenumbers:=true));
Enumerate(o);

comp:=First(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(OrbitGraph(o)), x-> 1 in x);;
t:=BlistList([1..Length(o)], comp);;
#p:=List(AsList(o), x-> MappingPermListList(x, img));
p:=EmptyPlist(Length(o));
for i in comp do 
  p[i]:=MappingPermSetSetNC(o[i], img);
od;

gens:=GeneratorsOfMonoid(s); 
g:=Group(());

if Length(img)<1000 then 
	bound:=Factorial(Length(img));
else
  bound:=infinity;
fi;

#use local variable for o!.orbitgraph

for i in comp do 
 for j in [1..Length(gens)] do #Length(gens)
   if IsBound(o!.orbitgraph[i][j]) and t[o!.orbitgraph[i][j]] then 
     g:=ClosureGroup(g, PermLeftQuoTransformationNC_C(f, f/p[i] *
      (gens[j]*p[o!.orbitgraph[i][j]])));
   fi;
   if Size(g)>=bound then #or use while; this only need if numb of gens large!!
     break;
   fi;
 od;
 if Size(g)>=bound then 
   break;
 fi;
od;

#####

if HasStrongOrbitsOfImages(M) then 
	sorbit:=StrongOrbitsOfImages(M);
else 
	sorbit:=[[],[],[]];
fi;

img:=ImageSetOfTransformation(x);
pos:=PositionProperty(sorbit[1], a-> img in a); 

if pos=fail then 
	gens:=GeneratorsOfSemigroup(M);
	n:=Length(img); i:=0;
	orb:=[img]; sets:=[img]; graph:=[[]]; reps:=[x];

	for y in orb do
		i:=i+1;
		for z in gens do
			new:=OnTuples(y,z); set:=Set(new); 

			if Size(set)=n and PositionProperty(sorbit[1], x-> set in x)=fail then
				j:=Position(sets, set); 
				if j=fail then 
					Add(orb, new); Add(sets, set); Add(reps, reps[i]*z);
						j:=Length(orb); Add(graph,[]); 
				fi;
				AddSet(graph[i],j);
			fi;
		od;
	od;

	scc:=STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(graph);
	
	schutzgps:=[]; perms:=[]; sccimgs:=[]; orbimgs:=[];

	for i in [1..Length(scc)] do 
#JDM maybe a problem here similar to that in StrongOrbitKernels
		comp:=scc[i]; sorb:=orb{comp}; Add(orbimgs, sorb);
		set:=sets{comp}; Add(sccimgs, set); rep:=reps[comp[1]]; img:=set[1];

		if 1 in comp then 
			pos:=i+Length(sorbit[1]);
		fi;

		Add(perms, List(sorb, y->MappingPermListList(y,sorb[1])));

# Schutzenberger groups

		schutzgens:=[()]; 
 
		for k in [1..Length(set)] do
			y:= set[k];
			z:=0;
			repeat #JDM can this loop be omitted by keeping track earlier?
				z:=z+1; s:=gens[z];
				j:= Position(set, OnSets(y, s));
				if j <> fail then
					AddSet(schutzgens, PermLeftQuoTransformation(rep, rep/perms[i][k] * (s*perms[i][j])));
				fi;
			until z=Length(gens) or Length(schutzgens)>=Factorial(n);
		od;
		Add(schutzgps, Group(schutzgens));
	od;

	Append(sorbit[1], sccimgs); Append(sorbit[2], perms); 
	Append(sorbit[3], schutzgps);

	SetStrongOrbitsOfImages(M, sorbit);
fi;

return [sorbit[1][pos], sorbit[2][pos], sorbit[3][pos]];

end);

#############################################################################


InstallMethod(GreensRClassData, "data structure of R-class of an element", true, [IsGreensRClass and IsAssociatedSemigpTransSemigp], 0,
function(class)
local rep, sorbit, pos, strongorb, perms, data, schutz;

rep:=Representative(class);
sorbit:=StrongOrbitOfImage(ParentAttr(class), rep);
pos:=Position(sorbit[1],ImageSetOfTransformation(rep));

if not pos=1 then 
	strongorb:=Concatenation(sorbit[1]{[pos..Length(sorbit[1])]}, sorbit[1]{[1..pos-1]});
	perms:=List(Concatenation(sorbit[2]{[pos..Length(sorbit[2])]}, 
	sorbit[2]{[1..pos-1]}), x-> x*sorbit[2][pos]^-1);
	schutz:=sorbit[3]^(sorbit[2][pos]^-1);
else
	strongorb:=sorbit[1];
	perms:=sorbit[2];
	schutz:=sorbit[3];
fi;

data:=RClassData(rec( rep:=rep, strongorb:=strongorb, 
perms:=perms, schutz:=schutz));

return data;

end);

#############################################################################
##
#M  GreensRClasses( <transsemigroup> ) 
##  
##  NOT Algorithm U.
##

InstallOtherMethod(GreensRClasses, "for a transformation semigroup", true,
 [IsTransformationSemigroup], 0,
function ( M )
local n, one, gens, images, positions, classes, classespart, reps, kernels, orb, x, img, k, j, i, r, pos, ker, new, s, class;

n := DegreeOfTransformationSemigroup( M );
one := TransformationNC( [ 1 .. n ] );

if IsTransformationMonoid( M )then
	gens := GeneratorsOfMonoid( M );
else
	gens := GeneratorsOfSemigroup( M );
fi;

images := List( [ 1 .. n ], x-> [] );
positions := List( [ 1 .. n ], x->[]) ;
classes := []; reps := [];
kernels := []; classespart:=[];
orb := [ one ];

for x in orb do
	img := ImageSetOfTransformation( x );
	k := Length( img );
	j := Position( images[k], img );
	if j = fail then
		if IsTransformationMonoid( M ) or not x = one then
			class:=GreensRClassOfElement(M, x);
			r := GreensRClassData(class);
			Add(classes, class);
			Add(classespart, [r]);
			Add( reps, [ x ] );
			Add( kernels, [ KernelOfTransformation( x ) ] );
			Append( images[k], r!.strongorb );
			j := Length( classespart );
			Append( positions[k], List( r!.strongorb,x ->j )); 
		fi;
		for s in gens do
			Add( orb, s * x );
		od;
	else
		if IsTransformationMonoid( M ) or not x = one then
			pos := positions[k][j];
			r := classespart[pos][1];
			x := x * r!.perms[Position( r!.strongorb, img )];
			ker := KernelOfTransformation( x );
			new := true;
			i:=0;

			repeat 
				i:=i+1;
					if ker = kernels[pos][i] and x in classespart[pos][i] then
						new := false;
					fi;
			until i=Length( kernels[pos] ) or new=false;

			if new then
				Add( reps[pos], x );
				Add( kernels[pos], ker );
				class:=GreensRClassOfElement(M, x);

				r:=RClassData( rec( rep:=x, strongorb:=r!.strongorb, perms:=r!.perms, schutz:=r!.schutz ));
				SetGreensRClassData(class, r);
				Add( classespart[pos], r); 
				Add(classes, class);
				for s in gens do
					Add( orb, s * x );
				od;
			fi;
		fi;
	fi;
od;

SetPositionsRClasses(M, positions);
SetGradedRClasses(M, classespart);
SetGradedImagesOfTransSemigroup( M, images);
SetInternalKernels(M, kernels);
SetGreensRClassReps(M, reps); 

return classes;

end);

#############################################################################
##
##	<#GAPDoc Label="GreensRClassReps">
##	<ManSection>
##	<Attr Name="GreensRClassReps" Arg="S"/>
##	<Description>
##	returns the representative of all the R-classes of the transformation 
##	semigroup <C>S</C> as a list of lists ordered by the rank of the 
##	representatives and the order that the representatives are produced by the 
##	function <Ref Attr="GreensRClasses" BookName="ref"/>.  Also 
##	<C>GreensRClassReps[i][j]</C> is the representative of <Ref 
##	Attr="GradedRClasses"/><C>[i][j]</C>.
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 1, 2, 1, 2, 1 ] ), 
##  &gt; Transformation( [ 3, 4, 2, 1, 4 ] ) ];;
##  gap&gt; S:=Semigroup(gens);; 
##  gap&gt; GreensRClassReps(S);
##  [ [ Transformation( [ 1, 2, 1, 2, 1 ] ), Transformation( [ 1, 2, 2, 1, 2 ] ), 
##        Transformation( [ 2, 1, 2, 1, 1 ] ) ], 
##    [ Transformation( [ 3, 4, 2, 1, 4 ] ) ] ]
##	</Example><!-- greens.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallOtherMethod(GreensRClassReps,  "for a transformation semigroup", 
true, [IsTransformationSemigroup], 0,
function(X)
GreensRClasses(X);
return GreensRClassReps(X);
end);

#############################################################################
##
##	<#GAPDoc Label="GradedRClasses">
##	<ManSection>
##	<Attr Name="GradedRClasses" Arg="S"/>
##	<Description>
##	returns the <C>GreensRClassData</C> for all the <M>R</M>-classes of <C>S</C> 
##	in a list of lists ordered by the rank of the representatives and the order 
##	that the classes are produced by the function 
##	<Ref Attr="GreensRClasses" BookName="ref"/>. Also <C>GreensRClassReps[i][j]
##	</C> is the representative of <Ref Attr="GradedRClasses"/><C>[i][j]</C>.
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 5, 1, 1, 5, 1 ] ), 
##   Transformation( [ 5, 2, 4, 3, 2 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; GradedRClasses(S);
##  [ [ GreensRClassData( Transformation( [ 5, 1, 1, 5, 1 ] ), [ [ 1, 5 ] ], 
##          [ () ], Group( [ (), (1,5) ] ) ), GreensRClassData( Transformation( 
##          [ 1, 1, 5, 1, 1 ] ), [ [ 1, 5 ] ], [ () ], Group( [ (), (1,5) ] ) ), 
##        GreensRClassData( Transformation( [ 1, 1, 1, 5, 1 ] ), [ [ 1, 5 ] ], 
##          [ () ], Group( [ (), (1,5) ] ) ) ], 
##    [ GreensRClassData( Transformation( [ 5, 2, 4, 3, 2 ] ), [ [ 2, 3, 4, 5 ] 
##           ], [ () ], Group( [ () ] ) ) ], 
##    [ GreensRClassData( Transformation( [ 2, 5, 5, 2, 5 ] ), [ [ 2, 5 ] ], 
##          [ () ], Group( [ () ] ) ), GreensRClassData( Transformation( 
##          [ 5, 2, 2, 5, 2 ] ), [ [ 2, 5 ] ], [ () ], Group( [ () ] ) ), 
##        GreensRClassData( Transformation( [ 5, 5, 2, 5, 5 ] ), [ [ 2, 5 ] ], 
##          [ () ], Group( [ () ] ) ), GreensRClassData( Transformation( 
##          [ 2, 2, 5, 2, 2 ] ), [ [ 2, 5 ] ], [ () ], Group( [ () ] ) ), 
##        GreensRClassData( Transformation( [ 5, 5, 5, 2, 5 ] ), [ [ 2, 5 ] ], 
##          [ () ], Group( [ () ] ) ), GreensRClassData( Transformation( 
##          [ 2, 2, 2, 5, 2 ] ), [ [ 2, 5 ] ], [ () ], Group( [ () ] ) ) ], 
##    [ GreensRClassData( Transformation( [ 2, 2, 3, 4, 2 ] ), [ [ 2, 3, 4 ] ], 
##          [ () ], Group( [ (), (3,4) ] ) ) ], 
##    [ GreensRClassData( Transformation( [ 1, 1, 1, 1, 1 ] ), 
##          [ [ 1 ], [ 5 ], [ 2 ] ], [ (), (1,2,3,4,5), (1,2) ], Group( 
##          [ () ] ) ) ] ]
##	</Example> <!-- greens.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>


InstallOtherMethod(GradedRClasses,  "for a transformation semigroup", 
true, [IsTransformationSemigroup], 0,
function(X)
GreensRClasses(X);
return GradedRClasses(X);
end);

#############################################################################
##
#M  Size( <RClassData> )  
##  
##  Algorithm C.
##   
##  returns the size of <RClassData>
##

InstallOtherMethod(Size,  "size of R-class from data structure", 
true, [IsGreensRClassData], 0,
function(data)
return Size(data!.schutz)*Size(data!.strongorb);
end);

#############################################################################
##
#M  AsSSortedList( <RClassData> )  
##  
##  Algorithm D.
##   
##  returns the elements of <RClassData>
##

InstallOtherMethod(AsSSortedList,"elements of R-class from data structure", 
true, [IsGreensRClassData], 0,
function(R)
local m, x, elts, grp, prod, g;

grp:= R!.schutz;
x:= R!.rep;

elts:= [];
for m in R!.perms do
	for g in grp do 
		prod:=x*(g*m^-1);
		#if not prod in elts then  JDM recent change here!!
			AddSet(elts, prod); 
		#fi;
	od;
od;

SetSize(R, Size(elts));
return elts;
end);

#############################################################################
##
#M  \in
##  
##  Algorithm E.
##   
##  tests membership in an R-class
##

InstallOtherMethod( \in, "membership test for an R-class data", true, [IsTransformation, IsGreensRClassData], 0, 
function(x, R)
local i, rep, grp;

rep:= R!.rep; 

# check degree, rank, and kernel.

if DegreeOfTransformation(x) <> DegreeOfTransformation(rep) or RankOfTransformation(x) <> RankOfTransformation(rep) or KernelOfTransformation(x) <> KernelOfTransformation(rep) then
	return false;
fi;

# check image.
i:= Position(R!.strongorb, ImageSetOfTransformation(x));
if i = fail then 
	return false;
fi;

# check the group.
return PermLeftQuoTransformation(rep, x * R!.perms[i]) in R!.schutz;
end);

#############################################################################
##
#M  Idempotents( <R> ) 
##
##  returns the idempotents in an R-class

InstallOtherMethod( Idempotents, "for R class data", true, [IsGreensRClassData], 0, 
function(R)
local idempotent, ker, img, idm, data;

idm:= [];
ker:= KernelOfTransformation(R!.rep);

# loop over the images.
for img in R!.strongorb do
	# check for cross section.
	if IsTransversal(ker, img) then
		Add(idm, Idempotent(ker, img));
	fi;
od;

# return the list of idempotents.
return idm;
end);

#############################################################################
##
#M  GreensHClasses( <R> ) 
##  
##  returns all the Green's H-classes of a Green's R-class
## 

InstallOtherMethod(GreensHClasses, "for Green's R-class", true, 
[IsGreensRClass], 0, 
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
##
#M DClassOfRClass( <class> ) ;
##
## Green's D-class of Green's R-class
##

InstallOtherMethod(DClassOfRClass, "for a transformation semigroup", true, [IsGreensRClass and IsAssociatedSemigpTransSemigp], 0, 
function(hc)

if HasGreensDClasses(ParentAttr(hc)) then 
	return First(GreensDClasses(ParentAttr(hc)), x-> Representative(hc) in x );
else
	return GreensDClassOfElement(ParentAttr(hc), Representative(hc));
fi;

end);

#############################################################################
##
##	<#GAPDoc Label="GreensLClassData">
##	<ManSection>
##	<Attr Name="GreensLClassData" Arg="S, f"/>
##	<Description>
##	if <C>C</C> satisfies <Ref Attr="IsGreensLClass" BookName="ref"/>, then 
##	<C>GreensLClassData</C> returns an object in the category 
##	<Ref Filt="IsGreensLClassData"/> with representation 
##	<Ref Filt="IsGreensLClassDataRep"/> and the following five components:
##	<List>
##		<Item><C>rep</C> the representative of the <M>L</M>-class</Item>
##		<Item><C>strongorb</C> the strong orbit of the kernel of <C>rep</C> under 
##		the action of the semigroup by <Ref Func="OnTuplesOfSetsAntiAction"/>
##		</Item>
##		<Item><C>relts</C> a list of relations such that 
##		<C>KernelOfTransformation(relts[i]*x)=strongorb[1]</C> whenever <C>x</C> 
##		has <C>KernelOfTransformation(x)=strongorb[i]</C></Item>
##		<Item><C>invrelts</C> the inverses of the relations <C>relts</C> 
##		(<C>KernelOfTransformation(invrelts[i]*rep)=strongorb[i]</C>)</Item>
##		<Item><C>schutz</C> the (generalised) left Schutzenberger group.</Item>
##	</List>
##	<!-- The components <C>strongorb</C>, <C>relts</C>, <C>invrelts</C>, and 
##	<C>schutz</C> are obtained using the function 
##	<Ref Func="StrongOrbitOfKernel"/>.-->
##	Further 
##	details can be found in Algorithm G, H, I, and J of <Cite Key="computing"/>.
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 4, 1, 4, 5, 3 ] ),
##  &gt; Transformation( [ 5, 3, 5, 4, 3 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; C:=GreensLClassOfElement(S, gens[1]*gens[2]*gens[1]);
##  {Transformation( [ 5, 3, 5, 4, 3 ] )}
##  gap&gt; GreensLClassData(C);
##  GreensLClassData( Transformation( [ 5, 3, 5, 4, 3 ] ), 
##  [ [ [ 1, 3 ], [ 2, 5 ], [ 4 ] ] ], [ Binary Relation on 5 points ], 
##  [ Binary Relation on 5 points ], Group( [ (), (3,5,4), (3,5) ] ) )
##	</Example> <!-- greens.tst --> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(GreensLClassData,"data structure of L-class of an element", 
true, [IsGreensLClass and IsAssociatedSemigpTransSemigp], 0,
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
##
#M  GreensLClasses( <transsemigroup> ) 
##  

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

InstallOtherMethod(AsSSortedList,  "elements of L-class from data structure", true, [IsGreensLClassData], 0,
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

InstallOtherMethod(DClassOfLClass, "for a transformation semigroup", true, [IsGreensHClass and IsAssociatedSemigpTransSemigp], 0, 
function(hc)

if HasGreensDClasses(ParentAttr(hc)) then 
	return First(GreensDClasses(ParentAttr(hc)), x-> Representative(hc) in x);
else
	return GreensDClassOfElement(ParentAttr(hc), Representative(hc));
fi;

end);

#############################################################################
##
##	<#GAPDoc Label="GreensHClassData">
##	<ManSection>
##	<Attr Name="GreensHClassData" Arg="S, f"/>
##	<Description>
##	if <C>C</C> satisfies <Ref Attr="IsGreensHClass" BookName="ref"/>, then 
##	<C>GreensLClassData</C> returns an object in the category 
##	<Ref Filt="IsGreensHClassData"/> with representation 
##	<Ref Filt="IsGreensHClassDataRep"/> and the following five components:
##	<List>
##		<Item><C>rep</C> the representative of the <M>H</M>-class</Item>
##		<Item><C>schutz</C> the intersection of the left Schutzenberger group and 
##		right Schutzenberger group of the <M>L</M>-class and <M>R</M>-class 
##		containing the representative <C>rep</C> (that is, the intersection of the 
##		<C>schutz</C> component of <Ref Func="GreensRClassData"/> and the 
##		<C>schutz</C> component of <Ref Func="GreensLClassData"/>).</Item>
##	</List>
##	Further details can be found in Algorithm K, L, M, and N of 
##	<Cite Key="computing"/>.
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 2, 2, 5, 2, 3 ] ), 
##  &gt; Transformation( [ 2, 5, 3, 5, 3 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; f:=Transformation( [ 5, 5, 3, 5, 3 ] );;
##  gap&gt; GreensHClassData(GreensHClassOfElement(S, f));
##  GreensHClassData( Transformation( [ 5, 5, 3, 5, 3 ] ), Group( () ) )
##	</Example> <!-- greens.tst --> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(GreensHClassData, "data structure of H-class of an element", 
true, [IsGreensHClass and IsAssociatedSemigpTransSemigp], 0,
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
true, [IsGreensHClassData], 0, 
x-> Size(x!.schutz));

#############################################################################
##
#F  AsSSortedList( <HClassData> ) 
##  
##  Algorithm M.
##   
##  returns the elements of <HClassData>
##

InstallOtherMethod(AsSSortedList,  "elements of H-class from data structure", 
true, [IsGreensHClassData], 0,
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
## JDM this could be shortened.
 
InstallOtherMethod( Idempotents, "for H class data", true, [IsGreensHClassData], 0,
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
 
InstallOtherMethod(IsGroupHClass, "for a transformation semigroup", true,[IsGreensHClass and IsAssociatedSemigpTransSemigp], 0, 
x-> ImageSetOfTransformation(Representative(x)^2)=ImageSetOfTransformation(Representative(x)));

#############################################################################
##
#M  RClassOfHClass( <class> ) ;
##
##  Green's R-class of Green's H-class
##

InstallOtherMethod(RClassOfHClass, "for a transformation semigroup", true, [IsGreensHClass and IsAssociatedSemigpTransSemigp], 0, 
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

InstallOtherMethod(LClassOfHClass, "for a transformation semigroup", true, [IsGreensHClass and IsAssociatedSemigpTransSemigp], 0, 
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

InstallOtherMethod(DClassOfHClass, "for a transformation semigroup", true, [IsGreensHClass and IsAssociatedSemigpTransSemigp], 0, 
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

InstallMethod(GreensHClasses, "for a transformation semigroup", true,[IsTransformationSemigroup], 0, 
x-> Concatenation(List(GreensDClasses(x), GreensHClasses)));

#############################################################################
##
##	<#GAPDoc Label="GreensDClassData">
##	<ManSection>
##	<Attr Name="GreensDClassData" Arg="S, f"/>
##	<Description>
##	if <C>C</C> satisfies <Ref Attr="IsGreensDClass" BookName="ref"/>, then 
##	<C>GreensDClassData</C> returns an object in the category 
##	<Ref Filt="IsGreensDClassData"/> with representation 
##	<Ref Filt="IsGreensDClassDataRep"/> and the following five components:
##	<List>
##		<Item><C>rep</C> the representative of the <M>D</M>-class</Item>
##		<Item><C>R</C> the result of <Ref Func="GreensRClassData"/> with argument 
##		<C>rep</C></Item>
##		<Item><C>L</C> the result of <Ref Func="GreensLClassData"/> with argument 
##		<C>rep</C></Item>
##		<Item><C>H</C> the result of <Ref Func="GreensHClassData"/> with argument 
##		<C>rep</C></Item>
##		<Item> <C>cosets</C> a transversal of right cosets of the 
##		Schutzenberger group of <C>H</C> in the Schutzenberger group of 
##		<C>R</C>.</Item>
##	</List>
##	Note that only the first three components are displayed. 
##	Further details can be found in Algorithm O, P, Q, R, S, and T of 
##	<Cite Key="computing"/>.
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 4, 1, 5, 2, 4 ] ), 
##  &gt; Transformation( [ 4, 4, 1, 5, 3 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; f:=Transformation( [ 5, 5, 3, 3, 3 ] );;
##  gap&gt; GreensDClassData(GreensDClassOfElement(S, f));
##  GreensDClassData( Transformation( [ 5, 5, 3, 3, 3 
##  ] ), GreensRClassData( Transformation( [ 5, 5, 3, 3, 3 
##  ] ) ), GreensLClassData( Transformation( [ 5, 5, 3, 3, 3 ] ) ) )
##	</Example> <!-- greens.tst --> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(GreensDClassData,  "data structure of D-class of an element", true, [IsGreensDClass and IsAssociatedSemigpTransSemigp], 0,
function(class)
local L, R, H, cosets, data, schutz, rep;

rep:=Representative(class);

L:=GreensLClassData(GreensLClassOfElement(ParentAttr(class), rep));
rep:=Representative(L); # rep has correct kernel
R:=GreensRClassData(GreensRClassOfElement(ParentAttr(class), rep));
rep:=Representative(R); # rep has correct image too
H:=GreensHClassData(GreensHClassOfElement(ParentAttr(class), rep));

schutz:=H!.schutz;
cosets:= RightCosets(R!.schutz, AsSubgroup(R!.schutz, H!.schutz));
Apply(cosets, Representative);

data:=DClassData(rec( rep:=rep, R:=R, L:=L, H:=H, cosets:=cosets, schutz:=schutz));

return data;
end);

######################

InstallOtherMethod(GreensDClassData,  "data structure of D-class of an element", 
true, [IsGreensDClass and IsAssociatedSemigpTransSemigp, IsGreensLClassData], 0,
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
#M  Size( <DClassData> ) 
##  
##  NOT Algorithm P.
##   
##  returns the size of <DClassData>, by dividing |G_L|*|G_R| by |G_H|.
##  

InstallOtherMethod(Size,  "size of D-class from data structure", true, [IsGreensDClassData], 0,
function(data)
return Size(data!.H!.schutz)^-1*Size(data!.R)*Size(data!.L);
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
true, [IsGreensDClassData], 0,
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
#M	\in
##	
##	NOT Algorithm R.
##
##	tests membership in a D-class
##

InstallOtherMethod( \in, "membership test for an D-class", true, 
[IsTransformation, IsGreensDClassData], 0,
function(x,D)
local i, c, rep, ker, img, quo, Rimages, Rmults, Lkers, Lmults, Lschutz, cosets;

# check degree, rank
rep:= D!.rep;

if x=rep then 
	return true;
elif DegreeOfTransformation(x) <> DegreeOfTransformation(rep) or RankOfTransformation(x) <> RankOfTransformation(rep) then
	return false;
fi;

Rimages:=D!.R!.strongorb;
Rmults:=D!.R!.perms;
Lkers:=D!.L!.strongorb;
Lmults:=D!.L!.relts;
Lschutz:=D!.L!.schutz;
cosets:=D!.cosets;

img:= ImageSetOfTransformation(x);
i:= Position(Rimages, img);
if i = fail then
	return false;
fi;

x:= x * Rmults[i];

# check kernel, and adjust.

ker:= KernelOfTransformation(x);
i:= Position(Lkers, ker);

if i = fail then 
	return false;
fi;

x:= AsTransformation(Lmults[i] * x);

# check the (cosets of the) group.
 
quo:= PermLeftQuoTransformation(rep, x);
for c in cosets do
	if quo/c in Lschutz then
		return true;
	fi;
od;

end);

#############################################################################
##
#M  IsRegularDClassData( <Dclassdata> ) 
##  
##  NOT Algorithm S.
##  
##  checks if <Dclassdata> is a regular D-class
##

InstallOtherMethod(IsRegularDClass, "for a Green's D-class of a transformation semigroup", true, [IsGreensDClass and IsAssociatedSemigpTransSemigp], 0, 
x-> IsRegularTransformation(ParentAttr(x), Representative(x)));

#############################################################################
##
#F	Idempotents( <Dclassdata> );
##
##	Algorithm T.
##
##	returns the idempotents of a D-class.
##
##	JDM add new version of this command for regular semigroups
##	JDM check which attempt given below is quicker :)
##	JDM include IsRegularDClass in this calculation, to stop 
##	JDM wasting lots of time calculating that there are no idempotents
##

InstallOtherMethod( Idempotents, "for GreensDClassData", true, [IsGreensDClassData], 0,
function(D)
 # first attempt
local idempotent, ker, img, idempots, Rimages, Lkers, idem;

idempots:= []; 

Rimages:=D!.R!.strongorb; 
Lkers:=D!.L!.strongorb;

#loop over the kernels.
for ker in Lkers do

	# loop over the images.
	for img in Rimages do
		# check for cross section
		if IsTransversal(ker, img) then 
			idem:=Idempotent(ker, img);
			if idem in D then
				Add(idempots, Idempotent(ker, img));
			fi; 
		fi;
	od;
od;

# return the list of idempotents.
return idempots;

# second attempt

# return Concatenation(List(GreensRClassesData(D), Idempotents));

end) ;

#############################################################################
## 
#M  GreensRClasses( <dclass> ) ;
##
##  expands all the R-classes of a D-class
##
##  JDM if all the R-classes are known then couldn't we just test which 
##  JDM representatives of those were in our D-class?
##

InstallOtherMethod(GreensRClasses, "for a GreensDClass of a transformation semigroup", true, [IsGreensDClass], 0, 
function(D)
local M, x, l, c, d, classes, data, class, grp, sch, strongorb, perms, cos;

# initialize
M:= ParentAttr(D); 
classes:= [];
x:=Representative(D); 
data:=GreensDClassData(D);

sch:= AsSubgroup(SchutzenbergerGroup(data!.L), SchutzenbergerGroup(data!.H));

# this gives a set of representatives of left cosets :)
cos:=List(RightCosets(SchutzenbergerGroup(data!.L), sch), x-> Representative(x)^-1);
grp:= SchutzenbergerGroup(data!.R); 
strongorb:=data!.R!.strongorb;
perms:=data!.R!.perms;

# loop over R class reps
for l in data!.L!.invrelts do
	d:= AsTransformationNC(l * x);

	# loop over cosets.
	for c in cos do
		class:=GreensRClassOfElement(M, d*c); 
		SetGreensRClassData(class, RClassData(rec( rep:=d*c, strongorb:=strongorb, perms:=perms, schutz:=grp )));
		SetDClassOfRClass(class, D);
		Add(classes, class);
	od;
od;

return classes;
end);

#############################################################################
##
#M  GreensDClasses( <transsemigroup> ) 
##  
##  Algorithm Y.
##  
##  finds all Green's D-classes of a transformation semigroup.
##

InstallMethod(GreensDClasses, "for a transformation semigroup", true, [IsTransformationSemigroup], 0,
function(X)
local classes, reps, class, repset;

GreensRClasses(X);

reps:=GreensRClassReps(X); classes:= []; 

for repset in reps do 
	repeat
		class:=GreensDClassOfElement(X, repset[1]);
		Add(classes, class);
		repset:= Filtered(repset, x-> not x in class);
	until repset = [];
od;

return classes;
end) ;

#############################################################################
##
#M  GreensLClasses( <dclass> ) 
##  
##  all L-classes of Green's D-class <dclass>.
##

InstallOtherMethod(GreensLClasses, "for transformation semigroups", true,[IsGreensDClass], 0,
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
# JDM this will only work if the representatives of D-classes are all
# JDM representatives of R-classes also.
# JDM why L? check which is faster
# JDM this ought also to account for what has already been calculated. I.e. 
# JDM if the GreensHClasses of GreensRClasses are already known then use them!

InstallOtherMethod(GreensHClasses, "for D-class", true, [IsGreensDClass], 0, 
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

			########################
			##										##
			## Global functions		##
			##										##
			########################

#############################################################################

###########################################################################
##
##	<#GAPDoc Label="Idempotents">
##	<ManSection>
##	<Func Name="Idempotents" Arg="S"/>
##	<Func Name="Idempotents" Arg="S, n"/>
##	<Description>
##	the first version of the function returns a list of the idempotents in 
##	<C>S</C>. The second version of the function returns a list of the 
##	idempotents in <C>S</C> of rank <C>n</C>. If you are only interested in the 
##	idempotents of a given rank, then the second version of the function will 
##	likely be faster.
##	<Example>
##  gap&gt; S:=Semigroup([ Transformation( [ 2, 3, 4, 1 ] ), 
##  &gt; Transformation( [ 3, 3, 1, 1 ] ) ]);;
##  gap&gt; Idempotents(S, 1);
##  [  ]
##  gap&gt; Idempotents(S, 2);                        
##  [ Transformation( [ 1, 1, 3, 3 ] ), Transformation( [ 1, 3, 3, 1 ] ), 
##    Transformation( [ 2, 2, 4, 4 ] ), Transformation( [ 4, 2, 2, 4 ] ) ]
##  gap&gt; Idempotents(S, 3);                        
##  [  ]
##  gap&gt; Idempotents(S, 4);                        
##  [ Transformation( [ 1, 2, 3, 4 ] ) ]
##  gap&gt; Idempotents(S);
##  [ Transformation( [ 1, 1, 3, 3 ] ), Transformation( [ 1, 2, 3, 4 ] ), 
##    Transformation( [ 1, 3, 3, 1 ] ), Transformation( [ 2, 2, 4, 4 ] ), 
##    Transformation( [ 4, 2, 2, 4 ] ) ]
##  gap&gt; Idempotents(S, 5);
##  fail
##	</Example> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallOtherMethod( Idempotents, "for a transformation semigroup", true, 
[IsTransformationSemigroup], 0,
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

InstallOtherMethod(Idempotents, "for a transformation semigroup and positive integer", true, 
[IsTransformationSemigroup, IsPosInt], 0,
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

############################################################################
##
#F	Algorithm V.
## 

InstallOtherMethod(Size, "for a transformation semigroup", true, [IsTransformationSemigroup], 0,
function(M)
return List(GradedRClasses(M),x->Size(x[1]))*List(GreensRClassReps(M), Length);
end) ;

#############################################################################
##
#M  Elements( <M> ) ;
##  
##  Algorithm W.
##
##  The set of elements of a transformation monoid is determined as the union
##  of the element sets of its R classes. 
##

#JDM elts:=Set(Concatentation(List(GreensRClasses(M), Enumerator))); 
#JDM how does GreensRClass known Enumerator before anything is invoked??

InstallOtherMethod(AsSSortedList, "for a transformation semigroup", true, [IsTransformationSemigroup], 0,
function(M)
local elts, rrel;

elts:=Union(List(GreensRClasses(M), Elements));
if not HasSize(M) then
	SetSize(M, Size(elts));
fi;

return elts;
end);

#############################################################################
##
#M  <x> in <M> . . . . . . . . . . . . . . . . . . . . . . . membership test.
##
##  A transformation <x>  lies in a transformation monoid  <M> if it  has the
##  same degree as <M> and if it is contained in one of the R classes of <M>.
##
##  Algorithm X.
##

InstallMethod(\in, "for a transformation semigroup", true,
[IsObject, IsTransformationSemigroup], 0,
function(x, M)
local i, j, k, pos, R, ker, kers, rrel;

# check degree.
if not IsTransformation(x) or DegreeOfTransformation(x) <> DegreeOfTransformationSemigroup(M) then
	return false;
fi;

#JDM new for 3.1.4

if HasAsSSortedList(M) then 
	return x in AsSSortedList(M);
fi;

#JDM end

k:= RankOfTransformation(x);

if k>Maximum(List(GeneratorsOfSemigroup(M), RankOfTransformation)) then 
	return false;
fi;

GreensRClasses(M);
pos:= Position(GradedImagesOfTransSemigroup(M)[k], ImageSetOfTransformation(x));

if pos = fail then
	return false;
fi;

GreensRClasses(M);

# locate representing R class.
j:= PositionsRClasses(M)[k][pos];
R:= GradedRClasses(M)[j];

# check kernels.
ker:= KernelOfTransformation(x);
kers:=InternalKernels(M)[j];

for i in [1..Length(kers)] do
	if kers[i] = ker and x in R[i] then 
		return true;
	fi;
od;

# if that fails.
return false;

end);

#############################################################################
##
##	<#GAPDoc Label="SchutzenbergerGroup">
##	<ManSection>
##	<Attr Name="SchutzenbergerGroup" Arg="D"/>
##	<Description>
##	if <C>D</C> satisfies <Ref Attr="IsGreensRClassData"/>, 
##	<Ref Attr="IsGreensLClassData"/>, <Ref Attr="IsGreensHClassData"/>, or 
##	<Ref Attr="IsGreensDClassData"/>, then <C>SchutzenbergerGroup</C> returns 
##	the <C>schutz</C> component of <C>D</C>. <P/>
##	
##	If <C>D</C> satisfies <Ref Attr="IsGreensRClass" BookName="ref"/>, 
##	<Ref Attr="IsGreensLClass" BookName="ref"/>, 
##	<Ref Attr="IsGreensHClass" BookName="ref"/>, 
##	<Ref Attr="IsGreensDClass" BookName="ref"/>, then <C>SchutzenbergerGroup</C> 
##	returns the <C>schutz</C> component of <C>GreensData</C> with argument
##	<C>D</C>.
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 4, 4, 3, 5, 3 ] ), 
##  &gt; Transformation( [ 5, 1, 1, 4, 1 ] ), 
##  &gt; Transformation( [ 5, 5, 4, 4, 5 ] ) ];;
##  gap&gt; f:=Transformation( [ 4, 5, 5, 5, 5 ] );;
##  gap&gt; SchutzenbergerGroup(GreensDClassOfElement(S, f));
##  Group([ (), (4,5) ])
##  gap&gt; SchutzenbergerGroup(GreensRClassOfElement(S, f));
##  Group([ (), (4,5) ])
##  gap&gt; SchutzenbergerGroup(GreensLClassOfElement(S, f));
##  Group([ (), (4,5) ])
##  gap&gt; SchutzenbergerGroup(GreensHClassOfElement(S, f));
##  Group([ (), (4,5) ])
##	</Example> <!-- greens.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(SchutzenbergerGroup, "for GreensData", true, 
[IsGreensData], 0, x-> x!.schutz );

InstallOtherMethod(SchutzenbergerGroup, "for GreensData", true, 
[IsGreensClass], 0, x-> GreensData(x)!.schutz );


#############################################################################
##
##	<#GAPDoc Label="PartialOrderOfDClasses">
##	<ManSection>
##	<Attr Name="PartialOrderOfDClasses" Arg="S"/>
##	<Description>
##	returns the partial order of the <C>D</C>-classes of <C>S</C> as a directed 
##	graph in <Package>GRAPE</Package> using the command 
##	<Display>
##	Graph(Group(()), [1..Length(GreensDClasses(S))], OnPoints, function(x,y)
##	return y in poset[x]; end, true); ;
##	</Display>
##	where <C>y</C> in <C>poset[x]</C> if and only if 
##	<M>S^1yS^1\subseteq S^1 xS^1</M>.
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

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