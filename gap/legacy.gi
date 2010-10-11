
#############################################################################

InstallMethod(SchutzenbergerGroup, "for GreensData",
[IsGreensData], x-> x!.schutz );

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
#JDM this should be removed to legacy.gi

InstallMethod(GreensHClassData, "data structure of H-class of an element", 
true, [IsGreensHClass and IsGreensClassOfTransSemigp], 0,
function(class)
local R, L, data;

R:= GreensRClassData(GreensRClassOfElement(ParentAttr(class), Representative(class))); 
L:= GreensLClassData(GreensLClassOfElement(ParentAttr(class), Representative(class)));

data:=HClassData(rec( rep:=Representative(class),  schutz:=Intersection(R!.schutz, L!.schutz)));

return data;
end) ;


###########################################################################
# move to legacy.gi JDM

InstallGlobalFunction(HClassData, function(list)
return Objectify(NewType(NewFamily("Green's H Class Data", IsGreensHClassData), 
IsGreensHClassData and IsGreensHClassDataRep), list);
end);

#############################################################################
# move to legacy.gi JDM

InstallMethod( ViewObj, "for object in `IsGreensHClassData'",
[ IsGreensHClassData and IsGreensHClassDataRep],
function( obj )
Print( "GreensHClassData( ", obj!.rep, ", ", obj!.schutz, " )" );
end );

## Commands to integrate MONOID functions with those in semirel.g* ##
#####################################################################
# move to legacy.gi JDM

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
# move to legacy.gi JDM

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
# move to legacy.gi JDM

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
# move to legacy.gi JDM

InstallOtherMethod(Representative, "for GreensData", 
[IsGreensData], x-> x!.rep);


#############################################################################
# 

InstallMethod(GreensDClassData,  "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
local rep, s, l, r, h, o;

Info(InfoWarning, 1, "this is a legacy from Monoid 3");

rep:=d!.rep;
s:=d!.parent;
o:=d!.o[1];
d:=d!.data;

l:=GreensLClassData(GreensLClassOfElement(s, rep));
r:=GreensRClassData(GreensRClassOfElement(s, rep));
h:=GreensHClassData(GreensHClassOfElement(s, rep));

return DClassData(rec( rep:=rep, R:=r, L:=l, H:=h, 
 cosets:=DClassRCosetsFromData(s, d, o), schutz:=SchutzenbergerGroup(h)));;
end);

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
