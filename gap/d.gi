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

# - d_schutz etc should be moved from OrbitsOfImages to OrbitsOfKernels

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

cosets:= List(RcapLSchutzGpCosetsInRFromData(s, d!.o[1], d!.data), x-> x^p); 

for i in cosets do
	if SiftedPermutation(l_schutz, (f/i)^p)=() then
		return true;
	fi;
od;

return false;
end);

#AsList

#new for 3.2!
#############################################################################
# JDM put in correct position in this file
# MNMN how to make RcapLSchutzGpCosetsInRFromData faster?!

InstallGlobalFunction(RcapLSchutzGpCosetsInRFromData, 
function(s, o, d)
local g, h, right;

if not IsBound(o!.d_schutz[d[1][4]][d[1][5]][d[1][6]]) then 
	o!.d_schutz[d[1][4]][d[1][5]][d[1][6]]:=[];
fi;

if not IsBound(o!.d_schutz[d[1][4]][d[1][5]][d[1][6]][3]) then 
	h:=RcapLSchutzGpFromData(s, o, d);
	g:=RClassSchutzGpFromData(s, d[1], o);
	if not Size(g)=1 then 
		right:=RightTransversal(g, h);
		right:=right{[2..Length(right)]};
	else
		right:=[];
	fi;
	o!.d_schutz[d[1][4]][d[1][5]][d[1][6]][3]:=right;
fi;

return o!.d_schutz[d[1][4]][d[1][5]][d[1][6]][3];
end);

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

#new for 3.2!
#############################################################################
# test for efficiency! JDM 
# MNMN how to make RcapLSchutzGpFromData faster?!
# JDM put in correct position in this file

InstallGlobalFunction(RcapLSchutzGpFromData, 
function(s, o, d)
local g, h, stab, p;

#o should be RClassImageOrbitFromData

if not IsBound(o!.d_schutz[d[1][4]][d[1][5]][d[1][6]]) then
	o!.d_schutz[d[1][4]][d[1][5]][d[1][6]]:=[];
fi;

if not IsBound(o!.d_schutz[d[1][4]][d[1][5]][d[1][6]][2]) then
	g:=RClassSchutzGpFromData(s, d[1], o);
	if not Size(g)=1 then 
		h:=LClassSchutzGpFromData(s, d[2]);
		if not Size(h)=1 then 
			stab:=LClassStabChainFromData(s, d[2]);
			if not stab=true then 
				p:=KerRightToImgLeft(s, d[2])^-1;
				h:=SubgroupProperty(g, x -> SiftedPermutation(stab, x^p)=());
			else
				p:=KerRightToImgLeft(s, d[2])^-1;
				h:=SubgroupProperty(g, x -> x^p in h);
			fi;
		fi;
	else
		h:=g;
	fi;
	o!.d_schutz[d[1][4]][d[1][5]][d[1][6]][2]:=h; 
fi;

return o!.d_schutz[d[1][4]][d[1][5]][d[1][6]][2];
end);

#new for 3.2!
#############################################################################
#
# JDM put in correct position in this file

InstallGlobalFunction(RcapLStabChainFromData, 
function(s, o, d)

if not IsBound(o!.d_schutz[d[1][4]][d[1][5]][d[1][6]]) then 
	o!.d_schutz[d[1][4]][d[1][5]][d[1][6]]:=[];
fi;

if not IsBound(o!.d_schutz[d[1][4]][d[1][5]][d[1][6]][1]) then
	o!.d_schutz[d[1][4]][d[1][5]][d[1][6]][1]:=
	 StabChainImmutable(RcapLSchutzGpFromData(s, o, d)); 
fi;

return o!.d_schutz[d[1][4]][d[1][5]][d[1][6]][1];
end);


#new for 3.2!
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
 cosets:=RcapLSchutzGpCosetsInRFromData(s, o, d), schutz:=SchutzenbergerGroup(h)));;
end);

# new for 3.2!
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

# new for 3.2!
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
d_ker:=InOrbitsOfKernels(s, rep, OrbitsOfKernels(s)!.orbits, 
[d_img[1], fail, fail, fail, fail, 0, fail], d_img);

if not d_ker[1] then #orbit of kernel not previously calculated!
	d_ker:=AddToOrbitsOfKernels(s, rep, d_ker[2]);
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

# new for 3.2!
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
		d[2]:=AddToOrbitsOfKernels(s, f, d[2][2]);
		rep:=DClassRepFromData(s, [d[1], d[2]]);
	fi;
elif OrbitsOfImages(s)!.finished then #f not in s!
	Info(InfoMonoidGreens, 2, "transformation is not an element of the semigroup");
	return fail;
else 
	Info(InfoMonoidGreens, 2, "transformation may not be an element of the semigroup");
	Error("Not yet implemented!"); #JDM
	d[1]:=AddToOrbitsOfImages(s, f, d[1][2]); 
	d[2]:=AddToOrbitsOfKernels(s, f, d[2][2]);
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


# new for 3.2!
#############################################################################

InstallMethod(GreensDClassReps, "for a trans. semigroup", 
[IsTransformationSemigroup], 
function(s)

ExpandDClassRepsData(s);
return List(DClassRepsData(s)!.data, x-> DClassRepFromData(s, x));
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
		d_ker:=AddToOrbitsOfKernels(s, f, d_ker[2]);
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

# new for 3.2!
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

# new for 3.2!
#############################################################################

InstallMethod(NrGreensDClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)

Info(InfoMonoidGreens, 4, "NrGreensDClasses");
ExpandDClassRepsData(s);
return Length(DClassRepsData(s)!.data);
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

# new for 3.2!
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


# new for 3.2!
############################################################################

InstallOtherMethod(SchutzenbergerGroup, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
d-> RcapLSchutzGpFromData(d!.parent, d!.o[1], d!.data));

#JDM is this correct? Compare it to SchutzenbergerGroup of R-class...

# new for 3.2!
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
Size(RcapLSchutzGpFromData(s, o[1], d));
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
	 Size(RcapLSchutzGpFromData(s, o_r, d)));
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
