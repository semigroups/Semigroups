#############################################################################
##
#W  greens_d_orb.gi
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

# - gap> s:=Semigroup(gens);
#<semigroup with 4 generators>
#gap> Sum(List(GreensDClasses(s), Size));
#87035
# should be 95540!

# - consolidate and clean up what's here already and do some testing!

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
local s, rep, o, i, r_schutz, l_schutz, cosets; 

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

l_schutz:=LClassSchutzGpFromData(s, d!.data[2])[1];

if l_schutz=true then
	return true;
fi;

f:= PermLeftQuoTransformationNC(rep, f);
r_schutz:= RClassSchutzGpFromData(s, d!.data[1])[2];
cosets:= DClassSchutzGpFromData(s, d!.data)[3]; 
# consider writing a DClassCosetsFromData and DClassSchutzGpStabChainFromData commands!
#cosets of intersection of r_schutz and l_schutz in r_schutz

for i in cosets do
	if SiftedPermutation(l_schutz, f/i)=() then
		return true;
	fi;
od;

return false;
end);

#AsList

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

#Info(InfoMonoidGreens, 4, "DClassRepsData");

return rec(
  finished:=false,
	data:=[]);
end);

#new for 3.2!
#############################################################################
# test for efficiency! JDM

InstallGlobalFunction(DClassSchutzGpFromData, 
function(s, d)
local O, g, h, dd;
#Info(InfoMonoidGreens, 4, "DClassSchutzGpFromData");

dd:=d[1]; 
O:=OrbitsOfImages(s)!.orbits;

if not IsBound(O[dd[1]][dd[2]]!.d_schutz[dd[4]][dd[5]][dd[6]]) then 
	g:=RClassSchutzGpFromData(s, d[1])[2];
	h:=Intersection(g, LClassSchutzGpFromData(s, d[2])[2]);
	O[dd[1]][dd[2]]!.d_schutz[dd[4]][dd[5]][dd[6]]:=[StabChainImmutable(g), 
	 g, List(RightCosets(g,h), Representative)]; 
	#JDM good idea to compute the stab. chain and cosets here?
fi;

return O[dd[1]][dd[2]]!.d_schutz[dd[4]][dd[5]][dd[6]];
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
local d_img, d_ker, rep, type, c, d;

#Info(InfoMonoidGreens, 4, "GreensDClassOfElement");

if not f in s then 
	Info(InfoWarning, 1, "transformation is not an element of the semigroup");
	return fail;
fi;

d_img:=InOrbitsOfImages(s, f)[2];
d_img[3]:=1;
rep:=d_img[7]; #the image of the rep lies in the first position of the scc 
# containing it!
d_ker:=InOrbitsOfKernels(s, rep);

if not d_ker[1] then #orbit of kernel not previously calculated!
	d_ker:=AddToOrbitsOfKernels(s, rep, d_ker[2]);
	d:=DClassRepsData(s)!.data;
	d[Length(d)+1]:=[d_img, d_ker];
fi;

type:=NewType( FamilyObj( s ), IsEquivalenceClass and 
	 IsEquivalenceClassDefaultRep and IsGreensDClass and 
	 IsGreensClassOfTransSemigp);

c:=Objectify( type, rec(parent:=s, data:=[d_img, d_ker], rep:=rep));
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

Info(InfoWarning, 2, "Warning: this function does not check that the transformation");
Info(InfoWarning, 2, "is an element of the semigroup. Do not use this function unless");
Info(InfoWarning, 2, "you are certain that the transformation is an element of the");
Info(InfoWarning, 2, "semigroup. If you are unsure, use GreensDClassOfElement instead.");
Info(InfoWarning, 2, "Using this function with a transformation that is not an element of"); 
Info(InfoWarning, 2, "the semigroup will have unpredictable effects on this and all");
Info(InfoWarning, 2, "subsequent computings involving the semigroup.");


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
	Info(InfoMonoidGreens, 2, "trans. is not an element of ", s);
	Info(InfoWarning, 1, "transformation is not an element of the semigroup");
	return fail;#
else 
	Info(InfoMonoidGreens, 2, "trans. may not be an element of ", s);
	#do whatever we do in the IteratorOfDClassReps algorithm...
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
d-> IsRegularRClassData(d!.parent, d!.data[1]));


#############################################################################
# JDM test the below for efficiency

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
			
			r:=IteratorOfRClassReps(s), 
			
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
			local f, o, d_img, d_ker, d;
		
			f:=NextIterator(iter!.r);
			
			if f=fail then 
				DClassRepsData(s)!.finished:=true;
				return fail;
			fi;
			
			o:=OrbitsOfImages(iter!.s);
			d_img:=f![4][1]; #the data for f!
			#JDM bad idea replace the iterator with an iterator of DClassRepsData!
			
			d_ker:=InOrbitsOfKernels(s, f);
			
			# R-class reps always have image in the first position of the 
			# scc containing their image hence we do not need to multiply f
			# by one of the perms to rectify its image.
			
			if not d_ker[1] then #this is a new element!
				d_ker:=AddToOrbitsOfKernels(s, f, d_ker[2]);
				d:=DClassRepsData(s)!.data;
				d[Length(d)+1]:=[d_img, d_ker];
				return f;
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
	local c, rep;
	
	rep:=NextIterator(iter!.reps);
	
	if rep=fail then 
		return fail;
	fi;
	
	iter!.i:=iter!.i+1;
	
	#c:=GreensDClassOfElement(iter!.s, rep, type);
	c:=Objectify( iter!.type, rec(parent:=s, 
	 data:=DClassRepsData(iter!.s)!.data[iter!.i], rep:=rep));
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
local s, ker, img;

s:=iter!.s;
ker:=OrbitsOfKernels(s);
img:=OrbitsOfImages(s);

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

# new for 3.2!
############################################################################

InstallOtherMethod(SchutzenbergerGroup, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
d-> DClassSchutzGpFromData(d!.parent, d!.data));

#JDM is this correct? Compare it to SchutzenbergerGroup of R-class

# new for 3.2!
#############################################################################

InstallOtherMethod(Size, "for an D-class of a trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
local r, l;
d:=d!.data;
r:=RClassSchutzGpFromData(s, d[1])[2];
l:=LClassSchutzGpFromData(s, d[2])[2];
return Size(r)*Length(RClassSCCFromData(s, d[1]))
*Length(LClassSCCFromData(s, d[2]))*Size(l)/Size(DClassSchutzGpFromData(s, d)[2]);
end);


#############################################################################
# JDM the following is very slow as it has to find the intersection of the 
# the schutz gps every time. Improve this!

InstallGlobalFunction(SizeDClassRepsData, 
function(s)
local data, i, d, l, r;

data:=DClassRepsData(s)!.data;
i:=0;

for d in data do
	r:=RClassSchutzGpFromData(s, d[1])[2];
	l:=LClassSchutzGpFromData(s, d[2])[2];
	i:=i+(Size(r)*Length(RClassSCCFromData(s,d[1]))
	 *Length(LClassSCCFromData(s, d[2]))*Size(l)/Size(DClassSchutzGpFromData(s, d))[2]);
od;

return i;
end);