#############################################################################
##
#W  h.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$

# Make sure all the description strings make sense and that they are uniform...


InstallMethod( \=, "for H-class and H-class of trans. semigp.",
[IsGreensHClass and IsGreensClassOfTransSemigp, IsGreensHClass and 
IsGreensClassOfTransSemigp],
function(h1, h2)
return h1!.parent=h2!.parent and h1!.rep in h2;
end);

############################################################################
#test if it's quicker to test h!.rep in r or vice versa 

InstallMethod( \=, "for R-class and H-class of trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp, IsGreensHClass and 
IsGreensClassOfTransSemigp],
function(r, h)
return r!.parent=h!.parent and h!.rep in r and Size(h)=Size(r);
end);

############################################################################

InstallMethod( \=, "for H-class and R-class of trans. semigp.",
[IsGreensHClass and IsGreensClassOfTransSemigp, IsGreensRClass and 
IsGreensClassOfTransSemigp],
function(h, r)
return r!.parent=h!.parent and h!.rep in r and Size(h)=Size(r);
end);


############################################################################

InstallMethod( \=, "for L-class and H-class of trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp, IsGreensHClass and 
IsGreensClassOfTransSemigp],
function(l, h)
return l!.parent=h!.parent and h!.rep in l and Size(h)=Size(l);
end);

############################################################################

InstallMethod( \=, "for H-class and L-class of trans. semigp.",
[IsGreensHClass and IsGreensClassOfTransSemigp, IsGreensLClass and 
IsGreensClassOfTransSemigp],
function(h, l)
return l!.parent=h!.parent and h!.rep in l and Size(h)=Size(l);
end);

# new for 4.0!
############################################################################

InstallOtherMethod( \in, "for trans. and H-class of trans. semigp.",
[IsTransformation, IsGreensHClass and IsGreensClassOfTransSemigp], 
function(f, h)
local rep;

rep:= h!.rep; 

if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or 
 RankOfTransformation(f) <> RankOfTransformation(rep) or 
 KernelOfTransformation(f) <> KernelOfTransformation(rep) or 
 ImageSetOfTransformation(f) <> ImageSetOfTransformation(rep) then
	return false;
fi;

return PermLeftQuoTransformationNC(rep, f) in SchutzenbergerGroup(h);  
end);

# new for 4.0!
#############################################################################
# this should be removed after the library method for AsSSortedList 
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)!

InstallOtherMethod(AsSSortedList, "for H-class of trans. semigp.",
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
Info(InfoMonoidGreens, 4, "AsSSortedList: for an H-class");
return ConstantTimeAccessList(EnumeratorSorted(h));
end);

# new for 4.0!
#############################################################################
#JDM should be s, rep, data, orbit where orbit is optional and the default
# is OrbitsOfImages(s), OrbitsOfKernels(s)

# data should be D-class data [d_img, d_ker] followed by [l, c, l'] where
# l is the position of img of rep in D-class R-class 
# c is the coset used to prove rep in D-clas
# l' is the position of ker of rep in D-class L-class

InstallGlobalFunction(CreateHClass, 
function(s, data, orbit, rep)
local d, h;

d:=[];
d[1]:=data[1]{[1..6]}; d[2]:=data[2]{[1..6]}; d[3]:=data[3];

h:=Objectify(HClassType(s), rec(parent:=s, data:=d, 
o:=orbit, rep:=rep));
SetRepresentative(h, rep);
SetEquivalenceClassRelation(h, GreensHRelation(s));
return h;
end);

#############################################################################
#

InstallOtherMethod(Enumerator, "for H-class of trans. semigp.", 
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
local enum;

Info(InfoMonoidGreens, 4, "Enumerator: for an H-class");

enum:=EnumeratorByFunctions(h, rec(
	
	schutz:=Enumerator(SchutzenbergerGroup(h)),
	
	###########################################################################
	
	ElementNumber:=function(enum, pos)
	local q, n, m, l;
	if pos>Length(enum) then 
		return fail;
	fi;
	
	return h!.rep*enum!.schutz[pos];
	end, 
	
	###########################################################################
	
	NumberElement:=function(enum, f)
	local rep, d, s, o, i, j, l;
	rep:=h!.rep;

	if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or
	 RankOfTransformation(f) <> RankOfTransformation(rep) or
	 ImageSetOfTransformation(f) <> ImageSetOfTransformation(rep) or
	 KernelOfTransformation(f) <> KernelOfTransformation(rep) then
		return fail;
	fi;
	
	return Position(enum!.schutz, PermLeftQuoTransformationNC(rep, f));
	end, 

	###########################################################################
	
	Membership:=function(elm, enum) 
	return elm in UnderlyingCollection(enum); #the H-class itself!
	end,
	
	Length:=enum -> Size(UnderlyingCollection(enum)),

	PrintObj:=function(enum)
	Print( "<enumerator of H-class>");
	return;
	end));

return enum;
end);

# new for 4.0!
############################################################################
# JDM clean the following up as per the comments in GreensLClassOfElement!

InstallMethod(GreensHClassOfElement, "for a trans. semigp. and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
local d, l;

Info(InfoMonoidGreens, 4, "GreensHClassOfElement");

if not f in s then 
	Info(InfoWarning, 1, "transformation is not an element of the semigroup");
	return fail;
fi;

d:=InOrbitsOfKernels(s, f);
l:=d[3][1][3];

if not d[2] then #D-class containing f not previously calculated
	d[3][1][3]:=RClassSCCFromData(s, d[3][1])[1];
	d:=StructuralCopy(AddToOrbitsOfKernels(s, d[3][1][7], d[3])); 
	d[2][8]:=1; #JDM this should probably go into AddToOrbitsOfKernels..
else
	d:=d[3];
fi;

Add(d, [l, d[2][8]]);

return CreateHClass(s, d, [OrbitsOfImages(s), OrbitsOfKernels(s)], 
 HClassRepFromData(s, d));
end);

# new for 4.0!
#############################################################################
# 

InstallOtherMethod(GreensHClassOfElementNC, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
local d, o1, o2, j, data;

Info(InfoMonoidGreens, 4, "GreensLClassOfElementNC");

d:=InOrbitsOfKernels(s, f);

if d[1] then 
	data:=[d[3][1]];
	Info(InfoMonoidGreens, 2, "transformation is an element of the semigroup");
  #the following is somewhat inefficient as we run f in s and InOrbitsOfKernels
  #again. JDM perhaps improve if necessary.
  
	return GreensHClassOfElement(s, f);
	
elif OrbitsOfImages(s)!.finished then #f not in s!
	Info(InfoMonoidGreens, 2, "transformation is not an element of the ",
	 "semigroup");
	return fail;
fi;

Info(InfoMonoidGreens, 2, "transformation may not be an element of the ",
 "semigroup");

#JDM the following might be more efficient if we only computed the 
#Schutz groups and took their intersection...

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

return CreateHClass(s, [d, d, [1,1]], [o1, o2], f);
end);

#new for 4.0!
#############################################################################
# JDM test!

InstallOtherMethod(GreensLClass, "for an H-class of a trans. semigroup", 
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
local s, d, o, rep;

s:=h!.parent;
d:=h!.data;
o:=h!.o;
rep:=LClassRepFromData(s, d, o);

return CreateLClass(s, d, o, rep);
end);

#new for 4.0!
#############################################################################
# JDM test!

InstallOtherMethod(GreensRClass, "for an H-class of a trans. semigroup", 
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
local s, d, o, rep;

s:=h!.parent;
d:=h!.data;
o:=h!.o;
rep:=RClassRepFromData(s, d[1], o[1]);

return CreateRClass(s, d[1], o[1], rep);
end);



############################################################################

InstallGlobalFunction(HClass, 
function(arg)

if IsTransformationSemigroup(arg[1]) and IsTransformation(arg[2]) then 
	return GreensHClassOfElement(arg[1], arg[2]);
fi;
Error("Usage: trans. semigp. and trans.");

return fail;
end);


# new for 4.0!
############################################################################

InstallMethod(HClassType, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s);

return NewType( FamilyObj( s ), IsEquivalenceClass and 
	 IsEquivalenceClassDefaultRep and IsGreensHClass and 
	 IsGreensClassOfTransSemigp);
end);


# new for 4.0!
############################################################################

InstallGlobalFunction(HClassRepFromData,
function(arg)
local s, d, f, o, perms, cosets, rels;

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3];
else
	o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];
fi;

f:=DClassRepFromData(s, d, o);
perms:=RClassPermsFromData(s, d[1], o[1]);
rels:=LClassRelsFromData(s, d[2], o[2]); 
#JDM is the above the syntax correct? Surely not!!
cosets:=DClassRCosetsFromData(s, d, o);

return rels[d[2][3]][1]*f*(cosets[d[3][2]]/perms[d[3][1]]);
end);

# new for 4.0!
############################################################################
# 

InstallOtherMethod(Idempotents, "for an H-class of a trans. semigp.", 
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
local f; 

if IsGroupHClass(h) then 
	f:=h!.rep;
	return [f*AsPermutation(f, ImageSetOfTransformation(f))^-1];
fi;

return [];
end);

# new for 4.0!
############################################################################
# JDM compare the following to IsRegularRClass..

InstallOtherMethod(IsGroupHClass, "for an H-class of a trans. semigp.", 
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
local f, i, j;

f:=h!.rep;
i:=ImageListOfTransformation(f);
j:=ImageSetOfTransformation(f); 

#JDM loop rather than form i{j} and then testing..
return IsDuplicateFreeList(i{j});
end);

# new for 4.0!
############################################################################

InstallMethod(ParentAttr, "for H-class of a trans. semigroup", 
[IsGreensHClass and IsGreensClassOfTransSemigp], x-> x!.parent);

# new for 4.0!
############################################################################
# JDM clean up! verify! test! clean up!

InstallOtherMethod(SchutzenbergerGroup, "for an H-class of a trans. semigp.",
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
local d, o, perms, cosets, s;
d:=h!.data;
o:=h!.o;
s:=h!.parent;
perms:=RClassPermsFromData(s, d[1], o[1]);
cosets:=DClassRCosetsFromData(s, d, o);

return DClassSchutzGpFromData(h!.parent, h!.data, h!.o)^
 (cosets[d[3][2]]/perms[d[3][1]]);
end); #JDM correct?

# new for 4.0!
############################################################################
#

InstallMethod(Size, "for an H-class of a trans. semigp.",
[IsGreensHClass and IsGreensClassOfTransSemigp],
h-> Size(DClassSchutzGpFromData(h!.parent, h!.data, h!.o)));


# new for 4.0!g
############################################################################
#

InstallOtherMethod(StructureDescription, "for group H-class of trans. semigp.",
[IsGreensHClass and IsGreensClassOfTransSemigp and IsGroupHClass],
h-> StructureDescription(Range(IsomorphismPermGroup(h))));





