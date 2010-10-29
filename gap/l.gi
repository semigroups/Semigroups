#############################################################################
##
#W  l.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$

#############################################################################
## Notes

# - this file is alphabetized, keep it that way!

# - this file should only contain functions relating to kernels/l-classes!

# - should be cleaned up like r.gi!!! In particular, standardise the inputs!

# - 
# greenslclassdata for legacy.gi 

# - require methods for \= between l-classes and other types of classes

# Conventions

# -low-level function make as few functions calls as possible, higher level ones
# must use LClassSchutzGp... etc.

#############################################################################

InstallMethod( \=, "for L-class and L-class of trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp, IsGreensLClass and 
IsGreensClassOfTransSemigp],
function(l1, l2)
return l1!.parent=l2!.parent and l1!.rep in l2;
end);

############################################################################

InstallMethod( \<, "for L-class and L-class of trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp, IsGreensLClass and 
IsGreensClassOfTransSemigp],
function(h1, h2)
return h1!.parent=h2!.parent and h1!.rep < h2!.rep;
end);

#############################################################################
# JDM not sure the following works! Test lots more! Clean up!

InstallOtherMethod( \in, "for trans. and L-class of trans. semigp.",
[IsTransformation, IsGreensLClass and IsGreensClassOfTransSemigp], 
function(f, l)
local rep, d, s, o, i, schutz, ol, perms, cosets;

rep:=l!.rep;

if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or 
 RankOfTransformation(f) <> RankOfTransformation(rep) or 
  ImageSetOfTransformation(f) <> ImageSetOfTransformation(rep) then
	return false;
fi;

if f=rep then 
	return true; 
fi;

d:=l!.data;
s:=l!.parent;
o:=l!.o;

ol:=LClassKernelOrbitFromData(s, d, o);
i:=Position(ol, KernelOfTransformation(f));

if i = fail or not ol!.truth[d[2][4]][i] then 
	return false;
fi;

schutz:=LClassStabChainFromData(s, d[2], l!.o[2]);

if schutz=true then 
	return true;
fi;

rep:=DClassRepFromData(s, d, l!.o);

perms:=RClassPermsFromData(s, d[1], o[1]);
cosets:=DClassRCosetsFromData(s, d, o);

return schutz=true or SiftedPermutation(schutz, 
 PermRightQuoTransformationNC(rep, 
  o!.rels[i][2]*f*(cosets[d[3][2]]/perms[d[3][1]])^-1));
end);

# new for 4.0!
#############################################################################
# this should be removed after the library method for AsSSortedList 
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)!

InstallOtherMethod(AsSSortedList, "for L-class of trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
Info(InfoMonoidGreens, 4, "AsSSortedList: for an L-class");
return ConstantTimeAccessList(EnumeratorSorted(l));
end);

# new for 4.0!
#############################################################################
#JDM should be s, rep, data, orbit where orbit is optional and the default
# is OrbitsOfImages(s), OrbitsOfKernels(s)

# data is the D-class data [d_img, d_ker] then [l, c] where l is the position
# of the image of the rep in the R-class image orbit of the D-class
# and c is the coset used to prove that the rep is in the D-class

InstallGlobalFunction(CreateLClass, 
function(s, data, orbit, rep)
local l, d;

d:=[];
d[1]:=data[1]{[1..6]}; d[2]:=data[2]{[1..6]}; d[3]:=data[3];

l:=Objectify(LClassType(s), rec(parent:=s, data:=d, 
o:=orbit, rep:=rep));
SetRepresentative(l, rep);
SetEquivalenceClassRelation(l, GreensLRelation(s));
return l;
end);

#############################################################################
#

InstallOtherMethod(DClassOfLClass, "for an L-class of a trans. semigroup",
[IsGreensLClass and IsGreensClassOfTransSemigp], 
l-> GreensDClass(l));

#############################################################################
#

InstallOtherMethod(Enumerator, "for L-class of trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
local enum, h;

Info(InfoMonoidGreens, 4, "Enumerator: for an L-class");

h:=List(Elements(SchutzenbergerGroup(l)), x-> l!.rep*x);
# probably the prev. line is a bad idea JDM also in 
# Enumerator for an R-class...

enum:=EnumeratorByFunctions(l, rec(
	
	rep:=l!.rep,
	
	h:=h, 
	
	len:=Length(h),
	
	###########################################################################
	
	ElementNumber:=function(enum, pos)
	local q, n, m, l;
	if pos>Length(enum) then 
		return fail;
	fi;
	
	if pos<=enum!.len then 
		return enum!.h[pos];
	fi;
	
	n:=pos-1;
	m:=enum!.len;
	
	q := QuoInt(n, m);
	pos:= [ q, n - q * m ]+1;
	
	l:=UnderlyingCollection(enum);

	return LClassRels(l)[LClassSCC(l)[pos[1]]][1]*enum!.h[pos[2]];
	end, 
	
	###########################################################################
	
	NumberElement:=function(enum, f)
	local rep, d, s, o, i, j, l;
	rep:=enum!.rep;

	if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or
	 RankOfTransformation(f) <> RankOfTransformation(rep) or
	 ImageSetOfTransformation(f) <> ImageSetOfTransformation(rep) then
		return fail;
	fi;
	
	if f=rep then 
		return 1;
	fi;
	
	l:=UnderlyingCollection(enum);
	d:=l!.data;
	s:=l!.parent;
	
	# check image is in the same weak orbit
	o:= LClassKernelOrbit(l);
	i:= Position(o, KernelOfTransformation(f));
	
	if i = fail or not o!.truth[d[2][4]][i] then #check they are in the same scc
		return fail;
	fi;
	
	j:= Position(Elements(SchutzenbergerGroup(l)),
	 PermLeftQuoTransformationNC(rep, o!.rels[i][2]*f));
	
	if j = fail then 
		return fail;
	fi;
	
	return Length(enum!.h)*(Position(LClassSCC(l), i)-1)+j;

	end, 

	###########################################################################
	
	Membership:=function(elm, enum) 
	return elm in UnderlyingCollection(enum); #the L-class itself!
	end,
	
	Length:=enum -> Size(UnderlyingCollection(enum)),

	PrintObj:=function(enum)
	Print( "<enumerator of L-class>");
	return;
	end));

return enum;
end);

#############################################################################
# JDM can't quite figure out how to get the right data here. 
# probably want to have the last component of h!.data being a pair
# where the first is the Lcoset used to find the rep and the 
# second is the Rcoset used to find the rep. In HClassRepFromData, 
# we use the Rcoset if h!.data[3][2] is bound and the Lcoset if h!.data[3][1]
# is bound!

InstallOtherMethod(GreensHClasses, "for an L-class of a trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(l)
local s, o, m, out, i, data, f, h;

s:=l!.parent; o:=l!.o; m:=NrGreensHClasses(l);
data:=l!.data; out:=EmptyPlist(m); data[3]:=[];


for i in [1..m] do 
	#data:=GreensHClassRepsData(l)[i]; 
	f:=GreensHClassReps(l)[i]; 
	#if HasGreensHClassReps(l) then 
	#	f:=GreensHClassReps(l)[i];
	#else
	#	f:=HClassRepFromData(s, data, o);
	#fi;
	h:=CreateHClass(s, data, o, f);
	SetGreensLClass(h, l);
	SetGreensDClass(h, GreensDClass(l));
	out[i]:=h;
od;

return out;
end);

#############################################################################

InstallOtherMethod(GreensHClassReps, "for an L-class of a trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
local f, cosets, rels, out, k, i, j;

f:=l!.rep;
cosets:=DClassLCosets(GreensDClass(l));
rels:=LClassRels(l);

out:=EmptyPlist(Length(rels)*Length(cosets));
SetNrGreensHClasses(l, Length(rels)*Length(cosets));
k:=0;

for i in rels do 
	i:=i[1]*f;
	for j in cosets do 
		k:=k+1;
		out[k]:=i*j;
	od;
od;

return out;
end);

#############################################################################

InstallOtherMethod(GreensHClassRepsData, "for an L-class of a trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
local f, scc, d, m, out, k, data, i, j;

Info(InfoWarning, 1, "this does not return the correct answer!");

f:= l!.rep;
scc:=LClassSCC(l);
d:=GreensDClass(l);
m:=Length(DClassLCosets(d));

out:=EmptyPlist(Length(scc)*m);
SetNrGreensHClasses(l, Length(scc)*m);

k:=0;
data:=l!.data;

for i in scc do 
	for j in [1..m] do 
		k:=k+1;
		out[k]:=StructuralCopy(data);
		out[k][2][3]:=i;
		out[k][3]:=[data[3][1], j]; #JDM the j can't be correct here!
	od;
od;

return out;
end);

# new for 4.0!
#############################################################################
# JDM test the efficiency of this function!

InstallMethod(GreensLClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local iter, out, i, f;

Info(InfoMonoidGreens, 4, "GreensLClasses");

iter:=IteratorOfGreensLClasses(s);
out:=EmptyPlist(NrGreensLClasses(s));
i:=0;

for f in iter do 
	i:=i+1;
	out[i]:=f;
od;

return out;
end);

# new for 4.0!
#############################################################################
# JDM test this! this for sure needs to be cleaned up!

InstallOtherMethod(GreensLClassOfElement, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
local d, data, l;

Info(InfoMonoidGreens, 4, "GreensLClassOfElement");

if not f in s then 
	Info(InfoWarning, 1, "transformation is not an element of the semigroup");
	return fail;
fi;

d:=InOrbitsOfKernels(s, f);
l:=d[3][1][3]; #l not fail since we've done f in s!

if not d[2] then #D-class containing f not previously calculated
	# store the D-class containing f
	d[3][1][3]:=RClassSCCFromData(s, d[3][1])[1];
	# not sure the prev. step is necessary! JDM2 
	d:=StructuralCopy(AddToOrbitsOfKernels(s, d[3][1][7], d[3])); 
	#d[3][1][7] = f with rectified image!
	d[2][8]:=1; #JDM this should probably go into AddToOrbitsOfKernels.. 
							# i.e. InOrbitsOfImages and AddToOrbitsOfImages should return 
							# the full 8 bits of data.
else
	d:=d[3];
fi;

# d[2][8] = coset used to prove f in d-class
# d[1][3] = position of image of f in orbit of image.

#if JDM2 above is correct (the step is not necessary, then we don't need the 
# following), we only need [d_img, d_ker]!
Add(d, [l, d[2][8]]);

d:=CreateLClass(s, d, [OrbitsOfImages(s), OrbitsOfKernels(s)], 
 LClassRepFromData(s, d));

return d;
end);

# new for 4.0!
#############################################################################
# JDM test this!

InstallOtherMethod(GreensLClassOfElementNC, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
local d, o1, o2, j, data;

Info(InfoMonoidGreens, 4, "GreensLClassOfElementNC");

d:=InOrbitsOfKernels(s, f);

if d[1] then 
	Info(InfoMonoidGreens, 2, "transformation is an element of the semigroup");
  #the following is somewhat inefficient as we run f in s and InOrbitsOfKernels
  #again. JDM perhaps improve if necessary.
  
	return GreensLClassOfElement(s, f);
	
elif OrbitsOfImages(s)!.finished then #f not in s!
	Info(InfoMonoidGreens, 2, "transformation is not an element of the ",
	 "semigroup");
	return fail;
fi;

Info(InfoMonoidGreens, 2, "transformation may not be an element of the ",
 "semigroup");

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

return CreateLClass(s, [d, d, [1,1]], [o1, o2], f);
end);


# new for 4.0!
#############################################################################

InstallMethod(GreensLClassReps, "for a trans. semigroup", 
[IsTransformationSemigroup], 
function(s)
local out, iter, i, f;
Info(InfoMonoidGreens, 4, "GreensLClassReps");

out:=EmptyPlist(NrGreensLClasses(s));
iter:=IteratorOfLClassReps(s);
i:=0;

for f in iter do 
	i:=i+1;
	out[i]:=f;
od;

return out;
end);

#############################################################################
# maybe make iterator at some point in the future JDM!
# JDM check for efficiency and test!

InstallOtherMethod( Idempotents, "for an L-class of a trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
local foo, out, f, n, img, o, scc, j, i;

if HasIsRegularLClass(l) and not IsRegularLClass(l) then 
	return [];
fi;

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

out:= EmptyPlist(Size(l)/NrGreensHClasses(l)); 
#JDM Sum_{k=0..n} C(n,k)*(n-k)^k= # idempotents in T(n)
f:=l!.rep; n:=Degree(f);
img:=ImageAndKernelOfTransformation(f)[1];
o:=LClassKernelOrbitFromData(l!.parent, l!.data, l!.o);
scc:=LClassSCC(l);  j:=0;

for i in scc do
	f:=TABLE_OF_TRANS_KERNEL(o[i], n);
	if foo(f, img) then 
		j:=j+1;
		out[j]:=IdempotentNC(o[i], img);
	fi;
od;

return out;
end);

###########################################################################
# JDM: don't see a current need for IsRegularLClassData...

InstallMethod(IsRegularLClass, "for an L-class of trans. semigroup",
[IsGreensClassOfTransSemigp], 
function(l)
local f, o, i;

if not IsGreensLClass(l) then 
	return false;
fi;

if HasIdempotents(l) then 
	return Length(Idempotents(l))>0; 
fi;

if HasIsRegularSemigroup(l!.parent) and IsRegularSemigroup(l!.parent) then 
  return true;
fi;

f:=ImageSetOfTransformation(l!.rep);
o:=LClassKernelOrbit(l){LClassSCC(l)};

for i in o do 
	if ForAll(i, x-> Size(Intersection(x, f))=1) then 
		return true;
	fi;
od;

return false;
end);

###########################################################################
# 

InstallGlobalFunction(IteratorOfGreensLClasses, 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfGreensLClasses");

iter:=IteratorByFunctions( rec(

	data:=IteratorOfLClassRepsData(s),
	
	IsDoneIterator := iter -> IsDoneIterator(iter!.data), 
	
	NextIterator:= function(iter)
	local d;
	
	d:=NextIterator(iter!.data);
	
	if d=fail then 
		return fail;
	fi;
	
	return CreateLClass(s, d, [OrbitsOfImages(s), OrbitsOfKernels(s)], 
	 LClassRepFromData(s, d));
	end,
	
	ShallowCopy:=iter-> rec(i:=0, s:=iter!.s, data:=IteratorOfLClassRepsData(s))
));

SetIsIteratorOfGreensLClasses(iter, true);
SetUnderlyingSemigroupOfIterator(iter, s);
return iter;
end);

###########################################################################
#

InstallGlobalFunction(IteratorOfLClassReps,
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfLClassReps");

iter:=IteratorByFunctions( rec(

	s:=s,
	
	data:=IteratorOfLClassRepsData(s),
	
	IsDoneIterator := iter-> IsDoneIterator(iter!.data),
	
	NextIterator := function(iter)
	if not IsDoneIterator(iter!.data) then 
		return LClassRepFromData(iter!.s, NextIterator(iter!.data));
	fi;
	return fail; end,
	
	ShallowCopy := iter -> rec( data:=IteratorOfLClassRepsData(
	iter!.s))
));

SetIsIteratorOfLClassReps(iter, true);
SetUnderlyingSemigroupOfIterator(iter, s);

return iter;
end);

###########################################################################
# JDM this could be better if we had IteratorOfDClassRepsData
# and IteratorOfLClassReps for a D-class.

InstallGlobalFunction(IteratorOfLClassRepsData, 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfLClassReps");

iter:=IteratorByFunctions( rec(
	
	ShallowCopy := iter -> rec( i:=0, s:=iter!.s, 
	last_called := NextIterator, last_value := 0, 
	chooser:=iter!.chooser, next:=iter!.next), #JDM correct?
	
	i:=0, # representative index i.e. which representative we are at

	next_value:=[],
	
	last_called_by_is_done:=false,
	
	d:=IteratorOfGreensDClasses(s),
	
	######################################################################

	IsDoneIterator:=function(iter) 

	if iter!.last_called_by_is_done then 
		return IsDoneIterator(iter!.d) and iter!.i>Length(iter!.next_value);
	fi;
	
	iter!.last_called_by_is_done:=true;
	iter!.i:=iter!.i+1;
	
	if IsDoneIterator(iter!.d) and iter!.i>Length(iter!.next_value) then 
		return true;
	elif iter!.i>Length(iter!.next_value) then 
		iter!.next_value:=GreensLClassRepsData(NextIterator(iter!.d));
		iter!.i:=1;
	fi;
	
	return false;
	end,

	######################################################################
	
	NextIterator:=function(iter) 
	
	if not iter!.last_called_by_is_done then 
		IsDoneIterator(iter);
	fi;
	
	if IsDoneIterator(iter) then 
		return fail;
	fi;
	
	iter!.last_called_by_is_done:=false;
	return iter!.next_value[iter!.i];
	end
	######################################################################
));

SetUnderlyingSemigroupOfIterator(iter, s);

return iter;
end);

###########################################################################
# 

InstallGlobalFunction(LClassData, function(list)
return Objectify(NewType(NewFamily("Green's L Class Data", IsGreensLClassData), 
IsGreensLClassData and IsGreensLClassDataRep), list);
end);

############################################################################


InstallMethod(LClassKernelOrbit, "for an L-class of a trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp],
l-> LClassKernelOrbitFromData(l!.parent, l!.data, l!.o));

############################################################################
# JDM change the syntax of the below so that d is really l!.d and o is l!.o?


InstallGlobalFunction(LClassKernelOrbitFromData,
function(arg)
local s, d;

s:=arg[1]; d:=arg[2][2];

if Length(arg)=3 then 
	return arg[3][2]!.orbits[d[1]][d[2]];
else
	return OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

end);

############################################################################

InstallMethod(LClassRels, "for an L-class of a trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
local s, d, o;

s:=l!.parent;
d:=l!.data[2];
o:=l!.o[2];

return LClassRelsFromData(s, d, o);
end);

############################################################################


InstallOtherMethod(LClassRels, "for an D-class of a trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local s, o;

s:=d!.parent;
o:=d!.o[2];
d:=d!.data[2];

return LClassRelsFromData(s, d, o);
end);

############################################################################
#

InstallGlobalFunction(LClassRelsFromData, 
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3]!.orbits[d[1]][d[2]];
else 
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

return o!.rels;
end);

# new for 4.0!
############################################################################

InstallGlobalFunction(LClassRepFromData,
function(arg)
local s, d, f, o, perms, cosets;

s:=arg[1]; d:=arg[2];
f:=CallFuncList(DClassRepFromData,arg);

if Length(arg)=3 then 
	o:=arg[3];
else
	o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];
fi;

perms:=RClassPermsFromData(s, d[1], o[1]);
cosets:=DClassRCosetsFromData(s, d, o);

return f*(cosets[d[3][2]]/perms[d[3][1]]);
end);

# new for 4.0!
############################################################################
# 

InstallMethod(LClassSCC, "for an L-class of a trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(l)
local s, d, o;

s:=l!.parent;
d:=l!.data[2];
o:=l!.o[2];

return LClassSCCFromData(s, d, o);
end);

# new for 4.0!
############################################################################
# 

InstallOtherMethod(LClassSCC, "for a D-class of a trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
local s, o;

s:=d!.parent;
o:=d!.o[2];
d:=d!.data[2];

return LClassSCCFromData(s, d, o);
end);

############################################################################

InstallGlobalFunction(LClassSCCFromData,
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3]!.orbits[d[1]][d[2]];
else 
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

return o!.scc[d[4]];
end);

# new for 4.0!
############################################################################
# JDM should this just be SchutzenbergerGroup?

InstallMethod(LClassSchutzGp, "for an L-class of a trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
local s, d, o;

s:=l!.parent;
d:=l!.data[2];
o:=l!.o[2];

return LClassSchutzGpFromData(s, d, o);
end);

# new for 4.0!
############################################################################

InstallOtherMethod(LClassSchutzGp, "for an D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local s, o;

s:=d!.parent;
o:=d!.o[2];
d:=d!.data[2];

return LClassSchutzGpFromData(s, d, o);
end);

# new for 4.0!
############################################################################

InstallGlobalFunction(LClassSchutzGpFromData, 
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3]!.orbits[d[1]][d[2]];
else 
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

return o!.schutz[d[4]][2];
end);

# new for 4.0!
############################################################################

InstallGlobalFunction(LClassStabChainFromData, 
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3]!.orbits[d[1]][d[2]];
else 
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

return o!.schutz[d[4]][1];
end);

# new for 4.0!
############################################################################

InstallMethod(LClassType, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s);

return NewType( FamilyObj( s ), IsEquivalenceClass and 
	 IsEquivalenceClassDefaultRep and IsGreensLClass and 
	 IsGreensClassOfTransSemigp);
end);

# new for 4.0!
#############################################################################

InstallMethod(NrGreensLClasses, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local i, d;

ExpandOrbitsOfKernels(s);
i:=0;
for d in OrbitsOfKernels(s)!.data do 
	i:=i+Length(DClassRCosetsFromData(s, d))*Length(RClassSCCFromData(s, d[1]));
od;
return i;
end);

# new for 4.0!
#############################################################################

InstallOtherMethod(NrGreensHClasses, "for an L-class of a trans. semigroup", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
l-> NrGreensRClasses(GreensDClass(l)));

# new for 4.0!
############################################################################
# JDM check for efficiency, and also test!

InstallOtherMethod(NrIdempotents, "for an L-class of a trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(l)
local img, n, o, m, i, id, j, k;

if HasIdempotents(l) then 
	return Length(Idempotents(l));
fi;

if HasIsRegularLClass(l) and not IsRegularLClass(l) then 
	return 0;
fi;

img:=Set(l!.rep![1]);
n:=Length(img);
o:=LClassKernelOrbit(l){LClassSCC(l)}; #JDM1
m:=0;

for i in o do
	id:=EmptyPlist(n);
	j:=1;
	k:=Intersection(i[j], img);
	
	while Length(k)=1 and j<=n-1 do 
		id{i[j]}:=List(i[j], x-> k[1]);
		j:=j+1;
		k:=Intersection(i[j], img);
	od;
	
	if j=n and Length(k)=1 then 
		m:=m+1;
	fi;
od;

return m;
end);

# new for 4.0!
############################################################################

InstallMethod(ParentAttr, "for L-class of a trans. semigroup", 
[IsGreensLClass and IsGreensClassOfTransSemigp], x-> x!.parent);

# new for 4.0!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfLClassReps], 
function(iter)
Print( "<iterator of L-class reps>");
return;
end);

#############################################################################
#

InstallMethod( PrintObj, "for object in `IsGreensLClassData'",
[ IsGreensLClassData and IsGreensLClassDataRep],
function( obj )
Print( "GreensLClassData( ", obj!.rep,  " )" );
end );


# new for 4.0!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfGreensLClasses], 
function(iter)
Print( "<iterator of L-classes>");
return;
end);

#############################################################################
# JDM clean up! verify! test! clean up!

InstallOtherMethod(SchutzenbergerGroup, "for an L-class of a trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
local g, d, o, perms, cosets, s;

g:=LClassSchutzGp(l); # the right schutz gp of the unique l-class used 
											# to create the d-class containing l. 

if Size(g)=1 then 
	return g;
fi;

d:=l!.data;
o:=l!.o;
s:=l!.parent;

perms:=RClassPermsFromData(s, d[1], o[1]);
cosets:=DClassRCosetsFromData(s, d, o);

return (g^KerRightToImgLeftFromData(s, d, o))^(cosets[d[3][2]]/perms[d[3][1]]);
end);

# new for 4.0!
#############################################################################
##  Algorithm C. 

InstallOtherMethod(Size, "for an L-class of a trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(l)

Info(InfoMonoidGreens, 4, "Size: for an L-class");

return Size(LClassSchutzGpFromData(l!.parent, l!.data[2], l!.o[2]))
 *Length(LClassSCC(l));
end);

#############################################################################
# 

InstallMethod( ViewObj, "for L-class data",
[ IsGreensLClassData and IsGreensLClassDataRep],
function( obj )
Print( "GreensLClassData( ", obj!.rep, ", ", obj!.strongorb,", ", obj!.relts,
", ", obj!.invrelts,", ", obj!.schutz, " )" );
end );
