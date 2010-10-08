#############################################################################
##
#W  greens_l_orb.gi
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

# Conventions

# -low-level function make as few functions calls as possible, higher level ones
# must use LClassSchutzGp... etc.

#############################################################################

InstallMethod( \=, "for L-class and L-class of trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp, IsGreensLClass and 
IsGreensClassOfTransSemigp],
function(d1, d2)
return d1!.rep in d2 and d2!.rep in d1;
end);

#############################################################################
# JDM not sure the following works!

InstallOtherMethod( \in, "for trans. and L-class of trans. semigp.",
[IsTransformation, IsGreensLClass], 
function(f, l)
local rep, d, o, i, schutz, ol;

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

ol:=LClassKernelOrbitFromData(s, d[2], o[2]);
i:=Position(ol, KernelOfTransformation(f));

if i = fail or not ol!.truth[d[2][4]][i] then 
	return false;
fi;

schutz:=LClassStabChainFromData(s, d[2], l!.o[2]);

if schutz=true then 
	return true;
fi;

rep:=DClassRepFromData(s, d, l!.o);

perms:=RClassPermsFromData(s, d[1], o[1]){RClassSCCFromData(s, d[1], o[1])};
cosets:=DClassRCosetsFromData(s, d, o);

return schutz=true or SiftedPermutation(schutz, 
 PermRightQuoTransformationNC(rep, 
  o!.rels[i][2]*f*(cosets[d[3][2]]/perms[d[3][1]])^-1));
end);

# new for 4.0!
#############################################################################
#JDM should be s, rep, data, orbit where orbit is optional and the default
# is OrbitsOfImages(s), OrbitsOfKernels(s)

InstallGlobalFunction(CreateLClass, 
function(s, data, orbit, rep)
local l;

l:=Objectify(LClassType(s), rec(parent:=s, data:=data, 
o:=orbit, rep:=rep));
SetRepresentative(l, rep);
SetEquivalenceClassRelation(l, GreensLRelation(s));
return l;
end);

###########################################################################
# 

InstallGlobalFunction(LClassData, function(list)
return Objectify(NewType(NewFamily("Green's L Class Data", IsGreensLClassData), 
IsGreensLClassData and IsGreensLClassDataRep), list);
end);

############################################################################

InstallGlobalFunction(LClassKernelOrbitFromData,
function(arg)
local s, d;

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	return arg[3]!.orbits[d[1]][d[2]];
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
d:=l!.data;
o:=l!.o;

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

perms:=RClassPermsFromData(s, d[1], o[1]){RClassSCCFromData(s, d[1], o[1])};
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

InstallMethod(LClassSchutzGp, "for an L-class of a trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
local s, d, o;

s:=l!.parent;
d:=l!.data;
o:=l!.o;

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
############################################################################

InstallMethod(PrintObj, [IsIteratorOfLClassReps], 
function(iter)
local O, s;

s:=UnderlyingSemigroupOfIterator(iter);
O:=OrbitsOfKernels(s);

Print( "<iterator of L-class reps, ", Length(O!.ht!.o), " candidates, ", 
 SizeOrbitsOfKernels(s), " elements, ", NrLClassesOrbitsOfKernels(s), 
 " L-classes>");
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
