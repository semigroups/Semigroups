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


InstallMethod( \=, "for H-class and H-class of trans. semigp.",
[IsGreensHClass and IsGreensClassOfTransSemigp, IsGreensHClass and 
IsGreensClassOfTransSemigp],
function(h1, h2)
return h1!.parent=h2!.parent and h1!.rep in h2;
end);

# new for 4.0!
############################################################################

InstallOtherMethod( \in, "for trans. and H-class of trans. semigp.",
[IsTransformation, IsGreensHClass and IsGreensClassOfTransSemigp], 
function(f, h)

Error("not yet implemented");

end);

# new for 4.0!
#############################################################################
#JDM should be s, rep, data, orbit where orbit is optional and the default
# is OrbitsOfImages(s), OrbitsOfKernels(s)

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
local s, d, f, o, perms, cosets;

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3];
else
	o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];
fi;

f:=RClassRepFromData(s, d[1], o[1]);
perms:=RClassPermsFromData(s, d[1], o[1]);
cosets:=DClassRCosetsFromData(s, d, o);

return f*(cosets[d[3][2]]/perms[d[3][1]]);
end);

# new for 4.0!
############################################################################
# JDM clean up! verify! test! clean up!

InstallOtherMethod(SchutzenbergerGroup, "for an H-class of a trans. semigp.",
[IsGreensHClass and IsGreensClassOfTransSemigp], 
h-> DClassSchutzGpFromData(h!.parent, h!.data, h!.o));

