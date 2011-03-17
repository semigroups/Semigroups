#############################################################################
##
#W  convenience.gi
#Y  Copyright (C) 2006-2011                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id: ideals.gi 110 2010-10-25 18:19:53Z jamesm $
##

#############################################################################
#

InstallGlobalFunction(DClass, 
function(arg)

if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
 and IsTransformation(arg[2]) then 
	return GreensDClassOfElement(arg[1], arg[2]);
elif Length(arg)=1 and ( IsGreensRClass(arg[1]) or IsGreensLClass(arg[1]) 
 or IsGreensHClass(arg[1]) ) then 
	return GreensDClass(arg[1]);
fi;

Info(InfoWarning, 1, "Usage: trans. semigp. and trans. or H-class, L-class,",
" or R-class.");
return fail;
end);

#############################################################################
#

InstallGlobalFunction(HClass, 
function(arg)

if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
 and IsTransformation(arg[2]) then 
	return GreensHClassOfElement(arg[1], arg[2]);
fi;

Info(InfoWarning, 1, "Usage: trans. semigp. and trans.");
return fail;
end);

#############################################################################
#

InstallGlobalFunction(LClass, 
function(arg)

if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
 and IsTransformation(arg[2]) then 
	return GreensLClassOfElement(arg[1], arg[2]);
elif Length(arg)=1 and IsGreensHClass(arg[1]) then 
	return GreensLClass(arg[1]);
fi;

Info(InfoWarning, 1, "Usage: trans. semigp. and trans. or H-class");
return fail;
end);

#############################################################################
#

InstallGlobalFunction(RClass, 
function(arg)

if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
 and IsTransformation(arg[2]) then 
	return GreensRClassOfElement(arg[1], arg[2]);
elif Length(arg)=1 and IsGreensHClass(arg[1]) then 
	return GreensRClass(arg[1]);
fi;

Info(InfoWarning, 1, "Usage: trans. semigp. and trans. or H-class");
return fail;
end);

