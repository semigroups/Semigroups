#############################################################################
##
#W  convenience.gi
#Y  Copyright (C) 2006-2011                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id: convenience.gi 110 2010-10-25 18:19:53Z jamesm $
##

# this file contains functions with shorter names than some library functions
# commonly used in the monoid package. 

# new for 4.0! - DClass - user function
#############################################################################
# Usage: (trans. semigp. and trans.) or H-class or L-class or R-class.

InstallGlobalFunction(DClass, 
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
   and IsTransformation(arg[2]) then 
    return GreensDClassOfElement(arg[1], arg[2]);
  elif Length(arg)=1 and IsGreensRClass(arg[1]) then 
    return DClassOfRClass(arg[1]);
  elif Length(arg)=1 and IsGreensLClass(arg[1]) then 
    return DClassOfLClass(arg[1]);
  elif Length(arg)=1 and IsGreensHClass(arg[1]) then 
    return DClassOfHClass(arg[1]);
  fi;
  
  Info(InfoWarning, 1, "Usage: (trans. semigp. and trans.) or H-class or",
  " L-class or R-class.");
  return fail;
end);

# new for 4.0! - DClassNC - user function
#############################################################################
# Usage: trans. semigp. and trans.

InstallGlobalFunction(DClassNC,
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1])
   and IsTransformation(arg[2]) then
    return GreensDClassOfElementNC(arg[1], arg[2]);
  fi;

  Info(InfoWarning, 1, "Usage: trans. semigp. and trans.");
  return fail;
end);

#new for 4.0! - Generators - "for a semigroup or monoid"
############################################################################

InstallMethod(Generators, "for a semigroup or monoid",
[IsSemigroup],
function(s)

  if IsMonoid(s) then
    return GeneratorsOfMonoid(s);
  fi;

  return GeneratorsOfSemigroup(s);
end);

# new for 4.0! - HClass - user function
#############################################################################
# Usage: trans. semigp. and trans.

InstallGlobalFunction(HClass, 
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
   and IsTransformation(arg[2]) then 
    return GreensHClassOfElement(arg[1], arg[2]);
  fi;

  Info(InfoWarning, 1, "Usage: trans. semigp. and trans.");
  return fail;
end);

# new for 4.0! - HClassNC - user function
#############################################################################
# Usage: trans. semigp. and trans.

InstallGlobalFunction(HClassNC, 
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
   and IsTransformation(arg[2]) then 
    return GreensHClassOfElementNC(arg[1], arg[2]);
  fi;

  Info(InfoWarning, 1, "Usage: trans. semigp. and trans.");
  return fail;
end);

# new for 4.0! - LClass - user function
#############################################################################
# Usage: (trans. semigp. and trans.) or H-class.

InstallGlobalFunction(LClass, 
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
   and IsTransformation(arg[2]) then 
    return GreensLClassOfElement(arg[1], arg[2]);
  elif Length(arg)=1 and IsGreensHClass(arg[1]) then 
    return LClassOfHClass(arg[1]);
  fi;
  
  Info(InfoWarning, 1, "Usage: (trans. semigp. and trans.) or H-class.");
  return fail;
end);

# new for 4.0! - LClassNC - user function
#############################################################################
# Usage: trans. semigp. and trans.

InstallGlobalFunction(LClassNC, 
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
   and IsTransformation(arg[2]) then 
    return GreensLClassOfElementNC(arg[1], arg[2]);
  fi;
  
  Info(InfoWarning, 1, "Usage: trans. semigp. and trans.");
  return fail;
end);

# new for 4.0! - RClass - user function
#############################################################################
# Usage: (trans. semigp. and trans.) or H-class.

InstallGlobalFunction(RClass, 
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
   and IsTransformation(arg[2]) then 
    return GreensRClassOfElement(arg[1], arg[2]);
  elif Length(arg)=1 and IsGreensHClass(arg[1]) then 
    return RClassOfHClass(arg[1]);
  fi;
  
  Info(InfoWarning, 1, "Usage: (trans. semigp. and trans.) or H-class.");
  return fail;
end);

# new for 4.0! - RClassNC - user function
#############################################################################
# Usage: trans. semigp. and trans.

InstallGlobalFunction(RClassNC, 
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
   and IsTransformation(arg[2]) then 
    return GreensRClassOfElementNC(arg[1], arg[2]);
  fi;
  
  Info(InfoWarning, 1, "Usage: trans. semigp. and trans.");
  return fail;
end);

#EOF
