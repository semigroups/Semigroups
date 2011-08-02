#############################################################################
##
#W  convenience.gi
#Y  Copyright (C) 2011                                      James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# this file contains functions with shorter names than some library functions
# commonly used in the monoid package. 

# new for 0.1! - DClass - user function
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

# new for 0.1! - DClassNC - user function
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

# new for 0.1! - Degree - "for a transformation"
#############################################################################
# Notes: returns DegreeOfTransformation.

InstallOtherMethod(Degree, "for a transformation",
[IsTransformation], f-> DegreeOfTransformation(f));

# new for 0.1! - Degree - "for a transformation semigroup"
#############################################################################
# Notes: returns DegreeOfTransformationSemigroup.

InstallOtherMethod(Degree, "for a transformation semigroup",
[IsTransformationSemigroup], s-> DegreeOfTransformationSemigroup(s));

# new for 0.1! - Rank - "for a transformation"
#############################################################################
# Notes: returns RankOfTransformation. 

InstallOtherMethod(Rank, "for a transformation",
[IsTransformation], f-> RankOfTransformation(f));

#new for 0.1! - Generators - "for a semigroup or monoid"
############################################################################
# Notes: returns the monoid generators of a monoid, and the semigroup generators
# of a semigroup. 


InstallOtherMethod(Generators, "for a semigroup or monoid",
[IsSemigroup],
function(s)

  if IsMonoid(s) then
    return GeneratorsOfMonoid(s);
  fi;

  return GeneratorsOfSemigroup(s);
end);

# new for 0.1! - HClass - user function
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

# new for 0.1! - HClassNC - user function
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

# new for 0.1! - LClass - user function
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

# new for 0.1! - LClassNC - user function
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

# new for 0.1! - RClass - user function
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

# new for 0.1! - RClassNC - user function
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
