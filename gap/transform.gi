#############################################################################
##
#W  transform.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# new

#CCC

# new for 1.0 - ChooseHashFunction - "for a transformation"
#############################################################################

InstallMethod(ChooseHashFunction, "for a transformation",
[IsTransformation, IsInt], 
function(f, hashlen)
  return rec(func:=function(x, data) 
    return ORB_HashFunctionForPlainFlatList(RanT(x), data); 
  end, data:=hashlen);
end);

#DDD
#EOF
