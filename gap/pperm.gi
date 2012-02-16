


# new for 0.7! - Dom - "for a partial perm"
############################################################################
# Notes: f[1] = deg ; f[2] = rank

InstallGlobalFunction(Dom,
function(f)

  if f[1]=0 then
    return [];
  fi;

  return f{[7+f[1]..6+f[1]+f[2]]};
end);

# new for 0.7! - ELM_LIST - "for a part perm and pos int"
############################################################################

InstallOtherMethod(ELM_LIST, "for a part perm and a pos int",
[IsPartialPerm, IsPosInt], ELM_LIST_PP);

# new for 0.7! - ELMS_LIST - "for a part perm and small dense list"
############################################################################

InstallOtherMethod(ELMS_LIST, "for a partial perm and a small dense list",
[IsPartialPerm, IsDenseList and IsSmallList ], ELMS_LIST_PP );

# new for 0.7! - InternalRepOfPartialPerm - "for a partial perm" 
############################################################################# 
 
InstallGlobalFunction(InternalRepOfPartialPerm,  
function(f) 
 
  if not IsPartialPerm(f) then  
    Error("the argument should be a partial perm,"); 
    return; 
  fi; 
 
  return f{[1..f[1]+3*f[2]+6]}; 
end); 

# new for 0.7! - PartialPermNC - "for a dense image list" 
############################################################################# 
# Notes: 0 is for undefined... 
 
InstallGlobalFunction(PartialPermNC, 
function(arg) 
   
  if Length(arg)=1 then  
    return Objectify(PartialPermType, DenseCreatePartPerm(arg[1])); 
  elif Length(arg)=2 then  
    return SparsePartialPermNC(arg[1], arg[2]); 
  fi; 
 
  Error("there should be one or two arguments,"); 
  return; 
end); 

# new for 0.7! - Ran - "for a partial perm"
############################################################################

InstallGlobalFunction(Ran, 
function(f)
  if f[1]=0 then
    return [];
  fi;

  return f{[7+f[1]+f[2]..6+f[1]+2*f[2]]};
end);

