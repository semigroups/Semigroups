
# new for 0.7! - \^ - "for a partial perm and neg int"
#############################################################################

InstallMethod(\^, "for a partial perm and neg int",
[IsPartialPerm, IsNegInt],
function(f, r)
  if r=-1 then  
    return InvPP(f);
  fi;
  return InvPP(f)^-r;
end);

# new for 0.7! - \^ - "for a pos int and partial perm" 
############################################################################# 
 
InstallMethod(\^, "for a pos int and partial perm", 
[IsPosInt, IsPartialPerm], 
function(i, f) 
  local j; 
  if i>f[1] then  
    return fail; 
  fi; 
 
  j:=f[6+i]; 
  if j<>0 then  
    return j; 
  fi; 
  return fail; 
end); 

# new for 0.7! - \* - "for a partial perm and partial perm"
#############################################################################
      
InstallMethod(\*, "for a partial perm and partial perm", 
[IsPartialPerm , IsPartialPerm ], ProdPP);

# new for 0.7! - \* - "for a partial perm and perm" 
############################################################################# 
  
InstallMethod(\*, "for a partial perm and perm", 
[IsPartialPerm , IsPerm], ProdPPPerm); 

# new for 0.7! - \* - "for a partial perm and perm" 
############################################################################# 

InstallMethod(\*, "for perm and partial perm",
[IsPerm , IsPartialPerm], ProdPermPP);

# new for 0.7! - \= - "for a partial perm and partial perm"
#############################################################################

InstallMethod(\=, "for a partial perm and partial perm",
[IsPartialPerm , IsPartialPerm ], EqPP);

# new for 0.7! - \< - "for a partial perm and partial perm"
#############################################################################

InstallMethod(\<, "for a partial perm and partial perm",
[IsPartialPerm , IsPartialPerm ], LeqPP);

# new for 0.7! - \/ - "for a partial perm and partial perm"
#############################################################################

InstallOtherMethod(\/, "for a partial perm and partial perm",
[IsPartialPerm, IsPartialPerm], QuoPP);

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

# new for 0.7! - FixedPoints - "for a partial perm" 
############################################################################# 

InstallMethod(FixedPointsOfPartialPerm, "for a partial perm",
[IsPartialPerm], FixedPointsPP);

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

#OOO

# new for 0.7! - OnIntegerSetsWithPartialPerm 
#############################################################################

InstallMethod(OnIntegerSetsWithPartialPerm, "for a set of pos ints and p perm",
[IsSet and IsCyclotomicCollection, IsPartialPerm], OnIntegerSetsWithPP);

#PPP

# new for 0.7! - PartialPermNC - "for a dense image list" 
############################################################################# 
# Notes: 0 is for undefined... 
 
InstallGlobalFunction(PartialPermNC, 
function(arg) 
   
  if Length(arg)=1 then  
    return DensePartialPermNC(arg[1]); 
  elif Length(arg)=2 then  
    return SparsePartialPermNC(arg[1], arg[2]); 
  fi; 
 
  Error("there should be one or two arguments,"); 
  return; 
end); 

# new for 0.7! - PrintObj - "for a partial perm"
#############################################################################

InstallMethod(PrintObj, "for a partial perm",
[IsPartialPerm],
function(f)
  local dom, ran;

  if f[1]=0 then
    Print("<empty mapping>");
    return;
  fi;
  dom:=Dom(f); ran:=Ran(f);

  if Rank(f)>2 then # JDM printing of [2,4] is [2, 4..4]!
    if IsRange(dom) then 
      ConvertToRangeRep(dom);
    fi;
    if IsRange(ran) then 
      ConvertToRangeRep(ran);
    fi;
  fi;
  if Rank(f)<20 then 
      
    if dom=ran then 
      Print("<partial identity on ", dom, ">");
      return;
    fi;
    Print(dom, " -> ", ran);
    return;
  fi;
  Print("<partial perm on ", Rank(f), " pts>");
  return;
end);

#RRR

# new for 0.7! - Ran - "for a partial perm"
############################################################################

InstallGlobalFunction(Ran, 
function(f)
  if f[1]=0 then
    return [];
  fi;

  return f{[7+f[1]+f[2]..6+f[1]+2*f[2]]};
end);

# new for 0.7! - RangeOfPartialPerm - "for a partial perm"
############################################################################

InstallMethod(RangeOfPartialPerm, "for a partial perm",
[IsPartialPerm], Ran);

# new for 0.7! - RangeSetOfPartialPerm - "for a partial perm"
############################################################################

InstallMethod(RangeSetOfPartialPerm, "for a partial perm",
[IsPartialPerm], RanSet);

# new for 0.7! - RankOfPartialPerm - "for a partial perm"
############################################################################

InstallMethod(RankOfPartialPerm, "for a partial perm",
[IsPartialPerm], f-> f[2]);

# new for 0.7! - RestrictedPartialPerm - "for a partial perm"
############################################################################

InstallMethod(RestrictedPartialPerm, "for a part perm and set",
[IsPartialPerm, IsSet and IsCyclotomicCollection], RestrictedPP);



