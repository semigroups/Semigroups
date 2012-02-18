
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
[IsPartialPerm , IsPerm], ProdPPPerm2); 

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

#AAA

# new for 0.7! - AsPermutation - "for a partial perm"
###########################################################################

InstallOtherMethod(AsPermutation, "for a partial perm",
[IsPartialPerm],
function(f)

  if not Dom(f)=RanSet(f) then
    return fail;
  fi;
  return MappingPermListList(Dom(f), Ran(f));
end);

# new for 0.7! - AsTransformationNC - "for a partial perm and pos int"
###########################################################################
# Notes: n is the total degree!

InstallOtherMethod(AsTransformationNC, "for a partial perm and deg",
[IsPartialPerm , IsPosInt],
function(f, n)
  local g, i;

  g:=ListWithIdenticalEntries(n,n);
  for i in [7..6+f[1]] do
    if f[i]=0 then
      g[i-6]:=n;
    else
      g[i-6]:=f[i];
    fi;
  od;

  return TransformationNC(g);
end);

# new for 0.7! - AsTransformationNC - "for a partial perm"
###########################################################################

InstallOtherMethod(AsTransformationNC, "for a partial perm and deg",
[IsPartialPerm],
function(f)
  return AsTransformationNC(f, MaxDomRan(f)+1);
end);

# new for 0.7! - AsTransformationNC - "for a partial perm"
###########################################################################

InstallOtherMethod(AsTransformation, "for a partial perm and deg",
[IsPartialPerm, IsPosInt],
function(f, n)
  if not n>MaxDomRan(f) then 
    Error("2nd argument should be larger than the largest point moved");
    return;
  fi;
  return AsTransformationNC(f, n);
end);

#DDD

# new for 0.7! - DenseRangeList - "for a partial perm"
#############################################################################

InstallGlobalFunction(DenseRangeList,
function(f)
  if f[1]=0 then
    return [];
  fi;
  return f{[7..6+f[1]]};
end);

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

# new for 0.7! - DomainOfPartialPerm - "for a partial perm"
############################################################################
# Notes: f![1] = deg ; f![2] = rank

InstallMethod(DomainOfPartialPerm, "for a partial perm",
[IsPartialPerm], Dom);

#EEE

# new for 0.7! - ELM_LIST - "for a part perm and pos int"
############################################################################

InstallOtherMethod(ELM_LIST, "for a part perm and a pos int",
[IsPartialPerm, IsPosInt], ELM_LIST_PP);

# new for 0.7! - ELMS_LIST - "for a part perm and small dense list"
############################################################################

InstallOtherMethod(ELMS_LIST, "for a partial perm and a small dense list",
[IsPartialPerm, IsDenseList and IsSmallList ], ELMS_LIST_PP );

#FFF

# new for 0.7! - FixedPoints - "for a partial perm" 
############################################################################# 

InstallMethod(FixedPointsOfPartialPerm, "for a partial perm",
[IsPartialPerm], FixedPointsPP);

#III

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

#MMM

# new for 0.7! - MaxDom - "for a partial perm" 
########################################################################### 
 
InstallGlobalFunction(MaxDom,
function(f) 
  if f[1]=0 then  
    return fail; 
  fi; 
  return f[1]; 
end); 

# new for 0.7! - MaxDomRan - "for a partial perm"
###########################################################################

InstallGlobalFunction(MaxDomRan,
function(f)
  if f[1]=0 then
    return fail;
  fi;
  return f[6];
end);

# new for 0.7! - MaxRan - "for a partial perm"
###########################################################################

InstallGlobalFunction(MaxRan, 
function(f)
  if f[1]=0 then
    return fail;
  fi;
  return f[4];
end);

# new for 0.7! - MinDom - "for a partial perm"
###########################################################################

InstallGlobalFunction(MinDom,
function(f)
  if f[1]=0 then
    return fail;
  fi;
  return f[7+f[1]];
end);

# new for 0.7! - MinDomRan - "for a partial perm"
###########################################################################

InstallGlobalFunction(MinDomRan, 
function(f)
  if f[1]=0 then
    return fail;
  fi;
  return f[5];
end);

# new for 0.7! - MinRan - "for a partial perm"
###########################################################################

InstallGlobalFunction(MinRan, 
function(f)
  if f[1]=0 then
    return fail;
  fi;
  return f[3];
end);

#OOO

# new for 0.7! - One - "for a partial perm"
#############################################################################

InstallMethod(One, "for a partial perm",
[IsPartialPerm ], f-> DensePartialPermNC(Union(Dom(f), RanSet(f))));

# new for 0.7! - One - "for a partial perm""
#############################################################################

InstallMethod(OneMutable, "for a partial perm",
[IsPartialPerm ], f-> DensePartialPermNC(Union(Dom(f), RanSet(f))));

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
      Print("<identity on ", dom, ">");
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

# new for 0.7! - RandomPartialPerm - "for a pos. int."
#############################################################################
# Notes: returns a partial permutation on at most n points. 

# JDM would be good to have a uniform distribution here...

InstallGlobalFunction(RandomPartialPerm,
function(n)
  local out, j, i;

  out:=EmptyPlist(n);
  for i in [1..n] do
    j:=Random([1..n]);
    if not j in out then
      out[i]:=j;
    else
      out[i]:=0;
    fi;
  od;

  return DensePartialPermNC(out);
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




