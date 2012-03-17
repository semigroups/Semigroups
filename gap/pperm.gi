#############################################################################
##
#W  pperm.gi
##Y  Copyright (C) 2011-12                                James D. Mitchell
##
###  Licensing information can be found in the README file of this package.
##
#############################################################################
##

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
[IsPosInt, IsPartialPerm], OnPointsPP);

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

#AAA

# new for 0.7! - AsPartialPermNC - "for a transformation"
###########################################################################

InstallMethod(AsPartialPermNC, "for a partial perm", 
[IsTransformation and IsTransformationRep],
function(f)
  local img, n;
  img:=f![1];
  n:=Length(f![1]);
  return PartialPermNC(List(img, function(x) 
    if x=n then 
      return 0; 
    else 
      return x; 
    fi;
  end));
end);

# new for 0.7! - AsPartialPerm - "for a permutation and a set"
###########################################################################

InstallOtherMethod(AsPartialPerm, "for a perm", 
[IsPerm],
function(p)
  return AsPartialPerm(p, MovedPoints(p)); 
end);

# new for 0.7! - AsPartialPerm - "for a permutation and a set"
###########################################################################

InstallMethod(AsPartialPerm, "for a perm and a set", 
[IsPerm, IsSet],
function(p, dom)
  return PartialPermNC(dom, OnTuples(dom, p)); 
end);

# new for 0.7! - AsPermutation - "for a partial perm"
###########################################################################

InstallOtherMethod(AsPermutation, "for a partial perm",
[IsPartialPerm],
function(f)

  if not DomPP(f)=RanSetPP(f) then
    return fail;
  fi;
  return MappingPermListList(DomPP(f), RanPP(f));
end);

# new for 0.7! - AsPermutationNC - "for a partial perm"
###########################################################################

InstallOtherMethod(AsPermutationNC, "for a partial perm",
[IsPartialPerm], f-> MappingPermListList(DomPP(f), RanPP(f)));

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

InstallOtherMethod(AsTransformationNC, "for a partial perm",
[IsPartialPerm],
function(f)
  return AsTransformationNC(f, MaxDomRanPP(f)+1);
end);

# new for 0.7! - AsTransformation - "for a partial perm"
###########################################################################

InstallOtherMethod(AsTransformation, "for a partial perm",
[IsPartialPerm], AsTransformationNC);

# new for 0.7! - AsTransformationNC - "for a partial perm"
###########################################################################

InstallOtherMethod(AsTransformation, "for a partial perm and deg",
[IsPartialPerm, IsPosInt],
function(f, n)
  if not n>MaxDomRanPP(f) then 
    Error("2nd argument should be larger than the largest point moved");
    return;
  fi;
  return AsTransformationNC(f, n);
end);

#DDD

# new for 0.7! - DegreeOfPartialPerm - "for a partial perm"
#############################################################################

InstallMethod(DegreeOfPartialPerm, "for a partial perm", [IsPartialPerm], 
f-> f[6]);

# new for 0.7! - DenseRangeList - "for a partial perm"
#############################################################################

InstallGlobalFunction(DenseRangeList,
function(f)
  if f[1]=0 then
    return [];
  fi;
  return f{[7..6+f[1]]};
end);

# new for 0.7! - DomainOfPartialPerm - "for a partial perm"
############################################################################
# Notes: f![1] = deg ; f![2] = rank

InstallMethod(DomainOfPartialPerm, "for a partial perm",
[IsPartialPerm], DomPP);

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

# new for 0.7! - IndexPeriodOfPartialPerm - "for a partial perm"
############################################################################# 
# JDM this could avoid multiplying just by considering the orbits of points
# under f as in PrettyPrint.

InstallGlobalFunction("IndexPeriodOfPartialPerm", 
function(f)
  local i, g;
  if not IsPartialPerm(f) then 
    Error("usage: a partial permutation,");
    return;
  fi;

  i:=1; g:=f;
  
  while not DomPP(g)=RanSetPP(g) do 
    i:=i+1; g:=g*f;
  od;
  return [i, Order(AsPermutation(g))];
end);

# new for 0.7! - InternalRepOfPartialPerm - "for a partial perm" 
############################################################################# 
 
InstallGlobalFunction(InternalRepOfPartialPerm,  
function(f) 
 
  if not IsPartialPerm(f) then  
    Error("the argument should be a partial perm,"); 
    return; 
  fi; 

  if f[1]=0 then 
    return f{[1..7]};
  fi;
  
  return f{[1..f[1]+3*f[2]+6]}; 
end); 

#MMM

# new for 0.7! - MaxDomPP - "for a partial perm" 
########################################################################### 
 
InstallGlobalFunction(MaxDomPP,
function(f) 
  if f[1]=0 then  
    return fail; 
  fi; 
  return f[1]; 
end); 

# new for 0.7! - MaxDomRanPP - "for a partial perm"
###########################################################################

InstallGlobalFunction(MaxDomRanPP,
function(f)
  if f[1]=0 then
    return fail;
  fi;
  return f[6];
end);

# new for 0.7! - MaxRanPP - "for a partial perm"
###########################################################################

InstallGlobalFunction(MaxRanPP, 
function(f)
  if f[1]=0 then
    return fail;
  fi;
  return f[4];
end);

# new for 0.7! - MinDomPP - "for a partial perm"
###########################################################################

InstallGlobalFunction(MinDomPP,
function(f)
  if f[1]=0 then
    return fail;
  fi;
  return f[7+f[1]];
end);

# new for 0.7! - MinDomRanPP - "for a partial perm"
###########################################################################

InstallGlobalFunction(MinDomRanPP, 
function(f)
  if f[1]=0 then
    return fail;
  fi;
  return f[5];
end);

# new for 0.7! - MinRanPP - "for a partial perm"
###########################################################################

InstallGlobalFunction(MinRanPP, 
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
[IsPartialPerm ], 
function(f)
  local x;
  x:=Union(DomPP(f), RanSetPP(f));
  return SparsePartialPermNC(x, x);
end);

# new for 0.7! - One - "for a partial perm""
#############################################################################

InstallMethod(OneMutable, "for a partial perm",
[IsPartialPerm ], One);

# new for 0.7! - OnIntegerSetsWithPartialPerm 
#############################################################################

InstallMethod(OnIntegerSetsWithPartialPerm, "for a set of pos ints and p perm",
[IsCyclotomicCollection, IsPartialPerm], OnIntegerSetsWithPP);

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
  dom:=DomPP(f); ran:=RanPP(f);

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

# new for 0.7! - PrettyPrintPP - "for a partial perm"
#############################################################################

InstallGlobalFunction(PrettyPrintPP,
function(f)
  local seen, dom, g, i, j, cycle, tmp;

  seen:=ListWithIdenticalEntries(f[1], false);;
  seen[1]:=true;
  dom:=DomPP(f);;
  g:=f^-1;
  i:=1;

  while i<>fail do 
    j:=dom[i];
    cycle:=[];
    repeat
      seen[j]:=true;
      Add(cycle, j);
      j:=j^f;
    until j=fail or j=dom[i];
    if j=fail then 
      tmp:=[];
      j:=dom[i]^g;
      while j<>fail do  
        seen[j]:=true;
        Add(tmp, j);
        j:=j^g;
      od;
      Print("[");
      cycle:=Concatenation(Reversed(tmp), cycle);
      for j in [1..Length(cycle)-1] do 
        Print(cycle[j], ",");
      od;
      Print(cycle[j+1],"]");
    elif Length(cycle)=1 then 
      Print("(", cycle[1], ")");
    else
      Print("(");
      for j in [1..Length(cycle)-1] do 
        Print(cycle[j], ",");
      od;
      Print(cycle[j+1],")");
    fi;
    i:=PositionProperty(dom, x-> not seen[x]);
  od;
end);

#RRR

# new for 0.7! - RandomPartialPerm - "for a pos. int."
#############################################################################
# Notes: returns a partial permutation on at most n points. 

# JDM would be good to have a uniform distribution here...

InstallGlobalFunction(RandomPartialPerm,
function(n)
  local out, j, i;

  if n>65535 then 
    Error("usage: can only create partial perms on at most 65535 pts,");
    return;
  fi;

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
[IsPartialPerm], RanPP);

# new for 0.7! - RangeSetOfPartialPerm - "for a partial perm"
############################################################################

InstallMethod(RangeSetOfPartialPerm, "for a partial perm",
[IsPartialPerm], RanSetPP);

# new for 0.7! - RankOfPartialPerm - "for a partial perm"
############################################################################

InstallMethod(RankOfPartialPerm, "for a partial perm",
[IsPartialPerm], f-> f[2]);

# new for 0.7! - RestrictedPartialPerm - "for a partial perm"
############################################################################

InstallMethod(RestrictedPartialPerm, "for a part perm and set",
[IsPartialPerm, IsList and IsCyclotomicCollection], 
function(f, set)
  if not IsSet(set) or not ForAll(set, IsPosInt) then 
    return fail;
  fi;
  return RestrictedPP(f, set);
end);

# new for 0.7! - RestrictedPartialPermNC - "for a partial perm"
############################################################################

InstallMethod(RestrictedPartialPermNC, "for a part perm and set",
[IsPartialPerm, IsList and IsCyclotomicCollection], RestrictedPP);

# new for 0.7! - SmallestIdempotentPower - "for a partial perm"
############################################################################

InstallOtherMethod(SmallestIdempotentPower, "for a partial perm",
[IsPartialPerm],
function(f)
  local g, i, p;

  g:=f; i:=1;

  while AsPermutation(g)<>() do 
    i:=i+1; g:=g*f;
  od;

  return i;
end);

#EOF
