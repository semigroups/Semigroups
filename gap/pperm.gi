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

InstallMethod(AsPartialPerm, "for a transformation", 
[IsTransformation and IsTransformationRep],
function(f)
  local img, n;
  img:=f![1];
  n:=Length(f![1]);
  if not n^f=n then 
    return fail; 
  fi;
  return PartialPerm(List(img, function(x) 
    if x=n then 
      return 0; 
    else 
      return x; 
    fi;
  end));
end);

InstallMethod(AsPartialPermNC, "for a transformation", 
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

InstallMethod(AsPartialPermNC, "for a perm", 
[IsPerm],
function(p)
  return AsPartialPerm(p, MovedPoints(p)); 
end);

InstallMethod(AsPartialPerm, "for a perm", 
[IsPerm], AsPartialPermNC);

# new for 0.7! - AsPartialPerm - "for a permutation and a set"
###########################################################################

InstallOtherMethod(AsPartialPermNC, "for a perm and a set", 
[IsPerm, IsList],
function(p, dom)
  return PartialPermNC(dom, OnTuples(dom, p)); 
end);

InstallOtherMethod(AsPartialPerm, "for a perm and a set", 
[IsPerm, IsList], 
function(p, dom)
  if ForAll(dom, IsPosInt) and IsSet(dom) then 
    return AsPartialPermNC(p, dom);
  fi;
  return fail;
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

InstallOtherMethod(AsPermutation, "for a partial perm",
[IsPartialPerm, IsList],
function(f, set)

  if not (IsSet(set) and ForAll(set, IsPosInt)) or not 
   OnIntegerSetsWithPP(set, f)=set then
    return fail;
  fi;
  return MappingPermListList(set, OnTuples(set, f));
end);

# new for 0.7! - AsPermutationNC - "for a partial perm"
###########################################################################
# JDM currently undocumented...

InstallOtherMethod(AsPermutationNC, "for a partial perm",
[IsPartialPerm], f-> MappingPermListList(DomPP(f), RanPP(f)));

InstallOtherMethod(AsPermutationNC, "for a partial perm",
[IsPartialPerm, IsList], function(f, set)
  return MappingPermListList(set, OnTuples(set, f));
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

InstallOtherMethod(AsTransformationNC, "for a partial perm",
[IsPartialPerm],
function(f)
  return AsTransformationNC(f, f[6]+1);
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
  if not n>f[6] then 
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

# new for 0.7! - Display - "for a partial perm"
#############################################################################

InstallMethod(Display, "for a partial perm",
[IsPartialPerm], function(f)
  Print("PartialPermNC( ", DomPP(f), ", ", RanPP(f), " )");
  return;
end);

# new for 0.7! - Display - "for a partial perm coll"
#############################################################################

InstallMethod(Display, "for a partial perm",
[IsPartialPermCollection],
function(coll)
  local i;

  Print("[ ");
  for i in [1..Length(coll)] do
    if not i=1 then Print(" "); fi;
    Display(coll[i]);
    if not i=Length(coll) then
      Print(",\n");
    else
      Print(" ]\n");
    fi;
  od;
  return;
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

InstallGlobalFunction(IndexPeriodOfPartialPerm, 
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

#LLL

# new for 0.7! - LargestMovedPoint - "for a partial perm" 
############################################################################# 

InstallOtherMethod(LargestMovedPoint, "for a partial perm",
[IsPartialPerm], LargestMovedPointPP);

#new for 0.7! - LargestMovedPoint - "for a partial perm semigroup"
###########################################################################

InstallOtherMethod(LargestMovedPoint, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> MaximumList(List(Generators(s), LargestMovedPointPP)));

#new for 0.7! - LargestMovedPoint - "for a partial perm collection"
###########################################################################

InstallOtherMethod(LargestMovedPoint, "for a partial perm collection",
[IsPartialPermCollection], coll-> MaximumList(List(coll, LargestMovedPointPP)));

#MMM

# new for 0.7! - MovedPoints - "for a partial perm" 
############################################################################# 

InstallOtherMethod(MovedPoints, "for a partial perm",
[IsPartialPerm], MovedPointsPP);

# new for 0.7! - MovedPoints - "for a partial perm semigroup"
#############################################################################

InstallOtherMethod(MovedPoints, "for a partial perm semigroup",
[IsPartialPermSemigroup],
s-> Union(List(GeneratorsOfSemigroup(s), MovedPointsPP)));

# new for 0.7! - MovedPoints - "for a partial perm coll"
#############################################################################

InstallOtherMethod(MovedPoints, "for a partial perm coll",
[IsPartialPermCollection],
s-> Union(List(s, MovedPointsPP)));

#NNN

# new for 0.7! - NaturalLeqPartialPerm - "for a pp and pp"
############################################################################# 

InstallMethod(NaturalLeqPartialPerm, "for a pp and pp",
[IsPartialPerm, IsPartialPerm], NaturalLeqPP);

# new for 0.7! - NrMovedPoints - "for a partial perm" 
############################################################################# 

InstallOtherMethod(NrMovedPoints, "for a partial perm",
[IsPartialPerm], NrMovedPointsPP);

# new for 0.7! - NrMovedPoints - "for a partial perm semigroup" 
############################################################################# 

InstallOtherMethod(NrMovedPoints, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> Length(MovedPoints(s)));

# new for 0.7! - NrMovedPoints - "for a partial perm collection" 
############################################################################# 

InstallOtherMethod(NrMovedPoints, "for a partial perm",
[IsPartialPermCollection], c-> Length(MovedPoints(c)));

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

# new for 0.7! - PartialPermNC
############################################################################# 
# Notes: 0 is for undefined... 

InstallGlobalFunction(PartialPerm, 
function(arg)
  if Length(arg)=2 and IsSet(arg[1]) and ForAll(arg[1], IsPosInt) and
   IsDuplicateFreeList(arg[2]) and ForAll(arg[2], IsPosInt) and
   Length(arg[1])=Length(arg[2]) then 
    return SparsePartialPermNC(arg[1], arg[2]);
  elif Length(arg)=1 then 
    if IsDuplicateFreeList( Filtered( arg[1], x -> x<>0 ) ) then
      return DensePartialPermNC(arg[1]);
    fi;
  fi;
  return fail;
end);

############################################################################# 

InstallGlobalFunction(PartialPermNC, 
function(arg) 
   
  if Length(arg)=1 then  
    return DensePartialPermNC(arg[1]); 
  elif Length(arg)=2 then  
    return SparsePartialPermNC(arg[1], arg[2]); 
  fi; 
 
  Error("usage: there should be one or two arguments,"); 
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

#SSS

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

# new for 0.7! - SmallestMovedPoint - "for a partial perm" 
############################################################################# 

InstallOtherMethod(SmallestMovedPoint, "for a partial perm",
[IsPartialPerm], SmallestMovedPointPP);

# new for 0.7! - SmallestMovedPoint - "for a partial perm semigroup" 
############################################################################# 

InstallOtherMethod(SmallestMovedPoint, "for a partial perm",
[IsPartialPermSemigroup], s-> MinimumList(List(Generators(s),                   SmallestMovedPointPP)));

# new for 0.7! - SmallestMovedPoint - "for a partial perm coll" 
############################################################################# 

InstallOtherMethod(SmallestMovedPoint, "for a partial perm",
[IsPartialPermCollection],
function(coll)
  local i, min, j;

  i:=PositionProperty(coll, x-> x[1]<>0);
  min:=SmallestMovedPointPP(coll[i]);

  for j in [i+1..Length(coll)] do 
    j:=SmallestMovedPointPP(coll[j]);
    if j<min and j>0 then 
      min:=j;
    fi;
  od;

  return min;
end);

#EOF
