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

MakeReadWriteGlobal("TransformationFamily");
UnbindGlobal("TransformationFamily");

MakeReadWriteGlobal("TransformationType");
UnbindGlobal("TransformationType");

BindGlobal("TransformationFamily", NewFamily("TransformationFamily",
 IsTransformation, CanEasilySortElements, CanEasilySortElements));
                    
BindGlobal("TransformationType", NewType(TransformationFamily,
 IsTransformation and IsDataObjectRep and IsActingElt));

# new for 1.0! - \* - "for a trans and trans"
############################################################################

InstallMethod(\*, "for transformation and transformation",
[IsTransformation, IsTransformation], ProdTT);

# new for 1.0! - \* - "for a trans and trans"
############################################################################

InstallMethod(\*, "for transformation and perm",
[IsTransformation, IsPerm], ProdTPerm);

# new for 1.0! - \* - "for a trans and trans"
############################################################################

InstallMethod(\*, "for perm and transformation",
[IsPerm, IsTransformation], ProdPermT);

# new for 1.0! - \< - "for a trans and trans"
############################################################################

InstallMethod(\<, "for perm and transformation",
[IsTransformation, IsTransformation], LeqT);

# new for 1.0! - \= - "for a trans and trans"
############################################################################

InstallMethod(\=, "for trans and trans",
[IsTransformation, IsTransformation], EqT);

# new for 1.0! - \^ - "for a pos int and trans"
############################################################################

InstallMethod(\^, "for a pos int and trans",
[IsPosInt, IsTransformation], 
function(i, f)
  if not 1<=i and i<=ELM_LIST_T(f, 1) then 
    Error("usage: an integer from 1 to the degree of the transformation,");
    return;
  fi;
  return ELM_LIST_T(f, i+4);
end);

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

# new for 1.0! - DegreeOfTransformation - "for a transformation"
############################################################################

MakeReadWriteGlobal("DegreeOfTransformation");
UnbindGlobal("DegreeOfTransformation");
BindGlobal("DegreeOfTransformation", f-> f[1]);

# new for 1.0! - Degree - "for a transformation"
#############################################################################
# Notes: returns DegreeOfTransformation.

InstallOtherMethod(Degree, "for a transformation",
[IsTransformation], f-> f[1]);

# mod for 1.0! - DegreeOfTransformationCollection - "for a trans. coll."
############################################################################

InstallMethod(DegreeOfTransformationCollection, "for a trans. coll.", 
[IsTransformationCollection], 
function(coll)

  if IsTransformationSemigroup(coll) then 
    return DegreeOfTransformationSemigroup(coll);
  fi;
  if not ForAll(coll, x-> x[1]=coll[1][1]) then 
    Error("usage: collection of transformations of equal degree,");
    return;
  fi;
  return coll[1][1];
end);

InstallOtherMethod(Degree, "for a transformation coll.",
[IsTransformationCollection], DegreeOfTransformationCollection);

# new for 1.0! - RankOfTransformation - "for a transformation"
############################################################################

MakeReadWriteGlobal("RankOfTransformation");
UnbindGlobal("RankOfTransformation");
BindGlobal("RankOfTransformation", f-> f[2]);

# mod for 1.0! - Rank - "for a transformation"
#############################################################################
# Notes: returns RankOfTransformation. 

InstallOtherMethod(Rank, "for a transformation",
[IsTransformation], f-> f[2]); 

#EEE

# new for 1.0! - ELM_LIST - "for a trans and pos int"
############################################################################

InstallOtherMethod(ELM_LIST, "for a trans and a pos int",
[IsTransformation, IsPosInt], ELM_LIST_T);

# new for 1.0! - ELMS_LIST - "for a trans and small dense list"
############################################################################

InstallOtherMethod(ELMS_LIST, "for a trans and a small dense list",
[IsTransformation, IsDenseList and IsSmallList ], ELMS_LIST_T );

#III

# new for 1.0! - InternalRepOfTransformation - "for a transformation" 
############################################################################# 
 
InstallGlobalFunction(InternalRepOfTransformation,  
function(f) 
  
  if not IsTransformation(f) then 
    Error("the argument should be a transformation,"); 
    return; 
  fi; 
  
  return f{[1..5+2*f[1]+f[2]]}; 
end); 

#PPP

# new for 1.0! - PrintObj - "for a transformation" 
############################################################################# 
 
InstallMethod(PrintObj, "for a transformation",
[IsTransformation], 
function(f) 
 
  if f[1]<20 then  
    Print("Transformation( ", RanT(f), " )");
    return;
  fi; 
  Print("<transformation on ", f[1], " pts with rank ", f[2], ">"); 
  return; 
end);

#TTT

# new for 1.0! - Transformation - "for a list of pos ints"
############################################################################# 

BindGlobal("Transformation", function(list) 
  local n;
  n:=Length(list);
  if ForAny([1..n], i-> not list[i] in [1..n]) then 
    Error("usage: a list of positive integers not greater than the length of the list,");
    return;
  fi;
  return TransformationNC(list);
end);

# new for 1.0! - TransformationOp - "for object, list, function"
###############################################################################

# based on PermutationOp in oprt.gi

InstallMethod(TransformationOp, "for object, list, function",
[IsObject, IsList, IsFunction],
function(f, D, act)
  local perm, out, new, i, pnt;

  perm:=();

  if IsPlistRep(D) and Length(D)>2 and CanEasilySortElements(D[1]) then 
    if not IsSSortedList(D) then 
      D:=ShallowCopy(D);
      perm:=Sortex(D);
      D:=Immutable(D);
      SetIsSSortedList(D, true);
    fi;
  fi;

  out:=EmptyPlist(Length(D));
  i:=0;

  for pnt in D do 
    new:=PositionCanonical(D, act(pnt, f));
    if new=fail then 
      return fail;
    fi;
    i:=i+1;
    out[i]:=new;
  od;

  out:=Transformation(out);
  
  if not IsOne(perm) then 
    out:=out^(perm^-1);
  fi;

  return out;
end);

InstallOtherMethod(TransformationOp, "for an obj and list",
[IsObject, IsList], 
function(obj, list) 
  return TransformationOp(obj, list, OnPoints);
end);

InstallOtherMethod(TransformationOp, "for an obj and list",
[IsObject, IsDomain], 
function(obj, D) 
  return TransformationOp(obj, Enumerator(D), OnPoints);
end);

InstallOtherMethod(TransformationOp, "for an obj and list",
[IsObject, IsDomain, IsFunction], 
function(obj, D, func) 
  return TransformationOp(obj, Enumerator(D), func);
end);

# new for 1.0! - TransformationOpNC - "for object, list, function"
###############################################################################

# based on PermutationOp in oprt.gi

# same as the above except no check that PositionCanonical is not fail and no
# check that the output is a transformation.

InstallMethod(TransformationOpNC, "for object, list, function",
[IsObject, IsList, IsFunction],
function(f, D, act)
  local perm, out, i, pnt;

  perm:=();

  if IsPlistRep(D) and Length(D)>2 and CanEasilySortElements(D[1]) then 
    if not IsSSortedList(D) then 
      D:=ShallowCopy(D);
      perm:=Sortex(D);
      D:=Immutable(D);
      SetIsSSortedList(D, true);
    fi;
  fi;

  out:=EmptyPlist(Length(D));
  i:=0;
  for pnt in D do 
    i:=i+1;
    out[i]:=PositionCanonical(D, act(pnt, f));
  od;

  out:=TransformationNC(out);

  if not IsOne(perm) then 
    out:=out^(perm^-1);
  fi;

  return out;
end);

InstallOtherMethod(TransformationOpNC, "for object, domain, function",
[IsObject, IsList],
function(f, D)
  return TransformationOpNC(f, Enumerator(D), OnPoints);
end);

InstallOtherMethod(TransformationOpNC, "for object, domain, function",
[IsObject, IsDomain],
function(f, D)
  return TransformationOpNC(f, Enumerator(D), OnPoints);
end);

InstallOtherMethod(TransformationOpNC, "for object, domain, function",
[IsObject, IsDomain, IsFunction],
function(f, D, act)
  return TransformationOpNC(f, Enumerator(D), act);
end);

# new for 1.0! - TransformationActionNC - "for semigroup, list, function"
###############################################################################
# JDM expand!!

InstallGlobalFunction(TransformationActionNC, 
function(arg)
  if (IsDomain(arg[2]) or IsList(arg[2])) and IsFunction(arg[3]) then 
    if IsMonoid(arg[1]) then 
      return Monoid(Generators(arg[1]), f-> 
       TransformationOpNC(f, arg[2], arg[3]));
    elif IsSemigroup(arg[1]) then 
      return Semigroup(Generators(arg[1]), f-> 
       TransformationOpNC(f, arg[2], arg[3]));
    fi;
  fi;
  return fail;
end);

############################################################################# 
#old
############################################################################# 

# - a method for RandomTransformation(m,n) i.e. a random transformation with
# a given rank. 

# new for 0.2! - \^ - "for a transformation and a permutation (citrus pkg)"
#############################################################################
# Notes: returns y^-1*x*y.

InstallMethod(\^, "for a transformation and a permutation (citrus pkg)", 
[IsTransformation and IsTransformationRep, IsPerm], 10,
function(x, y) 
  local xx, xy, y_inv, z;
  xx:=x![1]; xy:=OnTuples(xx, y); 
  y_inv:=OnTuples([1..Length(xx)], y^-1);
  z:=xy{y_inv}; MakeImmutable(z);
  return Objectify(TypeObj(x), [z]);
end);

#AAA

# new for 0.1! - AsPermutation - "for a permutation"
############################################################################
# Notes: just in case!

InstallMethod(AsPermutation, "for a permutation", [IsPerm], p -> p);

# mod for 1.0! - AsPermutation - "for a transformation"
###########################################################################
#JDM this could use the method below, and that should have some more checks!

InstallMethod(AsPermutation, "for a transformation", 
[IsTransformation], 
f-> AsPermutation(f, RanSetT(f)));

# mod for 1.0! - AsPermutation - "for a transformation and set"
###########################################################################

InstallOtherMethod(AsPermutation, "for a transformation and a set", 
[IsTransformation, IsList], 
function(f, list)
  local a;

  a:=RanT(f){list};
  
  if not Set(a)=Set(list) then 
    return fail;
  fi;

  return MappingPermListList(list, a);
end);

# new for 0.1! - ConstantTransformation - "for degree and value"
#############################################################################
# Usage: degree and value. 

InstallGlobalFunction(ConstantTransformation, 
function(m,n)
  if not (m>=n and IsPosInt(m) and IsPosInt(n)) then 
    Error("Usage: pos. ints. <degree> and <value>, <degree> must be at", 
    " least <value>,"); 
    return;
  fi;
  return TransformationNC(ListWithIdenticalEntries(m, n));;
end);

#DDD

#III

# new for 0.1! - Idempotent - "for a CanonicalTransSameKernel and image set"
#############################################################################
# Usage: a CanonicalTransSameKernel and a set of the same length on which it is
# injective. 

InstallGlobalFunction(Idempotent, 
function(ker, img)

  if Maximum(ker)=Length(img) and IsInjectiveTransOnList(ker, img) and
   IsSet(img) then 
    return IdempotentNC(ker, img);
  fi;

  return fail;
end);

# new for 0.1! - IdempotentNC - "for a CanonicalTransSameKernel and image set"
#############################################################################
# Usage: a CanonicalTransSameKernel and a set of the same length on which it is
# injective. 

InstallGlobalFunction(IdempotentNC, 
function(ker, img)
  local lookup, m, i;
  
  lookup:=EmptyPlist(Length(ker)); m:=Length(img);

  for i in [1..m] do
    lookup[ker[img[i]]]:=img[i];
  od;
  return TransformationNC(List(ker, x-> lookup[x]));
end);

# fix for 0.2! - IndexPeriodOfTransformation - "for a transformation"
#############################################################################

InstallGlobalFunction(IndexPeriodOfTransformation, 
function(f)
  local i, g, h, j;

  if not IsTransformation(f) then 
    Error("usage: a transformation");
    return;
  fi;

  i:=1; g:=f;

  while not IsInjectiveTransOnList(g, ImageSetOfTransformation(g)) do 
    i:=i+1; g:=g*f;
  od;
  
  return [i, Size(RClass(Semigroup(f), g))];
end);

# new for 0.2! - IsSubset - "for trans. semi. and trans. coll"
###########################################################################

InstallOtherMethod(IsSubset, "for trans. semi. and trans. coll",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup,
 IsTransformationCollection], 9999, 
function(s, coll)
  return ForAll(coll, x-> x in s);
end);

#OOO

# new for 0.5! - OneMutable - "for a transformation"
#############################################################################

InstallMethod(OneMutable, "for a transformation",
[IsTransformation], s-> TransformationNC([1..Degree(s)]*1));

# new for 1.0! - OneMutable - "for a transformation collection"
#############################################################################

InstallOtherMethod(OneMutable, "for a transformation coll",
[IsTransformationCollection], coll-> TransformationNC([1..Degree(coll)]*1));

# mod for 0.5! - OneMutable - "for a full transformation semigroup"
###########################################################################

InstallOtherMethod(OneMutable, "for a full transformation semigroup", 
[IsFullTransformationSemigroup], s->TransformationNC([1.. Degree(s)]*1));

#PPP

# new for 0.1! - PrintObj - "for a transformation semigroup"
###########################################################################

InstallMethod(PrintObj, "for a transformation semigroup (citrus pkg)", 
[IsTransformationSemigroup], 
function(s)

  Print( "<semigroup with " );
  if HasSize(s)  then
    Print(Size(s), " elts, " );
  fi;

  Print(Length(Generators(s)), " gens>" );

  return;
end);

#RRR

# new for 0.1! - RandomIdempotent - "for an image and pos. int."
#############################################################################
# Usage: returns a random idempotent with specified image and degree <n>. 

InstallOtherMethod(RandomIdempotent, "for an image and pos. int.",  
[IsCyclotomicCollection, IsPosInt],
function(img, n)

  if ForAll(img, IsPosInt) and Maximum(img)<=n then
    return RandomIdempotentNC(img, n);
  fi;
  Error("Usage: <img>, <n> where <img> is a list of pos. ints.",
  " and <n> is a pos. int. such that the maximum of <img> is not greater than",
  " <n>,");
  return;
end);

# new for 0.1! - RandomIdempotentNC - "for an image and pos. int."
#############################################################################
# Usage: returns a random idempotent with specified image and degree <n>. 

InstallOtherMethod(RandomIdempotentNC, "for an image and pos. int.", [IsCyclotomicCollection, IsPosInt],
function(img, n)

  return TransformationNC(List([1..n], 
    function(x) 
      if x in img then 
        return x;
      else
        return Random(img);
      fi; end));
end);

# new for 0.1! - RandomTransformation - "for a pos. int."
#############################################################################
# Notes: the library method is obtained by replacing TransformationNC 
# by Transformation.

InstallMethod(RandomTransformation, "for a pos. int.", [IsPosInt],
n-> TransformationNC( List( [ 1 .. n ], i-> Random( [ 1 .. n ] ))));

# new for 0.1! - RandomTransformationNC - "for an image and pos. int"
#############################################################################
# Usage: returns a RandomTransformation with image contained in <img> and 
# degree <n>. 

InstallOtherMethod(RandomTransformationNC, "for an image and a pos. int.",
[IsCyclotomicCollection, IsPosInt],
function(img, n)
  return TransformationNC(List([1..n], x-> Random(img)));
end);

# new for 0.1! - RandomTransformation - "for an image and a pos. int."
#############################################################################
# Usage: returns a Random Transformation with image contained in <img> and
# degree <n>. 

InstallOtherMethod(RandomTransformation,  "for an image and a pos. int.",
[IsCyclotomicCollection, IsPosInt],
function(img, n)

  if ForAll(img, IsPosInt) and Maximum(img)<=n then 
    return RandomTransformationNC(img, n);
  fi;
  Error("Usage: <img>, <n> where <img> is a list of pos. ints.",
  " and <n> is a pos. int. such that the maximum of <img> is not greater than",
  " <n>,");
  return;
end);

#SSS

# new for 0.1! - SmallestIdempotentPower - "for a transformation"
###########################################################################
# Notes: returns the smallest pos. int. such that f^r is an idempotent. 

InstallMethod(SmallestIdempotentPower, "for a transformation",
[IsTransformation], 
function(f)
  local g, i, p;

  g:=(); i:=0;
  
  repeat
    i:=i+1; g:=g*f;
  until AsPermutation(g)=();
  
  return i;
end);

#EOF
