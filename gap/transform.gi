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

# new for 1.0! - RankOfTransformation - "for a transformation"
############################################################################

MakeReadWriteGlobal("RankOfTransformation");
UnbindGlobal("RankOfTransformation");
BindGlobal("RankOfTransformation", f-> f[2]);

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
    Error("usage: a list of positive integers not greater than the length of the list");
    return;
  fi;
  return TransformationNC(list);
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

# new for 0.1! - AsPermOfKerImg - "for a transformation"
#############################################################################
# Notes: returns a perm such that i -> ker[i]^f, performs no checks. 

InstallGlobalFunction(AsPermOfKerImg,
function(f)
  local ker, img, n, p, i;

  ker:=KerT(f); img:=RanT(f); 
    n:=Length(img); p:=EmptyPlist(n); 
    for i in [1..n] do 
      p[ker[i]]:=img[i];
    od;
      
    return PermList(Concatenation(p, Difference([1..n], p)));
end);

# new for 0.1! - AsPermutation - "for a permutation"
############################################################################
# Notes: just in case!

InstallMethod(AsPermutation, "for a permutation", [IsPerm], p -> p);

# fix for 0.2! - AsPermutation - "for a transformation"
###########################################################################
#JDM this could use the method below, and that should have some more checks!

InstallMethod(AsPermutation, "for a transformation", 
[IsTransformation], 
f-> AsPermutation(f, ImageSetOfTransformation(f)));

# fix for 0.2! - AsPermutation - "for a transformation and set"
###########################################################################

InstallOtherMethod(AsPermutation, "for a transformation and a set", 
[IsTransformation, IsList], 
function(f, list)
  local a;

  a:=f![1]{list};
  
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

# mod for 1.0! - DegreeOfTransformationCollection - "for a trans. coll."
############################################################################
# undocumented.

InstallMethod(DegreeOfTransformationCollection, "for a trans. coll.", 
[IsTransformationCollection], 
function(coll)

  if IsTransformationSemigroup(coll) then 
    return DegreeOfTransformationSemigroup(coll);
  fi;
  return DegreeOfTransformation(coll[1]);
end);

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

# new for 0.1! - InversesOfTransformationNC - "for trans. semi. and trans."
#############################################################################

InstallMethod(InversesOfTransformationNC, "for trans. semigroup and trans.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsTransformation], 
function(s, f)
  local regular, out, img, j, g, kers, k, o, l, imgs, h, i;

  regular:=IsRegularSemigroup(s);

  if not (regular or IsRegularTransformation(s, f)) then 
    return [];
  fi;

  out:=[]; img:=ImageSetOfTransformation(f);
  j:=Length(img); g:=f![1];
  kers:=[]; k:=0;

  if not HasGradedKernelsOfTransSemigroup(s) then 
    o:=KernelsOfTransSemigroup(s, j);
    Enumerate(o);

    for i in [1..Length(o)] do 
      if Grades(o)[i]=j and IsInjectiveTransOnList(o[i], img) then 
        k:=k+1; kers[k]:=o[i];
      fi;
    od;
  
  else
    o:=GradedKernelsOfTransSemigroup(s)[j];
    for i in [1..Length(o)] do 
      if IsInjectiveTransOnList(o[i], img) then 
        k:=k+1;
        kers[k]:=o[i];
      fi;
    od;
  fi;

  l:=0; imgs:=ImagesOfTransSemigroup(s, j); Enumerate(imgs);

  for i in [1..Length(imgs)] do
    if Grades(imgs)[i]=j and IsInjectiveTransOnList(g, imgs[i]) then
      for k in kers do
        h:=IdempotentNC(k, img)*MappingPermListList(g{imgs[i]}, imgs[i]);
        if regular or h in s then 
          l:=l+1; out[l]:=h;
        fi;
      od;
    fi;
  od;

  return out;
end);

# new for 0.1! - InversesOfTransformation - "for trans. semi. and trans."
#############################################################################

InstallMethod(InversesOfTransformation, "for a trans. semigroup and a trans.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsTransformation],
function(s, f)

  if f in s then 
    return InversesOfTransformationNC(s, f);
  fi;

  return fail;
end);

# upd for 0.2! - IsRegularTransformation - "for a transformation"
###########################################################################

InstallMethod(IsRegularTransformation, "for a transformation", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsTransformation], 
function(s, f)
  local ker, m, o;

  if not DegreeOfTransformationSemigroup(s)=DegreeOfTransformation(f) then 
    Error("Usage: transformation semigroup and transformation of equal degree,");
    return;
  fi;

  if HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then
    Info(InfoCitrus, 2, "the semigroup is regular,");
    return true;
  fi;

  if not AsPermutation(f)=fail then     
    Info(InfoCitrus, 2, "the transformation belongs to a subgroup,");
    return true;
  fi;

  ker:=f![1]; m:=RankOfTransformation(f);
  
  o:=Orb(s, ImageSetOfTransformation(f), OnSets, 
          rec(lookingfor:=function(o, x) 
                return IsInjectiveTransOnList(ker, x); end,
              gradingfunc := function(o,x) return Length(x); end,
              onlygrades:=function(x,y) return x=y; end, 
              onlygradesdata:=m));
  Enumerate(o);

  return not PositionOfFound(o)=false;
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

# new for 0.5! - One - "for a transformation"
#############################################################################

InstallMethod(One, "for a transformation",
[IsTransformation], 10, s-> TransformationNC([1..Degree(s)]*1));

# mod for 0.5! - One - "for a full transformation semigroup"
###########################################################################
# Notes: this should not be necessary. Better if '\in' for a full
# transformation semigroup took priority over '\in' for a transformation
# semigroup

InstallOtherMethod(One, "for a full transformation semigroup", 
[IsFullTransformationSemigroup],  s -> 
 TransformationNC([1.. Degree(s)]*1));

# new for 0.5! - One - "for a transformation semigroup"
#############################################################################
# Notes: required due to hashing not working for ranges. 

InstallMethod(One, "for a transformation",
[IsTransformation], 10, s-> TransformationNC([1..Degree(s)]*1));

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

# new for 1.0! - Random - "for an acting semigroup"
#############################################################################
# move to greens.gi

InstallMethod(Random, "for an acting semigroup", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local data, gens, n, i, w, g;

  data:=SemigroupData(s);

  if not IsClosed(data) then 
    gens:=GeneratorsOfSemigroup(s);
    n:=Degree(s);
    i:=Random([1..2*Length(gens)]);
    w:=List([1..i], x-> Random([1..Length(gens)]));
    return EvaluateWord(gens, w);
  fi;

  i:=data!.modifier;

  n:=Random([1+i..Length(data)]);
  g:=Random(LambdaOrbSchutzGp(data[n][3], data[n][2]));
  i:=Random([1..Length(data[n][3])]);
  return data[n][4]*g*LambdaOrbMults(data[n][3], data[n][2])[i]^-1; 
end);

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
