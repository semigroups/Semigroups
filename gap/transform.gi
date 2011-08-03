#############################################################################
##
#W  transform.gi
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# if f is a transformation, then f![1] is the image list, f![2] is the image
# set, f![3] is the kernel, f![4] is AsPermOfKerImg, f![5] is the rank of f
# f![6] is the canonical trans. with same kernel

# new for 0.1! - \^ - "for a transformation and a permutation (citrus pkg)"
#############################################################################
# Notes: conjugates transformation by permutation.

InstallOtherMethod(\^, "for a transformation and a permutation (citrus pkg)",
[IsTransformation, IsPerm],
function(t,p) 
  return p^-1*t*p;
end); 

# new for 0.1! - \* - "for a transformation and a permutation (citrus pkg)"
#############################################################################

InstallMethod(\*, "for a transformation and a permutation (citrus pkg)", 
[IsTransformation and IsTransformationRep, IsPerm], 10,
function(x, y)
  local c;
  c:=OnTuples(x![1], y);
  MakeImmutable(c);
  return Objectify( TypeObj(x), [ c ] );
end);

# new for 0.1! - \* - "for a transformation and a transformation (citrus pkg)"
#############################################################################

InstallMethod(\*, "for a transformation and transformation (citrus pkg)", 
[IsTransformation and IsTransformationRep, 
 IsTransformation and IsTransformationRep], 10,
function(x, y)
  local  a, b, c;
  a := x![1]; b := y![1]; c := b{a};
  MakeImmutable(c);
  return Objectify( TypeObj(x), [ c ] );
end);

#AAA

# new for 0.1! - AsPermOfKerImg - "for a transformation"
#############################################################################
# Notes: returns a perm such that i -> ker[i]^f, performs no checks. 

InstallGlobalFunction(AsPermOfKerImg,
function(f)
  local ker, img, n, p, i;

  if not IsBound(f![4]) then 
    ker:=CanonicalTransSameKernel(f); img:=f![1]; 
    n:=Length(img); p:=EmptyPlist(n); 
    for i in [1..n] do 
      p[ker[i]]:=img[i];
    od;
      
    f![4]:=PermList(Concatenation(p, Difference([1..n], p)));
  fi;

  return f![4];
end);

# new for 0.1! - AsPermutation - "for a permutation"
############################################################################
# Notes: just in case!

InstallMethod(AsPermutation, "for a permutation", [IsPerm], p -> p);

# new for 0.1! - AsPermutation - "for a transformation"
###########################################################################

InstallMethod(AsPermutation, "for a transformation", 
[IsTransformation], 
function(f)
  local a, b;

  a:=ImageSetOfTransformation(f); b:=f![1]{a};
  return MappingPermListList(a, b);
end);

# new for 0.1! - AsPermutation - "for a transformation and set"
###########################################################################

InstallOtherMethod(AsPermutation, "for a transformation and a set", 
[IsTransformation, IsSet], 
function(f, set)
  local a;

  a:=f![1]{set};

  return MappingPermListList(set, a);
end);

#CCC

# new for 0.1! - ConstantTrans - "for degree and value"
#############################################################################
# Usage: degree and value. 

InstallGlobalFunction(ConstantTrans, 
function(m,n)
  if not m>=n then 
    Info(InfoWarning, 1, "Usage: degree and value, degree must be at least",  
    " value");
    return fail;
  fi;
  return TransformationNC(ListWithIdenticalEntries(m, n));;
end);

#DDD

# new for 0.1! - DegreeOfTransformationCollection - "for a trans. coll."
############################################################################

InstallMethod(DegreeOfTransformationCollection, "for a trans. coll.", 
[IsTransformationCollection], 
function(coll)

  if IsTransformationSemigroup(coll) then 
    return DataType(TypeObj(GeneratorsOfSemigroup(coll)[1]));
  fi;
  return DataType(TypeObj(coll[1]));
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
  return TransformationNC(List(ker, i-> img[i]));
end);

# new for 0.1! - IndexPeriodOfTransformation - "for a transformation"
#############################################################################

InstallMethod(IndexPeriodOfTransformation, "for a transformation", 
[IsTransformation], 
function(f)
  local i, g;

  i:=1; g:=f;

  while not IsInjectiveTransOnList(g, ImageSetOfTransformation(g)) do 
    i:=i+1; g:=g*f;
  od;

  return [i, Order(AsPermutation(g))];
end);

# new for 0.1! - InversesOfTransformationNC - "for trans. semi. and trans."
#############################################################################

InstallMethod(InversesOfTransformationNC, "for a trans. semigroup and a trans.", 
[IsTransformationSemigroup, IsTransformation], 
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
[IsTransformationSemigroup, IsTransformation],
function(s, f)

  if f in s then 
    return InversesOfTransformationNC(s, f);
  fi;

  return fail;
end);

# new for 0.1! - IsRegularTransformation - "for a transformation"
###########################################################################

InstallMethod(IsRegularTransformation, "for a transformation", 
[IsTransformationSemigroup, IsTransformation], 
function(s, f)
  local ker, m, o;

  if HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then 
    return true;
  fi;

  if not AsPermutation(f)=fail then      
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

#OOO

# new for 0.1! - One - "for a full transformation semigroup"
###########################################################################
# Notes: this should not be necessary. Better if '\in' for a full
# transformation semigroup took priority over '\in' for a transformation
# semigroup

InstallOtherMethod(One, "for a full transformation semigroup", 
[IsFullTransformationSemigroup],  x -> 
 TransformationNC([1.. DegreeOfTransformationSemigroup(x)]));

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

# new for 0.1! - Random - "for a transformation semigroup (citrus pkg)"
#############################################################################

InstallMethod(Random, "for a transformation semigroup (citrus pkg)", 
[IsTransformationSemigroup],
function(s)
  local o, gens, n, i, w, d, g;

  o:=OrbitsOfImages(s);

  if not o!.finished then 
    gens:=GeneratorsOfSemigroup(s);
    n:=DegreeOfTransformationSemigroup(s);
    i:=Random([1..2*Length(gens)]);
    w:=List([1..i], x-> Random([1..Length(gens)]));
    return EvaluateWord(gens, w);
  else
    d:=Random(o!.data);
    g:=Random(ImageOrbitSchutzGpFromData(s, d));
    i:=Random(ImageOrbitSCCFromData(s, d));
    return RClassRepFromData(s, d)*g*ImageOrbitPermsFromData(s, d)[i]^-1; 
  fi;
end);

# HEREHERE JDMJDM

# new for 0.1! - RandomIdempotent - "for an image and pos. int."
#############################################################################
# Usage: returns a RandomIdempotent with specified image and degree <n>. 

InstallOtherMethod(RandomIdempotent, "for an image and pos. int.",  
[IsCyclotomicCollection, IsPosInt],
function(img, n)

  if ForAll(img, IsPosInt) and Maximum(img)<n then
    return RandomIdempotentNC(img, n);
  fi;

  return fail;
end);

# new for 0.1! - RandomIdempotentNC - "for an image and pos. int."
#############################################################################
# Usage: returns a RandomIdempotent with specified image and degree <n>. 

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

  if ForAll(img, IsPosInt) and Maximum(img)<n+1 then 
    return RandomTransformationNC(img, n);
  fi;
  return fail;
end);

# new for 0.1! - RankOfTransformation - "for a transformation (citrus pkg)"
#############################################################################

InstallMethod(RankOfTransformation, "for a transformation (citrus pkg)", 
[IsTransformation], 
function(f)

  if not IsBound(f![5]) then 
    f![5]:=Length(ImageSetOfTransformation(f));
  fi;
  return f![5];
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
