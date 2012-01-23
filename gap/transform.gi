#############################################################################
##
#W  transform.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# if f is a transformation, then f![1] is the image list, f![2] is the image
# set, f![3] is the kernel, f![4] is AsPermOfKerImg, f![5] is the rank of f
# f![6] is the canonical trans. with same kernel

# a partial perm is [image list with 0 for undefined, rank (# non-zero), 
# degree, domain, range, max moved pt, min moved pt]

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

# new for 0.7! - \^ - "for a partial perm and neg int"
#############################################################################

if IsBound(InvPartPerm_C) then 
  InstallMethod(\^, "for a partial perm and neg int",
  [IsPartialPerm and IsPartialPermRep, IsNegInt],
  function(f, r)
    return PartialPermNC(InvPartPerm_C(f, Maximum(Ran(f))))^-r;
    # JDM better way?
  end);
else
  #JDM modify the method below...
  InstallMethod(\^, "for a partial perm and neg int", 
  [IsPartialPerm and IsPartialPermRep, IsNegInt],
  function(f, r)
    local ff, n, img, i;
    ff:=f![1];
    n:=Length(ff);
    img:=ListWithIdenticalEntries(n, 0);
    for i in [1..n] do 
      if not ff[i]=0 then 
        img[ff[i]]:=i;
      fi;
    od;
    return PartialPermNC(img)^-r;
  end);
fi;

# new for 0.7! - \^ - "for a pos int and partial perm"
#############################################################################

InstallMethod(\^, "for a pos int and partial perm",
[IsPosInt, IsPartialPerm],
function(i, f)
  local ff;

  ff:=f![1];
  if i<=Length(ff) and not ff[i]=0 then 
    return ff[i];
  fi;
  return fail;
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

# new for 0.2! - \* - "for permutation and transformation (citrus pkg)"
#############################################################################

InstallMethod(\*, "for a permutation and transformation (citrus pkg)",
[IsPerm, IsTransformation and IsTransformationRep], 10,
function(x, y)
  local yy, xx, z;
  yy:=y![1]; xx:=OnTuples([1..Length(yy)], x);
  z:=yy{xx}; MakeImmutable(z);
  return Objectify( TypeObj(y), [ z ] );
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

# new for 0.7! - \* - "for a partial perm and partial perm"
#############################################################################

if IsBound(ProdPartPerm_C) then 
  InstallMethod(\*, "for a partial perm and partial perm (C version)", 
    [IsPartialPerm and IsPartialPermRep, IsPartialPerm and IsPartialPermRep],
    function(f,g)
      return PartialPermNC(ProdPartPerm_C(f,g));
    end);
else
  InstallMethod(\*, "for a partial perm and partial perm",
    [IsPartialPerm and IsPartialPermRep, IsPartialPerm and IsPartialPermRep],
    function(f,g)
      local n, ff, gg, fg, j, i;
  
      ff:=f![1]; gg:=g![1];
      n:=Length(ff);
      fg:=EmptyPlist(n);

      for i in [1..n] do 
        j:=ff[i]; 
        if j = 0 then   
          fg[i]:=0;
        else
          fg[i]:=gg[j];
        fi;
      od;
      return PartialPermNC(fg);
    end);
fi;

# new for 0.7! - \< - "for a partial perm and partial perm"
#############################################################################

InstallMethod(\<, "for a partial perm and partial perm", 
  [IsPartialPerm and IsPartialPermRep, IsPartialPerm and IsPartialPermRep],
  function(f,g)
    return f![1]<g![1];
end);

# new for 0.7! - \= - "for a partial perm and partial perm"
#############################################################################

InstallMethod(\=, "for a partial perm and partial perm", 
  [IsPartialPerm and IsPartialPermRep, IsPartialPerm and IsPartialPermRep],
  function(f,g)
    return Dom(f)=Dom(g) and Ran(f)=Ran(g);
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
  fi; #JDM check if MappingPermListList isn't faster here!

  return f![4];
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

#CCC

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

# new for 0.7! - DegreeOfPartialPerm - "for a partial perm"
############################################################################

InstallMethod(DegreeOfPartialPerm, "for a partial perm",
[IsPartialPerm and IsPartialPermRep],
function(f)
  if not IsBound(f![3]) then 
    f![3]:=Length(f![1]);
  fi;
  return f![3];
end);

# new for 0.7! - DomainOfPartialPerm - "for a partial perm"
############################################################################

InstallMethod(DomainOfPartialPerm, "for a partial perm",
[IsPartialPerm and IsPartialPermRep],
function(f)
local domran;

  if IsBound(f![4]) then
    return f![4];
  fi;
  domran:=DomainAndRangeOfPartialPerm(f);
  f![4]:=domran[1];
  f![5]:=domran[2];
  return domran[1];
end);

# new for 0.1! - DegreeOfTransformationCollection - "for a trans. coll."
############################################################################
# undocumented.

InstallMethod(DegreeOfTransformationCollection, "for a trans. coll.", 
[IsTransformationCollection], 
function(coll)

  if IsTransformationSemigroup(coll) then 
    return DataType(TypeObj(GeneratorsOfSemigroup(coll)[1]));
  fi;
  return DataType(TypeObj(coll[1]));
end);

# new for 0.7! - DomainAndRangeOfPartialPerm - "for a partial perm."
############################################################################

if IsBound(DomRanPartPerm_C) then 
  InstallGlobalFunction(DomainAndRangeOfPartialPerm, 
    f-> DomRanPartPerm_C(f, Rank(f)));
else
  InstallGlobalFunction(DomainAndRangeOfPartialPerm, 
  function(f)
    local ff, n, dom, ran, m, i;

    if IsPartialPerm(f) then 
      ff:=f![1];
    else
      ff:=f;
    fi;

    n:=Length(ff);
    dom:=EmptyPlist(n);
    ran:=EmptyPlist(n);
    m:=0;

    for i in [1..n] do 
      if not ff[i]=0 then 
        m:=m+1;
        dom[m]:=i;
        ran[m]:=ff[i];
      fi;
    od;
    return [dom, ran];
  end);
fi;

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

InstallMethod(IndexPeriodOfTransformation, "for a transformation", 
[IsTransformation], 
function(f)
  local i, g, h, j;

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

#LLL

# new for 0.7! - LargestMovedPoint - "for a partial perm"
###########################################################################

if IsBound(LargestMovedPointPartPerm_C) then 
  InstallOtherMethod(LargestMovedPoint, "for a partial perm",
  [IsPartialPerm and IsPartialPermRep], 
  function(f) 
    if not IsBound(f![6]) then 
      f![6]:=LargestMovedPointPartPerm_C(f);
    fi;
    return f![6];
  end);
else
  InstallOtherMethod(LargestMovedPoint, "for a partial perm",
  [IsPartialPerm and IsPartialPermRep], 
  function(f)
    local ff, n, i;

    if not IsBound(f![6]) then 
      ff:=f![1];
      n:=DegreeOfPartialPerm(f);
      for i in [n, n-1..1] do 
        if not ff[i]=0 then 
          break;
        fi;
      od;
      f![6]:=i;
    fi;

    return f![6];
  end);
fi;

#OOO

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

# new for 0.7! - PartialPermNC - "for an image list"
#############################################################################
# Notes: 0 is for undefined...

InstallGlobalFunction(PartialPermNC,
  x-> Objectify(PartialPermType, [Immutable(x)]));

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

# new for 0.7! - PrintObj - "for a partial perm"
#############################################################################

InstallMethod(PrintObj, "for a partial perm",
[IsPartialPerm and IsPartialPermRep],
function(f)
Print(Dom(f), " -> ", Ran(f));
return;
end);

#RRR

# new for 0.1! - Random - "for a transformation semigroup (citrus pkg)"
#############################################################################
# move to greens.gi

InstallMethod(Random, "for a transformation semigroup (citrus pkg)", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
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

  return PartialPermNC(out);
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

# new for 0.7! - RangeOfPartialPerm - "for a partial perm"
############################################################################

InstallMethod(RangeOfPartialPerm, "for a partial perm",
[IsPartialPerm and IsPartialPermRep],
function(f)
local domran;

  if IsBound(f![5]) then
    return f![5];
  fi;
  domran:=DomainAndRangeOfPartialPerm(f);
  f![4]:=domran[1];
  f![5]:=domran[2];
  return domran[2];
end);

# new for 0.7! - RankOfPartialPerm - "for a partial perm"
############################################################################

if IsBound(RankPartPerm_C) then 
  InstallMethod(RankOfPartialPerm, "for a partial perm",
  [IsPartialPerm and IsPartialPermRep], 
  function(f)
    if not IsBound(f![2]) then 
      f![2]:=RankPartPerm_C(f);
    fi;
    return f![2];
  end);
else
  InstallMethod(RankOfPartialPerm, "for a partial perm",
  [IsPartialPerm and IsPartialPermRep],
  function(f)
    local rank, ff, n, i;
  
    if not IsBound(f![2]) then
      rank:=0;
      ff:=f![1];
      n:=Length(ff);
      for i in [1..n] do 
        if not ff[i]=0 then
          rank:=rank+1;
        fi;
      od;
      f![2]:=rank;
    fi;

  return f![2];
  end);
fi;

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
