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

# new for 0.1! - Idempotent - "for a CanonicalTransSameKernel and image"
#############################################################################
# Usage: a CanonicalTransSameKernel and a set of the same length on which ker in
# injective. 

InstallGlobalFunction(Idempotent, 
function(ker, img)

  if Maximum(ker)=Length(img) and IsInjectiveTransOnList(ker, img) and
   IsSet(img) then 
    return IdempotentNC(ker, img);
  fi;

  return fail;
end);

#############################################################################

#InstallGlobalFunction(IdempotentFromCanonTransImg, 
#function(f, img)
#  local lookup, m, i;
#
#  lookup:=EmptyPlist(Length(f)); m:=Length(img);
#  
#  for i in [1..m] do
#    lookup[f[img[i]]]:=img[i];
#  od;
#
#  return TransformationNC(List(f, x-> lookup[x]));
#end);

#############################################################################
# Usage: for C

InstallGlobalFunction(IdempotentNC, 
function(ker, img)
  return TransformationNC(List(ker, i-> img[i]));
end);

#############################################################################

InstallMethod(IndexPeriodOfTransformation, "for a transformation", 
[IsTransformation], 
function(x)
local i, y;

i:=1;
y:=x;

while not RankOfTransformation(y^2)=RankOfTransformation(y) do 
	i:=i+1; 
	y:=y*x;
od;

return [i, Order(AsPermutation(y))];
end);

#############################################################################
# JDM InversesOfTransformationNC should be revised when we get new C functions

InstallMethod(InversesOfTransformationNC, "for a trans. semigroup and a trans.", 
[IsTransformationSemigroup, IsTransformation], 
function(s, f)
local regular, foo, out, img, ker, j, g, imgs, o, kers, i, k, h, l, n;

regular:=IsRegularSemigroup(s);

if not (regular or IsRegularTransformation(s, f)) then 
	return [];
fi;

#############

out:=[];
img:=ImageAndKernelOfTransformation(f);
ker:=img[2]; img:=img[1];
j:=Length(img); g:=f![1]; n:=Length(g);

kers:=[];
k:=0;

if not HasGradedKernelsOfTransSemigroup(s) then 
	o:=KernelsOfTransSemigroup(s, j);
	Enumerate(o);

	for i in [1..Length(o)] do 
		if Grades(o)[i]=j and IsInjectiveTransOnList(o[i], img) then 
			k:=k+1;
			kers[k]:=o[i];
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

l:=0;
imgs:=ImagesOfTransSemigroup(s, j);
Enumerate(imgs);

for i in [1..Length(imgs)] do
	if Grades(imgs)[i]=j and foo(g, imgs[i]) then
		for k in kers do 
			h:=IdempotentNC(k, img)*MappingPermListList(g{imgs[i]}, imgs[i]);
			if regular or h in s then 
				l:=l+1;
				out[l]:=h;
			fi;
		od;
	fi;
od;

return out;
end);

############

InstallMethod(InversesOfTransformation, "for a trans. semigroup and a trans.", 
[IsTransformationSemigroup, IsTransformation],
function(S, f)

if f in S then 
	return InversesOfTransformationNC(S, f);
fi;

return fail;
end);
###########################################################################
# this should be modified!! JDM see IsRegularRClass

InstallMethod(IsRegularTransformation, "for a transformation", 
[IsTransformationSemigroup, IsTransformation], 
function(M, x)
local r, orb, gens, s, p, n;

if HasIsRegularSemigroup(M) and IsRegularSemigroup(M) then 
  return true;
fi;

n:= ImageSetOfTransformation(x);   
r:= Size(n);

if RankOfTransformation(x^2)=r then      
  return true;
fi;

# otherwise form the weak orbit of img x.
orb:= [n];   
gens:= GeneratorsOfSemigroup(M);
for p in orb do
  for s in gens do
    n:=Set(ImageListOfTransformation(s){p});
    if Size(n) = r and not n in orb then
            if Size(Set(ImageListOfTransformation(x){n})) = r then
               return true;
            fi;

            Add(orb, n);
         fi;
      od;
   od;

   # if we arrive here, no cross section has been found.
   return false;

end );

#OOO

######################################
#JDM this should not be necessary. Better if '\in' for a 
#IsFullTransformationSemigroup took priority 
#over '\in' for a TransformationSemigroup

InstallOtherMethod(One, "for a transformation semigroup", true, [IsFullTransformationSemigroup], 0, 
x-> TransformationNC([1..DegreeOfTransformationSemigroup(x)]));

#PPP

######################################

InstallMethod(PrintObj, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(S)

Print( "<trans. semigroup" );
if HasSize( S )  then
	Print( " of size ", Size( S ) );
fi;

Print( " with ", Length( GeneratorsOfSemigroup( S ) ), " generators>" );

return;
end);

#RRR

#############################################################################

InstallMethod(Random, "for a transformation semigroup (citrus pkg)", 
[IsTransformationSemigroup],
function(s)
local gens, n, i, w, d, g, h, o;

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

###########################################################################

InstallMethod(RandomIdempotent, "for a kernel", true, [IsCyclotomicCollColl], 0,
function(ker)
local dom;

dom:=Union(ker);

if ForAll(dom, IsPosInt) and IsRange(dom) then
	return RandomIdempotentNC(ker);
fi;

return fail;
end);

############

InstallOtherMethod(RandomIdempotent, "for an image and pos. int.", true, [IsCyclotomicCollection, IsPosInt], 0,
function(img, n)

if ForAll(img, IsPosInt) and Maximum(img)<n then
	return RandomIdempotentNC(img, n);
fi;

return fail;
end);

############

InstallOtherMethod(RandomIdempotentNC, "for an image and pos. int.", true, [IsCyclotomicCollection, IsPosInt], 0,
function(img, n)

return TransformationNC(List([1..n], function(x) 
if x in img then 
return x;
else
return Random(img);
fi; end) );
end);

############
#JDM redo the following!
InstallMethod(RandomIdempotentNC, "for a kernel", true, [IsCyclotomicCollColl], 0,
function(ker)
#return TransformationByKernelAndImageNC(ker, List(ker, Random));
end);

#############################################################################
# new for 4.0! the lib. method is obtained by replacing TransformationNC 
# by Transformation.

InstallMethod(RandomTransformation, "for a pos. int.", [IsPosInt],
n-> TransformationNC( List( [ 1 .. n ], i-> Random( [ 1 .. n ] ))));

#############################################################################

InstallOtherMethod(RandomTransformation,  "for a kernel and image", true, [IsCyclotomicCollColl, IsCyclotomicCollection], 0,     
function(ker, img)
local new, x;

if Length(ker)=Length(img) then 
  return RandomTransformationNC(ker, img);
fi;

return fail;
end);

############

InstallOtherMethod(RandomTransformationNC,  "for a kernel and image", true, [IsCyclotomicCollColl, IsCyclotomicCollection], 0,
function(ker, img)
local new, x, copy;

new:=[];
copy:=ShallowCopy(img);

repeat 
	x:=Random(copy);
	Add(new, x);
	SubtractSet(copy, [x]);
until copy=[];
#JDM
#return TransformationByKernelAndImageNC(ker, new);
end);

############

InstallOtherMethod(RandomTransformationNC,  "for an image and a pos. int.", true, [IsCyclotomicCollection, IsPosInt], 0,
function(img, n)

return TransformationNC(List([1..n], x-> Random(img)));
end);

############

InstallOtherMethod(RandomTransformation,  "for an image and a pos. int.", true, [IsCyclotomicCollection, IsPosInt], 0,
function(img, n)

if ForAll(img, IsPosInt) and Maximum(img)<n+1 then 
	return RandomTransformationNC(img, n);
fi;
return fail;
end);

############

InstallOtherMethod(RandomTransformationNC, "for a kernel", true, [IsCyclotomicCollColl], 0,
function(ker)
local dom, img, k, i;

dom:=[1..Maximum(Maximum(ker))];
img:=[];

for k in ker do 
	i:=Random(dom);
	Add(img, i);
	SubtractSet(dom, [i]);
od;

#return TransformationByKernelAndImageNC(ker, img);
end);

############

InstallOtherMethod(RandomTransformation, "for a kernel", true, [IsCyclotomicCollColl], 0,
function(ker)
local dom;

dom:=Union(ker);

if ForAll(dom, IsPosInt) and IsRange(dom) then
	return RandomTransformationNC(ker);
fi;

return fail;
end);

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

###########################################################################

InstallMethod(SmallestIdempotentPower, "for a transformation",
[IsTransformation],
function(f)
  local g, i, p;

  g:=(); i:=1;
  
  repeat
    i:=i+1; 
    g:=g*f; 
    p:=AsPermutation(g);
  until not p=fail;

  return i+Order(p);
end);

#EOF
