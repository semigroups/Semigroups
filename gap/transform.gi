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
# set, f![3] is the kernel as a partition, f![4] is ..., f![5] is the rank of f
# f![6] is the canonical trans. with same kernel

############################################################################
##  If trans is a permutation, then allow it to be converted into one.
##  return fail if the transformation is not a permutation.
##

InstallMethod(AsPermutation, "for a transformation", [IsTransformation],
       t->PermList(t![1])); 

InstallMethod(AsPermutation, "for a permutation", [IsPerm], p -> p);

InstallMethod(AsPermutation, "for binary relations on points", true,
        [IsBinaryRelation and IsBinaryRelationOnPointsRep], 0,
function(rel)
    if not IsMapping(rel) then
             Error("error, <rel> must be a mapping");
    fi;
    return AsPermutation(TransformationNC(Flat(Successors(rel))));
end);


InstallMethod(DegreeOfTransformationCollNC, "for a trans. coll.", 
[IsTransformationCollection], 
function(coll)

if IsTransformationSemigroup(coll) then 
	return DataType(TypeObj(GeneratorsOfSemigroup(coll)[1]));
fi;
return  DataType(TypeObj(coll[1]));
end);

#new for 4.0!
#############################################################################
#
InstallOtherMethod(Degree, "for a transformation", 
[IsTransformation],
function(f)
return DegreeOfTransformation(f);
end);

InstallOtherMethod(Degree, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
return DegreeOfTransformationSemigroup(s);
end);



#############################################################################
#

InstallOtherMethod(Rank, "for a transformation", 
[IsTransformation],
function(f)
return RankOfTransformation(f);
end);


#new for 4.0!
#############################################################################
#

InstallGlobalFunction(ConstantTransformation, 
function(m,n)
local f;
f:=TransformationNC(ListWithIdenticalEntries(m, n));;
#SetIsConstantTransformation(f, true);
return f;
end);

#InstallMethod(PrintObj, "for a constant transformation", 
#[IsConstantTransformation],
#function(f)
#Print("<constant with value ", f![1][1], " on ", Length(f![1]), " pts");
#end);

#new for 4.0!
#############################################################################
# returns a perm such that i -> ker[i]^f

InstallGlobalFunction(AsPermOfKerImg,
function(f)
  local ker, img, n, p, i;

  if not IsBound(f![4]) then 
    #ker:=KernelOfTransformation(f);
    #f![4]:=MappingPermListList([1..Length(ker)], List(ker, x-> f![1][x[1]]));
    ker:=CanonicalTransSameKernel(f); img:=f![1]; n:=Length(img);
    p:=EmptyPlist(n); 
    for i in [1..n] do 
      p[ker[i]]:=img[i];
    od;
      
    f![4]:=PermList(Concatenation(p, Difference([1..n], p)));
  fi;

  return f![4];
end);

#############################################################################
# new for 4.0!

InstallMethod(Random, "for a transformation semigroup (monoid pkg)", 
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

# new for 4.0!
#############################################################################

InstallMethod(RankOfTransformation, "for a transformation (monoid version)", 
[IsTransformation], 
function(f)

  if not IsBound(f![5]) then 
    f![5]:=Length(ImageSetOfTransformation(f));
  fi;
  return f![5];
end);

#############################################################################
# new for 4.0!

InstallMethod(\*, "for a transformation and a permutation (monoid pkg version)", 
[IsTransformation and IsTransformationRep, IsPerm], 10,
function(x, y)
local c;
c:=OnTuples(x![1], y);
MakeImmutable(c);
return Objectify( TypeObj(x), [ c ] );
end);

#############################################################################
# new for 4.0!

InstallMethod(\*, "for a transformation and transformation (monoid pkg version)", 
[IsTransformation and IsTransformationRep, 
IsTransformation and IsTransformationRep], 10,
function(x, y)
local  a, b, c;
a := x![1];
b := y![1];
c := b{a};
MakeImmutable(c);
return Objectify( TypeObj(x), [ c ] );
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

#############################################################################
# JDM this should be removed!

InstallGlobalFunction(IsTransversal, 
function(ker, img)

if Length(ker)=Length(img) then 
	return ForAll(ker, k-> ForAny(k, i-> i in img));
fi;

return false;

end);

InstallGlobalFunction(IdempotentFromCanonTransImg, 
function(f, img)
  local lookup, m, i;

  lookup:=EmptyPlist(Length(f)); m:=Length(img);
  
  for i in [1..m] do
    lookup[f[img[i]]]:=img[i];
  od;

  return TransformationNC(List(f, x-> lookup[x]));
end);



#############################################################################

InstallGlobalFunction(IdempotentNC, 
function(ker, img)
local e, l;
e:= [];
for l in ker do  
	 e{l}:= 0*l + Intersection(l, img)[1];  
od;
return TransformationNC(e);
end);

#############################################################################

InstallGlobalFunction(Idempotent, 
function(ker, img)
local dom;

if IsKerImgOfTransformation(ker, img) and IsTransversal(ker, img) then 
		return IdempotentNC(ker, img);
fi;

return fail;
end);

#############################################################################
##
#M  AsTransformationNC( <binary relation>)  
##   
##  returns the binary relation as a transformation. If <rel> is not a 
##  transformation the results are unpredictable.
##

InstallOtherMethod(AsTransformationNC, true, [IsBinaryRelation], 
0, function(rel)
return TransformationNC( Flat( Successors( rel ) ) );
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

if IsKerImgOfTransformation(ker, img) then 
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

return TransformationByKernelAndImageNC(ker, new);
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

return TransformationByKernelAndImageNC(ker, img);
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
##
##	<#GAPDoc Label="IndexPeriodOfTransformation">
##	<ManSection>
##	<Attr Name="IndexPeriodOfTransformation" Arg="x"/>
##	<Description>
##	returns the minimum numbers <M>m, r</M> such that <M>x^{m+r}=x^m</M>; known 
##	as the index and period of the transformation. 
##	<Example>
##  gap&gt; x:=Transformation( [ 3, 4, 4, 6, 1, 3, 3, 7, 1 ] );;
##  gap&gt; IndexPeriodOfTransformation(x);
##  [ 2, 3 ]
##  gap&gt; x^2=x^5;
##  true
##	</Example> <!-- transform.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
##	</Description>

InstallMethod(IndexPeriodOfTransformation, "for a transformation", true, [IsTransformation], 0, 
function(x)
local i, y;

i:=1;
y:=x;

while not RankOfTransformation(y^2)=RankOfTransformation(y) do 
	i:=i+1; 
	y:=y*x;
od;

return [i, Order(AsPermOfRange(y))];
end);

######################################

InstallMethod( PrintObj, "for a transformation semigroup", [IsTransformationSemigroup], 
function(S)

Print( "<trans. semigroup" );
if HasSize( S )  then
	Print( " of size ", Size( S ) );
fi;

Print( " with ", Length( GeneratorsOfSemigroup( S ) ), " generators>" );

return;

end);

######################################
######################################
#JDM this should not be necessary. Better if '\in' for a 
#IsFullTransformationSemigroup took priority 
#over '\in' for a TransformationSemigroup

InstallOtherMethod(One, "for a transformation semigroup", true, [IsFullTransformationSemigroup], 0, 
x-> TransformationNC([1..DegreeOfTransformationSemigroup(x)]));

###########################################################################
#

InstallMethod(AsPermutation, "for a transformation", 
[IsTransformation], 
function(f)
local i;

i:=ImageSetOfTransformation(f);
f:=f![1]{i};

if Length(f)=Length(i) then 
	return MappingPermListList(i, f);
fi;

return fail;
end);


###########################################################################
#

#JDM add error handling below... and review it!

InstallOtherMethod(AsPermutation, "for a transformation and a set", 
[IsTransformation, IsList], 
function(f, set)
local a;

a:=f![1]{set};

if IsDuplicateFree(a) then 
	return MappingPermListList(set, a);
fi;

return fail;
end);

###########################################################################
##
##	<#GAPDoc Label="SmallestIdempotentPower">
##	<ManSection> 
##	<Attr Name="SmallestIdempotentPower" Arg="f"/>
##	<Description>
##	returns the least natural number such that the transformation <C>f</C> is 
##	an idempotent.
##	<Example>
##  gap&gt; t:=Transformation( [ 6, 7, 4, 1, 7, 4, 6, 1, 3, 4 ] );;
##  gap&gt; SmallestIdempotentPower(t);
##  6
##  gap&gt; t:=Transformation( [ 6, 6, 6, 2, 7, 1, 5, 3, 10, 6 ] );;
##  gap&gt; SmallestIdempotentPower(t);
##  4
##	</Example>
##	</Description>  
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(SmallestIdempotentPower, "for a transformation", true, [IsTransformation], 0, 
function(f)
local g, i;

g:=();
i:=1;

repeat
	i:=i+1; 
	g:=g*f;
until not AsPermOfRange(g)=fail;

return i+Order(AsPermOfRange(g));

end);


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

InstallMethod(RandomIdempotentNC, "for a kernel", true, [IsCyclotomicCollColl], 0,
function(ker)
return TransformationByKernelAndImageNC(ker, List(ker, Random));
end);

############################################################################
##
#M  <trans>^perm
##
##  Makes sense in that permutations have inverses and are transformations
##

InstallOtherMethod(\^, "for a transformation and a permutation",[IsTransformation, IsPerm],
function(t,p)
	return p^-1*t*p;
end); 

#############################################################################

InstallMethod(\*, "trans * trans", IsIdenticalObj,
[IsTransformation and IsTransformationRep, 
 IsTransformation and IsTransformationRep], 
function(x, y) 
local a,b;

a:= x![1]; b := y![1];
#return TransformationNC(List([1 .. Length(a)], i -> b[a[i]]));
return TransformationNC(b{a});
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
