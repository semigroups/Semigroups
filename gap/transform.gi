#############################################################################
##
#W  transform.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
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

InstallMethod(AsPermutation, "for a permutation", [IsPerm],
       p->p);

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
    p:=EmptyPlist(n); set:=Set(img);
    for i in [1..n] do 
      p[ker[i]]:=img[i];
    od;
      
    f![4]:=PermList(Concatenation(p, Difference([1..n], p)));
  fi;

  return f![4];
end);

# new for 4.0!
############################################################################
# require a C version of PermRightQuoTransformation MNMN
# remove! JDM 

InstallGlobalFunction(PermRightQuoTransformationNC,
function(f,g)
local ker_f, ker_g, i, img, ker, out, j;

#ker_f:=List([1..Length(f![1])], x-> []);
#img:=[];

#for i in [1..Length(f![1])] do 
#	j:=f![1][i];
#	Add(ker_f[j], i);
#	AddSet(img, j);
#od;

ker_g:=List([1..Length(f![1])], x-> []);

for i in [1..Length(f![1])] do 
	Add(ker_g[g![1][i]], i);
od;

ker:=KernelOfTransformation(f);
out:=EmptyPlist(Length(ker));

for i in ker do 
	Add(out, ker_g[f![1][i[1]]]);
od;

#return PermListList(ker_f{img}, ker_g{img});
return PermListList(ker, out);
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

#############################################################################
##
##	<#GAPDoc Label="TransformationActionNC">
##	<ManSection>
##	<Oper Name="TransformationActionNC" Arg="list, act, elm"/>
##	<Description>
##	returns the list <C>list</C> acted on by <C>elm</C> via the action 
##	<C>act</C>.
##	<Example>
##  gap&gt; mat:=OneMutable(GeneratorsOfGroup(GL(3,3))[1]);
##  [ [ Z(3)^0, 0*Z(3), 0*Z(3) ], [ 0*Z(3), Z(3)^0, 0*Z(3) ], 
##    [ 0*Z(3), 0*Z(3), Z(3)^0 ] ]
##  gap&gt; mat[3][3]:=Z(3)*0; 
##  0*Z(3)
##  gap&gt; F:=BaseDomain(mat);
##  GF(3)
##  gap&gt; TransformationActionNC(Elements(F^3), OnRight, mat);
##  Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 10, 10, 10, 13, 13, 13, 16, 16, 
##    16, 19, 19, 19, 22, 22, 22, 25, 25, 25 ] )
##	</Example> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
##	</Description>

InstallMethod(TransformationActionNC, "for a list, action, element", true, [IsList, IsFunction, IsObject], 0, 
function(dom, act, act_elm)

return TransformationNC(List(dom, pt-> Position(dom, act(pt, act_elm))));
end);

#############################################################################
##
##	<#GAPDoc Label="AsPermOfRange">
##	<ManSection>
##	<Oper Name="AsPermOfRange" Arg="x"/>
##	<Description>
##	converts a transformation <C>x</C> that is a permutation of its image into 
##	that permutation.
##	<Example>
##  gap&gt; t:=Transformation([1,2,9,9,9,8,8,8,4]);
##  Transformation( [ 1, 2, 9, 9, 9, 8, 8, 8, 4 ] )
##  gap&gt; AsPermOfRange(t);
##  (4,9)
##  gap&gt; t*last;
##  Transformation( [ 1, 2, 4, 4, 4, 8, 8, 8, 9 ] )
##  gap&gt; AsPermOfRange(last);
##  ()
##	</Example> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
##	</Description>

##	JDM replaces PermRepTrans, should be removed!

InstallMethod(AsPermOfRange, "for a transformation",  [IsTransformation],
function(f)
local p, img, t;

#p:=AsPermutation(f); #JDM can be reincluded when AsPermutation is available in the release version 
p:=fail;

if p=fail then 
	img:=AsSet(f![1]);
	t:=OnTuples(img, f);
	if AsSet(t)=img then 
		p:=MappingPermListList(img, t);
	else
		p:=fail;
	fi;
fi;

return p;

end);

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

###########################################################################
##
##	<#GAPDoc Label="IsKerImgOfTransformation">
##	<ManSection> 
##	<Func Name="IsKerImgOfTransformation" Arg="ker, img"/>
##	<Description>
##	returns <C>true</C> if the arguments can be the kernel and image of 
##	a single transformation, respectively.  The argument <C>ker</C> should be a 
##	set of sets and <C>img</C> should be a sublist of the union of the classes 
##	of <C>ker</C>. 
##	<Example>
##  gap&gt; ker:=[[1,2,3],[5,6],[8]];
##  [ [ 1, 2, 3 ], [ 5, 6 ], [ 8 ] ]
##  gap&gt; img:=[1,2,9];
##  [ 1, 2, 9 ]
##  gap&gt; IsKerImgOfTransformation(ker,img);
##  false
##  gap&gt; ker:=[[1,2,3,4],[5,6,7],[8]];
##  [ [ 1, 2, 3, 4 ], [ 5, 6, 7 ], [ 8 ] ]
##  gap&gt; IsKerImgOfTransformation(ker,img);
##  false
##  gap&gt; img:=[1,2,8];
##  [ 1, 2, 8 ]
##  gap&gt; IsKerImgOfTransformation(ker,img);
##  true
##	</Example>
##	</Description>  
##	</ManSection>
##	<#/GAPDoc>

InstallGlobalFunction(IsKerImgOfTransformation, 
function(ker, img)
local dom;

if Length(ker)=Length(img) and ForAll(img, IsPosInt) and IsSet(ker) then 
	dom:=Union(ker);
	if ForAll(dom, IsPosInt) and IsRange(dom) and IsSubset(dom, img) then 
		return true;
	fi;
fi;

#Info(InfoWarning, 1, "the 1st argument should be a partition of a set and the 2nd argument");
#Info(InfoWarning, 1, "a subset of this set, and the arguments should have equal length");

return false;
end);

###########################################################################
##
##	<#GAPDoc Label="TransformationByKernelAndImage">
##	<ManSection><Heading>TransformationByKernelAndImage</Heading>
##	<Oper Name="TransformationByKernelAndImage" Arg="ker, img"/>
##	<Oper Name="TransformationByKernelAndImageNC" Arg="ker, img"/>
##	<Description>
##	returns the transformation <C>f</C> with kernel <C>ker</C> and image 
##	<C>img</C> where <C>(x)f=img[i]</C> for all <C>x</C> in <C>ker[i]</C>.  The 
##	argument <C>ker</C> should be a set of sets. <P/>
##
##	<C>TransformationByKernelAndImage</C> first checks that <C>ker</C> and 
##	<C>img</C> describe the kernel and image of a transformation whereas 
##	<C>TransformationByKernelAndImageNC</C> performs no such check.
##	<Example>
##  gap&gt; TransformationByKernelAndImageNC([[1,2,3,4],[5,6,7],[8]],[1,2,8]);
##  Transformation( [ 1, 1, 1, 1, 2, 2, 2, 8 ] )
##  gap&gt; TransformationByKernelAndImageNC([[1,6],[2,5],[3,4]], [4,5,6]);
##  Transformation( [ 4, 5, 6, 6, 5, 4 ] )
##	</Example>
##	</Description>  
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(TransformationByKernelAndImageNC, "for a kernel and image", true, [IsCyclotomicCollColl, IsCyclotomicCollection], 0, 
function(ker, img)
local m, list, i, j, f;

m:=Maximum(Maximum(ker));
list:=ListWithIdenticalEntries(m, 0);
m:=Length(ker);

for i in [1..m] do
	for j in ker[i] do 
		list[j]:=img[i];
	od;
od;

f:=TransformationNC(list);
SetKernelOfTransformation(f, ker);

if IsSet(img) then 
	SetImageSetOfTransformation(f, img);
fi;

return f;

end);

##############

InstallMethod(TransformationByKernelAndImage, "for a kernel and image", true, [IsCyclotomicCollColl, IsCyclotomicCollection], 0, 
function(ker, img)
local dom;

if IsKerImgOfTransformation(ker, img) then 
	return TransformationByKernelAndImageNC(ker, img);
fi;

return fail;
end);

###########################################################################
##
##	<#GAPDoc Label="AllTransformationsWithKerAndImg">
##	<ManSection><Heading>AllTransformationsWithKerAndImg</Heading>
##	<Oper Name="AllTransformationsWithKerAndImg" Arg="ker, img"/>
##	<Oper Name="AllTransformationsWithKerAndImgNC" Arg="ker, img"/>
##	<Description>
##	returns a list of all transformations with kernel <C>ker</C> and image
##	<C>img</C>. The kernel <C>ker</C> should be a set of sets and <C>img</C> 
##	should be a set.
##	<Example>
##  gap&gt; AllTransformationsWithKerAndImg([[1,6],[2,5],[3,4]], [4,5,6]);
##  [ Transformation( [ 4, 5, 6, 6, 5, 4 ] ), 
##    Transformation( [ 6, 5, 4, 4, 5, 6 ] ), 
##    Transformation( [ 6, 4, 5, 5, 4, 6 ] ), 
##    Transformation( [ 4, 6, 5, 5, 6, 4 ] ), 
##    Transformation( [ 5, 6, 4, 4, 6, 5 ] ), 
##    Transformation( [ 5, 4, 6, 6, 4, 5 ] ) ]
##	</Example>
##	</Description>  
##	</ManSection>
##	<#/GAPDoc>

#JDM include an iterator method

InstallMethod(AllTransformationsWithKerAndImgNC, "for a kernel and image", true, [IsCyclotomicCollColl, IsCyclotomicCollection], 0, 
function(ker, img)
local sym;

sym:=SymmetricGroup(Length(img));

return List(sym, x-> TransformationByKernelAndImageNC(ker, Permuted(img, x)));

end);

##############

InstallMethod(AllTransformationsWithKerAndImg, "for a kernel and image", true, [IsCyclotomicCollColl, IsCyclotomicCollection], 0, 
function(ker, img)

if IsKerImgOfTransformation(ker, img) then 
	return AllTransformationsWithKerAndImgNC(ker, img);
fi;

end);

###########################################################################
##
##	<#GAPDoc Label="KerImgOfTransformation">
##	<ManSection> 
##	<Oper Name="KerImgOfTransformation" Arg="f"/>
##	<Description>
##	returns the kernel and image set of the transformation <C>f</C>.  These 
##	attributes of <C>f</C> can be obtain separately using 
##	<Ref Attr="KernelOfTransformation" BookName="ref"/> and 
##	<Ref Attr="ImageSetOfTransformation" BookName="ref"/>, respectively.
##	<Example>
##  gap&gt; t:=Transformation( [ 10, 8, 7, 2, 8, 2, 2, 6, 4, 1 ] );;
##  gap&gt; KerImgOfTransformation(t);
##  [ [ [ 1 ], [ 2, 5 ], [ 3 ], [ 4, 6, 7 ], [ 8 ], [ 9 ], [ 10 ] ], 
##    [ 1, 2, 4, 6, 7, 8, 10 ] ]
##	</Example>
##	</Description>  
##	</ManSection>
##	<#/GAPDoc>

#JDM remove this!

InstallMethod(KerImgOfTransformation, "for a transformation", true, [IsTransformation], 0, x-> [KernelOfTransformation(x), AsSet(x![1])]);

###########################################################################
##
##	<#GAPDoc Label="AsBooleanMatrix">
##	<ManSection> 
##	<Oper Name="AsBooleanMatrix" Arg="f[,n]"/>
##	<Description>
##	returns the transformation or permutation <C>f</C> represented as an 
##	<C>n</C> by <C>n</C> Boolean matrix.  If <C>f</C> is a transformation, then 
##	<C>n</C> is the size of the domain of <C>f</C>. If <C>f</C> is a 
##	permutation, then <C>n</C> is the number of points moved by <C>f</C>. 
##	That is, if <C>f</C> is defined on an <C>n</C> element set, then 
##	<C>AsBooleanMatrix</C> returns the <C>n</C> by <C>n</C> matrix with 
##	<C>i,f(i)</C>th entry equal to <C>1</C> and all other entries <C>0</C>.
##	<Example>
##  gap&gt; t:=Transformation( [ 4, 2, 2, 1 ] );;
##  gap&gt; AsBooleanMatrix(t);
##  [ [ 0, 0, 0, 1 ], [ 0, 1, 0, 0 ], [ 0, 1, 0, 0 ], [ 1, 0, 0, 0 ] ]
##  gap&gt; t:=(1,4,5);;
##  gap&gt; AsBooleanMatrix(t);
##  [ [ 0, 0, 0, 1, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 1 ],
##    [ 1, 0, 0, 0, 0 ] ]
##  gap&gt; AsBooleanMatrix(t,3);
##  fail
##  gap&gt; AsBooleanMatrix(t,5);
##  [ [ 0, 0, 0, 1, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 1 ],
##    [ 1, 0, 0, 0, 0 ] ]
##  gap&gt; AsBooleanMatrix(t,6);
##  [ [ 0, 0, 0, 1, 0, 0 ], [ 0, 1, 0, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ], 
##    [ 0, 0, 0, 0, 1, 0 ], [ 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1 ] ]
##	</Example>
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(AsBooleanMatrix, "for a transformation", true, [IsTransformation], 0, 
function(f)
local n, out, i;

n:=Length(f![1]);
out:=List([1..n], x-> ListWithIdenticalEntries(n, 0));

for i in [1..n] do
	out[i][i^f]:=1;
od;

return out;
end);

###########

InstallOtherMethod(AsBooleanMatrix, "for a permutation", true, [IsPerm], 0, 
function(f)

return AsBooleanMatrix(f, LargestMovedPoint(f));

end);

###########

InstallOtherMethod(AsBooleanMatrix, "for a permutation and an pos. int.", true, [IsPerm, IsPosInt], 0, 
function(f,n)
local out, i;

if n<LargestMovedPoint(f) then 
	return fail;
fi;

out:=List([1..n], x-> ListWithIdenticalEntries(n, 0));

for i in [1..n] do
	out[i][i^f]:=1;
od;

return out;
end);

#############################################################################
##
##	<#GAPDoc Label="RandomIdempotent">
##	<ManSection><Heading>RandomIdempotent</Heading>
##	<Oper Name="RandomIdempotent" Arg="ker"/>
##	<Oper Name="RandomIdempotent" Arg="img, n"/>
##	<Oper Name="RandomIdempotentNC" Arg="ker"/>
##	<Oper Name="RandomIdempotentNC" Arg="img, n"/>
##	<Description>
##	returns a random idempotent with kernel <C>ker</C> if <C>ker</C> is a set of 
##	sets of positive integers and a random idempotent on <C>1,..,n</C> with 
##	image <C>img</C> if <C>img</C> is a list of positive integers.<P/>
##	<Example>
##  gap&gt; x:=RandomTransformation([[1,2,3], [4,5], [6,7,8]], [1,2,3]);;
##  Transformation( [ 2, 2, 2, 1, 1, 3, 3, 3 ] )
##  gap&gt; RandomIdempotent([1,2,3],5);
##  Transformation( [ 1, 2, 3, 1, 3 ] )
##  gap&gt; RandomIdempotent([[1,6], [2,4], [3,5]]);
##  Transformation( [ 1, 2, 5, 2, 5, 1 ] )
##	</Example> <!-- transform.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

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
