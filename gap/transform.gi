##
## transform.gi
## Version 3.1.3
## Fri  7 Nov 2008 17:45:12 GMT
##


##  <#GAPDoc Label="transformtop">
##  The functions in this chapter extend the functionality of &GAP; relating 
##  to transformations. 
##	<#/GAPDoc>


##  JDM install methods for partial transformations, partial bijections

###########################################################################
##
##  <#GAPDoc Label="IsRegularTransformation">
##  <ManSection>
##  <Oper Name="IsRegularTransformation" Arg="S, f"/>
##	<Description>
##	if <C>f</C> is a regular element of the transformation semigroup <C>S</C>, 
##	then <C>true</C> is returned. Otherwise <C>false</C> is returned.<P/> 
##
##	A transformation <C>f</C> is regular inside a transformation semigroup 
##	<C>S</C> if it lies inside a regular D-class. This is equivalent to the 
##	orbit of the image of <C>f</C> containing a transversal of the kernel of 
##	<C>f</C>.
## <Example>
##  gap&gt; g1:=Transformation([2,2,4,4,5,6]);;
##  gap&gt; g2:=Transformation([5,3,4,4,6,6]);;
##  gap&gt; m1:=Monoid(g1,g2);;
##  gap&gt; IsRegularTransformation(m1, g1);
##  true
##  gap&gt; img:=ImageSetOfTransformation(g1);
##  [ 2, 4, 5, 6 ]
##  gap&gt; ker:=KernelOfTransformation(g1);
##  [ [ 1, 2 ], [ 3, 4 ], [ 5 ], [ 6 ] ]
##  gap&gt; ForAny(MonoidOrbit(m1, img), x-> IsTransversal(ker, x));
##  true
##  gap&gt; IsRegularTransformation(m1, g2);
##  false
##  gap&gt; IsRegularTransformation(FullTransformationSemigroup(6), g2);
##  true
##	</Example> <!-- transform.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

InstallMethod(IsRegularTransformation, "for a transformation", true, [IsTransformationSemigroup, IsTransformation], 0,
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

            # did we find a cross section? JDM1
	    # JDM try IsDuplicateFreeList or IsTransversal here

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
##
##	<#GAPDoc Label="IsTransversal">
##	<ManSection>
##	<Func Name="IsTransversal" Arg="list1, list2"/>
##	<Description>
##	returns <C>true</C> if the list <C>list2</C> is a transversal of the list of 
##	lists <C>list1</C>. That is, if every list in <C>list1</C> contains exactly 
##	one element in <C>list2</C>.
##	<Example>
##  gap&gt; g1:=Transformation([2,2,4,4,5,6]);;
##  gap&gt; g2:=Transformation([5,3,4,4,6,6]);;
##  gap&gt; ker:=KernelOfTransformation(g2*g1);
##  [ [ 1 ], [ 2, 3, 4 ], [ 5, 6 ] ] 
##  gap&gt; im:=ImageListOfTransformation(g2);
##  [ 5, 3, 4, 4, 6, 6 ]
##  gap&gt; IsTransversal(ker, im);
##  false
##  gap&gt; IsTransversal([[1,2,3],[4,5],[6,7]], [1,5,6]);
##  true
##  </Example> <!-- transform.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

##	JDM This is inefficient and should be avoided if we have a transformation. 
##	JDM Use Length(OnSets(f![1], f))=Length(f![1]) instead for a trans. f.

InstallGlobalFunction(IsTransversal, 
function(ker, img)

if Length(ker)=Length(img) then 
	return ForAll(ker, k-> ForAny(k, i-> i in img));
fi;

return false;

end);

#############################################################################
##
##	<#GAPDoc Label="Idempotent">
##	<ManSection><Heading>Idempotent</Heading>
##	<Func Name="IdempotentNC" Arg="ker, img"/>
##	<Func Name="Idempotent" Arg="ker, img"/>
##	<Description>
##	<C>IdempotentNC</C> returns an idempotent with kernel <C>ker</C> and image 
##	<C>img</C> without checking <Ref Func="IsTransversal"/><C>(ker, im)</C>.<P/>
##
##	<C>Idempotent</C> returns an idempotent with kernel <C>ker</C> and image 
##	<C>img</C> after checking that <Ref Func="IsTransversal"/><C>(ker, im)</C> 
##	returns true. <P/>
##	<Example>
##  gap&gt; g1:=Transformation([2,2,4,4,5,6]);;
##  gap&gt; g2:=Transformation([5,3,4,4,6,6]);;
##  gap&gt; ker:=KernelOfTransformation(g2*g1);;
##  gap&gt; im:=ImageListOfTransformation(g2);;
##  gap&gt; Idempotent(ker, im);
##  Error,  the image must be a transversal of the kernel
##  [ ... ]
##  gap&gt; Idempotent([[1,2,3],[4,5],[6,7]], [1,5,6]);
##  Transformation( [ 1, 1, 1, 5, 5, 6, 6 ] )
##  gap&gt; IdempotentNC([[1,2,3],[4,5],[6,7]], [1,5,6]);
##  Transformation( [ 1, 1, 1, 5, 5, 6, 6 ] )
##	</Example> <!-- transform.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
##	</Description>

InstallGlobalFunction(IdempotentNC, 
function(ker, img)
      local e, l;
      e:= [];
      for l in ker do  
         e{l}:= 0*l + Intersection(l, img)[1];  
      od;
      return TransformationNC(e);
      #return Transformation(e);
end);

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
##
##	<#GAPDoc Label="RandomTransformation">
##	<ManSection><Heading>RandomTransformation</Heading>
##	<Oper Name="RandomTransformation" Arg="ker, img"/>
##	<Oper Name="RandomTransformationNC" Arg="ker, img"/>
##	<Oper Name="RandomTransformation" Arg="ker"/>
##	<Oper Name="RandomTransformationNC" Arg="ker"/>
##	<Oper Name="RandomTransformation" Arg="img, n"/>
##	<Oper Name="RandomTransformationNC" Arg="img, n"/>
##	<Description>
##	These are new methods for the existing library function 
##	<Ref Func="RandomTransformation" BookName="ref"/>.  A random transformation 
##	is returned that has the given image <C>img</C> and/or kernel <C>ker</C>. If no kernel 
##	<C>ker</C> is given, then a number <C>n</C> of elements in the domain of the 
##	transformation must be specified. Note that <C>ker</C> must be a set of sets with the same 
##	number of classes as <C>img</C> has elements.<P/>
##
##	The no check version does not check that <C>ker</C> and <C>img</C> can be 
##	the kernel and image of a transformation, respectively.
##	<Example>
##  gap&gt; x:=RandomTransformation([[1,2,3], [4,5], [6,7,8]], [1,2,3]);;
##  Transformation( [ 2, 2, 2, 1, 1, 3, 3, 3 ] )
##  gap&gt; RandomTransformation([[1,2,3],[5,7],[4,6]]); 
##  Transformation( [ 3, 3, 3, 6, 1, 6, 1 ] )
##  gap&gt; RandomTransformation([[1,2,3],[5,7],[4,6]]);
##  Transformation( [ 4, 4, 4, 7, 3, 7, 3 ] )
##  gap&gt; RandomTransformationNC([[1,2,3],[5,7],[4,6]]);
##  Transformation( [ 1, 1, 1, 7, 5, 7, 5 ] )
##  gap&gt; RandomTransformation([1,2,3], 6);             
##  Transformation( [ 2, 1, 2, 1, 1, 2 ] )
##  gap&gt; RandomTransformationNC([1,2,3], 6);
##  Transformation( [ 3, 1, 2, 2, 1, 2 ] )
##	</Example> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

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

if ForAll(img, IsPosInt) and Maximum(img)<n then 
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

##	JDM replaces PermRepTrans

InstallMethod(AsPermOfRange, "for a transformation", true, [IsTransformation], 0,
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


#############################################################################
##
##	<#GAPDoc Label="InversesOfTransformation">
##	<ManSection><Heading>InversesOfTransformation</Heading>
##	<Oper Name="InversesOfTransformation" Arg="S, f"/>
##	<Oper Name="InversesOfTransformationNC" Arg="S, f"/>
##	<Description>
##	returns a list of the inverses of the transformation <C>f</C> in the 
##	transformation semigroup <C>S</C>. The function 
##	<C>InversesOfTransformationNC</C> will not check that <C>f</C> is an element 
##	of <C>S</C>.
##	<Example>
##  gap&gt; S:=Semigroup([ Transformation( [ 3, 1, 4, 2, 5, 2, 1, 6, 1 ] ), 
##    Transformation( [ 5, 7, 8, 8, 7, 5, 9, 1, 9 ] ), 
##    Transformation( [ 7, 6, 2, 8, 4, 7, 5, 8, 3 ] ) ]);;
##  gap&gt; f:=Transformation( [ 3, 1, 4, 2, 5, 2, 1, 6, 1 ] );;
##  gap&gt; InversesOfTransformationNC(S, f);
##  [  ]
##  gap&gt; IsRegularTransformation(S, f);
##  false
##  gap&gt; f:=Transformation( [ 1, 9, 7, 5, 5, 1, 9, 5, 1 ] );;
##  gap&gt; inv:=InversesOfTransformation(S, f);
##  [ Transformation( [ 1, 5, 1, 1, 5, 1, 3, 1, 2 ] ), 
##    Transformation( [ 1, 5, 1, 2, 5, 1, 3, 2, 2 ] ), 
##    Transformation( [ 1, 2, 3, 5, 5, 1, 3, 5, 2 ] ) ]
##  gap&gt; IsRegularTransformation(S, f);
##  true
##	</Example>
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(InversesOfTransformationNC, "for a trans. semigroup and a transformation", true, [IsTransformationSemigroup, IsTransformation], 0,
function(S, f)
local img, ker, imgs, kers, p, out, i;

out:=[];

if IsRegularTransformation(S, f) then 
	img:=AsSet(f![1]);
	ker:=KernelOfTransformation(f);
	imgs:=ImagesOfTransSemigroup(S, Length(img));
	#JDM better to have iterator commands here?
	kers:=Filtered(KernelsOfTransSemigroup(S, Length(img)), x-> IsTransversal(x,img));

	for i in imgs do
		if Length(OnSets(i, f))=Length(img) then
			p:=MappingPermListList(f![1]{i}, i);
			out:=Concatenation(out, List(kers, k-> IdempotentNC(k, img)*p));
		fi;
	od;	
fi;

if IsRegularSemigroup(S) then 
	return out;
fi;

return Filtered(out, x-> x in S);
end);

############

InstallMethod(InversesOfTransformation, "for a trans. semigroup and a transformation", true, [IsTransformationSemigroup, IsTransformation], 0,
function(S, f)

if f in S then 
	return InversesOfTransformationNC(S, f);
fi;

return fail;
end);