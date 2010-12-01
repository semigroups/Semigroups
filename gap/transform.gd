#############################################################################
##
#W  transform.gd
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

##  JDM install methods for partial transformations, partial bijections

#JDM remove the below when MN includes it in orb.
DeclareGlobalFunction("CanonicalTransSameKernel");

#JDM new for 3.2!
DeclareOperation("DegreeOfTransformationCollNC", [IsTransformationCollection]);

DeclareAttribute("Generators", IsSemigroup);
#JDM new for 4.0!

DeclareOperation("IsSubsemigroup", [IsTransformationSemigroup, 
 IsTransformationSemigroup]);

if not IsBound(AsPermutation) then 
	#JDM should remove AsPermutation from the library
	DeclareOperation("AsPermutation",[IsObject]);
fi;


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
##  <Example>
##  gap> g1:=Transformation([2,2,4,4,5,6]);;
##  gap> g2:=Transformation([5,3,4,4,6,6]);;
##  gap> m1:=Monoid(g1,g2);;
##  gap> IsRegularTransformation(m1, g1);
##  true
##  gap> img:=ImageSetOfTransformation(g1);
##  [ 2, 4, 5, 6 ]
##  gap> ker:=KernelOfTransformation(g1);
##  [ [ 1, 2 ], [ 3, 4 ], [ 5 ], [ 6 ] ]
##  gap> ForAny(MonoidOrbit(m1, img), x-> IsTransversal(ker, x));
##  true
##  gap> IsRegularTransformation(m1, g2);
##  false
##  gap> IsRegularTransformation(FullTransformationSemigroup(6), g2);
##  true
##	</Example> <!-- transform.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

DeclareOperation("IsRegularTransformation", [IsTransformationSemigroup, IsTransformation]);

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
##  gap> g1:=Transformation([2,2,4,4,5,6]);;
##  gap> g2:=Transformation([5,3,4,4,6,6]);;
##  gap> ker:=KernelOfTransformation(g2*g1);
##  [ [ 1 ], [ 2, 3, 4 ], [ 5, 6 ] ] 
##  gap> im:=ImageListOfTransformation(g2);
##  [ 5, 3, 4, 4, 6, 6 ]
##  gap> IsTransversal(ker, im);
##  false
##  gap> IsTransversal([[1,2,3],[4,5],[6,7]], [1,5,6]);
##  true
##  </Example> <!-- transform.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

##	JDM This is inefficient and should be avoided if we have a transformation. 
##	JDM Use Length(OnSets(f![1], f))=Length(f![1]) instead for a trans. f.

DeclareGlobalFunction("IsTransversal");

#############################################################################
##
##	<#GAPDoc Label="Idempotent">
##	<ManSection><Heading>Idempotent</Heading>
##	<Func Name="IdempotentNC" Arg="ker, img"/>
##	<Func Name="Idempotent" Arg="ker, img"/>
##	<Description>
##	<C>IdempotentNC</C> returns an idempotent with kernel <C>ker</C> and image 
##	<C>img</C> without checking <Ref Func="IsTransversal"/> with arguments 
##	<C>ker</C> and <C>im</C>. 
##	<P/>
##
##	<C>Idempotent</C> returns an idempotent with kernel <C>ker</C> and image 
##	<C>img</C> after checking that <Ref Func="IsTransversal"/> with arguments
##	<C>ker</C> and <C>im</C> returns <C>true</C>. <P/>
##	<Example>
##  gap> g1:=Transformation([2,2,4,4,5,6]);;
##  gap> g2:=Transformation([5,3,4,4,6,6]);;
##  gap> ker:=KernelOfTransformation(g2*g1);;
##  gap> im:=ImageListOfTransformation(g2);;
##  gap> Idempotent(ker, im);
##  Error,  the image must be a transversal of the kernel
##  [ ... ]
##  gap> Idempotent([[1,2,3],[4,5],[6,7]], [1,5,6]);
##  Transformation( [ 1, 1, 1, 5, 5, 6, 6 ] )
##  gap> IdempotentNC([[1,2,3],[4,5],[6,7]], [1,5,6]);
##  Transformation( [ 1, 1, 1, 5, 5, 6, 6 ] )
##	</Example> <!-- transform.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
##	</Description>

DeclareGlobalFunction("IdempotentNC");
DeclareGlobalFunction("Idempotent");

#############################################################################
##
##	<#GAPDoc Label="RandomTransformation">
##	<ManSection><Heading>RandomTransformation</Heading>
##	<Oper Name="RandomTransformation" Arg="arg"/>
##	<Oper Name="RandomTransformationNC" Arg="arg"/>
##	<Description>
##	These are new methods for the existing library function 
##	<Ref Func="RandomTransformation" BookName="ref"/>. <P/>
##	
##	If the first argument is a kernel and the second an image, then a random 
##	transformation is returned with this kernel and image.A <E>kernel</E> is a 
##	set of sets that partition the set 
##	<C>1,...n</C> for some <C>n</C> and an <E>image</E> is a sublist of 
##	<C>1,...n</C>.<P/>
##	
##	If the argument is a kernel, then a random transformation is returned that 
##	has that kernel. <P/>
##
##	If the first argument is a set of positive integers <C>img</C> and 
##	the second a positive
## integer <C>n</C>, then a random transformation of degree <C>n</C> is returned 
##	with image contained in the set <C>img</C>.<P/>
##
##	The no check version does not check that the arguments can be 
##	the kernel and image of a transformation.  
##	<Example>
##  gap> RandomTransformation([[1,2,3], [4,5], [6,7,8]], [1,2,3]);;
##  Transformation( [ 2, 2, 2, 1, 1, 3, 3, 3 ] )
##  gap> RandomTransformation([[1,2,3],[5,7],[4,6]]); 
##  Transformation( [ 3, 3, 3, 6, 1, 6, 1 ] )
##  gap> RandomTransformation([[1,2,3],[5,7],[4,6]]);
##  Transformation( [ 4, 4, 4, 7, 3, 7, 3 ] )
##  gap> RandomTransformationNC([[1,2,3],[5,7],[4,6]]);
##  Transformation( [ 1, 1, 1, 7, 5, 7, 5 ] )
##  gap> RandomTransformation([1,2,3], 6);             
##  Transformation( [ 2, 1, 2, 1, 1, 2 ] )
##  gap> RandomTransformationNC([1,2,3], 6);
##  Transformation( [ 3, 1, 2, 2, 1, 2 ] )
##	</Example> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("RandomTransformationNC", [IsCyclotomicCollColl, IsCyclotomicCollection]);

#############################################################################
##
##	<#GAPDoc Label="IndexPeriodOfTransformation">
##	<ManSection>
##	<Attr Name="IndexPeriodOfTransformation" Arg="f"/>
##	<Description>
##	returns the minimum numbers <C>m, r</C> such that <C>f^(m+r)=f^m</C>; known 
##	as the <E>index</E> and <E>period</E> of the transformation. 
##	<Example>
##  gap> f:=Transformation( [ 3, 4, 4, 6, 1, 3, 3, 7, 1 ] );;
##  gap> IndexPeriodOfTransformation(f);
##  [ 2, 3 ]
##  gap> f^2=f^5;
##  true
##	</Example> <!-- transform.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
##	</Description>

DeclareAttribute("IndexPeriodOfTransformation", IsTransformation);

#############################################################################
##
##	<#GAPDoc Label="TransformationActionNC">
##	<ManSection>
##	<Oper Name="TransformationActionNC" Arg="list, act, elm"/>
##	<Description>
##	returns the list <C>list</C> acted on by <C>elm</C> via the action 
##	<C>act</C>.
##	<Example>
##  gap> mat:=OneMutable(GeneratorsOfGroup(GL(3,3))[1]);
##  [ [ Z(3)^0, 0*Z(3), 0*Z(3) ], [ 0*Z(3), Z(3)^0, 0*Z(3) ], 
##    [ 0*Z(3), 0*Z(3), Z(3)^0 ] ]
##  gap> mat[3][3]:=Z(3)*0; 
##  0*Z(3)
##  gap> F:=BaseDomain(mat);
##  GF(3)
##  gap> TransformationActionNC(Elements(F^3), OnRight, mat);
##  Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 10, 10, 10, 13, 13, 13, 16, 16, 
##    16, 19, 19, 19, 22, 22, 22, 25, 25, 25 ] )
##	</Example> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
##	</Description>

DeclareOperation("TransformationActionNC", [IsList, IsFunction, IsObject]);

#############################################################################
##
##	<#GAPDoc Label="AsPermOfRange">
##	<ManSection>
##	<Oper Name="AsPermOfRange" Arg="x"/>
##	<Description>
##	converts a transformation <C>x</C> that is a permutation of its image into 
##	that permutation.
##	<Example>
##  gap> t:=Transformation([1,2,9,9,9,8,8,8,4]);
##  Transformation( [ 1, 2, 9, 9, 9, 8, 8, 8, 4 ] )
##  gap> AsPermOfRange(t);
##  (4,9)
##  gap> t*last;
##  Transformation( [ 1, 2, 4, 4, 4, 8, 8, 8, 9 ] )
##  gap> AsPermOfRange(last);
##  ()
##	</Example> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
##	</Description>
##	</Description>

DeclareOperation("AsPermOfRange", [IsTransformation]);

###########################################################################
##
##	<#GAPDoc Label="SmallestIdempotentPower">
##	<ManSection> 
##	<Attr Name="SmallestIdempotentPower" Arg="f"/>
##	<Description>
##	returns the least natural number <C>n</C> such that the transformation 
##	<C>f^n</C> is an idempotent.
##	<Example>
##  gap> t:=Transformation( [ 6, 7, 4, 1, 7, 4, 6, 1, 3, 4 ] );;
##  gap> SmallestIdempotentPower(t);
##  6
##  gap> t:=Transformation( [ 6, 6, 6, 2, 7, 1, 5, 3, 10, 6 ] );;
##  gap> SmallestIdempotentPower(t);
##  4
##	</Example>
##	</Description>  
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("SmallestIdempotentPower", IsTransformation);

###########################################################################
##
##	<#GAPDoc Label="IsKerImgOfTransformation">
##	<ManSection> 
##	<Func Name="IsKerImgOfTransformation" Arg="ker, img"/>
##	<Description>
##	returns <C>true</C> if the arguments <C>ker</C> and <C>img</C> can be the 
##	kernel and image of 
##	a single transformation, respectively.  The argument <C>ker</C> should be a 
##	set of sets that partition the set <C>1,...n</C> for some <C>n</C> and 
##	<C>img</C> should be a sublist of <C>1,...n</C>.
##	<Example>
##  gap> ker:=[[1,2,3],[5,6],[8]];
##  [ [ 1, 2, 3 ], [ 5, 6 ], [ 8 ] ]
##  gap> img:=[1,2,9];
##  [ 1, 2, 9 ]
##  gap> IsKerImgOfTransformation(ker,img);
##  false
##  gap> ker:=[[1,2,3,4],[5,6,7],[8]];
##  [ [ 1, 2, 3, 4 ], [ 5, 6, 7 ], [ 8 ] ]
##  gap> IsKerImgOfTransformation(ker,img);
##  false
##  gap> img:=[1,2,8];
##  [ 1, 2, 8 ]
##  gap> IsKerImgOfTransformation(ker,img);
##  true
##	</Example>
##	</Description>  
##	</ManSection>
##	<#/GAPDoc>

DeclareGlobalFunction("IsKerImgOfTransformation");

###########################################################################
##
##	<#GAPDoc Label="TransformationByKernelAndImage">
##	<ManSection><Heading>TransformationByKernelAndImage</Heading>
##	<Oper Name="TransformationByKernelAndImage" Arg="ker, img"/>
##	<Oper Name="TransformationByKernelAndImageNC" Arg="ker, img"/>
##	<Description>
##	returns the transformation <C>f</C> with kernel <C>ker</C> and image 
##	<C>img</C> where <C>(x)f=img[i]</C> for all <C>x</C> in <C>ker[i]</C>.  The 
##	argument <C>ker</C> should be a 
##	set of sets that partition the set <C>1,...n</C> for some <C>n</C> and 
##	<C>img</C> should be a sublist of <C>1,...n</C>. <P/>
##
##	<C>TransformationByKernelAndImage</C> first checks that <C>ker</C> and 
##	<C>img</C> describe the kernel and image of a transformation whereas 
##	<C>TransformationByKernelAndImageNC</C> performs no such check.
##	<Example>
##  gap> TransformationByKernelAndImageNC([[1,2,3,4],[5,6,7],[8]],[1,2,8]);
##  Transformation( [ 1, 1, 1, 1, 2, 2, 2, 8 ] )
##  gap> TransformationByKernelAndImageNC([[1,6],[2,5],[3,4]], [4,5,6]);
##  Transformation( [ 4, 5, 6, 6, 5, 4 ] )
##	</Example>
##	</Description>  
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("TransformationByKernelAndImageNC", [IsCyclotomicCollColl, IsCyclotomicCollection]);
DeclareOperation("TransformationByKernelAndImage", [IsCyclotomicCollColl, IsCyclotomicCollection]);

###########################################################################
##
##	<#GAPDoc Label="AllTransformationsWithKerAndImg">
##	<ManSection><Heading>AllTransformationsWithKerAndImg</Heading>
##	<Oper Name="AllTransformationsWithKerAndImg" Arg="ker, img"/>
##	<Oper Name="AllTransformationsWithKerAndImgNC" Arg="ker, img"/>
##	<Description>
##	returns a list of all transformations with kernel <C>ker</C> and image
##	<C>img</C>.   The 
##	argument <C>ker</C> should be a 
##	set of sets that partition the set <C>1,...n</C> for some <C>n</C> and 
##	<C>img</C> should be a sublist of <C>1,...n</C>. <P/>
##
##	<C>AllTransformationsWithKerAndImg</C> first checks that <C>ker</C> and 
##	<C>img</C> describe the kernel and image of a transformation whereas 
##	<C>AllTransformationsWithKerAndImgNC</C> performs no such check.
##	<Example>
##  gap> AllTransformationsWithKerAndImg([[1,6],[2,5],[3,4]], [4,5,6]);
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

DeclareOperation("AllTransformationsWithKerAndImgNC", [IsCyclotomicCollColl, IsCyclotomicCollection]);
DeclareOperation("AllTransformationsWithKerAndImg", [IsCyclotomicCollColl, IsCyclotomicCollection]);


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
##  gap> t:=Transformation( [ 10, 8, 7, 2, 8, 2, 2, 6, 4, 1 ] );;
##  gap> KerImgOfTransformation(t);
##  [ [ [ 1 ], [ 2, 5 ], [ 3 ], [ 4, 6, 7 ], [ 8 ], [ 9 ], [ 10 ] ], 
##    [ 1, 2, 4, 6, 7, 8, 10 ] ]
##	</Example>
##	</Description>  
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("KerImgOfTransformation", [IsTransformation]);

###########################################################################
##
##	<#GAPDoc Label="AsBooleanMatrix">
##	<ManSection> 
##	<Oper Name="AsBooleanMatrix" Arg="f[,n]"/>
##	<Description>
##	returns the transformation or permutation <C>f</C> represented as an 
##	<C>n</C> by <C>n</C> Boolean matrix where <C>i,f(i)</C>th entries equal 
##	<C>1</C> and all other entries are <C>0</C>.<P/>
##
##	If <C>f</C> is a transformation, then 
##	<C>n</C> is the size of the domain of <C>f</C>. <P/>
##	
##	If <C>f</C> is a permutation, then <C>n</C> is the number of points moved by 
##	<C>f</C>. 
##	
##	<Example>
##  gap> t:=Transformation( [ 4, 2, 2, 1 ] );;
##  gap> AsBooleanMatrix(t);
##  [ [ 0, 0, 0, 1 ], [ 0, 1, 0, 0 ], [ 0, 1, 0, 0 ], [ 1, 0, 0, 0 ] ]
##  gap> t:=(1,4,5);;
##  gap> AsBooleanMatrix(t);
##  [ [ 0, 0, 0, 1, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 1 ],
##    [ 1, 0, 0, 0, 0 ] ]
##  gap> AsBooleanMatrix(t,3);
##  fail
##  gap> AsBooleanMatrix(t,5);
##  [ [ 0, 0, 0, 1, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 1 ],
##    [ 1, 0, 0, 0, 0 ] ]
##  gap> AsBooleanMatrix(t,6);
##  [ [ 0, 0, 0, 1, 0, 0 ], [ 0, 1, 0, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ], 
##    [ 0, 0, 0, 0, 1, 0 ], [ 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1 ] ]
##	</Example>
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("AsBooleanMatrix", [IsTransformation]);

#############################################################################
##
##	<#GAPDoc Label="RandomIdempotent">
##	<ManSection><Heading>RandomIdempotent</Heading>
##	<Oper Name="RandomIdempotent" Arg="arg"/>
##	<Oper Name="RandomIdempotentNC" Arg="arg"/>
##	<Description>
##	If the argument is a kernel, then a random idempotent is return that has 
##	that kernel. A <E>kernel</E> is a set of sets that partition the set 
##	<C>1,...n</C> for some <C>n</C> and an <E>image</E> is a sublist of 
##	<C>1,...n</C>.<P/>
##
##	If the first argument is an image <C>img</C> and the second a positive
## integer <C>n</C>, then a random idempotent of degree <C>n</C> is returned 
##	with image <C>img</C>.<P/>
##
##	The no check version does not check that the arguments can be 
##	the kernel and image of an idempotent.  
##	<Example>
##  gap> RandomIdempotent([[1,2,3], [4,5], [6,7,8]], [1,2,3]);;
##  fail
##  gap> RandomIdempotent([1,2,3],5);
##  Transformation( [ 1, 2, 3, 1, 3 ] )
##  gap> RandomIdempotent([[1,6], [2,4], [3,5]]);
##  Transformation( [ 1, 2, 5, 2, 5, 1 ] )
##	</Example> <!-- transform.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("RandomIdempotent", [IsCyclotomicCollColl]);
DeclareOperation("RandomIdempotentNC", [IsCyclotomicCollColl]);


#############################################################################
##
##	<#GAPDoc Label="InversesOfTransformation">
##	<ManSection><Heading>InversesOfTransformation</Heading>
##	<Oper Name="InversesOfTransformation" Arg="S, f"/>
##	<Oper Name="InversesOfTransformationNC" Arg="S, f"/>
##	<Description>
##	returns a list of the inverses of the transformation <C>f</C> in the 
##	transformation semigroup <C>S</C>. The function 
##	<C>InversesOfTransformationNC</C> does not check that <C>f</C> is an element 
##	of <C>S</C>.
##	<Example>
##  gap> S:=Semigroup([ Transformation( [ 3, 1, 4, 2, 5, 2, 1, 6, 1 ] ), 
##    Transformation( [ 5, 7, 8, 8, 7, 5, 9, 1, 9 ] ), 
##    Transformation( [ 7, 6, 2, 8, 4, 7, 5, 8, 3 ] ) ]);;
##  gap> f:=Transformation( [ 3, 1, 4, 2, 5, 2, 1, 6, 1 ] );;
##  gap> InversesOfTransformationNC(S, f);
##  [  ]
##  gap> IsRegularTransformation(S, f);
##  false
##  gap> f:=Transformation( [ 1, 9, 7, 5, 5, 1, 9, 5, 1 ] );;
##  gap> inv:=InversesOfTransformation(S, f);
##  [ Transformation( [ 1, 5, 1, 1, 5, 1, 3, 1, 2 ] ), 
##    Transformation( [ 1, 5, 1, 2, 5, 1, 3, 2, 2 ] ), 
##    Transformation( [ 1, 2, 3, 5, 5, 1, 3, 5, 2 ] ) ]
##  gap> IsRegularTransformation(S, f);
##  true
##	</Example>
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("InversesOfTransformationNC", [IsTransformationSemigroup, IsTransformation]);
DeclareOperation("InversesOfTransformation", [IsTransformationSemigroup, IsTransformation]);

# JDM new for 4.0!

DeclareGlobalFunction("PermRightQuoTransformationNC");
DeclareGlobalFunction("AsPermOfKerImg");
DeclareGlobalFunction("ConstantTransformation");
#DeclareCategory("IsConstantTransformation", IsTransformation);

