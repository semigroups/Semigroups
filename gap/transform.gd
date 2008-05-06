##
## transform.gd
## Version 3.1
## Fri May  2 17:42:56 BST 2008
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

DeclareOperation("IsRegularTransformation", [IsTransformationSemigroup, IsTransformation]);

#############################################################################
##
##	<#GAPDoc Label="IsTransversal">
##	<ManSection>
##	<Func Name="IsTransversal" Arg="ker, img"/>
##	<Description>
##	If <C>ker</C> is a partition of the set <C>{1,2,...,n}</C> and <C>img</C> 
##	a subset of the same set, then <C>IsTransversal</C>
##	returns <C>true</C> if <C>img</C> is a transversal of <C>ker</C>. That is, 
##	if every class in <C>ker</C> contains exactly one element in <C>img</C>. 
##	Otherwise <C>false</C> is returned.
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

DeclareGlobalFunction("IdempotentNC");
DeclareGlobalFunction("Idempotent");

#############################################################################
##
##	<#GAPDoc Label="PermRepTrans">
##	<ManSection>
##	<Oper Name="PermRepTrans" Arg="f"/>
##	<Description>
##	converts a transformation <C>f</C> that is a permutation of its image into 
##	that permutation.
##	<Example>
##  gap&gt; f:=Transformation([1,2,9,9,9,8,8,8,4]);
##  Transformation( [ 1, 2, 9, 9, 9, 8, 8, 8, 4 ] )
##  gap&gt; PermRepTrans(f);
##  (4,9)
##  gap&gt; f*last;
##  Transformation( [ 1, 2, 4, 4, 4, 8, 8, 8, 9 ] )
##  gap&gt; PermRepTrans(last);
##  ()
##	</Example> <!-- transform.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
##	</Description>

DeclareOperation("PermRepTrans", [IsTransformation]);

#############################################################################
##
##	<#GAPDoc Label="RandomTransformation">
##	<ManSection>
##	<Oper Name="RandomTransformation" Arg="ker, img"/>
##	<Description>
##	This is a new method for the existing library function 
##	<Ref Func="RandomTransformation" BookName="ref"/>.  A random transformation 
##	is returned that has the given image <C>img</C> and kernel <C>ker</C>. Note 
##	that <C>ker</C> must have the same number of classes as <C>img</C> has 
##	elements.
##	<Example>
##	gap&gt; f:=RandomTransformation([[1,2,3], [4,5], [6,7,8]], [1,2,3]);;
##	gap&gt; KernelOfTransformation(f)=[[1,2,3], [4,5], [6,7,8]];
##	true
##	gap&gt; ImageSetOfTransformation(f)=[1,2,3];
##	true
##	</Example> <!-- transform.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

#############################################################################
##
##	<#GAPDoc Label="IndexPeriodOfTransformation">
##	<ManSection>
##	<Oper Name="IndexPeriodOfTransformation" Arg="f"/>
##	<Description>
##	returns the minimum numbers <C>m, r</C> such that <C>f^(m+r)=f^m</C>; known 
##	as the <E>index</E> and <E>period</E> of the transformation. 
##	<Example>
##  gap&gt; f:=Transformation( [ 3, 4, 4, 6, 1, 3, 3, 7, 1 ] );;
##  gap&gt; IndexPeriodOfTransformation(f);
##  [ 2, 3 ]
##  gap&gt; f^2=f^5;
##  true
##	</Example> <!-- transform.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
##	</Description>

DeclareOperation("IndexPeriodOfTransformation", [IsTransformation]);