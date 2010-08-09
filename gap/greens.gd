#############################################################################
##
#W  greens.gd
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##
##  This file contains fast algorithms for computing Green's relations
##  and related notions for transformation semigroups and monoid. 
##  The theory behind these algorithms is developed in 
##  
##  [LPRR1] S. A.   Linton, G.  Pfeiffer, E.  F.  Robertson, and N.   Ruskuc,
##  Groups  and actions in  transformation semigroups, to appear in Math Z.
##  (1998).
##
##  The algorithms themselves are described in
##
##  [LPRR2] S. A.   Linton, G.  Pfeiffer, E.  F.  Robertson, and N.   Ruskuc,
##  Computing transformation semigroups, (1998), in preparation.
##  
##  Another reference is
##
##  [LM]  G.  Lallement and R. McFadden, On the   determination of Green's
##  relations in finite transformation semigroups, J. Symbolic Computation 10
##  (1990), 481--489.
##
#############################################################################
#############################################################################




###########################################################################
##
##	<#GAPDoc Label="IsGreensClassOfTransSemigp">
##	<ManSection>
##	<Prop Name="IsGreensClassOfTransSemigp" Arg="C"/>
##	<Description>
##	returns <C>true</C> if <C>C</C> is a Green's class of a transformation 
##	semigroup and returns <C>false</C> otherwise.<P/>
##
##	This attribute is required so that a Green's class knowns that it belongs to 
##	a transformation semigroup, so that the methods defined in the
##	<Package>MONOID</Package> are used in preference to those in the library.
##	<Example>
##  gap> a:=Transformation( [ 2, 1, 4, 5, 6, 3 ] );;
##  gap> b:=Transformation( [ 2, 3, 1, 5, 4, 1 ] );;
##  gap> M:=Semigroup(a,b);;
##  gap> GreensLClassOfElement(M,a);
##  {Transformation( [ 2, 1, 4, 5, 6, 3 ] )}
##  gap> IsGreensClassOfTransSemigp(last);
##  true
##  gap> f:=FreeSemigroup(3);;
##  gap> a:=f.1;; b:=f.2;; c:=f.3;; 
##  gap> s:=f/[[a^2, a], [b^2,b], [c^2,c], [a*b,a], [b*a,b], [a*c,a], 
##  > [c*a,c], [b*c,b],[c*b,c]];
##  &lt;fp semigroup on the generators [ s1, s2, s3 ]>
##  gap> Size(s);
##  3
##  gap> GreensLClassOfElement(s,a);
##  {s1}
##  gap> IsGreensClassOfTransSemigp(last);
##  false
##	</Example> <!-- greens.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

DeclareProperty("IsGreensClassOfTransSemigp", IsGreensClass);

###########################################################################
##
##	<#GAPDoc Label="RClassData">
##	<ManSection><Heading>XClassData</Heading>
##	<Func Name="RClassData" Arg="rec"/>
##	<Func Name="LClassData" Arg="rec"/>
##	<Func Name="HClassData" Arg="rec"/>
##	<Func Name="DClassData" Arg="rec"/>
##	<Description>
##	These function construct objects in the categories
##	<Ref Filt="IsGreensRClassData"/>,
##		<Ref Filt="IsGreensLClassData"/>,
##		<Ref Filt="IsGreensHClassData"/>, and 
##		<Ref Filt="IsGreensDClassData"/>
##	subcategories of <Ref Func="IsGreensData"/> using the output from 
##	<Ref Func="GreensRClassData"/>,  <Ref Func="GreensLClassData"/>, <Ref Func="GreensHClassData"/>, <Ref Func="GreensDClassData"/>.  
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareGlobalFunction("RClassData");
DeclareGlobalFunction("LClassData");
DeclareGlobalFunction("HClassData");
DeclareGlobalFunction("DClassData");

###########################################################################
##
##	<#GAPDoc Label="IsGreensData">
##	<ManSection><Heading>IsGreensData</Heading>
##	<Filt Name="IsGreensData" Arg="obj" Type="Category"/>
##	<Filt Name="IsGreensRClassData" Arg="obj" Type="Category"/>
##	<Filt Name="IsGreensLClassData" Arg="obj" Type="Category"/>
##	<Filt Name="IsGreensHClassData" Arg="obj" Type="Category"/>
##	<Filt Name="IsGreensDClassData" Arg="obj" Type="Category"/>
##	<Description>
##		returns <C>true</C> if <C>obj</C> lies in the category 
##		<C>IsGreensData</C>, <C>IsGreensRClassData</C>, <C>IsGreensLClassData</C>, 
##		<C>IsGreensHClassData</C>, <C>IsGreensDClassData</C>, respectively. The 
##		objects created using the functions <Ref Func="RClassData"/>, 
##		<Ref Func="LClassData"/>, <Ref Func="HClassData"/>, and 
##		<Ref Func="DClassData"/> lie in the 
##		categories <C>IsGreensRClassData</C>, <C>IsGreensLClassData</C>, 
##		<C>IsGreensHClassData</C>, <C>IsGreensDClassData</C>, respectively, and all 
##		these categories are contained in the category <C>IsGreensData</C>.
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

DeclareCategory("IsGreensData", IsObject);
DeclareCategory("IsGreensRClassData", IsGreensData);
DeclareCategory("IsGreensLClassData", IsGreensData);
DeclareCategory("IsGreensHClassData", IsGreensData);
DeclareCategory("IsGreensDClassData", IsGreensData);

###########################################################################
##
##	<#GAPDoc Label="GreensData">
##	<ManSection>
##	<Oper Name="GreensData" Arg="C"/>
##	<Description>
##	<List>
##	<Item> if <C>C</C> satisfies <Ref Prop="IsGreensRClass" BookName="ref"/>, 
##	then <C>GreensData</C> returns the value of <Ref Oper="GreensRClassData"/> 
##	with argument <C>C</C>.</Item>
##	<Item> if <C>C</C> satisfies <Ref Prop="IsGreensLClass" BookName="ref"/>, 
##	then <C>GreensData</C> returns the value of <Ref Oper="GreensLClassData"/> 
##	with argument <C>C</C>.</Item>
##	<Item> if <C>C</C> satisfies <Ref Prop="IsGreensHClass" BookName="ref"/>, 
##	then <C>GreensData</C> returns the value of <Ref Oper="GreensHClassData"/> 
##	with argument <C>C</C>.</Item>
##	<Item> if <C>C</C> satisfies <Ref Prop="IsGreensDClass" BookName="ref"/>, 
##	then <C>GreensData</C> returns the value of <Ref Oper="GreensDClassData"/> 
##	with argument <C>C</C>.</Item>
##	</List>
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

DeclareOperation("GreensData", [IsGreensClass and IsGreensClassOfTransSemigp]);

###########################################################################
##
##	<#GAPDoc Label="OnTuplesOfSetsAntiAction">
##	<ManSection>
##	<Func Name="OnTuplesOfSetsAntiAction" Arg="tup, f"/>
##	<Description>
##	returns the preimages of each of the sets in the tuple of sets <C>tup</C> 
##	under the transformation <C>f</C>. The tuple of sets <C>tup</C> can have 
##	any number of elements.
##	<Example>
##  gap> f:=Transformation( [ 8, 7, 5, 3, 1, 3, 8, 8 ] );;
##  gap> OnTuplesOfSetsAntiAction([ [ 1, 2 ], [ 3 ], [ 4 ], [ 5 ] ], f);
##  [ [ 5 ], [ 4, 6 ], [ 3 ] ]
##	</Example> <!-- greens.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

DeclareGlobalFunction("OnTuplesOfSetsAntiAction");

###########################################################################
##
##	<#GAPDoc Label="OnKernelsAntiAction">
##	<ManSection>
##	<Func Name="OnKernelsAntiAction" Arg="ker, f"/>
##	<Description>
##	returns the kernel of the product <C>f*g</C> of the transformation <C>f</C> 
##	with a transformation <C>g</C> having kernel <C>ker</C>. 
##	<Example>
##  gap> f:=Transformation( [ 8, 7, 5, 3, 1, 3, 8, 8 ] );;
##  gap> OnKernelsAntiAction([ [ 1, 2 ], [ 3 ], [ 4 ], [ 5 ], [ 6, 7, 8 ] ], f);
##  [ [ 1, 2, 7, 8 ], [ 3 ], [ 4, 6 ], [ 5 ] ]
##	</Example> <!-- greens.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

DeclareGlobalFunction("OnKernelsAntiAction");

###########################################################################
##
##	<#GAPDoc Label="ImagesOfTransSemigroup">
##	<ManSection>
##	<Attr Name="ImagesOfTransSemigroup" Arg="S[,n]"/>
##	<Description>
##	returns the set of all the image sets that elements of <C>S</C> admit. That 
##	is, the union of the orbits of the image sets of the generators of 
##	<C>S</C> under the action <Ref Func="OnSets" BookName="ref"/>. <P/>
##
##	If the 
##	optional second argument <C>n</C> (a positive integer) is present, then the 
##	list of image sets of size <C>n</C> is returned.  If you are only interested 
##	in the images of a given size, then the second version of the function will 
##	likely be faster.
##	<Example>
##  gap>  S:=Semigroup([ Transformation( [ 6, 4, 4, 4, 6, 1 ] ), 
##  > Transformation( [ 6, 5, 1, 6, 2, 2 ] ) ];;
##  gap> ImagesOfTransSemigroup(S, 6);
##  [  ]
##  gap> ImagesOfTransSemigroup(S, 5);
##  [  ]
##  gap> ImagesOfTransSemigroup(S, 4);
##  [ [ 1, 2, 5, 6 ] ]
##  gap> ImagesOfTransSemigroup(S, 3);
##  [ [ 1, 4, 6 ], [ 2, 5, 6 ] ]
##  gap> ImagesOfTransSemigroup(S, 2);
##  [ [ 1, 4 ], [ 2, 5 ], [ 2, 6 ], [ 4, 6 ] ]
##  gap> ImagesOfTransSemigroup(S, 1);
##  [ [ 1 ], [ 2 ], [ 4 ], [ 5 ], [ 6 ] ]
##  gap> ImagesOfTransSemigroup(S);
##  [ [ 1 ], [ 1, 2, 5, 6 ], [ 1, 4 ], [ 1, 4, 6 ], [ 2 ], [ 2, 5 ], [ 2, 5, 6 ], 
##    [ 2, 6 ], [ 4 ], [ 4, 6 ], [ 5 ], [ 6 ] ]
##  </Example> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

#DeclareOperation("ImagesOfTransSemigroup", [IsTransformationSemigroup, IsPosInt]);

## JDM why is this mutable?

DeclareAttribute("ImagesOfTransSemigroup", IsTransformationSemigroup, "mutable");

###########################################################################
##
##	<#GAPDoc Label="KernelsOfTransSemigroup">
##	<ManSection>
##	<Attr Name="KernelsOfTransSemigroup" Arg="S[, n]"/>
##	<Description>
##	returns the set of all the kernels that elements of <C>S</C> admit.
##	This is just the union of the orbits of the kernels of the generators of 
##	<C>S</C> under the action <Ref Func="OnKernelsAntiAction"/>.<P/>
##
##	If the optional second argument <C>n</C> (a positive integer) is present, 
##	then the list of image sets of size <C>n</C> is returned.  If you are only 
##	interested in the images of a given size, then the second version of the 
##	function will likely be faster.
##	<Example>
##  gap>  S:=Semigroup([ Transformation( [ 2, 4, 1, 2 ] ),
##  > Transformation( [ 3, 3, 4, 1 ] ) ]);
##  gap>  KernelsOfTransSemigroup(S);   
##  [ [ [ 1, 2 ], [ 3 ], [ 4 ] ], [ [ 1, 2 ], [ 3, 4 ] ], [ [ 1, 2, 3 ], 
##  [ 4 ] ], 
##    [ [ 1, 2, 3, 4 ] ], [ [ 1, 2, 4 ], [ 3 ] ], [ [ 1, 3, 4 ], [ 2 ] ], 
##    [ [ 1, 4 ], [ 2 ], [ 3 ] ], [ [ 1, 4 ], [ 2, 3 ] ] ]
##  gap>  KernelsOfTransSemigroup(S,1);
##  [ [ [ 1, 2, 3, 4 ] ] ]
##  gap>  KernelsOfTransSemigroup(S,2);
##  [ [ [ 1, 2 ], [ 3, 4 ] ], [ [ 1, 2, 3 ], [ 4 ] ], [ [ 1, 2, 4 ], [ 3 ] ], 
##    [ [ 1, 3, 4 ], [ 2 ] ], [ [ 1, 4 ], [ 2, 3 ] ] ]
##  gap>  KernelsOfTransSemigroup(S,3);
##  [ [ [ 1, 2 ], [ 3 ], [ 4 ] ], [ [ 1, 4 ], [ 2 ], [ 3 ] ] ]
##  gap>  KernelsOfTransSemigroup(S,4);
##  [  ]
##	</Example> <!-- greens.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

##  JDM why are these mutable?

DeclareAttribute("KernelsOfTransSemigroup", IsTransformationSemigroup, "mutable");

## JDM this could certainly be improved
## JDM probably only need one of these and InternalKernels... check properties.gi too :)
## JDM why are these mutable?

DeclareAttribute("InternalKernels", IsTransformationSemigroup, "mutable");

#############################################################################
##
##	<#GAPDoc Label="StrongOrbitOfImage">
##	<ManSection>
##	<Oper Name="StrongOrbitOfImage" Arg="S, f"/>
##	<Description>
##	returns a triple where
##	<List>
##	<Item>the first entry <C>imgs</C> is the strong orbit of the image set 
##	<C>A</C> 
##	of <C>f</C> under the action of  <C>S</C>. That is, the set of image sets 
##	<C>B</C> of 
##	elements of <C>S</C> such that there exist <C>g,h</C> in <C>S</C> with 
##	<C>OnSets(A, g)=B</C> and <C>OnSet(B, h)=A</C>. If the strong orbit of the 
##	image of <C>f</C> has 
##	already been calculated, then the image of <C>f</C> might not be the first 
##	entry in the list <C>imgs</C>.</Item>
##	<Item>the second entry is a list of permutations <C>mults</C> such that 
##	<C>OnSets(imgs[i], mults[i])=imgs[1]</C>.</Item>
##	<Item>the third entry is the Right Schutzenberger group associated to the
##	first entry in the list <C>imgs</C> (that is, the group of permutations 
##	arising 
##	from elements of the semigroup that stabilise the set <C>imgs[1]</C>).
##	</Item>
##	</List>
##	<Example>
##  gap> gens:=[ Transformation( [ 3, 5, 2, 5, 1 ] ), 
##  > Transformation( [ 4, 3, 2, 1, 5 ] ) ];;
##  gap> S:=Semigroup(gens);;
##  gap> f:=Transformation( [ 2, 1, 1, 1, 5 ] );;
##  gap> StrongOrbitOfImage(S, f);        
##  [ [ [ 1, 2, 5 ], [ 1, 3, 5 ], [ 1, 2, 3 ], [ 2, 3, 5 ], [ 2, 3, 4 ], 
##        [ 2, 4, 5 ], [ 3, 4, 5 ] ], 
##    [ (), (1,5,2,3), (1,2)(3,5,4), (1,3,2,5), (1,3)(2,5,4), (1,3,4,5,2), 
##        (1,3,2,4) ], Group([ (), (2,5), (1,5) ]) ]
##	</Example> <!-- greens.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("StrongOrbitOfImage", [IsTransformationSemigroup, IsTransformation]);

#############################################################################
##
##	<#GAPDoc Label="StrongOrbitsOfImages">
##	<ManSection>
##	<Attr Name="StrongOrbitsOfImages" Arg="S"/>
##	<Description>
##	this is a mutable attribute that stores the result of 
##	<Ref Oper="StrongOrbitOfImage"/> every time it is used. 
##	If <Ref Oper="StrongOrbitOfImage"/> has not been invoked for <C>S</C>, 
##	then an 
##	error is returned.
##	<Example>
##  gap> gens:=[ Transformation( [ 3, 5, 2, 5, 1 ] ), 
##  > Transformation( [ 4, 3, 2, 1, 5 ] ) ];;
##  gap> S:=Semigroup(gens);;
##  gap> f:=Transformation( [ 2, 1, 1, 1, 5 ] );;
##  gap> StrongOrbitOfImage(S, f);;
##  gap> StrongOrbitsOfImages(S);
##  [ [ [ [ 1, 2, 5 ], [ 1, 3, 5 ], [ 1, 2, 3 ], [ 2, 3, 5 ], [ 2, 3, 4 ], 
##            [ 2, 4, 5 ], [ 3, 4, 5 ] ] ], 
##    [ [ (), (1,5,2,3), (1,2)(3,5,4), (1,3,2,5), (1,3)(2,5,4), (1,3,4,5,2), 
##            (1,3,2,4) ] ], [ Group([ (), (2,5), (1,5) ]) ] ]
##  gap> f:=Transformation( [ 5, 5, 5, 5, 2 ] );
##  gap> StrongOrbitOfImage(S, f);;
##  gap> StrongOrbitsOfImages(S); 
##  [ [ [ [ 1, 2, 5 ], [ 1, 3, 5 ], [ 1, 2, 3 ], [ 2, 3, 5 ], [ 2, 3, 4 ], 
##            [ 2, 4, 5 ], [ 3, 4, 5 ] ], 
##        [ [ 2, 5 ], [ 1, 5 ], [ 1, 3 ], [ 2, 3 ], [ 2, 4 ], [ 4, 5 ], [ 3, 5 ], 
##            [ 1, 2 ], [ 3, 4 ] ] ], 
##    [ [ (), (1,5,2,3), (1,2)(3,5,4), (1,3,2,5), (1,3)(2,5,4), (1,3,4,5,2), 
##            (1,3,2,4) ], 
##        [ (), (1,5,2), (1,2)(3,5,4), (2,5,4,3), (2,5,4), (2,3,4,5), (2,3), 
##            (1,5,4,3), (2,3)(4,5) ] ], 
##    [ Group([ (), (2,5), (1,5) ]), Group([ (), (2,5) ]) ] ]
##	</Example><!-- greens.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("StrongOrbitsOfImages", IsTransformationSemigroup, "mutable");




#############################################################################
##
##	<#GAPDoc Label="IsGreensRClassDataRep">
##	<ManSection><Heading>IsGreensXClassDataRep</Heading>
##	<Filt Name="IsGreensRClassDataRep" Arg="obj" Type="Representation"/>
##	<Filt Name="IsGreensLClassDataRep" Arg="obj" Type="Representation"/>
##	<Filt Name="IsGreensHClassDataRep" Arg="obj" Type="Representation"/>
##	<Filt Name="IsGreensDClassDataRep" Arg="obj" Type="Representation"/>
##	<Description>
##	returns <C>true</C> if <C>obj</C> has <C>IsGreensRClassDataRep</C>, 
##	<C>IsGreensLClassDataRep</C>, <C>IsGreensHClassDataRep</C>, or 
##	<C>IsGreensDClassDataRep</C>, respectively as a representation.<P/> 
##
##	These representations each have several components detailed in 
##	<Ref Attr="GreensRClassData"/>, <Ref Attr="GreensLClassData"/>, 
##	<Ref Attr="GreensHClassData"/>, <Ref Attr="GreensDClassData"/>, 
##	respectively.
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareRepresentation("IsGreensRClassDataRep",  
		      IsComponentObjectRep and IsAttributeStoringRep, 
		      [ "rep", "strongorb", "perms", "schutz" ] );

DeclareRepresentation("IsGreensLClassDataRep",  
                       IsComponentObjectRep and IsAttributeStoringRep, 
                       [ "rep", "strongorb", "relts", "invrelts", "schutz" ] );

DeclareRepresentation("IsGreensHClassDataRep",  IsComponentObjectRep and IsAttributeStoringRep, 
                       [ "rep", "schutz" ] );

DeclareRepresentation("IsGreensDClassDataRep",  
		       IsComponentObjectRep and IsAttributeStoringRep, 
                       [ "rep", "R", "L", "H", "cosets", "schutz"] );

#############################################################################
##
##	<#GAPDoc Label="GreensRClassData">
##	<ManSection>
##	<Attr Name="GreensRClassData" Arg="C"/>
##	<Description>
##	if <C>C</C> satisfies <Ref Attr="IsGreensRClass" BookName="ref"/>, then 
##	<C>GreensRClassData</C> returns an object in the category 
##	<Ref Filt="IsGreensRClassData"/> with representation 
##	<Ref Filt="IsGreensRClassDataRep"/> and the following four components:
##	<List>
##		<Item> <C>rep</C> the representative of the <M>R</M>-class.</Item>
##		<Item> <C>strongorb</C> the strong orbit of the image of <C>rep</C> under 
##		the action of the semigroup on sets.</Item>
##		<Item><C>perms</C> a list of permutations that map the image of <C>rep</C> 
##		to the corresponding image in <C>strongorb</C>, that is, 
##		<C>OnSets(strongorb[i], perms[i])=strongorb[1]</C>.
##		</Item>
##		<Item> <C>schutz</C> the group of permutations arising from elements 
##		of the semigroup that stabilise the image of <C>rep</C> (called the <E>
##		(generalized) right Schutzenberger group</E>).</Item>
##	</List>
##	The components <C>strongorb</C>, <C>perms</C>, and <C>schutz</C> are 
##	obtained using the function <Ref Func="StrongOrbitOfImage"/>. Further 
##	details can be found in Algorithm C, D, E, F, and U of 
##	<Cite  Key="pfeiffer2"/>.
##	<Example>
##  gap> a:=Transformation( [ 2, 1, 4, 5, 6, 3 ] );;
##  gap> b:=Transformation( [ 2, 3, 1, 5, 4, 1 ] );;
##  gap> M:=Semigroup(a,b);;
##  gap> rc:=GreensRClassOfElement(M, a*b*a);
##  {Transformation( [ 4, 1, 6, 5, 2, 2 ] )}
##  gap> GreensRClassData(rc);
##  GreensRClassData( Transformation( [ 4, 1, 6, 5, 2, 2 ] ), [ [ 1, 2, 4, 5, 6 ], 
##  [ 1, 2, 3, 5, 6 ], [ 1, 2, 3, 4, 6 ], [ 1, 2, 3, 4, 5 ] ], [ (), (1,2)
##  (3,6,5,4), (3,5)(4,6), (1,6,3,2)(4,5) ], Group( [ (), (2,4,6), (2,6,4), 
##  (1,2,6)(4,5) ] ) )
##	</Example><!-- greens.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("GreensRClassData", IsGreensRClass and IsGreensClassOfTransSemigp);



#############################################################################
##
##	<#GAPDoc Label="GreensRClassReps">
##	<ManSection>
##	<Attr Name="GreensRClassReps" Arg="S"/>
##	<Description>
##	returns the representative of all the R-classes of the transformation 
##	semigroup <C>S</C> as a list of lists ordered by the rank of the 
##	representatives and the order that the representatives are produced by the 
##	function <Ref Attr="GreensRClasses" BookName="ref"/>.  Also 
##	<C>GreensRClassReps[i][j]</C> is the representative of 
##	<Ref Attr="GradedRClasses"/><C>[i][j]</C>.
##	<Example>
##  gap> gens:=[ Transformation( [ 1, 2, 1, 2, 1 ] ), 
##  > Transformation( [ 3, 4, 2, 1, 4 ] ) ];;
##  gap> S:=Semigroup(gens);; 
##  gap> GreensRClassReps(S);
##  [ [ Transformation( [ 1, 2, 1, 2, 1 ] ), 
##    Transformation( [ 1, 2, 2, 1, 2 ] ), 
##        Transformation( [ 2, 1, 2, 1, 1 ] ) ], 
##    [ Transformation( [ 3, 4, 2, 1, 4 ] ) ] ]
##	</Example><!-- greens.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("GreensRClassReps", IsTransformationSemigroup);



#############################################################################
##
##	<#GAPDoc Label="GradedRClasses">
##	<ManSection>
##	<Attr Name="GradedRClasses" Arg="S"/>
##	<Description>
##	returns the <C>GreensRClassData</C> for all the <M>R</M>-classes of <C>S</C> 
##	in a list of lists ordered by the rank of the representatives and the order 
##	that the classes are produced by the function 
##	<Ref Attr="GreensRClasses" BookName="ref"/>. Also <C>GreensRClassReps[i][j]
##	</C> is the representative of <Ref Attr="GradedRClasses"/><C>[i][j]</C>.
##	<Example>
##  gap> gens:=[ Transformation( [ 5, 1, 1, 5, 1 ] ), 
##  Transformation( [ 5, 2, 4, 3, 2 ] ) ];;
##  gap> S:=Semigroup(gens);;
##  gap> GradedRClasses(S);
##  [ [ GreensRClassData( Transformation( [ 5, 1, 1, 5, 1 ] ), [ [ 1, 5 ] ], 
##          [ () ], Group( [ (), (1,5) ] ) ), GreensRClassData( Transformation( 
##          [ 1, 1, 5, 1, 1 ] ), [ [ 1, 5 ] ], [ () ], Group( [ (), (1,5) ] ) ), 
##        GreensRClassData( Transformation( [ 1, 1, 1, 5, 1 ] ), [ [ 1, 5 ] ], 
##          [ () ], Group( [ (), (1,5) ] ) ) ], 
##    [ GreensRClassData( Transformation( [ 5, 2, 4, 3, 2 ] ), [ [ 2, 3, 4, 5 ] 
##           ], [ () ], Group( [ () ] ) ) ], 
##    [ GreensRClassData( Transformation( [ 2, 5, 5, 2, 5 ] ), [ [ 2, 5 ] ], 
##          [ () ], Group( [ () ] ) ), GreensRClassData( Transformation( 
##          [ 5, 2, 2, 5, 2 ] ), [ [ 2, 5 ] ], [ () ], Group( [ () ] ) ), 
##        GreensRClassData( Transformation( [ 5, 5, 2, 5, 5 ] ), [ [ 2, 5 ] ], 
##          [ () ], Group( [ () ] ) ), GreensRClassData( Transformation( 
##          [ 2, 2, 5, 2, 2 ] ), [ [ 2, 5 ] ], [ () ], Group( [ () ] ) ), 
##        GreensRClassData( Transformation( [ 5, 5, 5, 2, 5 ] ), [ [ 2, 5 ] ], 
##          [ () ], Group( [ () ] ) ), GreensRClassData( Transformation( 
##          [ 2, 2, 2, 5, 2 ] ), [ [ 2, 5 ] ], [ () ], Group( [ () ] ) ) ], 
##    [ GreensRClassData( Transformation( [ 2, 2, 3, 4, 2 ] ), [ [ 2, 3, 4 ] ], 
##          [ () ], Group( [ (), (3,4) ] ) ) ], 
##    [ GreensRClassData( Transformation( [ 1, 1, 1, 1, 1 ] ), 
##          [ [ 1 ], [ 5 ], [ 2 ] ], [ (), (1,2,3,4,5), (1,2) ], Group( 
##          [ () ] ) ) ] ]
##	</Example> <!-- greens.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("GradedRClasses", IsTransformationSemigroup);

#############################################################################
##
##	PositionsRClasses is also an internally required attribute.
## 

DeclareAttribute("PositionsRClasses", IsTransformationSemigroup);

#############################################################################
##
##	<#GAPDoc Label="GreensLClassData">
##	<ManSection>
##	<Attr Name="GreensLClassData" Arg="S, f"/>
##	<Description>
##	if <C>C</C> satisfies <Ref Attr="IsGreensLClass" BookName="ref"/>, then 
##	<C>GreensLClassData</C> returns an object in the category 
##	<Ref Filt="IsGreensLClassData"/> with representation 
##	<Ref Filt="IsGreensLClassDataRep"/> and the following five components:
##	<List>
##		<Item><C>rep</C> the representative of the <M>L</M>-class.</Item>
##		<Item><C>strongorb</C> the strong orbit of the kernel of <C>rep</C> under 
##		the action of the semigroup by <Ref Func="OnTuplesOfSetsAntiAction"/>.
##		</Item>
##		<Item><C>relts</C> a list of relations such that 
##		<Verb>KernelOfTransformation(relts[i]*x)=strongorb[1]</Verb> whenever 
##		<C>x</C> 
##		has <Verb>KernelOfTransformation(x)=strongorb[i].</Verb></Item>
##		<Item><C>invrelts</C> the inverses of the relations <C>relts</C>, so that 
##		<Verb>KernelOfTransformation(invrelts[i]*rep)=strongorb[i].</Verb></Item>
##		<Item><C>schutz</C> the (generalised) left Schutzenberger group.</Item>
##	</List>
##	Further 
##	details can be found in Algorithm G, H, I, and J of <Cite Key="pfeiffer2"/>.
##	<Example>
##  gap> gens:=[ Transformation( [ 4, 1, 4, 5, 3 ] ),
##  > Transformation( [ 5, 3, 5, 4, 3 ] ) ];;
##  gap> S:=Semigroup(gens);;
##  gap> C:=GreensLClassOfElement(S, gens[1]*gens[2]*gens[1]);
##  {Transformation( [ 5, 3, 5, 4, 3 ] )}
##  gap> GreensLClassData(C);
##  GreensLClassData( Transformation( [ 5, 3, 5, 4, 3 ] ), 
##  [ [ [ 1, 3 ], [ 2, 5 ], [ 4 ] ] ], [ Binary Relation on 5 points ], 
##  [ Binary Relation on 5 points ], Group( [ (), (3,5,4), (3,5) ] ) )
##	</Example> <!-- greens.tst --> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("GreensLClassData", IsGreensLClass and IsGreensClassOfTransSemigp);

#############################################################################
##
##	<#GAPDoc Label="GreensHClassData">
##	<ManSection>
##	<Attr Name="GreensHClassData" Arg="S, f"/>
##	<Description>
##	if <C>C</C> satisfies <Ref Attr="IsGreensHClass" BookName="ref"/>, then 
##	<C>GreensLClassData</C> returns an object in the category 
##	<Ref Filt="IsGreensHClassData"/> with representation 
##	<Ref Filt="IsGreensHClassDataRep"/> and the following two components:
##	<List>
##		<Item><C>rep</C> the representative of the <M>H</M>-class.</Item>
##		<Item><C>schutz</C> the intersection of the left Schutzenberger group and 
##		right Schutzenberger group of the <M>L</M>-class and <M>R</M>-class 
##		containing the representative <C>rep</C> (that is, the intersection of the 
##		<C>schutz</C> component of <Ref Func="GreensRClassData"/> and the 
##		<C>schutz</C> component of <Ref Func="GreensLClassData"/>).</Item>
##	</List>
##	Further details can be found in Algorithm K, L, M, and N of 
##	<Cite Key="pfeiffer2"/>.
##	<Example>
##  gap> gens:=[ Transformation( [ 2, 2, 5, 2, 3 ] ), 
##  > Transformation( [ 2, 5, 3, 5, 3 ] ) ];;
##  gap> S:=Semigroup(gens);;
##  gap> f:=Transformation( [ 5, 5, 3, 5, 3 ] );;
##  gap> GreensHClassData(GreensHClassOfElement(S, f));
##  GreensHClassData( Transformation( [ 5, 5, 3, 5, 3 ] ), Group( () ) )
##	</Example> <!-- greens.tst --> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("GreensHClassData", IsGreensHClass and IsGreensClassOfTransSemigp);

#############################################################################
##
##	<#GAPDoc Label="GreensDClassData">
##	<ManSection>
##	<Attr Name="GreensDClassData" Arg="S, f"/>
##	<Description>
##	if <C>C</C> satisfies <Ref Attr="IsGreensDClass" BookName="ref"/>, then 
##	<C>GreensDClassData</C> returns an object in the category 
##	<Ref Filt="IsGreensDClassData"/> with representation 
##	<Ref Filt="IsGreensDClassDataRep"/> and the following five components:
##	<List>
##		<Item><C>rep</C> the representative of the <M>D</M>-class.</Item>
##		<Item><C>R</C> the result of <Ref Func="GreensRClassData"/> with argument 
##		<C>rep</C>.</Item>
##		<Item><C>L</C> the result of <Ref Func="GreensLClassData"/> with argument 
##		<C>rep</C>.</Item>
##		<Item><C>H</C> the result of <Ref Func="GreensHClassData"/> with argument 
##		<C>rep</C>.</Item>
##		<Item> <C>cosets</C> a transversal of right cosets of the 
##		Schutzenberger group of <C>H</C> in the Schutzenberger group of 
##		<C>R</C>.</Item>
##	</List>
##	Note that only the first three components are displayed. 
##	Further details can be found in Algorithm O, P, Q, R, S, and T of 
##	<Cite Key="pfeiffer2"/>.
##	<Example>
##  gap> gens:=[ Transformation( [ 4, 1, 5, 2, 4 ] ), 
##  > Transformation( [ 4, 4, 1, 5, 3 ] ) ];;
##  gap> S:=Semigroup(gens);;
##  gap> f:=Transformation( [ 5, 5, 3, 3, 3 ] );;
##  gap> GreensDClassData(GreensDClassOfElement(S, f));
##  GreensDClassData( Transformation( [ 5, 5, 3, 3, 3 
##  ] ), GreensRClassData( Transformation( [ 5, 5, 3, 3, 3 
##  ] ) ), GreensLClassData( Transformation( [ 5, 5, 3, 3, 3 ] ) ) )
##	</Example> <!-- greens.tst --> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("GreensDClassData", IsGreensDClass and IsGreensClassOfTransSemigp);

#############################################################################
##
##	<#GAPDoc Label="SchutzenbergerGroup">
##	<ManSection>
##	<Attr Name="SchutzenbergerGroup" Arg="D"/>
##	<Description>
##	if <C>D</C> satisfies <Ref Attr="IsGreensRClassData"/>, 
##	<Ref Attr="IsGreensLClassData"/>, <Ref Attr="IsGreensHClassData"/>, or 
##	<Ref Attr="IsGreensDClassData"/>, then <C>SchutzenbergerGroup</C> returns the 
##	<C>schutz</C> component of <C>D</C>. <P/>
##	
##	If <C>D</C> satisfies <Ref Attr="IsGreensRClass" BookName="ref"/>, 
##	<Ref Attr="IsGreensLClass" BookName="ref"/>, 
##	<Ref Attr="IsGreensHClass" BookName="ref"/>, 
##	<Ref Attr="IsGreensDClass" BookName="ref"/>, then <C>SchutzenbergerGroup</C> 
##	returns the <C>schutz</C> component of <C>GreensData</C> with argument
##	<C>D</C>.
##	<Example>
##  gap> gens:=[ Transformation( [ 4, 4, 3, 5, 3 ] ), 
##  > Transformation( [ 5, 1, 1, 4, 1 ] ), 
##  > Transformation( [ 5, 5, 4, 4, 5 ] ) ];;
##  gap> f:=Transformation( [ 4, 5, 5, 5, 5 ] );;
##  gap> SchutzenbergerGroup(GreensDClassOfElement(S, f));
##  Group([ (), (4,5) ])
##  gap> SchutzenbergerGroup(GreensRClassOfElement(S, f));
##  Group([ (), (4,5) ])
##  gap> SchutzenbergerGroup(GreensLClassOfElement(S, f));
##  Group([ (), (4,5) ])
##  gap> SchutzenbergerGroup(GreensHClassOfElement(S, f));
##  Group([ (), (4,5) ])
##	</Example> <!-- greens.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

# JDM change this to 
# DeclareAttribute("SchutzenbergerGroup", IsGreensClass);
# when the time is right!

DeclareAttribute("SchutzenbergerGroup", IsGreensData);

#############################################################################
##
##	<#GAPDoc Label="PartialOrderOfDClasses">
##	<ManSection>
##	<Attr Name="PartialOrderOfDClasses" Arg="S"/>
##	<Description>
##	returns the partial order of the <C>D</C>-classes of <C>S</C> as a directed 
##	graph in <Package>GRAPE</Package>, if it is installed, using the command 
##	<Log>
##	Graph(Group(()), [1..Length(GreensDClasses(S))], OnPoints, function(x,y)
##	return y in poset[x]; end, true); ;
##	</Log>
##	where <C>y</C> in <C>poset[x]</C> if and only if 
##	<C>S^1yS^1</C> is a subset of  <C>S^1 xS^1</C>. <P/>
##	
##	If <Package>GRAPE</Package> is not loaded, 
##	then the list <C>poset</C> is returned instead.
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("PartialOrderOfDClasses", IsSemigroup, "mutable");

###########################################################################
##
##	<#GAPDoc Label="Idempotents">
##	<ManSection>
##	<Func Name="Idempotents" Arg="X[,n]"/>
##	<Description>
##	returns a list of the idempotents in the transformation semigroup or Green's 
##	class <C>X</C>. <P/>
##
##	If the optional second argument <C>n</C> is present, then a list of the 
##	idempotents in <C>S</C> of rank <C>n</C> is returned. If you are only 
##	interested in the 
##	idempotents of a given rank, then the second version of the function will 
##	likely be faster.
##	<Example>
##  gap> S:=Semigroup([ Transformation( [ 2, 3, 4, 1 ] ), 
##  > Transformation( [ 3, 3, 1, 1 ] ) ]);;
##  gap> Idempotents(S, 1);
##  [  ]
##  gap> Idempotents(S, 2);                        
##  [ Transformation( [ 1, 1, 3, 3 ] ), Transformation( [ 1, 3, 3, 1 ] ), 
##    Transformation( [ 2, 2, 4, 4 ] ), Transformation( [ 4, 2, 2, 4 ] ) ]
##  gap> Idempotents(S, 3);                        
##  [  ]
##  gap> Idempotents(S, 4);                        
##  [ Transformation( [ 1, 2, 3, 4 ] ) ]
##  gap> Idempotents(S);
##  [ Transformation( [ 1, 1, 3, 3 ] ), Transformation( [ 1, 2, 3, 4 ] ), 
##    Transformation( [ 1, 3, 3, 1 ] ), Transformation( [ 2, 2, 4, 4 ] ), 
##    Transformation( [ 4, 2, 2, 4 ] ) ]
##	</Example> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

#############################################################################
#############################################################################
# new for 3.2!

DeclareInfoClass("InfoMonoidGreens");

# in OrbitsOfImages:
# would it be better to have this stored in the (empty) record used to 
# create <s> in SemigroupByGenerators, to avoid method selection? Since
# this is called so so many times it might be worth it... JDM for MN

DeclareOperation("IsSubsemigroup", [IsTransformationSemigroup, 
 IsTransformationSemigroup]);

DeclareAttribute("OrbitsOfImages", IsTransformationSemigroup, "mutable");
DeclareAttribute("OrbitsOfKernels", IsTransformationSemigroup, "mutable");

DeclareGlobalFunction("ForwardOrbitOfImage");
DeclareGlobalFunction("ForwardOrbitOfKernel");

DeclareGlobalFunction("ForwardOrbitOfImageNC");
DeclareGlobalFunction("ForwardOrbitOfKernelNC");

DeclareGlobalFunction("DisplayOrbitsOfImages");
DeclareGlobalFunction("DisplayOrbitsOfKernels");

DeclareGlobalFunction("ExpandOrbitsOfImages");
DeclareGlobalFunction("ExpandOrbitsOfKernels");

DeclareGlobalFunction("SizeOrbitsOfImages");
DeclareGlobalFunction("SizeOrbitsOfKernels");

DeclareGlobalFunction("NrRClassesOrbitsOfImages");
DeclareGlobalFunction("NrLClassesOrbitsOfKernels");

DeclareGlobalFunction("InOrbitsOfImages");
DeclareGlobalFunction("InOrbitsOfKernels");

DeclareGlobalFunction("CreateSchreierTreeOfSCC");
DeclareGlobalFunction("CreateReverseSchreierTreeOfSCC");
DeclareGlobalFunction("TraceSchreierTreeOfSCCForward");
DeclareGlobalFunction("TraceSchreierTreeOfSCCBack");
DeclareGlobalFunction("MultipliersOfSCCOfImageOrbit");
DeclareGlobalFunction("MultipliersOfSCCOfKernelOrbit");
DeclareGlobalFunction("SchutzenbergerGroupOfSCCOfImageOrbit");
DeclareGlobalFunction("SchutzenbergerGroupOfSCCOfKernelOrbit");

DeclareGlobalFunction("RClassRepFromData");
DeclareGlobalFunction("LClassRepFromData");
DeclareGlobalFunction("RClassImageOrbitFromData");
DeclareGlobalFunction("RClassSchutzGpFromData");
DeclareGlobalFunction("RClassPermsFromData");
DeclareGlobalFunction("RClassSCCFromData");
DeclareOperation("GreensRClassOfElementNC", [IsTransformationSemigroup]);

DeclareGlobalFunction("LClassKernelOrbitFromData");
DeclareGlobalFunction("LClassSchutzGpFromData");
DeclareGlobalFunction("LClassRelsFromData");
DeclareGlobalFunction("LClassSCCFromData");
DeclareOperation("GreensLClassOfElementNC", [IsTransformationSemigroup]);

DeclareProperty("IsEnumeratorOfRClassElements", IsEnumeratorByFunctions);
DeclareGlobalFunction("IteratorOfRClassReps");
DeclareGlobalFunction("IteratorOfLClassReps");

DeclareProperty("IsIteratorOfRClassReps", IsIterator);
DeclareProperty("IsIteratorOfLClassReps", IsIterator);

DeclareGlobalFunction("IteratorOfGreensRClasses");
DeclareProperty("IsIteratorOfGreensRClasses", IsIterator);
DeclareProperty("IsIteratorOfSemigroup", IsIterator);
DeclareProperty("IsIteratorOfRClassElements", IsIterator);


DeclareAttribute("SemigroupOfIteratorOfRClassReps", IsIteratorOfRClassReps);
DeclareAttribute("SemigroupOfIteratorOfLClassReps", IsIteratorOfLClassReps);
DeclareAttribute("SemigroupOfIteratorOfGreensRClasses", 
IsIteratorOfGreensRClasses);
DeclareAttribute("NrGreensRClasses", IsTransformationSemigroup);
DeclareAttribute("NrGreensLClasses", IsTransformationSemigroup);

DeclareAttribute("GreensLClassReps", IsTransformationSemigroup);

# probably get rid of the following...

DeclareGlobalFunction("GreensRClassRepsNC");
DeclareGlobalFunction("IsInSCCOfOrbitNC");
DeclareGlobalFunction("IsInSCCOfOrbit");
DeclareGlobalFunction("IsInOrbit");
DeclareProperty("RClassRepsData", IsTransformationSemigroup);
