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
##	obtained using the function StrongOrbitOfImage. Further 
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

DeclareAttribute("GreensDClassData", IsGreensDClass and 
IsGreensClassOfTransSemigp);
