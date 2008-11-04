##
## autos.gd
## Version 3.1.2
## Fri 11 Jul 2008 13:36:12 BST
##

##  <#GAPDoc Label="autostop">
##  This section contains functions for computing automorphisms of 
##	transformation semigroups. 
##	<#/GAPDoc>
##  
##

###########################################################################
##
##  <#GAPDoc Label="InfoAutos">
##  <ManSection> 
##  <InfoClass Name="InfoAutos"/>
##  <Description>
##	This is the InfoClass for the functions in this chapter. Setting the value 
##	of <C>InfoAutos</C> to <C>1, 2, 3,</C> or <C>4</C> using the command 
##	<Ref Oper="SetInfoLevel" BookName="ref"/> will give different levels of 
##	information about what 
##	<C>GAP</C> is doing during a computation. 
##	In particular, if the level
##	of <C>InfoAutos</C> is set to <C>4</C>, then <Ref Attr="AutomorphismGroup"/> 
##	runs in interactive mode. 
##  </Description>
##  </ManSection>
##	<#/GAPDoc>

DeclareInfoClass("InfoAutos");

###########################################################################
##
##  <#GAPDoc Label="IsAutomorphismGroupOfSemigroup">
##  <ManSection> 
##  <Prop Name="IsAutomorphismGroupOfSemigroup" Arg="G"/>
##	<Description>
##	returns <C>true</C> if <C>G</C> is the automorphism group of a semigroup. 
##	Note that 
##	this property is set to <C>true</C> when the computation of the automorphism
##	group is performed. Otherwise, there is no method to check if an arbitrary
##	group satisfies this property; see 
##	<Ref Attr="AutomorphismGroup"/> for an example of the usage of this 
##	command.<P/>
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

DeclareProperty("IsAutomorphismGroupOfSemigroup", IsAutomorphismGroup);

###########################################################################
##
##  <#GAPDoc Label="IsInnerAutomorphismsOfSemigroup">
##  <ManSection> 
##  <Prop Name="IsInnerAutomorphismsOfSemigroup" Arg="G"/>
##	<Description>
##	returns <C>true</C> if <C>G</C> is the inner automorphism group of a 
##	transformation 
##	semigroup. <P/>
##
##	The notion of inner automorphisms of semigroups differs from the 
##	notion of the same name for groups. Indeed, if <C>S</C> is a semigroup of 
##	transformations of degree <C>n</C>, then <C>g</C> in the symmetric group  
##	<C>S_n</C> induces an 
##	inner automorphism of <C>S</C> if the mapping that takes <C>s</C> to 
##	<C>g^-1sg</C> for all <C>s</C> in <C>S</C> is an automorphism of 
##	<C>S</C>.<P/>
##
##	Note that 
##	this property is set to <C>true</C> when the computation of the inner 
##	automorphisms is performed. Otherwise, there is no method to check if an 
##	arbitrary group satisfies this property.
##	<Example>
##  gap&gt; S:=RandomSemigroup(5,5);
##  &lt;semigroup with 5 generators&gt;
##  gap&gt; I:=InnerAutomorphismsOfSemigroup(S);;
##  gap&gt; IsInnerAutomorphismsOfSemigroup(I);
##  true
##	</Example> <!-- autos3.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

DeclareProperty("IsInnerAutomorphismsOfSemigroup", IsGroup);

###########################################################################
##
##  <#GAPDoc Label="IsInnerAutomorphismsOfZeroGroup">
##  <ManSection>
##  <Prop Name="IsInnerAutomorphismsOfZeroGroup" Arg="G"/>
##	<Description>
##	returns <C>true</C> if <C>G</C> is the inner automorphism group of a zero 
##	group. 
##	This property is set to <C>true</C> when the computation of the inner 
##	automorphism group of the zero group is performed. Otherwise, there is no 
##	method to check if an arbitrary group satisfies this property. <P/>
##
##	Every inner automorphism of a zero group is just an inner automorphism of 
##	the underlying group that fixes the zero element. So, this notion of inner
##	automorphism corresponds to the notion of inner automorphisms of a group. 
##	<Example>
##  gap&gt; zg:=ZeroGroup(CyclicGroup(70));
##  &lt;zero group with 4 generators&gt;
##  gap&gt; I:=InnerAutomorphismsAutomorphismGroup(AutomorphismGroup(zg));
##  &lt;group of size 1 with 1 generators&gt;
##  gap&gt; IsInnerAutomorphismsOfZeroGroup(I);
##  true
##	</Example>
##	</Description> <!-- autos3.tst -->
##	</ManSection>
##	<#/GAPDoc>

DeclareProperty("IsInnerAutomorphismsOfZeroGroup", IsInnerAutomorphismsOfSemigroup);

###########################################################################
##
##  <#GAPDoc Label="InnerAutomorphismsOfSemigroupInGroup">
##  <ManSection>
##  <Oper Name="InnerAutomorphismsOfSemigroupInGroup" Arg="S, G[, bval]"/>
##	<Description>
##	<C>InnerAutomorphismsOfSemigroupInGroup</C> returns the group of 
##	inner automorphisms of the transformation semigroup <C>S</C> that also 
##	belong to the group <C>G</C>. The default setting is that the inner 
##	automorphisms of <C>S</C> are calculated first, then filtered to see which 
##	elements also belong to <C>G</C>.<P/>
##
##	If the optional argument <C>bval</C> is present and <C>true</C>, then the 
##	filtering is done as the inner automorphisms are found rather than after 
##	they have all been found. Otherwise, then this is equivalent to doing 
##	<C>InnerAutomorphismsOfSemigroupInGroup(S, G)</C>. <P/>
##
##	If <Ref InfoClass="InfoAutos"/> is set to level <C>4</C>, then a prompt will 
##	appear during the procedure to let you decide when the filtering should be 
##	done. In this case the value of <C>bval</C> is irrelevant.<P/>
##	<Example>
##  gap&gt; gens:=[ Transformation( [1,8,11,2,5,16,13,14,3,6,15,10,7,4,9,12 ] ), 
##  &gt;   Transformation( [1,16,9,6,5,8,13,12,15,2,3,4,7,10,11,14] ), 
##  &gt;   Transformation( [1,3,7,9,1,15,5,11,13,11,13,3,5,15,7,9 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; InnerAutomorphismsOfSemigroup(S);
##  &lt;group of size 16 with 3 generators&gt;
##  gap&gt; G:=Group(SemigroupHomomorphismByImagesOfGensNC(S, S, gens));
##  &lt;group with 1 generators&gt;
##  gap&gt; InnerAutomorphismsOfSemigroupInGroup(S, G);
##  &lt;group of size 1 with 1 generators&gt;
##  gap&gt; InnerAutomorphismsOfSemigroupInGroup(S, G, true);
##  &lt;group of size 1 with 1 generators&gt;
##  gap&gt; InnerAutomorphismsOfSemigroupInGroup(S, G, false);
##  &lt;group of size 1 with 1 generators&gt;
##	</Example> <!-- autos1.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("InnerAutomorphismsOfSemigroupInGroup", [IsTransformationSemigroup, IsObject, IsBool]);

###########################################################################
##
##  <#GAPDoc Label="InnerAutomorphismsOfSemigroup">
##  <ManSection>
##  <Attr Name="InnerAutomorphismsOfSemigroup" Arg="S"/>
##	<Description>
##
##	<C>InnerAutomorphismsOfSemigroup</C> returns the group of 
##	inner automorphisms of the transformation semigroup <C>S</C>. <P/>
##	
##	The same result can be obtained 
##	by applying <Ref Attr="InnerAutomorphismsAutomorphismGroup"/> to the result 
##	of <Ref Attr="AutomorphismGroup"/> of <C>S</C>. 
##
##	It is possible that the inner 
##	automorphism of <C>S</C> have been calculated at the same time as the entire 
##	automorphism group of <C>S</C> but it might not be.  
##	If the degree of <C>S</C>
##	is high, then this function may take a long time to return a value. <P/>
##
##	The notion of inner automorphisms of semigroups differs from the 
##	notion of the same name for groups. Indeed, if <C>S</C> is a semigroup of 
##	transformations of degree <C>n</C>, then <C>g</C> in the symmetric group 
##	<C>S_n</C> induces an inner 
##	automorphism of <C>S</C> if the mapping that takes <C>s</C> to <C>g^-1sg</C> 
##	for all 
##	<C>s</C> in <C>S</C> is an automorphism of <C>S</C>.
##	<Example>
##  gap&gt; x:=Transformation([2,3,4,5,6,7,8,9,1]);;
##  gap&gt; y:=Transformation([4,2,3,4,5,6,7,8,9]);;
##  gap&gt; S:=Semigroup(x,y);;
##  gap&gt; G:=InnerAutomorphismsOfSemigroup(S);
##  &lt;group of size 54 with 2 generators&gt;
##	</Example> <!-- autos1.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

DeclareAttribute("InnerAutomorphismsOfSemigroup", IsTransformationSemigroup);

###########################################################################
##
##  <#GAPDoc Label="InnerAutomorphismsAutomorphismGroup">
##  <ManSection> 
##  <Attr Name="InnerAutomorphismsAutomorphismGroup" Arg="autgroup"/>
##	<Description>
##
##	If <C>autgroup</C> satisfies <Ref Prop="IsAutomorphismGroupOfSemigroup"/> 
##	then, this attribute 
##	stores the subgroup of inner automorphisms of the original semigroup. <P/>
##
##	It is possible that the inner 
##	automorphisms of <C>autgroup</C> have been calculated at the same time as 
##	<C>autgroup</C> was calculated but they might not be.  
##	If the degree of underlying semigroup 
##	is high, then this function may take a long time to return a value. <P/>
##
##	The notion of inner automorphisms of semigroups differs from the 
##	notion of the same name for groups. Indeed, if <C>S</C> is a semigroup of 
##	transformations of degree <C>n</C>, then <C>g</C> in the symmetric group 
##	<C>S_n</C> induces an 
##	inner automorphism of <C>S</C> if the mapping that takes <C>s</C> to 
##	<C>g^-1sg</C> for all <C>s</C> in <C>S</C> is an automorphism of 
##	<C>S</C>.<P/>
##
##	If <C>autgroup</C> satisfies <Ref Prop="IsAutomorphismGroupOfZeroGroup"/>, 
##	then <C>InnerAutomorphismsAutomorphismGroup</C> returns the subgroup of 
##	inner automorphisms inside the automorphism 
##	group of the zero group by computing the inner automorphisms of the 
##	underlying group. Note that in this case the notion of inner automorphisms 
##	corresponds to that of the group theoretic notion.
##	<Example>
##  gap&gt; g1:=Transformation([3,3,2,6,2,4,4,6]);;
##  gap&gt; g2:=Transformation([5,1,7,8,7,5,8,1]);;
##  gap&gt; m6:=Semigroup(g1,g2);;
##  gap&gt; A:=AutomorphismGroup(m6);
##  &lt;group of size 12 with 2 generators&gt;
##  gap&gt; InnerAutomorphismsAutomorphismGroup(A);
##  &lt;group of size 12 with 2 generators&gt; 
##  gap&gt; last=InnerAutomorphismsOfSemigroup(m6); 
##	</Example>	<!-- autos2.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

###########################################################################
##
##  <#GAPDoc Label="AutomorphismsSemigroupInGroup">
##  <ManSection>
##  <Oper Name="AutomorphismsSemigroupInGroup" Arg="S, G[, bvals]"/>
##	<Description>
##	<C>AutomorphismsSemigroupInGroup</C> returns the group of 
##	automorphisms of the transformation semigroup <C>S</C> that also 
##	belong to the group <C>G</C>. If the value of <C>G</C> is <C>fail</C>, then 
##	<C>AutomorphismsSemigroupInGroup</C> returns the same value as
##	<Ref Attr="AutomorphismGroup"/>. The default setting is that the  
##	automorphisms of <C>S</C> are calculated first, then filtered to see which 
##	elements also belong to <C>G</C>.<P/>
##
##	The optional argument  <C>bvals</C> is a list of 
##	<C>5</C> Boolean variables that correspond to the following options:
##	<List>
##		<Item>if <C>bvals[1]</C> is <C>true</C>, then &GAP; will run a cheap check 
##		to see if all the automorphisms are inner. Note that this can return 
##		<C>false</C> when all the automorphisms are inner, that is the condition 
##		is sufficient but not necessary. The default setting is <C>false</C>.
##		</Item>
##		<Item>if <C>bvals[2]</C> is <C>true</C>, then &GAP; will try to compute 
##		the inner automorphisms of <C>S</C> before computing the entire
##		automorphism group. For semigroups of large degree this may not be 
##		sensible. The default setting is <C>false</C>.</Item>
##		<Item>if <C>bvals[3]</C> is <C>true</C>, then &GAP; will test elements in
##		the inner automorphism search space to see if they are in <C>G</C> as the 
##		inner automorphisms are found rather than after they have all been found.  
##		The default setting is <C>false</C>.</Item>
##		<Item>if <C>bvals[4]</C> is <C>true</C>, then &GAP; will test elements in
##		the outer (i.e. not inner) automorphism search space to see if they are in 
##		<C>G</C> as they are found rather than after they have all been found.  
##		The default setting is <C>false</C>.</Item>
##		<Item>if <C>bvals[5]</C> is <C>true</C>, then &GAP; will keep track of 
##		non-automorphisms in the search for outer automorphisms.  The default 
##		setting is <C>false</C>.</Item>
##	</List>
##
##	<B>Please note:</B> if <Package>grape</Package> is not loaded, then this 
##	function will not work when <C>S</C> satisfies 
##	<Ref Prop="IsTransformationSemigroup" BookName="ref"/> or 
##	<Ref Prop="IsReesZeroMatrixSemigroup" BookName="ref"/>.
##	<Example>
##  gap&gt; g1:=Transformation([5,4,4,2,1]);;
##  gap&gt; g2:=Transformation([2,5,5,4,1]);;
##  gap&gt; m2:=Monoid(g1,g2);;
##  gap&gt; A:=AutomorphismsSemigroupInGroup(m2, fail, 
##  &gt; [false, true, true, false, true]);
##  &lt;group of size 24 with 3 generators&gt;
##  gap&gt; g1:=Transformation([3,3,2,6,2,4,4,6,3,4,6]);;
##  gap&gt; g2:=Transformation([4,4,6,1,3,3,3,3,11,11,11]);;
##  gap&gt; m7:=Monoid(g1,g2);;
##  gap&gt; A:=AutomorphismsSemigroupInGroup(m7, fail, 
##  &gt; [false, true, false, false, true]);
##  &lt;group of size 2 with 2 generators&gt;
##  gap&gt; imgs:=[ [ Transformation( [ 1, 1, 5, 4, 3, 6, 7, 8, 9, 10, 11, 12 ] ), 
##  &gt;       Transformation( [ 1, 1, 5, 7, 4, 3, 6, 8, 9, 10, 11, 12 ] ), 
##  &gt;       Transformation( [ 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 8 ] ) ], 
##  &gt;   [ Transformation( [ 1, 1, 5, 4, 3, 6, 7, 8, 9, 10, 11, 12 ] ), 
##  &gt;       Transformation( [ 1, 1, 5, 3, 7, 4, 6, 8, 9, 10, 11, 12 ] ), 
##  &gt;       Transformation( [ 1, 2, 3, 4, 5, 6, 7, 11, 12, 8, 9, 10 ] ) ] ];;
##  gap&gt; gens:=List(imgs, x-&gt; SemigroupHomomorphismByImagesOfGensNC(S, S, x));;
##  gap&gt; G:=Group(gens);
##  &lt;group with 2 generators&gt;
##  gap&gt; A:=AutomorphismsSemigroupInGroup(S, G, 
##  &gt; [false, false, false, true, false]);
##  &lt;group of size 48 with 4 generators&gt;
##  gap&gt; Size(G);
##  48
##  gap&gt; A:=AutomorphismsSemigroupInGroup(S, G);
##  &lt;group of size 48 with 4 generators&gt;
##  gap&gt; gens:=[ Transformation( [ 1, 1, 4, 3, 5, 6, 7, 8, 9, 10, 11, 12 ] ), 
##  &gt;   Transformation( [ 1, 1, 4, 5, 6, 7, 3, 8, 9, 10, 11, 12 ] ), 
##  &gt;   Transformation( [ 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 8 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; A:=AutomorphismsSemigroupInGroup(S, G);
##  &lt;group of size 48 with 4 generators&gt;
##  gap&gt; HasAutomorphismGroup(S);
##  true
##  gap&gt; AutomorphismGroup(S);
##  &lt;group of size 480 with 7 generators&gt;
##	</Example> <!-- autos2.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

DeclareOperation("AutomorphismsSemigroupInGroup", [IsSemigroup, IsObject, IsList]);

###########################################################################
##
##  <#GAPDoc Label="AutomorphismGroup">
##  <ManSection>
##  <Attr Name="AutomorphismGroup" Arg="S"/>
##	<Description>
##	<C>AutomorphismGroup</C> returns the group of 
##	automorphisms of the transformation semigroup, zero group, zero semigroup, 
##	Rees matrix semigroup, or Rees 0-matrix semigroup <C>S</C>; that is, 
##	semigroups satisfying the properties <Ref Prop="IsTransformationSemigroup" 
##	BookName="ref"/>, <Ref Prop="IsZeroGroup"/>, <Ref Prop="IsZeroSemigroup"/>,  
##	<Ref Prop="IsReesMatrixSemigroup" BookName="ref"/>, or 
##	<Ref Prop="IsReesZeroMatrixSemigroup" BookName="ref"/>. <P/>
##
##	If <C>S</C> is a transformation semigroup, then <C>AutomorphismGroup</C> 
##	computes the automorphism group of <C>S</C> using the algorithm 
##	described in <Cite Key="computing"/>.  <P/>
##
##	If <C>S</C> is a (completely) simple 
##	transformation semigroup, then the automorphism group is computed by passing 
##	to an isomorphic Rees matrix semigroup. 
##
##	If <C>S</C> is a
##	transformation group, then the automorphism group is computed by passing to 
##	an isomorphic permutation group. 
##
##	If <C>S</C> has order 
##	<C>&lt;10</C>  and knows its Cayley table 
##	(<Ref Prop="MultiplicationTable" BookName="ref"/>), then the automorphism 
##	group is calculated by finding the setwise stabilizer of the Cayley table in
##	the symmetric group of degree <C>|S|</C> under the action on the Cayley 
##	table. <P/>
##
##	If <C>S</C> is a zero group, then <C>AutomorphismGroup</C> computes the 
##	automorphism group of the underlying group. Obviously, every automorphism of 
##	a zero group is the extension of an automorphism of the underlying group 
##	that fixes the zero element. <P/>
##	
##	If <C>S</C> is a zero semigroup, then every permutation of the elements of 
##	<C>S</C> that fixes the zero element is an 
##	automorphism. Thus the automorphism group of a zero semigroup of order 
##	<C>n</C> is isomorphic to the symmetric group on <C>n-1</C> elements. <P/>
##
##	If <C>S</C> is a Rees matrix semigroup or a Rees 0-matrix semigroup, then 
##	the automorphism group of <C>S</C> is calculated using the algorithm 
##	described in <Cite Key="computing" Where="Section 2"/>. In this case, the 
##	returned group has as many generators as elements. This may be changed in 
##	the future.<P/>
##
##	If <Ref InfoClass="InfoAutos"/> is set to level <C>4</C>, then prompts will 
##	appear during the procedure to allow you interactive control over the 
##	computation. 
##	<P/>
##
##	<B>Please note:</B> if  <Package>grape</Package> is not loaded, then this 
##	function will not work when <C>S</C> satisfies 
##	<Ref Prop="IsTransformationSemigroup" BookName="ref"/> or 
##	<Ref Prop="IsReesZeroMatrixSemigroup" BookName="ref"/>.
##	<Example>
##  gap&gt; g1:=Transformation([5,4,4,2,1]);;
##  gap&gt; g2:=Transformation([2,5,5,4,1]);;
##  gap&gt; m2:=Monoid(g1,g2);;
##  gap&gt; IsTransformationSemigroup(m2);
##  true
##  gap&gt; AutomorphismGroup(m2);
##  &lt;group of size 24 with 5 generators&gt;
##  gap&gt; IsAutomorphismGroupOfSemigroup(last);
##  true
##  gap&gt; zg:=ZeroGroup(CyclicGroup(70));
##  &lt;zero group with 4 generators&gt;
##  gap&gt; IsZeroGroup(zg);
##  true
##  gap&gt; AutomorphismGroup(zg);
##  &lt;group with 3 generators&gt;
##  gap&gt; IsAutomorphismGroupOfZeroGroup(last);
##  true
##  gap&gt; InnerAutomorphismsOfSemigroup(zg);
##  &lt;group of size 1 with 1 generators&gt;
##  gap&gt; InnerAutomorphismsAutomorphismGroup(AutomorphismGroup(zg));
##  &lt;group of size 1 with 1 generators&gt;
##  gap&gt; last2=InnerAutomorphismsAutomorphismGroup(AutomorphismGroup(zg));
##  true
##  gap&gt; S:=ZeroSemigroup(10);
##  &lt;zero semigroup with 10 elements&gt;
##  gap&gt; Size(S);
##  10
##  gap&gt; Elements(S);
##  [ 0, z1, z2, z3, z4, z5, z6, z7, z8, z9 ]
##  gap&gt; A:=AutomorphismGroup(S);
##  &lt;group with 2 generators&gt;
##  gap&gt; IsAutomorphismGroupOfZeroSemigroup(A);
##  true
##  gap&gt; Factorial(9)=Size(A);
##  true
##  gap&gt; G:=Group([ (2,5)(3,4) ]);;
##  gap&gt; mat:=[ [ (), (), (), (), () ], 
##  &gt;   [ (), (), (2,5)(3,4), (2,5)(3,4), () ], 
##  &gt;   [ (), (), (), (2,5)(3,4), (2,5)(3,4) ], 
##  &gt;   [ (), (2,5)(3,4), (), (2,5)(3,4), () ], 
##  &gt;   [ (), (2,5)(3,4), (), (2,5)(3,4), () ] ];;
##  gap&gt; rms:=ReesMatrixSemigroup(G, mat);
##  Rees Matrix Semigroup over Group([ (2,5)(3,4) ])
##  gap&gt; A:=AutomorphismGroup(rms);
##  &lt;group of size 12 with 12 generators&gt;
##  gap&gt; IsAutomorphismGroupOfRMS(A);
##  true
##  gap&gt; G:=ZeroGroup(Group([ (1,3)(2,5), (1,3,2,5) ]));;
##  gap&gt; elts:=Elements(G);;
##  gap&gt; mat:=[ [ elts[7], elts[1], elts[9], elts[1], elts[1] ], 
##  &gt;   [ elts[1], elts[1], elts[1], elts[9], elts[1] ], 
##  &gt;   [ elts[9], elts[1], elts[1], elts[4], elts[9] ], 
##  &gt;   [ elts[1], elts[1], elts[1], elts[1], elts[1] ], 
##  &gt;   [ elts[1], elts[5], elts[1], elts[1], elts[1] ] ];;
##  gap&gt; rzms:=ReesZeroMatrixSemigroup(G, mat);;
##  gap&gt; AutomorphismGroup(rzms);
##  gap&gt; IsAutomorphismGroupOfRZMS(A);
##  true
##  &lt;group of size 512 with 512 generators&gt;
##	</Example> <!-- autos3.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

###########################################################################
##
##  <#GAPDoc Label="IsAutomorphismGroupOfSimpleSemigp">
##  <ManSection>
##  <Prop Name="IsAutomorphismGroupOfSimpleSemigp" Arg="G"/>
##	<Description>
##	returns <C>true</C> if <C>G</C> is the automorphism group of a simple
##	transformation semigroup. This property is set to <C>true</C> when the 
##	computation of the automorphism group of the simple transformation semigroup 
##	is performed. Otherwise, there is no method to check if an arbitrary group 
##	satisfies this property; see 
##	<Ref Attr="AutomorphismGroup"/> for an example of the usage of this 
##	command.<P/>
##	<!--<Example>
##  gap&gt; g1:=Transformation([3,3,2,6,2,4,4,6]);;
##  gap&gt; g2:=Transformation([5,1,7,8,7,5,8,1]);;
##  gap&gt; cs1:=Semigroup(g1,g2);;
##  gap&gt; AutomorphismGroup(cs1);
##  &lt;group of size 12 with 2 generators&gt;
##  gap&gt; IsAutomorphismGroupOfSimpleSemigp(last);
##  true
##	</Example>  autos2.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

DeclareProperty("IsAutomorphismGroupOfSimpleSemigp", IsAutomorphismGroupOfSemigroup);


###########################################################################

#################################
## Automorphisms of Zero Groups #
#################################

###########################################################################

###########################################################################
##
##  <#GAPDoc Label="RightTransversal">
##  <ManSection>
##  <Attr Name="RightTransversal" Arg="autos, inner"/>
##	<Description>
##	returns a transversal of right cosets of the inner automorphisms
##	<C>inner</C> in the group of automorphism <C>autos</C> of a zero group by
##	finding the corresponding right transversal for the underlying group and 
##	applying <Ref Func="ZeroGroupAutomorphism"/>.
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

###########################################################################
##
##  <#GAPDoc Label="IsAutomorphismGroupOfZeroGroup">
##  <ManSection>
##  <Prop Name="IsAutomorphismGroupOfZeroGroup" Arg="G"/>
##	<Description>
##	returns <C>true</C> if <C>G</C> is the automorphism group of a zero group. 
##	This property is set to <C>true</C> when the computation of the automorphism 
##	group of the zero group is performed. Otherwise, there is no method to check 
##	if an arbitrary group satisfies this property; see 
##	<Ref Attr="AutomorphismGroup"/> for an example of the usage of this 
##	command.<P/>
##
##	Every automorphism of a zero group is just an automorphism of the underlying 
##	group that fixes the zero element. 
##
##	<!--<Example>
##  gap&gt; G:=Random(AllGroups(64)); 
##  &lt;pc group of size 64 with 6 generators&gt;
##  gap&gt; ZG:=ZeroGroup(G);
##  &lt;zero group with 7 generators&gt;
##  gap&gt; AutomorphismGroup(ZG);
##  &lt;group of size 1024 with 10 generators&gt;
##  gap&gt; IsAutomorphismGroupOfZeroGroup(last);
##  true
##	</Example>  autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareProperty("IsAutomorphismGroupOfZeroGroup", IsAutomorphismGroupOfSemigroup);

###########################################################################
##
##  <#GAPDoc Label="ZeroGroupAutomorphism">
##  <ManSection>
##  <Func Name="ZeroGroupAutomorphism" Arg="ZG, f"/>
##	<Description>
##	converts the group automorphism <C>f</C> of the underlying group of 
##	the zero group <C>ZG</C> into an automorphism of the zero group <C>ZG</C>.
##	<Example>
##  gap&gt; G:=Random(AllGroups(20));
##  &lt;pc group of size 20 with 3 generators&gt;
##  gap&gt; A:=AutomorphismGroup(G);
##  &lt;group with 2 generators&gt;
##  gap&gt; f:=Random(A);
##  [ f1*f2^4*f3 ] -&gt; [ f1*f2^2 ]
##  gap&gt; ZG:=ZeroGroup(G);
##  &lt;zero group with 4 generators&gt;
##  gap&gt; ZeroGroupAutomorphism(ZG, f);
##  &lt;mapping: &lt;zero group with 4 generators&gt; -&gt; &lt;zero group with 4 generators&gt; &gt;
##  gap&gt; IsZeroGroupAutomorphismRep(last);
##  true
##  gap&gt; UnderlyingGroupAutoOfZeroGroupAuto(last2)=f;
##  true
##	</Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareGlobalFunction("ZeroGroupAutomorphism");

###########################################################################
##
##  <#GAPDoc Label="IsZeroGroupAutomorphismRep">
##  <ManSection>
##  <Filt Type="Representation" Name="IsZeroGroupAutomorphismRep" Arg="f"/>
##	<Description>
##	returns <C>true</C> if the object <C>f</C> is represented as an automorphism 
##	of a zero group; see <Ref Func="ZeroGroupAutomorphism"/> for an example of 
##	the usage of this command.
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareRepresentation("IsZeroGroupAutomorphismRep",  IsAttributeStoringRep, ["grpauto"]);

###########################################################################
##
##  <#GAPDoc Label="UnderlyingGroupAutoOfZeroGroupAuto">
##  <ManSection>
##  <Attr Name="UnderlyingGroupAutoOfZeroGroupAuto" Arg="f"/>
##	<Description>
##	returns the underlying group automorphism of the zero group automorphism 
##	<C>f</C>. That is, the restriction of <C>f</C> to its source  
##	without the zero; see <Ref Func="ZeroGroupAutomorphism"/> for an example of 
##	the usage of this command.
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("UnderlyingGroupAutoOfZeroGroupAuto", IsZeroGroupAutomorphismRep);

###########################################################################

#####################################
## Automorphisms of zero semigroups #
#####################################

###########################################################################

###########################################################################
##
##  <#GAPDoc Label="IsAutomorphismGroupOfZeroSemigroup">
##  <ManSection>
##  <Prop Name="IsAutomorphismGroupOfZeroSemigroup" Arg="G"/>
##	<Description>
##
##	returns <C>true</C> if <C>G</C> is the automorphism group of a zero
##	 semigroup. 
##	This property is set to <C>true</C> when the computation of the automorphism 
##	group of the zero semigroup is performed. 
##	Otherwise, there is no method to check 
##	if an arbitrary group satisfies this property; see 
##	<Ref Attr="AutomorphismGroup"/> for an example of the usage of this 
##	command.<P/>
##
##	Every permutation of a zero semigroup that fixes the zero element is an 
##	automorphism. Thus the automorphism group of a zero semigroup of order 
##	<C>n</C> is isomorphic to the symmetric group on <C>n-1</C> elements. 
##
##	<!-- <Example>
##  gap&gt; S:=ZeroSemigroup(11);
##  &lt;zero semigroup with 11 elements&gt;
##  gap&gt; AutomorphismGroup(S);
##  &lt;group with 2 generators&gt;
##  gap&gt; IsAutomorphismGroupOfZeroSemigroup(last);
##  true
##	</Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareProperty("IsAutomorphismGroupOfZeroSemigroup", IsAutomorphismGroupOfSemigroup);

###########################################################################

###########################################
## Automorphisms of Rees matrix semigroups#
###########################################

###########################################################################
##
##  <#GAPDoc Label="IsAutomorphismGroupOfRMS">
##  <ManSection>
##  <Prop Name="IsAutomorphismGroupOfRMS" Arg="G"/>
##	<Description>
##	returns <C>true</C> if <C>G</C> is the automorphism group of a Rees matrix 
##	semigroup; that is, a semigroup created using the command 
##	<Ref Func="ReesMatrixSemigroup" BookName="ref"/> and/or satisfying 
##	<Ref Prop="IsReesMatrixSemigroup" BookName="ref"/>. <P/>
##
##	Note that 
##	this property is set to <C>true</C> when the computation of the automorphism
##	group is performed. Otherwise, there is no method to check if an arbitrary
##	group satisfies this property; see 
##	<Ref Attr="AutomorphismGroup"/> for an example of the usage of this 
##	command.<P/>
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

DeclareProperty("IsAutomorphismGroupOfRMS", IsAutomorphismGroup);

###########################################################################
##
##  <#GAPDoc Label="IsAutomorphismGroupOfRZMS">
##  <ManSection>
##  <Prop Name="IsAutomorphismGroupOfRZMS" Arg="G"/>
##	<Description>
##	returns <C>true</C> if <C>G</C> is the automorphism group of a Rees matrix 
##	semigroup; that is, a semigroup created using the command 
##	<Ref Func="ReesZeroMatrixSemigroup" BookName="ref"/> and/or satisfying 
##	<Ref Prop="IsReesZeroMatrixSemigroup" BookName="ref"/>. <P/>
##
##	Note that 
##	this property is set to <C>true</C> when the computation of the automorphism
##	group is performed. Otherwise, there is no method to check if an arbitrary
##	group satisfies this property; see 
##	<Ref Attr="AutomorphismGroup"/> for an example of the usage of this 
##	command.<P/>
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

DeclareProperty("IsAutomorphismGroupOfRZMS", IsAutomorphismGroup);

#############################################################################
##
##  <#GAPDoc Label="RMSIsoByTriple">
##  <ManSection>
##  <Func Name="RMSIsoByTriple" Arg="rms1, rms2, triple"/>
##	<Description>
##	this is a function to create an isomorphism between the Rees matrix 
##	semigroups 
##	<C>rms1</C> 
##	and <C>rms2</C> defined by <C>triple</C>. The first component of 
##	<C>triple</C> should be an isomorphism from the underlying group of 
##	<C>rms1</C> to the underlying group of <C>rms2</C>, the second component 
##	should be an isomorphism from the graph associated to the matrix of 
##	<C>rms1</C> to the graph associated with the matrix of <C>rms2</C>, and the 
##	third component should be a function (given as a list of image elements) 
##	from the index sets of <C>rms1</C> to the underlying group of <C>rms2</C>; 
##	see <Cite Key="computing" Where="Section 2"/> for further details. <P/>
##
##	Note that this function only creates an object with representation 
##	<Ref Filt="IsRMSIsoByTripleRep"/> and does not check that <C>triple</C> 
##	actually 
##	defines an isomorphism from <C>rms1</C> to <C>rms2</C> or that the arguments 
##	even make sense. To create an 
##	isomorphism from <C>rms1</C> to <C>rms2</C> use 
##	<Ref Oper="IsomorphismSemigroups"/>.  
##  <Example>
##  gap&gt; G:=Group((1,4,3,5,2));;
##  gap&gt; mat:=[ [ (), (), () ], [ (), (1,4,3,5,2), () ], [ (), (1,3,2,4,5), () ] ];;
##  gap&gt; rms:=ReesMatrixSemigroup(G, mat);;
##  gap&gt; l:=(4,6);;
##  gap&gt; g:=GroupHomomorphismByImages(G, G, [(1,4,3,5,2)], [(1,2,5,3,4)]);
##  [ (1,4,3,5,2) ] -> [ (1,2,5,3,4) ]
##  gap&gt; map:=[(), (1,5,4,2,3), (), (), (), () ];;
##  gap&gt; RMSIsoByTriple(rms, rms, [l, g, map]);
##  [ (4,6), GroupHomomorphismByImages( Group( [ (1,4,3,5,2) ] ), Group( 
##    [ (1,4,3,5,2) ] ), [ (1,4,3,5,2) ], [ (1,2,5,3,4) ] ), 
##    [ (), (1,5,4,2,3), (), (), (), () ] ]
##  gap&gt; IsRMSIsoByTripleRep(last);
##  true
##  gap&gt; #the previous actually defines an automorphism of rms
##  gap&gt; #on the other hand, the next example is nonsense but no error
##  gap&gt; #is given
##  gap&gt; RMSIsoByTriple(rms, rms, [l, g, [()]]);
##  [ (4,6), GroupHomomorphismByImages( Group( [ (1,4,3,5,2) ] ), Group( 
##    [ (1,4,3,5,2) ] ), [ (1,4,3,5,2) ], [ (1,2,5,3,4) ] ), [ () ] ]
##  </Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareGlobalFunction("RMSIsoByTriple");

#############################################################################
##
##  <#GAPDoc Label="IsRMSIsoByTripleRep">
##  <ManSection>
##  <Filt Type="Representation" Name="IsRMSIsoByTripleRep" Arg="f"/>
##	<Description>
##	returns <C>true</C> if the object <C>f</C> is represented as an isomorphism 
##	of Rees matrix semigroups by a triple; as explained in <Cite Key="computing" 
##	Where="Section 2"/>; see <Ref Func="RMSIsoByTriple"/> for an example of the 
##	usage of this command.<P/>
##
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareRepresentation("IsRMSIsoByTripleRep",  IsAttributeStoringRep, ["triple"]);


#############################################################################
##
##  <#GAPDoc Label="RightTransStabAutoGroup">
##  <ManSection>
##	<Oper Name="RightTransStabAutoGroup" Arg="S, elts, func"/> 
##	<Description>
##
##	returns a right transversal of the stabilizer w.r.t the action <C>func</C> 
##	of the elements 
##	<C>elts</C> in the automorphism group of the zero semigroup, Rees matrix
##	semigroup, or Rees 0-matrix semigroup <C>S</C>. That is, <C>S</C> 
##	satisfying <Ref Prop="IsZeroSemigroup"/>,  
##	<Ref Prop="IsReesMatrixSemigroup" BookName="ref"/>, or 
##	<Ref Prop="IsReesZeroMatrixSemigroup" BookName="ref"/>.
##	<Example>
##  gap&gt; S:=ZeroSemigroup(6);
##  &lt;zero semigroup with 6 elements&gt;
##  gap&gt; elts:=Elements(S);
##  [ 0, z1, z2, z3, z4, z5 ]
##  gap&gt; Length(RightTransStabAutoGroup(S, [elts[1]], OnSets));
##  1
##  gap&gt; Length(RightTransStabAutoGroup(S, [elts[1], elts[2]], OnSets));
##  5
##  gap&gt; Length(RightTransStabAutoGroup(S, [elts[1], elts[2]], OnTuples));
##  5
##  gap&gt; G:=Group([ (1,2) ]);;
##  gap&gt; mat:=[ [ (), (), () ], [ (), (1,2), () ], [ (), (1,2), (1,2) ], 
##  &gt;    [ (), (), () ], [ (), (1,2), () ] ];;
##  gap&gt; rms:=ReesMatrixSemigroup(G, mat);;
##  gap&gt; Size(rms);
##  30
##  gap&gt; GeneratorsOfSemigroup(rms);
##  [ (1,(),2), (1,(),3), (1,(),4), (1,(),5), (2,(),1), (3,(),1), (1,(1,2),1) ]
##  gap&gt; Length(RightTransStabAutoGroup(rms, last, OnSets));
##  4
##  gap&gt; Length(RightTransStabAutoGroup(rms, GeneratorsOfSemigroup(rms), 
##  &gt; OnTuples));
##  8
##  gap&gt; G:=ZeroGroup(Group([ (1,3) ]));;
##  gap&gt; z:=MultiplicativeZero(G);; x:=Elements(G)[2];;
##  gap&gt; mat:=[ [ z, z, z ], [ z, z, z ], [ z, z, z ], [ z, z, z ], [ z, x, z ] ];;
##  gap&gt; rzms:=ReesZeroMatrixSemigroup(G, mat);
##  gap&gt; Size(rzms);
##  31
##  gap&gt; Size(GeneratorsOfSemigroup(rzms));
##  6
##  gap&gt; Length(RightTransStabAutoGroup(rzms, GeneratorsOfSemigroup(rzms), 
##  &gt; OnSets));
##  512
##  gap&gt; A:=AutomorphismGroup(rzms);
##  &lt;group of size 3072 with 3072 generators&gt;
##  </Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("RightTransStabAutoGroup", [IsSemigroup, IsMultiplicativeElementCollection, IsFunction]);

#############################################################################
##
##  <#GAPDoc Label="RMSInducedFunction">
##  <ManSection>
##	<Oper Name="RMSInducedFunction" Arg="RMS, lambda, gamma, g"/> 
##	<Description>
##	<C>lambda</C> is an automorphism of the graph associated to the Rees matrix 
##	semigroup <C>RMS</C>, <C>gamma</C> an automorphism of the underlying group 
##	of <C>RMS</C>, and <C>g</C> an element of the underlying group of 
##	<C>RMS</C>.  The function <C>RMSInducedFunction</C> attempts to find the 
##	function determined by <C>lambda</C> and <C>gamma</C> from the union of the 
##	index sets <C>I</C> and <C>J</C> 
##	to the group <C>G</C> of the Rees matrix semigroup <C>RMS</C> over <C>G</C>, 
##	<C>I</C>, and <C>J</C> with respect to <C>P</C> where the first 
##	element is given by the element <C>g</C>.  If a conflict is found, then 
##	<C>false</C> is returned together with the induced map; see 
##	<Cite Key="computing" Where="Section 2"/> for further details.
##	<Example>
##  gap&gt; G:=Group([ (1,2) ]);;
##  gap&gt; mat:=[ [ (), (), () ], [ (), (1,2), () ], [ (), (1,2), (1,2) ], 
##  &gt;    [ (), (), () ], [ (), (1,2), () ] ];;
##  gap&gt; rms:=ReesMatrixSemigroup(G, mat);;
##  gap&gt; l:=(1,2)(4,5,6);
##  (1,2)(4,5,6)
##  gap&gt; gam:=One(AutomorphismGroup(G));
##  IdentityMapping( Group([ (1,2) ]) )
##  gap&gt; g:=(1,2);
##  gap&gt; RMSInducedFunction(rms, l, gam, g);
##  [ false, [ (1,2), (), (), (), (), (1,2), (1,2), () ] ]
##  gap&gt; RMSInducedFunction(rms, (4,7), gam, ());
##  [ true, [ (), (), (), (), (), (), (), () ] ]
##  </Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("RMSInducedFunction", [IsReesMatrixSemigroup, IS_PERM, IsGeneralMapping, IsMultiplicativeElement]);

###########################################################################

#############################################
## Automorphisms of Rees 0-matrix semigroups#
#############################################

###########################################################################

#############################################################################
##
##  <#GAPDoc Label="RZMSIsoByTriple">
##  <ManSection>
##  <Func Name="RZMSIsoByTriple" Arg="rzms1, rzms2, triple"/>
##	<Description>
##	this is a function to create an isomorphism between the Rees 0-matrix 
##	semigroups <C>rzms1</C> 
##	and <C>rzms2</C> defined by <C>triple</C>. The first component of 
##	<C>triple</C> should be an isomorphism from the underlying zero group of 
##	<C>rzms1</C> to the underlying zero group of <C>rzms2</C>, the second 
##	component 
##	should be an isomorphism from the graph associated to the matrix of 
##	<C>rzms1</C> to the graph associated with the matrix of <C>rzms2</C>, and 
##	the third component should be a function (given as a list of image elements) 
##	from the index sets of <C>rzms1</C> to the underlying zero group of 
##	<C>rzms2</C>; see <Cite Key="computing" Where="Section 2"/> for further 
##	details. <P/>
##
##	Note that this function only creates an object with representation 
##	<Ref Filt="IsRZMSIsoByTripleRep"/> and does not check that <C>triple</C> 
##	actually 
##	defines an isomorphism from <C>rzms1</C> to <C>rzms2</C> or that the 
##	arguments even make sense. To create an 
##	isomorphism from <C>rzms1</C> to <C>rzms2</C> use 
##	<Ref Oper="IsomorphismSemigroups"/>.  
##  <Example>
##  gap&gt; G:=Group((1,4,3,5,2));;
##  gap&gt; ZG:=ZeroGroup(G);
##  &lt;zero group with 2 generators&gt;
##  gap&gt; mat:=[ [ (), (), () ], [ (), (1,4,3,5,2), () ], [ (), (1,3,2,4,5), () ] ];;
##  gap&gt; mat:=List(mat, x-&gt; List(x, ZeroGroupElt));
##  [ [ (), (), () ], [ (), (1,4,3,5,2), () ], [ (), (1,3,2,4,5), () ] ]
##  gap&gt; rms:=ReesZeroMatrixSemigroup(ZG, mat);
##  Rees Zero Matrix Semigroup over &lt;zero group with 2 generators&gt;
##  gap&gt; l:=(4,6);;
##  gap&gt; g:=GroupHomomorphismByImages(G, G, [(1,4,3,5,2)], [(1,2,5,3,4)]);
##  [ (1,4,3,5,2) ] -&gt; [ (1,2,5,3,4) ]
##  gap&gt; g:=ZeroGroupAutomorphism(ZG, g);
##  &lt;mapping: &lt;zero group with 2 generators&gt; -&gt; &lt;zero group with 2 generators&gt; &gt;
##  gap&gt;  map:=List([(), (1,5,4,2,3), (), (), (), () ], ZeroGroupElt);;
##  gap&gt; RZMSIsoByTriple(rms, rms, [l, g, map]);
##  [ (4,6), &lt;mapping: &lt;zero group with 2 generators&gt; -&gt; &lt;zero group with 
##    2 generators&gt; &gt;, 
##  [ ZeroGroup(()), ZeroGroup((1,5,4,2,3)), ZeroGroup(()), ZeroGroup(()), 
##      ZeroGroup(()), ZeroGroup(()) ] ]
##  gap&gt; RZMSIsoByTriple(rms, rms, [l, g, [()]]);
##  [ (4,6), &lt;mapping: &lt;zero group with 2 generators&gt; -&gt; &lt;zero group with 
##    2 generators&gt; &gt;, [ () ] ]
##  gap&gt; IsRZMSIsoByTripleRep(last);
##  true    
##  </Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareGlobalFunction("RZMSIsoByTriple");

#############################################################################
##
##  <#GAPDoc Label="IsRZMSIsoByTripleRep">
##  <ManSection>
##  <Filt Type="Representation" Name="IsRZMSIsoByTripleRep" Arg="f"/>
##	<Description>
##	returns <C>true</C> if the object <C>f</C> is represented as an isomorphism 
##	of Rees matrix semigroups by a triple; as explained in 
##	<Cite Key="computing" Where="Section 2"/>; see 
##	<Ref Func="RZMSIsoByTriple"/> for an example of the usage of this 
##	command.<P/>
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareRepresentation("IsRZMSIsoByTripleRep",  IsAttributeStoringRep, ["triple"]);

#############################################################################
##
##  <#GAPDoc Label="RZMSGraph">
##  <ManSection>
##  <Attr Name="RZMSGraph" Arg="rzms"/>
##	<Description>
##  if <C>rzms</C> is a Rees 0-matrix semigroup over a zero group <C>G^0</C>, 
#3	index sets <C>I</C> and <C>J</C>, and matrix <C>P</C>, then 
##	<C>RZMSGraph</C> returns the 
##  undirected bipartite graph with <C>|I|+|J|</C> vertices and edge 
##	<C>(i,j)</C> if and only 
##	if <C>i&lt;|I|+1</C>, <C>j&gt;|I|</C> and <C>p_{j-|I|, i}</C> is not 
##	zero.<P/>
##
##	The returned object is a simple undirected graph created in 
##	<Package>GRAPE</Package> using the command
##	<Display>
##	Graph(Group(()), [1..n+m], OnPoints, adj, true);
##	</Display>
##	where <C>adj</C> is <C>true</C> if and only 
##	if <C>i&lt;|I|+1</C>, <C>j&gt;|I|</C> and <C>p_{j-|I|, i}</C> is not 
##	zero.<P/>
##
##	<B>Please note:</B> if <Package>grape</Package> is not loaded, then this 
##	function will not work.
##	<Example>
##  gap&gt; zg:=ZeroGroup(Group(()));;
##  gap&gt; z:=Elements(zg)[1];
##  0
##  gap&gt; x:=Elements(zg)[2];
##  ()
##  gap&gt; mat:=[ [ 0, 0, 0 ], [ (), 0, 0 ], [ (), (), 0 ] ];;
##  gap&gt; rzms:=ReesZeroMatrixSemigroup(zg, mat);;
##  gap&gt; RZMSGraph(rzms);
##  rec( isGraph := true, order := 6, group := Group(()), 
##    schreierVector := [ -1, -2, -3, -4, -5, -6 ], 
##    adjacencies := [ [ 5, 6 ], [ 6 ], [  ], [  ], [ 1 ], [ 1, 2 ] ], 
##    representatives := [ 1, 2, 3, 4, 5, 6 ], names := [ 1, 2, 3, 4, 5, 6 ] )
##  gap&gt; UndirectedEdges(last);
##  [ [ 1, 5 ], [ 1, 6 ], [ 2, 6 ] ]
##  </Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("RZMSGraph", IsReesZeroMatrixSemigroup, "mutable");

#############################################################################
##
##  <#GAPDoc Label="RZMSInducedFunction">
##  <ManSection>
##	<Oper Name="RZMSInducedFunction" Arg="RZMS, lambda, gamma, g, comp"/> 
##	<Description>
##	<C>lambda</C> is an automorphism of the graph associated to the Rees 0-
##	matrix semigroup <C>RZMS</C>, <C>gamma</C> an automorphism of the underlying 
##	zero group of <C>RZMS</C>, <C>comp</C> is a connected component of the graph 
##	associated to <C>RZMS</C>, and <C>g</C> is an element of the underlying zero 
##	group of <C>RZMS</C>.  The function <C>RZMSInducedFunction</C> attempts to 
##	find the partial function determined by <C>lambda</C> and <C>gamma</C> from 
##	<C>comp</C> to the zero group <C>G^0</C> 
##	of <C>G</C> of the Rees 0-matrix semigroup <C>RZMS</C> over <C>G^0</C>, 
##	<C>I</C>, and <C>J</C> with respect to <C>P</C> where the image of the first 
##	element 
##	in <C>comp</C> is given by the element <C>g</C>.  If a conflict is found, 
##	then <C>fail</C> is returned; see <Cite Key="computing" Where="Section 2"/> for 
##	further details.<P/>
##
##	<B>Please note:</B> if  <Package>grape</Package> is not loaded, then this 
##	function will not work.
##	<Example>
##  gap&gt; zg:=ZeroGroup(Group(()));;
##  gap&gt; z:=Elements(zg)[1];
##  0
##  gap&gt; x:=Elements(zg)[2];
##  ()
##  gap&gt; mat:=[ [ z, z, z ], [ x, z, z ], [ x, x, z ] ];;
##  gap&gt; rzms:=ReesZeroMatrixSemigroup(zg, mat);;
##  gap&gt; RZMSInducedFunction(rzms, (), One(AutomorphismGroup(zg)), x, 
##  &gt; [1,2,5,6])
##  [ (), (),,, (), () ]
##  gap&gt; RZMSInducedFunction(rzms, (), One(AutomorphismGroup(zg)), x, [3]);     
##  [ ,, () ]
##  gap&gt; RZMSInducedFunction(rzms, (), One(AutomorphismGroup(zg)), x, [4]);
##  [ ,,, () ]
##  gap&gt; zg:=ZeroGroup(Group([ (1,5,2,3), (1,4)(2,3) ]));;
##  gap&gt; elts:=Elements(zg);;
##  gap&gt; mat:=[ [ elts[1], elts[1], elts[11], elts[1], elts[1] ], 
##  &gt;    [ elts[1], elts[13], elts[21], elts[1], elts[1] ], 
##  &gt;    [ elts[1], elts[16], elts[1], elts[16], elts[3] ], 
##  &gt;    [ elts[10], elts[17], elts[1], elts[1], elts[1] ], 
##  &gt;    [ elts[1], elts[1], elts[1], elts[4], elts[1] ] ];
##  gap&gt; rzms:=ReesZeroMatrixSemigroup(zg, mat);                                   
##  gap&gt; RZMSInducedFunction(rzms, (), Random(AutomorphismGroup(zg)), 
##  &gt; Random(elts), [1..10])=fail;
##  false
##	</Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

## JDM should include some example where the about function returns fail. I couldn't find one today...

DeclareOperation("RZMSInducedFunction", [IsReesZeroMatrixSemigroup, IS_PERM, IsGeneralMapping, IsMultiplicativeElement, IsList]);

#############################################################################
##
##  <#GAPDoc Label="RZMStoRZMSInducedFunction">
##  <ManSection>
##	<Oper Name="RZMStoRZMSInducedFunction" Arg="RZMS1, RZMS2, lambda, gamma, elts"/>
##	<Description>
##	<C>lambda</C> is an automorphism of the graph associated to the Rees 0-
##	matrix semigroup <C>RZMS1</C> composed with isomorphism from that graph to the graph 
##	of <C>RZMS2</C>, <C>gamma</C> an automorphism of the 
##	underlying zero group of <C>RZMS1</C>,  and <C>elts</C> is a list of 
##	elements of the underlying zero group of <C>RZMS2</C>.  The function 
##	<C>RZMStoRZMSInducedFunction</C> attempts to find the function determined by
##	<C>lambda</C> and <C>gamma</C> from the union of the index sets <C>I</C> and
##	<C>J</C> of <C>RZMS1</C> to the zero group <C>G^0</C> of 
##	the Rees 0-matrix semigroup <C>RZMS2</C> over the zero group <C>G^0</C>, 
##	sets <C>I</C> and <C>J</C>, and matrix <C>P</C>
##	where the image of the first element in the <C>i</C>th connected
##	component of the associated graph of <C>RZMS1</C> is given by 
##	<C>elts[i]</C>.  If a conflict is found, then <C>false</C> is returned; see 
##	<Cite Key="computing" Where="Section 2"/> for further details.<P/>
##
##	<B>Please note:</B> if <Package>grape</Package> is not loaded, then this 
##	function will not work.
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 4, 4, 8, 8, 8, 8, 4, 8 ] ), 
##    Transformation( [ 8, 2, 8, 2, 5, 5, 8, 8 ] ), 
##    Transformation( [ 8, 8, 3, 7, 8, 3, 7, 8 ] ), 
##    Transformation( [ 8, 6, 6, 8, 6, 8, 8, 8 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; D:=GreensDClasses(S);;
##  gap&gt; rms1:=Range(IsomorphismReesMatrixSemigroupOfDClass(D[1]));
##  Rees Zero Matrix Semigroup over &lt;zero group with 2 generators&gt;
##  gap&gt; rms2:=Range(IsomorphismReesMatrixSemigroupOfDClass(D[4]));
##  Rees Zero Matrix Semigroup over &lt;zero group with 2 generators&gt;
##  gap&gt; gam:=One(AutomorphismGroup
##  &gt; (UnderlyingSemigroupOfReesZeroMatrixSemigroup(Group(rms1))));
##  IdentityMapping( &lt;zero group with 2 generators&gt; )
##  gap&gt; g:=One(UnderlyingSemigroupOfReesZeroMatrixSemigroup(rms2));
##  ()
##  gap&gt; RZMStoRZMSInducedFunction(rms1, rms2, (2,3)(5,6), gam, [g]);
##  [ (), (), (), (), (), () ]
##	</Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

## JDM should include some example where the about function returns fail. 
##  I couldn't find one today...

DeclareOperation("RZMStoRZMSInducedFunction", [IsReesZeroMatrixSemigroup,
IsReesZeroMatrixSemigroup, IS_PERM, IsGeneralMapping, IsList]);

###########################################################################

###################
## Isomorphisms	  #
###################

###########################################################################

###########################################################################
##
##  <#GAPDoc Label="IsomorphismAutomorphismGroupOfRMS">
##  <ManSection>
##  <Attr Name="IsomorphismAutomorphismGroupOfRMS" Arg="G"/>
##
##	<Description>
##	if <C>G</C> is the automorphism group of a simple transformation semigroup, 
##	then <C>IsomorphismAutomorphismGroupOfRMS</C> returns a 
##	<Ref Func="GroupHomomorphismByImages" BookName="ref"/> from the automorphism 
##	group of <C>G</C> to the automorphism group of an isomorphic Rees matrix 
##	semigroup, obtained by using <Ref Func="IsomorphismReesMatrixSemigroup"/>.
##	<Example>
##  gap&gt; g1:=Transformation([1,2,2,1,2]);;
##  gap&gt; g2:=Transformation([3,4,3,4,4]);;
##  gap&gt; g3:=Transformation([3,4,3,4,3]);;
##  gap&gt; g4:=Transformation([4,3,3,4,4]);;
##  gap&gt; cs5:=Semigroup(g1,g2,g3,g4);;
##  gap&gt; AutomorphismGroup(cs5);
##  &lt;group of size 16 with 3 generators&gt;
##  gap&gt; IsomorphismAutomorphismGroupOfRMS(last);
##  [ SemigroupHomomorphism ( &lt;semigroup with 4 generators&gt;-&gt;&lt;semigroup with 
##      4 generators&gt;), SemigroupHomomorphism ( &lt;semigroup with 
##      4 generators&gt;-&gt;&lt;semigroup with 4 generators&gt;), 
##    SemigroupHomomorphism ( &lt;semigroup with 4 generators&gt;-&gt;&lt;semigroup with 
##      4 generators&gt;) ] -&gt; 
##  [ [ (1,4)(2,3)(5,6), IdentityMapping( Group( [ (1,2) ] ) ), 
##        [ (), (1,2), (1,2), (), (), () ] ], 
##    [ (1,3,4,2), IdentityMapping( Group( [ (1,2) ] ) ), 
##        [ (), (), (), (), (), (1,2) ] ], 
##    [ (1,3)(2,4), IdentityMapping( Group( [ (1,2) ] ) ), 
##        [ (), (), (), (), (), (1,2) ] ] ] 
##	</Example> <!-- auto2.tst --> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("IsomorphismAutomorphismGroupOfRMS", IsAutomorphismGroupOfSimpleSemigp);

###########################################################################
##
##  <#GAPDoc Label="IsomorphismPermGroup">
##  <ManSection>
##  <Attr Name="IsomorphismPermGroup" Arg="G"/>
##	<Description>
##	if <C>G</C> satisfies <Ref Prop="IsAutomorphismGroupOfSimpleSemigp"/>, then 
##	<C>IsomorphismPermGroup</C> returns an isomorphism from <C>G</C> to a 
##	permutation group by composing the result 
##	<C>f</C> of <Ref Attr="IsomorphismAutomorphismGroupOfRMS"/> on <C>G</C> with
##	the result of <C>IsomorphismPermGroup</C> on <C>Range(f)</C>.<P/>
##	
##	if <C>G</C> satisfies <Ref Prop="IsAutomorphismGroupOfRMS"/> or 
##	<Ref Prop="IsAutomorphismGroupOfRZMS"/>, then <C>IsomorphismPermGroup</C> 
##	returns an isomorphism from <C>G</C> to a permutation group acting  
##	either on the elements of <C>S</C> or on 
##	itself, whichever gives a permutation group of lower degree.<P/>
##
##	if <C>G</C> is a transformation semigroup that satisfies 
##	<Ref Prop="IsGroupAsSemigroup"/>, then <C>IsomorphismPermGroup</C> 
##	returns an isomorphism from <C>G</C> to the permutation group obtained by 
##	applying <Ref Func="AsPermOfRange"/> to any element of <C>G</C>.<P/>
##
##	if <C>G</C> is a group <C>H</C>-class of a transformation semigroup, then 
##	<C>IsomorphismPermGroup</C> returns an isomorphism from <C>G</C> to the 
##	permutation group obtained by applying <Ref Func="AsPermOfRange"/> to any
##	element of <C>G</C>.
##	<Example>
##  gap&gt; g1:=Transformation([3,3,2,6,2,4,4,6]);;
##  gap&gt; g2:=Transformation([5,1,7,8,7,5,8,1]);;
##  gap&gt; cs1:=Semigroup(g1,g2);;
##  gap&gt; AutomorphismGroup(cs1);
##  &lt;group of size 12 with 2 generators&gt;
##  gap&gt; IsomorphismPermGroup(last);
##  [ SemigroupHomomorphism ( &lt;semigroup with 2 generators&gt;-&gt;&lt;semigroup with 
##      2 generators&gt;), SemigroupHomomorphism ( &lt;semigroup with 
##      2 generators&gt;-&gt;&lt;semigroup with 2 generators&gt;) ] -&gt; 
##  [ (1,11,2,12,3,10)(4,8,5,9,6,7), (1,6)(2,5)(3,4)(7,10)(8,12)(9,11) ]
##  gap&gt; Size(cs1);
##  96
##  gap&gt; a:=IdempotentNC([[1,3,4],[2,5],[6],[7],[8]],[3,5,6,7,8])*(3,5);;
##  gap&gt; b:=IdempotentNC([[1,3,4],[2,5],[6],[7],[8]],[3,5,6,7,8])*(3,6,7,8);;
##  gap&gt; S:=Semigroup(a,b);;
##  gap&gt; IsGroupAsTransSemigroup(S);
##  true
##  gap&gt; IsomorphismPermGroup(S);
##  SemigroupHomomorphism ( &lt;semigroup with 2 generators&gt;-&gt;Group(
##  [ (3,5), (3,6,7,8) ]))
##  gap&gt; gens:=[Transformation([3,5,3,3,5,6]), Transformation([6,2,4,2,2,6])];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; H:=GroupHClassOfGreensDClass(GreensDClassOfElement(S, Elements(S)[1]));
##  {Transformation( [ 2, 2, 2, 2, 2, 6 ] )}
##  gap&gt; IsomorphismPermGroup(H);
##  SemigroupHomomorphism ( {Transformation( [ 2, 2, 2, 2, 2, 6 ] )}-&gt;Group(()))
##	</Example> <!-- autos2.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

###########################################################################
##
##  <#GAPDoc Label="IsomorphismFpSemigroup">
##  <ManSection>
##  <Attr Name="IsomorphismFpSemigroup" Arg="S"/>
##	<Description>
##	returns an isomorphism to a finitely presented semigroup from the 
##	transformation semigroup <C>S</C>. This currently works by running the 
##	function
##	<Ref Func="FroidurePinExtendedAlg" BookName="ref"/> in the library.<P/>
##	 
##	If <C>S</C> satisfies <Ref Prop="IsMonoid" BookName="ref"/>, use the command 
##	<Ref Attr="IsomorphismFpMonoid"/> instead.
##	<Example> 
##  gap&gt; gens:=[ Transformation( [1,8,11,2,5,16,13,14,3,6,15,10,7,4,9,12 ] ), 
##  &gt;   Transformation( [1,16,9,6,5,8,13,12,15,2,3,4,7,10,11,14] ), 
##  &gt;   Transformation( [1,3,7,9,1,15,5,11,13,11,13,3,5,15,7,9 ] ) ];
##  gap&gt; S:=Semigroup(gens);
##  &lt;semigroup with 3 generators&gt;
##  gap&gt; IsomorphismFpSemigroup(last);
##  SemigroupHomomorphismByImages ( &lt;trans. semigroup of size 16 with 
##  3 generators&gt;-&gt;Semigroup( [ s1, s2, s3 ] ))
##	</Example> <!--autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

###########################################################################
##
##  <#GAPDoc Label="IsomorphismFpMonoid">
##  <ManSection>
##  <Attr Name="IsomorphismFpMonoid" Arg="S"/>
##	<Description>
##	returns an isomorphism to a finitely presented monoid from the 
##	transformation monoid <C>S</C>. Currently works by running the function
##	<Ref Func="FroidurePinExtendedAlg" BookName="ref"/> in the library.<P/> 
##	
##	If <C>S</C> satisfies <Ref Prop="IsSemigroup" BookName="ref"/>, use the 
##	command <Ref Attr="IsomorphismFpSemigroup"/> instead.
##	<Example> 
##  gap&gt; x:=Transformation([2,3,4,5,6,7,8,9,1]);;
##  gap&gt; y:=Transformation([4,2,3,4,5,6,7,8,9]);;
##  gap&gt; S:=Monoid(x,y);;
##  gap&gt; IsomorphismFpMonoid(last);
##  SemigroupHomomorphismByImages ( &lt;trans. semigroup of size 40266 with 
##  3 generators&gt;-&gt;Monoid( [ m1, m2 ], ... ))
##  gap&gt; Length(RelationsOfFpMonoid(Range(last)));
##  932
##	</Example>  <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

#############################################################################
##
##  <#GAPDoc Label="IsomorphismSemigroups">
##  <ManSection>
##	<Oper Name="IsomorphismSemigroups" Arg="S, T"/>
##	<Description>
##	this operation returns an isomorphism from the semigroup <C>S</C> to 
##	the semigroup <C>T</C> if one exists and returns <C>fail</C> otherwise. <P/>
##
##	<B>Please note:</B> this function currently only works for zero groups, zero 
##	semigroups, Rees matrix semigroups, and Rees 0-matrix semigroups.<P/>
##
##	<B>Please note:</B> if <Package>grape</Package> is not loaded, then this 
##	function will not work when <C>S</C> and <C>T</C> satisfy 
##	<Ref Prop="IsReesZeroMatrixSemigroup" BookName="ref"/>.
##
##	<Example>
##  gap&gt; ZG1:=ZeroGroup(Group((1,2,3,5,4)));
##  &lt;zero group with 2 generators&gt;
##  gap&gt; ZG2:=ZeroGroup(Group((1,2,3,4,5)));
##  &lt;zero group with 2 generators&gt;
##  gap&gt; IsomorphismSemigroups(ZG1, ZG2);
##  SemigroupHomomorphismByImagesOfGens ( &lt;zero group with 
##  2 generators&gt;-&gt;&lt;zero group with 2 generators&gt;)
##  gap&gt; ZG2:=ZeroGroup(Group((1,2,3,4)));
##  &lt;zero group with 2 generators&gt;
##  gap&gt; IsomorphismSemigroups(ZG1, ZG2);
##  fail
##  gap&gt; IsomorphismSemigroups(ZeroSemigroup(5),ZeroSemigroup(5));
##  IdentityMapping( &lt;zero semigroup with 5 elements&gt; )
##  gap&gt; IsomorphismSemigroups(ZeroSemigroup(5),ZeroSemigroup(6));
##  fail
##  gap&gt; gens:=[ Transformation( [ 4, 4, 8, 8, 8, 8, 4, 8 ] ), 
##  &gt;   Transformation( [ 8, 2, 8, 2, 5, 5, 8, 8 ] ), 
##  &gt;   Transformation( [ 8, 8, 3, 7, 8, 3, 7, 8 ] ), 
##  &gt;   Transformation( [ 8, 6, 6, 8, 6, 8, 8, 8 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; D:=GreensDClasses(S);;
##  gap&gt; rms1:=Range(IsomorphismReesMatrixSemigroupOfDClass(D[1]));;
##  gap&gt; rms2:=Range(IsomorphismReesMatrixSemigroupOfDClass(D[4]));;
##  gap&gt; IsomorphismSemigroups(rms1, rms2);
##  [ (2,3)(5,6), IdentityMapping( &lt;zero group with 2 generators&gt; ), 
##    [ ZeroGroup(()), ZeroGroup(()), ZeroGroup(()), ZeroGroup(()), 
##        ZeroGroup(()), ZeroGroup(()) ] ]
##  gap&gt; IsomorphismSemigroups(rms2, rms1);
##  [ (2,3)(5,6), IdentityMapping( &lt;zero group with 2 generators&gt; ), 
##    [ ZeroGroup(()), ZeroGroup(()), ZeroGroup(()), ZeroGroup(()),  
##        ZeroGroup(()), ZeroGroup(()) ] ]
##  gap&gt; rms2:=Range(IsomorphismReesMatrixSemigroupOfDClass(D[2]));
##  Group(())
##  gap&gt; IsomorphismSemigroups(rms2, rms1);
##  fail
##  gap&gt; rms2:=RandomReesZeroMatrixSemigroup(5,5,5);
##  Rees Zero Matrix Semigroup over &lt;zero group with 2 generators&gt;
##  gap&gt; IsomorphismSemigroups(rms2, rms1);
##  fail
##  gap&gt; rms2:=RandomReesMatrixSemigroup(5,5,5);
##  Rees Matrix Semigroup over Group([ (1,2)(3,4,5), (2,4,3), (1,4,5,3), 
##    (1,4,5,2) ])
##  gap&gt; IsomorphismSemigroups(rms2, rms1);
##  fail
##  gap&gt; IsomorphismSemigroups(rms1, rms2);
##  fail
##	</Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("IsomorphismSemigroups", [IsSemigroup, IsSemigroup]);

###########################################################################
## 
##  <#GAPDoc Label="IsomorphismReesMatrixSemigroupOfDClass">
##  <ManSection>
##	<Attr Name="IsomorphismReesMatrixSemigroupOfDClass" Arg="D"/>
##	<Description>
##	The <E>principal factor</E> of the <C>D</C>-class <C>D</C> is the semigroup 
##	with elements <C>D</C> and <C>0</C> and multiplication <C>x*y</C> defined to 
##	be the product <C>xy</C> in the semigroup containing <C>D</C> if 
##	<C>xy</C> in <C>D</C> and <C>0</C> otherwise. <P/>
##
##	<C>IsomorphismReesMatrixSemigroupOfDClass</C> returns an 
##	isomorphism from the principal factor of the
##	<C>D</C>-class <C>D</C> to a Rees matrix, Rees 0-matrix or 
##	zero semigroup, as given by the Rees-Suschewitsch Theorem; see 
##	<Cite Key="howie" Where="Theorem 3.2.3"/>. 
##	<Example> 
##  gap&gt; g1:=Transformation( [ 4, 6, 3, 8, 5, 6, 10, 4, 3, 7 ] );;
##  gap&gt; g2:=Transformation( [ 5, 6, 6, 3, 8, 6, 3, 7, 8, 4 ] );;
##  gap&gt; g3:=Transformation( [ 8, 6, 3, 2, 8, 10, 9, 2, 6, 2 ] );;
##  gap&gt; m23:=Monoid(g1,g2,g3);;
##  gap&gt; D:=GreensDClasses(m23)[17];
##  {Transformation( [ 7, 6, 6, 6, 7, 4, 8, 6, 6, 6 ] )}
##  gap&gt; IsomorphismReesMatrixSemigroupOfDClass(D);
##  SemigroupHomomorphism ( {Transformation( [ 7, 6, 6, 6, 7, 4, 8, 6, 6, 6 
##   ] )}-&gt;&lt;zero semigroup with 3 elements&gt;)
##  gap&gt; D:=GreensDClasses(m23)[77];
##  {Transformation( [ 6, 6, 6, 6, 6, 6, 6, 6, 6, 6 ] )}
##  gap&gt; IsomorphismReesMatrixSemigroupOfDClass(D);
##  SemigroupHomomorphism ( {Transformation( [ 6, 6, 6, 6, 6, 6, 6, 6, 6, 6 
##   ] )}-&gt;Rees Matrix Semigroup over Group(()))
##  gap&gt; D:=GreensDClasses(m23)[1];
##  {Transformation( [ 1 .. 10 ] )}
##  gap&gt; IsomorphismReesMatrixSemigroupOfDClass(D);
##  SemigroupHomomorphism ( {Transformation( [ 1 .. 10 ] )}-&gt;Group(()))
##  gap&gt; D:=GreensDClasses(m23)[23];
##  {Transformation( [ 6, 7, 3, 6, 6, 6, 6, 6, 7, 6 ] )}
##  gap&gt; IsomorphismReesMatrixSemigroupOfDClass(D);
##  SemigroupHomomorphism ( {Transformation( [ 6, 7, 3, 6, 6, 6, 6, 6, 7, 6 
##   ] )}-&gt;Rees Zero Matrix Semigroup over &lt;zero group with 3 generators&gt;)
##	</Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
##

DeclareAttribute("IsomorphismReesMatrixSemigroupOfDClass", IsGreensDClass and IsAssociatedSemigpTransSemigp);

#############################################################################
## 
##  <#GAPDoc Label="IsomorphismReesMatrixSemigroup">
##  <ManSection>
##	<Oper Name="IsomorphismReesMatrixSemigroup" Arg="S"/>
##	<Description>
##	returns an isomorphism from the (completely) simple transformation semigroup 
##	<C>S</C> to a Rees matrix semigroup, as given by the Rees-Suschewitsch 
##	Theorem; see <Cite Key="howie" Where="Theorem 3.2.3"/>.
##	<Example>
##  gap&gt; g1:=Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] );;
##  gap&gt; g2:=Transformation( [ 2, 3, 4, 5, 6, 8, 7, 1, 2, 2 ] );;
##  gap&gt; cs2:=Semigroup(g1,g2);;
##  gap&gt; IsomorphismReesMatrixSemigroup(cs2);
##  SemigroupHomomorphism ( &lt;semigroup with 
##  2 generators&gt;-&gt;Rees Matrix Semigroup over Group(
##  [ (2,5)(3,8)(4,6), (1,6,3)(5,8) ]))
##  </Example>  <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
##

###########################################################################
##
##	<#GAPDoc Label="IsInnerAutomorphismOfSemigroup">
##	<ManSection>
##	<Prop Name="IsInnerAutomorphismOfSemigroup" Arg="f"/>
##	<Description>
##	returns <C>true</C> if the general mapping <C>f</C> is an inner 
##	automorphism of a semigroup; see <Ref Oper="InnerAutomorphismOfSemigroup"/>
##	for further details.
##
##	<Example> 
##  gap&gt; S:=RandomSemigroup(2,9);;
##  gap&gt; f:=InnerAutomorphismOfSemigroupNC(S, (1,2)(3,4));
##  ^(1,2)(3,4)
##  gap&gt; IsInnerAutomorphismOfSemigroup(f);
##  true
##	</Example> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareProperty("IsInnerAutomorphismOfSemigroup", IsGeneralMapping);

###########################################################################
##
##	<#GAPDoc Label="InnerAutomorphismOfSemigroup">
##	<ManSection>
##	<Oper Name="InnerAutomorphismOfSemigroup" Arg="S, perm"/>
##	<Oper Name="InnerAutomorphismOfSemigroupNC" Arg="S, perm"/>
##	<Description>
##	returns the inner automorphism of the transformation semigroup <C>S</C> 
##	given by the permutation <C>perm</C>.  The degree of <C>perm</C> should be 
##	at most the degree of <C>S</C>.<P/>
##
##	The notion of inner automorphisms of semigroups differs from the 
##	notion of the same name for groups. Indeed, if <C>S</C> is a semigroup of 
##	transformations of degree <C>n</C>, then <C>g</C> in the symmetric group 
##	<C>S_n</C> induces an 
##	inner automorphism of <C>S</C> if the mapping that takes <C>s</C> to 
##	<C>g^-1sg</C> for all <C>s</C> in <C>S</C> is an automorphism of 
##	<C>S</C>.<P/>
##
##	<C>InnerAutomorphismOfSemigroup</C> checks that the mapping induced by 
##	<C>perm</C> is an automorphism and <C>InnerAutomorphismOfSemigroupNC</C> 
##	only creates the appropriate object without performing a check that the 
##	permutation actually induces an automorphism.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 6, 2, 7, 5, 3, 5, 4 ] ), 
##  &gt; Transformation( [ 7, 7, 5, 7, 2, 4, 3 ] ) ];;
##  gap&gt; S:=Monoid(gens);;
##  gap&gt; InnerAutomorphismOfSemigroup(S, (1,2,3,4,5));  
##  fail
##  gap&gt; InnerAutomorphismOfSemigroupNC(S, (1,2,3,4,5));
##  ^(1,2,3,4,5)
##  gap&gt; InnerAutomorphismOfSemigroup(S, ());
##  ^()
##	</Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("InnerAutomorphismOfSemigroupNC", [IsTransformationSemigroup, IsPerm]);
DeclareOperation("InnerAutomorphismOfSemigroup", [IsTransformationSemigroup, IsPerm]);

###########################################################################
##
##	<#GAPDoc Label="ConjugatorOfInnerAutomorphismOfSemigroup">
##	<ManSection>
##	<Attr Name="ConjugatorOfInnerAutomorphismOfSemigroup" Arg="f"/>
##	<Description>
##	returns the permutation <C>perm</C> used to construct the inner automorphism 
##	<C>f</C> of a semigroup; see <Ref Oper="InnerAutomorphismOfSemigroup"/> for 
##	further details.
##
##	<Example> 
##  gap&gt; S:=RandomSemigroup(3,8);;
##  gap&gt; f:=InnerAutomorphismOfSemigroupNC(S, (1,2)(3,4));
##  ^(1,2)(3,4)
##  gap&gt; ConjugatorOfInnerAutomorphismOfSemigroup(f);
##  (1,2)(3,4)
##	</Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("ConjugatorOfInnerAutomorphismOfSemigroup", IsInnerAutomorphismOfSemigroup);