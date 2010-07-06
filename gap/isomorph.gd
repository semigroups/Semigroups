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

DeclareAttribute("IsomorphismReesMatrixSemigroupOfDClass", IsGreensDClass and IsGreensClassOfTransSemigp);

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