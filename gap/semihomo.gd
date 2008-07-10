##
## semihomo.gd
## Version 3.1.2
## Thu 10 Jul 2008 20:25:38 BST
##

###########################################################################
##
##	<#GAPDoc Label="SemigroupHomomorphismByFunction">
##	<ManSection><Heading>SemigroupHomomorphismByFunction</Heading>
##	<Oper Name="SemigroupHomomorphismByFunction" Arg="S, T, func"/>
##	<Oper Name="SemigroupHomomorphismByFunctionNC" Arg="S, T, func"/>
##	<Description>
##	returns a semigroup homomorphism with representation
##	<C>IsSemigroupHomomorphismByFunctionRep</C> from the semigroup <C>S</C> to 
##	the semigroup <C>T</C> defined by the function <C>func</C>. <P/>
##
##	<C>SemigroupHomomorphismByFunction</C> will find an isomorphism from 
##	<C>S</C> to a finitely presented semigroup or monoid (using 
##	<Ref Oper="IsomorphismFpSemigroup"/> or <Ref Oper="IsomorphismFpMonoid"/>) 
##	and then check that the list of values under <C>func</C> of the generators 
##	of <C>S</C> satisfy the relations of this presentation. <P/>
##
##	<C>SemigroupHomomorphismByFunctionNC</C> does not check that <C>func</C> 
##	defines a homomorphism and, in this case <C>S</C> and <C>T</C> 
##	can be semigroups, <M>D</M>-classes, <M>H</M>-classes or any combination of 
##	these.<P/> 
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 1, 4, 3, 5, 2 ] ), 
##  &gt; Transformation( [ 2, 3, 1, 1, 2 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; gens:=[ Transformation( [ 1, 5, 1, 2, 1 ] ), 
##  &gt; Transformation( [ 5, 1, 4, 3, 2 ] ) ];;
##  gap&gt; T:=Semigroup(gens);;
##  gap&gt; idem:=Random(Idempotents(T));;
##  gap&gt; hom:=SemigroupHomomorphismByFunction(S, T, x-&gt; idem);
##  SemigroupHomomorphism ( &lt;semigroup with 2 generators&gt;-&gt;&lt;semigroup with 
##  2 generators&gt;)
##  gap&gt; hom:=SemigroupHomomorphismByFunctionNC(S, T, x-&gt; idem);
##  SemigroupHomomorphism ( &lt;semigroup with 2 generators&gt;-&gt;&lt;semigroup with 
##  2 generators&gt;)
##	</Example> <!-- semihomo.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("SemigroupHomomorphismByFunction", [IsSemigroup, IsSemigroup, IsFunction]);
DeclareOperation("SemigroupHomomorphismByFunctionNC", [IsSemigroup, IsSemigroup, IsFunction]);
DeclareRepresentation("IsSemigroupHomomorphismByFunctionRep", IsAttributeStoringRep, ["func"] );

###########################################################################
##
##	<#GAPDoc Label="SemigroupHomomorphismByImagesOfGens">
##	<ManSection><Heading>SemigroupHomomorphismByImagesOfGens</Heading>
##	<Oper Name="SemigroupHomomorphismByImagesOfGens" Arg="S, T, list"/>
##	<Oper Name="SemigroupHomomorphismByImagesOfGensNC" Arg="S, T, list"/>
##	<Description>
##	returns a semigroup homomorphism with representation
##	<C>IsSemigroupHomomorphismByImagesOfGensRep</C> from  <C>S</C> to 
##	<C>T</C> where the image of the <C>i</C>th generator of <C>S</C> is the 
##	<C>i</C>th position in <C>list</C>.<P/>
##
##	<C>SemigroupHomomorphismByImagesOfGens</C> will find an isomorphism from 
##	<C>S</C> to a finitely presented semigroup or monoid (using 
##	<Ref Attr="IsomorphismFpSemigroup"/> or <Ref Attr="IsomorphismFpMonoid"/>) 
##	and then check that <C>list</C> satisfies the relations of this 
##	presentation. <P/>
##
##	<C>SemigroupHomomorphismByImagesOfGensNC</C> does not check that <C>list</C> 
##	induces a homomorphism. <P/>
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 1, 4, 3, 5, 2 ] ), 
##  &gt; Transformation( [ 2, 3, 1, 1, 2 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; gens:=[ Transformation( [ 1, 5, 1, 2, 1 ] ), 
##  &gt; Transformation( [ 5, 1, 4, 3, 2 ] ) ];;
##  gap&gt; T:=Semigroup(gens);;
##  gap&gt; SemigroupHomomorphismByImagesOfGens(S, T, GeneratorsOfSemigroup(T));
##  fail
##  gap&gt; SemigroupHomomorphismByImagesOfGens(S, S, GeneratorsOfSemigroup(S));
##  SemigroupHomomorphismByImagesOfGens ( &lt;trans. semigroup of size 161 with 
##  2 generators&gt;-&gt;&lt;trans. semigroup of size 161 with 2 generators&gt;)
##	</Example> <!-- semihomo.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("SemigroupHomomorphismByImagesOfGens", [IsSemigroup, IsSemigroup, IsList]);
DeclareOperation("SemigroupHomomorphismByImagesOfGensNC", [IsSemigroup, IsSemigroup, IsList]);

DeclareRepresentation( "IsSemigroupHomomorphismByImagesOfGensRep", IsAttributeStoringRep, ["imgsgens"] );

###########################################################################
##
##	<#GAPDoc Label="SemigroupHomomorphismByImages">
##	<ManSection><Heading>SemigroupHomomorphismByImages</Heading>
##	<Oper Name="SemigroupHomomorphismByImages" Arg="S, T, list"/>
##	<Oper Name="SemigroupHomomorphismByImagesNC" Arg="S, T, list"/>
##	<Description>
##	returns a semigroup homomorphism with representation
##	<C>IsSemigroupHomomorphismByImagesRep</C> from  <C>S</C> to 
##	<C>T</C> where the image of the <C>i</C>th element of <C>S</C> is the 
##	<C>i</C>th position in <C>list</C>.<P/>
##
##	<C>SemigroupHomomorphismByImages</C> will find an isomorphism from 
##	<C>S</C> to a finitely presented semigroup or monoid (using 
##	<Ref Attr="IsomorphismFpSemigroup"/> or <Ref Attr="IsomorphismFpMonoid"/>) 
##	and then check that <C>list</C> satisfies the relations of this 
##	presentation. <P/>
##
##	<C>SemigroupHomomorphismByImagesNC</C> does not check that <C>list</C> 
##	induces a homomorphism. <P/>
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 2, 3, 4, 2, 4 ] ),
##  &gt; Transformation( [ 3, 4, 2, 1, 4 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; gens:=[ Transformation( [ 2, 4, 4, 1, 2 ] ),
##  &gt; Transformation( [ 5, 1, 1, 5, 1 ] ) ];;
##  gap&gt; T:=Semigroup(gens);;
##  gap&gt; idem:=Transformation( [ 5, 5, 5, 5, 5 ] );;
##  gap&gt; list:=List([1..Size(S)], x-&gt; idem);;
##  gap&gt; hom:=SemigroupHomomorphismByImages(S, T, list);
##  SemigroupHomomorphismByImagesOfGens ( &lt;trans. semigroup of size 164 with 
##  2 generators&gt;-&gt;&lt;trans. semigroup with 2 generators&gt;)
##  gap&gt; SemigroupHomomorphismByImagesNC(S, T, list);
##  SemigroupHomomorphismByImages ( &lt;trans. semigroup of size 164 with 
##  2 generators&gt;-&gt;&lt;trans. semigroup with 2 generators&gt;)
##	</Example> <!-- semihomo.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("SemigroupHomomorphismByImages", [IsSemigroup, IsSemigroup, IsList]);