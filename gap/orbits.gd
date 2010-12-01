#############################################################################
##
#W  orbits_no_orb.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

DeclareGlobalFunction("OnKernelsAntiAction2");

# new for 4.0!
#############################################################################
# undocumented...

DeclareGlobalFunction("HashFunctionForTransformation");

#############################################################################
##	<#GAPDoc Label="GradedImagesOfTransSemigroup">
##	<ManSection><Heading>Graded images and kernels</Heading>
##	<Attr Name="GradedImagesOfTransSemigroup" Arg="S"/>
##	<Attr Name="GradedKernelsOfTransSemigroup" Arg="S"/>
##	<Description>
##	<C>GradedImagesOfTransSemigroup</C> returns a list where the <C>i</C>th 
##	entry is a list of all the images of transformations in <C>S</C> with rank 
##	<C>i</C>.  
##	<P/>
##	
##	<C>GradedKernelsOfTransSemigroup</C> returns a list where the <C>i</C>th 
##	entry is a list of all the kernels of transformations in <C>S</C> with rank 
##	<C>i</C>.
##	<P/>
##	See also <Ref Attr="ImagesOfTransSemigroup"/> and 
##	<Ref Attr="KernelsOfTransSemigroup"/>.
##	<Example>
##  gap> s:=Semigroup(Transformation( [ 1, 5, 1, 1, 1 ] ), 
##  > Transformation( [ 4, 4, 5, 2, 2 ] ));;
##  gap> GradedImagesOfTransSemigroup(s);
##  [ [ [ 1 ], [ 4 ], [ 2 ], [ 5 ] ], [ [ 1, 5 ], [ 2, 4 ] ], 
##    [ [ 2, 4, 5 ] ], [  ], [  ] ]
##  gap> GradedKernelsOfTransSemigroup(s);
##  [ [ [ [ 1, 2, 3, 4, 5 ] ] ], 
##    [ [ [ 1, 3, 4, 5 ], [ 2 ] ], [ [ 1, 2, 3 ], [ 4, 5 ] ] ], 
##    [ [ [ 1, 2 ], [ 3 ], [ 4, 5 ] ] ], [  ], [  ] ]
##	</Example> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("GradedImagesOfTransSemigroup", IsTransformationSemigroup);
DeclareAttribute("GradedKernelsOfTransSemigroup", IsTransformationSemigroup);

# new for 4.0!
#############################################################################
# undocumented... creates a hash table from an image or kernel with that 
# image or kernel installed.

DeclareGlobalFunction("HashTableForImages");
DeclareGlobalFunction("HashTableForImagesFixedSize");
DeclareGlobalFunction("HashTableForKernels");

###########################################################################
##	<#GAPDoc Label="ImagesOfTransSemigroup">
##	<ManSection><Heading>Images and kernels</Heading>
##	<Attr Name="ImagesOfTransSemigroup" Arg="S[, n]"/>
##	<Attr Name="KernelsOfTransSemigroup" Arg="S[, n]"/>
##	<Description>
##	<C>ImagesOfTransSemigroup</C> returns the <Package>orb</Package> package 
##	<C>Orb</C> object:
##  <Display>
##	Orb(S, [1..Degree(S)], OnSets);
##	</Display>
##	<P/>
##	
##	<C>KernelsOfTransSemigroup</C> returns the <Package>orb</Package> package 
##	<C>Orb</C> object:
##  <Display>
##	Orb(S, [[1],..,[Degree(S)]], OnKernelsAntiAction);
##	</Display>
##	<P/>
##
##	If the optional second argument <C>n</C> (a positive integer) is present, 
##	then only the images or kernels of size at least <C>n</C> are found.<P/>
##	
##	See also <Ref Func="GradedImagesOfTransSemigroup"/> and 
##	<Ref Func="GradedKernelsOfTransSemigroup"/>.
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

DeclareAttribute("ImagesOfTransSemigroup", IsTransformationSemigroup, "mutable");
DeclareAttribute("KernelsOfTransSemigroup", IsTransformationSemigroup, "mutable");

###########################################################################
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
##	</Example>
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

DeclareGlobalFunction("OnTuplesOfSetsAntiAction");

###########################################################################
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
##	<#GAPDoc Label="StrongOrbitsInForwardOrbit">
##	<ManSection>
##	<Oper Name="StrongOrbitsInForwardOrbit" Arg="o"/>
##	<Description>
##	returns a list of the strong orbits contained in the <Package>orb</Package>
##	package orbit <C>o</C> provided that <C>o</C> was created with the 
##	option <C>orbitgraph</C> set to <C>true</C>. <P/>
##
##	<Example>
##  gap> new example required
##	</Example>
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

# new for 3.1.4!
DeclareGlobalFunction("StrongOrbitsInForwardOrbit");

