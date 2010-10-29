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

# new for 4.0!
#############################################################################
# undocumented...

DeclareGlobalFunction("HashFunctionForTransformation");

#############################################################################
##
##	<#GAPDoc Label="GradedImagesOfTransSemigroup">
##	<ManSection>
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
##	See also <Ref Func="ImagesOfTransSemigroup"/> and 
##	<Ref Func="KernelsOfTransSemigroup"/>.
##	<Example>
##  gap> s:=Semigroup(Transformation( [ 1, 5, 1, 1, 1 ] ), 
##  > Transformation( [ 4, 4, 5, 2, 2 ] ));;
##  gap> GradedImagesOfTransSemigroup(s);
##  [ [ [ 1 ], [ 4 ], [ 2 ], [ 5 ] ], [ [ 1, 5 ], [ 2, 4 ] ], [ [ 2, 4, 5 ] ], 
##    [  ], [  ] ]
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
DeclareGlobalFunction("HashTableForKernels");

###########################################################################
##	<#GAPDoc Label="ImagesOfTransSemigroup">
##	<ManSection>
##	<Attr Name="ImagesOfTransSemigroup" Arg="S[,n]"/>
##	<Attr Name="KernelsOfTransSemigroup" Arg="S[, n]"/>
##	<Description>
##	<C>ImagesOfTransSemigroup</C> returns the <Package>orb</Package> package 
##	<C>Orb</O> object:
##	<Log>
##	Orb(S, [1..Degree(S)], OnSets);
##	</Log>
##	<P/>
##	
##	<C>KernelsOfTransSemigroup</C> returns the <Package>orb</Package> package 
##	<C>Orb</O> object:
##	<Log>
##	Orb(S, [[1],..,[Degree(S)]], OnKernelsAntiAction);
##	</Log>
##	<P/>
##
##	If the optional second argument <C>n</C> (a positive integer) is present, 
##	then the 
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

DeclareAttribute("ImagesOfTransSemigroup", IsTransformationSemigroup);
DeclareAttribute("KernelsOfTransSemigroup", IsTransformationSemigroup);

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
##	</Example>
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
##	<#GAPDoc Label="KernelsOfTransSemigroup">
##	<ManSection>

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







#JDM new for 3.2!
DeclareInfoClass("InfoMonoidOrbits");

###########################################################################
##
##	<#GAPDoc Label="StrongOrbitsInForwardOrbit">
##	<ManSection>
##	<Oper Name="StrongOrbitsInForwardOrbit" Arg="s, x, act"/>
##	<Description>
##	returns a list of the strong orbits contained in the orbit of <C>x</C> under  
##	the action <C>act</C> of the transformation collection <C>s</C>.<P/>
##
##	<Example>
##  gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ), 
##  > Transformation( [ 2, 4, 1, 2 ] ), Transformation( [ 3, 1, 1, 3 ] ),
##  > Transformation( [ 3, 3, 4, 1 ] ) ];;
##  gap> s:=Semigroup(gens);;
##  gap> StrongOrbitsInForwardOrbit(s, [1,3,4], OnSets);
##  [ [ [ 3 ], [ 1 ], [ 2 ], [ 4 ] ], [ [ 1, 4 ], [ 1, 3 ], [ 1, 2 ], 
##  [ 2, 4 ], [ 3, 4 ] ], [ [ 1, 3, 4 ] ] ]
##	</Example>
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

#JDM new for 3.1.4!
DeclareGlobalFunction("StrongOrbitsInForwardOrbit");

