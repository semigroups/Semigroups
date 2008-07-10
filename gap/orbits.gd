##
## orbits.gd
## Version 3.1.2
## Thu 10 Jul 2008 20:25:38 BST
##

###########################################################################
##
##	<#GAPDoc Label="MonoidOrbit">
##	<ManSection>
##	<Oper Name="MonoidOrbit" Arg="S, obj[, act]"/>
##	<Description>
##	returns the orbit of <C>obj</C> under the action <C>act</C> of the 
##	transformation semigroup <C>S</C>. Usually, <C>obj</C> would be a point, 
##	list of points, or list of lists, and <C>act</C> would be 
##	<Ref Func="OnPoints" BookName="ref"/>, <Ref Func="OnSets" BookName="ref"/>, 
##	<Ref Func="OnTuples" BookName="ref"/>, or another action defined in 
##	<Ref Sect="Basic Actions" BookName="ref"/>. The argument <C>act</C> can be 
##	any function.<P/>
##
##	If the optional third argument 
##	<C>act</C> is not given, then <Ref Func="OnPoints" BookName="ref"/>, 
##	<Ref Func="OnSets" BookName="ref"/>, or 
##	<Ref Func="OnSetsSets" BookName="ref"/> is used as the default action 
##	depending on what <C>obj</C> is.<P/>
##
##	Further details can be found in Algorithm A and B of 
##	<Cite Key="pfeiffer2"/>.
##	<Example>
##  gap&gt; g1:=Transformation([3,3,2,6,2,4,4,6,3,4,6]);;
##  gap&gt; g2:=Transformation([4,4,6,1,3,3,3,3,11,11,11]);;
##  gap&gt; g3:=Transformation([2,2,3,4,4,6,6,6,6,6,11]);;
##  gap&gt; S:=Monoid(g1,g2,g3);;
##  gap&gt; MonoidOrbit(S, 1);
##  [ 1, 3, 4, 2, 6 ]
##  gap&gt; MonoidOrbit(S, [1,2], OnSets);
##  [ [ 1, 2 ], [ 3 ], [ 4 ], [ 2 ], [ 6 ], [ 1 ] ]
##  gap&gt; MonoidOrbit(S, [1,2], OnTuples);
##  [ [ 1, 2 ], [ 3, 3 ], [ 4, 4 ], [ 2, 2 ], [ 6, 6 ], [ 1, 1 ] ]
##  gap&gt; MonoidOrbit(S, 2, OnPoints);
##  [ 2, 3, 4, 6, 1 ]
##	</Example> <!-- orbits.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("MonoidOrbit", [IsTransformationSemigroup, IsObject, IsFunction]);

###########################################################################
##
##	<#GAPDoc Label="MonoidOrbits">
##	<ManSection>
##	<Oper Name="MonoidOrbits" Arg="S, list[, act]"/>
##	<Description>
##	returns a list of the orbits of the elements of <C>list</C> under the action 
##	<C>act</C> of the transformation semigroup <C>S</C> using the 
##	<Ref Oper="MonoidOrbit"/> function.<P/>
##
##	If the optional third argument 
##	<C>act</C> is not given, then <Ref Func="OnPoints" BookName="ref"/>, 
##	<Ref Func="OnSets" BookName="ref"/>, or 
##	<Ref Func="OnSetsSets" BookName="ref"/> is used as the default action 
##	depending on what <C>obj</C> is.<P/>
##
##	Further details can be found in Algorithm A and B of 
##	<Cite Key="pfeiffer2"/>.
##	<Example>
##  gap&gt; g1:=Transformation([3,3,2,6,2,4,4,6,3,4,6]);;
##  gap&gt; g2:=Transformation([4,4,6,1,3,3,3,3,11,11,11]);;
##  gap&gt; g3:=Transformation([2,2,3,4,4,6,6,6,6,6,11]);;
##  gap&gt; S:=Monoid(g1,g2,g3);;
##  gap&gt; MonoidOrbits(S, [1,2]);
##  [ [ 1, 3, 4, 2, 6 ], [ 2, 3, 4, 6, 1 ] ]
##  gap&gt; MonoidOrbits(S, [[1,2], [2,3]], OnSets);
##  [ [ [ 1, 2 ], [ 3 ], [ 4 ], [ 2 ], [ 6 ], [ 1 ] ], 
##    [ [ 2, 3 ], [ 4, 6 ], [ 1, 3 ] ] ]
##	</Example> <!-- orbits.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("MonoidOrbits", [IsTransformationSemigroup, IsList, IsFunction]);

###########################################################################
##
##	<#GAPDoc Label="GradedOrbit">
##	<ManSection>
##	<Oper Name="GradedOrbit" Arg="S, obj[, act], grad"/>
##	<Description>
##	returns the orbit of <C>obj</C> under the action 
##	<C>act</C> of the transformation semigroup <C>S</C> partitioned by the 
##	grading <C>grad</C>. That is, two elements lie in the same class if they 
##	have the same value under <C>grad</C>.<P/>
##	
##	(Recall that a <E>grading</E> is a function <C>f</C> from a transformation 
##	semigroup <C>S</C> of degree <C>n</C> to the natural 
##	numbers such that if <C>s</C> in <C>S</C> and <C>X</C> is a subset of 
##	<C>{1,...,n}</C>, then <C>(Xs)f</C> is at most <C>(X)f</C>. )<P/>
##	
##	Note that this function will not check if <C>grad</C> actually defines a 
##	grading or not.<P/>
##	
##	If the optional third argument 
##	<C>act</C> is not given, then <Ref Func="OnPoints" BookName="ref"/>, 
##	<Ref Func="OnSets" BookName="ref"/>, or 
##	<Ref Func="OnSetsSets" BookName="ref"/> is used as the default action 
##	depending on what <C>obj</C> is.<P/>
##
##	Further details can be found in Algorithm A and B of 
##	<Cite Key="pfeiffer2"/>.
##	<Example>
##  gap&gt; g1:=Transformation([3,3,2,6,2,4,4,6,3,4,6]);;
##  gap&gt; g2:=Transformation([4,4,6,1,3,3,3,3,11,11,11]);;
##  gap&gt; g3:=Transformation([2,2,3,4,4,6,6,6,6,6,11]);;
##  gap&gt; S:=Monoid(g1,g2,g3);;
##  gap&gt; GradedOrbit(S, [1,2], OnSets, Size);
##  [ [ [ 3 ], [ 4 ], [ 2 ], [ 6 ], [ 1 ] ], [ [ 1, 2 ] ] ]
##  gap&gt; GradedOrbit(S, [1,2], Size);
##  [ [ [ 3 ], [ 4 ], [ 2 ], [ 6 ], [ 1 ] ], [ [ 1, 2 ] ] ]
##  gap&gt; GradedOrbit(S, [1,3,4], OnTuples, function(x)
##  &gt; if 1 in x then return 2;
##  &gt; else return 1;
##  &gt; fi;
##  &gt; end); 
##  [ [ [ 3, 2, 6 ], [ 2, 3, 4 ], [ 6, 4, 3 ], [ 4, 6, 2 ] ], 
##    [ [ 1, 3, 4 ], [ 4, 6, 1 ], [ 3, 1, 6 ] ] ]
##	</Example> <!-- orbits.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("GradedOrbit", [IsTransformationSemigroup, IsObject, IsFunction, IsFunction]);

###########################################################################
##
##	<#GAPDoc Label="ShortOrbit">
##	<ManSection>
##	<Oper Name="ShortOrbit" Arg="S, obj[, act], grad"/>
##	<Description>
##	returns the elements of the orbit of <C>obj</C> under the action 
##	<C>act</C> of the transformation semigroup <C>S</C> with the same value as 
##	<C>obj</C> under the grading <C>grad</C>.<P/>
##	
##	(Recall that a <E>grading</E> is a function <C>f</C> from a transformation 
##	semigroup <C>S</C> of degree <C>n</C> to the natural 
##	numbers such that if <C>s</C> in <C>S</C> and <C>X</C> is a subset of 
##	<C>{1,...,n}</C>, then <C>(Xs)f</C> is at most <C>(X)f</C>. )<P/>
##	
##	Note that this function will not check if <C>grad</C> actually defines a 
##	grading or not.<P/>
##	
##	If the optional third argument 
##	<C>act</C> is not given, then <Ref Func="OnPoints" BookName="ref"/>, 
##	<Ref Func="OnSets" BookName="ref"/>, or 
##	<Ref Func="OnSetsSets" BookName="ref"/> is used as the default action 
##	depending on what <C>obj</C> is.<P/>
##
##	Further details can be found in Algorithm A and B of 
##	<Cite Key="pfeiffer2"/>.
##	<Example>
##  gap&gt; g1:=Transformation([3,3,2,6,2,4,4,6,3,4,6]);;
##  gap&gt; g2:=Transformation([4,4,6,1,3,3,3,3,11,11,11]);;
##  gap&gt; g3:=Transformation([2,2,3,4,4,6,6,6,6,6,11]);;
##  gap&gt; S:=Monoid(g1,g2,g3);;
##  gap&gt; ShortOrbit(S, [1,2], Size);
##  [ [ 1, 2 ] ]
##  gap&gt; ShortOrbit(S, [2,4], Size);
##  [ [ 2, 4 ], [ 3, 6 ], [ 1, 4 ] ]
##  gap&gt; ShortOrbit(S, [1,2], OnTuples, Size);
##  [ [ 1, 2 ], [ 3, 3 ], [ 4, 4 ], [ 2, 2 ], [ 6, 6 ], [ 1, 1 ] ]
##	</Example> <!-- orbits.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("ShortOrbit", [IsTransformationSemigroup, IsObject, IsFunction, IsFunction]);

###########################################################################
##
##	<#GAPDoc Label="StrongOrbit">
##	<ManSection>
##	<Oper Name="StrongOrbit" Arg="S, obj[, act]"/>
##	<Description>
##	returns the strong orbit of <C>obj</C> under the action <C>act</C> of the 
##	transformation semigroup <C>S</C>. Usually, <C>obj</C> would be a point, 
##	list of points, or list of lists, and <C>act</C> would be 
##	<Ref Func="OnPoints" BookName="ref"/>, <Ref Func="OnSets" BookName="ref"/>, 
##	<Ref Func="OnTuples" BookName="ref"/>, or another action defined in 
##	<Ref Sect="Basic Actions" BookName="ref"/>. The argument <C>act</C> can be 
##	any function.<P/>
##
##	If the optional third argument <C>act</C> is not given and <C>obj</C> is a 
##	point, then <Ref Func="OnPoints" BookName="ref"/> is the default action.<P/>
##
##	Further details can be found in Algorithm A and B of 
##	<Cite Key="pfeiffer2"/>.
##	<Example>
##  gap&gt; g1:=Transformation([3,3,2,6,2,4,4,6,3,4,6]);;
##  gap&gt; g2:=Transformation([4,4,6,1,3,3,3,3,11,11,11]);;
##  gap&gt; g3:=Transformation([2,2,3,4,4,6,6,6,6,6,11]);;
##  gap&gt; S:=Monoid(g1,g2,g3);;
##  gap&gt; StrongOrbit(S, 4, OnPoints);
##  [ 1, 3, 2, 4, 6 ]
##  gap&gt; StrongOrbit(S, 4); 
##  [ 1, 3, 2, 4, 6 ]
##  gap&gt; StrongOrbit(S, [2,3], OnSets);
##  [ [ 2, 3 ], [ 4, 6 ], [ 1, 3 ] ] 
##  gap&gt; StrongOrbit(S, [2,3], OnTuples);
##  [ [ 2, 3 ], [ 3, 2 ], [ 4, 6 ], [ 6, 4 ], [ 1, 3 ], [ 3, 1 ] ]
##	</Example> <!-- orbits.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("StrongOrbit", [IsTransformationSemigroup, IsObject, IsFunction]);

###########################################################################
##
##	<#GAPDoc Label="StrongOrbits">
##	<ManSection>
##	<Oper Name="StrongOrbits" Arg="S, obj[, act]"/>
##	<Description>
##	returns a list of the strong orbits of the elements of <C>list</C> under the 
##	action <C>act</C> of the transformation semigroup <C>S</C> using the 
##	<Ref Oper="StrongOrbit"/> function.<P/>
##
##	If the optional third argument 
##	<C>act</C> is not given, then <Ref Func="OnPoints" BookName="ref"/>, 
##	<Ref Func="OnSets" BookName="ref"/>, or 
##	<Ref Func="OnSetsSets" BookName="ref"/> is used as the default action 
##	depending on what <C>obj</C> is.<P/>
##
##	Further details can be found in Algorithm A and B of 
##	<Cite Key="pfeiffer2"/>.
##	<Example>
##  gap&gt; g1:=Transformation([3,3,2,6,2,4,4,6,3,4,6]);;
##  gap&gt; g2:=Transformation([4,4,6,1,3,3,3,3,11,11,11]);;
##  gap&gt; g3:=Transformation([2,2,3,4,4,6,6,6,6,6,11]);;
##  gap&gt; S:=Monoid(g1,g2,g3);;
##  gap&gt; StrongOrbits(S, [1..6]);
##  [ [ 1, 3, 2, 4, 6 ], [ 5 ] ]
##  gap&gt; StrongOrbits(S, [[1,2],[2,3]], OnSets);
##  [ [ [ 1, 2 ] ], [ [ 2, 3 ], [ 4, 6 ], [ 1, 3 ] ] ]
##  gap&gt; StrongOrbits(S, [[1,2],[2,3]], OnTuples);
##  [ [ [ 1, 2 ] ], [ [ 2, 3 ], [ 3, 2 ], [ 4, 6 ], [ 6, 4 ], 
##    [ 1, 3 ], [ 3, 1 ] ] ]
##	</Example> <!-- orbits.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("StrongOrbits", [IsTransformationSemigroup, IsObject, IsFunction]);

###########################################################################
##
##	<#GAPDoc Label="GradedStrongOrbit">
##	<ManSection>
##	<Oper Name="GradedStrongOrbit" Arg="S, obj[, act], grad"/>
##	<Description>
##	returns the strong orbit of <C>obj</C> under the action 
##	<C>act</C> of the transformation semigroup <C>S</C> partitioned by the 
##	grading <C>grad</C>. That is, two elements lie in the same class if they 
##	have the same value under <C>grad</C>.<P/>
##	
##	(Recall that a <E>grading</E> is a function <C>f</C> from a transformation 
##	semigroup <C>S</C> of degree <C>n</C> to the natural 
##	numbers such that if <C>s</C> in <C>S</C> and <C>X</C> is a subset of 
##	<C>{1,...,n}</C>, then <C>(Xs)f</C> is at most <C>(X)f</C>. )<P/>
##	
##	Note that this function will not check if <C>grad</C> actually defines a 
##	grading or not.<P/>
##	
##	If the optional third argument 
##	<C>act</C> is not given, then <Ref Func="OnPoints" BookName="ref"/>, 
##	<Ref Func="OnSets" BookName="ref"/>, or 
##	<Ref Func="OnSetsSets" BookName="ref"/> is used as the default action 
##	depending on what <C>obj</C> is.<P/>
##
##	Further details can be found in Algorithm A and B of 
##	<Cite Key="pfeiffer2"/>.
##	<Example>
##  gap&gt; g1:=Transformation([3,3,2,6,2,4,4,6,3,4,6]);;
##  gap&gt; g2:=Transformation([4,4,6,1,3,3,3,3,11,11,11]);;
##  gap&gt; g3:=Transformation([2,2,3,4,4,6,6,6,6,6,11]);;
##  gap&gt; S:=Monoid(g1,g2,g3);;
##  gap&gt; GradedStrongOrbit(S, [1,3,4], OnTuples, function(x)
##  &gt; if 1 in x then return 2; else return 1; fi; end);
##  [ [ [ 3, 2, 6 ], [ 2, 3, 4 ], [ 6, 4, 3 ], [ 4, 6, 2 ] ], 
##    [ [ 1, 3, 4 ], [ 4, 6, 1 ], [ 3, 1, 6 ] ] ]
##  gap&gt; GradedStrongOrbit(S, [1,3,4], OnTuples, Size);
##  [ [ [ 1, 3, 4 ], [ 3, 2, 6 ], [ 4, 6, 1 ], [ 2, 3, 4 ], [ 6, 4, 3 ], 
##    [ 4, 6, 2 ], [ 3, 1, 6 ] ] ]
##	</Example> <!-- orbits.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("GradedStrongOrbit", [IsTransformationSemigroup, IsObject, IsFunction, IsFunction]);

###########################################################################
##
##	<#GAPDoc Label="ShortStrongOrbit">
##	<ManSection>
##	<Oper Name="ShortStrongOrbit" Arg="S, obj[, act], grad"/>
##	<Description>
##	returns the elements of the orbit of <C>obj</C> under the action 
##	<C>act</C> of the transformation semigroup <C>S</C> with the same value as 
##	<C>obj</C> under the grading <C>grad</C>.<P/>
##	
##	(Recall that a <E>grading</E> is a function <C>f</C> from a transformation 
##	semigroup <C>S</C> of degree <C>n</C> to the natural 
##	numbers such that if <C>s</C> in <C>S</C> and <C>X</C> is a subset of 
##	<C>{1,...,n}</C>, then <C>(Xs)f</C> is at most <C>(X)f</C>. )<P/>
##	
##	Note that this function will not check if <C>grad</C> actually defines a 
##	grading or not.<P/>
##	
##	If the optional third argument 
##	<C>act</C> is not given, then <Ref Func="OnPoints" BookName="ref"/>, 
##	<Ref Func="OnSets" BookName="ref"/>, or 
##	<Ref Func="OnSetsSets" BookName="ref"/> is used as the default action 
##	depending on what <C>obj</C> is.<P/>
##
##	Further details can be found in Algorithm A and B of 
##	<Cite Key="pfeiffer2"/>.
##	<Example>
##  gap&gt; g1:=Transformation([3,3,2,6,2,4,4,6,3,4,6]);;
##  gap&gt; g2:=Transformation([4,4,6,1,3,3,3,3,11,11,11]);;
##  gap&gt; g3:=Transformation([2,2,3,4,4,6,6,6,6,6,11]);;
##  gap&gt; S:=Monoid(g1,g2,g3);;
##  gap&gt;ShortStrongOrbit(S, [1,3,4], OnTuples, function(x) 
##  &gt;  if 1 in x then return 2; else return 1; fi; end);
##  [ [ 1, 3, 4 ], [ 4, 6, 1 ], [ 3, 1, 6 ] ]
##	</Example> <!-- orbits.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareOperation("ShortStrongOrbit", [IsTransformationSemigroup, IsObject, IsFunction, IsFunction]);