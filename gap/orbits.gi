##
## orbits.gi
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
##	<Ref Func="OnTuples" BookName="ref"/>, or other actions defined in 
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
##	<Cite Key="computing"/>.
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

InstallMethod(MonoidOrbit, "for an arbitrary object and action", true, [IsTransformationSemigroup, IsObject, IsFunction], 0, 
function(M, pt, action)
local orbit, gens, x, y, new;
   	
orbit:= [pt];  
gens:= GeneratorsOfSemigroup(M);
	
for x in orbit do
  for y in gens do
    new:= action(x,y);
    if not new in orbit then
      Add(orbit, new);
    fi;
  od;
od;

return orbit;

end);

#####################

InstallOtherMethod(MonoidOrbit, "for an integer, set of integers, or set of sets of integers", true, [IsTransformationSemigroup, IsObject], 0, 
function(M, pt)

if IsPosInt(pt) then
   return MonoidOrbit(M, pt, OnPoints);
elif IsCyclotomicCollection(pt) then
   return MonoidOrbit(M, pt, OnSets);
elif IsCyclotomicCollColl(pt) then 
   return MonoidOrbit(M, pt, OnSetsSets);
fi;

end);

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
##	<Cite Key="computing"/>.
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

InstallMethod(MonoidOrbits, "for an arbitrary list and action", true, [IsTransformationSemigroup, IsList, IsFunction], 0, 
function(M, objt, action)

return List(objt, x->MonoidOrbit(M, x, action));

end);

#####################

InstallOtherMethod(MonoidOrbits, "for a list of integers and OnPoints", true, [IsTransformationSemigroup, IsCyclotomicCollection], 0, 
function(M, list)

return MonoidOrbits(M, list, OnPoints);

end);

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
##	(Recall that a <E>grading</E> is a function <M>f</M> from a transformation 
##	semigroup <M>S</M> of degree <M>n</M> to the natural 
##	numbers such that if <M>s\in S</M> and <M>X</M> is a subset of 
##	<M>\{1,\ldots, n\}</M>, then <M>(Xs)f\leq (X)f</M>, that is the value of 
##	<M>Xs</M> under <M>f</M> is not greater than the value of <M>X</M> under 
##	<M>f</M>.)<P/>
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
##	<Cite Key="computing"/>.
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

InstallMethod(GradedOrbit, "for an arbitary object, action, and grading", true, [IsTransformationSemigroup, IsObject, IsFunction, IsFunction], 0, 
function(M, obj, action, grad)
local orbit, gens, gradorbit, x, y, z, new, g, n;
 
n:=grad(obj);
gens:= GeneratorsOfSemigroup(M);
gradorbit:=List([1..n], x->[]);
Add(gradorbit[n], obj);

for x in [n, n-1..1] do
  for y in gradorbit[x] do
    for z in gens do
      new:= action(y, z);
      g:= grad(new);
      if not new in gradorbit[g] then   
        Add(gradorbit[g], new);
      fi;
    od;
  od;
od;

return gradorbit;

end);

#####################

InstallOtherMethod(GradedOrbit, "for an integer, set of integers, or set of sets of integers", true, [IsTransformationSemigroup, IsObject, IsFunction], 0, 
function(M, obj, grad)

if IsPosInt(obj) then
   return GradedOrbit(M, obj, OnPoints, grad);
elif IsCyclotomicCollection(obj) then
   return GradedOrbit(M, obj, OnSets, grad);
elif IsCyclotomicCollColl(obj) then
   return GradedOrbit(M, obj, OnSetsSets, grad);
fi;

end);

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
##	(Recall that a <E>grading</E> is a function <M>f</M> from a transformation 
##	semigroup <M>S</M> of degree <M>n</M> to the natural 
##	numbers such that if <M>s\in S</M> and <M>X</M> is a subset of 
##	<M>\{1,\ldots, n\}</M>, then <M>(Xs)f\leq (X)f</M>, that is the value of 
##	<M>Xs</M> under <M>f</M> is not greater than the value of <M>X</M> under 
##	<M>f</M>.)<P/>
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
##	<Cite Key="computing"/>.
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

InstallMethod(ShortOrbit, "for an arbitary object, action, and grading", true, [IsTransformationSemigroup, IsObject, IsFunction, IsFunction], 0, 

function(M, obj, action, grad)
local orbit, n, gens, x, y, new;

orbit:= [obj];  n:= grad(obj);
gens:=GeneratorsOfSemigroup(M);

for x in orbit do
   for y in gens do
      new:= action(x,y);
      if n=grad(new)  and not new in orbit then
         Add(orbit, new);
      fi;
   od;
od;

return orbit;
end );

#####################

InstallOtherMethod(ShortOrbit, "for an integer, set of integers, or set of sets of integers", true, [IsTransformationSemigroup, IsObject, IsFunction], 0, 

function(M, obj, grad)

if IsPosInt(obj) then 
   return ShortOrbit(M, obj, OnPoints, grad);
elif IsCyclotomicCollection(obj) then
   return ShortOrbit(M, obj, OnSets, grad);
elif IsCyclotomicCollColl(obj) then
   return ShortOrbit(M, obj, OnSetsSets, grad);
fi;

end);

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
##	<Ref Func="OnTuples" BookName="ref"/>, or other actions defined in 
##	<Ref Sect="Basic Actions" BookName="ref"/>. The argument <C>act</C> can be 
##	any function.<P/>
##
##	If the optional third argument <C>act</C> is not given and <C>obj</C> is a 
##	point, then <Ref Func="OnPoints" BookName="ref"/> is the default action.<P/>
##
##	Further details can be found in Algorithm A and B of 
##	<Cite Key="computing"/>.
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

InstallMethod(StrongOrbit, "for arbitrary object and action", true, [IsTransformationSemigroup, IsObject, IsFunction], 0,  

function(M, obj, action)

local orbit, i, j, back, s, pnt, new, a, n;

if IsPosInt(obj) and action=OnPoints then 
   return StrongOrbit(M, obj);
else
   # initialize.
   orbit:= [obj];  i:= 0;  back:= [[]]; 

   # form the (weak, but graded) orbit.
   for pnt in orbit do

      # keep track of position of 'pnt'.
      i:= i+1;

      # loop over the generators.
      for s in GeneratorsOfMonoid(M) do
         new:=action(pnt,s);
         j:= Position(orbit, new);
   
         # install new point, if necessary.
         if j = fail then
            Add(orbit, new);  Add(back, []);
            j:= Length(orbit);
         fi;
   
         # remember predecessor.
         AddSet(back[j], i);
      od;
   od;

   # form the transitive closure.
   n:= Length(orbit);
   for j in [1..n] do
      for i in [1..n] do
         if j in back[i] then
            UniteSet(back[i], back[j]);
         fi;
      od;
   od; #JDM make use of STRONGLY_CONNECTED_COMPONENTS_DIGRAPH here?

   # return predecessors of point 1.
   AddSet(back[1], 1);
   return orbit{back[1]};
fi;

end );

#####################

InstallOtherMethod(StrongOrbit, "for a positive integer", true, [IsTransformationSemigroup, IsPosInt], 0,  
function(M, int)
local cone, gens, cones, x; 

cone:=MonoidOrbit(M, int);
gens:=GeneratorsOfSemigroup(M);

cones:=[];
cones[int]:=cone;

for x in [1..DegreeOfTransformationSemigroup(M)] do 
   if x in cone then 
      cones[x]:=MonoidOrbit(M, x); 
   else 
      cones[x]:=[];
   fi;
od;

#JDM cones:=List(cone, x-> MonoidOrbit(M,x));

#JDM better way?
return First(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(cones), x-> int in x);

end);

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
##	<Cite Key="computing"/>.
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

InstallMethod(StrongOrbits, "for arbitrary object and action", true, [IsTransformationSemigroup, IsObject, IsFunction], 0,   

function(M, obj, action)

local orbits, x;
 
orbits:= [];

for x in obj do 
   Add(orbits, StrongOrbit(M, x, action));
od;
#JDM orbits:=List(obj, x-> StrongOrbit(M,x));

# return list of orbits.
return orbits;

end );


InstallOtherMethod(StrongOrbits, "for arbitrary set of sets and action", true, [IsTransformationSemigroup, IsSet, IsFunction], 0,   

function(M, set, action)

local orbit, orbits;
 
orbits:= [];

while set <> [] do
   orbit:=StrongOrbit(M, set[1], action);
   Add(orbits, orbit); 
   SubtractSet(set, orbit);
od;

# return list of orbits.
return orbits;

end );

#####################

InstallOtherMethod(StrongOrbits, "for a list of integers under OnPoints", true, [IsTransformationSemigroup, IsCyclotomicCollection], 0,  
function(M, ints)
local gens, cones, x;

if not ForAll(ints, x-> IsPosInt(x) and x<DegreeOfTransformationSemigroup(M)+1) then 
   return fail;
else
   gens:=GeneratorsOfSemigroup(M);
   cones:=[];

   for x in [1..DegreeOfTransformationSemigroup(M)] do 
      if x in ints then 
         cones[x]:=MonoidOrbit(M, x);
      else 
         cones[x]:=[];
      fi;
   od;

#JDM cones:=List(cone, x-> MonoidOrbit(M,x));

#JDM better way?
return Filtered(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(cones), x-> ForAny(ints, y-> y in x));

fi;

end);

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
##	(Recall that a <E>grading</E> is a function <M>f</M> from a transformation 
##	semigroup <M>S</M> of degree <M>n</M> to the natural 
##	numbers such that if <M>s\in S</M> and <M>X</M> is a subset of 
##	<M>\{1,\ldots, n\}</M>, then <M>(Xs)f\leq (X)f</M>, that is the value of 
##	<M>Xs</M> under <M>f</M> is not greater than the value of <M>X</M> under 
##	<M>f</M>.)<P/>
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
##	<Cite Key="computing"/>.
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

InstallMethod( GradedStrongOrbit, "for an arbitrary object, action, and grading", true, [IsTransformationSemigroup, IsObject, IsFunction, IsFunction], 0,  
function(M, obj, action, grad)

local orbit;

orbit:=StrongOrbit(M, obj, action);

#JDM better to do it online than to filter
return Filtered(List([1..grad(obj)], x-> Filtered(orbit, y-> grad(y)=x)), x-> not x=[]);
end );

#####################

InstallOtherMethod( GradedStrongOrbit, "for a positive integer, OnPoints, and arbitrary grading", true, [IsTransformationSemigroup, IsPosInt, IsFunction], 0,  
function(M, int, grad)
return GradedStrongOrbit(M, int, OnPoints, grad);
end);

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
##	(Recall that a <E>grading</E> is a function <M>f</M> from a transformation 
##	semigroup <M>S</M> of degree <M>n</M> to the natural 
##	numbers such that if <M>s\in S</M> and <M>X</M> is a subset of 
##	<M>\{1,\ldots, n\}</M>, then <M>(Xs)f\leq (X)f</M>, that is the value of 
##	<M>Xs</M> under <M>f</M> is not greater than the value of <M>X</M> under 
##	<M>f</M>.)<P/>
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
##	<Cite Key="computing"/>.
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

##  JDM is this valid?? NO!
##

InstallMethod(ShortStrongOrbit,  "for arbitrary object and action", true, [IsTransformationSemigroup, IsObject, IsFunction, IsFunction], 0,  

function(M, obj, action, grad)

return First(GradedStrongOrbit(M, obj, action, grad), x-> obj in x);

end);

InstallOtherMethod(ShortStrongOrbit, "for a positive integer", true, [IsTransformationSemigroup, IsPosInt, IsFunction], 0,  
function(M, int, grad)
local cone, gens, cones, x, n;

cone:=MonoidOrbit(M, int);
gens:=GeneratorsOfSemigroup(M);

cones:=[];
cones[int]:=cone;

for x in [1..DegreeOfTransformationSemigroup(M)] do 
   if x in cone then 
      cones[x]:=MonoidOrbit(M, x);
   else 
      cones[x]:=[];
   fi;
od;

n:=grad(int);

#JDM better way? and advantage?
return Filtered(First(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(cones), x-> int in x), y-> grad(y)=n);

end);