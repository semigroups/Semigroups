#############################################################################
##
#W  standard/greens/greens-inverse.tst
#Y  Copyright (C) 2015                                   Wilfred A. Wilson
##                                                       
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/greens/greens-inverse.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#T# greens-inverse: SchutzenbergerGroup, for an L-class of an inverse op sgp
gap> S := InverseSemigroup([
>  Bipartition([[1, -4], [2, -2], [3, -3], [4, 5, -1, -5]]),
>  Bipartition([[1, -1], [2, -3], [3, 5, -4, -5], [4, -2]]),
>  Bipartition([[1, -4], [2, -1], [3, 5, -3, -5], [4, -2]])],
>  rec(generic := false));;
gap> x := Bipartition([[1, -2], [2, -4], [3, 4, 5, -1, -3, -5]]);;
gap> L := LClass(S, x);
<Green's L-class: <block bijection: [ 1, 4, 5, -1, -3, -5 ], [ 2, -4 ], 
  [ 3, -2 ]>>
gap> x in L;
true
gap> SchutzenbergerGroup(L);
Group([ (2,3) ])
gap> L := GreensLClassOfElementNC(S, x, true);;
gap> x in L;
true
gap> SchutzenbergerGroup(L);
Group([ (2,3) ])

#T# greens-inverse: DClassOfXClass, for an X=R/L/H-class
gap> S := InverseSemigroup([
>  Bipartition([[1, -4], [2, -2], [3, -3], [4, 5, -1, -5]]),
>  Bipartition([[1, -1], [2, -3], [3, 5, -4, -5], [4, -2]]),
>  Bipartition([[1, -4], [2, -1], [3, 5, -3, -5], [4, -2]])],
>  rec(generic := false));;
gap> x := Bipartition([[1, -1], [2, -4], [3, -2], [4, 5, -3, -5]]);;
gap> D := DClassOfLClass(LClass(S, x));
<Green's D-class: <block bijection: [ 1, 5, -1, -5 ], [ 2, -2 ], [ 3, -3 ], 
  [ 4, -4 ]>>
gap> x in D;
true
gap> D = DClass(S, x);
true
gap> DClassOfRClass(RClass(S, x));
<Green's D-class: <block bijection: [ 1, 5, -1, -5 ], [ 2, -2 ], [ 3, -3 ], 
  [ 4, -4 ]>>
gap> x in D;
true
gap> D = DClass(S, x);
true
gap> DClassOfHClass(HClass(S, x));
<Green's D-class: <block bijection: [ 1, 5, -1, -5 ], [ 2, -2 ], [ 3, -3 ], 
  [ 4, -4 ]>>
gap> x in D;
true
gap> D = DClass(S, x);
true

#T# greens-inverse: LClassOfHClass, for an H-class
gap> S := InverseSemigroup([
>  Bipartition([[1, -4], [2, -2], [3, -3], [4, 5, -1, -5]]),
>  Bipartition([[1, -1], [2, -3], [3, 5, -4, -5], [4, -2]]),
>  Bipartition([[1, -4], [2, -1], [3, 5, -3, -5], [4, -2]])],
>  rec(generic := false));;
gap> x := Bipartition([[1, -4], [2, 3, 5, -2, -3, -5], [4, -1]]);;
gap> L := LClassOfHClass(HClass(S, x));
<Green's L-class: <block bijection: [ 1, 4, 5, -2, -3, -5 ], [ 2, -4 ], 
  [ 3, -1 ]>>
gap> L = LClass(S, x);
true

#T# greens-inverse: GreensHClassOfElement, for a Green's class and an element
gap> S := InverseSemigroup([
> PartialPerm([1, 2, 3, 4], [4, 1, 2, 6]),
> PartialPerm([1, 2, 4], [4, 6, 3]),
> PartialPerm([1, 2, 3, 5], [6, 4, 5, 1])],
> rec(generic := false));;
gap> x := PartialPerm([4, 1, 2, 0]);;
gap> GreensHClassOfElement(DClass(S, x), x);
<Green's H-class: [3,2,1,4]>
gap> GreensHClassOfElement(RClass(S, x), x);
<Green's H-class: [3,2,1,4]>
gap> GreensHClassOfElement(LClass(S, x), x);
<Green's H-class: [3,2,1,4]>

#T# greens-inverse: \in, for a D-class and an element, 1
gap> S := InverseSemigroup([
>  Bipartition([[1, -4], [2, -2], [3, -3], [4, 5, -1, -5]]),
>  Bipartition([[1, -1], [2, -3], [3, 5, -4, -5], [4, -2]]),
>  Bipartition([[1, -4], [2, -1], [3, 5, -3, -5], [4, -2]])],
>  rec(generic := false));;
gap> x := Bipartition([[1, -4], [2, 3, 5, -2, -3, -5], [4, -1]]);;
gap> D := DClass(S, x);;
gap> PartialPerm([]) in D;
false
gap> Bipartition([[1, -4], [2, 3, 5, -2, -3, -5], [4, -1], [6, -6]]) in D;
false
gap> Bipartition([[1, -4], [2, 3, 5, -2, -3, -5, 4, -1]]) in D;
false
gap> Bipartition([[1, -4], [2, 3, 5, -2], [-3, -5, 4, -1]]) in D;
false
gap> Bipartition([[1], [2, -3], [3, -4], [4, -5], [5], [-1], [-2]]) in D;
false
gap> Bipartition([[1, 2, 5, -1, -2, -3], [3, -4], [4, -5]]) in D;
false

#T# greens-inverse: \in, for a D-class and an element, 2
gap> S := InverseSemigroup([
>   PartialPerm([1, 2, 3, 5, 6, 7], [5, 7, 1, 9, 4, 2]),
>   PartialPerm([1, 2, 3, 6, 8], [2, 6, 7, 9, 1]), 
>   PartialPerm([1, 2, 3, 4, 5, 8], [7, 1, 4, 3, 2, 6]), 
>   PartialPerm([1, 2, 3, 4, 5, 7, 9], [5, 3, 8, 1, 9, 4, 6])]);;
gap> x := PartialPerm([2, 4, 5, 7], [4, 1, 6, 7]);;
gap> D := DClass(S, x);;
gap> PartialPerm([2, 4, 5, 7], [2, 4, 7, 5]) in D;
false

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(D);
gap> Unbind(R);
gap> Unbind(L);
gap> Unbind(H);
gap> Unbind(x);

#E# 
gap> STOP_TEST("Semigroups package: standard/greens/greens-inverse.tst");
