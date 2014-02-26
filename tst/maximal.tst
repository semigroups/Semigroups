#############################################################################
##
#W  maximal.tst
##  Test file for Maximal Subsemigroups related algorithms
##
#############################################################################
##
gap> START_TEST("Semigroups package: maximal.tst");
gap> LoadPackage("semigroups", false);;

# 
gap> SemigroupsStartTest();

# Our first example
gap> S:=Semigroup([ Transformation( [ 1, 3, 4, 1, 3, 5 ] ), 
>   Transformation( [ 1, 5, 3, 5, 5, 5 ] ), 
>   Transformation( [ 2, 4, 6, 1, 6, 5 ] ), 
>   Transformation( [ 3, 2, 4, 2, 3, 3 ] ), 
>   Transformation( [ 4, 1, 2, 6, 2, 1 ] ), 
>   Transformation( [ 4, 6, 4, 3, 3, 3 ] ), 
>   Transformation( [ 4, 6, 5, 5, 2, 6 ] ), 
>   Transformation( [ 5, 1, 6, 1, 6, 3 ] ), 
>   Transformation( [ 5, 2, 5, 3, 5, 3 ] ), 
>   Transformation( [ 6, 4, 5, 5, 1, 6 ] ) ]);
<transformation semigroup on 6 pts with 10 generators>
gap> Size(S);
7039

# Subsemigroups of T2
gap> T2:=FullTransformationMonoid(2);         
<full transformation semigroup on 2 pts>
gap> Subsemigroups(T2);
[ <trivial transformation group on 2 pts with 1 generator>, 
  <commutative transformation monoid of size 2, on 2 pts with 1 generator>, 
  <transformation monoid of size 3, on 2 pts with 2 generators>, 
  <transformation semigroup of size 2, on 2 pts with 2 generators>, 
  <trivial transformation group>, 
  <transformation group of size 2, on 2 pts with 1 generator>, 
  <commutative transformation monoid of size 2, on 2 pts with 1 generator>, 
  <trivial transformation group on 2 pts with 1 generator>, 
  <full transformation semigroup on 2 pts> ]
gap> Size(last);
9

#
gap> SemigroupsStopTest();

#
gap> STOP_TEST("Semigroups package: maximal.tst", 10000);

