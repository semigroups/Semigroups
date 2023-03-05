############################################################################
##
#W  extreme/maximal.tst
#Y  Copyright (C) 2013-16                                   Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local B, C, G, R, R1, R2, R3, S, T, T3, U, a, acting, an, correct, gens, mat
#@local max, s1, s2, t1, t2
gap> START_TEST("Semigroups package: extreme/maximal.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();;

# MaximalTest1: IsMaximalSubsemigroup
gap> S := Semigroup([
>   Transformation([1, 2, 4, 4, 1]),
>   Transformation([4, 4, 1, 4]),
>   Transformation([5, 1, 4, 2, 3])]);
<transformation semigroup of degree 5 with 3 generators>
gap> T := Semigroup([
>   Transformation([5, 1, 4, 2, 3]),
>   Transformation([4, 4, 2, 4, 1]),
>   Transformation([3, 1, 2, 2, 2])]);
<transformation semigroup of degree 5 with 3 generators>
gap> IsMaximalSubsemigroup(S, T);
true
gap> U := Semigroup([
>   Transformation([5, 5, 1, 1, 5]),
>   Transformation([2, 2, 3, 4, 3]),
>   Transformation([3, 4, 5, 4, 3])]);
<transformation semigroup of degree 5 with 3 generators>
gap> IsSubsemigroup(S, U);
true
gap> IsMaximalSubsemigroup(S, U);
false
gap> IsSubsemigroup(U, S);
false
gap> IsMaximalSubsemigroup(U, S);
false
gap> IsSubsemigroup(S, S);
true
gap> S <> S;
false
gap> IsMaximalSubsemigroup(S, S);
false

# MaximalTest2: MaximalSubsemigroups for a Rees matrix semigroup
gap> G := Group([(1, 2), (3, 4)]);
Group([ (1,2), (3,4) ])
gap> mat := [[(), (1, 2), (1, 2)(3, 4)], [(), (1, 2), ()]];;
gap> R := ReesMatrixSemigroup(G, mat);  # 3x2 RMS over C2 x C2
<Rees matrix semigroup 3x2 over Group([ (1,2), (3,4) ])>
gap> max := MaximalSubsemigroups(R);
[ <Rees matrix semigroup 3x1 over Group([ (1,2), (3,4) ])>, 
  <Rees matrix semigroup 3x1 over Group([ (1,2), (3,4) ])>, 
  <Rees matrix semigroup 2x2 over Group([ (1,2), (3,4) ])>, 
  <Rees matrix semigroup 2x2 over Group([ (1,2), (3,4) ])>, 
  <Rees matrix semigroup 2x2 over Group([ (1,2), (3,4) ])>, 
  <subsemigroup of 3x2 Rees matrix semigroup with 3 generators> ]
gap> NrMaximalSubsemigroups(R);
6
gap> G := Group([(1, 2, 3)]);
Group([ (1,2,3) ])
gap> mat := [[(), (1, 2, 3)], [(), (1, 2, 3)]];;
gap> R := ReesMatrixSemigroup(G, mat);  # 2x2 RMS over C3
<Rees matrix semigroup 2x2 over Group([ (1,2,3) ])>
gap> max := MaximalSubsemigroups(R);
[ <Rees matrix semigroup 2x1 over Group([ (1,2,3) ])>, 
  <Rees matrix semigroup 2x1 over Group([ (1,2,3) ])>, 
  <Rees matrix semigroup 1x2 over Group([ (1,2,3) ])>, 
  <Rees matrix semigroup 1x2 over Group([ (1,2,3) ])>, 
  <subsemigroup of 2x2 Rees matrix semigroup with 2 generators> ]
gap> G := Group([(1, 2, 3), (1, 2)]);;
gap> mat := [[(), (1, 3, 2)], [(1, 3), (2, 3)], [(1, 2, 3), ()]];;
gap> R := ReesMatrixSemigroup(G, mat);  # 2x3 RMS over Sym(3)
<Rees matrix semigroup 2x3 over Group([ (1,2,3), (1,2) ])>
gap> max := MaximalSubsemigroups(R);
[ <Rees matrix semigroup 2x2 over Group([ (1,2,3), (1,2) ])>, 
  <Rees matrix semigroup 2x2 over Group([ (1,2,3), (1,2) ])>, 
  <Rees matrix semigroup 2x2 over Group([ (1,2,3), (1,2) ])>, 
  <Rees matrix semigroup 1x3 over Group([ (1,2,3), (1,2) ])>, 
  <Rees matrix semigroup 1x3 over Group([ (1,2,3), (1,2) ])>, 
  <subsemigroup of 2x3 Rees matrix semigroup with 3 generators> ]
gap> S := max[6];  # a non-RMS subsemigroup of an RMS
<subsemigroup of 2x3 Rees matrix semigroup with 3 generators>
gap> IsReesMatrixSubsemigroup(S);
true
gap> IsReesMatrixSemigroup(S);
false
gap> IsRegularSemigroup(S);
true
gap> max := MaximalSubsemigroups(S);;
gap> NrMaximalSubsemigroups(S);
5
gap> IsDuplicateFreeList(max);
true
gap> T := FullTransformationMonoid(3);;
gap> mat := [[Transformation([3, 2, 3])]];;
gap> R := ReesMatrixSemigroup(T, mat);  # 1x1 RMS over a non-group semigroup
<Rees matrix semigroup 1x1 over <full transformation monoid of degree 3>>
gap> IsReesMatrixSubsemigroup(R);
true
gap> IsReesMatrixSemigroup(R);
true
gap> IsGroup(T);
false
gap> IsSimpleSemigroup(T);
false
gap> NrMaximalSubsemigroups(R);
6
gap> S := Semigroup([
> Transformation([1, 2, 1]),
> Transformation([1, 2, 2])]);
<transformation semigroup of degree 3 with 2 generators>
gap> mat := [[Transformation([1, 2, 1])]];;
gap> R := ReesMatrixSemigroup(S, mat);  # simple 1x1 RMS over non-group semigroup
<Rees matrix semigroup 1x1 over <transformation semigroup of degree 3 with 2 
  generators>>
gap> IsReesMatrixSubsemigroup(R);
true
gap> IsReesMatrixSemigroup(R);
true
gap> IsGroup(S);
false
gap> IsSimpleSemigroup(S);
true
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 1x1 Rees matrix semigroup with 1 generator>, 
  <subsemigroup of 1x1 Rees matrix semigroup with 1 generator> ]

# MaximalTest4: MaximalSubsemigroups for a Rees 0-matrix semigroup
gap> R := ReesZeroMatrixSemigroup(Group(()), [[0]]);  # a non-regular RZMS
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> MaximalSubsemigroups(R);
[ <commutative Rees 0-matrix semigroup ideal with 1 generator> ]
gap> R1 := ReesZeroMatrixSemigroup(Group(()), [[()]]);  # 2-elt regular RZMS
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> MaximalSubsemigroups(R1);
[ <subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator>, 
  <subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator> ]
gap> R2 := last[2];  # RZMS subsemigroup, but not itself a RZMS
<subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator>
gap> IsReesZeroMatrixSubsemigroup(R2);
true
gap> IsReesZeroMatrixSemigroup(R2);
false
gap> MaximalSubsemigroups(R2);
[  ]
gap> s1 := Transformation([1, 2, 1]);;
gap> s2 := Transformation([1, 2, 2]);;
gap> S := Semigroup([s1, s2]);
<transformation semigroup of degree 3 with 2 generators>
gap> R := ReesZeroMatrixSemigroup(S, [[s1]]);  # 0-simple RZMS over non-gp
<Rees 0-matrix semigroup 1x1 over <transformation semigroup of size 2, 
  degree 3 with 2 generators>>
gap> IsReesZeroMatrixSemigroup(R);
true
gap> IsRegularSemigroup(R);
true
gap> G := UnderlyingSemigroup(R);
<transformation semigroup of size 2, degree 3 with 2 generators>
gap> IsGroup(G);
false
gap> IsZeroSimpleSemigroup(R);
true
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators>, 
  <subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators>, 
  <subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators> ]
gap> t1 := Transformation([4, 3, 1, 3]);;
gap> t2 := Transformation([3, 3, 2, 2]);;
gap> T := Semigroup([t1, t2]);
<transformation semigroup of degree 4 with 2 generators>
gap> IsRegularSemigroup(T);
true
gap> IsGroup(T);
false
gap> mat := [[t2, t1], [t1, t2]];;
gap> R3 := ReesZeroMatrixSemigroup(T, mat);  # a RZMS over a non-group semigroup
<Rees 0-matrix semigroup 2x2 over <regular transformation semigroup 
  of size 39, degree 4 with 2 generators>>
gap> IsReesZeroMatrixSubsemigroup(R3);
true
gap> IsReesZeroMatrixSemigroup(R3);
true
gap> IsRegularSemigroup(R3);
true
gap> max := MaximalSubsemigroups(R3);;
gap> NrMaximalSubsemigroups(R3);
10
gap> IsDuplicateFreeList(max);
true
gap> G := Group([(1, 2), (1, 2, 3)]);;
gap> mat := [[(1, 2), 0], [0, (2, 3)]];;
gap> R := ReesZeroMatrixSemigroup(G, mat);  # un-connected 2x2 inverse RZMS / S3
<Rees 0-matrix semigroup 2x2 over Group([ (1,2), (1,2,3) ])>
gap> max := MaximalSubsemigroups(R);
[ <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators> ]
gap> NrMaximalSubsemigroups(R);
13
gap> IsDuplicateFreeList(max);
true
gap> ForAll(max, x -> IsMaximalSubsemigroup(R, x));
true
gap> G := Group([(1, 2, 3, 4), (1, 3, 2, 4)]);;
gap> mat := [
> [(), (4, 3), 0, 0, 0],
> [(1, 3)(2, 4), 0, 0, 0, 0],
> [0, 0, (4, 3), 0, 0],
> [0, 0, 0, (), 0],
> [0, 0, 0, (), ()],
> [0, 0, 0, (), 0]];;
gap> R := ReesZeroMatrixSemigroup(G, mat);  # (3-component) 5x6 RZMS over Sym(4)
<Rees 0-matrix semigroup 5x6 over Group([ (1,2,3,4), (1,3,2,4) ])>
gap> NrMaximalSubsemigroups(R);
116
gap> mat := [
>   [(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>   [0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>   [0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>   [0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0],
>   [0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0],
>   [0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0],
>   [0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0],
>   [0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0],
>   [0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0],
>   [0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0],
>   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0],
>   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0],
>   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ()]];;
gap> G := Group(());;
gap> R := ReesZeroMatrixSemigroup(G, mat);;  # 13x13 inverse RZMS over triv group
gap> NrMaximalSubsemigroups(R);
8190
gap> G := Group([(1, 4, 2), (1, 4, 5)]);;
gap> mat := [
> [0, 0, (1, 4, 2), (1, 4, 5), 0],
> [0, (), (1, 2, 5), 0, 0],
> [(), (1, 2, 5), 0, 0, 0],
> [(), 0, 0, 0, (1, 5, 2)],
> [0, 0, 0, (1, 4, 2), (1, 4, 5)]];;
gap> R1 := ReesZeroMatrixSemigroup(G, mat);  # a connected 5x5 RZMS over Sym(3)
<Rees 0-matrix semigroup 5x5 over Group([ (1,4,2), (1,4,5) ])>
gap> max := MaximalSubsemigroups(R1);
[ <Rees 0-matrix semigroup 5x4 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 5x4 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 5x4 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 5x4 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 5x4 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 4x5 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 4x5 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 4x5 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 4x5 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 4x5 over Group([ (1,4,2), (1,4,5) ])>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 10 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 10 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 10 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 10 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 10 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 10 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 10 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 10 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 10 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 10 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 10 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 10 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 10 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 10 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 10 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 10 generators> ]
gap> NrMaximalSubsemigroups(R1);
26

# MaximalTest5: MaximalSubsemigroups for a transformation semigroup
gap> S := Semigroup(Transformation([]));  # trivial semigroup
<trivial transformation group of degree 0 with 1 generator>
gap> MaximalSubsemigroups(S);
[  ]
gap> S := Semigroup(Transformation([2, 3, 1]));  # group C3 as semigroup
<commutative transformation semigroup of degree 3 with 1 generator>
gap> MaximalSubsemigroups(S);
[ <trivial transformation group of degree 0 with 1 generator> ]
gap> S := Semigroup([
> Transformation([1, 2, 1]),
> Transformation([1, 2, 2])]);  # simple semigroup
<transformation semigroup of degree 3 with 2 generators>
gap> max := MaximalSubsemigroups(S);
[ <commutative transformation semigroup of degree 3 with 1 generator>, 
  <commutative transformation semigroup of degree 3 with 1 generator> ]
gap> List(max, Size);
[ 1, 1 ]
gap> S := Monoid([
> Transformation([1, 1])]);  # simple semigroup with adjoined zero
<commutative transformation monoid of degree 2 with 1 generator>
gap> max := MaximalSubsemigroups(S);;
gap> ForAll(max, IsTrivial);
true
gap> S := Monoid([
> Transformation([1, 1, 2])]);  # semigroup with gen in non-regular D-class
<commutative transformation monoid of degree 3 with 1 generator>
gap> max := MaximalSubsemigroups(S);;
gap> List(max, Elements);
[ [ Transformation( [ 1, 1, 1 ] ), Transformation( [ 1, 1, 2 ] ) ], 
  [ Transformation( [ 1, 1, 1 ] ), IdentityTransformation ] ]
gap> S := Monoid([
> Transformation([1, 1, 1, 1, 1]),  # semigroup with a result arising
> Transformation([2, 1, 4, 3, 2]),  # from non-maximal regular D-class
> Transformation([2, 1, 4, 3, 4])]);  # which intersects every H-class
<transformation monoid of degree 5 with 3 generators>
gap> max := MaximalSubsemigroups(S);;
gap> NrMaximalSubsemigroups(S);
5
gap> IsDuplicateFreeList(max);
true
gap> ForAll(max, x -> IsMaximalSubsemigroup(S, x));
true
gap> List(max, Elements);
[ [ Transformation( [ 1, 1, 1, 1, 1 ] ), Transformation( [ 1, 2, 3, 4, 1 ] ), 
      Transformation( [ 1, 2, 3, 4, 3 ] ), Transformation( [ 2, 1, 4, 3, 2 ] )
        , Transformation( [ 2, 1, 4, 3, 4 ] ), 
      Transformation( [ 2, 2, 2, 2, 2 ] ) ], 
  [ Transformation( [ 1, 2, 3, 4, 1 ] ), Transformation( [ 1, 2, 3, 4, 3 ] ), 
      IdentityTransformation, Transformation( [ 2, 1, 4, 3, 2 ] ), 
      Transformation( [ 2, 1, 4, 3, 4 ] ) ], 
  [ Transformation( [ 1, 1, 1, 1, 1 ] ), Transformation( [ 1, 2, 3, 4, 3 ] ), 
      IdentityTransformation, Transformation( [ 2, 1, 4, 3, 4 ] ), 
      Transformation( [ 2, 2, 2, 2, 2 ] ) ], 
  [ Transformation( [ 1, 1, 1, 1, 1 ] ), Transformation( [ 1, 2, 3, 4, 1 ] ), 
      IdentityTransformation, Transformation( [ 2, 1, 4, 3, 2 ] ), 
      Transformation( [ 2, 2, 2, 2, 2 ] ) ], 
  [ Transformation( [ 1, 1, 1, 1, 1 ] ), Transformation( [ 1, 2, 3, 4, 1 ] ), 
      Transformation( [ 1, 2, 3, 4, 3 ] ), IdentityTransformation, 
      Transformation( [ 2, 2, 2, 2, 2 ] ) ] ]
gap> S := Monoid([
> Transformation([2, 2, 4, 4, 6, 6, 8, 8, 10, 10, 12, 12, 14, 14, 16, 16,
>      18, 18, 20, 20, 22, 22, 24, 24, 14]),
>  Transformation([12, 12, 10, 10, 8, 8, 6, 6, 4, 4, 2, 2, 24, 24, 22, 22,
>      20, 20, 18, 18, 16, 16, 14, 14, 12]),
>  Transformation([5, 5, 7, 7, 1, 1, 3, 3, 11, 11, 9, 9, 17, 17, 19, 19, 13,
>      13, 15, 15, 23, 23, 21, 21, 5])]);  # highlights a special case
<transformation monoid of degree 25 with 3 generators>
gap> max := MaximalSubsemigroups(S);;
gap> NrMaximalSubsemigroups(S);
9
gap> correct := [
>  Semigroup([
>    Transformation([2, 2, 4, 4, 6, 6, 8, 8, 10, 10, 12, 12, 14, 14, 16,
>        16, 18, 18, 20, 20, 22, 22, 24, 24, 14]),
>    Transformation([5, 5, 7, 7, 1, 1, 3, 3, 11, 11, 9, 9, 17, 17, 19, 19,
>        13, 13, 15, 15, 23, 23, 21, 21, 5]),
>    Transformation([12, 12, 10, 10, 8, 8, 6, 6, 4, 4, 2, 2, 24, 24, 22,
>        22, 20, 20, 18, 18, 16, 16, 14, 14, 12])]),
>  Monoid([
>    Transformation([3, 3, 1, 1, 9, 9, 11, 11, 5, 5, 7, 7, 15, 15, 13, 13,
>        21, 21, 23, 23, 17, 17, 19, 19, 15]),
>    Transformation([11, 11, 9, 9, 7, 7, 5, 5, 3, 3, 1, 1, 23, 23, 21, 21,
>        19, 19, 17, 17, 15, 15, 13, 13, 11])]),
>  Monoid([
>    Transformation([10, 10, 12, 12, 4, 4, 2, 2, 8, 8, 6, 6, 22, 22, 24,
>        24, 16, 16, 14, 14, 20, 20, 18, 18, 22]),
>    Transformation([12, 12, 10, 10, 8, 8, 6, 6, 4, 4, 2, 2, 24, 24, 22,
>        22, 20, 20, 18, 18, 16, 16, 14, 14, 12])]),
>  Monoid([
>    Transformation([8, 8, 6, 6, 12, 12, 10, 10, 2, 2, 4, 4, 20, 20, 18,
>        18, 24, 24, 22, 22, 14, 14, 16, 16, 8]),
>    Transformation([11, 11, 9, 9, 7, 7, 5, 5, 3, 3, 1, 1, 23, 23, 21, 21,
>        19, 19, 17, 17, 15, 15, 13, 13, 11])]),
>  Monoid([
>    Transformation([1, 1, 3, 3, 5, 5, 7, 7, 9, 9, 11, 11, 13, 13, 15, 15,
>        17, 17, 19, 19, 21, 21, 23, 23, 13]),
>    Transformation([10, 10, 12, 12, 4, 4, 2, 2, 8, 8, 6, 6, 22, 22, 24,
>        24, 16, 16, 14, 14, 20, 20, 18, 18, 22]),
>    Transformation([12, 12, 10, 10, 8, 8, 6, 6, 4, 4, 2, 2, 24, 24, 22,
>        22, 20, 20, 18, 18, 16, 16, 14, 14, 24])]),
>  Monoid([
>    Transformation([1, 1, 3, 3, 5, 5, 7, 7, 9, 9, 11, 11, 13, 13, 15, 15,
>        17, 17, 19, 19, 21, 21, 23, 23, 1]),
>    Transformation([10, 10, 12, 12, 4, 4, 2, 2, 8, 8, 6, 6, 22, 22, 24,
>        24, 16, 16, 14, 14, 20, 20, 18, 18, 22])]),
>  Monoid([
>    Transformation([1, 1, 3, 3, 5, 5, 7, 7, 9, 9, 11, 11, 13, 13, 15, 15,
>        17, 17, 19, 19, 21, 21, 23, 23, 1]),
>    Transformation([12, 12, 10, 10, 8, 8, 6, 6, 4, 4, 2, 2, 24, 24, 22,
>        22, 20, 20, 18, 18, 16, 16, 14, 14, 24])]),
>  Monoid([
>    Transformation([1, 1, 3, 3, 5, 5, 7, 7, 9, 9, 11, 11, 13, 13, 15, 15,
>        17, 17, 19, 19, 21, 21, 23, 23, 1]),
>    Transformation([6, 6, 8, 8, 2, 2, 4, 4, 12, 12, 10, 10, 18, 18, 20,
>        20, 14, 14, 16, 16, 24, 24, 22, 22, 18])]),
>  Monoid([
>    Transformation([1, 1, 3, 3, 5, 5, 7, 7, 9, 9, 11, 11, 13, 13, 15, 15,
>        17, 17, 19, 19, 21, 21, 23, 23, 1]),
>    Transformation([4, 4, 2, 2, 10, 10, 12, 12, 6, 6, 8, 8, 16, 16, 14,
>        14, 22, 22, 24, 24, 18, 18, 20, 20, 16])])];;
gap> ForAll(max, x -> IsMaximalSubsemigroup(S, x));
true
gap> T3 := FullTransformationMonoid(3);  # Trans(3)
<full transformation monoid of degree 3>
gap> max := MaximalSubsemigroups(T3);;
gap> IsDuplicateFreeList(max);
true
gap> NrMaximalSubsemigroups(T3);
5
gap> correct := [
>   Semigroup([
>     Transformation([2, 3, 1]),
>     Transformation([3, 1, 1])]),
>   Semigroup([
>     Transformation([1, 3, 2]),
>     Transformation([3, 1, 1]),
>     Transformation([3, 3, 2]),
>     Transformation([1, 3, 1])]),
>   Semigroup([Transformation([2, 1]),
>     Transformation([3, 3, 1]),
>     Transformation([1, 3, 3]),
>     Transformation([1, 2, 2])]),
>   Semigroup([Transformation([3, 2, 1]),
>     Transformation([1, 2, 1]),
>     Transformation([3, 3, 1]),
>     Transformation([1, 1, 2])]),
>   Semigroup([Transformation([2, 1]),
>     Transformation([2, 3, 1]),
>     Transformation([2, 2, 2])])];;
gap> max = correct or max = correct{[1, 3, 4, 2, 5]};
true
gap> S := Semigroup([
>   Transformation([2, 1, 5, 2, 4]),
>   Transformation([2, 3, 4, 3, 1]),
>   Transformation([3, 4, 1, 4, 3]),
>   Transformation([3, 4, 2, 2, 2]),
>   Transformation([5, 1, 1, 2, 3])]);  # A random example
<transformation semigroup of degree 5 with 5 generators>
gap> max := MaximalSubsemigroups(S);;
gap> NrMaximalSubsemigroups(S);
8
gap> correct := [
>   Semigroup([
>     Transformation([1, 5, 3, 1, 2]),
>     Transformation([2, 1, 5, 2, 4]),
>     Transformation([2, 3, 4, 3, 1]),
>     Transformation([3, 2, 2, 1, 4]),
>     Transformation([3, 4, 1, 4, 3]),
>     Transformation([3, 4, 2, 2, 2]),
>     Transformation([4, 2, 2, 1])]),
>   Semigroup([
>     Transformation([1, 5, 3, 1, 2]),
>     Transformation([2, 3, 4, 3, 1]),
>     Transformation([3, 4, 1, 4, 3]),
>     Transformation([3, 4, 2, 2, 2]),
>     Transformation([4, 2, 2, 1]),
>     Transformation([4, 3, 1, 4, 3]),
>     Transformation([5, 1, 1, 2, 3])]),
>  Semigroup([
>    Transformation([1, 2, 2, 3, 4]),
>    Transformation([2, 1, 1, 5, 3]),
>    Transformation([2, 1, 5, 2, 4]),
>    Transformation([2, 3, 4, 3, 1]),
>    Transformation([3, 1, 5, 3, 1]),
>    Transformation([3, 4, 1, 4, 3]),
>    Transformation([3, 4, 2, 2, 2])]),
>  Semigroup([
>    Transformation([2, 1, 4, 2, 3]),
>    Transformation([2, 3, 4, 3, 1]),
>    Transformation([2, 4, 5, 2, 1]),
>    Transformation([3, 4, 1, 4, 3]),
>    Transformation([3, 4, 2, 2, 2]),
>    Transformation([5, 1, 1, 2, 3])]),
>  Semigroup([
>    Transformation([1, 2, 2, 3, 4]),
>    Transformation([2, 1, 5, 2, 4]),
>    Transformation([2, 3, 4, 3, 4]),
>    Transformation([3, 4, 1, 4, 3]),
>    Transformation([3, 4, 2, 2, 2]),
>    Transformation([3, 4, 3, 4, 2]),
>    Transformation([5, 1, 1, 2, 3])]),
>  Semigroup([
>    Transformation([2, 1, 5, 2, 4]),
>    Transformation([2, 3, 4, 3, 1]),
>    Transformation([3, 4, 2, 4, 3]),
>    Transformation([4, 3, 2, 2, 2]),
>    Transformation([5, 1, 1, 2, 3])]),
>  Semigroup([
>    Transformation([2, 1, 5, 2, 4]),
>    Transformation([2, 3, 4, 3, 1]),
>    Transformation([4, 3, 1, 3, 4]),
>    Transformation([5, 1, 1, 2, 3]),
>    Transformation([5, 5, 2, 2, 2])]),
>  Semigroup([
>    Transformation([2, 1, 5, 2, 4]),
>    Transformation([2, 3, 4, 3, 1]),
>    Transformation([4, 3, 1, 1, 1]),
>    Transformation([5, 1, 1, 2, 3])])];;
gap> max = correct or max = correct{[2, 1, 4, 3, 5, 6, 8, 7]};
true

# MaximalTest7: MaximalSubsemigroups for an inverse semigroup of partial 
# permutations
gap> gens := [
>   PartialPerm([1, 3, 4, 5], [6, 5, 2, 4]),
>   PartialPerm([1, 2, 3, 4, 6], [5, 4, 3, 1, 6]),
>   PartialPerm([1, 2, 5, 7], [3, 1, 4, 6])];;
gap> S := InverseSemigroup(gens);  # a random inverse semigroup of partial perms
<inverse partial perm semigroup of rank 7 with 3 generators>
gap> max := MaximalSubsemigroups(S);;
gap> NrMaximalSubsemigroups(S);
6
gap> IsDuplicateFreeList(max);
true
gap> a := List(max, Size);;
gap> Sort(a);
gap> a;
[ 713, 713, 714, 714, 715, 715 ]
gap> gens := [PartialPerm([1, 2, 3, 4], [3, 2, 5, 4]),
>  PartialPerm([1, 2, 4], [3, 5, 4]),
>  PartialPerm([1, 2, 3, 4], [5, 2, 3, 1]),
>  PartialPerm([1, 3, 4, 5], [5, 3, 4, 1]),
>  PartialPerm([1, 2, 3, 4, 5], [5, 4, 3, 2, 1])];;
gap> S := InverseSemigroup(gens);  # a random inverse semigroup of partial perms
<inverse partial perm semigroup of rank 5 with 5 generators>
gap> max := MaximalSubsemigroups(S);;
gap> NrMaximalSubsemigroups(S);
9
gap> List(max, Size);
[ 924, 924, 892, 892, 892, 892, 892, 892, 955 ]
gap> correct := [
>  Semigroup([
>      PartialPerm([2, 3, 4, 5], [2, 4, 3, 5]),
>      PartialPerm([2, 3, 4, 5], [4, 5, 2, 3]),
>      PartialPerm([1, 2, 3, 5], [1, 3, 2, 5]),
>      PartialPerm([1, 2, 3, 5], [2, 4, 3, 5]),
>      PartialPerm([1, 2, 3, 5], [3, 1, 5, 2]),
>      PartialPerm([1, 2, 3, 4, 5], [5, 4, 3, 2, 1])]),
>  Semigroup([
>      PartialPerm([2, 3, 4, 5], [2, 1, 4, 3]),
>      PartialPerm([2, 3, 4, 5], [2, 4, 3, 5]),
>      PartialPerm([2, 3, 4, 5], [2, 5, 1, 3]),
>      PartialPerm([1, 2, 3, 5], [3, 5, 1, 2]),
>      PartialPerm([1, 2, 3, 5], [5, 2, 3, 1]),
>      PartialPerm([1, 2, 3, 4, 5], [5, 4, 3, 2, 1])]),
>  Semigroup([
>      PartialPerm([2, 3, 4, 5], [1, 3, 2, 5]),
>      PartialPerm([2, 3, 4, 5], [2, 4, 3, 5]),
>      PartialPerm([2, 3, 4, 5], [5, 4, 3, 2]),
>      PartialPerm([1, 2, 3, 5], [2, 4, 3, 5]),
>      PartialPerm([1, 2, 3, 4, 5], [5, 4, 3, 2, 1])]),
>  Semigroup([
>      PartialPerm([2, 3, 4, 5], [2, 4, 3, 5]),
>      PartialPerm([2, 3, 4, 5], [2, 5, 1, 3]),
>      PartialPerm([2, 3, 4, 5], [5, 4, 3, 2]),
>      PartialPerm([1, 3, 4, 5], [3, 5, 2, 4]),
>      PartialPerm([1, 2, 3, 4, 5], [5, 4, 3, 2, 1])]),
>  Semigroup([
>      PartialPerm([2, 3, 4, 5], [1, 3, 2, 5]),
>      PartialPerm([2, 3, 4, 5], [4, 5, 2, 3]),
>      PartialPerm([2, 3, 4, 5], [5, 4, 3, 2]),
>      PartialPerm([1, 2, 3, 5], [2, 4, 3, 5]),
>      PartialPerm([1, 2, 3, 4, 5], [5, 4, 3, 2, 1])]),
>  Semigroup([
>      PartialPerm([2, 3, 4, 5], [4, 5, 2, 3]),
>      PartialPerm([2, 3, 4, 5], [5, 4, 3, 1]),
>      PartialPerm([2, 3, 4, 5], [5, 4, 3, 2]),
>      PartialPerm([1, 2, 3, 5], [2, 3, 4, 5]),
>      PartialPerm([1, 2, 3, 4, 5], [5, 4, 3, 2, 1])]),
>  Semigroup([
>      PartialPerm([2, 3, 4, 5], [1, 3, 2, 5]),
>      PartialPerm([2, 3, 4, 5], [4, 2, 5, 3]),
>      PartialPerm([1, 3, 4, 5], [5, 3, 4, 2]),
>      PartialPerm([1, 2, 3, 4, 5], [5, 4, 3, 2, 1])]),
>  Semigroup([
>      PartialPerm([2, 3, 4, 5], [1, 2, 3, 5]),
>      PartialPerm([2, 3, 4, 5], [4, 2, 5, 3]),
>      PartialPerm([1, 3, 4, 5], [5, 4, 3, 2]),
>      PartialPerm([1, 2, 3, 4, 5], [5, 4, 3, 2, 1])]),
>  Monoid([
>      PartialPerm([1, 2, 3, 4], [3, 2, 5, 4]),
>      PartialPerm([1, 2, 3, 4], [5, 2, 3, 1]),
>      PartialPerm([2, 3, 4, 5], [2, 1, 4, 3]),
>      PartialPerm([2, 3, 4, 5], [2, 4, 3, 5]),
>      PartialPerm([2, 3, 4, 5], [4, 1, 5, 3]),
>      PartialPerm([2, 3, 4, 5], [4, 5, 2, 3]),
>      PartialPerm([1, 3, 4, 5], [3, 5, 2, 4]),
>      PartialPerm([1, 2, 3, 5], [4, 2, 3, 1])])];;
gap> max = correct or max = correct{[1, 2, 4, 3, 5, 6, 8, 7, 9]};
true

# MaximalTest8: MaximalSubsemigroups for a semigroup of partitions
gap> B := PartitionMonoid(3);  # partition monoid of degree 3
<regular bipartition *-monoid of size 203, degree 3 with 4 generators>
gap> max := MaximalSubsemigroups(B);;
gap> NrMaximalSubsemigroups(B);
8
gap> List(max, Size);
[ 200, 199, 199, 199, 167, 167, 167, 167 ]
gap> S := max[1];;  # the first maximal subsemigroup of partition monoid 3
gap> NrMaximalSubsemigroups(S);
5

# a random example
gap> B := Semigroup([
> Bipartition([[1, 2, -4], [3, 4, 5, 7], [6, -5, -7], [-1, -3, -6], [-2]]),
> Bipartition([[1, 5, 6, -2, -4, -6, -7], [2, 3, 4, -3, -5], [7, -1]]),
> Bipartition([[1, 6], [2, 3, 4], [5], [7, -6, -7], [-1, -3, -4], [-2], [-5]]),
> Bipartition([[1, -4], [2, 3, 7], [4, 5, -7], [6, -1, -2, -3, -5], [-6]]),
> Bipartition([[1, 7, -3], [2, 4, 5, -4], [3, 6], [-1], [-2], [-5], [-6],
>              [-7]]),
> Bipartition([[1, 5, -2, -4, -6, -7], [2, -1, -3, -5], [3, 4, 6, 7]])]);
<bipartition semigroup of degree 7 with 6 generators>
gap> max := MaximalSubsemigroups(B);;
gap> NrMaximalSubsemigroups(B);
7
gap> correct := [
>  Semigroup([
>    Bipartition([[1, 2, 6, -6, -7], [3, 4, 5, 7], [-1, -3, -4], [-2], [-5]]),
>    Bipartition([[1, 2], [3, 4, 5, 7], [6, -6, -7], [-1, -3, -4], [-2], [-5]]),
>    Bipartition([[1, 5, 6, -2, -4, -6, -7], [2, 3, 4, -3, -5], [7, -1]]),
>    Bipartition([[1, 6], [2, 3, 4], [5], [7, -4, -5, -7], [-1, -3, -6], [-2]]),
>    Bipartition([[1, 6], [2, 3, 4], [5], [7, -4], [-1, -3, -6], [-2],
>                 [-5, -7]]),
>    Bipartition([[1, -4], [2, 3, 7], [4, 5, -7], [6, -1, -2, -3, -5], [-6]]),
>    Bipartition([[1, 7, -3], [2, 4, 5, -4], [3, 6], [-1], [-2], [-5], [-6],
>                 [-7]]),
>    Bipartition([[1, 5, -2, -4, -6, -7], [2, -1, -3, -5], [3, 4, 6, 7]])]),
>  Semigroup([
>    Bipartition([[1, 2, 3, 4, 5, 6, -1, -2, -3, -5, -7], [7, -4], [-6]]),
>    Bipartition([[1, 2, 6, -3, -5], [3, 4, 5, 7], [-1], [-2, -4, -6, -7]]),
>    Bipartition([[1, 2, -4], [3, 4, 5, 7], [6, -5, -7], [-1, -3, -6], [-2]]),
>    Bipartition([[1, 6], [2, 3, 4], [5], [7, -6, -7], [-1, -3, -4], [-2],
>                 [-5]]),
>    Bipartition([[1, 6, -2, -3, -4, -5, -6, -7], [2, 3, 7], [4, 5, -1]]),
>    Bipartition([[1, -4], [2, 3, 7], [4, 5, -7], [6, -1, -2, -3, -5], [-6]]),
>    Bipartition([[1, 7, -3], [2, 4, 5, -4], [3, 6], [-1], [-2], [-5], [-6],
>                 [-7]]),
>    Bipartition([[1, 5, -2, -4, -6, -7], [2, -1, -3, -5], [3, 4, 6, 7]])]),
>  Semigroup([
>    Bipartition([[1, 2, -4], [3, 4, 5, 7], [6, -5, -7], [-1, -3, -6], [-2]]),
>    Bipartition([[1, 5, 6, -2, -4, -6, -7], [2, 3, 4, -3, -5], [7, -1]]),
>    Bipartition([[1, 6], [2, 3, 4], [5], [7, -4, -5, -7], [-1, -3, -6], [-2]]),
>    Bipartition([[1, 6], [2, 3, 4], [5], [7], [-1, -3, -4], [-2], [-5],
>                 [-6, -7]]),
>    Bipartition([[1, -4], [2, 3, 7], [4, 5, -7], [6, -1, -2, -3, -5], [-6]]),
>    Bipartition([[1, 7, -3], [2, 4, 5, -4], [3, 6], [-1], [-2], [-5], [-6],
>                 [-7]]),
>    Bipartition([[1, 5, -2, -4, -6, -7], [2, -1, -3, -5], [3, 4, 6, 7]])]),
>  Semigroup([
>    Bipartition([[1, 2, -4], [3, 4, 5, 7], [6, -5, -7], [-1, -3, -6], [-2]]),
>    Bipartition([[1, 2], [3, 4, 5, 7], [6, -6, -7], [-1, -3, -4], [-2], [-5]]),
>    Bipartition([[1, 5, 6, -2, -4, -6, -7], [2, 3, 4, -3, -5], [7, -1]]),
>    Bipartition([[1, 6], [2, 3, 4], [5], [7], [-1, -3, -4], [-2], [-5],
>                 [-6, -7]]),
>    Bipartition([[1, 6], [2, 3, 7], [4, 5, -1, -2, -3, -5, -7], [-4], [-6]]),
>    Bipartition([[1, -4], [2, 3, 7], [4, 5, -7], [6, -1, -2, -3, -5], [-6]]),
>    Bipartition([[1, 7, -3], [2, 4, 5, -4], [3, 6], [-1], [-2], [-5], [-6],
>                 [-7]]),
>    Bipartition([[1, 5, -2, -4, -6, -7], [2, -1, -3, -5], [3, 4, 6, 7]])]),
>  Semigroup([
>    Bipartition([[1, 2, 3, 4, 5, 6, -1, -2, -3, -5, -7], [7, -4], [-6]]),
>    Bipartition([[1, 2, 6, -4, -7], [3, 4, 5, 7], [-1, -2, -3, -5], [-6]]),
>    Bipartition([[1, 2, 6, -7], [3, 4, 5, 7], [-1, -2, -3, -5], [-4], [-6]]),
>    Bipartition([[1, 2, -4], [3, 4, 5, 7], [6, -5, -7], [-1, -3, -6], [-2]]),
>    Bipartition([[1, 5, 6, -2, -4, -6, -7], [2, 3, 4, -3, -5], [7, -1]]),
>    Bipartition([[1, 6], [2, 3, 4], [5], [7, -6, -7], [-1, -3, -4], [-2],
>                 [-5]]),
>    Bipartition([[1, 6, -1, -2, -3, -5, -7], [2, 3, 7], [4, 5, -4], [-6]]),
>    Bipartition([[1, 6, -2, -3, -4, -5, -6, -7], [2, 3, 7], [4, 5, -1]]),
>    Bipartition([[1, 6], [2, 3, 7], [4, 5, -4, -5, -7], [-1, -3, -6], [-2]]),
>    Bipartition([[1, 7, -3], [2, 4, 5, -4], [3, 6], [-1], [-2], [-5], [-6],
>       [-7]]),
>    Bipartition([[1, 5, -2, -4, -6, -7], [2, -1, -3, -5], [3, 4, 6, 7]])]),
>  Semigroup([
>    Bipartition([[1, 2, 4, 5, 7, -6, -7], [3, 6], [-1, -3, -4], [-2], [-5]]),
>    Bipartition([[1, 2, -4], [3, 4, 5, 7], [6, -5, -7], [-1, -3, -6], [-2]]),
>    Bipartition([[1, 5, 6, -2, -4, -6, -7], [2, 3, 4, -3, -5], [7, -1]]),
>    Bipartition([[1, 6], [2, 3, 4], [5], [7, -3, -4], [-1], [-2], [-5], [-6],
>                 [-7]]),
>    Bipartition([[1, 6], [2, 3, 4], [5], [7, -4], [-1], [-2], [-3], [-5], [-6],
>                 [-7]]),
>    Bipartition([[1, -4], [2, 3, 7], [4, 5, -7], [6, -1, -2, -3, -5], [-6]]),
>    Bipartition([[1, 7], [2, 4, 5, -6, -7], [3, 6], [-1, -3, -4], [-2], [-5]]),
>    Bipartition([[1, 5, -2, -4, -6, -7], [2, -1, -3, -5], [3, 4, 6, 7]])]),
>  Semigroup([
>    Bipartition([[1, 2, 5, -6, -7], [3, 4, 6, 7], [-1, -3, -4], [-2], [-5]]),
>    Bipartition([[1, 2, -4], [3, 4, 5, 7], [6, -5, -7], [-1, -3, -6], [-2]]),
>    Bipartition([[1, 5, 6, -2, -4, -6, -7], [2, 3, 4, -3, -5], [7, -1]]),
>    Bipartition([[1, 6], [2, 3, 4], [5], [7, -2, -4, -6, -7], [-1, -3, -5]]),
>    Bipartition([[1, -4], [2, 3, 7], [4, 5, -7], [6, -1, -2, -3, -5], [-6]]),
>    Bipartition([[1, 7, -3], [2, 4, 5, -4], [3, 6], [-1], [-2], [-5], [-6],
>                 [-7]])])];;
gap> max = correct;
true

# MaximalTest9: MaximalSubsemigroups for a semigroup of block bijections

# a random inverse semigroup of block bijections
gap> C := InverseSemigroup([
>  Bipartition([[1, -4], [2, -5], [3, 4, 5, 6, 7, -1, -2, -3, -6, -7]]),
>  Bipartition([[1, -6], [2, -3], [3, 5, 6, 7, -1, -4, -5, -7], [4, -2]]),
>  Bipartition([[1, -6], [2, -2], [3, 6, 7, -1, -5, -7], [4, -4], [5, -3]]),
>  Bipartition([[1, -6], [2, -3], [3, -2], [4, 5, 7, -1, -5, -7], [6, -4]])]);
<inverse block bijection semigroup of degree 7 with 4 generators>
gap> max := MaximalSubsemigroups(C);;
gap> NrMaximalSubsemigroups(C);
8
gap> Number(max, x -> Size(x) = 446);
6
gap> Number(max, x -> Size(x) = 433);
2
gap> correct := [
>  Semigroup([
>    Bipartition([[1, 2, 3, 6, 7, -2, -3, -5, -6, -7], [4, -1], [5, -4]]),
>    Bipartition([[1, 4, 5, 6, 7, -1, -2, -3, -6, -7], [2, -4], [3, -5]]),
>    Bipartition([[1, 5, 7, -3, -6, -7], [2, -2], [3, -5], [4, -4], [6, -1]]),
>    Bipartition([[1, 5, 7, -4, -5, -7], [2, -3], [3, -2], [4, -6], [6, -1]]),
>    Bipartition([[1, -4], [2, -2], [3, 5, 6, 7, -1, -3, -6, -7], [4, -5]]),
>    Bipartition([[1, -6], [2, -2], [3, 5, 6, 7, -1, -3, -5, -7], [4, -4]]),
>    Bipartition([[1, -6], [2, -3], [3, -2], [4, 5, 7, -1, -5, -7], [6, -4]]),
>    Bipartition([[1, -6], [2, -2], [3, 6, 7, -1, -5, -7], [4, -4], [5, -3]])]),
>  Semigroup([
>    Bipartition([[1, 2, 3, 6, 7, -1, -4, -5, -6, -7], [4, -2], [5, -3]]),
>    Bipartition([[1, -4], [2, 3, 5, 6, 7, -1, -2, -3, -6, -7], [4, -5]]),
>    Bipartition([[1, 4, 5, 7, -2, -4, -5, -7], [2, -6], [3, -3], [6, -1]]),
>    Bipartition([[1, -1], [2, 4, 5, 7, -3, -5, -6, -7], [3, -2], [6, -4]]),
>    Bipartition([[1, 5, 7, -3, -6, -7], [2, -2], [3, -5], [4, -4], [6, -1]]),
>    Bipartition([[1, 5, 7, -4, -5, -7], [2, -3], [3, -2], [4, -6], [6, -1]]),
>    Bipartition([[1, -6], [2, -3], [3, -2], [4, 5, 7, -1, -5, -7], [6, -4]]),
>    Bipartition([[1, -6], [2, -2], [3, 6, 7, -1, -5, -7], [4, -4], [5, -3]])]),
>  Semigroup([
>    Bipartition([[1, 4, 5, 7, -1, -3, -5, -7], [2, -4], [3, -2], [6, -6]]),
>    Bipartition([[1, 4, 5, 7, -1, -5, -6, -7], [2, -3], [3, -2], [6, -4]]),
>    Bipartition([[1, -6], [2, 4, 5, 7, -1, -4, -5, -7], [3, -3], [6, -2]]),
>    Bipartition([[1, 5, 7, -3, -6, -7], [2, -2], [3, -5], [4, -4], [6, -1]]),
>    Bipartition([[1, -6], [2, -3], [3, -2], [4, 5, 6, 7, -1, -4, -5, -7]]),
>    Bipartition([[1, -1], [2, -2], [3, -3], [4, 5, 7, -4, -5, -7], [6, -6]]),
>    Bipartition([[1, -1], [2, -3], [3, 6, 7, -4, -5, -7], [4, -6], [5, -2]]),
>    Bipartition([[1, -6], [2, -2], [3, 6, 7, -1, -5, -7], [4, -4], [5, -3]])]),
>  Semigroup([
>    Bipartition([[1, 3, 6, 7, -1, -4, -5, -7], [2, -3], [4, -6], [5, -2]]),
>    Bipartition([[1, 4, 5, 7, -2, -4, -5, -7], [2, -6], [3, -3], [6, -1]]),
>    Bipartition([[1, -6], [2, -2], [3, 4, 6, 7, -1, -4, -5, -7], [5, -3]]),
>    Bipartition([[1, 5, 7, -3, -6, -7], [2, -2], [3, -5], [4, -4], [6, -1]]),
>    Bipartition([[1, 5, 7, -4, -5, -7], [2, -3], [3, -2], [4, -6], [6, -1]]),
>    Bipartition([[1, -6], [2, -3], [3, 5, 6, 7, -1, -4, -5, -7], [4, -2]]),
>    Bipartition([[1, -6], [2, -3], [3, -2], [4, 5, 7, -1, -5, -7], [6, -4]]),
>    Bipartition([[1, -1], [2, -2], [3, 6, 7, -3, -6, -7], [4, -4], [5, -5]])]),
>  Semigroup([
>    Bipartition([[1, 4, 5, 7, -1, -3, -5, -7], [2, -4], [3, -2], [6, -6]]),
>    Bipartition([[1, 4, 5, 7, -1, -5, -6, -7], [2, -3], [3, -2], [6, -4]]),
>    Bipartition([[1, 5, 7, -1, -5, -7], [2, -2], [3, -3], [4, -4], [6, -6]]),
>    Bipartition([[1, 5, 7, -4, -5, -7], [2, -3], [3, -2], [4, -6], [6, -1]]),
>    Bipartition([[1, -6], [2, -3], [3, 5, 6, 7, -1, -4, -5, -7], [4, -2]]),
>    Bipartition([[1, -6], [2, -3], [3, -2], [4, 5, 6, 7, -1, -4, -5, -7]]),
>    Bipartition([[1, -1], [2, -5], [3, -2], [4, 5, 7, -3, -6, -7], [6, -4]]),
>    Bipartition([[1, -1], [2, -3], [3, 6, 7, -4, -5, -7], [4, -6], [5, -2]])]),
>  Semigroup([
>    Bipartition([[1, 3, 5, 7, -1, -4, -5, -7], [2, -3], [4, -2], [6, -6]]),
>    Bipartition([[1, 4, 5, 7, -3, -4, -6, -7], [2, -2], [3, -5], [6, -1]]),
>    Bipartition([[1, 4, 5, 7, -3, -5, -6, -7], [2, -4], [3, -2], [6, -1]]),
>    Bipartition([[1, 5, 6, 7, -1, -4, -5, -7], [2, -3], [3, -2], [4, -6]]),
>    Bipartition([[1, 5, 7, -1, -5, -7], [2, -2], [3, -3], [4, -4], [6, -6]]),
>    Bipartition([[1, -1], [2, -5], [3, -2], [4, 5, 7, -3, -6, -7], [6, -4]]),
>    Bipartition([[1, -1], [2, -3], [3, 6, 7, -4, -5, -7], [4, -6], [5, -2]]),
>    Bipartition([[1, -6], [2, -2], [3, 6, 7, -1, -5, -7], [4, -4], [5, -3]])]),
>  Semigroup([
>    Bipartition([[1, 4, 5, 7, -1, -3, -6, -7], [2, -5], [3, -2], [6, -4]]),
>    Bipartition([[1, 4, 5, 7, -3, -4, -6, -7], [2, -2], [3, -5], [6, -1]]),
>    Bipartition([[1, 4, 5, 7, -3, -5, -6, -7], [2, -4], [3, -2], [6, -1]]),
>    Bipartition([[1, -6], [2, 4, 5, 7, -1, -4, -5, -7], [3, -3], [6, -2]]),
>    Bipartition([[1, 5, 7, -4, -5, -7], [2, -3], [3, -2], [4, -6], [6, -1]]),
>    Bipartition([[1, -6], [2, -3], [3, -2], [4, 5, 7, -1, -5, -7], [6, -4]]),
>    Bipartition([[1, -1], [2, -2], [3, 6, 7, -3, -6, -7], [4, -4], [5, -5]]),
>    Bipartition([[1, -1], [2, -3], [3, 6, 7, -4, -5, -7], [4, -6], [5, -2]])]),
>  Semigroup([
>    Bipartition([[1, 4, 5, 7, -2, -4, -5, -7], [2, -6], [3, -3], [6, -1]]),
>    Bipartition([[1, 4, 5, 7, -4, -5, -6, -7], [2, -3], [3, -2], [6, -1]]),
>    Bipartition([[1, 5, 6, 7, -1, -4, -5, -7], [2, -3], [3, -2], [4, -6]]),
>    Bipartition([[1, 5, 7, -3, -6, -7], [2, -2], [3, -5], [4, -4], [6, -1]]),
>    Bipartition([[1, -6], [2, -3], [3, 5, 6, 7, -1, -4, -5, -7], [4, -2]]),
>    Bipartition([[1, -1], [2, -2], [3, -3], [4, 5, 7, -4, -5, -7], [6, -6]]),
>    Bipartition([[1, -1], [2, -5], [3, -2], [4, 5, 7, -3, -6, -7], [6, -4]]),
>    Bipartition([[1, -6], [2, -2], [3, 6, 7, -1, -5, -7], [4, -4],
>                 [5, -3]])])];;
gap> max = correct;
true

# MaximalTest10: MaximalSubsemigroups for a transformation semigroup ideal
gap> S := SingularTransformationSemigroup(5);  # Trans(5) \ Sym(5)
<regular transformation semigroup ideal of degree 5 with 1 generator>
gap> max := MaximalSubsemigroups(S);;
gap> NrMaximalSubsemigroups(S);
40
gap> S = max[1];
false

# MaximalTest11: Issue 107
# (problems with Green's classes of ideals, and inverse semigroups)
gap> gens := [PartialPerm([1, 2, 3, 4], [3, 2, 5, 4]),
>  PartialPerm([1, 2, 4], [3, 5, 4]),
>  PartialPerm([1, 2, 3, 4], [5, 2, 3, 1]),
>  PartialPerm([1, 3, 4, 5], [5, 3, 4, 1]),
>  PartialPerm([1, 2, 3, 4, 5], [5, 4, 3, 2, 1])];;
gap> S := InverseSemigroup(gens);;
gap> S := Semigroup(S);;
gap> NrMaximalSubsemigroups(S);
9

# maximal: NrMaximalSubsemigroups, for a transformation semigroup, 1
gap> S := Semigroup([Transformation([1, 4, 2, 5, 8, 6, 2, 7]),
>  Transformation([2, 6, 6, 4, 2, 2, 1, 2]),
>  Transformation([3, 6, 6, 7, 8, 2, 1, 5]),
>  Transformation([3, 7, 4, 5, 4, 6, 6, 2]),
>  Transformation([5, 7, 5, 2, 8, 1, 2, 4])],
>  rec(acting := true));;
gap> NrMaximalSubsemigroups(S);
5

# maximal: NrMaximalSubsemigroups, for a transformation semigroup, 2
gap> S := Semigroup([Transformation([8, 4, 6, 4, 5, 3, 8, 8]),
>  Transformation([1, 3, 5, 5, 8, 4, 1, 8]),
>  Transformation([1, 7, 5, 3, 3, 7, 7, 5]),
>  Transformation([4, 7, 4, 5, 1, 7, 1]),
>  Transformation([5, 5, 8, 1, 4, 1, 4, 5]),
>  Transformation([2, 8, 8, 6, 5, 8, 1, 3]),
>  Transformation([6, 6, 2, 8, 5, 4, 4, 5])]);;
gap> NrMaximalSubsemigroups(S);
39

# maximal: NrMaximalSubsemigroups, for a transformation semigroup, 3
gap> S := Semigroup([Transformation([6, 3, 2, 5, 2, 8, 4, 2]),
>  Transformation([1, 6, 6, 3, 3, 5, 4, 5]),
>  Transformation([4, 2, 7, 3, 8, 5, 3, 3]),
>  Transformation([2, 8, 1, 8, 3, 7, 4, 4]),
>  Transformation([4, 6, 6, 8, 8, 1, 7, 8]),
>  Transformation([6, 1, 3, 6, 7, 5, 7, 6]),
>  Transformation([2, 7, 7, 4, 1, 5, 8, 2])]);;
gap> NrMaximalSubsemigroups(S);
18

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/maximal.tst");
