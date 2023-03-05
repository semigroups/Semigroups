#############################################################################
##
#W  extreme/transform.tst
#Y  Copyright (C) 2011-16                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##


#@local S, a, d, dc, f, filt, g1, g2, gens, h, hc, i, im, inv, iso, ker, m1
#@local m18, m6, perm, perms, s, t, x
gap> START_TEST("Semigroups package: extreme/transform.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# TransformTest1
gap> gens := [Transformation([2, 3, 2, 4, 3]),
> Transformation([4, 5, 2, 2, 4]),
> Transformation([4, 3, 2, 1, 4]), Transformation([5, 5, 1, 3, 1])];;
gap> S := Semigroup(gens);;
gap> List(Elements(S), x -> IsRegularSemigroupElement(S, x));
[ true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, false, true, true, true, true, true, true, 
  true, true, false, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, false, false, true, true, true, true, true, false, false, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, false, false, true, true, false, false, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, false, false, true, true, false, false, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, false, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, false, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, false, false, false, false, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, false, false, true, true, false, false, true, true, true, 
  true, true, true, true, true, true, true, true, false, false, true, false, 
  false, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, false, false, true, true, true, true, true, true, 
  true, false, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true ]
gap> Collected(last);
[ [ true, 357 ], [ false, 31 ] ]
gap> gens := [Transformation([3, 4, 4, 2, 5, 4]),
> Transformation([2, 4, 5, 3, 2, 3]),
> Transformation([5, 4, 3, 4, 1, 5]),
> Transformation([1, 6, 2, 1, 4, 5]),
> Transformation([4, 6, 5, 5, 2, 3]),
> Transformation([1, 3, 6, 2, 1, 5]),
> Transformation([3, 6, 2, 6, 5, 4]),
> Transformation([3, 2, 6, 6, 1, 1])];;
gap> S := Semigroup(gens);;
gap> Collected(List(Elements(S), x -> IsRegularSemigroupElement(S, x)));
[ [ true, 21786 ] ]
gap> gens := [Transformation([3, 3, 3, 3]), Transformation([2, 4, 2, 4]),
> Transformation([2, 3, 2, 3]), Transformation([4, 1, 4, 3]),
> Transformation([1, 4, 4, 1]), Transformation([2, 2, 3, 1]),
> Transformation([2, 4, 3, 4]), Transformation([2, 2, 1, 2]),
> Transformation([2, 2, 1, 3]), Transformation([1, 2, 2, 3]),
> Transformation([2, 4, 3, 2]), Transformation([2, 3, 3, 3])];;
gap> S := Monoid(gens);;
gap> Collected(List(Elements(S), x -> IsRegularSemigroupElement(S, x)));
[ [ true, 179 ] ]
gap> Idempotent([1, 3, 6, 8], [1, 1, 2, 2, 3, 3, 4, 4]);
Transformation( [ 1, 1, 3, 3, 6, 6, 8, 8 ] )
gap> Idempotent([1, 7], [1, 2, 1, 2, 1, 1, 2]);
Transformation( [ 1, 7, 1, 7, 1, 1, 7 ] )
gap> Idempotent([1, 3, 6, 8], [1, 1, 2, 2, 3, 3, 4, 4]);
Transformation( [ 1, 1, 3, 3, 6, 6, 8, 8 ] )
gap> Idempotent([1, 7], [1, 2, 1, 2, 1, 1, 2]);
Transformation( [ 1, 7, 1, 7, 1, 1, 7 ] )
gap> filt := Filtered(FullTransformationSemigroup(5), x ->
> RankOfTransformation(x ^ 2, 5) = RankOfTransformation(x, 5));;
gap> Length(filt);
1305
gap> filt := Filtered(Elements(FullTransformationSemigroup(3)),
> x -> RankOfTransformation(x ^ 2, 3) = RankOfTransformation(x, 3));
[ Transformation( [ 1, 1, 1 ] ), Transformation( [ 1, 1 ] ), 
  Transformation( [ 1, 2, 1 ] ), Transformation( [ 1, 2, 2 ] ), 
  IdentityTransformation, Transformation( [ 1, 3, 2 ] ), 
  Transformation( [ 1, 3, 3 ] ), Transformation( [ 2, 1, 1 ] ), 
  Transformation( [ 2, 1, 2 ] ), Transformation( [ 2, 1 ] ), 
  Transformation( [ 2, 2, 2 ] ), Transformation( [ 2, 2 ] ), 
  Transformation( [ 2, 3, 1 ] ), Transformation( [ 2, 3, 2 ] ), 
  Transformation( [ 3, 1, 1 ] ), Transformation( [ 3, 1, 2 ] ), 
  Transformation( [ 3, 2, 1 ] ), Transformation( [ 3, 2, 3 ] ), 
  Transformation( [ 3, 3, 1 ] ), Transformation( [ 3, 3, 2 ] ), 
  Transformation( [ 3, 3, 3 ] ) ]
gap> Length(filt);
21
gap> perms := List(filt, PermutationOfImage);
[ (), (), (), (), (), (2,3), (), (1,2), (1,2), (1,2), (), (), (1,2,3), (2,3), 
  (1,3), (1,3,2), (1,3), (), (1,3), (2,3), () ]
gap> ker := List(filt, x -> FlatKernelOfTransformation(x, 3));
[ [ 1, 1, 1 ], [ 1, 1, 2 ], [ 1, 2, 1 ], [ 1, 2, 2 ], [ 1, 2, 3 ], 
  [ 1, 2, 3 ], [ 1, 2, 2 ], [ 1, 2, 2 ], [ 1, 2, 1 ], [ 1, 2, 3 ], 
  [ 1, 1, 1 ], [ 1, 1, 2 ], [ 1, 2, 3 ], [ 1, 2, 1 ], [ 1, 2, 2 ], 
  [ 1, 2, 3 ], [ 1, 2, 3 ], [ 1, 2, 1 ], [ 1, 1, 2 ], [ 1, 1, 2 ], 
  [ 1, 1, 1 ] ]
gap> im := List(filt, x -> ImageSetOfTransformation(x, 3));
[ [ 1 ], [ 1, 3 ], [ 1, 2 ], [ 1, 2 ], [ 1, 2, 3 ], [ 1, 2, 3 ], [ 1, 3 ], 
  [ 1, 2 ], [ 1, 2 ], [ 1, 2, 3 ], [ 2 ], [ 2, 3 ], [ 1, 2, 3 ], [ 2, 3 ], 
  [ 1, 3 ], [ 1, 2, 3 ], [ 1, 2, 3 ], [ 2, 3 ], [ 1, 3 ], [ 2, 3 ], [ 3 ] ]
gap> List([1 .. 21], i -> Idempotent(im[i], ker[i]) * perms[i]);
[ Transformation( [ 1, 1, 1 ] ), Transformation( [ 1, 1 ] ), 
  Transformation( [ 1, 2, 1 ] ), Transformation( [ 1, 2, 2 ] ), 
  IdentityTransformation, Transformation( [ 1, 3, 2 ] ), 
  Transformation( [ 1, 3, 3 ] ), Transformation( [ 2, 1, 1 ] ), 
  Transformation( [ 2, 1, 2 ] ), Transformation( [ 2, 1 ] ), 
  Transformation( [ 2, 2, 2 ] ), Transformation( [ 2, 2 ] ), 
  Transformation( [ 2, 3, 1 ] ), Transformation( [ 2, 3, 2 ] ), 
  Transformation( [ 3, 1, 1 ] ), Transformation( [ 3, 1, 2 ] ), 
  Transformation( [ 3, 2, 1 ] ), Transformation( [ 3, 2, 3 ] ), 
  Transformation( [ 3, 3, 1 ] ), Transformation( [ 3, 3, 2 ] ), 
  Transformation( [ 3, 3, 3 ] ) ]
gap> Set(last) = Set(filt);
true
gap> g1 := Transformation([1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10]);;
gap> g2 := Transformation([2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7]);;
gap> s := Monoid(g1, g2);;
gap> d := GreensDClasses(s);;
gap> h := GroupHClassOfGreensDClass(MinimalDClass(s));
<Green's H-class: Transformation( [ 4, 2, 2, 4, 5, 6, 7, 7, 7 ] )>
gap> perm := IsomorphismPermGroup(h);;
gap> Size(Range(perm)) = Size(h);
true
gap> g1 := Transformation([2, 1, 4, 5, 6, 7, 3, 2, 1]);;
gap> g2 := Transformation([2, 1, 4, 2, 1, 4, 2, 1, 4]);;
gap> m18 := Monoid(g1, g2);;
gap> d := GreensDClasses(m18);;
gap> i := Position(d, DClass(m18, Transformation([2, 1, 4, 5, 6, 7, 3, 2, 1])));;
gap> dc := d[i];
<Green's D-class: Transformation( [ 2, 1, 4, 5, 6, 7, 3, 2, 1 ] )>
gap> RankOfTransformation(Representative(dc));
7
gap> hc := GroupHClassOfGreensDClass(dc);
<Green's H-class: Transformation( [ 1, 2, 3, 4, 5, 6, 7, 1, 2 ] )>
gap> iso := IsomorphismPermGroup(hc);;
gap> s := Semigroup(Elements(hc));;
gap> Size(s);
10
gap> IsGroupAsSemigroup(s);
true
gap> iso := IsomorphismPermGroup(s);;
gap> g1 := Transformation([3, 3, 2, 6, 2, 4, 4, 6]);;
gap> g2 := Transformation([5, 1, 7, 8, 7, 5, 8, 1]);;
gap> m6 := Semigroup(g1, g2);;
gap> dc := GreensDClasses(m6);;
gap> hc := GroupHClassOfGreensDClass(dc[1]);
<Green's H-class: Transformation( [ 2, 2, 3, 4, 3, 6, 6, 4 ] )>
gap> s := Semigroup(Elements(hc));;
gap> iso := IsomorphismPermGroup(s);;
gap> g1 := Transformation([2, 2, 4, 4, 5, 6]);;
gap> g2 := Transformation([5, 3, 4, 4, 6, 6]);;
gap> m1 := Monoid(g1, g2);;
gap> IsRegularSemigroupElement(m1, g1);
true
gap> IsRegularSemigroupElement(m1, g2);
false
gap> IsRegularSemigroupElement(FullTransformationSemigroup(6), g2);
true
gap> ker := FlatKernelOfTransformation(g2 * g1);
[ 1, 2, 2, 2, 3, 3 ]
gap> im := ImageListOfTransformation(g2);
[ 5, 3, 4, 4, 6, 6 ]
gap> IsInjectiveListTrans(im, ker);
false
gap> Idempotent([1, 2, 5], ker);
Transformation( [ 1, 2, 2, 2, 5, 5 ] )
gap> Idempotent([1, 5, 6], [1, 1, 1, 2, 2, 3, 3]);
Transformation( [ 1, 1, 1, 5, 5, 6, 6 ] )
gap> t := Transformation([1, 2, 9, 9, 9, 8, 8, 8, 4]);;
gap> PermutationOfImage(t);
(4,9)
gap> t * last;
Transformation( [ 1, 2, 4, 4, 4, 8, 8, 8 ] )
gap> PermutationOfImage(last);
()
gap> x := Transformation([3, 4, 4, 6, 1, 3, 3, 7, 1]);;
gap> IndexPeriodOfSemigroupElement(x);
[ 2, 3 ]
gap> x ^ 2 = x ^ 5;
true
gap> t := Transformation([6, 7, 4, 1, 7, 4, 6, 1, 3, 4]);;
gap> SmallestIdempotentPower(t);
3
gap> t := Transformation([6, 6, 6, 2, 7, 1, 5, 3, 10, 6]);;
gap> SmallestIdempotentPower(t);
2
gap> s := Semigroup([Transformation([3, 1, 4, 2, 5, 2, 1, 6, 1]),
> Transformation([5, 7, 8, 8, 7, 5, 9, 1, 9]),
> Transformation([7, 6, 2, 8, 4, 7, 5, 8, 3])]);;
gap> f := Transformation([3, 1, 4, 2, 5, 2, 1, 6, 1]);;
gap> InversesOfSemigroupElement(s, f);
[  ]
gap> IsRegularSemigroupElement(s, f);
false
gap> f := Transformation([1, 9, 7, 5, 5, 1, 9, 5, 1]);;
gap> inv := InversesOfSemigroupElement(s, f);;
gap> Sort(inv);
gap> inv;
[ Transformation( [ 1, 2, 3, 5, 5, 1, 3, 5, 2 ] ), 
  Transformation( [ 1, 5, 1, 1, 5, 1, 3, 1, 2 ] ), 
  Transformation( [ 1, 5, 1, 2, 5, 1, 3, 2, 2 ] ) ]
gap> IsRegularSemigroupElement(s, f);
true
gap> ForAll(inv, g -> f * g * f = f and g * f * g = g);
true
gap> gens := [Transformation([3, 6, 4, 2, 4, 5]),
> Transformation([5, 1, 5, 4, 1, 5]),
> Transformation([5, 6, 5, 3, 4, 2]),
> Transformation([6, 6, 6, 4, 6, 2]),
> Transformation([4, 4, 4, 2, 4, 3]),
> Transformation([1, 1, 2, 2, 6, 2]),
> Transformation([5, 2, 4, 4, 6, 5]),
> Transformation([4, 6, 3, 5, 6, 6]),
> Transformation([4, 6, 3, 3, 1, 2]),
> Transformation([1, 6, 4, 4, 5, 4])];;
gap> s := Semigroup(gens);;
gap> Number(GreensRClasses(s), IsRegularGreensClass);
98
gap> Number(RClassReps(s), x -> IsRegularSemigroupElement(s, x));
98
gap> Size(s);
8175
gap> Transformation([9, 45, 53, 15, 42, 97, 71, 66, 7, 88, 6, 98, 95, 36,
> 20, 59, 94, 6, 81, 70, 65, 29, 78, 37, 74, 48, 52, 4, 32, 93, 18, 13, 55,
> 94, 49, 42, 99, 46, 35, 84, 52, 79, 80, 7, 85, 53, 89, 70, 79, 27, 84,
> 99, 9, 73, 33, 70, 77, 69, 41, 18, 63, 29, 42, 33, 75, 56, 79, 63, 89,
> 90, 64, 98, 49, 35, 100, 89, 71, 3, 70, 20, 2, 26, 11, 39, 9, 7, 89, 90,
> 48, 89, 85, 8, 56, 42, 10, 61, 25, 98, 55, 39]);;
gap> f := last;;
gap> SmallestIdempotentPower(f);
12
gap> f ^ 24 = f ^ 12;
true
gap> ForAny([1 .. 11], i -> f ^ (2 * i) = f ^ i);
false
gap> s := FullTransformationSemigroup(6);;
gap> f := Transformation([2, 1, 3, 4, 5, 1]);;
gap> inv := InversesOfSemigroupElement(s, f);;
gap> Sort(inv);
gap> inv;
[ Transformation( [ 2, 1, 3, 4, 5, 1 ] ), 
  Transformation( [ 2, 1, 3, 4, 5, 2 ] ), 
  Transformation( [ 2, 1, 3, 4, 5, 3 ] ), 
  Transformation( [ 2, 1, 3, 4, 5, 4 ] ), 
  Transformation( [ 2, 1, 3, 4, 5, 5 ] ), 
  Transformation( [ 6, 1, 3, 4, 5, 1 ] ), 
  Transformation( [ 6, 1, 3, 4, 5, 3 ] ), 
  Transformation( [ 6, 1, 3, 4, 5, 4 ] ), 
  Transformation( [ 6, 1, 3, 4, 5, 5 ] ), 
  Transformation( [ 6, 1, 3, 4, 5, 6 ] ) ]
gap> ForAll(last, g -> f * g * f = f and g * f * g = g);
true
gap> gens := [
>  Transformation([3, 3, 3, 3]), Transformation([2, 4, 2, 4]),
>  Transformation([2, 3, 2, 3]), Transformation([4, 1, 4, 3]),
>  Transformation([1, 4, 4, 1]), Transformation([2, 2, 3, 1]),
>  Transformation([2, 4, 3, 4]), Transformation([2, 2, 1, 2]),
>  Transformation([2, 2, 1, 3]), Transformation([1, 2, 2, 3]),
>  Transformation([2, 4, 3, 2]), Transformation([2, 3, 3, 3])];;
gap> s := Monoid(gens);;
gap> f := Transformation([3, 1, 1, 3]);;
gap> d := DClass(s, f);;
gap> Size(s);;
gap> Size(d);
84
gap> f in d;
true
gap> Transformation([2, 4, 4, 2]) in d;
true
gap> a := List(GreensDClasses(s), Size);;
gap> Sort(a);
gap> a;
[ 1, 4, 84, 90 ]
gap> MultiplicativeZero(s);
fail
gap> MinimalIdeal(s);
<simple transformation semigroup ideal of degree 4 with 1 generator>
gap> Size(last);
4
gap> IsRightZeroSemigroup(last2);
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/transform.tst");
