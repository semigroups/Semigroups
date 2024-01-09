#############################################################################
##
#W  extreme/greens-acting.tst
#Y  Copyright (C) 2011-21                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# takes approx. 1 minute to run currently!

#@local C, D, H, I, L, M, R, S, a, acting, b, c, d, dd, dr, elts, enum, f, g
#@local gens, h, hh, i, idem, iter, iter1, iter2, iter3, j, l, ll, m, out, out2
#@local out3, r, r1, r2, r3, rc, reps, rr, s, t, x
gap> START_TEST("Semigroups package: extreme/greens-acting.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();
gap> SEMIGROUPS.DefaultOptionsRec.acting := true;;

# GreensTest1
gap> gens :=
> [[Transformation([3, 4, 4, 3, 1, 1, 5])],
>  [Transformation([1, 1, 4, 3]), Transformation([2, 2, 2, 2]),
>   Transformation([3, 3, 1])],
>  [Transformation([4, 4, 2, 3, 3]), Transformation([5, 2, 4, 3, 1])],
>  [Transformation([1, 5, 4, 1, 5]), Transformation([2, 4, 1, 3, 1])],
>  [Transformation([4, 1, 2, 3]), Transformation([4, 3, 4, 1])],
>  [Transformation([2, 1, 3, 1, 4, 3]),
>   Transformation([2, 2, 2, 2, 1, 2]),
>   Transformation([5, 3, 4, 3, 5, 6]),
>   Transformation([6, 4, 1, 4, 5, 3]),
>   Transformation([6, 5, 2, 6, 3, 4])],
>  [Transformation([3, 5, 5, 1, 4, 7, 5])],
>  [Transformation([2, 5, 6, 1, 1, 3]), Transformation([3, 1, 6, 2, 5, 2]),
>   Transformation([5, 4, 2, 3, 3, 5]), Transformation([6, 6, 5, 5, 2, 2])],
>  [Transformation([1, 5, 3, 2, 3]), Transformation([4, 3, 2, 5, 2]),
>   Transformation([5, 4, 1, 2, 2]), Transformation([5, 5, 5, 1, 1])],
>  [Transformation([2, 4, 4, 7, 2, 1, 2])],
>  [Transformation([3, 4, 2, 4, 6, 7, 4]),
>       Transformation([4, 6, 3, 2, 4, 5, 4]),
>       Transformation([6, 2, 3, 5, 5, 2, 2]),
>       Transformation([6, 5, 4, 5, 2, 4, 4]),
>       Transformation([7, 6, 7, 5, 6, 5, 7])],
>   [Transformation([3, 2, 3, 3, 1]), Transformation([4, 5, 1, 2, 4])],
>   [Transformation([1, 4, 3, 4]), Transformation([2, 2, 1, 1]),
>       Transformation([3, 1, 3]), Transformation([4, 4, 3, 1])],
>   [Transformation([1, 2, 2, 3, 3]), Transformation([4, 3, 4, 3, 2]),
>       Transformation([5, 3, 4, 5, 4])],
>   [Transformation([4, 3, 6, 4, 6, 1]),
>       Transformation([4, 4, 4, 6, 3, 1])],
>   [Transformation([1, 4, 3, 4]), Transformation([3, 3, 3, 3]),
>       Transformation([3, 4, 1, 4]), Transformation([4, 3, 1, 4])],
>   [Transformation([1, 3, 3, 5, 2]), Transformation([3, 4, 5, 1, 1])],
>   [Transformation([2, 6, 4, 6, 5, 2]),
>       Transformation([3, 5, 6, 2, 5, 1]),
>       Transformation([5, 1, 3, 3, 3, 1]),
>       Transformation([6, 4, 4, 6, 6, 3])],
>   [Transformation([1, 3, 3, 3])],
>   [Transformation([4, 1, 2, 2, 3]), Transformation([4, 2, 3, 2, 2])],
>   [Transformation([1, 4, 6, 4, 4, 7, 2]),
>       Transformation([1, 6, 5, 1, 7, 2, 7]),
>       Transformation([2, 2, 7, 2, 1, 4, 4]),
>       Transformation([5, 6, 2, 6, 3, 3, 5])],
>   [Transformation([1, 1, 3, 1]), Transformation([4, 2, 3, 4]),
>       Transformation([4, 4, 2, 2])],
>   [Transformation([3, 2, 1, 1]), Transformation([4, 1, 3, 2]),
>       Transformation([4, 4, 1, 2])],
>   [Transformation([1, 6, 4, 2, 5, 3, 2]),
>       Transformation([4, 1, 4, 7, 4, 4, 5])],
>   [Transformation([2, 4, 5, 4, 4])],
>   [Transformation([1, 4, 2, 3]), Transformation([4, 3, 3, 3])],
>   [Transformation([1, 1, 3, 1, 4])],
>   [Transformation([4, 3, 3, 6, 7, 2, 3]),
>       Transformation([6, 6, 4, 4, 2, 1, 4])],
>   [Transformation([2, 2, 4, 6, 4, 3]),
>       Transformation([3, 4, 1, 1, 5, 2]),
>       Transformation([4, 4, 6, 4, 6, 1])],
>   [Transformation([3, 5, 4, 2, 1, 2, 2]),
>       Transformation([7, 7, 1, 5, 7, 1, 6])],
>   [Transformation([3, 4, 1, 4]), Transformation([4, 3, 2, 2]),
>       Transformation([4, 4, 1, 4])],
>   [Transformation([3, 7, 4, 4, 3, 3, 5]),
>       Transformation([4, 6, 1, 1, 6, 4, 1]),
>       Transformation([6, 5, 7, 2, 1, 1, 3])],
>   [Transformation([1, 2, 4, 1]), Transformation([4, 1, 2, 1]),
>       Transformation([4, 2, 2, 4])],
>   [Transformation([2, 1, 2, 2]), Transformation([2, 4, 1, 1]),
>       Transformation([4, 2, 4, 3]), Transformation([4, 4, 1, 2])],
>   [Transformation([1, 1, 1, 1, 1, 4]),
>       Transformation([3, 3, 2, 4, 1, 3]),
>       Transformation([4, 5, 2, 4, 4, 5]),
>       Transformation([5, 3, 2, 6, 6, 4]),
>       Transformation([6, 6, 5, 5, 1, 1])],
>   [Transformation([1, 2, 4, 1]), Transformation([2, 4, 1, 2]),
>       Transformation([3, 3, 1]), Transformation([3, 4, 1, 2]),
>       Transformation([4, 1, 4, 3])],
>   [Transformation([1, 7, 6, 1, 7, 5, 5]),
>       Transformation([2, 7, 1, 4, 7, 6, 2]),
>       Transformation([4, 3, 7, 2, 6, 3, 4]),
>       Transformation([4, 7, 2, 1, 7, 5, 4]),
>       Transformation([5, 7, 5, 5, 5, 3, 5])],
>   [Transformation([2, 4, 4, 3])],
>   [Transformation([4, 6, 5, 1, 4, 4])],
>   [Transformation([2, 3, 4, 5, 3]), Transformation([4, 1, 5, 1, 3]),
>       Transformation([4, 1, 5, 5, 3])],
>   [Transformation([1, 3, 1, 2, 2]), Transformation([2, 3, 5, 2, 4]),
>       Transformation([2, 4, 3, 2]), Transformation([4, 4, 2, 1, 2])],
>   [Transformation([1, 4, 2, 4]), Transformation([2, 2, 1]),
>       Transformation([3, 2, 2, 2])],
>   [Transformation([1, 5, 1, 1, 5]), Transformation([4, 3, 1, 3, 2])],
>   [Transformation([1, 3, 4, 4]), Transformation([2, 1, 3, 3]),
>       Transformation([4, 1, 3, 4]), Transformation([4, 2, 3, 3])],
>   [Transformation([4, 3, 2, 2, 1, 4, 2]),
>       Transformation([6, 5, 2, 7, 2, 2, 7])],
>   [Transformation([2, 4, 4, 3]), Transformation([3, 4, 1, 3]),
>       Transformation([4, 1, 3, 2]), Transformation([4, 4, 1, 1])],
>   [Transformation([1, 2, 5, 2, 1]), Transformation([3, 2, 2, 4, 2]),
>       Transformation([4, 5, 1, 1, 2]),
>       Transformation([5, 5, 5, 2, 1])],
>   [Transformation([1, 2, 4, 4]), Transformation([2, 1, 2, 1]),
>       Transformation([2, 3, 2, 3]), Transformation([3, 2, 1, 3]),
>       Transformation([3, 4, 3, 2])],
>   [Transformation([1, 1, 1, 2, 2])],
>   [Transformation([4, 4, 3, 3, 3, 2]),
>       Transformation([4, 6, 3, 6, 4, 3]),
>       Transformation([6, 4, 1, 3, 4, 5])],
>   [Transformation([1, 1, 4, 3]), Transformation([3, 1, 3, 2])],
>   [Transformation([1, 3, 5, 3, 3]), Transformation([1, 5, 4, 4, 3]),
>       Transformation([2, 5, 3, 1, 1])],
>   [Transformation([3, 2, 3]), Transformation([3, 4, 3, 1]),
>       Transformation([3, 4, 4, 4]), Transformation([4, 3, 1, 3])],
>   [Transformation([2, 2, 5, 2, 2, 5]),
>       Transformation([2, 6, 5, 2, 6, 3]),
>       Transformation([4, 2, 4, 5, 5]),
>       Transformation([5, 4, 1, 4, 2, 2])],
>   [Transformation([1, 1]), Transformation([3, 1, 2, 2])],
>   [Transformation([1, 1, 4, 5, 5, 3]),
>       Transformation([6, 4, 4, 5, 6, 5])],
>   [Transformation([1, 4, 5, 3, 1, 7, 3]),
>       Transformation([1, 6, 6, 5, 2, 4, 2])],
>   [Transformation([3, 3, 1, 2, 3]), Transformation([5, 5, 1, 5, 3]),
>       Transformation([5, 5, 5, 5, 2])],
>   [Transformation([1, 2, 5, 1, 5]),
>       Transformation([5, 4, 5, 5, 3, 2])],
>   [Transformation([1, 2, 1, 3]), Transformation([2, 3, 4, 4]),
>       Transformation([4, 1, 1, 1])],
>   [Transformation([1, 2, 2, 3, 2]), Transformation([4, 3, 2, 4, 1]),
>       Transformation([5, 1, 2, 2, 1]), Transformation([5, 2, 4, 1, 4]),
>       Transformation([5, 5, 4, 2, 2])],
>   [Transformation([2, 1, 2, 3]), Transformation([2, 2, 3, 2])],
>   [Transformation([4, 2, 1, 3])],
>   [IdentityTransformation, Transformation([2, 2]),
>       Transformation([2, 2, 4, 3])],
>   [Transformation([2, 1, 2, 1]), Transformation([3, 4, 2, 4])],
>   [Transformation([3, 4, 1, 2, 2, 2]),
>       Transformation([4, 4, 4, 2, 5, 3]),
>       Transformation([5, 6, 6, 5, 5, 4])],
>   [Transformation([1, 4, 1, 6, 4, 6]),
>       Transformation([2, 4, 2, 5, 5]),
>       Transformation([3, 6, 2, 1, 4, 6]),
>       Transformation([4, 6, 2, 4, 1, 2])],
>   [Transformation([1, 3, 3, 3]), Transformation([2, 1, 3, 1]),
>       Transformation([3, 1, 2, 3])],
>   [Transformation([1, 4, 1, 2]), Transformation([2, 2, 3, 2]),
>       Transformation([3, 3, 4, 3]), Transformation([4, 3, 3, 4]),
>       Transformation([4, 4, 4, 2])],
>   [Transformation([1, 2, 1]), Transformation([4, 1, 1, 2]),
>       Transformation([4, 3, 3, 2])],
>   [Transformation([2, 3, 6, 7, 1, 2, 6])],
>   [Transformation([4, 1, 1, 3, 3, 3])],
>   [Transformation([3, 3, 2, 7, 5, 5, 1]),
>       Transformation([3, 5, 5, 4, 1, 3, 2]),
>       Transformation([4, 1, 3, 6, 6, 6, 5]),
>       Transformation([7, 2, 7, 2, 7, 7, 2])],
>   [Transformation([1, 1, 7, 5, 2, 1, 2]),
>       Transformation([2, 7, 2, 6, 7, 5, 7]),
>       Transformation([4, 5, 7, 4, 3, 1, 4])],
>   [Transformation([3, 6, 4, 4, 2, 5, 1]),
>       Transformation([4, 1, 2, 5, 7, 7, 3]),
>       Transformation([4, 4, 1, 1, 6, 2]),
>       Transformation([5, 7, 6, 6, 1, 4, 5])],
>   [Transformation([1, 1, 1, 2]), Transformation([1, 3, 1, 3]),
>       Transformation([1, 4, 3, 3]), Transformation([3, 1, 1, 1]),
>       Transformation([4, 2, 3, 4])],
>   [Transformation([1, 3, 3, 2, 1, 3]),
>       Transformation([1, 5, 5, 6, 5, 2]),
>       Transformation([6, 3, 1, 1, 5, 5]),
>       Transformation([6, 3, 1, 5, 2, 4])],
>   [Transformation([2, 6, 1, 3, 1, 5]),
>       Transformation([4, 3, 3, 5, 5, 5]),
>       Transformation([4, 5, 6, 4, 4, 2]),
>       Transformation([6, 3, 5, 4, 1, 4])],
>   [Transformation([3, 1, 2, 2, 3]), Transformation([3, 2, 1, 2]),
>       Transformation([3, 3, 4, 2, 4])],
>   [Transformation([1, 7, 1, 6, 6, 5, 3]),
>       Transformation([2, 6, 5, 6, 1, 5, 6]),
>       Transformation([3, 4, 6, 1, 5, 1, 6]),
>       Transformation([7, 5, 7, 2, 5, 7, 4])],
>   [Transformation([2, 1, 2, 2, 4]), Transformation([2, 1, 4, 1, 3]),
>       Transformation([3, 3, 1, 3, 2]),
>       Transformation([5, 4, 5, 4, 5])],
>   [Transformation([2, 1, 4, 3]), Transformation([2, 3, 4, 4]),
>       Transformation([3, 3, 1, 1])],
>   [Transformation([2, 1, 1, 2])],
>   [Transformation([1, 3, 1, 3, 3]), Transformation([2, 1, 1, 4, 1]),
>       Transformation([4, 5, 1, 5, 4]), Transformation([5, 4, 3, 4, 2]),
>       Transformation([5, 5, 5, 3, 4])],
>   [Transformation([5, 5, 5, 5, 5])],
>   [Transformation([3, 2, 1, 2, 6, 6]),
>       Transformation([6, 2, 1, 4, 3, 2])],
>   [Transformation([3, 4, 4, 2, 4, 7, 2]),
>       Transformation([4, 1, 7, 7, 7, 1, 3]),
>       Transformation([5, 5, 5, 4, 4, 3, 4]),
>       Transformation([6, 6, 6, 3, 6, 7, 2]),
>       Transformation([7, 7, 5, 1, 7, 2, 3])],
>   [Transformation([1, 5, 3, 3, 1, 2, 2]),
>       Transformation([3, 4, 1, 6, 3, 4, 5]),
>       Transformation([4, 1, 2, 1, 6, 7, 5]),
>       Transformation([4, 2, 7, 2, 4, 1, 1]),
>       Transformation([7, 7, 7, 1, 5, 4, 4])],
>   [Transformation([1, 3, 2, 6, 5, 5]),
>       Transformation([3, 1, 2, 5, 6, 2]),
>       Transformation([5, 5, 1, 5, 3, 5]),
>       Transformation([6, 6, 1, 5, 6, 2])],
>   [Transformation([1, 4, 3, 3, 4, 3]),
>       Transformation([3, 1, 2, 5, 2, 5]),
>       Transformation([3, 2, 1, 6, 5, 4]),
>       Transformation([5, 2, 1, 1, 3, 1]),
>       Transformation([6, 4, 1, 1, 1, 1])],
>   [Transformation([4, 2, 3, 3, 4])],
>   [Transformation([1, 4, 4, 4, 3, 1, 5]),
>       Transformation([4, 7, 3, 6, 1, 7, 6])],
>   [Transformation([4, 3, 5, 7, 7, 1, 6])],
>   [Transformation([2, 2, 4, 1])],
>   [Transformation([1, 1, 2, 6, 4, 6]),
>       Transformation([4, 2, 3, 1, 2, 2]),
>       Transformation([4, 2, 4, 3, 6, 5])],
>   [Transformation([2, 3, 6, 4, 7, 4, 6]),
>       Transformation([4, 4, 3, 2, 6, 4, 6]),
>       Transformation([4, 6, 6, 5, 4, 6]),
>       Transformation([5, 6, 1, 6, 3, 5, 1])],
>   [Transformation([1, 1, 5, 3, 1]), Transformation([2, 2, 4, 2, 3]),
>       Transformation([2, 3, 4, 4]), Transformation([2, 4, 2, 4])],
>   [Transformation([3, 1, 1, 5, 3]), Transformation([3, 3, 5, 3, 1])],
>   [Transformation([4, 3, 3, 5, 2, 5]),
>       Transformation([6, 1, 2, 4, 1, 3])],
>   [Transformation([2, 3, 4, 3, 3]), Transformation([3, 5, 2, 4, 2]),
>       Transformation([3, 5, 2, 5, 2]),
>       Transformation([5, 3, 3, 5, 2])]];;
gap> out := [];;
gap> for x in gens do
> s := Semigroup(x);
> Add(out, [NrRClasses(s), Size(s)]);
> od;
gap> out;
[ [ 3, 4 ], [ 2, 10 ], [ 3, 14 ], [ 12, 211 ], [ 4, 28 ], [ 378, 4818 ],
  [ 2, 5 ], [ 92, 7142 ], [ 81, 615 ], [ 2, 4 ], [ 158, 2255 ], [ 18, 99 ],
  [ 9, 50 ], [ 16, 76 ], [ 17, 77 ], [ 6, 13 ], [ 19, 330 ], [ 120, 1263 ],
  [ 1, 1 ], [ 14, 53 ], [ 216, 1306 ], [ 6, 12 ], [ 15, 235 ], [ 23, 235 ],
  [ 2, 2 ], [ 3, 9 ], [ 2, 2 ], [ 17, 206 ], [ 22, 506 ], [ 24, 340 ],
  [ 7, 39 ], [ 99, 495 ], [ 10, 18 ], [ 10, 100 ], [ 34, 843 ], [ 14, 210 ],
  [ 546, 3538 ], [ 2, 3 ], [ 2, 3 ], [ 35, 448 ], [ 21, 515 ], [ 9, 14 ],
  [ 5, 11 ], [ 17, 23 ], [ 28, 763 ], [ 15, 199 ], [ 21, 170 ], [ 11, 142 ],
  [ 2, 2 ], [ 33, 1259 ], [ 6, 25 ], [ 64, 426 ], [ 9, 40 ], [ 46, 388 ],
  [ 6, 25 ], [ 11, 49 ], [ 48, 391 ], [ 7, 40 ], [ 13, 18 ], [ 6, 48 ],
  [ 30, 792 ], [ 7, 11 ], [ 1, 3 ], [ 2, 3 ], [ 8, 17 ], [ 15, 115 ],
  [ 49, 1724 ], [ 8, 45 ], [ 6, 46 ], [ 8, 66 ], [ 2, 4 ], [ 1, 3 ],
  [ 322, 4344 ], [ 30, 661 ], [ 1597, 63890 ], [ 10, 76 ], [ 173, 9084 ],
  [ 74, 3931 ], [ 15, 117 ], [ 163, 4804 ], [ 14, 106 ], [ 10, 28 ],
  [ 1, 2 ], [ 53, 328 ], [ 1, 1 ], [ 17, 26 ], [ 172, 1443 ], [ 230, 15176 ],
  [ 83, 1382 ], [ 158, 1074 ], [ 2, 2 ], [ 26, 535 ], [ 3, 6 ], [ 3, 3 ],
  [ 44, 1834 ], [ 158, 1776 ], [ 19, 326 ], [ 9, 45 ], [ 32, 379 ],
  [ 23, 149 ] ]
gap> m := Semigroup(gens[32]);;
gap> Size(m);
495
gap> ForAll(GreensRClasses(m), x -> ForAll(Idempotents(x), y -> y in x));
true
gap> idem := Set(Concatenation(List(GreensRClasses(m), Idempotents)));;
gap> idem = Set(Idempotents(m));
true
gap> H := GreensHClasses(m);;
gap> I := Concatenation(List(GreensRClasses(m), GreensHClasses));;
gap> ForAll(H, x -> Number(I, y -> Representative(x) in y) = 1);
true
gap> Set(Concatenation(List(GreensDClasses(m), GreensHClasses))) =
> Set(GreensHClasses(m));
true
gap> m := Semigroup(gens[74]);;
gap> r := GreensRClassOfElement(m, Transformation([2, 1, 2, 2, 1, 2, 1]));;
gap> d := DClassOfRClass(r);;
gap> dr := GreensRClasses(d);;
gap> r2 := First(dr, x -> x = r);;
gap> DClassOfRClass(r2) = d;
true
gap> m := Semigroup(GeneratorsOfSemigroup(m));;
gap> r := GreensRClassOfElement(m, Transformation([2, 1, 2, 2, 1, 2, 1]));;
gap> d := DClassOfRClass(r);;
gap> dr := GreensRClasses(d);;
gap> r2 := First(dr, x -> x = r);;
gap> DClassOfRClass(r2) = d;
true
gap> out := [];;
gap> for x in gens do
> s := Semigroup(x);
> Add(out, NrLClasses(s));
> od;
gap> out;
[ 3, 5, 2, 19, 9, 46, 2, 39, 25, 2, 789, 21, 11, 25, 42, 10, 23, 87, 1, 24,
  195, 9, 15, 28, 2, 7, 2, 18, 26, 25, 10, 45, 13, 11, 94, 15, 80, 2, 2, 103,
  21, 10, 7, 14, 27, 14, 20, 13, 2, 30, 9, 23, 17, 34, 8, 13, 31, 10, 17, 12,
  68, 10, 1, 2, 8, 22, 201, 7, 10, 11, 2, 1, 363, 68, 2423, 11, 57, 84, 12,
  156, 16, 10, 1, 52, 1, 20, 257, 74, 333, 74, 2, 28, 3, 3, 35, 93, 18, 16,
  25, 33 ]
gap> ForAll(GreensLClasses(m), x -> ForAll(Idempotents(x), y ->
> y in x));
true
gap> idem := Set(Concatenation(List(GreensLClasses(m), Idempotents)));
[ Transformation( [ 1, 1, 1, 1, 1, 1, 1 ] ),
  Transformation( [ 1, 1, 1, 1, 5, 1, 1 ] ),
  Transformation( [ 1, 1, 1, 1, 5, 1, 5 ] ),
  Transformation( [ 1, 1, 1, 1, 5, 5, 1 ] ),
  Transformation( [ 1, 1, 1, 4, 1, 1, 1 ] ),
  Transformation( [ 1, 1, 1, 4, 1, 4, 1 ] ),
  Transformation( [ 1, 1, 1, 4, 4, 1, 4 ] ),
  Transformation( [ 1, 1, 1, 4, 5, 1, 5 ] ),
  Transformation( [ 1, 1, 1, 5, 5, 1, 5 ] ),
  Transformation( [ 1, 1, 4, 4, 1, 1, 1 ] ),
  Transformation( [ 1, 1, 4, 4, 4, 1, 4 ] ),
  Transformation( [ 1, 1, 5, 5, 5, 1, 5 ] ),
  Transformation( [ 1, 2, 1, 1, 1, 1, 1 ] ),
  Transformation( [ 1, 2, 1, 1, 2, 1, 1 ] ),
  Transformation( [ 1, 2, 1, 1, 2, 1, 2 ] ),
  Transformation( [ 1, 2, 1, 1, 2, 2, 2 ] ),
  Transformation( [ 1, 2, 1, 1, 5, 1, 1 ] ),
  Transformation( [ 1, 2, 1, 2, 2, 1, 2 ] ),
  Transformation( [ 1, 2, 1, 2, 2, 2, 2 ] ),
  Transformation( [ 1, 2, 2, 1, 1, 1, 1 ] ),
  Transformation( [ 1, 2, 2, 1, 1, 2, 1 ] ),
  Transformation( [ 1, 2, 2, 1, 2, 2, 1 ] ),
  Transformation( [ 1, 2, 2, 1, 5, 5, 1 ] ),
  Transformation( [ 1, 4, 1, 4, 4, 1, 4 ] ),
  Transformation( [ 1, 4, 1, 4, 4, 4, 4 ] ),
  Transformation( [ 1, 5, 1, 1, 5, 1, 1 ] ),
  Transformation( [ 1, 5, 1, 1, 5, 1, 5 ] ),
  Transformation( [ 1, 5, 1, 1, 5, 5, 5 ] ),
  Transformation( [ 1, 5, 1, 4, 5, 4, 5 ] ),
  Transformation( [ 1, 5, 1, 5, 5, 1, 5 ] ),
  Transformation( [ 1, 5, 1, 5, 5, 5, 5 ] ),
  Transformation( [ 1, 5, 5, 1, 5, 5, 1 ] ),
  Transformation( [ 2, 2, 2, 2, 2, 2, 2 ] ),
  Transformation( [ 2, 2, 2, 2, 2, 6, 2 ] ),
  Transformation( [ 2, 2, 2, 2, 5, 2, 2 ] ),
  Transformation( [ 2, 2, 2, 2, 5, 2, 5 ] ),
  Transformation( [ 2, 2, 2, 2, 5, 5, 2 ] ),
  Transformation( [ 2, 2, 2, 2, 6, 6, 2 ] ),
  Transformation( [ 2, 2, 2, 2, 7, 2, 7 ] ),
  Transformation( [ 2, 2, 2, 5, 5, 2, 5 ] ),
  Transformation( [ 2, 2, 2, 6, 2, 6, 2 ] ),
  Transformation( [ 2, 2, 2, 7, 7, 2, 7 ] ),
  Transformation( [ 2, 2, 5, 5, 5, 2, 5 ] ),
  Transformation( [ 2, 2, 7, 7, 7, 2, 7 ] ),
  Transformation( [ 3, 3, 3, 3, 3, 3, 3 ] ),
  Transformation( [ 3, 3, 3, 3, 5, 3, 3 ] ),
  Transformation( [ 3, 3, 3, 3, 5, 3, 5 ] ),
  Transformation( [ 3, 3, 3, 3, 5, 5, 3 ] ),
  Transformation( [ 3, 3, 3, 3, 7, 3, 7 ] ),
  Transformation( [ 3, 3, 3, 4, 3, 3, 3 ] ),
  Transformation( [ 3, 3, 3, 4, 3, 4, 3 ] ),
  Transformation( [ 3, 3, 3, 4, 4, 3, 4 ] ),
  Transformation( [ 3, 3, 3, 4, 5, 3, 5 ] ),
  Transformation( [ 3, 3, 3, 4, 7, 3, 7 ] ),
  Transformation( [ 3, 3, 3, 5, 5, 3, 5 ] ),
  Transformation( [ 3, 3, 3, 7, 7, 3, 7 ] ),
  Transformation( [ 3, 4, 3, 4, 4, 3, 4 ] ),
  Transformation( [ 3, 4, 3, 4, 4, 4, 4 ] ),
  Transformation( [ 3, 5, 3, 3, 5, 3, 3 ] ),
  Transformation( [ 3, 5, 3, 3, 5, 3, 5 ] ),
  Transformation( [ 3, 5, 3, 3, 5, 5, 5 ] ),
  Transformation( [ 3, 5, 3, 4, 5, 4, 5 ] ),
  Transformation( [ 3, 5, 3, 5, 5, 3, 5 ] ),
  Transformation( [ 3, 5, 3, 5, 5, 5, 5 ] ),
  Transformation( [ 3, 7, 3, 3, 7, 3, 7 ] ),
  Transformation( [ 3, 7, 3, 3, 7, 7, 7 ] ),
  Transformation( [ 3, 7, 3, 4, 7, 4, 7 ] ),
  Transformation( [ 3, 7, 3, 7, 7, 3, 7 ] ),
  Transformation( [ 3, 7, 3, 7, 7, 7, 7 ] ),
  Transformation( [ 4, 3, 3, 4, 3, 3, 4 ] ),
  Transformation( [ 4, 3, 3, 4, 4, 3, 4 ] ),
  Transformation( [ 4, 3, 3, 4, 4, 4, 4 ] ),
  Transformation( [ 4, 3, 3, 4, 5, 5, 4 ] ),
  Transformation( [ 4, 4, 4, 4, 4, 4, 4 ] ),
  Transformation( [ 4, 4, 4, 4, 5, 4, 4 ] ),
  Transformation( [ 4, 4, 4, 4, 5, 4, 5 ] ),
  Transformation( [ 4, 4, 4, 4, 5, 5, 4 ] ),
  Transformation( [ 4, 4, 4, 4, 7, 4, 7 ] ),
  Transformation( [ 4, 5, 4, 4, 5, 4, 4 ] ),
  Transformation( [ 4, 5, 4, 4, 5, 4, 5 ] ),
  Transformation( [ 4, 5, 4, 4, 5, 5, 5 ] ),
  Transformation( [ 4, 5, 5, 4, 5, 5, 4 ] ),
  Transformation( [ 4, 7, 4, 4, 7, 4, 7 ] ),
  Transformation( [ 4, 7, 4, 4, 7, 7, 7 ] ),
  Transformation( [ 5, 2, 2, 5, 5, 2, 5 ] ),
  Transformation( [ 5, 2, 2, 5, 5, 5, 5 ] ),
  Transformation( [ 5, 2, 5, 5, 5, 5, 5 ] ),
  Transformation( [ 5, 3, 3, 5, 5, 3, 5 ] ),
  Transformation( [ 5, 3, 3, 5, 5, 5, 5 ] ),
  Transformation( [ 5, 5, 3, 3, 5, 5, 5 ] ),
  Transformation( [ 5, 5, 4, 4, 5, 5, 5 ] ),
  Transformation( [ 5, 5, 5, 4, 5, 4, 5 ] ),
  Transformation( [ 5, 5, 5, 4, 5, 5, 5 ] ),
  Transformation( [ 5, 5, 5, 5, 5, 5, 5 ] ),
  Transformation( [ 6, 2, 2, 6, 6, 6, 6 ] ),
  Transformation( [ 6, 2, 6, 2, 2, 6, 2 ] ),
  Transformation( [ 6, 2, 6, 6, 2, 6, 2 ] ),
  Transformation( [ 6, 2, 6, 6, 2, 6, 6 ] ),
  Transformation( [ 6, 2, 6, 6, 6, 6, 6 ] ),
  Transformation( [ 6, 6, 6, 6, 6, 6, 6 ] ),
  Transformation( [ 6, 6, 6, 6, 7, 6, 7 ] ),
  Transformation( [ 6, 6, 6, 7, 7, 6, 7 ] ),
  Transformation( [ 6, 6, 7, 7, 7, 6, 7 ] ),
  Transformation( [ 6, 7, 6, 6, 7, 6, 7 ] ),
  Transformation( [ 6, 7, 6, 7, 7, 6, 7 ] ),
  Transformation( [ 7, 2, 2, 7, 2, 2, 7 ] ),
  Transformation( [ 7, 2, 2, 7, 5, 5, 7 ] ),
  Transformation( [ 7, 2, 2, 7, 6, 6, 7 ] ),
  Transformation( [ 7, 2, 2, 7, 7, 2, 7 ] ),
  Transformation( [ 7, 2, 2, 7, 7, 7, 7 ] ),
  Transformation( [ 7, 2, 7, 7, 2, 7, 7 ] ),
  Transformation( [ 7, 2, 7, 7, 5, 7, 7 ] ),
  Transformation( [ 7, 2, 7, 7, 7, 7, 7 ] ),
  Transformation( [ 7, 3, 3, 7, 3, 3, 7 ] ),
  Transformation( [ 7, 3, 3, 7, 7, 3, 7 ] ),
  Transformation( [ 7, 3, 3, 7, 7, 7, 7 ] ),
  Transformation( [ 7, 5, 5, 7, 5, 5, 7 ] ),
  Transformation( [ 7, 5, 7, 7, 5, 7, 7 ] ),
  Transformation( [ 7, 6, 6, 7, 6, 6, 7 ] ),
  Transformation( [ 7, 6, 6, 7, 7, 6, 7 ] ),
  Transformation( [ 7, 7, 3, 3, 7, 7, 7 ] ),
  Transformation( [ 7, 7, 4, 4, 7, 7, 7 ] ),
  Transformation( [ 7, 7, 7, 4, 7, 4, 7 ] ),
  Transformation( [ 7, 7, 7, 4, 7, 7, 7 ] ),
  Transformation( [ 7, 7, 7, 6, 7, 6, 7 ] ),
  Transformation( [ 7, 7, 7, 7, 5, 5, 7 ] ),
  Transformation( [ 7, 7, 7, 7, 5, 7, 7 ] ),
  Transformation( [ 7, 7, 7, 7, 6, 6, 7 ] ),
  Transformation( [ 7, 7, 7, 7, 7, 6, 7 ] ),
  Transformation( [ 7, 7, 7, 7, 7, 7, 7 ] ) ]
gap> idem = Set(Idempotents(m));
true
gap> m := Semigroup(gens[30]);;
gap> r := GreensLClassOfElement(m, Transformation([3, 3, 3, 3, 3, 3, 5]));;
gap> d := DClassOfLClass(r);;
gap> dr := GreensLClasses(d);;
gap> r2 := First(dr, x -> x = r);;
gap> DClassOfLClass(r2) = d;
true
gap> m := Semigroup(GeneratorsOfSemigroup(m));
<transformation semigroup of degree 7 with 2 generators>
gap>  r := GreensLClassOfElement(m, Transformation([3, 3, 3, 3, 3, 3, 5]));
<Green's L-class: Transformation( [ 3, 3, 3, 3, 3, 3, 5 ] )>
gap> Transformation([3, 3, 3, 3, 3, 3, 5]) in last;
true
gap> d := DClassOfLClass(r);;
gap> dr := GreensLClasses(d);;
gap> r2 := First(dr, x -> x = r);;
gap> DClassOfLClass(r2) = d;
true
gap> out := [];;
gap> for i in gens do
> s := Semigroup(i);
> Add(out, [NrHClasses(s), Length(GreensHClasses(s))]);
> od;
gap> out;
[ [ 3, 3 ], [ 5, 5 ], [ 3, 3 ], [ 77, 77 ], [ 13, 13 ], [ 1281, 1281 ],
  [ 2, 2 ], [ 1032, 1032 ], [ 231, 231 ], [ 2, 2 ], [ 1355, 1355 ],
  [ 57, 57 ], [ 28, 28 ], [ 48, 48 ], [ 57, 57 ], [ 12, 12 ], [ 139, 139 ],
  [ 508, 508 ], [ 1, 1 ], [ 36, 36 ], [ 801, 801 ], [ 10, 10 ], [ 71, 71 ],
  [ 130, 130 ], [ 2, 2 ], [ 7, 7 ], [ 2, 2 ], [ 83, 83 ], [ 158, 158 ],
  [ 172, 172 ], [ 22, 22 ], [ 285, 285 ], [ 17, 17 ], [ 40, 40 ],
  [ 377, 377 ], [ 67, 67 ], [ 1285, 1285 ], [ 2, 2 ], [ 2, 2 ], [ 212, 212 ],
  [ 153, 153 ], [ 14, 14 ], [ 9, 9 ], [ 22, 22 ], [ 239, 239 ], [ 65, 65 ],
  [ 91, 91 ], [ 55, 55 ], [ 2, 2 ], [ 367, 367 ], [ 15, 15 ], [ 168, 168 ],
  [ 26, 26 ], [ 207, 207 ], [ 14, 14 ], [ 29, 29 ], [ 274, 274 ], [ 22, 22 ],
  [ 17, 17 ], [ 26, 26 ], [ 253, 253 ], [ 10, 10 ], [ 1, 1 ], [ 2, 2 ],
  [ 13, 13 ], [ 64, 64 ], [ 605, 605 ], [ 20, 20 ], [ 25, 25 ], [ 33, 33 ],
  [ 2, 2 ], [ 1, 1 ], [ 1520, 1520 ], [ 307, 307 ], [ 9625, 9625 ],
  [ 41, 41 ], [ 1885, 1885 ], [ 945, 945 ], [ 54, 54 ], [ 1297, 1297 ],
  [ 58, 58 ], [ 18, 18 ], [ 1, 1 ], [ 173, 173 ], [ 1, 1 ], [ 25, 25 ],
  [ 737, 737 ], [ 2807, 2807 ], [ 636, 636 ], [ 495, 495 ], [ 2, 2 ],
  [ 201, 201 ], [ 3, 3 ], [ 3, 3 ], [ 471, 471 ], [ 715, 715 ], [ 118, 118 ],
  [ 28, 28 ], [ 197, 197 ], [ 88, 88 ] ]
gap> out := [];; out2 := [];; out3 := [];;
gap> for i in gens do
> s := Semigroup(i);
> Add(out, [Number(GreensDClasses(s), IsRegularDClass), NrDClasses(s)]);
> Add(out2, List(GreensDClasses(s), x -> Length(Idempotents(x))));
> Add(out3, NrIdempotents(s));
> if not Number(GreensHClasses(s), IsGroupHClass)
> = Length(Idempotents(s)) then
> Print("Something is wrong! ", Position(gens, i), "\n");
> fi;
> od;
gap> Perform(out2, Sort);
gap> out;
[ [ 1, 3 ], [ 2, 2 ], [ 2, 2 ], [ 4, 4 ], [ 3, 3 ], [ 6, 9 ], [ 1, 2 ],
  [ 5, 5 ], [ 4, 6 ], [ 1, 2 ], [ 6, 75 ], [ 3, 10 ], [ 3, 4 ], [ 4, 8 ],
  [ 3, 12 ], [ 3, 5 ], [ 4, 4 ], [ 4, 16 ], [ 1, 1 ], [ 4, 10 ], [ 6, 101 ],
  [ 4, 5 ], [ 4, 4 ], [ 4, 8 ], [ 1, 2 ], [ 2, 3 ], [ 1, 2 ], [ 3, 6 ],
  [ 5, 7 ], [ 5, 6 ], [ 3, 4 ], [ 5, 19 ], [ 3, 8 ], [ 3, 3 ], [ 5, 13 ],
  [ 4, 4 ], [ 6, 36 ], [ 1, 2 ], [ 1, 2 ], [ 4, 14 ], [ 4, 4 ], [ 3, 7 ],
  [ 3, 4 ], [ 4, 11 ], [ 4, 4 ], [ 4, 4 ], [ 4, 7 ], [ 3, 3 ], [ 1, 2 ],
  [ 4, 4 ], [ 3, 4 ], [ 4, 7 ], [ 4, 6 ], [ 4, 16 ], [ 4, 4 ], [ 3, 7 ],
  [ 6, 8 ], [ 3, 4 ], [ 3, 13 ], [ 3, 3 ], [ 4, 7 ], [ 3, 7 ], [ 1, 1 ],
  [ 2, 2 ], [ 2, 4 ], [ 4, 9 ], [ 4, 10 ], [ 3, 3 ], [ 3, 3 ], [ 3, 3 ],
  [ 1, 2 ], [ 1, 1 ], [ 5, 54 ], [ 3, 10 ], [ 7, 32 ], [ 3, 4 ], [ 5, 7 ],
  [ 5, 15 ], [ 5, 5 ], [ 5, 22 ], [ 2, 7 ], [ 3, 5 ], [ 1, 1 ], [ 4, 17 ],
  [ 1, 1 ], [ 4, 14 ], [ 5, 62 ], [ 6, 11 ], [ 5, 26 ], [ 5, 15 ], [ 1, 2 ],
  [ 3, 8 ], [ 1, 3 ], [ 1, 3 ], [ 6, 6 ], [ 4, 19 ], [ 3, 4 ], [ 3, 5 ],
  [ 4, 6 ], [ 3, 13 ] ]
gap> out2;
[ [ 0, 0, 1 ], [ 1, 4 ], [ 1, 2 ], [ 1, 5, 7, 30 ], [ 1, 4, 4 ],
  [ 0, 0, 0, 1, 1, 6, 11, 167, 168 ], [ 0, 1 ], [ 2, 6, 42, 169, 197 ],
  [ 0, 0, 2, 5, 18, 58 ], [ 0, 1 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 5,
      8, 46, 159 ], [ 0, 0, 0, 0, 0, 0, 0, 1, 5, 19 ], [ 0, 2, 4, 11 ],
  [ 0, 0, 0, 0, 1, 2, 4, 14 ], [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3, 12 ],
  [ 0, 0, 1, 2, 3 ], [ 2, 5, 17, 39 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 6, 24, 137 ], [ 1 ],
  [ 0, 0, 0, 0, 0, 0, 1, 1, 3, 10 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
      1, 4, 6, 7, 221 ], [ 0, 1, 2, 2, 3 ], [ 1, 4, 12, 24 ],
  [ 0, 0, 0, 0, 1, 1, 7, 34 ], [ 0, 1 ], [ 0, 1, 3 ], [ 0, 1 ],
  [ 0, 0, 0, 5, 9, 36 ], [ 0, 0, 1, 1, 5, 17, 50 ], [ 0, 1, 1, 7, 7, 63 ],
  [ 0, 1, 4, 8 ], [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 7, 13, 69
     ], [ 0, 0, 0, 0, 0, 1, 2, 4 ], [ 4, 4, 17 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 6, 20, 93 ], [ 1, 4, 10, 24 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 1, 1, 1, 7, 105, 199 ], [ 0, 1 ], [ 0, 1 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 5, 13, 48 ], [ 1, 5, 20, 51 ],
  [ 0, 0, 0, 0, 1, 2, 3 ], [ 0, 1, 3, 3 ], [ 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3 ]
    , [ 2, 7, 27, 82 ], [ 1, 4, 9, 24 ], [ 0, 0, 0, 1, 3, 5, 38 ],
  [ 4, 6, 24 ], [ 0, 1 ], [ 1, 6, 47, 121 ], [ 0, 1, 4, 5 ],
  [ 0, 0, 0, 1, 5, 14, 42 ], [ 0, 0, 1, 1, 3, 8 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 6, 6, 80 ], [ 1, 1, 3, 6 ],
  [ 0, 0, 0, 0, 1, 4, 11 ], [ 0, 0, 1, 1, 4, 6, 20, 65 ], [ 0, 1, 4, 10 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2 ], [ 2, 4, 10 ],
  [ 0, 0, 0, 1, 5, 34, 62 ], [ 0, 0, 0, 0, 1, 1, 2 ], [ 1 ], [ 1, 1 ],
  [ 0, 0, 3, 4 ], [ 0, 0, 0, 0, 0, 1, 1, 6, 26 ],
  [ 0, 0, 0, 0, 0, 0, 2, 6, 47, 121 ], [ 2, 3, 10 ], [ 1, 4, 11 ],
  [ 3, 4, 15 ], [ 0, 1 ], [ 1 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 1, 3, 7, 122, 248 ], [ 0, 0, 0, 0, 0, 0, 0, 7, 12, 111 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 1, 1, 7, 9, 258, 430, 889 ], [ 0, 1, 4, 20 ],
  [ 0, 0, 1, 6, 12, 231, 324 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3, 6, 143, 163 ], [ 1, 1, 3, 4, 24 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 5, 7, 140, 277 ],
  [ 0, 0, 0, 0, 0, 5, 23 ], [ 0, 0, 1, 4, 4 ], [ 1 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 5, 5, 52 ], [ 1 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 2 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 7, 11, 177 ],
  [ 0, 0, 0, 0, 0, 1, 2, 7, 38, 390, 434 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 5, 9,
      40, 114 ], [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 6, 32, 65 ], [ 0, 1 ],
  [ 0, 0, 0, 0, 0, 6, 16, 74 ], [ 0, 0, 1 ], [ 0, 0, 1 ],
  [ 1, 1, 4, 6, 65, 114 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 7, 40, 200 ],
  [ 0, 5, 8, 44 ], [ 0, 0, 1, 3, 10 ], [ 0, 0, 1, 6, 6, 73 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 4, 33 ] ]
gap> out3;
[ 1, 5, 3, 43, 9, 354, 1, 416, 83, 1, 220, 25, 17, 21, 16, 6, 63, 168, 1, 15,
  240, 8, 41, 43, 1, 4, 1, 50, 74, 79, 13, 92, 7, 25, 122, 39, 314, 1, 1, 67,
  77, 6, 7, 7, 118, 38, 47, 34, 1, 175, 10, 62, 13, 93, 11, 16, 97, 15, 4,
  16, 102, 4, 1, 2, 7, 34, 176, 15, 16, 22, 1, 1, 381, 130, 1595, 25, 574,
  316, 33, 430, 28, 9, 1, 63, 1, 5, 197, 872, 173, 106, 1, 96, 1, 1, 191,
  248, 57, 14, 86, 38 ]
gap> a := Transformation([2, 1, 4, 5, 6, 3]);;
gap> b := Transformation([2, 3, 1, 5, 4, 1]);;
gap> M := Semigroup(a, b);;
gap> GreensLClassOfElement(M, a);
<Green's L-class: Transformation( [ 2, 1, 4, 5, 6, 3 ] )>
gap> Transformation([2, 1, 4, 5, 6, 3]) in last;
true
gap> f := FreeSemigroup(3);;
gap> a := f.1;; b := f.2;; c := f.3;;
gap> s := f / [
> [a ^ 2, a], [b ^ 2, b], [c ^ 2, c], [a * b, a], [b * a, b],
> [a * c, a], [c * a, c], [b * c, b], [c * b, c]];
<fp semigroup with 3 generators and 9 relations of length 30>
gap> Size(s);
3
gap> GreensLClassOfElement(s, s.1);
<Green's L-class: s1>
gap> gens := [Transformation([2, 2, 5, 2, 3]),
> Transformation([2, 5, 3, 5, 3])];;
gap> S := Semigroup(gens);;
gap> f := Transformation([5, 5, 3, 5, 3]);;
gap> GreensHClassOfElement(S, f);;
gap> Representative(last);
Transformation( [ 5, 5, 3, 5, 3 ] )
gap> IsTrivial(SchutzenbergerGroup(last2));
true
gap> gens := [Transformation([4, 1, 4, 5, 3]),
> Transformation([5, 3, 5, 4, 3])];;
gap> S := Semigroup(gens);;
gap> C := GreensLClassOfElement(S, gens[1] * gens[2] * gens[1]);
<Green's L-class: Transformation( [ 5, 3, 5, 4, 3 ] )>
gap> Transformation([5, 3, 5, 4, 3]) in last;
true
gap> Representative(C);
Transformation( [ 5, 3, 5, 4, 3 ] )
gap> AsList(RhoOrb(C));
[ [ 1, 2, 1, 3, 2 ] ]
gap> RhoOrbMults(RhoOrb(C),
> RhoOrbSCCIndex(C)){OrbSCC(RhoOrb(C))[RhoOrbSCCIndex(C)]};
[ [ IdentityTransformation, IdentityTransformation ] ]
gap> SchutzenbergerGroup(C);
Sym( [ 3 .. 5 ] )
gap> gens := [Transformation([1, 2, 1, 2, 1]),
> Transformation([3, 4, 2, 1, 4])];;
gap> S := Semigroup(gens);;
gap> RClassReps(S);
[ Transformation( [ 1, 2, 1, 2, 1 ] ), Transformation( [ 3, 4, 2, 1, 4 ] ),
  Transformation( [ 1, 2, 2, 1, 2 ] ), Transformation( [ 2, 1, 2, 1, 1 ] ) ]
gap> a := Transformation([2, 1, 4, 5, 6, 3]);;
gap> b := Transformation([2, 3, 1, 5, 4, 1]);;
gap> M := Semigroup(a, b);;
gap> rc := GreensRClassOfElement(M, a * b * a);
<Green's R-class: Transformation( [ 4, 1, 6, 5, 2, 2 ] )>
gap> Transformation([5, 2, 1, 4, 3, 3]) in last;
true
gap> Transformation([4, 1, 6, 5, 2, 2]) in rc;
true
gap> Representative(rc);
Transformation( [ 4, 1, 6, 5, 2, 2 ] )
gap> SchutzenbergerGroup(rc);
Group([ (1,3,5), (1,2,3)(4,5) ])
gap> gens := [Transformation([4, 1, 5, 2, 4]),
> Transformation([4, 4, 1, 5, 3])];;
gap> S := Semigroup(gens);;
gap> f := Transformation([5, 5, 3, 3, 3]);;
gap> GreensDClassOfElement(S, f);
<Green's D-class: Transformation( [ 5, 5, 3, 3, 3 ] )>
gap> Transformation([4, 4, 5, 5, 5]) in last;
true
gap> gens := [Transformation([4, 4, 3, 5, 3]),
> Transformation([5, 1, 1, 4, 1]),
> Transformation([5, 5, 4, 4, 5])];;
gap> S := Semigroup(gens);;
gap> f := Transformation([4, 5, 5, 5, 5]);;
gap> SchutzenbergerGroup(GreensDClassOfElement(S, f));
Group([ (4,5) ])
gap> SchutzenbergerGroup(GreensRClassOfElement(S, f));
Group([ (4,5) ])
gap> SchutzenbergerGroup(GreensLClassOfElement(S, f));
Group([ (4,5) ])
gap> SchutzenbergerGroup(GreensHClassOfElement(S, f));
Group([ (4,5) ])
gap> S := Semigroup([Transformation([2, 3, 4, 1]),
> Transformation([3, 3, 1, 1])]);;
gap> Idempotents(S);
[ IdentityTransformation, Transformation( [ 1, 1, 3, 3 ] ),
  Transformation( [ 1, 3, 3, 1 ] ), Transformation( [ 2, 2, 4, 4 ] ),
  Transformation( [ 4, 2, 2, 4 ] ) ]
gap> gens := [Transformation([4, 3, 3, 6, 7, 2, 3]),
>   Transformation([6, 6, 4, 4, 2, 1, 4])];;
gap> S := Semigroup(gens);;
gap> Length(GreensRClasses(S));
17
gap> r := GreensRClasses(S)[10];;
gap> Representative(r);
Transformation( [ 3, 3, 3, 3, 3, 2, 3 ] )
gap> SchutzenbergerGroup(r);
Group([ (2,3) ])
gap> Number(GreensDClasses(S), IsRegularDClass);
3
gap> gens := [
>  Transformation([1, 54, 25, 4, 49, 30, 7, 56, 51, 44, 31, 62, 13, 20, 35,
>  38, 61, 18, 37, 14, 63, 42, 23, 24, 3, 58, 27, 34, 55, 6, 11, 32, 45, 28,
>  15, 36, 19, 16, 59, 64, 41, 22, 53, 10, 33, 46, 47, 50, 5, 48, 9, 52, 43,
>  2, 29, 8, 57, 26, 39, 60, 17, 12, 21, 40]),
>  Transformation([1, 56, 21, 36, 61, 26, 7, 48, 3, 28, 55, 14, 41, 54, 33,
>  62, 43, 4, 11, 2, 51, 40, 13, 46, 63, 22, 47, 6, 19, 58, 29, 18, 39, 30,
>  45, 52, 31, 12, 35, 10, 23, 64, 5, 34, 59, 24, 57, 38, 17, 16, 25, 60,
>  49, 8, 37, 50, 27, 42, 15, 32, 53, 20, 9, 44]),
>  Transformation([1, 64, 33, 18, 11, 8, 7, 10, 15, 14, 17, 26, 23, 22, 25,
>  30, 29, 32, 49, 42, 39, 38, 41, 46, 45, 48, 57, 54, 53, 56, 61, 60, 63,
>  2, 3, 4, 5, 6, 9, 12, 13, 16, 19, 20, 21, 24, 27, 28, 31, 34, 35, 36, 37,
>  40, 43, 44, 47, 50, 51, 52, 55, 58, 59, 62]),
>  Transformation([1, 3, 19, 27, 31, 33, 1, 63, 29, 59, 61, 21, 23, 51, 53,
>  25, 55, 57, 5, 9, 11, 35, 41, 7, 37, 39, 13, 15, 43, 45, 17, 47, 49, 35,
>  43, 47, 49, 3, 17, 45, 13, 15, 37, 39, 5, 7, 41, 9, 11, 51, 55, 57, 19,
>  25, 53, 21, 23, 59, 61, 27, 29, 63, 31, 33]),
>  Transformation([1, 18, 57, 46, 41, 36, 7, 4, 47, 18, 13, 60, 1, 32, 27,
>  52, 23, 24, 23, 32, 27, 60, 1, 24, 57, 52, 7, 4, 41, 36, 13, 46, 47, 4,
>  27, 24, 23, 52, 57, 32, 1, 60, 13, 18, 47, 46, 7, 36, 41, 36, 47, 46, 13,
>  18, 41, 4, 7, 52, 57, 24, 23, 60, 27, 32])];;
gap> s := Semigroup(gens);;
gap> DegreeOfTransformationSemigroup(s);
64
gap> f := Transformation(
> [53, 15, 42, 7, 6, 36, 20, 59, 6, 29, 37, 48, 52, 4, 32, 18,
> 13, 55, 49, 42, 46, 35, 52, 7, 53, 27, 9, 33, 41, 18, 63, 29, 42, 33, 56,
> 63, 64, 49, 35, 3, 20, 2, 26, 11, 39, 9, 7, 48, 8, 56, 42, 10, 61, 25, 55,
> 39, 62, 21, 34, 57, 44, 14, 14, 53]);;
gap> f in s;
false
gap> GreensRClassOfElement(s, f);
Error, the 2nd argument (a mult. elt.) does not belong to the 1st argument (a \
semigroup)
gap> f := Transformation([1, 33, 49, 57, 61, 63, 1, 59, 53, 51, 55, 39, 41,
> 35, 37, 45, 43, 47, 11, 15, 17, 3, 13, 7, 5, 9, 23, 25, 19, 21, 29, 27, 31,
> 3, 19, 27, 31, 33, 29, 21, 23, 25, 5, 9, 11, 7, 13, 15, 17, 35, 43, 47, 49,
> 45, 37, 39, 41, 51, 55, 57, 53, 59, 61, 63]);;
gap> f in s;
true
gap> r1 := GreensRClassOfElement(s, f);;
gap> gens := [Transformation([2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]),
>  Transformation([1, 3, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]),
>  Transformation([1, 2, 4, 4, 5, 6, 7, 8, 9, 10, 11, 12]),
>  Transformation([1, 2, 3, 5, 5, 6, 7, 8, 9, 10, 11, 12]),
>  Transformation([1, 2, 3, 4, 6, 6, 7, 8, 9, 10, 11, 12]),
>  Transformation([1, 2, 3, 4, 5, 7, 7, 8, 9, 10, 11, 12]),
>  Transformation([1, 2, 3, 4, 5, 6, 8, 8, 9, 10, 11, 12]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 9, 9, 10, 11, 12]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 8, 10, 10, 11, 12]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 11, 12]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 12]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 12]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 11, 12]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 8, 8, 10, 11, 12]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 7, 9, 10, 11, 12]),
>  Transformation([1, 2, 3, 4, 5, 6, 6, 8, 9, 10, 11, 12]),
>  Transformation([1, 2, 3, 4, 5, 5, 7, 8, 9, 10, 11, 12]),
>  Transformation([1, 2, 3, 4, 4, 6, 7, 8, 9, 10, 11, 12]),
>  Transformation([1, 2, 3, 3, 5, 6, 7, 8, 9, 10, 11, 12]),
>  Transformation([1, 2, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12]),
>  Transformation([1, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12])];;
gap> s := Semigroup(gens);;
gap> f := Transformation([1, 1, 2, 4, 5, 5, 5, 5, 6, 12, 12, 12]);;
gap> r := GreensRClassOfElementNC(s, f);;
gap> r2 := GreensRClassOfElement(s, f);;
gap> r = r2;
true
gap> Representative(r) in r2;
true
gap> Representative(r2) in r;
true
gap> gens := [Transformation([5, 1, 4, 6, 2, 3]),
> Transformation([1, 2, 3, 4, 5, 6]),
> Transformation([4, 6, 3, 4, 2, 5]),
> Transformation([5, 4, 6, 3, 1, 3]),
> Transformation([2, 2, 6, 5, 4, 3]),
> Transformation([3, 5, 5, 1, 2, 4]),
> Transformation([6, 5, 1, 3, 3, 4]),
> Transformation([1, 3, 4, 3, 2, 1])];;
gap> s := Semigroup(gens);;
gap> Size(s);
43779
gap> NrRClasses(s);
200
gap> iter := IteratorOfRClasses(s);
<iterator>
gap> r := NextIterator(iter);;
gap> r := NextIterator(iter);
<Green's R-class: Transformation( [ 4, 6, 3, 4, 2, 5 ] )>
gap> Transformation([4, 6, 3, 4, 2, 5]) in last;
true
gap> r := NextIterator(iter);
<Green's R-class: Transformation( [ 2, 6, 3, 4, 5, 4 ] )>
gap> Transformation([2, 6, 3, 4, 5, 4]) in last;
true
gap> r := NextIterator(iter);
<Green's R-class: Transformation( [ 2, 2, 6, 5, 4, 3 ] )>
gap> Transformation([2, 2, 6, 5, 4, 3]) in last;
true
gap> r := NextIterator(iter);
<Green's R-class: Transformation( [ 3, 5, 5, 2, 6, 4 ] )>
gap> Transformation([3, 5, 5, 2, 6, 4]) in last;
true
gap> r := NextIterator(iter);
<Green's R-class: Transformation( [ 3, 2, 5, 4, 4 ] )>
gap> Transformation([3, 2, 5, 4, 4]) in last;
true
gap> r := NextIterator(iter);
<Green's R-class: Transformation( [ 1, 3, 4, 3, 2, 1 ] )>
gap> Transformation([1, 3, 4, 3, 2, 1]) in last;
true
gap> d := DClassOfRClass(r);
<Green's D-class: Transformation( [ 1, 3, 4, 3, 2, 1 ] )>
gap> Transformation([1, 3, 4, 3, 2, 1]) in last;
true
gap> Size(d);
23400
gap> Position(GreensDClasses(s), d);
3
gap> List(GreensRClasses(s), x -> Position(GreensDClasses(s),
> DClassOfRClass(x)));
[ 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 2, 3, 3, 3, 2, 3, 4, 2, 3, 3, 3, 4, 2, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 3, 4, 4, 3, 3, 4, 3, 4, 4, 4, 4, 4, 4, 4,
  4, 4, 4, 5, 2, 3, 3, 3, 3, 3, 4, 4, 5, 3, 3, 4, 3, 3, 4, 4, 2, 3, 3, 4, 4,
  4, 4, 4, 4, 3, 3, 4, 4, 4, 3, 3, 3, 4, 4, 4, 4, 4, 2, 3, 3, 4, 3, 4, 4, 3,
  3, 4, 4, 3, 4, 3, 4, 4, 4, 4, 3, 4, 4, 4, 4, 3, 4, 4, 4, 4, 3, 4, 4, 4, 5,
  3, 4, 4, 4, 4, 4, 4, 3, 4, 4, 5, 4, 4, 4, 4, 5, 5, 5, 5, 4, 5, 5, 5, 4, 5,
  5, 5, 4, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 3, 4, 3, 3, 4, 5, 5, 5, 3,
  4, 3, 5, 5, 3, 3, 4, 5, 3, 3, 4, 5, 3, 4, 4, 5, 4, 4, 5, 5, 5, 6, 4, 4, 4 ]
gap> gens := [Transformation([1, 2, 6, 4, 7, 5, 7]),
>  Transformation([2, 1, 6, 4, 5, 6, 7]),
>  Transformation([1, 7, 4, 6, 3, 2, 6]),
>  Transformation([5, 2, 4, 5, 6, 6, 3]),
>  Transformation([1, 4, 5, 6, 7, 3, 7]),
>  Transformation([1, 5, 2, 7, 3, 6, 6]),
>  Transformation([7, 3, 5, 6, 2, 1, 2]),
>  Transformation([3, 6, 2, 6, 7, 4, 1]),
>  Transformation([2, 5, 3, 3, 1, 6, 7]),
>  Transformation([3, 7, 6, 7, 4, 2, 5]),
>  Transformation([2, 6, 2, 5, 4, 7, 3]),
>  Transformation([2, 7, 6, 4, 5, 4, 3]),
>  Transformation([3, 3, 2, 7, 5, 1, 3]),
>  Transformation([7, 1, 1, 3, 1, 6, 2]),
>  Transformation([5, 6, 3, 2, 1, 4, 6]),
>  Transformation([2, 3, 1, 7, 2, 3, 4]),
>  Transformation([5, 2, 2, 5, 7, 6, 1]),
>  Transformation([2, 5, 7, 4, 5, 3, 1]),
>  Transformation([5, 2, 4, 5, 7, 3, 4]),
>  Transformation([7, 5, 1, 2, 2, 5, 3]),
>  Transformation([7, 3, 3, 5, 1, 7, 4]),
>  Transformation([1, 6, 6, 3, 3, 7, 1])];;
gap> s := Semigroup(gens);
<transformation semigroup of degree 7 with 22 generators>
gap> Size(s);
677391
gap> f := Transformation([1, 5, 3, 2, 7, 6, 7]);;
gap> f in s;
true
gap> d := GreensDClassOfElement(s, f);
<Green's D-class: Transformation( [ 1, 5, 3, 2, 7, 6, 7 ] )>
gap> Transformation([1, 6, 4, 7, 5, 2, 5]) in last;
true
gap> reps := Set(RClassReps(d));
[ Transformation( [ 1, 4, 5, 2, 7, 6, 6 ] ),
  Transformation( [ 1, 5, 7, 2, 4, 6, 2 ] ),
  Transformation( [ 1, 6, 4, 7, 5, 2, 5 ] ),
  Transformation( [ 4, 2, 6, 2, 5, 7, 1 ] ),
  Transformation( [ 4, 5, 6, 7, 1, 6, 2 ] ),
  Transformation( [ 4, 7, 6, 2, 7, 5, 1 ] ),
  Transformation( [ 5, 2, 4, 6, 1, 7, 2 ] ),
  Transformation( [ 5, 4, 7, 7, 1, 6, 2 ] ) ]
gap> Set(reps, x -> Position(GreensRClasses(s), GreensRClassOfElement(s, x)));
[ 1, 2, 3, 5, 6, 7, 13, 16 ]
gap> r := GreensRClasses(s)[63];;
gap> Idempotents(r);
[ Transformation( [ 1, 7, 3, 4, 5, 7, 7 ] ),
  Transformation( [ 1, 2, 3, 4, 5, 2, 2 ] ),
  Transformation( [ 1, 6, 3, 4, 5, 6, 6 ] ) ]
gap> last[2] in r;
true
gap> gens := [Transformation([2, 8, 3, 7, 1, 5, 2, 6]),
>  Transformation([3, 5, 7, 2, 5, 6, 3, 8]),
>  Transformation([4, 1, 8, 3, 5, 7, 3, 5]),
>  Transformation([4, 3, 4, 5, 6, 4, 1, 2]),
>  Transformation([5, 4, 8, 8, 5, 6, 1, 5]),
>  Transformation([6, 7, 4, 1, 4, 1, 6, 2]),
>  Transformation([7, 1, 2, 2, 2, 7, 4, 5]),
>  Transformation([8, 8, 5, 1, 7, 5, 2, 8])];;
gap> s := Semigroup(gens{[1 .. 4]});;
gap> gens[5] in s;
false
gap> gens := [Transformation([3, 4, 1, 2, 1]),
> Transformation([4, 2, 1, 5, 5]),
> Transformation([4, 2, 2, 2, 4])];;
gap> s := Semigroup(gens{[1, 2]});;
gap> gens[3] in s;
false
gap> gens := [Transformation([6, 7, 1, 2, 3, 4, 5]),
>  Transformation([7, 6, 5, 4, 3, 2, 1]),
>  Transformation([4, 5, 6, 7, 1, 2, 3]),
>  Transformation([5, 6, 6, 5, 4, 3, 4]),
>  Transformation([5, 4, 3, 2, 3, 3, 4]),
>  Transformation([5, 4, 3, 3, 4, 4, 4]),
>  Transformation([1, 7, 1, 1, 1, 1, 2]),
>  Transformation([5, 6, 6, 5, 4, 4, 5])];;
gap> s := Semigroup(gens);;
gap> f := Transformation([7, 6, 5, 4, 3, 2, 1]);;
gap> f in s;
true
gap> r := RClass(s, f);;
gap> l := LClass(s, f);
<Green's L-class: Transformation( [ 7, 6, 5, 4, 3, 2, 1 ] )>
gap> Transformation([7, 6, 5, 4, 3, 2, 1]) in last;
true
gap> h := HClass(s, f);
<Green's H-class: Transformation( [ 7, 6, 5, 4, 3, 2, 1 ] )>
gap> Transformation([7, 6, 5, 4, 3, 2, 1]) in last;
true
gap> d := DClass(s, f);
<Green's D-class: Transformation( [ 7, 6, 5, 4, 3, 2, 1 ] )>
gap> Transformation([7, 6, 5, 4, 3, 2, 1]) in last;
true
gap> AsSSortedList(r) = AsSSortedList(l);
true
gap> AsSSortedList(r) = AsSSortedList(h);
true
gap> AsSSortedList(l) = AsSSortedList(r);
true
gap> AsSSortedList(h) = AsSSortedList(r);
true
gap> AsSSortedList(d) = AsSSortedList(r);
true
gap> AsSSortedList(r) = AsSSortedList(d);
true
gap> AsSSortedList(r) = AsSSortedList(r);
true
gap> r = l; r = h; l = r; h = r; d = r; r = d; r = r;
false
false
false
false
false
false
true
gap> f := Transformation([5, 4, 3, 3, 4, 4, 4]);;
gap> rr := RClass(s, f);; ll := LClass(s, f);;
gap> hh := HClass(s, f);; dd := DClass(s, f);;
gap> r = rr; r = ll; r = hh; r = dd; rr = ll; rr = hh; rr = dd;
false
false
false
false
false
false
false
gap> gens := [Transformation([5, 1, 4, 6, 2, 3]),
> Transformation([1, 2, 3, 4, 5, 6]),
> Transformation([4, 6, 3, 4, 2, 5]),
> Transformation([5, 4, 6, 3, 1, 3]),
> Transformation([2, 2, 6, 5, 4, 3]),
> Transformation([3, 5, 5, 1, 2, 4]),
> Transformation([6, 5, 1, 3, 3, 4]),
> Transformation([1, 3, 4, 3, 2, 1])];;
gap> s := Semigroup(gens);;
gap> f := Transformation([6, 1, 1, 2, 5, 3]);;
gap> r := RClass(s, f);
<Green's R-class: Transformation( [ 6, 1, 1, 2, 5, 3 ] )>
gap> Transformation([5, 4, 4, 6, 2, 3]) in last;
true
gap> List(gens, x -> x in r);
[ false, false, false, false, false, true, false, false ]
gap> rr := RClass(s, gens[6]);
<Green's R-class: Transformation( [ 3, 5, 5, 1, 2, 4 ] )>
gap> Transformation([3, 5, 5, 2, 6, 4]) in last;
true
gap> gens[6] in rr; r = rr; f in rr; f in r;
true
true
true
true
gap> Size(r); Number(s, x -> x in r);
720
720
gap> gens := [Transformation([5, 6, 7, 3, 1, 4, 2, 8]),
>   Transformation([3, 6, 8, 5, 7, 4, 2, 8])];;
gap> s := Semigroup(gens);;
gap> f := Transformation([5, 2, 7, 4, 8, 6, 8, 8]);;
gap> r := RClass(s, f);;
gap> Size(r);
1200
gap> ForAll(r, x -> x in r);
true
gap> g := Transformation([6, 8, 2, 5, 8, 4, 8, 8]);;
gap> rr := RClass(s, g);;
gap> ForAny(rr, x -> x in r);
false
gap> ForAny(r, x -> x in rr);
false
gap> gens := [Transformation([2, 3, 4, 5, 1, 8, 7, 6, 2, 7]),
> Transformation([3, 8, 7, 4, 1, 4, 3, 3, 7, 2])];;
gap> s := Monoid(gens);;
gap> f := Transformation([3, 7, 7, 4, 3, 4, 3, 3, 3, 3]);;
gap> r := RClass(s, f);;
gap> ForAll(r, x -> x in r);
true
gap> gens := [Transformation([1, 3, 2, 3]),
>  Transformation([1, 4, 1, 2]),
>  Transformation([3, 4, 2, 2]),
>  Transformation([4, 1, 2, 1])];;
gap> s := Monoid(gens);;
gap> f := Transformation([3, 1, 1, 1]);;
gap> r := RClass(s, f);;
gap> Set(Filtered(s, x -> x in r)) = Elements(r);
true
gap> gens := [Transformation([2, 8, 3, 7, 1, 5, 2, 6]),
>   Transformation([3, 5, 7, 2, 5, 6, 3, 8]),
>   Transformation([4, 1, 8, 3, 5, 7, 3, 5]),
>   Transformation([4, 3, 4, 5, 6, 4, 1, 2]),
>   Transformation([5, 4, 8, 8, 5, 6, 1, 5]),
>   Transformation([6, 7, 4, 1, 4, 1, 6, 2]),
>   Transformation([7, 1, 2, 2, 2, 7, 4, 5]),
>   Transformation([8, 8, 5, 1, 7, 5, 2, 8])];;
gap> s := Semigroup(gens);;
gap> iter := IteratorOfRClasses(s);
<iterator>
gap> repeat r := NextIterator(iter); until Size(r) > 1;
gap> repeat r := NextIterator(iter); until Size(r) > 1;
gap> repeat r := NextIterator(iter); until Size(r) > 1;
gap> r = RClass(s, Transformation([1, 5, 8, 8, 8, 1, 7, 2]));
true
gap> Size(r);
2640
gap> enum := Enumerator(r);
<enumerator of <Green's R-class: Transformation( [ 1, 5, 8, 8, 8, 1, 7, 2 ] )>
 >
gap> Transformation([1, 5, 8, 8, 8, 1, 7, 2]) in last;
true
gap> enum[1];
Transformation( [ 1, 5, 8, 8, 8, 1, 7, 2 ] )
gap> enum[2];
Transformation( [ 6, 1, 3, 3, 3, 6, 4, 2 ] )
gap> enum[43];
Transformation( [ 2, 6, 3, 3, 3, 2, 1, 7 ] )
gap> enum[1368];
Transformation( [ 2, 1, 8, 8, 8, 2, 3, 6 ] )
gap> Position(enum, last);
1368
gap> ForAll([1 .. 2640], x -> Position(enum, enum[x]) = x);
true
gap> for i in enum do od;
gap> AsSet(enum) = AsSSortedList(r);
true
gap> Set(List(AsSSortedList(r), x -> Position(enum, x))) = [1 .. 2640];
true
gap> ForAll(AsSSortedList(r), x -> x in r);
true
gap> s := Semigroup(gens);
<transformation semigroup of degree 8 with 8 generators>
gap> r := RClass(s, Transformation([3, 5, 2, 2, 3, 5, 2, 3]));;
gap> r = RClass(s, Transformation([8, 5, 1, 1, 8, 5, 1, 8]));
true
gap> enum := Enumerator(r);;
gap> ForAll([1 .. Length(enum)], x -> Position(enum, enum[x]) = x);
true
gap> ForAll(enum, x -> x in enum);
true
gap> AsSet(enum) = AsSSortedList(r);
true
gap> AsList(enum) = AsSSortedList(r);
false
gap> Size(enum);
330
gap> Size(r);
330
gap> ForAll(r, x -> x in enum);
true
gap> rr := RClass(s, Random(r));;
gap> ForAll(rr, x -> x in enum);
true
gap> Set(List(rr, x -> Position(enum, x))) = [1 .. Length(enum)];
true
gap> rr := RClass(s, Transformation([5, 1, 5, 3, 8, 1, 5, 7]));
<Green's R-class: Transformation( [ 5, 1, 5, 3, 8, 1, 5, 7 ] )>
gap> Transformation([5, 1, 5, 3, 8, 1, 5, 7]) in last;
true
gap> ForAny(rr, x -> x in enum);
false
gap> ForAny(rr, x -> not Position(enum, x) = fail);
false
gap> ForAll(rr, x -> Position(enum, x) = fail);
true
gap> f := Transformation([2, 2, 6, 4, 1, 6, 3, 2]);;
gap> s := Semigroup(gens);
<transformation semigroup of degree 8 with 8 generators>
gap> r := GreensRClassOfElementNC(s, f);
<Green's R-class: Transformation( [ 2, 2, 6, 4, 1, 6, 3, 2 ] )>
gap> Transformation([2, 2, 6, 4, 1, 6, 3, 2]) in last;
true
gap> Size(r);
2640
gap> enum := Enumerator(r);
<enumerator of <Green's R-class: Transformation( [ 2, 2, 6, 4, 1, 6, 3, 2 ] )>
 >
gap> Transformation([2, 2, 6, 4, 1, 6, 3, 2]) in last;
true
gap> enum[1];
Transformation( [ 2, 2, 6, 4, 1, 6, 3, 2 ] )
gap> enum[1000];
Transformation( [ 6, 6, 1, 5, 4, 1, 2, 6 ] )
gap> Position(enum, last);
1000
gap> ForAll([1 .. 2640], x -> Position(enum, enum[x]) = x);
true
gap> AsSet(enum) = AsSSortedList(r);
true
gap> Set(List(AsSSortedList(r), x -> Position(enum, x))) = [1 .. 2640];
true
gap> ForAll(AsSSortedList(r), x -> x in enum);
true
gap> gens := [Transformation([1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10]),
> Transformation([2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7])];;
gap> s := Monoid(gens);;
gap> f := Transformation([2, 2, 7, 7, 11, 4, 5, 4, 10, 10, 6]);;
gap> r := RClass(s, f);
<Green's R-class: Transformation( [ 2, 2, 7, 7, 11, 4, 5, 4, 10, 10, 6 ] )>
gap> Transformation([2, 2, 7, 7, 11, 4, 5, 4, 10, 10, 6]) in last;
true
gap> AsList(r);;
gap> Size(r);
2520
gap> Length(AsList(r));
2520
gap> ForAll(AsList(r), x -> x in r);
true
gap> gens := [Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>   Transformation([3, 1, 4, 2, 5, 2, 1, 6, 1, 7]),
>   Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]),
>   Transformation([4, 7, 6, 9, 10, 1, 3, 6, 6, 2]),
>   Transformation([5, 9, 10, 9, 6, 3, 8, 4, 6, 5]),
>   Transformation([6, 2, 2, 7, 8, 8, 2, 10, 2, 4]),
>   Transformation([6, 2, 8, 4, 7, 5, 8, 3, 5, 8]),
>   Transformation([7, 1, 4, 3, 2, 7, 7, 6, 6, 5]),
>   Transformation([7, 10, 10, 1, 7, 9, 10, 4, 2, 10]),
>   Transformation([10, 7, 10, 8, 8, 7, 5, 9, 1, 9])];;
gap> s := Semigroup(gens);;
gap> iter := IteratorOfRClasses(s);;
gap> repeat
>   r := NextIterator(iter);
> until IsDoneIterator(iter) or Size(r) > 1000;
gap> r;
<Green's R-class: Transformation( [ 2, 6, 7, 2, 6, 9, 9, 1, 1, 5 ] )>
gap> Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]) in last;
true
gap> Size(r);
12960
gap> AsList(r);;
gap> ForAll(AsList(r), x -> x in r);
true
gap> gens := [Transformation([12, 10, 8, 5, 1, 5, 12, 12, 8, 2, 6, 2]),
> Transformation([5, 6, 10, 11, 10, 4, 10, 12, 5, 7, 4, 10]),
> Transformation([6, 8, 12, 5, 4, 8, 10, 7, 4, 1, 10, 11])];;
gap> s := Monoid(gens);;
gap> iter := IteratorOfRClasses(s);
<iterator>
gap> for r in iter do if Size(r) > 1000 then break; fi; od;
gap> r;
<Green's R-class: Transformation( [ 2, 2, 12, 1, 12, 1, 2, 2, 12, 10, 5,
   10 ] )>
gap> Size(r);
2760
gap> AsList(r);;
gap> Length(AsList(r));
2760
gap> s := Monoid(gens);;
gap> iter := IteratorOfRClasses(s);
<iterator>
gap> for r in iter do if Size(r) > 1000 then break; fi; od;
gap> r;
<Green's R-class: Transformation( [ 2, 2, 12, 1, 12, 1, 2, 2, 12, 10, 5,
   10 ] )>
gap> enum := Enumerator(r);
<enumerator of <Green's R-class: Transformation( [ 2, 2, 12, 1, 12, 1, 2, 2,
   12, 10, 5, 10 ] )>>
gap> for i in enum do od;
gap> s := Semigroup([
>  Transformation([12, 10, 8, 5, 1, 5, 12, 12, 8, 2, 6, 2]),
>  Transformation([5, 6, 10, 11, 10, 4, 10, 12, 5, 7, 4, 10]),
>  Transformation([6, 8, 12, 5, 4, 8, 10, 7, 4, 1, 10, 11])]);;
gap> f := Transformation([4, 8, 4, 8, 4, 8, 4, 8, 4, 8, 4, 8]);;
gap> gens := [Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>   Transformation([3, 1, 4, 2, 5, 2, 1, 6, 1, 7]),
>   Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]),
>   Transformation([4, 7, 6, 9, 10, 1, 3, 6, 6, 2]),
>   Transformation([5, 9, 10, 9, 6, 3, 8, 4, 6, 5]),
>   Transformation([6, 2, 2, 7, 8, 8, 2, 10, 2, 4]),
>   Transformation([6, 2, 8, 4, 7, 5, 8, 3, 5, 8]),
>   Transformation([7, 1, 4, 3, 2, 7, 7, 6, 6, 5]),
>   Transformation([7, 10, 10, 1, 7, 9, 10, 4, 2, 10]),
>   Transformation([10, 7, 10, 8, 8, 7, 5, 9, 1, 9])];;
gap> s := Semigroup(gens);;
gap> f := Transformation([2, 2, 4, 7, 4, 7, 2, 7, 2, 2]);;
gap> r := RClass(s, f);
<Green's R-class: Transformation( [ 2, 2, 4, 7, 4, 7, 2, 7, 2, 2 ] )>
gap> Size(r);
702
gap> Transformation([8, 8, 6, 2, 6, 2, 8, 2, 8, 8]) in r;
true
gap> Set(Idempotents(r));
[ Transformation( [ 1, 1, 3, 4, 3, 4, 1, 4, 1, 1 ] ),
  Transformation( [ 1, 1, 3, 6, 3, 6, 1, 6, 1, 1 ] ),
  Transformation( [ 1, 1, 3, 8, 3, 8, 1, 8, 1, 1 ] ),
  Transformation( [ 1, 1, 5, 4, 5, 4, 1, 4, 1, 1 ] ),
  Transformation( [ 1, 1, 5, 6, 5, 6, 1, 6, 1, 1 ] ),
  Transformation( [ 1, 1, 5, 8, 5, 8, 1, 8, 1, 1 ] ),
  Transformation( [ 2, 2, 3, 4, 3, 4, 2, 4, 2, 2 ] ),
  Transformation( [ 2, 2, 3, 6, 3, 6, 2, 6, 2, 2 ] ),
  Transformation( [ 2, 2, 3, 8, 3, 8, 2, 8, 2, 2 ] ),
  Transformation( [ 2, 2, 5, 4, 5, 4, 2, 4, 2, 2 ] ),
  Transformation( [ 2, 2, 5, 6, 5, 6, 2, 6, 2, 2 ] ),
  Transformation( [ 2, 2, 5, 8, 5, 8, 2, 8, 2, 2 ] ),
  Transformation( [ 7, 7, 3, 4, 3, 4, 7, 4, 7, 7 ] ),
  Transformation( [ 7, 7, 3, 6, 3, 6, 7, 6, 7, 7 ] ),
  Transformation( [ 7, 7, 3, 8, 3, 8, 7, 8, 7, 7 ] ),
  Transformation( [ 7, 7, 5, 4, 5, 4, 7, 4, 7, 7 ] ),
  Transformation( [ 7, 7, 5, 6, 5, 6, 7, 6, 7, 7 ] ),
  Transformation( [ 7, 7, 5, 8, 5, 8, 7, 8, 7, 7 ] ),
  Transformation( [ 9, 9, 3, 4, 3, 4, 9, 4, 9, 9 ] ),
  Transformation( [ 9, 9, 3, 6, 3, 6, 9, 6, 9, 9 ] ),
  Transformation( [ 9, 9, 3, 8, 3, 8, 9, 8, 9, 9 ] ),
  Transformation( [ 9, 9, 5, 4, 5, 4, 9, 4, 9, 9 ] ),
  Transformation( [ 9, 9, 5, 6, 5, 6, 9, 6, 9, 9 ] ),
  Transformation( [ 9, 9, 5, 8, 5, 8, 9, 8, 9, 9 ] ),
  Transformation( [ 10, 10, 3, 4, 3, 4, 10, 4, 10, 10 ] ),
  Transformation( [ 10, 10, 3, 6, 3, 6, 10, 6, 10, 10 ] ),
  Transformation( [ 10, 10, 3, 8, 3, 8, 10, 8, 10, 10 ] ),
  Transformation( [ 10, 10, 5, 4, 5, 4, 10, 4, 10, 10 ] ),
  Transformation( [ 10, 10, 5, 6, 5, 6, 10, 6, 10, 10 ] ),
  Transformation( [ 10, 10, 5, 8, 5, 8, 10, 8, 10, 10 ] ) ]
gap> gens := [Transformation([10, 8, 7, 4, 1, 4, 10, 10, 7, 2]),
> Transformation([5, 2, 5, 5, 9, 10, 8, 3, 8, 10])];;
gap> s := Monoid(gens);;
gap> f := Transformation([1, 1, 10, 8, 8, 8, 1, 1, 10, 8]);;
gap> r := RClass(s, f);;
gap> IsRegularGreensClass(r);
true
gap> Set(Idempotents(r));
[ Transformation( [ 2, 2, 3, 5, 5, 5, 2, 2, 3, 5 ] ),
  Transformation( [ 2, 2, 3, 10, 10, 10, 2, 2, 3, 10 ] ),
  Transformation( [ 2, 2, 9, 5, 5, 5, 2, 2, 9, 5 ] ),
  Transformation( [ 2, 2, 9, 10, 10, 10, 2, 2, 9, 10 ] ),
  Transformation( [ 8, 8, 3, 5, 5, 5, 8, 8, 3, 5 ] ),
  Transformation( [ 8, 8, 3, 10, 10, 10, 8, 8, 3, 10 ] ),
  Transformation( [ 8, 8, 9, 5, 5, 5, 8, 8, 9, 5 ] ),
  Transformation( [ 8, 8, 9, 10, 10, 10, 8, 8, 9, 10 ] ) ]
gap> h := HClass(s, f);;
gap> f := Transformation([5, 9, 8, 8, 8, 8, 5, 5, 8, 5]);;
gap> r := RClass(s, f);;
gap> NrIdempotents(r);
6
gap> IsRegularGreensClass(r);
true
gap> f := Transformation([8, 9, 3, 3, 3, 3, 8, 8, 3, 10]);;
gap> r := RClass(s, f);;
gap> Idempotents(r);;
gap> IsRegularGreensClass(r);
true
gap> gens := [Transformation([2, 3, 4, 5, 1, 8, 7, 6, 2, 7]),
> Transformation([3, 8, 7, 4, 1, 4, 3, 3, 7, 2])];;
gap> s := Monoid(gens);;
gap> f := Transformation([3, 8, 7, 4, 1, 4, 3, 3, 7, 2]);;
gap> r := RClass(s, f);;
gap> IsRegularGreensClass(r);
false
gap> Idempotents(r);
[  ]
gap> NrIdempotents(r);
0
gap> gens := [Transformation([5, 6, 7, 3, 1, 4, 2, 8]),
>   Transformation([3, 6, 8, 5, 7, 4, 2, 8])];;
gap> s := Semigroup(gens);;
gap> Iterator(s);
<iterator>
gap> iter := last;
<iterator>
gap> for i in [1 .. 10000] do NextIterator(iter); od;
gap> iter := Iterator(s);
<iterator>
gap> j := 0; for i in iter do j := j + 1; od;
0
gap> j;
52300
gap> gens := [
>   Transformation([5, 23, 27, 8, 21, 49, 36, 33, 4, 44, 3, 49, 48, 18, 10,
>     30, 47, 3, 41, 35, 33, 15, 39, 19, 37, 24, 26, 2, 16, 47, 9, 7, 28, 47,
>     25, 21, 50, 23, 18, 42, 26, 40, 40, 4, 43, 27, 45, 35, 40, 14]),
>   Transformation([42, 50, 5, 37, 17, 35, 39, 35, 21, 9, 32, 15, 21, 17, 38,
>     28, 40, 32, 45, 45, 32, 49, 25, 18, 50, 45, 36, 2, 35, 10, 1, 13, 6,
>     20, 5, 4, 45, 45, 24, 45, 43, 4, 28, 21, 5, 31, 13, 49, 28, 20])];;
gap> s := Semigroup(gens);
<transformation semigroup of degree 50 with 2 generators>
gap> f := Transformation([39, 6, 6, 17, 25, 17, 39, 28, 28, 5, 6, 17, 4, 25,
> 32, 25, 32, 6, 4, 6, 28, 28, 32, 17, 17, 5, 17, 39, 17, 32, 5, 25, 6, 32,
> 39, 25, 28, 6, 25, 39, 17, 17, 17, 28, 17, 6, 6, 6, 17, 39]);;
gap> r := RClass(s, f);;
gap> r = RClass(s, Transformation([5, 13, 13, 45, 20, 45, 5, 50, 50, 32, 13,
>   45, 17, 20, 40, 20, 40, 13, 17, 13, 50, 50, 40, 45, 45, 32, 45, 5, 45, 40,
>   32, 20, 13, 40, 5, 20, 50, 13, 20, 5, 45, 45, 45, 50, 45, 13, 13, 13, 45,
>   5]));
true
gap> Size(r);
30683520
gap> iter := Iterator(r);
<iterator>
gap> for i in [1 .. 100000] do NextIterator(iter); od;
gap> gens := [Transformation([3, 12, 14, 4, 11, 18, 17, 2, 2, 9, 5, 15, 2, 18,
> 17, 8, 20, 10, 19, 12]),
> Transformation([13, 1, 8, 5, 4, 14, 13, 11, 12, 9, 13, 20, 20, 2, 14, 18,
> 20, 7, 3, 19])];;
gap> s := Semigroup(gens);;
gap> f := Transformation([12, 14, 3, 11, 4, 18, 12, 14, 12, 14, 12, 18, 18, 3,
> 18, 3, 18, 14, 18, 2]);;
gap> r := RClass(s, f);;
gap> r = RClass(s, Transformation([12, 14, 3, 11, 4, 18, 12, 14, 12, 14, 12,
> 18, 18, 3, 18, 3, 18, 14, 18, 2]));
true
gap> NrHClasses(r);
177
gap> GreensHClasses(r);;
gap> Length(HClassReps(r)) = NrHClasses(r);
true
gap> ForAll([1 .. 177], i -> HClassReps(r)[i] in GreensHClasses(r)[i]);
true
gap> gens := [Transformation([2, 6, 1, 7, 5, 3, 4]),
>   Transformation([5, 3, 7, 2, 1, 6, 4]),
>   Transformation([2, 5, 5, 3, 4, 2, 3]),
>   Transformation([1, 5, 1, 6, 1, 5, 6]),
>   Transformation([6, 2, 2, 2, 5, 1, 2]),
>   Transformation([7, 5, 4, 4, 4, 5, 5]),
>   Transformation([5, 1, 6, 1, 1, 5, 1]),
>   Transformation([3, 5, 2, 3, 2, 2, 3])];;
gap> s := Semigroup(gens);;
gap> Size(s);
21343
gap> f := Transformation([7, 3, 7, 7, 7, 7, 3]);;
gap> r := RClass(s, f);
<Green's R-class: Transformation( [ 7, 3, 7, 7, 7, 7, 3 ] )>
gap> Transformation([2, 5, 2, 2, 2, 2, 5]) in last;
true
gap> NrHClasses(r);
21
gap> gens := [Transformation([3, 6, 9, 1, 4, 7, 2, 5, 8]),
>   Transformation([3, 6, 9, 7, 1, 4, 5, 8, 2]),
>   Transformation([8, 2, 5, 5, 4, 5, 5, 2, 8]),
>   Transformation([1, 1, 9, 1, 1, 6, 1, 1, 3]),
>   Transformation([9, 2, 2, 8, 5, 8, 8, 8, 5])];;
gap> s := Semigroup(gens);;
gap> iter := IteratorOfRClasses(s);
<iterator>
gap> NextIterator(iter);;
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 8, 2, 5, 5, 4, 5, 5, 2, 8 ] )>
gap> Transformation([8, 2, 5, 5, 4, 5, 5, 2, 8]) in last;
true
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 4, 4, 5, 4, 4, 2, 4, 4, 8 ] )>
gap> Transformation([4, 4, 5, 4, 4, 2, 4, 4, 8]) in last;
true
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 4, 8, 8, 5, 2, 5, 5, 5, 2 ] )>
gap> Transformation([4, 8, 8, 5, 2, 5, 5, 5, 2]) in last;
true
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 5, 5, 8, 8, 5, 5, 2, 4, 2 ] )>
gap> Transformation([5, 5, 8, 8, 5, 5, 2, 4, 2]) in last;
true
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 5, 5, 8, 5, 8, 5, 4, 2, 2 ] )>
gap> Transformation([5, 5, 8, 5, 8, 5, 4, 2, 2]) in last;
true
gap> R := NextIterator(iter);;
gap> R = RClass(s, Transformation([2, 2, 4, 4, 5, 4, 4, 2, 2]));
true
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 8, 8, 8, 8, 8, 5, 8, 8, 5 ] )>
gap> Transformation([8, 8, 8, 8, 8, 5, 8, 8, 5]) in last;
true
gap> iter := IteratorOfRClasses(s);
<iterator>
gap> NextIterator(iter);;
gap> Representative(last) in s;
true
gap> Size(s);
82953
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 8, 2, 5, 5, 4, 5, 5, 2, 8 ] )>
gap> Transformation([8, 2, 5, 5, 4, 5, 5, 2, 8]) in last;
true
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 4, 4, 5, 4, 4, 2, 4, 4, 8 ] )>
gap> Transformation([4, 4, 5, 4, 4, 2, 4, 4, 8]) in last;
true
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 4, 8, 8, 5, 2, 5, 5, 5, 2 ] )>
gap> Transformation([4, 8, 8, 5, 2, 5, 5, 5, 2]) in last;
true
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 5, 5, 8, 8, 5, 5, 2, 4, 2 ] )>
gap> Transformation([5, 5, 8, 8, 5, 5, 2, 4, 2]) in last;
true
gap> GreensRClasses(s);;
gap> iter := IteratorOfRClasses(s);
<iterator>
gap> for i in [1 .. 10] do NextIterator(iter); od;
gap> iter2 := IteratorOfRClasses(s);
<iterator>
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ] )>
gap> Transformation([1, 1, 1, 1, 1, 1, 1, 1, 1]) in last;
true
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 8, 5, 5, 5, 5, 5, 5, 5, 5 ] )>
gap> Transformation([8, 5, 5, 5, 5, 5, 5, 5, 5]) in last;
true
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 8, 5, 2, 4, 5, 5, 8, 2, 5 ] )>
gap> Transformation([8, 5, 2, 4, 5, 5, 8, 2, 5]) in last;
true
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 8, 5, 2, 5, 4, 5, 2, 5, 8 ] )>
gap> Transformation([8, 5, 2, 5, 4, 5, 2, 5, 8]) in last;
true
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 5, 8, 2, 2, 5, 2, 2, 8, 5 ] )>
gap> Transformation([5, 8, 2, 2, 5, 2, 2, 8, 5]) in last;
true
gap> iter1 := IteratorOfRClasses(s);;
gap> iter3 := IteratorOfRClasses(s);;
gap> iter2 := IteratorOfRClasses(s);;
gap> for i in [1 .. 100] do
>   r1 := NextIterator(iter1);
>   r2 := NextIterator(iter2);
>   r3 := NextIterator(iter3);
>   if r1 <> r2 or r2 <> r3 then
>     ErrorNoReturn("Problem in IteratorOfRClasses 1");
>   fi;
> od;
gap> for i in iter1 do od;
gap> IsDoneIterator(iter1);
true
gap> IsDoneIterator(iter2);
false
gap> IsDoneIterator(iter3);
false
gap> GreensDClasses(s);;
gap> for i in [1 .. 100] do
> r2 := NextIterator(iter2);
> r3 := NextIterator(iter3);
> if r2 <> r3 then
>   ErrorNoReturn("Problem in IteratorOfRClasses 2");
> fi;
> od;
gap> iter2;
<iterator>
gap> ShallowCopy(iter2);
<iterator>
gap> iter := last;
<iterator>
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 5, 2, 5, 8, 8, 4, 2, 5, 5 ] )>
gap> Transformation([5, 2, 5, 8, 8, 4, 2, 5, 5]) in last;
true
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 2, 8, 8, 8, 8, 8, 5, 8, 8 ] )>
gap> Transformation([2, 8, 8, 8, 8, 8, 5, 8, 8]) in last;
true
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 2, 8, 8, 8, 8, 8, 8, 8, 5 ] )>
gap> Transformation([2, 8, 8, 8, 8, 8, 8, 8, 5]) in last;
true
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 2, 5, 8, 8, 8, 8, 8, 8, 8 ] )>
gap> Transformation([2, 5, 8, 8, 8, 8, 8, 8, 8]) in last;
true
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 8, 8, 5, 5, 2, 2, 2, 5, 2 ] )>
gap> Transformation([8, 8, 5, 5, 2, 2, 2, 5, 2]) in last;
true
gap> iter3 := IteratorOfRClasses(s);
<iterator>
gap> out := [];
[  ]
gap> for i in iter3 do Add(out, i); od;
gap> Length(out);
503
gap> NrRClasses(s);
503
gap> s := Semigroup(gens);
<transformation semigroup of degree 9 with 5 generators>
gap> iter1 := IteratorOfRClasses(s);
<iterator>
gap> for i in iter1 do
> od;
gap> iter1 := IteratorOfRClasses(s);
<iterator>
gap> j := 0;
0
gap> for i in iter1 do
>   j := j + 1;
>   if i <> out[j] then
>     ErrorNoReturn("Problems with IteratorOfRClasses 3");
>   fi;
> od;
gap> s := Semigroup(gens);;
gap> GreensRClasses(s) = out;
true
gap> s := Semigroup(gens);;
gap> f := Transformation([8, 8, 8, 8, 8, 8, 8, 8, 8]);;
gap> r := RClass(s, f);
<Green's R-class: Transformation( [ 8, 8, 8, 8, 8, 8, 8, 8, 8 ] )>
gap> Transformation([1, 1, 1, 1, 1, 1, 1, 1, 1]) in last;
true
gap> GreensRClasses(s) = out;
true
gap> s := Semigroup(gens);;
gap> f := Transformation([6, 6, 6, 4, 6, 4, 4, 4, 6]);;
gap> f in s;
true
gap> GreensRClasses(s) = out;
true
gap> iter := IteratorOfRClasses(s);
<iterator>
gap> for i in [1 .. 500] do NextIterator(iter); od;
gap> s := Semigroup(gens);;
gap> iter := IteratorOfRClasses(s);;
gap> for i in [1 .. 1000] do NextIterator(iter); od;
gap> iter := ShallowCopy(iter);
<iterator>
gap> out := [];;
gap> for i in iter do Add(out, i); od;
gap> Set(out) = Set(GreensRClasses(s));
true
gap> gens := [Transformation([1, 2, 5, 4, 3, 8, 7, 6]),
>   Transformation([1, 6, 3, 4, 7, 2, 5, 8]),
>   Transformation([2, 1, 6, 7, 8, 3, 4, 5]),
>   Transformation([3, 2, 3, 6, 1, 6, 1, 2]),
>   Transformation([5, 2, 3, 6, 3, 4, 7, 4])];;
gap> s := Semigroup(gens);
<transformation semigroup of degree 8 with 5 generators>
gap> f := Transformation([2, 5, 4, 7, 4, 3, 6, 3]);;
gap> r := RClass(s, f);
<Green's R-class: Transformation( [ 2, 5, 4, 7, 4, 3, 6, 3 ] )>
gap> Transformation([2, 5, 4, 7, 4, 3, 6, 3]) in last;
true
gap> NrHClasses(r);
12
gap> d := DClass(r);
<Green's D-class: Transformation( [ 2, 5, 4, 7, 4, 3, 6, 3 ] )>
gap> Transformation([2, 5, 4, 7, 4, 3, 6, 3]) in last;
true
gap> NrHClasses(d);
72
gap> l := LClass(s, f);
<Green's L-class: Transformation( [ 2, 5, 4, 7, 4, 3, 6, 3 ] )>
gap> Transformation([2, 5, 4, 7, 4, 3, 6, 3]) in last;
true
gap> NrHClasses(l);
6
gap> NrHClasses(s);
1555
gap> gens := [Transformation([4, 6, 5, 2, 1, 3]),
>   Transformation([6, 3, 2, 5, 4, 1]),
>   Transformation([1, 2, 4, 3, 5, 6]),
>   Transformation([3, 5, 6, 1, 2, 3]),
>   Transformation([5, 3, 6, 6, 6, 2]),
>   Transformation([2, 3, 2, 6, 4, 6]),
>   Transformation([2, 1, 2, 2, 2, 4]),
>   Transformation([4, 4, 1, 2, 1, 2])];;
gap> s := Semigroup(gens);;
gap> NrRClasses(s);
150
gap> Size(s);
6342
gap> f := Transformation([1, 3, 3, 1, 3, 5]);;
gap> d := DClass(s, f);
<Green's D-class: Transformation( [ 1, 3, 3, 1, 3, 5 ] )>
gap> Transformation([2, 4, 2, 2, 2, 1]) in last;
true
gap> NrRClasses(d);
87
gap> s := Semigroup(Transformation([2, 1, 4, 5, 6, 3]),
> Transformation([2, 3, 1, 5, 4, 1]));;
gap> f := Transformation([2, 1, 4, 5, 6, 3]);;
gap> GreensLClassOfElement(s, f);
<Green's L-class: Transformation( [ 2, 1, 4, 5, 6, 3 ] )>
gap> Transformation([2, 1, 4, 5, 6, 3]) in last;
true
gap> s := Semigroup(Transformation([4, 4, 3, 5, 3]),
> Transformation([5, 1, 1, 4, 1]),
> Transformation([5, 5, 4, 4, 5]));;
gap> f := Transformation([4, 5, 5, 5, 5]);;
gap> SchutzenbergerGroup(DClass(s, f));
Group([ (4,5) ])
gap> SchutzenbergerGroup(RClass(s, f));
Group([ (4,5) ])
gap> SchutzenbergerGroup(LClass(s, f));
Group([ (4,5) ])
gap> SchutzenbergerGroup(HClass(s, f));
Group([ (4,5) ])
gap> s := FullTransformationSemigroup(8);;
gap> f := Transformation([1, 1, 2, 3, 4, 5, 6, 7]);;
gap> Size(s);
16777216
gap> iter := IteratorOfDClasses(s);
<iterator>
gap> d := NextIterator(iter);;
gap> d := NextIterator(iter);
<Green's D-class: Transformation( [ 1, 2, 3, 4, 5, 6, 7, 1 ] )>
gap> Transformation([1, 2, 3, 4, 5, 6, 7, 1]) in last;
true
gap> r := GreensRClassOfElement(s, f);
<Green's R-class: Transformation( [ 1, 1, 2, 3, 4, 5, 6, 7 ] )>
gap> Transformation([1, 1, 2, 3, 4, 5, 6, 7]) in last;
true
gap> elts := Elements(r);;
gap> for i in elts do if not i in d then Print("Error 1");fi; od;
gap> rr := GreensRClassOfElement(s, Transformation([1, 1, 1, 2, 3, 4, 5, 6]));;
gap> rr = RClass(s, Transformation([1, 1, 1, 3, 4, 5, 6, 7]));
true
gap> elts := Elements(rr);; d := DClassOfRClass(rr);;
gap> for i in elts do if not i in d then Print("Error 2");  fi; od;
gap> Sum(List(GreensDClasses(s), Size)); 8 ^ 8;
16777216
16777216
gap> iter := IteratorOfDClassReps(s);
<iterator>
gap> for i in [1 .. 8] do NextIterator(iter); od;
gap> IsDoneIterator(iter);
true
gap> s := FullTransformationSemigroup(10);;
gap> f := Transformation([8, 10, 8, 5, 6, 10, 7, 2, 9, 9]);;
gap> d := GreensDClassOfElementNC(s, f);;  # 1s with NC check efficiency here!
gap> Idempotents(d);;  # 1.3s
gap> NrIdempotents(d);
41160
gap> ForAll(Idempotents(d), IsIdempotent);
true
gap> gens := [Transformation([2, 8, 3, 7, 1, 5, 2, 6]),
>   Transformation([3, 5, 7, 2, 5, 6, 3, 8]),
>  Transformation([4, 1, 8, 3, 5, 7, 3, 5]),
>  Transformation([4, 3, 4, 5, 6, 4, 1, 2]),
>  Transformation([5, 4, 8, 8, 5, 6, 1, 5]),
>  Transformation([6, 7, 4, 1, 4, 1, 6, 2]),
>  Transformation([7, 1, 2, 2, 2, 7, 4, 5]),
>  Transformation([8, 8, 5, 1, 7, 5, 2, 8])];;
gap> s := Semigroup(gens);;
gap> f := Transformation([5, 2, 7, 2, 7, 2, 5, 8]);;
gap> d := GreensDClassOfElementNC(s, f);;
gap> d := GreensDClassOfElement(s, f);;
gap> RClassReps(d);;
gap> NrRClasses(d);
260
gap> s := FullTransformationSemigroup(5);;
gap> f := Transformation([5, 5, 1, 1, 3]);;
gap> d := GreensDClassOfElement(s, f);;
gap> ForAll(RClassReps(d), x -> x in d);
true
gap> Length(Set(RClassReps(d), x -> RClass(d, x))) = Length(RClassReps(d));
true
gap> gens := [Transformation([2, 8, 3, 7, 1, 5, 2, 6]),
>  Transformation([3, 5, 7, 2, 5, 6, 3, 8]),
>  Transformation([6, 7, 4, 1, 4, 1, 6, 2]),
>  Transformation([8, 8, 5, 1, 7, 5, 2, 8])];;
gap> s := Semigroup(gens);;
gap> f := Transformation([1, 1, 7, 6, 6, 7, 2, 1]);;
gap> d := GreensDClassOfElement(s, f);
<Green's D-class: Transformation( [ 1, 1, 7, 6, 6, 7, 2, 1 ] )>
gap> Transformation([1, 1, 7, 6, 6, 7, 2, 1]) in last;
true
gap> RClassReps(d);;
gap> NrRClasses(d);
1728
gap> gens := [Transformation([5, 6, 7, 3, 1, 4, 2, 8]),
> Transformation([3, 6, 8, 5, 7, 4, 2, 8])];;
gap> s := Semigroup(gens);;
gap> f := Transformation([4, 8, 8, 7, 6, 2, 5, 8]);;
gap> r := GreensRClassOfElement(s, f);;
gap> r = RClass(s, Transformation([4, 8, 8, 7, 6, 2, 5, 8]));
true
gap> d := DClassOfRClass(r);;
gap> d = DClass(s, Transformation([4, 8, 8, 7, 6, 2, 5, 8]));
true
gap> gens := [Transformation([5, 6, 7, 3, 1, 4, 2, 8]),
>  Transformation([3, 6, 8, 5, 7, 4, 2, 8])];;
gap> s := Semigroup(gens);;
gap> f := Transformation([3, 6, 8, 5, 7, 4, 2, 8]);;
gap> r := GreensRClassOfElement(s, f);
<Green's R-class: Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] )>
gap> Transformation([3, 6, 8, 5, 7, 4, 2, 8]) in last;
true
gap> d := DClassOfRClass(r);
<Green's D-class: Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] )>
gap> Transformation([3, 6, 8, 5, 7, 4, 2, 8]) in last;
true
gap> R := GreensRClasses(d);
[ <Green's R-class: Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] )>,
  <Green's R-class: Transformation( [ 7, 4, 2, 8, 3, 5, 6, 8 ] )>,
  <Green's R-class: Transformation( [ 3, 5, 6, 2, 7, 8, 4, 8 ] )>,
  <Green's R-class: Transformation( [ 7, 8, 4, 6, 3, 2, 5, 8 ] )>,
  <Green's R-class: Transformation( [ 3, 2, 5, 4, 7, 6, 8, 8 ] )>,
  <Green's R-class: Transformation( [ 7, 6, 8, 5, 3, 4, 2, 8 ] )>,
  <Green's R-class: Transformation( [ 3, 4, 2, 8, 7, 5, 6, 8 ] )>,
  <Green's R-class: Transformation( [ 7, 5, 6, 2, 3, 8, 4, 8 ] )>,
  <Green's R-class: Transformation( [ 3, 8, 4, 6, 7, 2, 5, 8 ] )>,
  <Green's R-class: Transformation( [ 7, 2, 5, 4, 3, 6, 8, 8 ] )> ]
gap> Transformation([3, 6, 8, 5, 7, 4, 2, 8])
> in RClass(s, Transformation([3, 6, 8, 5, 7, 4, 2, 8]));
true
gap> Transformation([7, 4, 2, 8, 3, 5, 6, 8])
> in RClass(s, Transformation([7, 4, 2, 8, 3, 5, 6, 8]));
true
gap> Transformation([3, 5, 6, 2, 7, 8, 4, 8])
> in RClass(s, Transformation([3, 5, 6, 2, 7, 8, 4, 8]));
true
gap> Transformation([7, 8, 4, 6, 3, 2, 5, 8])
> in RClass(s, Transformation([7, 8, 4, 6, 3, 2, 5, 8]));
true
gap> Transformation([3, 2, 5, 4, 7, 6, 8, 8])
> in RClass(s, Transformation([3, 2, 5, 4, 7, 6, 8, 8]));
true
gap> Transformation([7, 6, 8, 5, 3, 4, 2, 8])
> in RClass(s, Transformation([7, 6, 8, 5, 3, 4, 2, 8]));
true
gap> Transformation([3, 4, 2, 8, 7, 5, 6, 8])
> in RClass(s, Transformation([3, 4, 2, 8, 7, 5, 6, 8]));
true
gap> Transformation([7, 5, 6, 2, 3, 8, 4, 8])
> in RClass(s, Transformation([7, 5, 6, 2, 3, 8, 4, 8]));
true
gap> Transformation([3, 8, 4, 6, 7, 2, 5, 8])
> in RClass(s, Transformation([3, 8, 4, 6, 7, 2, 5, 8]));
true
gap> Transformation([7, 2, 5, 4, 3, 6, 8, 8])
> in RClass(s, Transformation([7, 2, 5, 4, 3, 6, 8, 8]));
true
gap> r := R[10];;
gap> HClassReps(r);
[ Transformation( [ 7, 2, 5, 4, 3, 6, 8, 8 ] ),
  Transformation( [ 3, 7, 1, 6, 4, 2, 8, 8 ] ),
  Transformation( [ 4, 3, 5, 2, 6, 7, 8, 8 ] ),
  Transformation( [ 6, 4, 1, 7, 2, 3, 8, 8 ] ),
  Transformation( [ 2, 6, 5, 3, 7, 4, 8, 8 ] ),
  Transformation( [ 7, 2, 1, 4, 3, 6, 8, 8 ] ),
  Transformation( [ 3, 7, 5, 6, 4, 2, 8, 8 ] ),
  Transformation( [ 4, 3, 1, 2, 6, 7, 8, 8 ] ),
  Transformation( [ 6, 4, 5, 7, 2, 3, 8, 8 ] ),
  Transformation( [ 2, 6, 1, 3, 7, 4, 8, 8 ] ) ]

# GreensTest2
gap> t := FullTransformationSemigroup(5);;
gap> iter := Iterator(t);
<iterator of semigroup>
gap> for i in iter do od;
gap> IsDoneIterator(iter);
true
gap> iter := ShallowCopy(iter);
<iterator of semigroup>
gap> NextIterator(iter);
Transformation( [ 1, 1, 1, 1, 1 ] )
gap> NextIterator(iter);
Transformation( [ 1, 1, 1, 1, 2 ] )
gap> NextIterator(iter);
Transformation( [ 1, 1, 1, 1, 3 ] )
gap> NextIterator(iter);
Transformation( [ 1, 1, 1, 1, 4 ] )
gap> NextIterator(iter);
Transformation( [ 1, 1, 1, 1 ] )
gap> NextIterator(iter);
Transformation( [ 1, 1, 1, 2, 1 ] )
gap> IsDoneIterator(iter);
false
gap> for i in iter do od;
gap> IsDoneIterator(iter);
true

# GreensTest3: GreensLClassOfElement(D-class, transformation);
gap> gens := [Transformation([2, 8, 3, 7, 1, 5, 2, 6]),
>   Transformation([3, 5, 7, 2, 5, 6, 3, 8]),
>   Transformation([4, 1, 8, 3, 5, 7, 3, 5]),
>   Transformation([4, 3, 4, 5, 6, 4, 1, 2]),
>   Transformation([5, 4, 8, 8, 5, 6, 1, 5]),
>   Transformation([6, 7, 4, 1, 4, 1, 6, 2]),
>   Transformation([7, 1, 2, 2, 2, 7, 4, 5]),
>   Transformation([8, 8, 5, 1, 7, 5, 2, 8])];;
gap> s := Semigroup(gens);;
gap> f := Transformation([3, 2, 5, 3, 5, 3, 3, 3]);;
gap> d := DClass(s, f);;
gap> d = DClass(s, Transformation([8, 1, 5, 8, 5, 8, 8, 8]));
true
gap> f := Transformation([6, 4, 8, 8, 8, 4, 6, 4]);;
gap> l := LClass(d, f);
<Green's L-class: Transformation( [ 6, 4, 8, 8, 8, 4, 6, 4 ] )>
gap> Transformation([6, 8, 4, 6, 4, 6, 6, 6]) in last;
true
gap> ll := LClass(s, f);
<Green's L-class: Transformation( [ 6, 4, 8, 8, 8, 4, 6, 4 ] )>
gap> Transformation([6, 8, 4, 6, 4, 6, 6, 6]) in last;
true
gap> l = ll;
true
gap> ll = l;
true
gap> ll < l;
false
gap> l < ll;
false
gap> Elements(l) = Elements(ll);
true
gap> Size(l); Size(ll);
2256
2256
gap> DClassOfLClass(ll) = DClassOfLClass(l);
true
gap> DClassOfLClass(ll) = d;
true
gap> NrHClasses(l);
376
gap> NrHClasses(ll);
376
gap> HClassReps(l) = HClassReps(ll);
true
gap> NrIdempotents(ll);
109
gap> NrIdempotents(l);
109
gap> IsRegularDClass(d);
true
gap> Size(s);
597369
gap> Set(HClasses(l)) = Set(HClasses(ll));
true

# GreensTest4: GreensHClassOfElement(D-class, transformation);
gap> gens := [Transformation([2, 1, 4, 5, 3, 7, 8, 9, 10, 6]),
> Transformation([1, 2, 4, 3, 5, 6, 7, 8, 9, 10]),
> Transformation([1, 2, 3, 4, 5, 6, 10, 9, 8, 7]),
> Transformation([9, 1, 4, 3, 6, 9, 3, 4, 3, 9])];;
gap> s := Monoid(gens);;
gap> GreensDClasses(s);;
gap> f := Transformation([8, 5, 10, 5, 8, 5, 5, 5, 5, 10]);;
gap> d := DClass(s, f);;
gap> d = DClass(s, Transformation([3, 9, 3, 4, 9, 3, 4, 3, 4, 3]));
true
gap> f := Transformation([6, 4, 5, 6, 4, 4, 6, 6, 4, 5]);;
gap> h := GreensHClassOfElement(d, f);
<Green's H-class: Transformation( [ 6, 4, 5, 6, 4, 4, 6, 6, 4, 5 ] )>
gap> Transformation([6, 4, 5, 6, 4, 4, 6, 6, 4, 5]) in last;
true
gap> hh := HClass(s, f);
<Green's H-class: Transformation( [ 6, 4, 5, 6, 4, 4, 6, 6, 4, 5 ] )>
gap> Transformation([6, 4, 5, 6, 4, 4, 6, 6, 4, 5]) in last;
true
gap> hh = h;
true
gap> h = hh;
true
gap> Elements(h) = Elements(hh);
true
gap> f := Transformation([4, 8, 8, 8, 4, 4, 8, 4, 4, 8]);;
gap> d := DClass(s, f);;
gap> d = DClass(s, Transformation([3, 4, 3, 4, 4, 3, 4, 3, 4, 3]));
true
gap> f := Transformation([4, 3, 3, 4, 3, 3, 3, 3, 4, 4]);;
gap> h := HClass(d, f);
<Green's H-class: Transformation( [ 4, 3, 3, 4, 3, 3, 3, 3, 4, 4 ] )>
gap> Transformation([4, 3, 3, 4, 3, 3, 3, 3, 4, 4]) in last;
true
gap> hh := HClass(s, f);;
gap> hh = h;
true
gap> h = hh;
true
gap> Elements(h) = Elements(hh);
true

# GreensTest5
gap> S := Semigroup(Transformation([3, 4, 4, 4]),
> Transformation([4, 3, 1, 2]));;
gap> GreensDClasses(S);
[ <Green's D-class: Transformation( [ 3, 4, 4, 4 ] )>,
  <Green's D-class: Transformation( [ 4, 3, 1, 2 ] )>,
  <Green's D-class: Transformation( [ 4, 4, 4, 4 ] )> ]
gap> GreensHClasses(S);
[ <Green's H-class: Transformation( [ 3, 4, 4, 4 ] )>,
  <Green's H-class: Transformation( [ 1, 2, 2, 2 ] )>,
  <Green's H-class: Transformation( [ 4, 4, 3, 4 ] )>,
  <Green's H-class: Transformation( [ 2, 2, 1, 2 ] )>,
  <Green's H-class: Transformation( [ 4, 3, 4, 4 ] )>,
  <Green's H-class: Transformation( [ 2, 1, 2, 2 ] )>,
  <Green's H-class: Transformation( [ 4, 4, 4, 3 ] )>,
  <Green's H-class: Transformation( [ 2, 2, 2, 1 ] )>,
  <Green's H-class: Transformation( [ 4, 3, 1, 2 ] )>,
  <Green's H-class: Transformation( [ 4, 4, 4, 4 ] )>,
  <Green's H-class: Transformation( [ 2, 2, 2, 2 ] )>,
  <Green's H-class: Transformation( [ 3, 3, 3, 3 ] )>,
  <Green's H-class: Transformation( [ 1, 1, 1, 1 ] )> ]
gap> GreensLClasses(S);
[ <Green's L-class: Transformation( [ 3, 4, 4, 4 ] )>,
  <Green's L-class: Transformation( [ 1, 2, 2, 2 ] )>,
  <Green's L-class: Transformation( [ 4, 3, 1, 2 ] )>,
  <Green's L-class: Transformation( [ 4, 4, 4, 4 ] )>,
  <Green's L-class: Transformation( [ 2, 2, 2, 2 ] )>,
  <Green's L-class: Transformation( [ 3, 3, 3, 3 ] )>,
  <Green's L-class: Transformation( [ 1, 1, 1, 1 ] )> ]
gap> GreensRClasses(S);
[ <Green's R-class: Transformation( [ 3, 4, 4, 4 ] )>,
  <Green's R-class: Transformation( [ 4, 3, 1, 2 ] )>,
  <Green's R-class: Transformation( [ 4, 4, 4, 4 ] )>,
  <Green's R-class: Transformation( [ 4, 4, 3, 4 ] )>,
  <Green's R-class: Transformation( [ 4, 3, 4, 4 ] )>,
  <Green's R-class: Transformation( [ 4, 4, 4, 3 ] )> ]
gap> D := GreensDClasses(S)[1];
<Green's D-class: Transformation( [ 3, 4, 4, 4 ] )>
gap> Transformation([3, 4, 4, 4]) in D;
true
gap> GreensLClasses(D);
[ <Green's L-class: Transformation( [ 3, 4, 4, 4 ] )>,
  <Green's L-class: Transformation( [ 1, 2, 2, 2 ] )> ]
gap> GreensRClasses(D);
[ <Green's R-class: Transformation( [ 3, 4, 4, 4 ] )>,
  <Green's R-class: Transformation( [ 4, 4, 3, 4 ] )>,
  <Green's R-class: Transformation( [ 4, 3, 4, 4 ] )>,
  <Green's R-class: Transformation( [ 4, 4, 4, 3 ] )> ]
gap> GreensHClasses(D);
[ <Green's H-class: Transformation( [ 3, 4, 4, 4 ] )>,
  <Green's H-class: Transformation( [ 1, 2, 2, 2 ] )>,
  <Green's H-class: Transformation( [ 4, 4, 3, 4 ] )>,
  <Green's H-class: Transformation( [ 2, 2, 1, 2 ] )>,
  <Green's H-class: Transformation( [ 4, 3, 4, 4 ] )>,
  <Green's H-class: Transformation( [ 2, 1, 2, 2 ] )>,
  <Green's H-class: Transformation( [ 4, 4, 4, 3 ] )>,
  <Green's H-class: Transformation( [ 2, 2, 2, 1 ] )> ]
gap> R := GreensRClasses(D)[1];
<Green's R-class: Transformation( [ 3, 4, 4, 4 ] )>
gap> Transformation([3, 4, 4, 4]) in last;
true
gap> GreensHClasses(R);
[ <Green's H-class: Transformation( [ 3, 4, 4, 4 ] )>,
  <Green's H-class: Transformation( [ 1, 2, 2, 2 ] )> ]
gap> Transformation([1, 2, 2, 2])
> in HClass(R, Transformation([1, 2, 2, 2]));
true
gap> L := GreensLClasses(D)[1];
<Green's L-class: Transformation( [ 3, 4, 4, 4 ] )>
gap> Transformation([3, 4, 4, 4]) in L;
true
gap> GreensHClasses(L);
[ <Green's H-class: Transformation( [ 3, 4, 4, 4 ] )>,
  <Green's H-class: Transformation( [ 4, 4, 3, 4 ] )>,
  <Green's H-class: Transformation( [ 4, 3, 4, 4 ] )>,
  <Green's H-class: Transformation( [ 4, 4, 4, 3 ] )> ]

# GreensTest6
gap> S := Semigroup(Transformation([3, 4, 4, 4]),
> Transformation([4, 3, 1, 2]));;
gap> DClassReps(S);
[ Transformation( [ 3, 4, 4, 4 ] ), Transformation( [ 4, 3, 1, 2 ] ),
  Transformation( [ 4, 4, 4, 4 ] ) ]
gap> HClassReps(S);
[ Transformation( [ 3, 4, 4, 4 ] ), Transformation( [ 1, 2, 2, 2 ] ),
  Transformation( [ 4, 3, 1, 2 ] ), Transformation( [ 4, 4, 4, 4 ] ),
  Transformation( [ 2, 2, 2, 2 ] ), Transformation( [ 3, 3, 3, 3 ] ),
  Transformation( [ 1, 1, 1, 1 ] ), Transformation( [ 4, 4, 3, 4 ] ),
  Transformation( [ 2, 2, 1, 2 ] ), Transformation( [ 4, 3, 4, 4 ] ),
  Transformation( [ 2, 1, 2, 2 ] ), Transformation( [ 4, 4, 4, 3 ] ),
  Transformation( [ 2, 2, 2, 1 ] ) ]
gap> LClassReps(S);
[ Transformation( [ 3, 4, 4, 4 ] ), Transformation( [ 1, 2, 2, 2 ] ),
  Transformation( [ 4, 3, 1, 2 ] ), Transformation( [ 4, 4, 4, 4 ] ),
  Transformation( [ 2, 2, 2, 2 ] ), Transformation( [ 3, 3, 3, 3 ] ),
  Transformation( [ 1, 1, 1, 1 ] ) ]
gap> RClassReps(S);
[ Transformation( [ 3, 4, 4, 4 ] ), Transformation( [ 4, 3, 1, 2 ] ),
  Transformation( [ 4, 4, 4, 4 ] ), Transformation( [ 4, 4, 3, 4 ] ),
  Transformation( [ 4, 3, 4, 4 ] ), Transformation( [ 4, 4, 4, 3 ] ) ]
gap> D := GreensDClasses(S)[1];
<Green's D-class: Transformation( [ 3, 4, 4, 4 ] )>
gap> Transformation([3, 4, 4, 4]) in last;
true
gap> LClassReps(D);
[ Transformation( [ 3, 4, 4, 4 ] ), Transformation( [ 1, 2, 2, 2 ] ) ]
gap> HClassReps(D);
[ Transformation( [ 3, 4, 4, 4 ] ), Transformation( [ 1, 2, 2, 2 ] ),
  Transformation( [ 4, 4, 3, 4 ] ), Transformation( [ 2, 2, 1, 2 ] ),
  Transformation( [ 4, 3, 4, 4 ] ), Transformation( [ 2, 1, 2, 2 ] ),
  Transformation( [ 4, 4, 4, 3 ] ), Transformation( [ 2, 2, 2, 1 ] ) ]
gap> RClassReps(D);
[ Transformation( [ 3, 4, 4, 4 ] ), Transformation( [ 4, 4, 3, 4 ] ),
  Transformation( [ 4, 3, 4, 4 ] ), Transformation( [ 4, 4, 4, 3 ] ) ]
gap> R := GreensRClasses(D)[1];;
gap> HClassReps(R);
[ Transformation( [ 3, 4, 4, 4 ] ), Transformation( [ 1, 2, 2, 2 ] ) ]
gap> L := GreensLClasses(D)[1];;
gap> HClassReps(L);
[ Transformation( [ 3, 4, 4, 4 ] ), Transformation( [ 4, 4, 3, 4 ] ),
  Transformation( [ 4, 3, 4, 4 ] ), Transformation( [ 4, 4, 4, 3 ] ) ]

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/greens-acting.tst");
