#############################################################################
##
#W  extreme/inverse.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local D, IsIsometryPP, L, R, S, acting, ccong, classx, classy, classz, cong
#@local d, f, gens, h, i, inv, iso, iter, j, k, l, m, n, pairs, q, r, s, x, y, z
gap> START_TEST("Semigroups package: extreme/inverse.tst");
gap> LoadPackage("semigroups", false);;

# 
gap> SEMIGROUPS.StartTest();
gap> SEMIGROUPS.DefaultOptionsRec.acting := true;;

# InverseTest1
gap> gens := [PartialPermNC([1, 2, 4], [1, 5, 2]),
> PartialPermNC([1, 2, 3], [2, 3, 5]),
> PartialPermNC([1, 3, 4], [2, 5, 4]),
> PartialPermNC([1, 2, 4], [3, 1, 2]),
> PartialPermNC([1, 2, 3], [3, 1, 4]),
> PartialPermNC([1, 2, 4], [3, 5, 2]),
> PartialPermNC([1, 2, 3, 4], [4, 1, 5, 2]),
> PartialPermNC([1, 2, 4, 5], [4, 3, 5, 2]),
> PartialPermNC([1, 2, 3, 4, 5], [5, 2, 4, 3, 1]),
> PartialPermNC([1, 3, 5], [5, 4, 1])];;
gap> s := InverseSemigroup(gens);
<inverse partial perm semigroup of rank 5 with 10 generators>
gap> Size(s);
860
gap> NrRClasses(s);
31
gap> RClassReps(s);
[ <identity partial perm on [ 1, 2, 5 ]>, [4,2,5](1), [3,2,5,1], [4,1](2)(5), 
  [3,1,5](2), [3,5][4,1,2], [3,5,2][4,1], [4,5,1,2], [3,5,2](1), 
  [3,2,5][4,1], <identity partial perm on [ 1, 2, 4, 5 ]>, [3,2,5,4,1], 
  [3,5](1,4,2), [3,4](1,5)(2), <identity partial perm on [ 1, 2, 3, 4, 5 ]>, 
  <identity partial perm on [ 2, 3 ]>, [1,2][5,3], [4,3](2), [5,3,2], 
  [1,2][4,3], [4,2][5,3], [1,3,2], [4,2](3), [5,2,3], [1,2,3], 
  <identity partial perm on [ 2 ]>, [5,2], [1,2], [3,2], [4,2], 
  <empty partial perm> ]
gap> List(last, DomainOfPartialPerm);
[ [ 1, 2, 5 ], [ 1, 2, 4 ], [ 2, 3, 5 ], [ 2, 4, 5 ], [ 1, 2, 3 ], 
  [ 1, 3, 4 ], [ 3, 4, 5 ], [ 1, 4, 5 ], [ 1, 3, 5 ], [ 2, 3, 4 ], 
  [ 1, 2, 4, 5 ], [ 2, 3, 4, 5 ], [ 1, 2, 3, 4 ], [ 1, 2, 3, 5 ], 
  [ 1, 2, 3, 4, 5 ], [ 2, 3 ], [ 1, 5 ], [ 2, 4 ], [ 3, 5 ], [ 1, 4 ], 
  [ 4, 5 ], [ 1, 3 ], [ 3, 4 ], [ 2, 5 ], [ 1, 2 ], [ 2 ], [ 5 ], [ 1 ], 
  [ 3 ], [ 4 ], [  ] ]
gap> IsDuplicateFreeList(last);
true

# InverseTest2
gap> s := InverseSemigroup(PartialPermNC([1, 2], [1, 2]),
> PartialPermNC([1, 2], [1, 3]));;
gap> GreensHClasses(s);
[ <Green's H-class: <identity partial perm on [ 1, 2 ]>>, 
  <Green's H-class: [2,3](1)>, <Green's H-class: [3,2](1)>, 
  <Green's H-class: <identity partial perm on [ 1, 3 ]>>, 
  <Green's H-class: <identity partial perm on [ 1 ]>> ]
gap> s := InverseSemigroup(Generators(s));;
gap> HClassReps(s);
[ <identity partial perm on [ 1, 2 ]>, [2,3](1), [3,2](1), 
  <identity partial perm on [ 1, 3 ]>, <identity partial perm on [ 1 ]> ]
gap> GreensHClasses(s);
[ <Green's H-class: <identity partial perm on [ 1, 2 ]>>, 
  <Green's H-class: [2,3](1)>, <Green's H-class: [3,2](1)>, 
  <Green's H-class: <identity partial perm on [ 1, 3 ]>>, 
  <Green's H-class: <identity partial perm on [ 1 ]>> ]

# InverseTest3
gap> gens := [PartialPermNC([1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19],
> [9, 18, 20, 11, 5, 16, 8, 19, 14, 13, 1]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 14, 16, 18, 19, 20],
> [13, 1, 8, 5, 4, 14, 11, 12, 9, 20, 2, 18, 7, 3, 19])];;
gap> s := InverseSemigroup(gens);;
gap> d := DClass(s, Generators(s)[1]);
<Green's D-class: [2,18][3,20][6,5,11,19,1,9][7,16,13][12,14](8)>
gap> PartialPerm([1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20], 
>                [1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20]) in d;
true
gap> Size(s);
60880
gap> h := HClass(d, Generators(s)[1]);
<Green's H-class: [2,18][3,20][6,5,11,19,1,9][7,16,13][12,14](8)>
gap> Generators(s)[1] in h;
true
gap> Generators(s)[1] in h;
true
gap> DClassOfHClass(h) = d;
true
gap> DClassOfHClass(h);
<Green's D-class: [2,18][3,20][6,5,11,19,1,9][7,16,13][12,14](8)>
gap> PartialPerm([1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20], 
>                [1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20]) in last;
true
gap> r := RClassOfHClass(h);
<Green's R-class: [2,18][3,20][6,5,11,19,1,9][7,16,13][12,14](8)>
gap> Representative(h) in r;
true
gap> ForAll(h, x -> x in r);
true
gap> l := LClass(h);
<Green's L-class: [2,18][3,20][6,5,11,19,1,9][7,16,13][12,14](8)>
gap> PartialPerm([1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20], 
>                [1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20]) in l;
true
gap> ForAll(h, x -> x in l);
true
gap> Representative(l) in l;
true
gap> IsGreensLClass(l);
true
gap> DClassOfLClass(l) = d;
true
gap> DClassOfRClass(r) = d;
true
gap> S := InverseSemigroup(PartialPermNC([1, 2, 3, 6, 8, 10],
> [2, 6, 7, 9, 1, 5]), PartialPermNC([1, 2, 3, 4, 6, 7, 8, 10],
> [3, 8, 1, 9, 4, 10, 5, 6]));
<inverse partial perm semigroup of rank 10 with 2 generators>
gap> f := Generators(S)[1];
[3,7][8,1,2,6,9][10,5]
gap> h := HClass(S, f);
<Green's H-class: [3,7][8,1,2,6,9][10,5]>
gap> IsGreensHClass(h);
true
gap> RClassOfHClass(h);
<Green's R-class: [3,7][8,1,2,6,9][10,5]>
gap> LClassOfHClass(h);
<Green's L-class: [3,7][8,1,2,6,9][10,5]>
gap> PartialPerm([1, 2, 5, 6, 7, 9], [1, 2, 5, 6, 7, 9]) in last;
true
gap> r := RClassOfHClass(h);
<Green's R-class: [3,7][8,1,2,6,9][10,5]>
gap> l := LClass(h);
<Green's L-class: [3,7][8,1,2,6,9][10,5]>
gap> PartialPerm([1, 2, 5, 6, 7, 9], [1, 2, 5, 6, 7, 9]) in last;
true
gap> DClass(r) = DClass(l);
true
gap> DClass(h) = DClass(l);
true
gap> f := PartialPermNC([1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19],
> [9, 18, 20, 11, 5, 16, 8, 19, 14, 13, 1]);;
gap> h := HClass(s, f);
<Green's H-class: [2,18][3,20][6,5,11,19,1,9][7,16,13][12,14](8)>
gap> ForAll(h, x -> x in RClassOfHClass(h));
true
gap> Size(h);
1
gap> IsGroupHClass(h);
false

# InverseTest5 
gap> s := Semigroup([Transformation([3, 2, 1, 6, 5, 4]),
> Transformation([4, 7, 3, 1, 6, 5, 7])]);
<transformation semigroup of degree 7 with 2 generators>
gap> iso := IsomorphismPartialPermSemigroup(s);;
gap> inv := InverseGeneralMapping(iso);;
gap> f := Transformation([1, 7, 3, 4, 5, 6, 7]);;
gap> f ^ iso;
<identity partial perm on [ 1, 3, 4, 5, 6, 7 ]>
gap> (f ^ iso) ^ inv;
Transformation( [ 1, 7, 3, 4, 5, 6, 7 ] )
gap> ForAll(s, f -> (f ^ iso) ^ inv = f);
true

# InverseTest6 
gap> s := Semigroup(Transformation([2, 5, 1, 7, 3, 7, 7]),
> Transformation([3, 6, 5, 7, 2, 1, 7]));;
gap> iso := IsomorphismPartialPermSemigroup(s);;
gap> inv := InverseGeneralMapping(iso);;
gap> f := Transformation([7, 1, 7, 7, 7, 7, 7]);;
gap> f ^ iso;
[2,1](7)
gap> (f ^ iso) ^ inv = f;
true
gap> f := Random(s);;
gap> (f ^ iso) ^ inv = f;
true
gap> f := Random(s);;
gap> (f ^ iso) ^ inv = f;
true
gap> f := Random(s);;
gap> (f ^ iso) ^ inv = f;
true
gap> f := Random(s);;
gap> (f ^ iso) ^ inv = f;
true
gap> f := Random(s);;
gap> (f ^ iso) ^ inv = f;
true
gap> f := Random(s);;
gap> (f ^ iso) ^ inv = f;
true
gap> Size(Range(iso));
631
gap> ForAll(s, f -> f ^ iso in Range(iso));
true
gap> Size(s);
631

# InverseTest8 
gap> s := InverseSemigroup(PartialPermNC([1, 2, 3], [2, 4, 1]),
> PartialPermNC([1, 3, 4], [3, 4, 1]));;
gap> GreensDClasses(s);
[ <Green's D-class: <identity partial perm on [ 1, 2, 4 ]>>, 
  <Green's D-class: <identity partial perm on [ 1, 3, 4 ]>>, 
  <Green's D-class: <identity partial perm on [ 1, 3 ]>>, 
  <Green's D-class: <identity partial perm on [ 4 ]>>, 
  <Green's D-class: <empty partial perm>> ]
gap> GreensHClasses(s);
[ <Green's H-class: <identity partial perm on [ 1, 2, 4 ]>>, 
  <Green's H-class: [4,2,1,3]>, <Green's H-class: [3,1,2,4]>, 
  <Green's H-class: <identity partial perm on [ 1, 2, 3 ]>>, 
  <Green's H-class: <identity partial perm on [ 1, 3, 4 ]>>, 
  <Green's H-class: <identity partial perm on [ 1, 3 ]>>, 
  <Green's H-class: [3,1,2]>, <Green's H-class: [1,4][3,2]>, 
  <Green's H-class: [1,3,4]>, <Green's H-class: [3,1,4]>, 
  <Green's H-class: [1,2](3)>, <Green's H-class: [2,1,3]>, 
  <Green's H-class: <identity partial perm on [ 1, 2 ]>>, 
  <Green's H-class: [1,2,4]>, <Green's H-class: [1,4][2,3]>, 
  <Green's H-class: [2,4](1)>, <Green's H-class: [1,3](2)>, 
  <Green's H-class: [2,3][4,1]>, <Green's H-class: [4,2,1]>, 
  <Green's H-class: <identity partial perm on [ 2, 4 ]>>, 
  <Green's H-class: [2,4,3]>, <Green's H-class: [2,1](4)>, 
  <Green's H-class: [4,2,3]>, <Green's H-class: [4,3,1]>, 
  <Green's H-class: [3,2][4,1]>, <Green's H-class: [3,4,2]>, 
  <Green's H-class: <identity partial perm on [ 3, 4 ]>>, 
  <Green's H-class: [3,4,1]>, <Green's H-class: [4,3,2]>, 
  <Green's H-class: [4,1,3]>, <Green's H-class: [4,2](1)>, 
  <Green's H-class: [1,2](4)>, <Green's H-class: [1,4,3]>, 
  <Green's H-class: <identity partial perm on [ 1, 4 ]>>, 
  <Green's H-class: [1,3][4,2]>, <Green's H-class: [2,1](3)>, 
  <Green's H-class: [3,1](2)>, <Green's H-class: [3,2,4]>, 
  <Green's H-class: [2,3,4]>, <Green's H-class: [2,4][3,1]>, 
  <Green's H-class: <identity partial perm on [ 2, 3 ]>>, 
  <Green's H-class: <identity partial perm on [ 4 ]>>, 
  <Green's H-class: [4,1]>, <Green's H-class: [4,3]>, 
  <Green's H-class: [4,2]>, <Green's H-class: [1,4]>, 
  <Green's H-class: <identity partial perm on [ 1 ]>>, 
  <Green's H-class: [1,3]>, <Green's H-class: [1,2]>, 
  <Green's H-class: [3,4]>, <Green's H-class: [3,1]>, 
  <Green's H-class: <identity partial perm on [ 3 ]>>, 
  <Green's H-class: [3,2]>, <Green's H-class: [2,4]>, 
  <Green's H-class: [2,1]>, <Green's H-class: [2,3]>, 
  <Green's H-class: <identity partial perm on [ 2 ]>>, 
  <Green's H-class: <empty partial perm>> ]
gap> IsDuplicateFree(last);
true
gap> GreensLClasses(s);
[ <Green's L-class: <identity partial perm on [ 1, 2, 4 ]>>, 
  <Green's L-class: [4,2,1,3]>, 
  <Green's L-class: <identity partial perm on [ 1, 3, 4 ]>>, 
  <Green's L-class: <identity partial perm on [ 1, 3 ]>>, 
  <Green's L-class: [3,1,2]>, <Green's L-class: [1,4][3,2]>, 
  <Green's L-class: [1,3,4]>, <Green's L-class: [3,1,4]>, 
  <Green's L-class: [1,2](3)>, 
  <Green's L-class: <identity partial perm on [ 4 ]>>, 
  <Green's L-class: [4,1]>, <Green's L-class: [4,3]>, 
  <Green's L-class: [4,2]>, <Green's L-class: <empty partial perm>> ]
gap> GreensRClasses(s);
[ <Green's R-class: <identity partial perm on [ 1, 2, 4 ]>>, 
  <Green's R-class: [3,1,2,4]>, 
  <Green's R-class: <identity partial perm on [ 1, 3, 4 ]>>, 
  <Green's R-class: <identity partial perm on [ 1, 3 ]>>, 
  <Green's R-class: [2,1,3]>, <Green's R-class: [2,3][4,1]>, 
  <Green's R-class: [4,3,1]>, <Green's R-class: [4,1,3]>, 
  <Green's R-class: [2,1](3)>, 
  <Green's R-class: <identity partial perm on [ 4 ]>>, 
  <Green's R-class: [1,4]>, <Green's R-class: [3,4]>, 
  <Green's R-class: [2,4]>, <Green's R-class: <empty partial perm>> ]
gap> D := GreensDClasses(s)[2];
<Green's D-class: <identity partial perm on [ 1, 3, 4 ]>>
gap> GreensLClasses(D);
[ <Green's L-class: <identity partial perm on [ 1, 3, 4 ]>> ]
gap> GreensRClasses(D);
[ <Green's R-class: <identity partial perm on [ 1, 3, 4 ]>> ]
gap> GreensHClasses(D);
[ <Green's H-class: <identity partial perm on [ 1, 3, 4 ]>> ]
gap> D := GreensDClasses(s)[3];
<Green's D-class: <identity partial perm on [ 1, 3 ]>>
gap> PartialPerm([1, 3], [1, 3]) in last;
true
gap> GreensLClasses(D);
[ <Green's L-class: <identity partial perm on [ 1, 3 ]>>, 
  <Green's L-class: [3,1,2]>, <Green's L-class: [1,4][3,2]>, 
  <Green's L-class: [1,3,4]>, <Green's L-class: [3,1,4]>, 
  <Green's L-class: [1,2](3)> ]
gap> GreensRClasses(D);
[ <Green's R-class: <identity partial perm on [ 1, 3 ]>>, 
  <Green's R-class: [2,1,3]>, <Green's R-class: [2,3][4,1]>, 
  <Green's R-class: [4,3,1]>, <Green's R-class: [4,1,3]>, 
  <Green's R-class: [2,1](3)> ]
gap> GreensHClasses(D);
[ <Green's H-class: <identity partial perm on [ 1, 3 ]>>, 
  <Green's H-class: [3,1,2]>, <Green's H-class: [1,4][3,2]>, 
  <Green's H-class: [1,3,4]>, <Green's H-class: [3,1,4]>, 
  <Green's H-class: [1,2](3)>, <Green's H-class: [2,1,3]>, 
  <Green's H-class: <identity partial perm on [ 1, 2 ]>>, 
  <Green's H-class: [1,2,4]>, <Green's H-class: [1,4][2,3]>, 
  <Green's H-class: [2,4](1)>, <Green's H-class: [1,3](2)>, 
  <Green's H-class: [2,3][4,1]>, <Green's H-class: [4,2,1]>, 
  <Green's H-class: <identity partial perm on [ 2, 4 ]>>, 
  <Green's H-class: [2,4,3]>, <Green's H-class: [2,1](4)>, 
  <Green's H-class: [4,2,3]>, <Green's H-class: [4,3,1]>, 
  <Green's H-class: [3,2][4,1]>, <Green's H-class: [3,4,2]>, 
  <Green's H-class: <identity partial perm on [ 3, 4 ]>>, 
  <Green's H-class: [3,4,1]>, <Green's H-class: [4,3,2]>, 
  <Green's H-class: [4,1,3]>, <Green's H-class: [4,2](1)>, 
  <Green's H-class: [1,2](4)>, <Green's H-class: [1,4,3]>, 
  <Green's H-class: <identity partial perm on [ 1, 4 ]>>, 
  <Green's H-class: [1,3][4,2]>, <Green's H-class: [2,1](3)>, 
  <Green's H-class: [3,1](2)>, <Green's H-class: [3,2,4]>, 
  <Green's H-class: [2,3,4]>, <Green's H-class: [2,4][3,1]>, 
  <Green's H-class: <identity partial perm on [ 2, 3 ]>> ]
gap> h := last[9];;
gap> L := LClass(D, Representative(h));
<Green's L-class: [1,2,4]>
gap> Position(HClasses(L), h);
2
gap> DClassOfLClass(L) = D;
true
gap> LClassOfHClass(h) = L;
true
gap> R := RClassOfHClass(h);
<Green's R-class: [1,2,4]>
gap> Position(HClasses(R), h);
3
gap> DClassOfRClass(R) = D;
true

# InverseTest9 
gap> s := InverseSemigroup(
> PartialPermNC([1, 2, 3, 5], [1, 4, 6, 3]),
> PartialPermNC([1, 2, 3, 4, 6], [3, 6, 4, 5, 1]));;
gap> f := PartialPermNC([1, 4, 6], [6, 3, 1]);;
gap> D := DClass(s, f);
<Green's D-class: [4,3](1,6)>
gap> PartialPerm([3, 4, 6], [3, 4, 6]) in last;
true
gap> LClass(s, f) = LClass(D, f);
true
gap> RClass(s, f) = RClass(D, f);
true
gap> R := RClass(s, f);
<Green's R-class: [4,3](1,6)>
gap> HClass(s, f) = HClass(R, f);
true
gap> HClass(D, f) = HClass(R, f);
true
gap> L := LClass(s, f);
<Green's L-class: [4,3](1,6)>
gap> HClass(D, f) = HClass(L, f);
true
gap> HClass(s, f) = HClass(L, f);
true

# InverseTest10
gap> s := POI(10);
<inverse partial perm monoid of rank 10 with 10 generators>
gap> f := PartialPermNC([2, 4, 5, 7], [2, 3, 5, 7]);;
gap> l := LClassNC(s, f);
<Green's L-class: [4,3](2)(5)(7)>
gap> PartialPerm([2, 3, 5, 7], [2, 3, 5, 7]) in last;
true
gap> l := LClass(s, f);
<Green's L-class: [4,3](2)(5)(7)>
gap> s := POI(15);
<inverse partial perm monoid of rank 15 with 15 generators>
gap> f := PartialPermNC([1, 3, 5, 8, 9, 10, 12, 13, 14],
> [2, 3, 4, 7, 9, 11, 12, 13, 15]);;
gap> l := LClass(s, f);
<Green's L-class: [1,2][5,4][8,7][10,11][14,15](3)(9)(12)(13)>
gap> l := LClassNC(s, f);
<Green's L-class: [1,2][5,4][8,7][10,11][14,15](3)(9)(12)(13)>
gap> PartialPerm([2, 3, 4, 7, 9, 11, 12, 13, 15], [2, 3, 4, 7, 9, 11, 12, 13, 15]) in last;
true
gap> s := POI(15);;
gap> l := LClassNC(s, f);
<Green's L-class: [1,2][5,4][8,7][10,11][14,15](3)(9)(12)(13)>
gap> PartialPerm([2, 3, 4, 7, 9, 11, 12, 13, 15], [2, 3, 4, 7, 9, 11, 12, 13, 15]) in last;
true
gap> l = LClass(s, f);
true
gap> f := PartialPermNC([1, 2, 4, 7, 8, 11, 12], [1, 2, 6, 7, 9, 10, 11]);;
gap> l := LClass(POI(12), f);
<Green's L-class: [4,6][8,9][12,11,10](1)(2)(7)>
gap> f := PartialPermNC([1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13],
> [1, 2, 3, 4, 5, 7, 8, 9, 10, 12, 13]);;
gap> l := LClass(POI(13), f);
<Green's L-class: [6,5][11,12](1)(2)(3)(4)(7)(8)(9)(10)(13)>
gap> f := PartialPermNC([1, 2, 3, 4, 7, 8, 9, 10],
> [2, 3, 4, 5, 6, 8, 10, 11]);;
gap> l := LClass(POI(13), f);
<Green's L-class: [1,2,3,4,5][7,6][9,10,11](8)>
gap> LClassNC(POI(13), f);
<Green's L-class: [1,2,3,4,5][7,6][9,10,11](8)>
gap> PartialPerm([2, 3, 4, 5, 6, 8, 10, 11], [2, 3, 4, 5, 6, 8, 10, 11]) in last;
true
gap> RClassNC(POI(13), f);
<Green's R-class: [1,2,3,4,5][7,6][9,10,11](8)>
gap> HClassNC(POI(13), f);
<Green's H-class: [1,2,3,4,5][7,6][9,10,11](8)>
gap> DClassNC(POI(13), f);
<Green's D-class: [1,2,3,4,5][7,6][9,10,11](8)>
gap> PartialPerm([2, 3, 4, 5, 6, 8, 10, 11], [2, 3, 4, 5, 6, 8, 10, 11]) in last;
true
gap> s := POI(13);
<inverse partial perm monoid of rank 13 with 13 generators>
gap> D := DClassNC(s, f);
<Green's D-class: [1,2,3,4,5][7,6][9,10,11](8)>
gap> PartialPerm([2, 3, 4, 5, 6, 8, 10, 11], [2, 3, 4, 5, 6, 8, 10, 11]) in last;
true
gap> l := LClassNC(s, f);
<Green's L-class: [1,2,3,4,5][7,6][9,10,11](8)>
gap> PartialPerm([2, 3, 4, 5, 6, 8, 10, 11], [2, 3, 4, 5, 6, 8, 10, 11]) in last;
true
gap> l := LClass(s, f);
<Green's L-class: [1,2,3,4,5][7,6][9,10,11](8)>
gap> s := POI(15);
<inverse partial perm monoid of rank 15 with 15 generators>
gap> f := PartialPermNC([1, 3, 5, 8, 9, 10, 12, 13, 14],
> [2, 3, 4, 7, 9, 11, 12, 13, 15]);;
gap> l := LClass(s, f);
<Green's L-class: [1,2][5,4][8,7][10,11][14,15](3)(9)(12)(13)>
gap> l := LClassNC(s, f);
<Green's L-class: [1,2][5,4][8,7][10,11][14,15](3)(9)(12)(13)>
gap> PartialPerm([2, 3, 4, 7, 9, 11, 12, 13, 15], [2, 3, 4, 7, 9, 11, 12, 13, 15]) in last;
true
gap> s := POI(15);;
gap> l := LClassNC(s, f);
<Green's L-class: [1,2][5,4][8,7][10,11][14,15](3)(9)(12)(13)>
gap> PartialPerm([2, 3, 4, 7, 9, 11, 12, 13, 15], [2, 3, 4, 7, 9, 11, 12, 13, 15]) in last;
true
gap> l = LClass(s, f);
true
gap> f := PartialPermNC([1, 2, 4, 7, 8, 11, 12], [1, 2, 6, 7, 9, 10, 11]);;
gap> l := LClass(POI(12), f);
<Green's L-class: [4,6][8,9][12,11,10](1)(2)(7)>
gap> f := PartialPermNC([1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13],
> [1, 2, 3, 4, 5, 7, 8, 9, 10, 12, 13]);;
gap> l := LClass(POI(13), f);
<Green's L-class: [6,5][11,12](1)(2)(3)(4)(7)(8)(9)(10)(13)>
gap> f := PartialPermNC([1, 2, 3, 4, 7, 8, 9, 10],
> [2, 3, 4, 5, 6, 8, 10, 11]);;
gap> l := LClass(POI(13), f);
<Green's L-class: [1,2,3,4,5][7,6][9,10,11](8)>
gap> LClassNC(POI(13), f);
<Green's L-class: [1,2,3,4,5][7,6][9,10,11](8)>
gap> PartialPerm([2, 3, 4, 5, 6, 8, 10, 11], [2, 3, 4, 5, 6, 8, 10, 11]) in last;
true
gap> RClassNC(POI(13), f);
<Green's R-class: [1,2,3,4,5][7,6][9,10,11](8)>
gap> HClassNC(POI(13), f);
<Green's H-class: [1,2,3,4,5][7,6][9,10,11](8)>
gap> DClassNC(POI(13), f);
<Green's D-class: [1,2,3,4,5][7,6][9,10,11](8)>
gap> PartialPerm([2, 3, 4, 5, 6, 8, 10, 11], [2, 3, 4, 5, 6, 8, 10, 11]) in last;
true
gap> s := POI(13);
<inverse partial perm monoid of rank 13 with 13 generators>
gap> D := DClassNC(s, f);
<Green's D-class: [1,2,3,4,5][7,6][9,10,11](8)>
gap> PartialPerm([2, 3, 4, 5, 6, 8, 10, 11], [2, 3, 4, 5, 6, 8, 10, 11]) in last;
true
gap> LClassNC(s, f) = LClass(D, f);
true
gap> LClass(s, f) = LClassNC(D, f);
true
gap> LClassNC(s, f) = LClassNC(D, f);
true
gap> LClassNC(s, f) = LClassNC(D, f);
true
gap> RClass(s, f) = RClassNC(D, f);
true
gap> RClassNC(s, f) = RClassNC(D, f);
true
gap> RClassNC(s, f) = RClass(D, f);
true
gap> R := RClassNC(s, f);
<Green's R-class: [1,2,3,4,5][7,6][9,10,11](8)>
gap> HClass(s, f) = HClass(R, f);
true
gap> HClassNC(s, f) = HClass(R, f);
true
gap> HClassNC(s, f) = HClassNC(R, f);
true
gap> HClass(s, f) = HClassNC(R, f);
true
gap> L := LClassNC(s, f);
<Green's L-class: [1,2,3,4,5][7,6][9,10,11](8)>
gap> PartialPerm([2, 3, 4, 5, 6, 8, 10, 11], [2, 3, 4, 5, 6, 8, 10, 11]) in last;
true
gap> HClass(s, f) = HClassNC(L, f);
true
gap> HClass(s, f) = HClass(L, f);
true
gap> HClassNC(L, f) = HClass(D, f);
true
gap> HClassNC(L, f) = HClassNC(s, f);
true
gap> HClass(D, f) = HClassNC(s, f);
true

# InverseTest11
gap> m := InverseSemigroup(
> PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
>  18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
>  37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55,
>  56, 57, 58, 59, 60, 61, 62, 63, 64], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
>  12, 13, 14, 15, 16, 33, 34, 35, 36, 37, 39, 40, 41, 43, 44, 46, 49, 50, 52,
>  55, 59, 17, 18, 19, 20, 21, 38, 22, 23, 24, 42, 25, 26, 45, 27, 47, 48, 28,
>  29, 51, 30, 53, 54, 31, 56, 57, 58, 32, 60, 61, 62, 63, 64]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
>  18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
>  37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55,
>  56, 57, 58, 59, 60, 61, 62, 63, 64], [1, 2, 3, 4, 9, 10, 11, 13, 5, 6, 7,
>  12, 8, 14, 15, 16, 17, 18, 19, 21, 20, 22, 24, 23, 26, 25, 27, 29, 28, 30,
>  31, 32, 33, 34, 35, 37, 36, 38, 39, 41, 40, 42, 44, 43, 45, 46, 48, 47, 50,
>  49, 51, 52, 54, 53, 55, 57, 56, 58, 59, 61, 60, 62, 63, 64]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
>  18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
>  37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55,
>  56, 57, 58, 59, 60, 61, 62, 63, 64], [1, 2, 3, 4, 5, 6, 7, 8, 17, 18, 19,
>  20, 22, 23, 25, 28, 9, 10, 11, 12, 21, 13, 14, 24, 15, 26, 27, 16, 29, 30,
>  31, 32, 33, 34, 35, 36, 38, 37, 39, 40, 42, 41, 43, 45, 44, 47, 46, 48, 49,
>  51, 50, 53, 52, 54, 56, 55, 57, 58, 60, 59, 61, 62, 63, 64]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
>  18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
>  37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55,
>  56, 57, 58, 59, 60, 61, 62, 63, 64], [1, 3, 2, 4, 5, 7, 6, 8, 9, 11, 10,
>  12, 13, 15, 14, 16, 17, 19, 18, 20, 21, 22, 25, 26, 23, 24, 27, 28, 29, 31,
>  30, 32, 33, 35, 34, 36, 37, 38, 39, 43, 44, 45, 40, 41, 42, 46, 47, 48, 49,
>  50, 51, 55, 56, 57, 52, 53, 54, 58, 59, 60, 61, 63, 62, 64]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
>  18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
>  37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55,
>  56, 57, 58, 59, 60, 61, 62, 63, 64], [1, 2, 5, 6, 3, 4, 7, 8, 9, 10, 12,
>  11, 14, 13, 15, 16, 17, 18, 20, 19, 21, 23, 22, 24, 25, 27, 26, 28, 30, 29,
>  31, 32, 33, 34, 36, 35, 37, 38, 40, 39, 41, 42, 43, 46, 47, 44, 45, 48, 49,
>  52, 53, 50, 51, 54, 55, 56, 58, 57, 59, 60, 62, 61, 63, 64]),
> PartialPermNC([2, 4, 6, 8, 10, 13, 14, 16, 18, 22, 23, 24, 28, 29, 30, 32,
>  34, 39, 40, 41, 42, 49, 50, 51, 52, 53, 54, 59, 60, 61, 62, 64],
> [3, 4, 7, 8, 11, 13, 15, 16, 19, 22, 25, 26, 28, 29, 31, 32, 35, 39, 43, 44,
>  45, 49, 50, 51, 55, 56, 57, 59, 60, 61, 63, 64]));;
gap> DClassReps(m);
[ <identity partial perm on 
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 2\
1, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,\
 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 6\
0, 61, 62, 63, 64 ]>, 
  <identity partial perm on 
    [ 3, 4, 7, 8, 11, 13, 15, 16, 19, 22, 25, 26, 28, 29, 31, 32, 35, 39, 43, \
44, 45, 49, 50, 51, 55, 56, 57, 59, 60, 61, 63, 64 ]>, 
  <identity partial perm on 
    [ 4, 8, 13, 16, 22, 28, 29, 32, 39, 49, 50, 51, 59, 60, 61, 64 ]>, 
  <identity partial perm on [ 39, 49, 50, 51, 59, 60, 61, 64 ]>, 
  <identity partial perm on [ 31, 32, 63, 64 ]>, 
  <identity partial perm on [ 61, 64 ]>, <identity partial perm on [ 64 ]> ]
gap> NrLClasses(m);
64
gap> IsRTrivial(m);
false
gap> Size(m);
13327
gap> f := PartialPermNC([27, 30, 31, 32, 58, 62, 63, 64],
> [8, 16, 28, 60, 49, 59, 32, 64]);;
gap> d := DClassNC(m, f);
<Green's D-class: [27,8][30,16][31,28][58,49][62,59][63,32,60](64)>
gap> PartialPerm([8, 16, 28, 32, 49, 59, 60, 64], [8, 16, 28, 32, 49, 59, 60, 64]) in last;
true
gap> LClassReps(d);
[ <identity partial perm on [ 8, 16, 28, 32, 49, 59, 60, 64 ]>, 
  [8,13][28,29][49,50][60,61](16)(32)(59)(64), 
  [8,22][16,28,29][49,51][59,60,61](32)(64), 
  [8,39][16,49,51][28,50][32,59,60,61](64), 
  [8,40][16,49,53][28,52][32,59,60,62](64), [8,23][16,28,30][49,53][59,60,62]
    (32)(64), [8,14][28,30][49,52][60,62](16)(32)(59)(64), 
  [8,15][28,31][49,55][60,63](16)(32)(59)(64), 
  [8,25][16,28,31][49,56][59,60,63](32)(64), 
  [8,43][16,49,56][28,55][32,59,60,63](64), 
  [8,44][16,50][28,55][32,59,61][49,57][60,63](64), 
  [8,41][16,50][28,52][32,59,61][49,54][60,62](64), 
  [8,24][16,29][28,30][49,54][59,61][60,62](32)(64), 
  [8,26][16,29][28,31][49,57][59,61][60,63](32)(64), 
  [8,27][16,30][28,31][49,58][59,62][60,63](32)(64), 
  [8,46][16,52][28,55][32,59,62][49,58][60,63](64), 
  [8,47][16,53][28,56][32,60,63][49,58][59,62](64), 
  [8,45][16,51][28,56][32,60,63][49,57][59,61](64), 
  [8,42][16,51][28,53][32,60,62][49,54][59,61](64), 
  [8,48][16,54][28,57][32,61][49,58][59,62][60,63](64) ]
gap> List(DClasses(m), NrRClasses);
[ 1, 6, 15, 20, 15, 6, 1 ]
gap> d := DClasses(m)[6];
<Green's D-class: <identity partial perm on [ 61, 64 ]>>
gap> PartialPerm([61, 64], [61, 64]) in last;
true
gap> LClassReps(d);
[ <identity partial perm on [ 61, 64 ]>, [61,60](64), [61,59](64), 
  [61,32](64), [61,62](64), [61,63](64) ]
gap> RClassReps(d);
[ <identity partial perm on [ 61, 64 ]>, [60,61](64), [59,61](64), 
  [32,61](64), [62,61](64), [63,61](64) ]
gap> d := DClassNC(m, Representative(d));
<Green's D-class: <identity partial perm on [ 61, 64 ]>>
gap> PartialPerm([61, 64], [61, 64]) in last;
true
gap> LClassReps(d);
[ <identity partial perm on [ 61, 64 ]>, [61,60](64), [61,59](64), 
  [61,32](64), [61,62](64), [61,63](64) ]
gap> RClassReps(m);
[ <identity partial perm on 
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 2\
1, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,\
 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 6\
0, 61, 62, 63, 64 ]>, 
  <identity partial perm on 
    [ 3, 4, 7, 8, 11, 13, 15, 16, 19, 22, 25, 26, 28, 29, 31, 32, 35, 39, 43, \
44, 45, 49, 50, 51, 55, 56, 57, 59, 60, 61, 63, 64 ]>, 
  [2,3][6,7][10,11][14,15][18,19][23,25][24,26][30,31][34,35][40,43][41,44]
    [42,45][52,55][53,56][54,57][62,63](4)(8)(13)(16)(22)(28)(29)(32)(39)(49)
    (50)(51)(59)(60)(61)(64), [5,3][6,4][12,11][14,13][20,19][23,22][27,26]
    [30,29][36,35][40,39][46,44][47,45][52,50][53,51][58,57][62,61](7)(8)(15)
    (16)(25)(28)(31)(32)(43)(49)(55)(56)(59)(60)(63)(64), 
  [9,3][10,4][12,11,7][14,13,8][21,19][24,22][27,26,25][30,29,28][37,35]
    [41,39][46,44,43][48,45][52,50,49][54,51][58,57,56][62,61,60](15)(16)(31)
    (32)(55)(59)(63)(64), [17,3][18,4][20,11][21,19,7][23,13][24,22,8]
    [27,26,25,15][30,29,28,16][38,35][42,39][47,44][48,45,43][53,50][54,51,49]
    [58,57,56,55][62,61,60,59](31)(32)(63)(64), 
  [33,3][34,4][36,11][37,19][38,35,7][40,13][41,22][42,39,8][46,26][47,44,25]
    [48,45,43,15][52,29][53,50,28][54,51,49,16][58,57,56,55,31]
    [62,61,60,59,32](63)(64), 
  <identity partial perm on 
    [ 4, 8, 13, 16, 22, 28, 29, 32, 39, 49, 50, 51, 59, 60, 61, 64 ]>, 
  [6,4][14,13][23,22][30,29][40,39][52,50][53,51][62,61](8)(16)(28)(32)(49)
    (59)(60)(64), [7,4][15,13][25,22][31,29][43,39][55,50][56,51][63,61](8)
    (16)(28)(32)(49)(59)(60)(64), [11,4][15,13,8][26,22][31,29,28][44,39]
    [55,50,49][57,51][63,61,60](16)(32)(59)(64), 
  [10,4][14,13,8][24,22][30,29,28][41,39][52,50,49][54,51][62,61,60](16)(32)
    (59)(64), [18,4][23,13][24,22,8][30,29,28,16][42,39][53,50][54,51,49]
    [62,61,60,59](32)(64), [19,4][25,13][26,22,8][31,29,28,16][45,39][56,50]
    [57,51,49][63,61,60,59](32)(64), [35,4][43,13][44,22][45,39,8][55,29]
    [56,50,28][57,51,49,16][63,61,60,59,32](64), 
  [34,4][40,13][41,22][42,39,8][52,29][53,50,28][54,51,49,16][62,61,60,59,32]
    (64), [36,4][40,8][43,13][46,22][47,39][52,28][53,49,16][55,29][56,50]
    [58,51][62,60,59,32][63,61](64), [20,4][23,8][25,13][27,22][30,28,16]
    [31,29][47,39][53,49][56,50][58,51][62,60,59][63,61](32)(64), 
  [12,4][14,8][15,13][27,22][30,28][31,29][46,39][52,49][55,50][58,51][62,60]
    [63,61](16)(32)(59)(64), [21,4][24,8][26,13][27,22][30,28][31,29,16]
    [48,39][54,49][57,50][58,51][62,60][63,61,59](32)(64), 
  [37,4][41,8][44,13][46,22][48,39][52,28][54,49][55,29][57,50,16][58,51]
    [62,60][63,61,59,32](64), [38,4][42,8][45,13][47,22][48,39][53,28][54,49]
    [56,29][57,50][58,51,16][62,60,32][63,61,59](64), 
  <identity partial perm on [ 39, 49, 50, 51, 59, 60, 61, 64 ]>, 
  [22,39][28,49][29,50][32,59](51)(60)(61)(64), 
  [13,39][16,49][29,50,51][32,59,60](61)(64), 
  [8,39][16,49,51][28,50][32,59,60,61](64), 
  [14,39][16,49][30,50][32,59,60][52,51][62,61](64), 
  [15,39][16,49][31,50][32,59,60][55,51][63,61](64), 
  [25,39][28,49][31,50][32,59][56,51][63,61](60)(64), 
  [23,39][28,49][30,50][32,59][53,51][62,61](60)(64), 
  [40,39][52,50][53,51][62,61](49)(59)(60)(64), 
  [43,39][55,50][56,51][63,61](49)(59)(60)(64), 
  [44,39][55,50,49][57,51][63,61,60](59)(64), 
  [26,39][29,49][31,50][32,59][57,51][63,61,60](64), 
  [24,39][29,49][30,50][32,59][54,51][62,61,60](64), 
  [41,39][52,50,49][54,51][62,61,60](59)(64), 
  [42,39][53,50][54,51,49][62,61,60,59](64), 
  [45,39][56,50][57,51,49][63,61,60,59](64), 
  [47,39][53,49][56,50][58,51][62,60,59][63,61](64), 
  [46,39][52,49][55,50][58,51][62,60][63,61](59)(64), 
  [27,39][30,49][31,50][32,59][58,51][62,60][63,61](64), 
  [48,39][54,49][57,50][58,51][62,60][63,61,59](64), 
  <identity partial perm on [ 31, 32, 63, 64 ]>, [30,31][62,63](32)(64), 
  [29,31][61,63](32)(64), [28,31][60,63](32)(64), [16,31][59,63](32)(64), 
  [49,31][59,32][60,63](64), [50,31][59,32][61,63](64), 
  [51,31][60,32][61,63](64), [53,31][60,32][62,63](64), 
  [52,31][59,32][62,63](64), [55,31][59,32](63)(64), [56,31][60,32](63)(64), 
  [57,31][61,32](63)(64), [54,31][61,32][62,63](64), [58,31][62,32](63)(64), 
  <identity partial perm on [ 61, 64 ]>, [60,61](64), [59,61](64), 
  [32,61](64), [62,61](64), [63,61](64), <identity partial perm on [ 64 ]> ]
gap> RClassReps(d);
[ <identity partial perm on [ 61, 64 ]>, [60,61](64), [59,61](64), 
  [32,61](64), [62,61](64), [63,61](64) ]
gap> Size(d);
36
gap> Size(DClasses(m)[6]);
36

# InverseTest12
gap> s := InverseSemigroup([PartialPermNC([1, 2, 3, 5], [2, 1, 6, 3]),
> PartialPermNC([1, 2, 3, 6], [3, 5, 2, 6])]);;
gap> f := PartialPermNC([1 .. 3], [6, 3, 1]);;
gap> d := DClassNC(s, f);
<Green's D-class: [2,3,1,6]>
gap> PartialPerm([1, 3, 6], [1, 3, 6]) in last;
true
gap> GroupHClass(d);
<Green's H-class: <identity partial perm on [ 1, 3, 6 ]>>
gap> PartialPerm([1, 3, 6], [1, 3, 6]) in last;
true
gap> StructureDescription(GroupHClass(d));
"1"
gap> ForAny(DClasses(s), x -> not IsTrivial(GroupHClass(x)));
true
gap> D := First(DClasses(s), x -> not IsTrivial(GroupHClass(x)));
<Green's D-class: <identity partial perm on [ 1, 2 ]>>
gap> PartialPerm([1, 2], [1, 2]) in D;
true
gap> StructureDescription(GroupHClass(D));
"C2"

# InverseTest13
gap> s := InverseSemigroup(
> [PartialPermNC([1, 2, 3, 4, 5, 7], [10, 6, 3, 4, 9, 1]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8], [6, 10, 7, 4, 8, 2, 9, 1])]);;
gap> Idempotents(s, 1);
[ <identity partial perm on [ 4 ]> ]
gap> Idempotents(s, 0);
[  ]
gap> PartialPermNC([]) in s;
false
gap> Idempotents(s, 2);
[ <identity partial perm on [ 3, 4 ]>, <identity partial perm on [ 4, 7 ]>, 
  <identity partial perm on [ 2, 4 ]>, <identity partial perm on [ 4, 10 ]>, 
  <identity partial perm on [ 1, 4 ]>, <identity partial perm on [ 4, 9 ]>, 
  <identity partial perm on [ 4, 8 ]>, <identity partial perm on [ 4, 6 ]>, 
  <identity partial perm on [ 4, 5 ]> ]
gap> Idempotents(s, 10);
[  ]
gap> f := PartialPermNC([2, 4, 9, 10], [7, 4, 3, 2]);;
gap> r := RClassNC(s, f);
<Green's R-class: [9,3][10,2,7](4)>
gap> Idempotents(r);
[ <identity partial perm on [ 2, 4, 9, 10 ]> ]

# InverseTest14
gap> s := InverseSemigroup([
> PartialPerm([1, 2, 3, 4, 5, 6, 9], [1, 5, 9, 2, 6, 10, 7]),
> PartialPerm([1, 3, 4, 7, 8, 9], [9, 4, 1, 6, 2, 8])]);
<inverse partial perm semigroup of rank 10 with 2 generators>
gap> ForAll(RClasses(s), IsRegularGreensClass);
true

# InverseTest15
gap> s := InverseSemigroup(
> PartialPermNC([1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 15],
> [6, 4, 18, 3, 11, 8, 5, 14, 19, 13, 12, 20, 1]),
> PartialPermNC([1, 2, 4, 5, 6, 7, 9, 10, 11, 12, 15, 16, 18, 20],
> [1, 18, 3, 7, 4, 9, 19, 5, 14, 16, 12, 17, 15, 6]));;
gap> iter := IteratorOfDClassReps(s);
<iterator>
gap> NextIterator(iter);
[2,4,3,18][7,8,5][9,14,20][10,19][15,1,6,11,13,12]
gap> NextIterator(iter);
[2,18,15,12,16,17][10,5,7,9,19][11,14][20,6,4,3](1)
gap> NextIterator(iter);
[1,11,12][2,3][4,18][7,5][9,20][15,6,13]
gap> NextIterator(iter);
[2,3,15,1,4][8,7][13,16](6,14)
gap> NextIterator(iter);
[3,2,6][8,10,9,11][13,15,1,20](4)
gap> NextIterator(iter);
[6,1,12][8,9][11,4,18][13,14,19,5](3)
gap> NextIterator(iter);
[3,6,1,18,4][8,5][14,7](11,20)
gap> NextIterator(iter);
[3,6][4,20][9,5][16,15,2][17,12,18][19,7,10](1)
gap> NextIterator(iter);
[1,14][9,6][11,16](4,15)(7)
gap> NextIterator(iter);
[7,10][11,15,20](2,4)
gap> NextIterator(iter);
[8,9][13,17][14,4][15,1,3,12]
gap> NextIterator(iter);
[4,2,1,14][9,6](15)
gap> NextIterator(iter);
[2,20][4,6][10,7][13,18][15,1]
gap> NextIterator(iter);
[11,6][18,1,4,15][20,14]
gap> NextIterator(iter);
[6,4,2][7,11][18,1,20]
gap> NextIterator(iter);
[2,12][5,19][10,9][15,17][18,16][20,3](1)
gap> NextIterator(iter);
[8,14][11,3,18][13,20](6)
gap> NextIterator(iter);
[4,15][6,1,16][8,19,7][11,3]
gap> NextIterator(iter);
[1,13,9][6,15][11,2][14,10][19,8](3,4)
gap> NextIterator(iter);
[11,15][12,6][13,1][18,2]
gap> NextIterator(iter);
[11,1][13,20,7](5)(6,18)
gap> NextIterator(iter);
[11,14][18,2][20,6,15](1,3)(8)
gap> NextIterator(iter);
[3,20][8,10][14,5][18,6,1,2]
gap> NextIterator(iter);
[3,4][5,9][12,1,20][18,6][19,11]
gap> NextIterator(iter);
[4,1,20][7,8][9,11][15,3]
gap> NextIterator(iter);
[7,9,4,12][11,17][15,3]
gap> NextIterator(iter);
[2,6,7][14,1][15,18]
gap> NextIterator(iter);
[3,2,6][8,10][13,15,1,20]
gap> NextIterator(iter);
[2,13,6][4,18][9,12][10,20][15,11](3)
gap> NextIterator(iter);
[2,1,11][9,20][15,18]
gap> NextIterator(iter);
[2,14][4,1][13,3](15)
gap> NextIterator(iter);
[5,10][6,2][18,20]
gap> NextIterator(iter);
[2,16][10,19][18,17](1)
gap> NextIterator(iter);
[4,1,3][20,15](2)
gap> NextIterator(iter);
[6,11,18][8,20]
gap> NextIterator(iter);
[3,15][13,6,4]
gap> NextIterator(iter);
[4,1][11,18][19,8](6)
gap> NextIterator(iter);
[4,12][6,1,17][19,9]
gap> NextIterator(iter);
[3,6,18][13,7](4)
gap> NextIterator(iter);
[11,18][12,20][13,1]
gap> NextIterator(iter);
[5,8][6,3][11,15][13,14][18,1]
gap> NextIterator(iter);
[5,14][12,6][18,11][19,13](3)
gap> NextIterator(iter);
[3,6][5,7][12,1][18,20]
gap> NextIterator(iter);
[1,11][15,3,13][16,6][19,5]
gap> NextIterator(iter);
<empty partial perm>
gap> NextIterator(iter);
[4,2][7,10][11,15,20]
gap> s := RandomInverseSemigroup(IsPartialPermSemigroup, 2, 20);;
gap> iter := IteratorOfDClassReps(s);
<iterator>
gap> s := RandomInverseSemigroup(IsPartialPermSemigroup, 2, 100);;
gap> iter := IteratorOfLClassReps(s);
Error, Variable: 'IteratorOfLClassReps' must have a value
gap> for i in [1 .. 10000] do NextIterator(iter); od;
Error, <iter> is exhausted
gap> s := RandomInverseSemigroup(IsPartialPermSemigroup, 2, 10);;
gap> iter := IteratorOfLClassReps(s);
Error, Variable: 'IteratorOfLClassReps' must have a value
gap> for i in iter do od;
gap> iter := IteratorOfDClassReps(s);
<iterator>
gap> for i in iter do od;

# InverseTest17
gap> s := InverseSemigroup(
> [PartialPermNC([1, 2, 3, 5, 7, 9, 10], [6, 7, 2, 9, 1, 5, 3]),
> PartialPermNC([1, 2, 3, 5, 6, 7, 9, 10], [8, 1, 9, 4, 10, 5, 6, 7])]);;
gap> NrIdempotents(s);
236
gap> f := PartialPermNC([2, 3, 7, 9, 10], [7, 2, 1, 5, 3]);;
gap> d := DClassNC(s, f);;
gap> NrIdempotents(d);
13
gap> l := LClass(d, f);
<Green's L-class: [9,5][10,3,2,7,1]>
gap> PartialPerm([1, 2, 3, 5, 7], [1, 2, 3, 5, 7]) in last;
true
gap> NrIdempotents(l);
1
gap> DClass(l);
<Green's D-class: [9,5][10,3,2,7,1]>
gap> PartialPerm([1, 2, 3, 5, 7], [1, 2, 3, 5, 7]) in last;
true
gap> DClass(l) = d;
true
gap> NrIdempotents(DClass(l));
13

# InverseTest18
gap> S := InverseSemigroup(
> PartialPermNC([1, 2, 3], [1, 3, 5]),
> PartialPermNC([1, 2, 4], [1, 2, 3]),
> PartialPermNC([1, 2, 5], [4, 5, 2]));;
gap> f := PartialPermNC([1, 5], [3, 2]);;
gap> SchutzenbergerGroup(LClass(S, f));
Group(())
gap> SchutzenbergerGroup(RClass(S, f));
Group(())
gap> SchutzenbergerGroup(HClass(S, f));
Group(())
gap> SchutzenbergerGroup(DClass(S, f));
Group(())
gap> List(DClasses(S), SchutzenbergerGroup);
[ Group(()), Group(()), Group(()), Group(()), Group([ (2,5) ]), Group(()) ]

# InverseTest19
gap> s := InverseSemigroup(
> [PartialPerm([1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9]),
>  PartialPerm([1, 2, 3, 4, 5, 7, 8, 9], [1, 2, 3, 4, 5, 7, 8, 9]),
>  PartialPerm([1, 3, 4, 7, 8, 9], [1, 3, 4, 7, 8, 9]),
>  PartialPerm([3, 7, 8, 9], [2, 7, 8, 9]),
>  PartialPerm([1, 7, 9], [1, 7, 9]),
>  PartialPerm([1, 7, 9], [8, 7, 9])]);;
gap> Size(s);
12
gap> IsDTrivial(s);
false

# InverseTest20 
gap> IsIsometryPP := function(f)
> local n, i, j, k, l;
>  n := RankOfPartialPerm(f);
>  for i in [1 .. n - 1] do
>    k := DomainOfPartialPerm(f)[i];
>    for j in [i + 1 .. n] do
>      l := DomainOfPartialPerm(f)[j];
>      if not AbsInt(k ^ f - l ^ f) = AbsInt(k - l) then
>        return false;
>      fi;
>    od;
>  od;
>  return true;
> end;;
gap> s := InverseSubsemigroupByProperty(SymmetricInverseSemigroup(5),
> IsIsometryPP);;
gap> Size(s);
142
gap> s := InverseSubsemigroupByProperty(SymmetricInverseSemigroup(6),
> IsIsometryPP);;
gap> Size(s);
319
gap> s := InverseSubsemigroupByProperty(SymmetricInverseSemigroup(7),
> IsIsometryPP);;
gap> Size(s);
686

# InverseCongTest1: Create an inverse semigroup
gap> s := InverseSemigroup([PartialPerm([1, 2, 3, 5], [2, 7, 3, 4]),
>  PartialPerm([1, 3, 4, 5], [7, 2, 4, 6]),
>  PartialPerm([1, 2, 3, 4, 6], [2, 3, 4, 6, 1]),
>  PartialPerm([1, 2, 4, 6], [2, 4, 3, 7]),
>  PartialPerm([1, 2, 4, 6], [3, 1, 7, 2]),
>  PartialPerm([1, 2, 5, 6], [5, 1, 6, 3]),
>  PartialPerm([1, 2, 3, 6], [7, 3, 4, 2])]);;
gap> cong := SemigroupCongruence(s,
>  [PartialPerm([4], [7]), PartialPerm([2], [1])]);
<2-sided semigroup congruence over <inverse partial perm semigroup 
 of size 4165, rank 7 with 7 generators> with 1 generating pairs>

# InverseCongTest3: Try some methods
gap> x := PartialPerm([4], [5]);;
gap> y := PartialPerm([1, 2, 5], [5, 1, 6]);;
gap> z := PartialPerm([6], [1]);;
gap> [x, y] in cong;
false
gap> [x, z] in cong;
true
gap> [y, z] in cong;
false

# InverseCongTest4: Congruence classes
gap> classx := EquivalenceClassOfElement(cong, x);
<2-sided congruence class of [4,5]>
gap> classy := EquivalenceClassOfElement(cong, y);;
gap> classz := EquivalenceClassOfElement(cong, z);;
gap> classx = classy;
false
gap> classz = classx;
true
gap> x in classx;
true
gap> y in classx;
false
gap> x in classz;
true
gap> z * y in classz * classy;
true
gap> y * x in classy * classx;
true
gap> Size(classx);
50

# InverseCongTest5: Quotients
gap> q := s / cong;;

# InverseCongTest6:
# Convert to and from semigroup congruence by generating pairs
gap> pairs := GeneratingPairsOfSemigroupCongruence(cong);;
gap> ccong := SemigroupCongruence(s, pairs);;
gap> ccong = cong;
true
gap> ccong := AsSemigroupCongruenceByGeneratingPairs(cong);
<2-sided semigroup congruence over <inverse partial perm semigroup 
 of size 4165, rank 7 with 7 generators> with 1 generating pairs>
gap> [x, y] in ccong;
false
gap> [x, z] in ccong;
true
gap> [y, z] in ccong;
false

# InverseCongTest7: Universal congruence
gap> s := InverseSemigroup(PartialPerm([1], [2]), PartialPerm([2], [1]));
<inverse partial perm semigroup of rank 2 with 2 generators>
gap> Size(s);
5
gap> SemigroupCongruence(s, [s.1, s.1 * s.2]);
<universal semigroup congruence over <0-simple inverse partial perm semigroup 
 of size 5, rank 2 with 2 generators>>

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/inverse.tst");
