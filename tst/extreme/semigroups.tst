#############################################################################
##
#W  extreme/semigroups.tst
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##


#@local S, acting, f, g, gens, i, inv, iso, s, small, u, x, y, z
gap> START_TEST("Semigroups package: extreme/semigroups.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();
gap> SEMIGROUPS.DefaultOptionsRec.acting := true;;

# previously the second arg here (an ideal) would have been added using
#   AsList to the set of generators of S. This is slow with the acting stuff
#   turned off. 
gap> S := Semigroup(AsSemigroup(IsPartialPermSemigroup, AlternatingGroup(8)),
>                   SemigroupIdeal(SymmetricInverseMonoid(8),
>                                  PartialPerm([1 .. 7])));
<partial perm semigroup of rank 8 with 19 generators>
gap> Size(S);
1421569

# SemigroupsTest1: Inverse semigroup of partial perms
gap> gens := [PartialPermNC([1, 2, 3, 4, 6, 7, 10], [5, 3, 4, 1, 9, 6, 8]),
> PartialPermNC([1, 2, 3, 5, 6, 7, 9], [8, 9, 6, 7, 3, 4, 5]),
> PartialPermNC([1, 2, 3, 5, 6, 8, 9], [2, 4, 1, 7, 3, 10, 8]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 8], [8, 4, 5, 3, 7, 2, 10]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 8], [9, 2, 8, 10, 6, 3, 7]),
> PartialPermNC([1, 2, 4, 6, 7, 8], [3, 1, 5, 7, 6, 10]),
> PartialPermNC([1, 2, 4, 5, 6, 7, 10], [1, 8, 2, 5, 3, 7, 9]),
> PartialPermNC([1, 2, 3, 4, 5, 8, 9], [8, 2, 9, 5, 7, 6, 10]),
> PartialPermNC([1, 2, 4, 5, 6, 7], [4, 8, 10, 3, 6, 9]),
> PartialPermNC([1, 2, 4, 6, 8, 9], [2, 6, 4, 8, 10, 3]),
> PartialPermNC([1, 2, 3, 4, 6, 7, 9], [4, 10, 9, 5, 6, 2, 3]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 7], [7, 4, 8, 6, 1, 5, 10]),
> PartialPermNC([1, 3, 4, 7, 8], [1, 6, 3, 5, 9]),
> PartialPermNC([1, 2, 3, 5, 6, 7, 9], [6, 5, 1, 4, 10, 7, 8]),
> PartialPermNC([1, 2, 3, 5, 7, 8, 9], [4, 6, 2, 5, 10, 3, 8]),
> PartialPermNC([1, 2, 3, 4, 6, 9], [3, 7, 8, 2, 5, 10]),
> PartialPermNC([1, 2, 3, 4, 7, 8, 10], [3, 4, 2, 1, 5, 10, 8]),
> PartialPermNC([1, 2, 3, 4, 5, 8, 9, 10], [6, 5, 10, 7, 9, 1, 4, 8]),
> PartialPermNC([1, 2, 3, 5, 8, 9], [6, 10, 7, 8, 5, 3]),
> PartialPermNC([1, 2, 3, 4, 5, 10], [3, 7, 10, 8, 2, 6]),
> PartialPermNC([1, 2, 5, 6, 10], [7, 4, 8, 3, 2]),
> PartialPermNC([1, 2, 4, 5, 6, 8, 10], [6, 9, 5, 7, 4, 2, 1]),
> PartialPermNC([1, 2, 3, 4, 6, 9], [8, 2, 4, 7, 9, 1]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 9, 10], [9, 3, 4, 2, 6, 10, 8, 5]),
> PartialPermNC([1, 2, 3, 10], [4, 9, 7, 10]),
> PartialPermNC([1, 2, 3, 4, 5, 7, 8, 10], [7, 10, 1, 2, 9, 8, 3, 5]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 7], [9, 6, 4, 3, 10, 1, 7]),
> PartialPermNC([1, 2, 3, 5, 7, 8, 10], [5, 9, 4, 8, 1, 7, 2]),
> PartialPermNC([1, 2, 3, 6, 7, 9, 10], [4, 2, 5, 7, 3, 6, 9]),
> PartialPermNC([1, 2, 3, 4, 5, 8, 9], [9, 8, 7, 10, 4, 5, 2]),
> PartialPermNC([1, 2, 3, 4, 5, 8, 9], [4, 10, 1, 2, 8, 3, 9]),
> PartialPermNC([1, 2, 4, 5, 9, 10], [10, 8, 3, 7, 1, 2]),
> PartialPermNC([1, 2, 3, 5, 9, 10], [6, 10, 2, 7, 3, 4]),
> PartialPermNC([1, 2, 3, 4, 10], [8, 9, 10, 7, 6]),
> PartialPermNC([1, 2, 3, 6], [9, 10, 5, 8]),
> PartialPermNC([1, 2, 3, 5, 6, 8], [9, 8, 5, 7, 6, 10]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 7], [1, 8, 5, 2, 10, 4, 6]),
> PartialPermNC([1, 2, 3, 5, 8, 10], [4, 5, 8, 7, 9, 2]),
> PartialPermNC([1, 2, 4, 5, 6, 8, 9], [8, 1, 3, 10, 2, 6, 5]),
> PartialPermNC([1, 2, 3, 4, 5, 7, 8, 10], [10, 8, 3, 7, 6, 2, 1, 9]),
> PartialPermNC([1, 3, 4, 6, 10], [7, 2, 8, 6, 5]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 9], [8, 3, 9, 10, 2, 1, 5]),
> PartialPermNC([1, 3, 5, 6, 10], [9, 3, 4, 8, 6]),
> PartialPermNC([1, 2, 3, 4, 5, 7, 9], [7, 3, 2, 8, 4, 1, 10]),
> PartialPermNC([1, 2, 3, 4, 6, 7], [6, 3, 5, 7, 10, 8]),
> PartialPermNC([1, 2, 3, 4, 5, 6], [8, 7, 9, 4, 3, 6]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 8], [2, 1, 9, 7, 4, 5, 8]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 10], [10, 2, 6, 1, 3, 5, 9]),
> PartialPermNC([1, 2, 3, 4, 6, 10], [2, 3, 1, 4, 6, 10]),
> PartialPermNC([1, 2, 3, 4, 8, 9, 10], [8, 2, 7, 6, 5, 3, 4]),
> PartialPermNC([1, 2, 3, 7, 9], [9, 5, 8, 1, 7]),
> PartialPermNC([1, 2, 3, 4, 5, 10], [10, 6, 9, 5, 2, 1]),
> PartialPermNC([1, 2, 3, 4, 6, 7, 8], [9, 3, 10, 1, 7, 4, 6]),
> PartialPermNC([1, 3, 4, 5, 7, 10], [1, 6, 7, 5, 10, 4]),
> PartialPermNC([1, 3, 4, 6, 7, 8], [8, 2, 3, 1, 10, 7]),
> PartialPermNC([1, 2, 3, 5, 8, 10], [2, 6, 5, 8, 3, 4]),
> PartialPermNC([1, 2, 3, 5, 8, 9], [1, 10, 9, 3, 6, 4]),
> PartialPermNC([1, 2, 3, 4, 6, 9, 10], [10, 2, 6, 1, 8, 5, 7]),
> PartialPermNC([1, 2, 3, 5, 7], [1, 2, 10, 8, 9]),
> PartialPermNC([1, 2, 4, 5, 6, 9], [9, 5, 4, 10, 7, 2]),
> PartialPermNC([1, 2, 3, 4, 5, 7, 10], [3, 10, 2, 4, 8, 7, 6]),
> PartialPermNC([1, 3, 4, 5, 6, 7], [6, 7, 3, 10, 4, 5]),
> PartialPermNC([1, 2, 3, 4, 6, 8], [3, 4, 9, 7, 10, 1]),
> PartialPermNC([1, 2, 3, 5, 7], [7, 5, 10, 3, 1]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 7, 9, 10],
> [5, 7, 1, 3, 2, 8, 4, 6, 10]),
> PartialPermNC([1, 2, 3, 4, 6, 7, 9], [6, 8, 2, 7, 1, 5, 4]),
> PartialPermNC([1, 2, 3, 4, 6, 8, 9], [8, 4, 6, 5, 9, 10, 1]),
> PartialPermNC([1, 2, 3, 4, 6, 7, 9], [5, 9, 3, 1, 6, 8, 10]),
> PartialPermNC([1, 2, 3, 4, 5, 7], [9, 3, 7, 4, 1, 10]),
> PartialPermNC([1, 3, 4, 5, 7, 9, 10], [7, 10, 2, 8, 9, 1, 6]),
> PartialPermNC([1, 3, 4, 5, 7, 8, 9], [7, 2, 3, 5, 10, 4, 6]),
> PartialPermNC([1, 2, 3, 7, 8, 9], [10, 7, 5, 4, 1, 9]),
> PartialPermNC([1, 3, 4, 5, 6, 8, 9], [8, 1, 7, 10, 4, 5, 9]),
> PartialPermNC([1, 2, 3, 4, 5], [10, 3, 7, 1, 6]),
> PartialPermNC([1, 2, 3, 5, 7, 8, 9, 10], [3, 5, 7, 6, 10, 2, 9, 4]),
> PartialPermNC([1, 2, 3, 4, 9], [5, 8, 7, 4, 6]),
> PartialPermNC([1, 2, 3, 4, 5, 8, 9], [2, 1, 10, 5, 8, 9, 6]),
> PartialPermNC([1, 2, 3, 5, 6, 7, 8], [8, 3, 2, 4, 9, 7, 1]),
> PartialPermNC([1, 2, 3, 4, 5, 7, 8], [3, 5, 1, 9, 4, 6, 8]),
> PartialPermNC([1, 2, 3, 5, 6, 8], [3, 7, 2, 5, 10, 9]),
> PartialPermNC([1, 2, 3, 6, 7, 9, 10], [5, 7, 2, 4, 6, 10, 1]),
> PartialPermNC([1, 2, 3, 5, 6, 8], [7, 5, 4, 1, 3, 10]),
> PartialPermNC([1, 2, 3, 6, 7, 8, 9, 10], [9, 6, 8, 3, 5, 7, 10, 4]),
> PartialPermNC([1, 2, 3, 5, 8], [8, 2, 5, 1, 10]),
> PartialPermNC([1, 2, 3, 7, 8, 9], [5, 3, 7, 1, 2, 6]),
> PartialPermNC([1, 2, 3, 4, 9], [6, 9, 10, 2, 1]),
> PartialPermNC([1, 2, 3, 5, 9], [8, 3, 9, 7, 5]),
> PartialPermNC([1, 2, 3, 4, 6, 8, 9, 10], [6, 7, 10, 1, 4, 2, 8, 5]),
> PartialPermNC([1, 2, 3, 4, 5, 6], [7, 10, 3, 9, 2, 8]),
> PartialPermNC([1, 2, 4, 5, 6, 8], [9, 1, 8, 7, 6, 3]),
> PartialPermNC([1, 2, 4, 5, 6, 7], [6, 4, 1, 5, 3, 9]),
> PartialPermNC([1, 2, 3, 5, 6, 7, 10], [8, 2, 4, 9, 3, 7, 10]),
> PartialPermNC([1, 2, 4, 5, 6, 8], [6, 2, 3, 8, 4, 10]),
> PartialPermNC([1, 2, 3, 6, 7, 9, 10], [10, 2, 7, 4, 9, 1, 8]),
> PartialPermNC([1, 2, 4, 5, 6, 7, 8], [2, 3, 8, 6, 9, 5, 1]),
> PartialPermNC([1, 3, 4, 5, 7, 8], [4, 8, 2, 6, 5, 9]),
> PartialPermNC([1, 2, 3, 5, 7, 8, 9], [6, 9, 7, 3, 5, 2, 10]),
> PartialPermNC([1, 2, 4, 7, 9, 10], [8, 2, 3, 5, 1, 6]),
> PartialPermNC([1, 2, 5, 8, 9, 10], [7, 1, 3, 10, 5, 8]),
> PartialPermNC([1, 2, 3, 4, 5, 6, 9], [1, 3, 5, 4, 9, 6, 7])];;
gap> s := InverseSemigroup(gens, rec(acting := true));
<inverse partial perm semigroup of rank 10 with 100 generators>
gap> Size(s);
89616897
gap> s := InverseSemigroup(gens[1]);
<inverse partial perm semigroup of rank 10 with 1 generator>
gap> for i in [2 .. 100] do
> s := ClosureInverseSemigroup(s, gens[i], rec(acting := true));
> od;
gap> s;
<inverse partial perm semigroup of rank 10 with 54 generators>
gap> Size(s);
89616897
gap> s := InverseMonoid(gens, rec(acting := true));
<inverse partial perm monoid of rank 10 with 100 generators>
gap> Size(s);
89616898
gap> IsSubsemigroup(s, InverseSemigroup(s, rec(small := true, acting := true)));
true
gap> s := InverseMonoid(gens[1]);;
gap> for i in [2 .. 100] do
> s := ClosureInverseSemigroup(s, gens[i], rec(acting := true));
> od;
gap> s;
<inverse partial perm monoid of rank 10 with 54 generators>
gap> Size(s);
89616898
gap> NrDClasses(s);
15
gap> s := InverseSemigroup(gens, rec(small := true, acting := true));;
gap> NrDClasses(s);
14

# SemigroupsTest2: Inverse monoid of partial perms
gap> s := InverseMonoid(PartialPermNC([1, 2, 3, 5], [5, 6, 8, 2]),
> PartialPermNC([1, 2, 3, 5, 9, 10], [7, 2, 1, 5, 9, 4]));;
gap> Generators(s);
[ [1,5,2,6][3,8], [3,1,7][10,4](2)(5)(9) ]
gap> GeneratorsOfInverseSemigroup(s);
[ [1,5,2,6][3,8], [3,1,7][10,4](2)(5)(9), 
  <identity partial perm on [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]> ]
gap> GeneratorsOfInverseMonoid(s);
[ [1,5,2,6][3,8], [3,1,7][10,4](2)(5)(9) ]
gap> GeneratorsOfSemigroup(s);
[ <identity partial perm on [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]>, 
  [1,5,2,6][3,8], [3,1,7][10,4](2)(5)(9), [6,2,5,1][8,3], 
  [4,10][7,1,3](2)(5)(9) ]
gap> GeneratorsOfMonoid(s);
[ [1,5,2,6][3,8], [3,1,7][10,4](2)(5)(9), [6,2,5,1][8,3], 
  [4,10][7,1,3](2)(5)(9) ]

# SemigroupsTest3: Dihedral (perm) group to a partial perm semigroup
gap> g := DihedralGroup(8);;
gap> g := Range(IsomorphismPermGroup(g));
Group([ (1,2)(3,8)(4,6)(5,7), (1,3,4,7)(2,5,6,8), (1,4)(2,6)(3,7)(5,8) ])
gap> iso := IsomorphismPartialPermSemigroup(g);;
gap> Range(iso);
<partial perm group of size 8, rank 8 with 3 generators>
gap> inv := InverseGeneralMapping(iso);;
gap> f := (1, 5)(2, 3)(4, 8)(6, 7);;
gap> f ^ iso;
(1,5)(2,3)(4,8)(6,7)
gap> (f ^ iso) ^ inv;
(1,5)(2,3)(4,8)(6,7)
gap> ForAll(g, f -> (f ^ iso) ^ inv = f);
true
gap> Size(Range(iso));
8

# SemigroupsTest4: Symmetric (perm) group to a partial perm semigroup
gap> s := Range(IsomorphismPartialPermSemigroup(SymmetricGroup(4)));
<partial perm group of size 24, rank 4 with 2 generators>
gap> iso := IsomorphismPermGroup(s);;
gap> Range(iso);
Group([ (1,2,3,4), (1,2) ])
gap> inv := InverseGeneralMapping(iso);;
gap> f := PartialPerm([1, 2, 3, 4], [2, 1, 3, 4]);
(1,2)(3)(4)
gap> f in s;
true
gap> f ^ iso;
(1,2)
gap> (f ^ iso) ^ inv;
(1,2)(3)(4)
gap> ForAll(s, f -> (f ^ iso) ^ inv = f);
true
gap> Size(s);
24
gap> Size(Range(iso));
24

# SemigroupsTest5: FreeInverseSemigroup
gap> S := FreeInverseSemigroup(3);
<free inverse semigroup on the generators [ x1, x2, x3 ]>
gap> Size(S);
infinity
gap> x := S.1;
x1
gap> y := S.2;
x2
gap> z := S.3;
x3
gap> u := x ^ 5 * y ^ 3 * z;
x1*x1*x1*x1*x1*x2*x2*x2*x3
gap> u ^ -1;
x3^-1*x2^-1*x2^-1*x2^-1*x1^-1*x1^-1*x1^-1*x1^-1*x1^-1
gap> x ^ 2 * y = x ^ 2 * y;
true
gap> x * x ^ -1 = y * y ^ -1;
false

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/semigroups.tst");
