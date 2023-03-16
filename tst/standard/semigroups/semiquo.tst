#############################################################################
##
#W  standard/semigroups/semiquo.tst
#Y  Copyright (C) 2015-2022                              James D. Mitchell 
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local G, H, I, J, Q, R, S, T, cong, gens, images, map, pair
gap> START_TEST("Semigroups package: standard/semigroups/semiquo.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# quotients, OneImmutable
gap> S := PartitionMonoid(4);
<regular bipartition *-monoid of size 4140, degree 4 with 4 generators>
gap> cong := SemigroupCongruence(S, [S.3, S.4]);
<2-sided semigroup congruence over <regular bipartition *-monoid 
 of size 4140, degree 4 with 4 generators> with 1 generating pairs>
gap> T := S / cong;;
gap> Size(T);
25
gap> One(T);
<2-sided congruence class of <block bijection: [ 1, -1 ], [ 2, -2 ], 
 [ 3, -3 ], [ 4, -4 ]>>

# quotients, GeneratorsOfSemigroup
gap> S := JonesMonoid(5);
<regular bipartition *-monoid of degree 5 with 4 generators>
gap> I := SemigroupIdeal(S, S.4);
<regular bipartition *-semigroup ideal of degree 5 with 1 generator>
gap> J := SemigroupIdeal(I, Bipartition([[1, -3], [2, -4], [3, 4], [5, -5],
> [-1, -2]]));
<regular bipartition *-semigroup ideal of degree 5 with 1 generator>
gap> T := I / J;;
gap> HasGeneratorsOfMagma(T);
false
gap> GeneratorsOfSemigroup(T);
[ <2-sided congruence class of <bipartition: [ 1, -3 ], [ 2, -4 ], [ 3, 4 ], 
     [ 5, -5 ], [ -1, -2 ]>> ]

# quotients, Rees quotient
gap> S := PartitionMonoid(4);
<regular bipartition *-monoid of size 4140, degree 4 with 4 generators>
gap> I := SemigroupIdeal(S, S.4);
<regular bipartition *-semigroup ideal of degree 4 with 1 generator>
gap> T := S / I;;
gap> Size(T);
25

# quotients, PROD_SCL_LIST_DEFAULT, PROD_LIST_SCL_DEFAULT
gap> S := Semigroup([Matrix(IsTropicalMaxPlusMatrix, [[0, 0], [1, 1]], 2),
>  Matrix(IsTropicalMaxPlusMatrix, [[1, 2], [0, -infinity]], 2),
>  Matrix(IsTropicalMaxPlusMatrix, [[2, 2], [1, 0]], 2)]);
<semigroup of 2x2 tropical max-plus matrices with 3 generators>
gap> cong := SemigroupCongruence(S, [S.3, S.1]);
<2-sided semigroup congruence over <non-regular semigroup 
 of size 9, 2x2 tropical max-plus matrices with 3 generators> with 
1 generating pairs>
gap> T := S / cong;;
gap> AsList(T) * T.1;
[ <2-sided congruence class of Matrix(IsTropicalMaxPlusMatrix, 
     [[1, 1], [2, 2]], 2)>, 
  <2-sided congruence class of Matrix(IsTropicalMaxPlusMatrix, 
     [[2, 2], [0, 0]], 2)>, 
  <2-sided congruence class of Matrix(IsTropicalMaxPlusMatrix, 
     [[2, 2], [2, 2]], 2)> ]
gap> T.1 * AsList(T);
[ <2-sided congruence class of Matrix(IsTropicalMaxPlusMatrix, 
     [[1, 1], [2, 2]], 2)>, 
  <2-sided congruence class of Matrix(IsTropicalMaxPlusMatrix, 
     [[1, 2], [2, 2]], 2)>, 
  <2-sided congruence class of Matrix(IsTropicalMaxPlusMatrix, 
     [[2, 2], [2, 2]], 2)> ]
gap> GreensRClasses(S) * T.1;
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `*' on 2 arguments
gap> T.1 * GreensRClasses(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `*' on 2 arguments

# quotients, ViewObj
gap> S := Semigroup([Transformation([2, 3, 2]), Transformation([3, 1, 3])]);;
gap> pair := [Transformation([3, 2, 3]), Transformation([1, 1, 1])];;
gap> cong := SemigroupCongruence(S, [pair]);;
gap> Q := S / cong;
<quotient of <2-sided semigroup congruence over <transformation semigroup of 
 degree 3 with 2 generators> with 1 generating pairs>>
gap> Size(Q);
1
gap> I := MinimalIdeal(S);
<simple transformation semigroup ideal of degree 3 with 1 generator>
gap> R := S / I;;
gap> Size(R);
5

# Issue 456 - Quotient semigroups are slow
gap> S := InverseSemigroup([PartialPerm([1, 2, 3, 4, 5], [1, 2, 3, 4, 5]),
>   PartialPerm([1, 2, 3, 4, 5, 7], [2, 5, 1, 4, 3, 6]),
>   PartialPerm([1, 2, 3, 4, 5, 6], [4, 3, 5, 1, 2, 9]),
>   PartialPerm([1, 2, 3, 4, 5, 10], [5, 2, 4, 1, 3, 10]),
>   PartialPerm([1, 2, 3, 4, 5, 8], [1, 5, 4, 2, 3, 7]),
>   PartialPerm([1, 2, 3, 4, 5, 7], [3, 4, 1, 5, 2, 6]),
>   PartialPerm([1, 2, 3, 4, 5, 10], [1, 2, 4, 3, 5, 10]),
>   PartialPerm([1, 2, 3, 4, 5, 7], [2, 1, 4, 3, 5, 9]),
>   PartialPerm([1, 2, 3, 4, 5, 10], [2, 4, 5, 1, 3, 9]),
>   PartialPerm([1, 2, 3, 4, 5, 9], [2, 3, 4, 1, 5, 6]),
>   PartialPerm([1, 2, 3, 4, 5, 9], [2, 1, 3, 4, 5, 9]),
>   PartialPerm([1, 2, 3, 4, 5, 9], [5, 4, 1, 3, 2, 7]),
>   PartialPerm([1, 2, 3, 4, 5, 7], [4, 2, 5, 1, 3, 8]),
>   PartialPerm([1, 2, 3, 4, 5, 9], [4, 1, 3, 2, 5, 10])]);;
gap> Size(S);
720
gap> cong := MinimumGroupCongruence(S);
<semigroup congruence over <inverse partial perm semigroup of size 720, 
 rank 10 with 14 generators> with congruence pair (6,1)>
gap> Size(S / cong);
120
gap> IsomorphismPermGroup(S / cong);
<quotient of <semigroup congruence over <inverse partial perm semigroup 
  of size 720, rank 10 with 14 generators> with congruence pair (6,1)>> -> 
<permutation group of size 120 with 2 generators>

# Issue 456 - Quotient semigroups are slow
gap> S := InverseSemigroup([PartialPerm([1, 2, 3, 4, 5], [1, 2, 3, 4, 5]),
>   PartialPerm([1, 2, 3, 4, 5, 6], [5, 2, 3, 1, 4, 8]),
>   PartialPerm([1, 2, 3, 4, 5, 6], [3, 4, 5, 2, 1, 7]),
>   PartialPerm([1, 2, 3, 4, 5, 8], [3, 1, 4, 2, 5, 8]),
>   PartialPerm([1, 2, 3, 4, 5, 7], [1, 4, 3, 2, 5, 7]),
>   PartialPerm([1, 2, 3, 4, 5, 7], [2, 4, 5, 1, 3, 8]),
>   PartialPerm([1, 2, 3, 4, 5, 7], [1, 2, 5, 4, 3, 8]),
>   PartialPerm([1, 2, 3, 4, 5, 8], [4, 5, 2, 3, 1, 6])]);
<inverse partial perm semigroup of rank 8 with 8 generators>
gap> Size(S);
336
gap> cong := MinimumGroupCongruence(S);
<semigroup congruence over <inverse partial perm semigroup of size 336, 
 rank 8 with 8 generators> with congruence pair (4,1)>
gap> Size(S / cong);
120
gap> IsomorphismPermGroup(S / cong);
<quotient of <semigroup congruence over <inverse partial perm semigroup 
  of size 336, rank 8 with 8 generators> with congruence pair (4,1)>> -> 
<permutation group of size 120 with 2 generators>

# Issue 454 - Missing functionality for quotient semigroups
gap> S := InverseSemigroup([PartialPerm([1, 2, 3, 4], [1, 2, 3, 4]),
>   PartialPerm([1, 2, 3, 4, 5, 6, 7, 8, 11, 12, 13, 14, 19, 20, 29, 30],
>      [2, 1, 4, 3, 6, 5, 8, 7, 12, 11, 14, 13, 20, 19, 30, 29]),
>   PartialPerm([1, 2, 3, 4, 5, 6, 7, 8, 11, 12, 13, 14, 19, 20, 29, 30],
>      [4, 3, 2, 1, 8, 7, 6, 5, 10, 9, 16, 15, 18, 17, 32, 31]),
>   PartialPerm([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 16, 17, 18, 31, 32],
>      [2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 16, 15, 18, 17, 32, 31]),
>   PartialPerm([1, 2, 3, 4, 21, 22, 23, 24, 25, 26, 27, 28, 33, 34, 35, 36],
>      [4, 3, 2, 1, 24, 23, 22, 21, 28, 27, 26, 25, 36, 35, 34, 33]),
>   PartialPerm([1, 2, 3, 4, 21, 22, 23, 24, 25, 26, 27, 28, 33, 34, 35, 36],
>      [3, 4, 1, 2, 23, 24, 21, 22, 27, 28, 25, 26, 35, 36, 33, 34]),
>   PartialPerm([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 16, 17, 18, 31, 32],
>      [4, 3, 2, 1, 8, 7, 6, 5, 12, 11, 14, 13, 20, 19, 30, 29]),
>   PartialPerm([1, 2, 3, 4, 19, 20, 29, 30], [4, 3, 2, 1, 18, 17, 32, 31]),
>   PartialPerm([1, 2, 3, 4, 22, 24, 33, 35], [4, 3, 2, 1, 23, 21, 36, 34])]);
<inverse partial perm semigroup of rank 36 with 9 generators>
gap> cong := MinimumGroupCongruence(S);
<semigroup congruence over <inverse partial perm semigroup of size 36, 
 rank 36 with 9 generators> with congruence pair (9,1)>
gap> G := S / cong;
<quotient of <semigroup congruence over <inverse partial perm semigroup 
 of size 36, rank 36 with 9 generators> with congruence pair (9,1)>>
gap> IrredundantGeneratingSubset(S / cong);
[ <2-sided congruence class of (1,3)(2,4)(21,23)(22,24)(25,27)(26,28)(33,35)
    (34,36)>, <2-sided congruence class of [11,10][12,9][13,16][14,15][19,18]
    [20,17][29,32][30,31](1,4)(2,3)(5,8)(6,7)> ]
gap> SmallSemigroupGeneratingSet(S / cong);
[ <2-sided congruence class of <identity partial perm on [ 1, 2, 3, 4 ]>>, 
  <2-sided congruence class of (1,3)(2,4)(21,23)(22,24)(25,27)(26,28)(33,35)
    (34,36)>, <2-sided congruence class of (1,2)(3,4)(5,6)(7,8)(11,12)(13,14)
    (19,20)(29,30)> ]
gap> GeneratorsSmallest(S / cong);
[ <2-sided congruence class of <identity partial perm on [ 1, 2, 3, 4 ]>>, 
  <2-sided congruence class of (1,2)(3,4)(5,6)(7,8)(11,12)(13,14)(19,20)
    (29,30)>, <2-sided congruence class of [11,10][12,9][13,16][14,15][19,18]
    [20,17][29,32][30,31](1,4)(2,3)(5,8)(6,7)> ]

# Issue 816 - Subsemigroups of quotient semigroups
gap> S := Semigroup(Transformation([2, 1, 5, 1, 5]),
>                   Transformation([1, 1, 1, 5, 3]), 
>                   Transformation([2, 5, 3, 5, 3]));;
gap> cong := SemigroupCongruence(S, [Transformation([1, 2, 5, 2, 5]),
>                                    Transformation([2, 1, 5, 1, 5])]);
<2-sided semigroup congruence over <transformation semigroup of degree 5 with 
 3 generators> with 1 generating pairs>
gap> T := S / cong;;
gap> gens := GeneratorsOfSemigroup(S);;
gap> images := List(gens, gen -> EquivalenceClassOfElement(cong, gen));;
gap> Q := Subsemigroup(T, images);
<semigroup with 3 generators>
gap> Q = T;
true
gap> Factorization(T, images[2]);
[ 2 ]
gap> Factorization(Q, images[2]);
[ 2 ]
gap> H := SubsemigroupNC(T, images);
<semigroup with 3 generators>
gap> T = H;
true
gap> Factorization(H, images[2]);
[ 2 ]
gap> map := IsomorphismFpSemigroup(T);
<quotient of <2-sided semigroup congruence over <transformation semigroup of 
  degree 5 with 3 generators> with 1 generating pairs>> -> 
<fp semigroup with 3 generators and 8 relations of length 33>
gap> map := IsomorphismFpSemigroup(Q);
<semigroup of size 4, with 3 generators> -> 
<fp semigroup with 3 generators and 8 relations of length 33>
gap> IsomorphismFpSemigroup(H);
<semigroup of size 4, with 3 generators> -> 
<fp semigroup with 3 generators and 8 relations of length 33>

# Quotients of quotients
gap> S := Semigroup(Transformation([2, 1, 5, 1, 5]),
>                   Transformation([1, 1, 1, 5, 3]),
>                   Transformation([2, 5, 3, 5, 3]));;
gap> cong := SemigroupCongruence(S, [Transformation([1, 2, 5, 2, 5]),
>                                    Transformation([2, 1, 5, 1, 5])]);
<2-sided semigroup congruence over <transformation semigroup of degree 5 with 
 3 generators> with 1 generating pairs>
gap> Q := S / cong;
<quotient of <2-sided semigroup congruence over <transformation semigroup of 
 degree 5 with 3 generators> with 1 generating pairs>>
gap> CongruencesOfSemigroup(Q);
[ <2-sided semigroup congruence over <quotient of <2-sided semigroup congruenc\
e over <transformation semigroup of degree 5 with 3 generators> with 
    1 generating pairs>> with 0 generating pairs>, 
  <universal semigroup congruence over <quotient of <2-sided semigroup congrue\
nce over <transformation semigroup of degree 5 with 3 generators> with 
    1 generating pairs>>>, 
  <2-sided semigroup congruence over <quotient of <2-sided semigroup congruenc\
e over <transformation semigroup of degree 5 with 3 generators> with 
    1 generating pairs>> with 1 generating pairs>, 
  <2-sided semigroup congruence over <quotient of <2-sided semigroup congruenc\
e over <transformation semigroup of degree 5 with 3 generators> with 
    1 generating pairs>> with 1 generating pairs>, 
  <2-sided semigroup congruence over <quotient of <2-sided semigroup congruenc\
e over <transformation semigroup of degree 5 with 3 generators> with 
    1 generating pairs>> with 1 generating pairs>, 
  <2-sided semigroup congruence over <quotient of <2-sided semigroup congruenc\
e over <transformation semigroup of degree 5 with 3 generators> with 
    1 generating pairs>> with 1 generating pairs> ]
gap> map := QuotientSemigroupHomomorphism(Q);
<transformation semigroup of degree 5 with 3 generators> -> 
<quotient of <2-sided semigroup congruence over <transformation semigroup of 
  degree 5 with 3 generators> with 1 generating pairs>>
gap> Q := Q / SemigroupCongruence(Q,
> [[Transformation([2, 1, 5, 1, 5]) ^ map, 
>   Transformation([2, 5, 3, 5, 3]) ^ map]]);
<quotient of <2-sided semigroup congruence over <quotient of <2-sided semigrou\
p congruence over <transformation semigroup of degree 5 with 3 generators>
  with 1 generating pairs>> with 1 generating pairs>>
gap> CongruencesOfSemigroup(Q);
[ <2-sided semigroup congruence over <quotient of <2-sided semigroup congruenc\
e over <quotient of <2-sided semigroup congruence over <transformation 
     semigroup of degree 5 with 3 generators> with 1 generating pairs>> with 
    1 generating pairs>> with 0 generating pairs>, 
  <universal semigroup congruence over <quotient of <2-sided semigroup congrue\
nce over <quotient of <2-sided semigroup congruence over <transformation 
     semigroup of degree 5 with 3 generators> with 1 generating pairs>> with 
    1 generating pairs>>> ]
gap> Size(Q);
2

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/semigroups/semiquo.tst");
