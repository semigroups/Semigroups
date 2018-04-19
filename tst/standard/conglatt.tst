#############################################################################
##
#W  standard/conglatt.tst
#Y  Copyright (C) 2014-16                                   Michael Torpey
##                                                          Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/conglatt.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# Robustness against infinite semigroups
gap> S := FreeSemigroup(2);;
gap> congs := CongruencesOfSemigroup(S);
Error, Semigroups: PosetOfPrincipalCongruences: usage,
first argument <S> must be an enumerable finite semigroup,
gap> poset := PosetOfPrincipalLeftCongruences(S);
Error, Semigroups: PosetOfPrincipalLeftCongruences: usage,
first argument <S> must be an enumerable finite semigroup,
gap> poset := PosetOfPrincipalRightCongruences(S);
Error, Semigroups: PosetOfPrincipalRightCongruences: usage,
first argument <S> must be an enumerable finite semigroup,

# LatticeOfCongruences
gap> S := PartitionMonoid(2);;
gap> l := LatticeOfCongruences(S);
[ [  ], [ 1, 3, 4 ], [ 1 ], [ 1 ], [ 1, 3, 9 ], [ 1, 2, 3, 4, 5, 9, 10 ], 
  [ 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13 ], [ 1, 3 ], [ 1 ], [ 1, 4, 9 ], 
  [ 1, 2, 3, 4, 8 ], [ 1, 3, 5, 8, 9 ], 
  [ 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12 ] ]
gap> Print(l, "\n");
[ [  ], [ 1, 3, 4 ], [ 1 ], [ 1 ], [ 1, 3, 9 ], [ 1, 2, 3, 4, 5, 9, 10 ], 
  [ 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13 ], [ 1, 3 ], [ 1 ], [ 1, 4, 9 ], 
  [ 1, 2, 3, 4, 8 ], [ 1, 3, 5, 8, 9 ], 
  [ 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12 ] ]
gap> IsBound(l[4]);
true
gap> l[4];
[ 1 ]
gap> S := OrderEndomorphisms(2);;
gap> CongruencesOfSemigroup(S);
[ <semigroup congruence over <regular transformation monoid of size 3, 
     degree 2 with 2 generators> with 0 generating pairs>, 
  <semigroup congruence over <regular transformation monoid of size 3, 
     degree 2 with 2 generators> with 1 generating pairs>, 
  <semigroup congruence over <regular transformation monoid of size 3, 
     degree 2 with 2 generators> with 1 generating pairs> ]
gap> l := LatticeOfCongruences(S);
[ [  ], [ 1, 3 ], [ 1 ] ]
gap> CongruencesOfPoset(l) = CongruencesOfSemigroup(S);
true
gap> DotString(l);
"//dot\ngraph graphname {\n     node [shape=circle]\n2 -- 3\n3 -- 1\n }"
gap> S := Semigroup([Transformation([1, 4, 3, 1, 4, 2]),
>                    Transformation([1, 6, 6, 3, 6, 6])]);;
gap> l := LatticeOfCongruences(S);
[ [  ], [ 1 ], [ 1, 2, 4 ], [ 1, 2 ], [ 1, 2, 3, 4 ] ]
gap> DotString(l, rec(info := true)) = Concatenation("//dot\ngraph graphname",
> " {\n     node [shape=circle]\nR2 -- T\nR3 -- 4\n4 -- R2\nU -- R3\n }");
true
gap> S := Semigroup([Transformation([1, 1, 2, 1]),
>                    Transformation([3, 3, 1, 2])]);;
gap> l := LatticeOfCongruences(S);;
gap> DotString(l) = Concatenation(
> "//dot\ngraph graphname {\n     node [shape=point]\n2 -- 3\n2 -- 7\n3 -- 8\n",
> "4 -- 1\n5 -- 22\n6 -- 5\n6 -- 18\n7 -- 8\n7 -- 25\n8 -- 9\n9 -- 1\n10 -- 33\n",
> "11 -- 5\n11 -- 23\n12 -- 5\n13 -- 10\n13 -- 41\n14 -- 1\n15 -- 9\n16 -- 2\n16",
> " -- 17\n16 -- 31\n17 -- 3\n17 -- 33\n18 -- 16\n18 -- 22\n18 -- 26\n19 -- 6\n1",
> "9 -- 11\n19 -- 12\n19 -- 20\n20 -- 18\n20 -- 23\n20 -- 27\n20 -- 36\n21 -- 2",
> "\n21 -- 24\n21 -- 30\n22 -- 10\n22 -- 17\n23 -- 13\n23 -- 22\n23 -- 37\n24 --",
> " 3\n24 -- 32\n25 -- 4\n25 -- 9\n26 -- 10\n26 -- 31\n27 -- 13\n27 -- 26\n27 --",
> " 40\n28 -- 4\n28 -- 14\n29 -- 15\n29 -- 25\n30 -- 7\n30 -- 32\n30 -- 38\n31 -",
> "- 7\n31 -- 29\n31 -- 33\n32 -- 8\n32 -- 34\n33 -- 8\n33 -- 15\n34 -- 9\n34 --",
> " 14\n35 -- 15\n35 -- 34\n36 -- 16\n36 -- 21\n36 -- 37\n36 -- 40\n37 -- 17\n37",
> " -- 24\n37 -- 41\n38 -- 25\n38 -- 28\n38 -- 34\n39 -- 29\n39 -- 35\n39 -- 38",
> "\n40 -- 30\n40 -- 31\n40 -- 39\n40 -- 41\n41 -- 32\n41 -- 33\n41 -- 35\n }");
true

# Left/RightCongruences (as a list)
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> Size(LeftCongruencesOfSemigroup(S));
21
gap> Size(RightCongruencesOfSemigroup(S));
31

# LatticeOfLeft/RightCongruences
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> LatticeOfLeftCongruences(S);
[ [  ], [ 1, 9, 12 ], [ 1 ], [ 1, 2, 3, 9, 12, 13, 15, 17 ], 
  [ 1, 3, 8, 11, 12, 13, 16, 17 ], [ 1, 3 ], 
  [ 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21 ], 
  [ 1, 11, 13 ], [ 1, 12 ], [ 1, 3, 12, 13, 17 ], [ 1, 13 ], [ 1 ], [ 1 ], 
  [ 1, 2, 3, 4, 9, 11, 12, 13, 15, 16, 17, 21 ], [ 1, 3, 9, 12, 13, 17 ], 
  [ 1, 3, 11, 12, 13, 17 ], [ 1, 3, 12, 13 ], 
  [ 1, 3, 5, 8, 9, 11, 12, 13, 15, 16, 17, 21 ], 
  [ 1, 3, 6, 9, 10, 11, 12, 13, 15, 16, 17, 20, 21 ], [ 1, 3, 6, 12, 13, 17 ],
  [ 1, 3, 9, 11, 12, 13, 15, 16, 17 ] ]
gap> LatticeOfRightCongruences(S);
[ [  ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1, 2, 5, 8, 14, 24 ], 
  [ 1, 3, 5, 10, 12, 23 ], [ 1 ], [ 1, 4, 8, 10 ], [ 1 ], [ 1 ], [ 1 ], 
  [ 1, 3, 8, 11 ], [ 1 ], [ 1, 2, 10, 11 ], [ 1, 2, 3, 4 ], 
  [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 19, 20, 21, 
      22, 23, 24, 25, 26, 27, 28, 29, 30, 31 ], [ 1, 2, 12 ], [ 1, 3, 14 ], 
  [ 1, 4, 5 ], [ 1, 4, 11, 12, 14, 29 ], [ 1, 5, 11 ], [ 1, 5, 12 ], 
  [ 1, 5, 14 ], [ 1, 2, 5, 6, 8, 12, 14, 18, 23, 24, 27, 29, 31 ], 
  [ 1, 3, 5, 7, 10, 12, 14, 19, 23, 24, 28, 29, 31 ], [ 1, 8, 12 ], 
  [ 1, 10, 14 ], [ 1, 12, 14 ], 
  [ 1, 4, 5, 11, 12, 14, 20, 21, 22, 23, 24, 29, 31 ], 
  [ 1, 5, 12, 14, 23, 24, 29 ] ]
gap> LatticeOfCongruences(S);
[ [  ], [ 1, 3, 4 ], [ 1 ], [ 1, 3 ] ]
gap> Size(CongruencesOfSemigroup(S));
4

# LatticeOfLeft/RightCongruences with restriction
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> restriction := Subsemigroup(S, [Transformation([1, 1, 1]),
>                                    Transformation([2, 2, 2]),
>                                    Transformation([3, 3, 3])]);;
gap> latt := LatticeOfLeftCongruences(S, restriction);
[ [  ], [ 1 ], [ 1 ], [ 1 ], [ 1, 2, 3, 4 ] ]
gap> restriction := [Transformation([3, 2, 3]),
>                    Transformation([3, 1, 3]),
>                    Transformation([2, 2, 2])];;
gap> latt := LatticeOfRightCongruences(S, restriction);
[ [  ], [ 1, 3, 4 ], [ 1 ], [ 1 ] ]
gap> congs := CongruencesOfPoset(latt);;
gap> Length(congs);
4
gap> IsDuplicateFreeList(congs);
true
gap> restriction := [Transformation([3, 1, 3]), Transformation([3, 2, 3])];;
gap> latt := LatticeOfCongruences(S, restriction);
[ [  ], [ 1 ] ]
gap> restriction := [Transformation([3, 3, 3])];;
gap> latt := LatticeOfCongruences(S, restriction);
[ [  ] ]

# LatticeOf(Left/Right)Congruences with invalid restriction
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> restriction := [Transformation([1, 1, 1]), Transformation([2, 2, 2, 2])];;
gap> LatticeOfCongruences(S, restriction);
Error, Semigroups: PosetOfPrincipalCongruences: usage,
<restriction> must be a subset of <S>,
gap> LatticeOfLeftCongruences(S, restriction);
Error, Semigroups: PosetOfPrincipalLeftCongruences: usage,
<restriction> must be a subset of <S>,
gap> LatticeOfRightCongruences(S, restriction);
Error, Semigroups: PosetOfPrincipalRightCongruences: usage,
<restriction> must be a subset of <S>,

# Left/RightCongruences (as a list)
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> Size(LeftCongruencesOfSemigroup(S));
21
gap> Size(RightCongruencesOfSemigroup(S));
31

# PosetOfPrincipalLeft/RightCongruences
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> PosetOfPrincipalLeftCongruences(S);
[ [ 8, 11 ], [  ], [ 1, 2, 8, 11, 12 ], [ 2, 7, 10, 11, 12 ], [ 2 ], 
  [ 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12 ], [ 10, 12 ], [ 11 ], [ 2, 11, 12 ], 
  [ 12 ], [  ], [  ] ]
gap> PosetOfPrincipalRightCongruences(S);
[ [  ], [  ], [  ], [  ], [ 1, 4, 7, 13 ], [ 2, 4, 9, 11 ], [  ], 
  [ 3, 7, 9 ], [  ], [  ], [  ], [ 2, 7, 10 ], [  ], [ 1, 9, 10 ], 
  [ 1, 2, 3 ] ]
gap> PosetOfPrincipalCongruences(S);
[ [ 2, 3 ], [  ], [ 2 ] ]
gap> Size(PrincipalCongruencesOfSemigroup(S));
3

# PosetOfPrincipalLeft/RightCongruences with restriction
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> restriction := Subsemigroup(S, [Transformation([1, 1, 1]),
>                                    Transformation([2, 2, 2]),
>                                    Transformation([3, 3, 3])]);;
gap> latt := PosetOfPrincipalLeftCongruences(S, restriction);
[ [  ], [  ], [  ] ]
gap> restriction := [Transformation([3, 2, 3]),
>                    Transformation([3, 1, 3]),
>                    Transformation([2, 2, 2])];;
gap> latt := PosetOfPrincipalRightCongruences(S, restriction);
[ [ 2, 3 ], [  ], [  ] ]
gap> CongruencesOfPoset(latt);
[ <right semigroup congruence over <transformation semigroup of degree 3 with 
     2 generators> with 1 generating pairs>, <right semigroup congruence over 
    <transformation semigroup of degree 3 with 2 generators> with 
    1 generating pairs>, <right semigroup congruence over <transformation 
     semigroup of degree 3 with 2 generators> with 1 generating pairs> ]
gap> restriction := [Transformation([3, 1, 3]), Transformation([3, 2, 3])];;
gap> latt := PosetOfPrincipalCongruences(S, restriction);
[ [  ] ]
gap> restriction := [Transformation([3, 3, 3])];;
gap> latt := PosetOfPrincipalCongruences(S, restriction);
[  ]

# PosetOfPrincipal(Left/Right)Congruences with invalid restriction
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> restriction := [Transformation([1, 1, 1]), Transformation([2, 2, 2, 2])];;
gap> PosetOfPrincipalCongruences(S, restriction);
Error, Semigroups: PosetOfPrincipalCongruences: usage,
<restriction> must be a subset of <S>,
gap> PosetOfPrincipalLeftCongruences(S, restriction);
Error, Semigroups: PosetOfPrincipalLeftCongruences: usage,
<restriction> must be a subset of <S>,
gap> PosetOfPrincipalRightCongruences(S, restriction);
Error, Semigroups: PosetOfPrincipalRightCongruences: usage,
<restriction> must be a subset of <S>,

# PrincipalCongruencesOfSemigroup
gap> S := Semigroup(Transformation([1, 3, 2]),
>                   Transformation([3, 1, 3]));;
gap> congs := PrincipalCongruencesOfSemigroup(S);
[ <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 1 generating pairs>, 
  <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 1 generating pairs>, 
  <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 1 generating pairs>, 
  <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 1 generating pairs>, 
  <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 1 generating pairs> ]

# PrincipalLeft/RightCongruencesOfSemigroup
gap> S := Semigroup([Transformation([1, 1]), Transformation([2, 1])]);;
gap> Length(PrincipalLeftCongruencesOfSemigroup(S));
3
gap> Length(PrincipalRightCongruencesOfSemigroup(S));
4
gap> PrincipalRightCongruencesOfSemigroup(S)[1];
<right semigroup congruence over <transformation semigroup of size 4, 
 degree 2 with 2 generators> with 1 generating pairs>
gap> PrincipalLeftCongruencesOfSemigroup(S)[2];
<left semigroup congruence over <transformation semigroup of size 4, degree 2 
 with 2 generators> with 1 generating pairs>

# MinimalCongruencesOfSemigroup
gap> S := Semigroup([Transformation([1, 3, 2]), Transformation([3, 1, 3])]);;
gap> min := MinimalCongruencesOfSemigroup(S);
[ <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 1 generating pairs> ]
gap> congs := CongruencesOfSemigroup(S);
[ <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 0 generating pairs>, 
  <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 1 generating pairs>, 
  <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 1 generating pairs>, 
  <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 1 generating pairs>, 
  <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 1 generating pairs>, 
  <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 1 generating pairs> ]
gap> l := LatticeOfCongruences(S);
[ [  ], [ 1, 5, 6 ], [ 1, 2, 4, 5, 6 ], [ 1, 2, 5, 6 ], [ 1, 6 ], [ 1 ] ]
gap> Position(congs, min[1]) = Position(l, [1]);
true
gap> minl := MinimalLeftCongruencesOfSemigroup(S);;
gap> Size(minl);
3
gap> minr := MinimalRightCongruencesOfSemigroup(S);;
gap> Size(minr);
9
gap> PositionsProperty(minl, c -> IsSubrelation(min[1], c));
[ 1, 2, 3 ]
gap> PositionsProperty(minr, c -> IsSubrelation(min[1], c));
[ 9 ]

# Biggish example which forces garbage collection
gap> S := Semigroup([Transformation([4, 2, 4, 4, 1]),
>                    Transformation([4, 4, 1, 2, 2]),
>                    Transformation([3, 3, 1, 2, 5])]);;
gap> MinimalCongruencesOfSemigroup(S);
[ <semigroup congruence over <transformation semigroup of size 68, degree 5 
     with 3 generators> with 1 generating pairs>, 
  <semigroup congruence over <transformation semigroup of size 68, degree 5 
     with 3 generators> with 1 generating pairs>, 
  <semigroup congruence over <transformation semigroup of size 68, degree 5 
     with 3 generators> with 1 generating pairs> ]

# JoinSemilatticeOfCongruences
gap> S := SymmetricInverseMonoid(2);;
gap> pair1 := [PartialPerm([1], [1]), PartialPerm([2], [1])];;
gap> pair2 := [PartialPerm([1], [1]), PartialPerm([1, 2], [1, 2])];;
gap> pair3 := [PartialPerm([1, 2], [1, 2]), PartialPerm([1, 2], [2, 1])];;
gap> coll := [RightSemigroupCongruence(S, pair1),
>             RightSemigroupCongruence(S, pair2),
>             RightSemigroupCongruence(S, pair3)];;
gap> JoinSemilatticeOfCongruences(coll, JoinRightSemigroupCongruences);
[ [  ], [  ], [ 1 ], [ 1, 2, 3 ] ]
gap> JoinSemilatticeOfCongruences(coll, JoinLeftSemigroupCongruences);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `JoinLeftSemigroupCongruences' on 2 argu\
ments

# MinimalCongruences
gap> S := SymmetricInverseMonoid(2);;
gap> pair1 := [PartialPerm([1], [1]), PartialPerm([2], [1])];;
gap> pair2 := [PartialPerm([1], [1]), PartialPerm([1, 2], [1, 2])];;
gap> pair3 := [PartialPerm([1, 2], [1, 2]), PartialPerm([1, 2], [2, 1])];;
gap> coll := [RightSemigroupCongruence(S, pair1),
>             RightSemigroupCongruence(S, pair2),
>             RightSemigroupCongruence(S, pair3)];;
gap> MinimalCongruences(coll) = coll{[1, 2]};
true
gap> MinimalCongruences(PosetOfCongruences(coll)) = coll{[1, 2]};
true
gap> poset := LatticeOfCongruences(S);
[ [  ], [ 1 ], [ 1, 2, 4 ], [ 1, 2 ] ]
gap> MinimalCongruences(poset);
[ <semigroup congruence over <symmetric inverse monoid of degree 2> with 
    0 generating pairs> ]
gap> MinimalCongruences([]);
[  ]

# PosetOfCongruences
gap> S := OrderEndomorphisms(2);;
gap> pair1 := [Transformation([1, 1]), IdentityTransformation];;
gap> pair2 := [IdentityTransformation, Transformation([2, 2])];;
gap> coll := [RightSemigroupCongruence(S, pair1),
>             RightSemigroupCongruence(S, pair2),
>             RightSemigroupCongruence(S, [])];;
gap> PosetOfCongruences(coll);
[ [ 3 ], [ 3 ], [  ] ]

# Trivial poset
gap> poset := PosetOfCongruences([]);
[  ]
gap> CongruencesOfPoset(poset);
[  ]
gap> Size(poset);
0
gap> JoinSemilatticeOfCongruences(poset, JoinSemigroupCongruences);
[  ]
gap> MinimalCongruences(poset);
[  ]

# Test Issue 309
gap> S := Semigroup(Transformation([2, 1, 4, 3, 5, 2]),
>                   Transformation([3, 4, 1, 2, 5, 3]),
>                   Transformation([5, 5, 5, 5, 5, 5]));;
gap> LatticeOfCongruences(S);
[ [  ], [ 1 ], [ 1 ], [ 1 ], [ 1, 2, 3, 4, 6 ], [ 1, 2, 3, 4 ] ]

# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(coll);
gap> Unbind(congs);
gap> Unbind(l);
gap> Unbind(latt);
gap> Unbind(min);
gap> Unbind(minl);
gap> Unbind(minr);
gap> Unbind(pair1);
gap> Unbind(pair2);
gap> Unbind(pair3);
gap> Unbind(poset);
gap> Unbind(restriction);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/conglatt.tst");
