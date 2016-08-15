#############################################################################
##
#W  standard/conglatt.tst
#Y  Copyright (C) 2014-16                                   Michael Torpey
##                                                          Wilfred Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/conglatt.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

#T# LatticeOfCongruences
gap> S := PartitionMonoid(2);;
gap> l := LatticeOfCongruences(S);
[ [  ], [ 1, 3, 7 ], [ 1 ], [ 1, 3, 8 ], 
  [ 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13 ], [ 1, 2, 3, 4, 7, 8, 9 ], [ 1 ], 
  [ 1 ], [ 1, 7, 8 ], [ 1, 3 ], [ 1, 2, 3, 7, 10 ], [ 1, 3, 4, 8, 10 ], 
  [ 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12 ] ]
gap> Print(l, "\n");
[ [  ], [ 1, 3, 7 ], [ 1 ], [ 1, 3, 8 ], 
  [ 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13 ], [ 1, 2, 3, 4, 7, 8, 9 ], [ 1 ], 
  [ 1 ], [ 1, 7, 8 ], [ 1, 3 ], [ 1, 2, 3, 7, 10 ], [ 1, 3, 4, 8, 10 ], 
  [ 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12 ] ]
gap> IsBound(l[4]);
true
gap> l[4];
[ 1, 3, 8 ]
gap> S := OrderEndomorphisms(2);;
gap> CongruencesOfSemigroup(S);
[ <semigroup congruence over <regular transformation monoid of size 3, 
     degree 2 with 2 generators> with 0 generating pairs>, 
  <universal semigroup congruence over <regular transformation monoid 
     of size 3, degree 2 with 2 generators>>, 
  <semigroup congruence over <regular transformation monoid of size 3, 
     degree 2 with 2 generators> with 1 generating pairs> ]
gap> l := LatticeOfCongruences(S);
[ [  ], [ 1, 3 ], [ 1 ] ]
gap> DotString(l);
"//dot\ngraph graphname {\n     node [shape=circle]\n2 -- 3\n3 -- 1\n }"
gap> S := Semigroup([Transformation([1, 4, 3, 1, 4, 2]),
>                    Transformation([1, 6, 6, 3, 6, 6])]);;
gap> l := LatticeOfCongruences(S);
[ [  ], [ 1 ], [ 1, 2, 5 ], [ 1, 2, 3, 5 ], [ 1, 2 ] ]
gap> DotString(l, rec(info := true)) = Concatenation("//dot\ngraph graphname",
> " {\n     node [shape=circle]\nR2 -- T\nR3 -- 5\nU -- R3\n5 -- R2\n }");
true
gap> S := Semigroup([Transformation([1, 1, 2, 1]),
>                    Transformation([3, 3, 1, 2])]);;
gap> l := LatticeOfCongruences(S);;
gap> DotString(l) = Concatenation(
> "//dot\ngraph graphname {\n     node [shape=point]\n2 -- 3\n2 -- 10\n3 ",
> "-- 13\n4 -- 22\n5 -- 1\n6 -- 4\n6 -- 23\n7 -- 5\n8 -- 1\n9 -- 4\n9 -- ",
> "19\n10 -- 13\n10 -- 25\n11 -- 4\n12 -- 2\n12 -- 14\n12 -- 28\n13 -- 5",
> "\n14 -- 3\n14 -- 29\n15 -- 29\n16 -- 15\n16 -- 41\n17 -- 1\n18 -- 6\n18",
> " -- 9\n18 -- 11\n18 -- 20\n19 -- 12\n19 -- 22\n19 -- 31\n20 -- 19\n20 ",
> "-- 23\n20 -- 32\n20 -- 35\n21 -- 2\n21 -- 24\n21 -- 34\n22 -- 14\n22 -",
> "- 15\n23 -- 16\n23 -- 22\n23 -- 37\n24 -- 3\n24 -- 36\n25 -- 5\n25 -- ",
> "8\n26 -- 5\n26 -- 17\n27 -- 7\n27 -- 25\n28 -- 10\n28 -- 27\n28 -- 29",
> "\n29 -- 7\n29 -- 13\n30 -- 7\n30 -- 26\n31 -- 15\n31 -- 28\n32 -- 16\n3",
> "2 -- 31\n32 -- 40\n33 -- 8\n33 -- 17\n34 -- 10\n34 -- 36\n34 -- 38\n35",
> " -- 12\n35 -- 21\n35 -- 37\n35 -- 40\n36 -- 13\n36 -- 26\n37 -- 14\n37",
> " -- 24\n37 -- 41\n38 -- 25\n38 -- 26\n38 -- 33\n39 -- 27\n39 -- 30\n39",
> " -- 38\n40 -- 28\n40 -- 34\n40 -- 39\n40 -- 41\n41 -- 29\n41 -- 30\n41",
> " -- 36\n }");
true

#LatticeOfLeft/RightCongruences
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> LatticeOfLeftCongruences(S);
[ [  ], [ 1, 5, 9 ], [ 1 ], [ 1, 3, 5, 11, 12, 13, 15, 17 ], [ 1 ], 
  [ 1, 2, 3, 5, 9, 12, 15, 16 ], [ 1, 3 ], 
  [ 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21 ], 
  [ 1, 5 ], [ 1, 3, 5, 12, 15 ], [ 1, 12, 13 ], [ 1 ], [ 1, 12 ], 
  [ 1, 2, 3, 5, 6, 9, 12, 13, 15, 16, 17, 21 ], [ 1, 3, 5, 12 ], 
  [ 1, 3, 5, 9, 12, 15 ], [ 1, 3, 5, 12, 13, 15 ], 
  [ 1, 3, 4, 5, 9, 11, 12, 13, 15, 16, 17, 21 ], [ 1, 3, 5, 7, 12, 15 ], 
  [ 1, 3, 5, 7, 9, 10, 12, 13, 15, 16, 17, 19, 21 ], 
  [ 1, 3, 5, 9, 12, 13, 15, 16, 17 ] ]
gap> Size(LeftCongruencesOfSemigroup(S));
21
gap> LatticeOfRightCongruences(S);
[ [  ], [ 1 ], [ 1 ], [ 1 ], [ 1, 2, 4, 8, 12, 22 ], [ 1, 3, 4, 7, 14, 23 ], 
  [ 1 ], [ 1 ], [ 1 ], [ 1, 7, 8, 9 ], [ 1 ], [ 1 ], [ 1, 3, 8, 11 ], [ 1 ], 
  [ 1, 2, 7, 11 ], [ 1, 2, 3, 9 ], 
  [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 19, 20, 21, 
      22, 23, 24, 25, 26, 27, 28, 29, 30, 31 ], [ 1, 2, 14 ], [ 1, 3, 12 ], 
  [ 1, 4, 9 ], [ 1, 4, 11 ], [ 1, 4, 12 ], [ 1, 4, 14 ], 
  [ 1, 2, 4, 5, 8, 12, 14, 18, 22, 23, 27, 29, 31 ], 
  [ 1, 3, 4, 6, 7, 12, 14, 19, 22, 23, 26, 29, 31 ], [ 1, 7, 12 ], 
  [ 1, 8, 14 ], [ 1, 9, 11, 12, 14, 29 ], [ 1, 12, 14 ], 
  [ 1, 4, 9, 11, 12, 14, 20, 21, 22, 23, 28, 29, 31 ], 
  [ 1, 4, 12, 14, 22, 23, 29 ] ]
gap> Size(RightCongruencesOfSemigroup(S));
31
gap> LatticeOfCongruences(S);
[ [  ], [ 1, 3, 4 ], [ 1 ], [ 1, 3 ] ]
gap> Size(CongruencesOfSemigroup(S));
4

#T# MinimalCongruencesOfSemigroup
gap> S := Semigroup([Transformation([1,3,2]), Transformation([3,1,3])]);;
gap> min := MinimalCongruencesOfSemigroup(S);
[ <semigroup congruence over <transformation semigroup of degree 3 with 2 
     generators> with 1 generating pairs> ]
gap> congs := CongruencesOfSemigroup(S);
[ <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 0 generating pairs>, 
  <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 1 generating pairs>, 
  <universal semigroup congruence over <transformation semigroup of size 13, 
     degree 3 with 2 generators>>, 
  <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 1 generating pairs>, 
  <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 1 generating pairs>, 
  <semigroup congruence over <transformation semigroup of size 13, degree 3 
     with 2 generators> with 1 generating pairs> ]
gap> l := LatticeOfCongruences(S);
[ [  ], [ 1, 4, 5 ], [ 1, 2, 4, 5, 6 ], [ 1 ], [ 1, 4 ], [ 1, 2, 4, 5 ] ]
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
[ 5 ]

#T# SEMIGROUPS.LatticeOfXCongruences transrep
gap> S := Semigroup( [ Transformation( [ 4, 2, 3, 3 ] ),
>   Transformation( [ 4, 4, 4, 4 ] ) ] );;
gap> l := SEMIGROUPS.LatticeOfXCongruences(S, "Right", rec(transrep := true));
[ [  ] ]
gap> l := SEMIGROUPS.LatticeOfXCongruences(S, "Right", rec(transrep := true));
[ [  ] ]
gap> LatticeOfCongruences(S);
[ [  ], [ 1 ], [ 1 ], [ 1, 2, 6 ], [ 1, 2, 3 ], [ 1 ], 
  [ 1, 2, 3, 4, 5, 6, 8 ], [ 1, 3, 6 ] ]
gap> LatticeOfRightCongruences(S);
[ [  ], [ 1 ], [ 1 ], [ 1, 2, 6 ], [ 1, 2, 3 ], [ 1 ], [ 1, 2 ], 
  [ 1, 2, 3, 4, 5, 6, 7, 9 ], [ 1, 3, 6 ] ]
gap> S := Semigroup([Transformation([1,3,1]), Transformation([2,2,2])]);;
gap> l := SEMIGROUPS.LatticeOfXCongruences(S, "Right", rec(transrep := true));
[ [  ], [ 1 ], [ 1 ] ]
gap> S := Semigroup([Transformation([2,3,2]), Transformation([3,1,3])]);;
gap> l := SEMIGROUPS.LatticeOfXCongruences(S, "Right", rec(transrep := true));
[ [  ], [ 1 ], [ 1 ], [ 1, 3, 6, 13 ], [ 1 ], [ 1 ], [ 1, 2, 9, 12 ], 
  [ 1, 5 ], [ 1 ], [ 1, 2, 3, 5 ], [ 1, 2, 3, 5, 8, 10 ], [ 1, 2, 9 ], 
  [ 1, 3, 6 ], [ 1, 5, 6, 9 ], [ 1, 5, 6, 8, 9, 14 ] ]
gap> l := SEMIGROUPS.LatticeOfXCongruences(S, "Right", rec(transrep := true,
>                                                          1gen := true));
[ [  ], [ 1 ], [ 1 ], [ 1, 3, 6 ], [ 1 ], [ 1 ], [ 1, 2, 9 ], [ 1, 5 ], [ 1 ] 
 ]

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(congs);
gap> Unbind(l);
gap> Unbind(min);
gap> Unbind(minl);
gap> Unbind(minr);

#E#
gap> STOP_TEST("Semigroups package: standard/conglatt.tst");
