#############################################################################
##
#W  standard/congruences/conglatt.tst
#Y  Copyright (C) 2014-2022                                 Wilf A. Wilson
##                                                          Michael Young
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local D, S, coll, congs, info, l, latt, min, minl, minr, numbers, pair1
#@local pair2, pair3, poset, restriction, x
gap> START_TEST("Semigroups package: standard/congruences/conglatt.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# Robustness against infinite semigroups
gap> S := FreeSemigroup(2);;
gap> congs := CongruencesOfSemigroup(S);
Error, the argument (a semigroup) must be finite and have CanUseFroidurePin
gap> poset := PosetOfPrincipalLeftCongruences(S);
Error, the argument (a semigroup) must be finite and have CanUseFroidurePin
gap> poset := PosetOfPrincipalRightCongruences(S);
Error, the argument (a semigroup) must be finite and have CanUseFroidurePin

# LatticeOfCongruences
gap> S := PartitionMonoid(2);;
gap> l := LatticeOfCongruences(S);
<lattice of 13 two-sided congruences over <regular bipartition *-monoid 
 of size 15, degree 2 with 3 generators>>
gap> IsIsomorphicDigraph(l,
> DigraphFromDigraph6String("&L~~gpU{yksMEB@?_?XozWKcAI@B?__"));
true
gap> IsLatticeDigraph(l);
true
gap> S := OrderEndomorphisms(2);;
gap> CongruencesOfSemigroup(S);
[ <2-sided semigroup congruence over <regular transformation monoid 
     of size 3, degree 2 with 2 generators> with 0 generating pairs>, 
  <universal semigroup congruence over <regular transformation monoid 
     of size 3, degree 2 with 2 generators>>, 
  <2-sided semigroup congruence over <regular transformation monoid 
     of size 3, degree 2 with 2 generators> with 1 generating pairs> ]
gap> l := LatticeOfCongruences(S);
<lattice of 3 two-sided congruences over <regular transformation monoid 
 of size 3, degree 2 with 2 generators>>
gap> IsIsomorphicDigraph(l, DigraphFromDigraph6String("&ByW"));
true
gap> Print(l, "\n");
PosetOfCongruences( 
[ 
  SemigroupCongruence( Monoid( 
    [ Transformation( [ 1, 1 ] ), Transformation( [ 2, 2 ] ) ] ), [  ] ), 
  SemigroupCongruence( Monoid( 
    [ Transformation( [ 1, 1 ] ), Transformation( [ 2, 2 ] ) ] ), 
    [ [ Transformation( [ 1, 1 ] ), IdentityTransformation ] ] ), 
  SemigroupCongruence( Monoid( 
    [ Transformation( [ 1, 1 ] ), Transformation( [ 2, 2 ] ) ] ), 
    [ [ Transformation( [ 1, 1 ] ), Transformation( [ 2, 2 ] ) ] ] ) ] )
gap> CongruencesOfPoset(l) = CongruencesOfSemigroup(S);
true
gap> DotString(l);
"//dot\ngraph graphname {\n     node [shape=circle]\n2 -- 3\n3 -- 1\n }"
gap> S := Semigroup([Transformation([1, 4, 3, 1, 4, 2]),
>                    Transformation([1, 6, 6, 3, 6, 6])]);;
gap> l := LatticeOfCongruences(S);;
gap> IsIsomorphicDigraph(l, DigraphFromDigraph6String("&D}{ho_"));
true
gap> DotString(l, rec(info := true));;
gap> S := Semigroup([Transformation([1, 1, 2, 1]),
>                    Transformation([3, 3, 1, 2])]);;
gap> l := LatticeOfCongruences(S);;
gap> IsIsomorphicDigraph(l, DigraphFromDigraph6String(
> Concatenation(
> "&h~~~~~~}a?Jo?A@kK^{?EAk?nF{J_ooG????_?O???P_DwX`CvnN}rrLn}~~n~wW{Mr??",
> "??_G?????_O????A_qG???D`uYn{K]~XimwG?m??G?op|_?W?_?w??????_?????@_?????",
> "B_?C?WWMo????_K_???@?\\_?oP_Dwz`[A?B_o????B?_????F@gJ_O@[EgR???[C_a@?Do",
> "W`C?D@uQbKK]^XaeW?SFXI~o?gKa?z_??W??_?A?q?@_??@oQAw??B?_Co??E@?G_?gKa?X",
> "_")));
true

# the string depends on the representation of the semigroup
gap> DotString(l);;
gap> DotString(l, rec(numbers := true));;
gap> IsCongruencePoset(l);
true
gap> IsDigraph(l);
true
gap> IsPartialOrderDigraph(l);
true

# Left/RightCongruences (as a list)
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> Size(LeftCongruencesOfSemigroup(S));
21
gap> Size(RightCongruencesOfSemigroup(S));
31

# LatticeOfLeft/RightCongruences
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> l := LatticeOfLeftCongruences(S);
<lattice of 21 left congruences over <transformation semigroup of size 11, 
 degree 3 with 2 generators>>
gap> IsIsomorphicDigraph(l, DigraphFromDigraph6String(
> "&T~~~ycA?Nc^wcA?A_@?K?E?_??U?GSgXgC_CAqTitj~Eu~wCA?C_XgSAlEc^wC?G?_?_C?E?_Pg"));
true
gap> l := LatticeOfRightCongruences(S);
<lattice of 31 right congruences over <transformation semigroup of size 11, 
 degree 3 with 2 generators>>
gap> IsIsomorphicDigraph(l, DigraphFromDigraph6String(
> Concatenation(
> "&^~~~~~g_F_OAGHgC?`?r?GM?H^EA?C@??_A?O?kP?S?A?_??D`OA_?IgoC@",
> "AETv??a???_HSzo?A_????o????G????E@???A_O??@G?_??a?O??O_GA?GJEA?CBb??A?",
> "_??@?G???_I???OA_??GbM??C?A??A?p_")));
true
gap> IsIsomorphicDigraph(DigraphFromDigraph6String("&C|FS"),
> LatticeOfCongruences(S));
true
gap> Size(CongruencesOfSemigroup(S));
4
gap> IsPartialOrderDigraph(l);
true
gap> IsLatticeDigraph(l);
true

# LatticeOfLeft/RightCongruences with restriction
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> restriction := Subsemigroup(S, [Transformation([1, 1, 1]),
>                                    Transformation([2, 2, 2]),
>                                    Transformation([3, 3, 3])]);;
gap> PosetOfPrincipalLeftCongruences(S, Combinations(AsList(restriction), 2));
<poset of 3 left congruences over <transformation semigroup of size 11, 
 degree 3 with 2 generators>>
gap> latt := LatticeOfLeftCongruences(S, Combinations(AsList(restriction), 2));
<lattice of 5 left congruences over <transformation semigroup of size 11, 
 degree 3 with 2 generators>>
gap> IsIsomorphicDigraph(latt, DigraphFromDigraph6String("&D}cgo_"));
true
gap> restriction := [Transformation([3, 2, 3]),
>                    Transformation([3, 1, 3]),
>                    Transformation([2, 2, 2])];;
gap> latt := LatticeOfRightCongruences(S, Combinations(restriction, 2));
<lattice of 4 right congruences over <transformation semigroup of size 11, 
 degree 3 with 2 generators>>
gap> IsIsomorphicDigraph(latt, DigraphFromDigraph6String("&C|ES"));
true
gap> congs := CongruencesOfPoset(latt);;
gap> Length(congs);
4
gap> IsDuplicateFreeList(congs);
true
gap> restriction := [Transformation([3, 1, 3]), Transformation([3, 2, 3])];;
gap> latt := LatticeOfCongruences(S, Combinations(restriction, 2));
<lattice of 2 two-sided congruences over <transformation semigroup 
 of size 11, degree 3 with 2 generators>>
gap> InNeighbours(latt);
[ [ 1 ], [ 1, 2 ] ]
gap> restriction := [Transformation([3, 3, 3])];;
gap> latt := LatticeOfCongruences(S, Combinations(restriction, 2));
<lattice of 1 two-sided congruence over <transformation semigroup of size 11, 
 degree 3 with 2 generators>>
gap> InNeighbours(latt);
[ [ 1 ] ]

# LatticeOf(Left/Right)Congruences with invalid restriction
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> restriction := [Transformation([1, 1, 1]), Transformation([2, 2, 2, 2])];;
gap> LatticeOfCongruences(S, restriction);
Error, the 2nd argument (a list or collection) must be empty or a mult. elt. c\
oll. coll.
gap> LatticeOfLeftCongruences(S, restriction);
Error, the 2nd argument (a list or collection) must be empty or a mult. elt. c\
oll. coll.
gap> LatticeOfRightCongruences(S, restriction);
Error, the 2nd argument (a list or collection) must be empty or a mult. elt. c\
oll. coll.

# Left/RightCongruences (as a list)
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> Size(LeftCongruencesOfSemigroup(S));
21
gap> Size(RightCongruencesOfSemigroup(S));
31

# PosetOfPrincipalLeft/RightCongruences
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> poset := PosetOfPrincipalLeftCongruences(S);
<poset of 12 left congruences over <transformation semigroup of size 11, 
 degree 3 with 2 generators>>
gap> IsIsomorphicDigraph(poset,
>      DigraphFromDigraph6String("&Kh?^GH?D?B?@?D_hO@GDclYLl"));
true
gap> poset := PosetOfPrincipalRightCongruences(S);
<poset of 15 right congruences over <transformation semigroup of size 11, 
 degree 3 with 2 generators>>
gap> IsIsomorphicDigraph(poset,
> DigraphFromDiSparse6String(".NkR@RyJofoPdM?qPEUsbFpfSRHVqACl_CRn"));
true
gap> poset := PosetOfPrincipalCongruences(S);
<lattice of 3 two-sided congruences over <transformation semigroup 
 of size 11, degree 3 with 2 generators>>
gap> IsIsomorphicDigraph(poset, DigraphByInNeighbours(
> [[1, 2, 3], [2], [2, 3]]));
true
gap> Print(poset, "\n");
PosetOfCongruences( 
[ SemigroupCongruence( Semigroup( [ Transformation( [ 1, 3, 1 ] ), 
      Transformation( [ 2, 3, 3 ] ) ] ), 
    [ [ Transformation( [ 1, 1, 1 ] ), Transformation( [ 1, 3, 1 ] ) ] ] ), 
  SemigroupCongruence( Semigroup( [ Transformation( [ 1, 3, 1 ] ), 
      Transformation( [ 2, 3, 3 ] ) ] ), 
    [ [ Transformation( [ 1, 3, 1 ] ), Transformation( [ 3, 1, 3 ] ) ] ] ), 
  SemigroupCongruence( Semigroup( [ Transformation( [ 1, 3, 1 ] ), 
      Transformation( [ 2, 3, 3 ] ) ] ), 
    [ [ Transformation( [ 1, 1, 1 ] ), Transformation( [ 2, 2, 2 ] ) ] ] ) ] )
gap> Size(PrincipalCongruencesOfSemigroup(S));
3

# PosetOfPrincipalLeft/RightCongruences with restriction
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> restriction := Subsemigroup(S, [Transformation([1, 1, 1]),
>                                    Transformation([2, 2, 2]),
>                                    Transformation([3, 3, 3])]);;
gap> latt := PosetOfPrincipalLeftCongruences(S,
> Combinations(AsList(restriction), 2));
<poset of 3 left congruences over <transformation semigroup of size 11, 
 degree 3 with 2 generators>>
gap> InNeighbours(latt);
[ [ 1 ], [ 2 ], [ 3 ] ]
gap> restriction := [Transformation([3, 2, 3]),
>                    Transformation([3, 1, 3]),
>                    Transformation([2, 2, 2])];;
gap> latt := PosetOfPrincipalRightCongruences(S, Combinations(restriction, 2));
<poset of 3 right congruences over <transformation semigroup of size 11, 
 degree 3 with 2 generators>>
gap> InNeighbours(latt);
[ [ 1, 2, 3 ], [ 2 ], [ 3 ] ]
gap> CongruencesOfPoset(latt);
[ <right semigroup congruence over <transformation semigroup of size 11, 
     degree 3 with 2 generators> with 1 generating pairs>, 
  <right semigroup congruence over <transformation semigroup of size 11, 
     degree 3 with 2 generators> with 1 generating pairs>, 
  <right semigroup congruence over <transformation semigroup of size 11, 
     degree 3 with 2 generators> with 1 generating pairs> ]
gap> restriction := [Transformation([3, 1, 3]), Transformation([3, 2, 3])];;
gap> latt := PosetOfPrincipalCongruences(S, Combinations(restriction, 2));;
gap> InNeighbours(latt);
[ [ 1 ] ]
gap> restriction := [Transformation([3, 3, 3])];;
gap> latt := PosetOfPrincipalCongruences(S, Combinations(restriction, 2));
<empty congruence poset>
gap> InNeighbours(latt);
[  ]
gap> IsPartialOrderDigraph(latt);
true

# PosetOfPrincipal(Left/Right)Congruences with invalid restriction
gap> S := Semigroup([Transformation([1, 3, 1]), Transformation([2, 3, 3])]);;
gap> restriction := [Transformation([1, 1, 1]), Transformation([2, 2, 2, 2])];;
gap> PosetOfPrincipalCongruences(S, Combinations(restriction, 2));
Error, the 2nd argument (a list) must consist of pairs of the 1st argument (a \
semigroup)
gap> PosetOfPrincipalLeftCongruences(S, restriction);
Error, the 2nd argument (a list or collection) must be empty or a mult. elt. c\
oll. coll.
gap> PosetOfPrincipalRightCongruences(S, restriction);
Error, the 2nd argument (a list or collection) must be empty or a mult. elt. c\
oll. coll.

# PrincipalCongruencesOfSemigroup
gap> S := Semigroup(Transformation([1, 3, 2]),
>                   Transformation([3, 1, 3]));;
gap> Length(PrincipalCongruencesOfSemigroup(S));
5

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
gap> min := MinimalCongruencesOfSemigroup(S);;
gap> Length(min);
1
gap> Length(CongruencesOfSemigroup(S));
6
gap> l := LatticeOfCongruences(S);;
gap> IsIsomorphicDigraph(l,
> DigraphByInNeighbours(
> [[1], [1, 2, 5, 6], [1, 2, 3, 4, 5, 6], [1, 2, 4, 5, 6],
> [1, 5, 6], [1, 6]]));
true
gap> minl := MinimalLeftCongruencesOfSemigroup(S);;
gap> Size(minl);
3
gap> minr := MinimalRightCongruencesOfSemigroup(S);;
gap> Size(minr);
9
gap> PositionsProperty(minl, c -> IsSubrelation(min[1], c));
[ 1, 2, 3 ]
gap> PositionsProperty(minr, c -> IsSubrelation(min[1], c)) in [[7], [8]];
true

# Biggish example which forces garbage collection
gap> S := Semigroup([Transformation([4, 2, 4, 4, 1]),
>                    Transformation([4, 4, 1, 2, 2]),
>                    Transformation([3, 3, 1, 2, 5])]);;
gap> Length(MinimalCongruencesOfSemigroup(S));
3

# JoinSemilatticeOfCongruences
gap> S := SymmetricInverseMonoid(2);;
gap> pair1 := [PartialPerm([1], [1]), PartialPerm([2], [1])];;
gap> pair2 := [PartialPerm([1], [1]), PartialPerm([1, 2], [1, 2])];;
gap> pair3 := [PartialPerm([1, 2], [1, 2]), PartialPerm([1, 2], [2, 1])];;
gap> coll := [RightSemigroupCongruence(S, pair1),
>             RightSemigroupCongruence(S, pair2),
>             RightSemigroupCongruence(S, pair3)];;
gap> l := JoinSemilatticeOfCongruences(coll);
<poset of 4 right congruences over <symmetric inverse monoid of degree 2>>
gap> IsIsomorphicDigraph(l, DigraphFromDigraph6String("&ClRC"));
true
gap> JoinSemilatticeOfCongruences(coll);
<poset of 4 right congruences over <symmetric inverse monoid of degree 2>>

# MinimalCongruences
gap> S := SymmetricInverseMonoid(2);;
gap> pair1 := [PartialPerm([1], [1]), PartialPerm([2], [1])];;
gap> pair2 := [PartialPerm([1], [1]), PartialPerm([1, 2], [1, 2])];;
gap> pair3 := [PartialPerm([1, 2], [1, 2]), PartialPerm([1, 2], [2, 1])];;
gap> coll := [RightSemigroupCongruence(S, pair1),
>             RightSemigroupCongruence(S, pair2),
>             RightSemigroupCongruence(S, pair3)];;
gap> MinimalCongruences(PosetOfCongruences(coll)) = coll{[1, 2]};
true
gap> MinimalCongruences(PosetOfCongruences(coll)) = coll{[1, 2]};
true
gap> poset := LatticeOfCongruences(S);
<lattice of 4 two-sided congruences over 
 <symmetric inverse monoid of degree 2>>
gap> IsIsomorphicDigraph(poset, DigraphFromDigraph6String("&C|qK"));
true
gap> Print(poset, "\n");
PosetOfCongruences( 
[ SemigroupCongruence( InverseMonoid( [ PartialPerm( [ 1, 2 ], [ 2, 1 ] ), 
      PartialPerm( [ 1 ], [ 1 ] ) ] ), [  ] ), 
  SemigroupCongruence( InverseMonoid( [ PartialPerm( [ 1, 2 ], [ 2, 1 ] ), 
      PartialPerm( [ 1 ], [ 1 ] ) ] ), 
    [ [ PartialPerm( [  ], [  ] ), PartialPerm( [ 1, 2 ], [ 1, 2 ] ) ] ] ), 
  SemigroupCongruence( InverseMonoid( [ PartialPerm( [ 1, 2 ], [ 2, 1 ] ), 
      PartialPerm( [ 1 ], [ 1 ] ) ] ), 
    [ [ PartialPerm( [ 1, 2 ], [ 1, 2 ] ), PartialPerm( [ 1, 2 ], [ 2, 1 ] ) 
         ] ] ), SemigroupCongruence( InverseMonoid( 
    [ PartialPerm( [ 1, 2 ], [ 2, 1 ] ), PartialPerm( [ 1 ], [ 1 ] ) ] ), 
    [ [ PartialPerm( [  ], [  ] ), PartialPerm( [ 1 ], [ 1 ] ) ] ] ) ] )
gap> MinimalCongruences(poset);
[ <2-sided semigroup congruence over <symmetric inverse monoid of degree 2> wi\
th 0 generating pairs> ]
gap> MinimalCongruences(PosetOfCongruences([]));
[  ]

# PosetOfCongruences
gap> S := OrderEndomorphisms(2);;
gap> pair1 := [Transformation([1, 1]), IdentityTransformation];;
gap> pair2 := [IdentityTransformation, Transformation([2, 2])];;
gap> coll := [RightSemigroupCongruence(S, pair1),
>             RightSemigroupCongruence(S, pair2),
>             RightSemigroupCongruence(S, [])];;
gap> poset := PosetOfCongruences(coll);
<poset of 3 right congruences over <regular transformation monoid of size 3, 
 degree 2 with 2 generators>>
gap> InNeighbours(poset);
[ [ 1, 3 ], [ 2, 3 ], [ 3 ] ]

# Trivial poset
gap> poset := PosetOfCongruences([]);
<empty congruence poset>
gap> CongruencesOfPoset(poset);
[  ]
gap> DigraphNrVertices(poset);
0
gap> JoinSemilatticeOfCongruences(poset);
Error, cannot form the join semilattice of an empty congruence poset without t\
he underlying semigroup being set
gap> MinimalCongruences(poset);
[  ]

# Test Issue 309
gap> S := Semigroup(Transformation([2, 1, 4, 3, 5, 2]),
>                   Transformation([3, 4, 1, 2, 5, 3]),
>                   Transformation([5, 5, 5, 5, 5, 5]));;
gap> l := LatticeOfCongruences(S);;
gap> IsIsomorphicDigraph(l, DigraphByInNeighbours(
> [[1], [1, 2], [1, 3], [1, 4], [1, 2, 3, 4, 5, 6],
> [1, 2, 3, 4, 6]]));
true

# Test for correct ordering of congruences and lattice nodes
gap> S := InverseSemigroup(PartialPerm([1, 3], [2, 4]),
>                          PartialPerm([1], [1]));;
gap> D := DigraphReflexiveTransitiveReduction(LatticeOfRightCongruences(S));
<immutable digraph with 22 vertices, 49 edges>
gap> x := DigraphSinks(D)[1];;
gap> NrEquivalenceClasses(RightCongruencesOfSemigroup(S)[x]);
1

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/congruences/conglatt.tst");
