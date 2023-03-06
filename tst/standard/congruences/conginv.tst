#############################################################################
##
#W  standard/congruences/conginv.tst
#Y  Copyright (C) 2014-2022                                 Michael Young
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local S, T, ccong, classx, classy, classz, cong, cong1, cong2
#@local cong_by_ker_trace_threshold, g, min, pair, pair1, pair2, pairs, q
#@local ttrace, utrace, x, y, z
gap> START_TEST("Semigroups package: standard/congruences/conginv.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# Always use kernel-trace methods if possible in these tests
gap> SEMIGROUPS.DefaultOptionsRec.cong_by_ker_trace_threshold := 0;;

# InverseCongTest1: Create an inverse semigroup congruence
gap> S := InverseSemigroup([PartialPerm([1, 2, 3], [2, 5, 3]),
>  PartialPerm([1, 2, 4], [3, 1, 5]),
>  PartialPerm([1, 2, 5], [5, 1, 3]),
>  PartialPerm([1, 2, 3], [3, 4, 2])]);;
gap> cong := SemigroupCongruence(S,
>  [PartialPerm([4], [4]), PartialPerm([2], [1])]);
<semigroup congruence over <inverse partial perm semigroup of size 206, 
 rank 5 with 4 generators> with congruence pair (41,16)>
gap> ccong := SemigroupCongruenceByGeneratingPairs(S,
>  [[PartialPerm([4], [4]), PartialPerm([2], [1])]]);
<2-sided semigroup congruence over <inverse partial perm semigroup 
 of size 206, rank 5 with 4 generators> with 1 generating pairs>
gap> KernelOfSemigroupCongruence(ccong) = cong!.kernel;
true
gap> ccong := SemigroupCongruenceByGeneratingPairs(S,
>  [[PartialPerm([4], [4]), PartialPerm([2], [1])]]);;
gap> TraceOfSemigroupCongruence(ccong) = cong!.traceBlocks;
true

# Try some methods
gap> x := PartialPerm([1], [2]);;
gap> y := PartialPerm([2, 5], [2, 1]);;
gap> z := PartialPerm([5], [5]);;
gap> [x, y] in cong;
false
gap> [x, z] in cong;
true
gap> [y, z] in cong;
false

# Get the Kernel and Trace out
gap> Size(KernelOfSemigroupCongruence(cong));
41
gap> AsSortedList(List(TraceOfSemigroupCongruence(cong), AsSortedList));
[ [ <empty partial perm>, <identity partial perm on [ 1 ]>, 
      <identity partial perm on [ 2 ]>, <identity partial perm on [ 3 ]>, 
      <identity partial perm on [ 4 ]>, <identity partial perm on [ 5 ]> ], 
  [ <identity partial perm on [ 1, 2 ]> ], 
  [ <identity partial perm on [ 2, 3 ]> ], 
  [ <identity partial perm on [ 1, 3 ]> ], 
  [ <identity partial perm on [ 1, 2, 3 ]> ], 
  [ <identity partial perm on [ 3, 4 ]> ], 
  [ <identity partial perm on [ 2, 4 ]> ], 
  [ <identity partial perm on [ 2, 3, 4 ]> ], 
  [ <identity partial perm on [ 1, 4 ]> ], 
  [ <identity partial perm on [ 1, 2, 4 ]> ], 
  [ <identity partial perm on [ 3, 5 ]> ], 
  [ <identity partial perm on [ 2, 5 ]> ], 
  [ <identity partial perm on [ 2, 3, 5 ]> ], 
  [ <identity partial perm on [ 1, 5 ]> ], 
  [ <identity partial perm on [ 1, 3, 5 ]> ], 
  [ <identity partial perm on [ 1, 2, 5 ]> ] ]

# Congruence classes
gap> classx := EquivalenceClassOfElement(cong, x);
<2-sided congruence class of [1,2]>
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
26
gap> AsSSortedList(classy);
[ [5,1](2) ]

# Quotients
gap> q := S / cong;;

# Convert to and from semigroup congruence by generating pairs
gap> pairs := GeneratingPairsOfSemigroupCongruence(cong);;
gap> ccong := SemigroupCongruence(S, pairs);;
gap> ccong = cong;
true
gap> ccong := AsSemigroupCongruenceByGeneratingPairs(cong);
<2-sided semigroup congruence over <inverse partial perm semigroup 
 of size 206, rank 5 with 4 generators> with 1 generating pairs>
gap> [x, y] in ccong;
false
gap> [x, z] in ccong;
true
gap> [y, z] in ccong;
false

# InverseCongTest2: Universal congruence
gap> S := InverseSemigroup(PartialPerm([1], [2]), PartialPerm([2], [1]));
<inverse partial perm semigroup of rank 2 with 2 generators>
gap> Size(S);
5
gap> SemigroupCongruence(S, [S.1, S.1 * S.2]);
<universal semigroup congruence over <0-simple inverse partial perm semigroup 
 of size 5, rank 2 with 2 generators>>

# InverseSemigroupCongruenceByKernelTrace: Bad Input
gap> S := InverseSemigroup([PartialPerm([2, 4], [5, 6]),
>                           PartialPerm([1, 2], [3, 4])]);;
gap> T := InverseSemigroup([PartialPerm([2, 4], [5, 7]),
>                           PartialPerm([1, 2], [8, 6])]);;
gap> utrace := [Idempotents(S)];;
gap> ttrace := List(Idempotents(S), e -> [e]);;
gap> cong := InverseSemigroupCongruenceByKernelTrace(S, T, utrace);
Error, the 2nd argument is not an inverse subsemigroup of the 1st argument (an\
 inverse semigroup)
gap> T := InverseSemigroup([PartialPerm([2, 4], [5, 6])]);;
gap> cong := InverseSemigroupCongruenceByKernelTrace(S, T, utrace);
Error, the 2nd argument (an inverse semigroup) does not contain all of the ide\
mpotents of the 1st argument (an inverse semigroup)
gap> T := IdempotentGeneratedSubsemigroup(S);;
gap> T := InverseSemigroup(T, PartialPerm([1, 2], [3, 4]));;
gap> cong := InverseSemigroupCongruenceByKernelTrace(S, T, utrace);
Error, the 2nd argument (an inverse semigroup) must be self-conjugate
gap> T := IdempotentGeneratedSubsemigroup(S);;
gap> cong := InverseSemigroupCongruenceByKernelTrace(S, T, utrace);
Error, not a valid congruence pair (C1)
gap> cong := InverseSemigroupCongruenceByKernelTrace(S, S, ttrace);
Error, not a valid congruence pair (C2)
gap> InverseSemigroupCongruenceByKernelTrace(LeftZeroSemigroup(2), fail, fail);
Error, the 1st argument is not an inverse semigroup with inverse op

# ImagesElm: Bad Input
gap> S := InverseSemigroup([PartialPerm([1, 2], [5, 2]),
>                           PartialPerm([1, 3], [4, 3])]);;
gap> T := IdempotentGeneratedSubsemigroup(S);;
gap> ttrace := List(Idempotents(S), e -> [e]);;
gap> cong := InverseSemigroupCongruenceByKernelTrace(S, T, ttrace);;
gap> ImagesElm(cong, (1, 5, 2));
Error, the 2nd argument (a mult. elt. with inverse) does not belong to the ran\
ge of the 1st argument (a congruence)

# \in, EquivalenceClassOfElement: Bad Input
gap> S := InverseSemigroup([PartialPerm([1, 2, 3], [2, 5, 3]),
>                           PartialPerm([1, 2, 4], [3, 1, 5]),
>                           PartialPerm([1, 2, 5], [5, 1, 3]),
>                           PartialPerm([1, 2, 3], [3, 4, 2])]);;
gap> cong := SemigroupCongruence(S,
>  [PartialPerm([4], [4]), PartialPerm([2], [1])]);;
gap> [2] in cong;
Error, the 1st argument (a list) does not have length 2
gap> [PartialPerm([4], [4]), 42] in cong;
Error, the items in the 1st argument (a list) do not all belong to the range o\
f the 2nd argument (a 2-sided semigroup congruence)
gap> EquivalenceClassOfElement(cong, (2, 5, 4));
Error, the 2nd argument (a mult. elt.) does not belong to the range of the 1st\
 argument (a 2-sided congruence)

# Congruence Class Multiplication: Bad Input
gap> S := InverseSemigroup([PartialPerm([1, 2, 3], [2, 5, 3]),
>                           PartialPerm([1, 2, 4], [3, 1, 5]),
>                           PartialPerm([1, 2, 5], [5, 1, 3])]);;
gap> cong1 := SemigroupCongruence(S,
>             [PartialPerm([], []), PartialPerm([1, 2, 5], [4, 2, 1])],
>             [PartialPerm([], []), PartialPerm([1, 2], [1, 3])]);;
gap> cong2 := SemigroupCongruence(S,
>             [PartialPerm([], []), PartialPerm([1, 2, 5], [4, 2, 1])],
>             [PartialPerm([], []), PartialPerm([1, 2, 3], [2, 5, 3])]);;
gap> x := EquivalenceClassOfElement(cong1, PartialPerm([1, 2, 3], [2, 5, 3]));;
gap> y := EquivalenceClassOfElement(cong2, PartialPerm([1, 2, 4], [3, 1, 5]));;
gap> x * y;
Error, the arguments (cong. classes) are not classes of the same congruence

# Non-inverse semigroups
gap> S := Semigroup([Transformation([3, 4, 3, 2]),
>                    Transformation([4, 4, 4, 2])]);;
gap> cong := SemigroupCongruence(S, [Transformation([2, 4, 2, 2]),
>                                    Transformation([4, 2, 4, 4])]);;
gap> KernelOfSemigroupCongruence(cong);
Error, the range of the argument (a congruence) must be an inverse semigroup w\
ith inverse op
gap> TraceOfSemigroupCongruence(cong);
Error, the range of the argument (a congruence) must be an inverse semigroup w\
ith inverse op
gap> AsInverseSemigroupCongruenceByKernelTrace(cong);
Error, the range of the argument (a congruence) must be an inverse semigroup w\
ith inverse op

# AsInverseSemigroupCongruenceByKernelTrace: More tests
gap> S := InverseSemigroup([PartialPerm([1, 2, 3], [1, 3, 4]),
>                           PartialPerm([1, 2, 3, 4], [2, 4, 1, 5]),
>                           PartialPerm([1, 3, 5], [5, 1, 3])]);;
gap> cong := SemigroupCongruence(S,
>       [PartialPerm([1], [1]), PartialPerm([], [])],
>       [PartialPerm([5], [3]), PartialPerm([], [])]);
<semigroup congruence over <inverse partial perm semigroup of size 258, 
 rank 5 with 3 generators> with congruence pair (44,19)>
gap> cong := SemigroupCongruence(S,
>        [PartialPerm([1, 3, 5], [1, 3, 5]),
>         PartialPerm([1, 2, 4], [3, 1, 2])]);
<semigroup congruence over <inverse partial perm semigroup of size 258, 
 rank 5 with 3 generators> with congruence pair (256,3)>

# MinimumGroupCongruence
gap> S := InverseSemigroup([PartialPerm([1, 2, 5, 6], [5, 2, 1, 4]),
>                           PartialPerm([1, 2, 3, 4, 5, 7],
>                                       [1, 4, 6, 3, 5, 2])]);;
gap> cong := MinimumGroupCongruence(S);;
gap> NrEquivalenceClasses(cong);
2
gap> S := InverseSemigroup([PartialPerm([1, 2, 3, 4, 6], [3, 2, 1, 4, 7]),
>                           PartialPerm([1, 2, 3, 7], [3, 1, 2, 5])]);;
gap> cong := MinimumGroupCongruence(S);;
gap> q := S / cong;;
gap> IsGroupAsSemigroup(q);
true
gap> g := Range(IsomorphismPermGroup(q));;
gap> StructureDescription(g);
"S3"

# JoinSemigroupCongruences
gap> S := InverseMonoid([PartialPerm([1, 2], [3, 1])]);;
gap> pair := [PartialPerm([1, 2], [3, 1]), PartialPerm([], [])];;
gap> cong := SemigroupCongruence(S, pair);;
gap> min := MinimumGroupCongruence(S);;
gap> JoinSemigroupCongruences(cong, min);
<semigroup congruence over <inverse partial perm monoid of size 15, rank 3 
 with 1 generator> with congruence pair (15,1)>
gap> IsSubrelation(last, cong);
true

# MeetSemigroupCongruences
gap> S := InverseSemigroup([PartialPerm([1, 2], [2, 1]),
>                           PartialPerm([1, 3], [3, 1])]);;
gap> pair1 := [PartialPerm([], []), PartialPerm([1, 3], [1, 3])];;
gap> pair2 := [PartialPerm([], []), PartialPerm([1, 2], [1, 2])];;
gap> cong1 := SemigroupCongruence(S, pair1);;
gap> cong2 := SemigroupCongruence(S, pair2);;
gap> MeetSemigroupCongruences(cong1, cong2);
<semigroup congruence over <inverse partial perm semigroup of size 14, rank 3 
 with 2 generators> with congruence pair (12,3)>

# Bad input: different semigroups
gap> S := InverseSemigroup([PartialPerm([1, 2], [2, 1]),
>                           PartialPerm([1, 3], [3, 1])]);;
gap> T := InverseSemigroup([PartialPerm([1, 2], [3, 1])]);;
gap> S = T;
false
gap> pair := [PartialPerm([1, 2], [3, 1]), PartialPerm([], [])];;
gap> cong1 := MinimumGroupCongruence(S);;
gap> cong2 := SemigroupCongruence(T, pair);;
gap> MeetSemigroupCongruences(cong1, cong2);
Error, cannot form the meet of congruences over different semigroups
gap> JoinSemigroupCongruences(cong1, cong2);
Error, cannot form the join of congruences over different semigroups
gap> IsSubrelation(cong1, cong2);
Error, the 1st and 2nd arguments are congruences over different semigroups

# EquivalenceRelationCanonicalLookup
gap> S := InverseSemigroup([PartialPerm([1, 2], [1, 2]),
>                           PartialPerm([1, 2], [2, 3])]);;
gap> pairs := [PartialPerm([], []), PartialPerm([1], [1])];;
gap> cong := SemigroupCongruence(S, pairs);
<semigroup congruence over <inverse partial perm semigroup of size 14, rank 3 
 with 2 generators> with congruence pair (12,3)>
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 2, 3, 4, 4, 4, 5, 4, 4, 4, 4, 4, 4, 4 ]

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/congruences/conginv.tst");
