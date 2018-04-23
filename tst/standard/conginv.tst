#############################################################################
##
#W  standard/conginv.tst
#Y  Copyright (C) 2014-16                                   Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/conginv.tst");
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
<semigroup congruence over <inverse partial perm semigroup of size 206, 
 rank 5 with 4 generators> with 1 generating pairs>
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
gap> classx := CongruenceClassOfElement(cong, x);
<congruence class of [1,2]>
gap> classy := CongruenceClassOfElement(cong, y);;
gap> classz := CongruenceClassOfElement(cong, z);;
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
<semigroup congruence over <inverse partial perm semigroup of size 206, 
 rank 5 with 4 generators> with 1 generating pairs>
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
Error, Semigroups: InverseSemigroupCongruenceByKernelTrace: usage,
the second arg <kernel> must be an inverse subsemigroup of the
first arg <S>,
gap> T := InverseSemigroup([PartialPerm([2, 4], [5, 6])]);;
gap> cong := InverseSemigroupCongruenceByKernelTrace(S, T, utrace);
Error, Semigroups: InverseSemigroupCongruenceByKernelTrace: usage,
the second arg <kernel> must contain all the
idempotents of the first arg <S>,
gap> T := IdempotentGeneratedSubsemigroup(S);;
gap> T := InverseSemigroup(T, PartialPerm([1, 2], [3, 4]));;
gap> cong := InverseSemigroupCongruenceByKernelTrace(S, T, utrace);
Error, Semigroups: InverseSemigroupCongruenceByKernelTrace: usage,
the second arg <kernel> must be self-conjugate,
gap> T := IdempotentGeneratedSubsemigroup(S);;
gap> cong := InverseSemigroupCongruenceByKernelTrace(S, T, utrace);
Error, Semigroups: InverseSemigroupCongruenceByKernelTrace:
not a valid congruence pair (C1),
gap> cong := InverseSemigroupCongruenceByKernelTrace(S, S, ttrace);
Error, Semigroups: InverseSemigroupCongruenceByKernelTrace:
not a valid congruence pair (C2),

# ImagesElm: Bad Input
gap> S := InverseSemigroup([PartialPerm([1, 2], [5, 2]),
>                           PartialPerm([1, 3], [4, 3])]);;
gap> T := IdempotentGeneratedSubsemigroup(S);;
gap> ttrace := List(Idempotents(S), e -> [e]);;
gap> cong := InverseSemigroupCongruenceByKernelTrace(S, T, ttrace);;
gap> ImagesElm(cong, (1, 5, 2));
Error, Semigroups: ImagesElm: usage,
the first arg <cong> is not defined over the semigroup of the second
argument <elm>,

# \in, EquivalenceClassOfElement: Bad Input
gap> S := InverseSemigroup([PartialPerm([1, 2, 3], [2, 5, 3]),
>                           PartialPerm([1, 2, 4], [3, 1, 5]),
>                           PartialPerm([1, 2, 5], [5, 1, 3]),
>                           PartialPerm([1, 2, 3], [3, 4, 2])]);;
gap> cong := SemigroupCongruence(S,
>  [PartialPerm([4], [4]), PartialPerm([2], [1])]);;
gap> [2] in cong;
Error, Semigroups: \in (for a relation): usage,
the first arg <pair> must be a list of length 2,
gap> [PartialPerm([4], [4]), 42] in cong;
Error, Semigroups: \in (for a relation): usage,
elements of the first arg <pair> must be
in the range of the second arg <cong>,
gap> EquivalenceClassOfElement(cong, (2, 5, 4));
Error, Semigroups: EquivalenceClassOfElement: usage,
the second arg <elm> must be in the
semigroup of the first arg <cong>,

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
gap> x := CongruenceClassOfElement(cong1, PartialPerm([1, 2, 3], [2, 5, 3]));;
gap> y := CongruenceClassOfElement(cong2, PartialPerm([1, 2, 4], [3, 1, 5]));;
gap> x * y;
Error, Semigroups: \*: usage,
the arguments must be classes of the same congruence,

# Non-inverse semigroups
gap> S := Semigroup([Transformation([3, 4, 3, 2]),
>                    Transformation([4, 4, 4, 2])]);;
gap> cong := SemigroupCongruence(S, [Transformation([2, 4, 2, 2]),
>                                    Transformation([4, 2, 4, 4])]);;
gap> KernelOfSemigroupCongruence(cong);
Error, Semigroups: KernelOfSemigroupCongruence: usage,
<cong> must be over an inverse semigroup with inverse op,
gap> TraceOfSemigroupCongruence(cong);
Error, Semigroups: TraceOfSemigroupCongruence: usage,
<cong> must be over an inverse semigroup with inverse op,
gap> AsInverseSemigroupCongruenceByKernelTrace(cong);
Error, Semigroups: AsInverseSemigroupCongruenceByKernelTrace: usage,
<cong> must be over an inverse semigroup with inverse op,

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
gap> cong := MinimumGroupCongruence(S);
<semigroup congruence over <inverse partial perm semigroup of rank 7 with 2 
 generators> with congruence pair (59,1)>
gap> NrEquivalenceClasses(cong);
2
gap> S := InverseSemigroup([PartialPerm([1, 2, 3, 4, 6], [3, 2, 1, 4, 7]),
>                           PartialPerm([1, 2, 3, 7], [3, 1, 2, 5])]);;
gap> cong := MinimumGroupCongruence(S);
<semigroup congruence over <inverse partial perm semigroup of rank 7 with 2 
 generators> with congruence pair (7,1)>
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
Error, Semigroups: MeetSemigroupCongruences: usage,
congruences must be defined over the same semigroup,
gap> JoinSemigroupCongruences(cong1, cong2);
Error, Semigroups: JoinSemigroupCongruences: usage,
congruences must be defined over the same semigroup,
gap> IsSubrelation(cong1, cong2);
Error, Semigroups: IsSubrelation: usage,
congruences must be defined over the same semigroup,

# EquivalenceRelationCanonicalLookup
gap> S := InverseSemigroup([PartialPerm([1, 2], [1, 2]),
>                           PartialPerm([1, 2], [2, 3])]);;
gap> pairs := [PartialPerm([], []), PartialPerm([1], [1])];;
gap> cong := SemigroupCongruence(S, pairs);
<semigroup congruence over <inverse partial perm semigroup of size 14, rank 3 
 with 2 generators> with congruence pair (12,3)>
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 2, 3, 4, 4, 4, 5, 4, 4, 4, 4, 4, 4, 4 ]

# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(T);
gap> Unbind(ccong);
gap> Unbind(classx);
gap> Unbind(classy);
gap> Unbind(classz);
gap> Unbind(cong);
gap> Unbind(cong1);
gap> Unbind(cong2);
gap> Unbind(g);
gap> Unbind(min);
gap> Unbind(pair);
gap> Unbind(pair1);
gap> Unbind(pair2);
gap> Unbind(pairs);
gap> Unbind(q);
gap> Unbind(ttrace);
gap> Unbind(utrace);
gap> Unbind(x);
gap> Unbind(y);
gap> Unbind(z);

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/conginv.tst");
