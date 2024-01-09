#############################################################################
##
#W  testinstall.tst
#Y  Copyright (C) 2011-17                               James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local B, C, D, F, G, H, I, L, LL, M, M0, P, R, R1, R2, S, T, U, V, W, acting
#@local bug, c, cong, contain, ec, es, f, file, g, gens, gns, hom, i, id, inv
#@local iso, iter, latt, log, looking, lookingfor, map, mat, max, n, number, o
#@local pair, pairs, r, regular, rel, rels, s, s1, s2, sgns, slist, small, t
#@local tab, tuples, u, x, y, z
gap> START_TEST("Semigroups package: testinstall.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# TestInstall3
gap> S := Semigroup(Transformation([2, 3, 4, 1, 1, 1]));;
gap> IsMonoidAsSemigroup(S);
true
gap> IsMonoid(S);
false
gap> iso := IsomorphismTransformationMonoid(S);;
gap> IsCommutativeSemigroup(Range(iso));
true
gap> IsMonogenicSemigroup(Range(iso));
true
gap> RespectsMultiplication(iso);
true
gap> ForAll(S, x -> (x ^ iso) ^ InverseGeneralMapping(iso) = x);
true

# TestInstall4
gap> S := Semigroup(Transformation([1, 1, 1]), Transformation([3, 1, 2]));
<transformation semigroup of degree 3 with 2 generators>
gap> IsSimpleSemigroup(S);
false

# TestInstall5
gap> S := SingularTransformationSemigroup(6);
<regular transformation semigroup ideal of degree 6 with 1 generator>
gap> Size(S);
45936

# TestInstall6
gap> S := Semigroup(IdentityTransformation, rec(acting := true));;
gap> LambdaOrb(S);
<open orbit, 1 points with Schreier tree with log>
gap> Enumerate(last);
<closed orbit, 2 points with Schreier tree with log>

# TestInstall7
gap> gens := [Transformation([1, 3, 2, 3]),
>             Transformation([1, 4, 1, 2]),
>             Transformation([3, 4, 2, 2]),
>             Transformation([4, 1, 2, 1])];;
gap> S := Monoid(gens);;
gap> Size(S); NrRClasses(S); NrLClasses(S); NrDClasses(S);
69
17
21
9
gap> NrIdempotents(S); NrRegularDClasses(S); IsRegularSemigroup(S);
22
6
false
gap> x := Transformation([1, 3, 4, 1]);;
gap> x in S;
false
gap> x := Transformation([1, 1, 3, 1]);;
gap> x in S;
true
gap> T := Semigroup(gens{[1 .. 3]});
<transformation semigroup of degree 4 with 3 generators>
gap> ForAll(T, x -> x in S);
true
gap> Size(T);
60

# TestInstall8: Issue 2
gap> S := Semigroup(Transformation([4, 4, 4, 4]));;
gap> AsList(S);
[ Transformation( [ 4, 4, 4, 4 ] ) ]

# TestInstall9: Issue 3
gap> S := Semigroup(Transformation([3, 5, 5, 5, 3]),
>                   Transformation([5, 5, 2, 1, 5]));;
gap> x := Transformation([3, 3, 5, 3, 3]);;
gap> IsRegularSemigroupElement(S, x);
true
gap> x := Transformation([5, 5, 5, 5, 5]);;
gap> IsRegularSemigroupElement(S, x);
true
gap> x := Transformation([3, 5, 5, 5, 3]);;
gap> IsRegularSemigroupElement(S, x);
true
gap> IsRegularSemigroup(S);
false
gap> x := Transformation([5, 5, 2, 1, 5]);;
gap> IsRegularSemigroupElement(S, x);
false

# TestInstall10: Issue 9
gap> gens := [Transformation([1, 2, 3, 9, 5, 11, 7, 8, 9, 10, 11, 12]),
> Transformation([1, 2, 3, 9, 5, 11, 9, 8, 9, 8, 11, 12]),
> Transformation([1, 2, 5, 7, 8, 11, 9, 12, 9, 12, 11, 10]),
> Transformation([1, 2, 8, 9, 5, 11, 9, 8, 9, 10, 11, 12]),
> Transformation([1, 2, 8, 11, 12, 7, 11, 8, 11, 10, 9, 12]),
> Transformation([1, 2, 10, 9, 12, 11, 9, 10, 9, 8, 11, 12]),
> Transformation([1, 2, 12, 4, 10, 6, 7, 12, 9, 12, 11, 10]),
> Transformation([1, 5, 3, 11, 5, 9, 11, 8, 11, 10, 9, 12]),
> Transformation([1, 5, 8, 6, 12, 7, 11, 10, 11, 10, 9, 12]),
> Transformation([1, 5, 8, 11, 12, 7, 11, 8, 11, 10, 9, 12]),
> Transformation([1, 5, 12, 7, 8, 11, 9, 12, 9, 12, 11, 10]),
> Transformation([1, 8, 3, 9, 5, 11, 9, 8, 9, 10, 11, 12]),
> Transformation([1, 8, 5, 7, 8, 11, 9, 12, 9, 12, 11, 10]),
> Transformation([1, 12, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]),
> Transformation([1, 12, 10, 9, 12, 11, 7, 10, 4, 10, 6, 12]),
> Transformation([2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11]),
> Transformation([3, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11]),
> Transformation([5, 6, 7, 8, 11, 12, 10, 9, 10, 9, 12, 11]),
> Transformation([5, 6, 11, 8, 7, 12, 10, 11, 10, 11, 12, 9]),
> Transformation([5, 7, 10, 11, 9, 5, 11, 8, 11, 8, 12, 9]),
> Transformation([5, 10, 7, 5, 10, 11, 12, 9, 12, 9, 11, 8]),
> Transformation([7, 3, 11, 9, 8, 5, 9, 11, 9, 11, 5, 3]),
> Transformation([7, 5, 8, 6, 12, 7, 11, 10, 11, 10, 9, 12]),
> Transformation([7, 12, 11, 10, 5, 9, 10, 11, 8, 11, 9, 12]),
> Transformation([9, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]),
> Transformation([9, 11, 8, 5, 11, 9, 12, 3, 5, 8, 9, 11]),
> Transformation([11, 7, 9, 5, 10, 11, 12, 9, 12, 9, 11, 8])];;
gap> S := Semigroup(gens);;
gap> NrDClasses(S);
232
gap> Size(S);
11858
gap> NrRClasses(S);
1455
gap> NrLClasses(S);
690
gap> NrHClasses(S);
5356
gap> NrIdempotents(S);
300
gap> Sum(List(GreensDClasses(S), NrRClasses)) = NrRClasses(S);
true
gap> ForAll(Concatenation(List(GreensDClasses(S), RClassReps)),
>                         x -> x in S);
true

# TestInstall12
gap> gens := [Transformation([1, 2, 3, 5, 4, 6, 7, 8]),
>             Transformation([4, 4, 3, 1, 5, 6, 3, 8]),
>             Transformation([3, 6, 1, 7, 3, 4, 8, 3]),
>             Transformation([1, 2, 3, 4, 5, 3, 7, 8]),
>             Transformation([1, 2, 3, 4, 1, 6, 7, 8]),
>             Transformation([8, 8, 3, 4, 5, 7, 6, 1])];;
gap> s := Monoid(gens);
<transformation monoid of degree 8 with 6 generators>
gap> t := ClosureSemigroup(s, [Transformation([4, 4, 3, 1, 5, 6, 3, 8])]);
<transformation monoid of degree 8 with 6 generators>
gap> Size(t) = Size(Semigroup(Generators(t)));
true

# TestInstall13
gap> S := Semigroup(Transformation([1, 2, 3, 4, 5, 6, 7, 8, 9]),
>                   Transformation([1, 2, 3, 4, 5, 6, 7, 9, 8]),
>                   Transformation([7, 2, 8, 4, 5, 6, 1, 9, 8]),
>                   Transformation([5, 5, 3, 4, 1, 6, 7, 8, 9]),
>                   Transformation([5, 7, 3, 4, 1, 6, 7, 8, 9]),
>                   Transformation([1, 2, 8, 6, 5, 4, 7, 9, 8]),
>                   Transformation([1, 8, 6, 2, 7, 8, 8, 9, 5]),
>                   Transformation([1, 2, 3, 8, 8, 7, 7, 9, 5]),
>                   Transformation([1, 2, 3, 1, 8, 7, 7, 5, 9]),
>                   Transformation([7, 7, 2, 7, 8, 8, 9, 5, 1]),
>                   Transformation([7, 2, 5, 2, 8, 8, 1, 9, 5]),
>                   Transformation([7, 2, 8, 1, 8, 7, 1, 9, 5]),
>                   Transformation([1, 1, 4, 8, 9, 9, 8, 5, 7]),
>                   Transformation([1, 1, 1, 2, 5, 5, 7, 8, 9]),
>                   Transformation([1, 2, 1, 1, 8, 7, 7, 5, 9]),
>                   Transformation([1, 2, 8, 8, 8, 2, 7, 9, 5]),
>                   Transformation([7, 2, 7, 1, 8, 8, 1, 5, 9]),
>                   Transformation([8, 8, 2, 8, 5, 5, 9, 7, 1]),
>                   Transformation([1, 2, 1, 1, 5, 5, 7, 8, 9]),
>                   Transformation([5, 5, 4, 5, 8, 8, 9, 7, 1]),
>                   Transformation([1, 2, 8, 8, 8, 1, 7, 9, 5]),
>                   Transformation([7, 2, 7, 2, 5, 5, 1, 8, 9]),
>                   rec(acting := true));;
gap> x := Transformation([7, 7, 4, 2, 1, 8, 8, 9, 5]);;
gap> D := DClass(S, Transformation([1, 8, 6, 2, 7, 8, 8, 9, 5]));;
gap> L := LClass(D, x);
<Green's L-class: Transformation( [ 7, 7, 4, 2, 1, 8, 8, 9, 5 ] )>
gap> LL := LClass(S, x);
<Green's L-class: Transformation( [ 7, 7, 4, 2, 1, 8, 8, 9, 5 ] )>
gap> List(HClassReps(LL), x -> x in LL);
[ true, true, true, true ]
gap> List(HClassReps(L), x -> x in L);
[ true, true, true, true ]
gap> L = LL;
true
gap> LL < L;
false
gap> L < LL;
false
gap> Elements(L) = Elements(LL);
true
gap> Size(L); Size(LL);
8
8
gap> DClassOfLClass(LL) = DClassOfLClass(L);
true
gap> DClassOfLClass(L) = D;
true
gap> NrHClasses(L); NrHClasses(LL);
4
4
gap> HClassReps(L);
[ Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] ), 
  Transformation( [ 7, 7, 4, 2, 1, 8, 8, 9, 5 ] ), 
  Transformation( [ 1, 8, 4, 2, 7, 8, 8, 5 ] ), 
  Transformation( [ 7, 7, 4, 2, 1, 8, 8, 5 ] ) ]
gap> HClassReps(LL);
[ Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] ), 
  Transformation( [ 7, 7, 4, 2, 1, 8, 8, 9, 5 ] ), 
  Transformation( [ 1, 8, 4, 2, 7, 8, 8, 5 ] ), 
  Transformation( [ 7, 7, 4, 2, 1, 8, 8, 5 ] ) ]
gap> Idempotents(L);
[  ]
gap> Idempotents(LL);
[  ]
gap> IsRegularDClass(D);
false
gap> Size(S);
6982
gap> Set(HClasses(L)) = Set(HClasses(LL));
true
gap> SchutzenbergerGroup(L);
Group([ (5,9), (1,7) ])
gap> g := SchutzenbergerGroup(LL);
Group([ (5,9), (1,7) ])

# TestInstall14: IsomorphismTransformationSemigroup/Monoid
gap> G := Group([(5, 9), (1, 7)]);;
gap> IsomorphismTransformationSemigroup(G);;
gap> S := Range(last);
<transformation group of degree 9 with 2 generators>
gap> IsGroupAsSemigroup(S);
true
gap> Generators(S);
[ Transformation( [ 1, 2, 3, 4, 9, 6, 7, 8, 5 ] ), 
  Transformation( [ 7, 2, 3, 4, 5, 6, 1 ] ) ]
gap> T := Range(IsomorphismTransformationMonoid(G));
<transformation group of degree 9 with 2 generators>
gap> Generators(T);
[ Transformation( [ 1, 2, 3, 4, 9, 6, 7, 8, 5 ] ), 
  Transformation( [ 7, 2, 3, 4, 5, 6, 1 ] ) ]
gap> H := Range(IsomorphismPermGroup(T));
Group([ (), (5,9), (1,7) ])

# TestInstall15: Issue 22 - takes about 49ms
gap> x := Transformation([2, 12, 10, 7, 6, 11, 8, 3, 4, 5, 1, 11]);;
gap> S := FullTransformationSemigroup(12);;
gap> S := Semigroup(S, rec(acting := true, regular := true));;
gap> InversesOfSemigroupElement(S, x);
[ Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 3, 3, 6, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 7, 3, 6, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 6, 3, 6, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 2, 3, 6, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 4, 3, 6, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 11, 3, 6, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 1, 3, 6, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 5, 3, 6, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 10, 3, 6, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 9, 3, 6, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 8, 3, 6, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 3, 3, 12, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 7, 3, 12, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 12, 3, 12, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 2, 3, 12, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 4, 3, 12, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 11, 3, 12, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 1, 3, 12, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 5, 3, 12, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 10, 3, 12, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 9, 3, 12, 2 ] ), 
  Transformation( [ 11, 1, 8, 9, 10, 5, 4, 7, 8, 3, 12, 2 ] ) ]

# TestInstall16
gap> file := Concatenation(PackageInfo("semigroups")[1]!.InstallationPath,
>                          "/data/tst/testinstall.pickle");;
gap>  ReadGenerators(file, 1);
[ <identity partial perm on [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]>, 
  <identity partial perm on [ 1, 2, 3, 4, 5, 6, 7, 9 ]>, 
  <identity partial perm on [ 1, 2, 3, 4, 5, 6, 9 ]>, 
  <identity partial perm on [ 1, 2, 3, 4, 5, 9 ]>, 
  <identity partial perm on [ 1, 2, 3, 4, 9 ]>, 
  <identity partial perm on [ 1, 2, 3, 9 ]>, 
  <identity partial perm on [ 1, 2, 9 ]>, <identity partial perm on [ 1, 9 ]> 
 ]

# TestInstall17
gap> S := InverseSemigroup(
> PartialPermNC([1, 2], [3, 1]),
> PartialPermNC([1, 2, 3], [1, 3, 4]),
> PartialPermNC([1, 2, 3], [2, 4, 1]),
> PartialPermNC([1, 3, 4], [3, 4, 1]));;
gap> Size(S); NrRClasses(S); NrLClasses(S); NrDClasses(S);
116
14
14
4
gap> NrIdempotents(S); NrRegularDClasses(S); IsRegularSemigroup(S);
14
4
true
gap> ForAll(S, x -> x in S);
true
gap> T := InverseSemigroup(Generators(S){[1 .. 3]});
<inverse partial perm semigroup of rank 4 with 3 generators>
gap> ForAll(T, x -> x in S);
true
gap> Size(T);
98

# TestInstall18
gap> S := InverseSemigroup(PartialPermNC([1, 3, 5, 6, 7], [9, 1, 5, 3, 8]),
>                          PartialPermNC([1, 2, 3, 5, 6, 7, 9, 10],
>                                        [4, 10, 5, 6, 7, 1, 3, 2]));;
gap> x := PartialPermNC([3, 4, 5, 6], [1, 3, 6, 5]);;
gap> D := DClass(S, x);;
gap> F := InjectionPrincipalFactor(D);; G := InverseGeneralMapping(F);;
gap> (x ^ F) ^ G = x;
true
gap> ForAll(D, x -> (x ^ F) ^ G = x);
true

# TestInstall18a
gap> S := InverseSemigroup(PartialPermNC([1, 3, 5, 6, 7], [9, 1, 5, 3, 8]),
>                          PartialPermNC([1, 2, 3, 5, 6, 7, 9, 10],
>                                        [4, 10, 5, 6, 7, 1, 3, 2]));;
gap> D := DClass(S, PartialPerm([2, 10], [2, 10]));
<Green's D-class: <identity partial perm on [ 2, 10 ]>>
gap> F := IsomorphismReesMatrixSemigroup(D);;
gap> G := InverseGeneralMapping(F);;
gap> ForAll(D, x -> (x ^ F) ^ G = x);
true

# TestInstall19: from JS' MultiplicativeZero.tst
gap> S := InverseMonoid(PartialPerm([1, 2, 3, 4]),
>                       PartialPerm([1, 3, 2, 4]),
>                       PartialPerm([1, 2, 0, 0]),
>                       PartialPerm([1, 0, 0, 4]));;
gap> x := PartialPerm([1, 0, 0, 0]);;
gap> x in S;
true
gap> ForAll(S, y -> x * y = x and y * x = x);
true
gap> x;
<identity partial perm on [ 1 ]>
gap> MultiplicativeZero(S);
<identity partial perm on [ 1 ]>

# TestInstall20: from JS' PartialPermInjective.tst
gap> PartialPerm([0, 0, 1, 2]);
[3,1][4,2]

# TestInstall21: from JS' RestricterPartialPerm.tst
gap> x := PartialPerm([2 .. 7], [1 .. 6]);
> RestrictedPartialPerm(x, [2 .. 7]);
[7,6,5,4,3,2,1]
[7,6,5,4,3,2,1]

# TestInstall22: from JS' SizeInverseMonoid.tst
gap> S := InverseMonoid(PartialPerm([1, 2, 3, 4, 5, 6, 7, 8],
>                                   [1, 6, 3, 4, 8, 2, 7, 5]),
>                       PartialPerm([1, 2, 3, 4, 5, 6, 7, 8],
>                                   [1, 2, 7, 4, 8, 6, 3, 5]),
>                       PartialPerm([1, 4, 5, 8], [1, 4, 5, 8]),
>                       PartialPerm([1, 2, 4, 6], [1, 2, 4, 6]),
>                       PartialPerm([1, 3, 4, 7], [1, 3, 4, 7]),
>                       PartialPerm([1], [1]));;
gap> [Size(S), Size(AsSet(S))];
[ 12, 12 ]

# TestInstall23: from JS' email
gap> S := InverseMonoid(PartialPerm([1, 3, 2]), PartialPerm([1]));;
gap> [Size(S), Size(AsSet(S))];
[ 3, 3 ]
gap> Elements(S);
[ <identity partial perm on [ 1 ]>, <identity partial perm on [ 1, 2, 3 ]>, 
  (1)(2,3) ]

# TestInstall24
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

# TestInstall25: Issue 27 in the new numbering...
gap> S := Semigroup(List(GeneratorsOfSemigroup(FullTransformationSemigroup(3)),
>  x -> AsTransformation(x, 4)));;
gap> IsFullTransformationSemigroup(S);
true
gap> IdentityTransformation in S;
true

# TestInstall26: Issue 23 in the new numbering...
gap> S := FullTransformationSemigroup(3);;
gap> x := Transformation([4, 3, 1, 2]);;
gap> ClosureSemigroup(S, x);
<transformation monoid of degree 4 with 4 generators>

# TestInstall27: Issue 36 in the new numbering...
gap> S := Semigroup(IdentityTransformation);
<trivial transformation group of degree 0 with 1 generator>
gap> SmallGeneratingSet(S);
[ IdentityTransformation ]

# TestInstall28: MaximalSubsemigroups of Rees 0-matrix semigroups
gap> G := Group((1, 2), (3, 4));;
gap> mat := [[(), ()], [(), 0], [(), (1, 2)]];;
gap> R := ReesZeroMatrixSemigroup(G, mat);;
gap> max := MaximalSubsemigroups(R);;
gap> Length(max);
6
gap> IsDuplicateFreeList(max);
true
gap> ForAll(MaximalSubsemigroups(R), U -> IsMaximalSubsemigroup(R, U));
true

# TestInstall29: ClosureSemigroup with an element of higher degree
gap> S := Semigroup(Transformation([1, 3, 3, 2]),
>                   Transformation([4, 1, 4, 2]),
>                   Transformation([4, 2, 3, 3]),
>                   Transformation([4, 4, 4, 4]));;
gap> Size(S);
130
gap> x := Transformation([3, 5, 1, 5, 2]);;
gap> T := ClosureSemigroup(S, x);;
gap> Size(T);
1619

# TestInstall30: bug with Elements and IsomorphismPermGroup for group H-class
gap> R := ReesZeroMatrixSemigroup(Group(()),
> [[(), (), ()], [(), 0, 0], [(), 0, 0]]);
<Rees 0-matrix semigroup 3x3 over Group(())>
gap> R := ReesZeroMatrixSubsemigroup(R, [2, 3], Group(()), [2, 3]);
<Rees 0-matrix semigroup 2x2 over Group(())>
gap> H := First(HClasses(R), IsGroupHClass);
<Green's H-class: 0>
gap> Elements(H);
[ 0 ]
gap> f := IsomorphismPermGroup(H);;
gap> g := InverseGeneralMapping(f);;
gap> Elements(H)[1] ^ f;
()
gap> () ^ g;
0

# TestInstall31: Issue 47: bug in ClosureSemigroup caused which assumed
# that if the rank of an R-class rep was greater than the maximum rank of the
# collection being added, then we hadn't seen an R-class rep with the same
# rho-value before.
gap> S := Semigroup([Transformation([1, 2, 4, 6, 1, 6]),
> Transformation([1, 6, 1, 1, 6, 5]),
> Transformation([2, 6, 2, 4, 3, 2]),
> Transformation([4, 1, 3, 6, 1, 5]),
> Transformation([4, 1, 4, 2, 4, 2]),
> Transformation([6, 6, 4, 6, 1, 1])]);
<transformation semigroup of degree 6 with 6 generators>
gap> T := Semigroup([Transformation([1, 5, 3, 4, 5]),
> Transformation([6, 4, 3, 5, 4, 1]),
> Transformation([1, 2, 4, 6, 1, 6]),
> Transformation([1, 5, 1, 6, 3, 1]),
> Transformation([4, 1, 6, 5, 4, 5]),
> Transformation([2, 6, 2, 4, 3, 2]),
> Transformation([2, 1, 2, 4, 4, 2]),
> Transformation([4, 5, 4, 4, 5, 3]),
> Transformation([4, 4, 4, 5, 4, 3]),
> Transformation([6, 1, 6, 6, 4, 6]),
> Transformation([5, 6, 6, 6, 6, 1]),
> Transformation([4, 4, 5, 4, 3, 3])]);
<transformation semigroup of degree 6 with 12 generators>
gap> IsMaximalSubsemigroup(S, T);
true
gap> T := Semigroup(T, rec(small := true));;
gap> IsMaximalSubsemigroup(S, T);
true

# TestInstall32: From Jack Schmidt 06/02/14 by email
gap> S := InverseMonoid(
>  PartialPerm(
>     [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
>        21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32],
>     [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 30, 29, 32, 31,
>        26, 25, 28, 27, 18, 17, 20, 19, 22, 21, 24, 23]),
>  PartialPerm(
>     [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
>        21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32],
>     [1, 10, 16, 7, 5, 14, 4, 11, 9, 2, 8, 15, 13, 6, 12, 3, 17, 22, 32, 27,
>        21, 18, 28, 31, 25, 30, 20, 23, 29, 26, 24, 19]),
>  PartialPerm([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16],
>     [1, 2, 3, 4, 16, 15, 13, 14, 10, 9, 12, 11, 7, 8, 6, 5]),
>  PartialPerm([1, 2, 5, 6, 9, 10, 13, 14, 17, 18, 21, 22, 25, 26, 29, 30],
>     [1, 17, 5, 29, 9, 21, 13, 25, 30, 14, 26, 6, 18, 10, 22, 2]),
>  PartialPerm([1, 2, 5, 6, 9, 10, 13, 14, 19, 20, 23, 24, 27, 28, 31, 32],
>     [1, 10, 5, 14, 9, 2, 13, 6, 19, 24, 23, 20, 27, 32, 31, 28]),
>  PartialPerm([1, 6, 9, 14, 18, 22, 25, 29],
>     [1, 6, 9, 14, 18, 22, 25, 29]),
>  PartialPerm([1, 5, 9, 13, 19, 23, 27, 31],
>     [1, 5, 9, 13, 19, 23, 27, 31]),
>  PartialPerm([1, 6, 9, 14, 17, 21, 26, 30],
>     [1, 6, 9, 14, 17, 21, 26, 30]));;
gap> ForAll(S, x -> ForAll(LClass(S, x), y -> y in S));
true

# TestInstall33: From Jack Schmidt 07/02/14 by email
gap> AsSet(InverseMonoid(PartialPerm([1, 2]), PartialPerm([1])));
[ <identity partial perm on [ 1 ]>, <identity partial perm on [ 1, 2 ]> ]

# TestInstall34: Issue #57 (problem in INV_KER_TRANS)
gap> S := Semigroup(Transformation([1, 1, 1]),
>                   Transformation([1, 1, 4, 4, 5]));;
gap> Size(S);
2
gap> IsMonogenicSemigroup(S);
false

# TestInstall35: Issue pointed out by WAW caused by
# IsInvLambdaOrb being inherited from the argument of ClosureSemigroup
# by its output, when the output wasn't an InverseOp semigroup...
gap> S := Semigroup(Bipartition([[1, -2], [2, -3], [3, -1]]),
>                   Bipartition([[1, 2, -2], [3, -1, -3]]),
>                   Bipartition([[1, -1, -3], [2, -2], [3]]),
>                   Bipartition([[1, 3, -1], [2, -2], [-3]]));;
gap> gens := [Bipartition([[1, 3, -1], [2, -2], [-3]]),
>             Bipartition([[1, -1, -3], [2, -2], [3]]),
>             Bipartition([[1, 2, -2], [3, -1, -3]])];;
gap> V := SemigroupIdealByGenerators(S, gens);;
gap> tuples := [Bipartition([[1, -1], [2, -2], [3, -3]])];;
gap> Semigroup(V, tuples, rec(small := true));;

# TestInstall36: Issue pointed out by WAW, caused by typo in ClosureSemigroup
# (the parent of an R-class was set to be the subsemigroup not the new parent
# semigroup)
gap> for i in [1 .. 6] do
>   V := Semigroup([PartialPerm([1, 2, 4, 5, 6], [1, 5, 3, 4, 6]),
>                   PartialPerm([1, 2, 4, 5, 6], [2, 1, 5, 4, 3]),
>                   PartialPerm([1, 3, 4, 5, 6], [1, 4, 5, 2, 6]),
>                   PartialPerm([1, 2, 3, 4, 5], [2, 1, 6, 5, 4]),
>                   PartialPerm([1, 2, 3, 6], [4, 3, 2, 6]),
>                   PartialPerm([1, 2, 4, 6], [2, 1, 5, 3]),
>                   PartialPerm([1, 2, 3, 6], [5, 2, 1, 3]),
>                   PartialPerm([2, 3, 4, 6], [3, 2, 1, 6]),
>                   PartialPerm([1, 2, 6], [3, 2, 6])],
>                   rec(small := true));
>   IsInverseSemigroup(V);
> od;

# TestInstall37: Issue #63 (problem with Monoid and InverseMonoid when one
# of the arguments is a monoid).
gap> S := Semigroup(PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]));
<trivial partial perm group of rank 5 with 1 generator>
gap> T := Monoid(S, PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3]));;
gap> Length(GeneratorsOfMonoid(T)) = 2;
true
gap> One(S) in T;
true
gap> One(S) = One(T);
false
gap> GeneratorsOfSemigroup(T) =
> [PartialPerm([1, 2, 3, 4, 5, 6]),
>  PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]),
>  PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3])];
true
gap> GeneratorsOfMonoid(T) =
> [PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]),
>  PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3])];
true
gap> S := InverseSemigroup(PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]));;
gap> T := InverseMonoid(S, PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3]));;
gap> Length(GeneratorsOfInverseMonoid(T)) = 2;
true
gap> GeneratorsOfMonoid(T) =
> [PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]),
>  PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3]),
>  PartialPerm([1, 2, 3, 4, 5], [4, 1, 6, 3, 2])];
true
gap> GeneratorsOfSemigroup(T) =
> [PartialPerm([1, 2, 3, 4, 5, 6], [1, 2, 3, 4, 5, 6]),
>  PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]),
>  PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3]),
>  PartialPerm([1, 2, 3, 4, 5], [4, 1, 6, 3, 2])];
true
gap> GeneratorsOfInverseMonoid(T) =
> [PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]),
>   PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3])];
true
gap> GeneratorsOfInverseSemigroup(T) =
> [PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]),
>  PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3]),
>  PartialPerm([1, 2, 3, 4, 5, 6], [1, 2, 3, 4, 5, 6])];
true
gap> One(S) in T;
true

# TestInstall38: Issue 33 (problem with Rees factor semigroups)
gap> I := SemigroupIdealByGenerators(FullTransformationSemigroup(4),
> [Transformation([1, 2, 2, 2])]);;
gap> cong := ReesCongruenceOfSemigroupIdeal(I);;
gap> hom := HomomorphismQuotientSemigroup(cong);;
gap> T := Range(hom);;
gap> IsSemigroup(T);
true
gap> Size(T);
169
gap> u := Image(hom, Transformation([1, 1, 1, 1]));
<2-sided congruence class of Transformation( [ 1, 2, 2, 2 ] )>
gap> t := Image(hom, Transformation([2, 1, 2, 3]));
<2-sided congruence class of Transformation( [ 2, 1, 2, 3 ] )>
gap> u * t;
<2-sided congruence class of Transformation( [ 1, 2, 2, 2 ] )>
gap> t * u;
<2-sided congruence class of Transformation( [ 1, 2, 2, 2 ] )>
gap> S := Semigroup(u, t);
<semigroup with 2 generators>
gap> Size(S);
2

# TestInstall39: Issue 56
# (Monoid/InverseMonoid removes One inappropriately sometimes)
gap> M := InverseMonoid(PartialPerm([1, 2]), PartialPerm([1]));
<inverse partial perm monoid of rank 2 with 2 generators>
gap> One(M) in M;
true
gap> AsSet(M);
[ <identity partial perm on [ 1 ]>, <identity partial perm on [ 1, 2 ]> ]
gap> M := InverseMonoid(PartialPerm([1, 2]), PartialPerm([1]));
<inverse partial perm monoid of rank 2 with 2 generators>
gap> AsSet(M);
[ <identity partial perm on [ 1 ]>, <identity partial perm on [ 1, 2 ]> ]

# TestInstall40: Issue 4 in Orb (logs are not properly updated if the
# enumeration stops early because we find something we are looking for)
gap> gens := [PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]),
>  PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3])];;
gap> o := Orb([gens[1]], [0], OnPosIntSetsPartialPerm, rec(log := true,
> lookingfor := function(o, x) return x = [1, 2, 4, 5, 6]; end));
<open orbit, 1 points looking for sth. with log>
gap>  Enumerate(o);
<open orbit, 2 points looking for sth. with log>
gap> o!.looking := false;
false
gap> Enumerate(o);
<closed orbit, 2 points with log>
gap> o!.logind;
[ 1, 0 ]
gap> o!.logpos;
3
gap> o!.log;
[ -1, 2 ]
gap> AddGeneratorsToOrbit(o, [gens[2]]);
<closed orbit, 12 points with log>

# TestInstall41: Issue 72
# (problem with IsomorphismTransformationSemigroup when applied to a
# binary relation monoid)
gap> B := Monoid(BinaryRelationOnPoints([[2], [1, 2], [1, 3]]),
>                BinaryRelationOnPoints([[3], [1, 2], [1, 3]]),
>                BinaryRelationOnPoints([[1, 2, 3], [1, 2], [3]]));;
gap> Size(B);
16
gap> IsMonoid(B);
true
gap> iso := IsomorphismTransformationSemigroup(B);;
gap> T := Range(iso);
<transformation monoid of degree 6 with 3 generators>
gap> Size(T);
16
gap> IsMonoid(T);
true

# TestInstall42: Issue 82 (couldn't take quotients by ideals!)
gap> S := Monoid(
>  Transformation([3, 3, 3, 3]), Transformation([2, 4, 2, 4]),
>  Transformation([2, 3, 2, 3]), Transformation([4, 1, 4, 3]),
>  Transformation([1, 4, 4, 1]), Transformation([2, 2, 3, 1]),
>  Transformation([2, 4, 3, 4]), Transformation([2, 2, 1, 2]),
>  Transformation([2, 2, 1, 3]), Transformation([1, 2, 2, 3]),
>  Transformation([2, 4, 3, 2]), Transformation([2, 3, 3, 3]));;
gap> I := SemigroupIdeal(S, S.3);;
gap> IsRegularSemigroup(I);
true
gap> S / I;;

# TestInstall43: Issue 89
gap> S := Semigroup(Transformation([2, 1, 3, 1, 4, 3]),
>                   Transformation([2, 2, 2, 2, 1, 2]),
>                   Transformation([5, 3, 4, 3, 5]),
>                   Transformation([6, 4, 1, 4, 5, 3]),
>                   Transformation([6, 5, 2, 6, 3, 4]));;
gap> NrIdempotents(S) = Number(HClasses(S), IsGroupHClass);
true

# TestInstall44: Issue 96 (problem with using the partial order of D-classes
# as an isomorphism invariant)
gap> S := Semigroup(Transformation([1, 2, 1]),
>                   Transformation([1, 2, 3, 2]),
>                   Transformation([2, 2, 2]),
>                   Transformation([4, 2, 1, 4]));;
gap> T := Semigroup(Transformation([1, 2, 3, 1]),
>                   Transformation([2, 2, 3, 1]),
>                   Transformation([2, 3, 3, 1]),
>                   Transformation([1, 3, 3]),
>                   Transformation([2, 3, 3, 3]),
>                   Transformation([3, 2, 3, 3]));;
gap> IsIsomorphicSemigroup(S, T);
true
gap> SmallestMultiplicationTable(S) = SmallestMultiplicationTable(T);
true

# TestInstall46: Issue 98
# (incorrect definition of partition monoid on 1 point)
gap> GeneratorsOfSemigroup(PartitionMonoid(1));
[ <block bijection: [ 1, -1 ]>, <bipartition: [ 1 ], [ -1 ]> ]

# TestInstall47: Issue 101 (incorrect method for
# DegreeOfTransformationSemigroup for a transformation group with 0 generators)
gap> G := GroupOfUnits(FullTransformationSemigroup(1));;
gap> G;
<trivial transformation group of degree 0 with 1 generator>

# TestInstall48: Issue 101
# (incorrect method for AsPartialPerm for a perm and zero)
gap> G := GroupOfUnits(Semigroup(PartialPerm([])));;
gap> G;
<trivial partial perm group of rank 0 with 1 generator>

# TestInstall49: Issue 103
# (problem with Enumerate(LambdaOrb(I)) when T is an inverse semigroup but
# doesn't know it at the start)
gap> S := POI(5);;
gap> T := Semigroup(S, PartialPerm([1, 2, 3, 4, 5], [2, 3, 4, 5, 1]));;
gap> I := SemigroupIdeal(T, [PartialPerm([1, 2, 4, 5], [1, 2, 3, 5])]);;
gap> IsInverseSemigroup(I);
true
gap> Size(I);
626

# TestInstall50: Issue 105 (CyclesOfPartialPerm returned nonsense)
gap> x := PartialPerm([1, 2, 3, 4, 5, 8, 10], [3, 1, 4, 2, 5, 6, 7]);;
gap> CyclesOfPartialPerm(x);
[ [ 5 ], [ 1, 3, 4, 2 ] ]

# TestInstall51: Issue 107
# (problems with Green's classes of ideals, and inverse semigroups)
gap> S := Monoid(PartialPermNC([1], [1]),
>                PartialPermNC([1], [2]),
>                PartialPermNC([2], [1]));;
gap> I := SemigroupIdeal(S, PartialPermNC([], []));;
gap> GeneratorsOfSemigroup(I);
[ <empty partial perm> ]

# TestInstall52: Issue 107 (problems with Green's classes of ideals, and
# inverse semigroups)
gap> S := [SymmetricInverseMonoid(2)];;
gap> S[2] := MaximalSubsemigroups(S[1]);;
gap> S[3] := List(S[2], MaximalSubsemigroups);;

# TestInstall53: Issue 109 (problem with IsReesZeroMatrixSemigroup on the
# subsemigroup generated by 0)
gap> R1 := ReesZeroMatrixSemigroup(Group(()), [[()]]);;
gap> R2 := Semigroup(MultiplicativeZero(R1));;
gap> IsReesZeroMatrixSubsemigroup(R2);
true
gap> IsReesZeroMatrixSemigroup(R2);
false

# TestInstall54: FreeBand
gap> S := FreeBand("a", "b", "c", "d", "e");
<free band on the generators [ a, b, c, d, e ]>
gap> iter := Iterator(S);
<iterator>
gap> for i in [1 .. 100] do
>      NextIterator(iter);
>    od;
gap> x := NextIterator(iter);
ece
gap> for i in [1 .. 10] do
>      NextIterator(iter);
>    od;
gap> y := NextIterator(iter);
abce
gap> x * y;
eceaceababce
gap> x ^ 2;
ece
gap> y ^ 2;
abce

# TestInstall55: Issue 110 (MaximalSubsemigroups for an non-regular RZMS)
gap> S := ReesZeroMatrixSemigroup(Group(()), [[(), 0], [0, ()]]);;
gap> S := Semigroup(RMSElement(S, 2, (), 2),
>                   RMSElement(S, 1, (), 2));;
gap> max := MaximalSubsemigroups(S);;
gap> Length(max);
2
gap> IsDuplicateFreeList(max);
true
gap> ForAll(max, x -> IsMaximalSubsemigroup(S, x));
true

# TestInstall56: Issue 122 (Problem with XClassType for inverse ideals)
gap> S := Semigroup(
> PartialPerm([1, 2, 3, 4], [2, 3, 4, 1]),
> PartialPerm([1, 2, 3, 4], [2, 1, 3, 4]),
> PartialPerm([1, 3], [2, 3]), rec(acting := true));;
gap> x := PartialPerm([], []);;
gap> I := SemigroupIdeal(S, x);;
gap> L := GreensLClassOfElement(I, x);
<Green's L-class: <empty partial perm>>
gap> SchutzenbergerGroup(L);
Group(())

# TestInstall57: Issue 123 (Incorrect method for IsZeroSemigroup for
# non-acting semigroup)
gap> x := Transformation([1, 1, 2, 3]);;
gap> S := Semigroup(x);;
gap> I := SemigroupIdeal(S, x);;
gap> IsZeroSemigroup(S);
false
gap> IsZeroSemigroup(Semigroup(x, rec(acting := false)));
false
gap> IsZeroSemigroup(I);
false

# TestInstall58: Issue 121:
# MultiplicativeZero for full transformation monoid on one point
gap> T := Transformation([1]);;
gap> S := Semigroup(T);;
gap> MultiplicativeZero(S) = T;
true
gap> gens := [Transformation([1, 2, 1]), Transformation([1, 2, 2])];;
gap> S := Semigroup(gens);;
gap> IsRectangularBand(S) and not IsTrivial(S);
true
gap> MultiplicativeZero(S);
fail

# TestInstall59: Issue 88:
# Something called by `JoinIrreducibleDClasses` of an acting semigroup ideal
# of a bipartition semigroup calls `GeneratorsOfSemigroup`
gap> S := InverseMonoid(DualSymmetricInverseMonoid(6), rec(acting := true));;
gap> x := Bipartition([[1, 2, -3], [3, -1, -2], [4, -4],
> [5, -5], [6, -6]]);;
gap> I := SemigroupIdeal(S, x);
<inverse bipartition semigroup ideal of degree 6 with 1 generator>
gap> JoinIrreducibleDClasses(I);
[ <Green's D-class: <block bijection: [ 1, 2, 3, 4, 6, -1, -2, -3, -4, -6 ], 
      [ 5, -5 ]>> ]
gap> I;
<inverse block bijection semigroup ideal of degree 6 with 1 generator>
gap> S := InverseMonoid(DualSymmetricInverseMonoid(3));;
gap> x := Bipartition([[1, 2, -1, -2], [3, -3]]);;
gap> I := SemigroupIdeal(S, x);
<inverse bipartition semigroup ideal of degree 3 with 1 generator>
gap> JoinIrreducibleDClasses(I);
[ <Green's D-class: <block bijection: [ 1, 2, -1, -2 ], [ 3, -3 ]>> ]

# TestInstall60: Issue 94:
# EquivalenceClasses of trivial congruence returns empty list
gap> S := FullTransformationSemigroup(6);;
gap> R := PrincipalFactor(MinimalDClass(S));
<Rees matrix semigroup 1x6 over Group(())>
gap> cong := SemigroupCongruenceByGeneratingPairs(R, []);;
gap> EquivalenceClasses(cong);
[ <2-sided congruence class of (1,(),1)>, 
  <2-sided congruence class of (1,(),2)>, 
  <2-sided congruence class of (1,(),3)>, 
  <2-sided congruence class of (1,(),4)>, 
  <2-sided congruence class of (1,(),5)>, 
  <2-sided congruence class of (1,(),6)> ]

# TestInstall61: Issue 95:
# No zero class in semigroup congruence EquivalenceClasses (generating pairs)
gap> P := [[(), 0, (1, 3), (1, 3), 0, (), 0],
>   [(), (1, 3), 0, 0, (1, 3), (), 0], [(), (1, 3), 0, (), 0, 0, ()],
>   [0, (), (1, 3), (1, 3), (), 0, 0], [0, 0, 0, (), (), (1, 3), ()],
>   [(), 0, (1, 3), 0, (), 0, ()]];;
gap> R := ReesZeroMatrixSemigroup(Group([(1, 3)]), P);;
gap> x := ReesZeroMatrixSemigroupElement(R, 1, (1, 3), 1);;
gap> y := ReesZeroMatrixSemigroupElement(R, 1, (), 1);;
gap> cong := SemigroupCongruenceByGeneratingPairs(R, [[x, y]]);;
gap> c := Set(EquivalenceClasses(cong));
[ <2-sided congruence class of (1,(),1)>, 
  <2-sided congruence class of (1,(),2)>, 
  <2-sided congruence class of (1,(),3)>, 
  <2-sided congruence class of (1,(),4)>, 
  <2-sided congruence class of (1,(),5)>, 
  <2-sided congruence class of (1,(),6)>, 
  <2-sided congruence class of (2,(),1)>, 
  <2-sided congruence class of (3,(),1)>, 
  <2-sided congruence class of (4,(),1)>, 
  <2-sided congruence class of (5,(),1)>, 
  <2-sided congruence class of (6,(),1)>, 
  <2-sided congruence class of (7,(),1)>, <2-sided congruence class of 0>, 
  <2-sided congruence class of (2,(),2)>, 
  <2-sided congruence class of (2,(),3)>, 
  <2-sided congruence class of (2,(),4)>, 
  <2-sided congruence class of (2,(),5)>, 
  <2-sided congruence class of (2,(),6)>, 
  <2-sided congruence class of (3,(),2)>, 
  <2-sided congruence class of (3,(),3)>, 
  <2-sided congruence class of (3,(),4)>, 
  <2-sided congruence class of (3,(),5)>, 
  <2-sided congruence class of (3,(),6)>, 
  <2-sided congruence class of (4,(),2)>, 
  <2-sided congruence class of (4,(),3)>, 
  <2-sided congruence class of (4,(),4)>, 
  <2-sided congruence class of (4,(),5)>, 
  <2-sided congruence class of (4,(),6)>, 
  <2-sided congruence class of (5,(),2)>, 
  <2-sided congruence class of (5,(),3)>, 
  <2-sided congruence class of (5,(),4)>, 
  <2-sided congruence class of (5,(),5)>, 
  <2-sided congruence class of (5,(),6)>, 
  <2-sided congruence class of (6,(),2)>, 
  <2-sided congruence class of (6,(),3)>, 
  <2-sided congruence class of (6,(),4)>, 
  <2-sided congruence class of (6,(),5)>, 
  <2-sided congruence class of (6,(),6)>, 
  <2-sided congruence class of (7,(),2)>, 
  <2-sided congruence class of (7,(),3)>, 
  <2-sided congruence class of (7,(),4)>, 
  <2-sided congruence class of (7,(),5)>, 
  <2-sided congruence class of (7,(),6)> ]
gap> ForAny(c, x -> MultiplicativeZero(R) in x);
true

# TestInstall62: Issue 119:
# Bug in NrEquivalenceClasses for Rees congruences
gap> I := SemigroupIdealByGenerators(FullTransformationSemigroup(4),
> [Transformation([1, 2, 2, 2])]);
<regular transformation semigroup ideal of degree 4 with 1 generator>
gap> cong := ReesCongruenceOfSemigroupIdeal(I);
<Rees congruence of <regular transformation semigroup ideal of degree 4 with
  1 generator> over <full transformation monoid of degree 4>>
gap> NrEquivalenceClasses(cong);
169

# TestInstall65: Issue 126:
gap> mat := [
> [(), 0, 0, 0, 0],
> [0, (3, 4), 0, 0, 0],
> [0, 0, (), 0, 0],
> [0, 0, 0, (1, 2), 0],
> [0, 0, 0, 0, (1, 2)(3, 4)]];;
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2), (3, 4)]), mat);;
gap> gens := [
> MultiplicativeZero(R),
> RMSElement(R, 4, (), 4),
> RMSElement(R, 4, (1, 2)(3, 4), 4),
> RMSElement(R, 5, (1, 2)(3, 4), 5),
> RMSElement(R, 5, (1, 2), 5),
> RMSElement(R, 4, (1, 2), 4),
> RMSElement(R, 1, (1, 2), 1),
> RMSElement(R, 1, (3, 4), 1),
> RMSElement(R, 2, (3, 4), 2),
> RMSElement(R, 2, (1, 2), 2),
> RMSElement(R, 1, (1, 2), 1),
> RMSElement(R, 3, (), 3),
> RMSElement(R, 3, (1, 2), 3),
> RMSElement(R, 1, (1, 2), 1)];;
gap> U := Semigroup(gens);;
gap> Filtered(R, x -> x in U);;
gap> x := RMSElement(R, 1, (), 2);;
gap> x in U;
false
gap> IsInverseSemigroup(U);
true
gap> x in U;
false

# TestInstall63: Issue 127:
# Bug in Enumerate for an acting semigroup ideal that knows it is regular at its
# point of creation.
gap> S := Semigroup([[[Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>                     [0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2)],
>                     [0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2)],
>                     [0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0]],
>                    [[Z(2 ^ 2), Z(2) ^ 0, Z(2 ^ 2), Z(2) ^ 0],
>                     [Z(2 ^ 2) ^ 2, Z(2 ^ 2), Z(2 ^ 2) ^ 2, Z(2 ^ 2)],
>                     [Z(2) ^ 0, Z(2 ^ 2) ^ 2, Z(2) ^ 0, Z(2 ^ 2) ^ 2],
>                     [Z(2) ^ 0, Z(2 ^ 2) ^ 2, Z(2) ^ 0, Z(2 ^ 2) ^ 2]],
>                    [[0 * Z(2), Z(2 ^ 2) ^ 2, 0 * Z(2), Z(2) ^ 0],
>                     [Z(2 ^ 2) ^ 2, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0],
>                     [Z(2 ^ 2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2)],
>                     [Z(2 ^ 2) ^ 2, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0]]]);
<monoid with 2 generators>
gap> T := AsSemigroup(IsTransformationSemigroup, S);
<transformation monoid of size 21, degree 21 with 2 generators>
gap> Size(T);
21
gap> I := SemigroupIdeal(T, Idempotents(T));;
gap> Size(I);
21

# TestInstall64: Bug fixed by changeset 949553d
gap> S := InverseSemigroup(PartialPerm([1], [2]), PartialPerm([2], [1]));
<inverse partial perm semigroup of rank 2 with 2 generators>
gap> Size(S);
5
gap> C := SemigroupCongruence(S, [S.1, S.1 * S.2]);
<universal semigroup congruence over <0-simple inverse partial perm semigroup 
 of size 5, rank 2 with 2 generators>>
gap> IsUniversalSemigroupCongruence(C);
true

# TestInstall65: Fixed bug where the GeneratorsOfMonoid were incorrectly set
#   for partial perm monoids/inverse monoids, due to removal of the One.
gap> S := Semigroup(PartialPerm([1]), PartialPerm([]));
<partial perm monoid of rank 1 with 2 generators>
gap> S := Monoid(PartialPerm([1]), PartialPerm([]));
<partial perm monoid of rank 1 with 2 generators>
gap> S := InverseSemigroup(PartialPerm([1]), PartialPerm([]));
<inverse partial perm monoid of rank 1 with 2 generators>
gap> S := InverseMonoid(PartialPerm([1]), PartialPerm([]));
<inverse partial perm monoid of rank 1 with 2 generators>

# TestInstall66: semigroups with 0 generators are not allowed in Semigroups,
#i.e. this example shouldn't use Semigroups code and this is here to make sure
#that it does not. This is from bugfix.tst.
gap> AsGroup([-1, 1]);
fail

# TestInstall67: semigroups with infinity generators are not allowed in
# Semigroups, i.e. this example shouldn't use Semigroups code and this is here
# to make sure that it does not. This is from bugfix.tst.
gap> FreeMonoid(infinity, "m", []);
<free monoid with infinity generators>

# TestInstall65: Issue #131
gap> S := FullTransformationSemigroup(3);;
gap> I := SemigroupIdeal(FullTransformationSemigroup(3),
>                        Transformation([1, 1, 2]));;
gap> T := S / I;;
gap> One(T);
<2-sided congruence class of IdentityTransformation>

# TestInstall66: Second bug in Issue #131
gap> I := SemigroupIdeal(FullTransformationSemigroup(3),
>                        Transformation([1, 1, 1]));;
gap> hom := HomomorphismQuotientSemigroup(ReesCongruenceOfSemigroupIdeal(I));;
gap> map := IsomorphismTransformationSemigroup(Range(hom));;

# Checking for correct non-removal of one from generating sets in
# SemigroupByGenerators
gap> S := Semigroup(PartialPerm([1]));
<trivial partial perm group of rank 1 with 1 generator>
gap> GeneratorsOfSemigroup(S);
[ <identity partial perm on [ 1 ]> ]
gap> GeneratorsOfMonoid(S);
[ <identity partial perm on [ 1 ]> ]
gap> GeneratorsOfInverseSemigroup(S);
[ <identity partial perm on [ 1 ]> ]
gap> GeneratorsOfInverseMonoid(S);
[ <identity partial perm on [ 1 ]> ]
gap> S := Semigroup(IdentityTransformation);
<trivial transformation group of degree 0 with 1 generator>
gap> GeneratorsOfSemigroup(S);
[ IdentityTransformation ]
gap> GeneratorsOfMonoid(S);
[ IdentityTransformation ]

# Checking for correct non-removal of one from generating sets in
# MonoidByGenerators
gap> S := Monoid(PartialPerm([1]));
<trivial partial perm group of rank 1 with 1 generator>
gap> S := Monoid(PartialPerm([1]));
<trivial partial perm group of rank 1 with 1 generator>
gap> GeneratorsOfSemigroup(S);
[ <identity partial perm on [ 1 ]> ]
gap> GeneratorsOfMonoid(S);
[ <identity partial perm on [ 1 ]> ]
gap> GeneratorsOfInverseSemigroup(S);
[ <identity partial perm on [ 1 ]> ]
gap> GeneratorsOfInverseMonoid(S);
[ <identity partial perm on [ 1 ]> ]
gap> S := Monoid(IdentityTransformation);
<trivial transformation group of degree 0 with 1 generator>
gap> GeneratorsOfSemigroup(S);
[ IdentityTransformation ]
gap> GeneratorsOfMonoid(S);
[ IdentityTransformation ]

# Checking for correct non-removal of one from generating sets in
# InverseSemigroupByGenerators JDM
gap> S := InverseSemigroup(PartialPerm([1]));
<trivial partial perm group of rank 1 with 1 generator>
gap> GeneratorsOfSemigroup(S);
[ <identity partial perm on [ 1 ]> ]
gap> GeneratorsOfMonoid(S);
[ <identity partial perm on [ 1 ]> ]
gap> GeneratorsOfInverseSemigroup(S);
[ <identity partial perm on [ 1 ]> ]
gap> GeneratorsOfInverseMonoid(S);
[ <identity partial perm on [ 1 ]> ]
gap> S := InverseSemigroup(IdentityTransformation);
<trivial transformation group of degree 0 with 1 generator>
gap> GeneratorsOfSemigroup(S);
[ IdentityTransformation ]
gap> GeneratorsOfMonoid(S);
[ IdentityTransformation ]
gap> GeneratorsOfInverseSemigroup(S);
[ IdentityTransformation ]
gap> GeneratorsOfInverseMonoid(S);
[ IdentityTransformation ]

# Checking for correct non-removal of one from generating sets in
# InverseMonoidByGenerators JDM
gap> S := InverseMonoid(PartialPerm([1]));
<trivial partial perm group of rank 1 with 1 generator>
gap> GeneratorsOfSemigroup(S);
[ <identity partial perm on [ 1 ]> ]
gap> GeneratorsOfMonoid(S);
[ <identity partial perm on [ 1 ]> ]
gap> GeneratorsOfInverseSemigroup(S);
[ <identity partial perm on [ 1 ]> ]
gap> GeneratorsOfInverseMonoid(S);
[ <identity partial perm on [ 1 ]> ]
gap> S := InverseMonoid(IdentityTransformation);
<trivial transformation group of degree 0 with 1 generator>
gap> GeneratorsOfSemigroup(S);
[ IdentityTransformation ]
gap> GeneratorsOfMonoid(S);
[ IdentityTransformation ]
gap> GeneratorsOfInverseSemigroup(S);
[ IdentityTransformation ]
gap> GeneratorsOfInverseMonoid(S);
[ IdentityTransformation ]

# Checking GroupByGenerators
gap> S := Group(PartialPerm([1]));
<trivial partial perm group of rank 1 with 1 generator>
gap> GeneratorsOfSemigroup(S);
[ <identity partial perm on [ 1 ]> ]
gap> GeneratorsOfMonoid(S);
[ <identity partial perm on [ 1 ]> ]
gap> GeneratorsOfGroup(S);
[ <identity partial perm on [ 1 ]> ]
gap> GeneratorsOfInverseSemigroup(S);
[ <identity partial perm on [ 1 ]> ]
gap> GeneratorsOfInverseMonoid(S);
[ <identity partial perm on [ 1 ]> ]
gap> S := Group(IdentityTransformation);;
gap> IsGroup(S) and IsGroupAsSemigroup(S) and IsTrivial(S);
true
gap> GeneratorsOfSemigroup(S);
[ IdentityTransformation ]
gap> GeneratorsOfMonoid(S);
[ IdentityTransformation ]
gap> GeneratorsOfGroup(S);
[ IdentityTransformation ]
gap> GeneratorsOfInverseSemigroup(S);
[ IdentityTransformation ]
gap> GeneratorsOfInverseMonoid(S);
[ IdentityTransformation ]

# Test for Issue 136
gap> S := Semigroup(PartialPerm([2, 0]));
<commutative partial perm semigroup of rank 1 with 1 generator>
gap> CyclesOfPartialPermSemigroup(S);
[  ]

# AsXSemigroup for trivial transformation semigroup
gap> S := Semigroup(IdentityTransformation);
<trivial transformation group of degree 0 with 1 generator>
gap> AsSemigroup(IsTransformationSemigroup, S);
<trivial transformation group of degree 0 with 1 generator>
gap> AsSemigroup(IsPartialPermSemigroup, S);
<trivial partial perm group of rank 0 with 1 generator>
gap> AsSemigroup(IsBipartitionSemigroup, S);
<trivial block bijection group of degree 1 with 1 generator>
gap> AsSemigroup(IsPBRSemigroup, S);
<trivial pbr group of degree 1 with 1 generator>
gap> AsSemigroup(IsBooleanMatSemigroup, S);
<trivial group of 1x1 boolean matrices with 1 generator>

# Issue #138: InversesOfSemigroupElement for trivial transformation monoid
gap> x := IdentityTransformation;;
gap> InversesOfSemigroupElement(FullTransformationMonoid(1), x);
[ IdentityTransformation ]

# GroupOfUnits error (Monoid used inappropriately instead of Semigroup)
gap> S := Semigroup([Bipartition([[1], [-1]])]);
<commutative bipartition semigroup of degree 1 with 1 generator>
gap> Size(S);
1
gap> Size(GroupOfUnits(S));
1
gap> S := Semigroup(IdentityTransformation);
<trivial transformation group of degree 0 with 1 generator>
gap> GroupOfUnits(S);
<trivial transformation group of degree 0 with 1 generator>
gap> Size(GroupOfUnits(S));
1
gap> S := Semigroup(Transformation([1, 1]));
<commutative transformation semigroup of degree 2 with 1 generator>
gap> Size(GroupOfUnits(S));
1

# IsInverseSemigroup fall back method was wrong.
gap> S := Semigroup([Bipartition([[1, 2], [-1], [-2]]),
>  Bipartition([[1, -1], [2], [-2]]),
>  Bipartition([[1], [2, -1], [-2]]),
>  Bipartition([[1, -2], [2], [-1]]),
>  Bipartition([[1], [2, -2], [-1]]),
>  Bipartition([[1], [2], [-1], [-2]])]);;
gap> IsInverseSemigroup(S);
false
gap> S := Semigroup(S, rec(acting := false));;
gap> IsInverseSemigroup(S);
false

# Bipartition semigroups of degree 0, Issue #139
gap> AsSemigroup(IsBipartitionSemigroup, CyclicGroup(IsPermGroup, 1));
<trivial block bijection group of degree 0 with 1 generator>
gap> AsSemigroup(IsBipartitionSemigroup, Group(()));
<trivial block bijection group of degree 0 with 1 generator>
gap> Semigroup(Bipartition([]));
<trivial block bijection group of degree 0 with 1 generator>
gap> JonesMonoid(0);
<trivial block bijection group of degree 0 with 1 generator>
gap> PartitionMonoid(0);
<trivial block bijection group of degree 0 with 1 generator>

# Fixed unconvert for matrix over semiring
gap> S := FullTropicalMinPlusMonoid(2, 3);
<monoid of 2x2 tropical min-plus matrices with 7 generators>
gap> AsSemigroup(IsTransformationSemigroup, S);
<transformation monoid of size 625, degree 625 with 7 generators>

# Test for not being allowed to generate a semigroup with bipartitions of
# different degree
gap> Semigroup(Bipartition([[-1, 1]]), Bipartition([]));
Error, Usage: Semigroup(<gen>,...), Semigroup(<gens>), Semigroup(<D>),

# Test for not being allowed to generate a semigroup with matrices of
# different dimensions
gap> Semigroup(
> Matrix(Integers, [[0, -3, 0, -2],
>                          [-1, 1, -1, 0],
>                          [0, 1, 0, 1],
>                          [0, 0, 2, 0]]),
> Matrix(Integers, [[4, 0, -2],
>                          [1, -3, 0],
>                          [5, -1, -4]]));
Error, Usage: Semigroup(<gen>,...), Semigroup(<gens>), Semigroup(<D>),

# Test for Issue 141
gap> S := Semigroup(Bipartition ([[1, 4], [2, 3], [-1, -4], [-2, -3]]),
>                   Bipartition([[1, 2], [3, 4], [-1, -4], [-2, -3]]));;
gap> PartialOrderOfDClasses(S);
<immutable empty digraph with 1 vertex>

# Test for Issue 144
gap> S := Semigroup(Bipartition([[1, 2], [-1], [-2]]),
>                   Bipartition([[1, -1], [2], [-2]]),
>                   Bipartition([[1], [2, -1], [-2]]),
>                   Bipartition([[1, -2], [2], [-1]]),
>                   Bipartition([[1], [2, -2], [-1]]),
>                   Bipartition([[1], [2], [-1], [-2]]));;
gap> IsInverseSemigroup(S);
false
gap> S := Semigroup(Bipartition([[1, 2], [-1], [-2]]),
>                   Bipartition([[1, -1], [2], [-2]]),
>                   Bipartition([[1], [2, -1], [-2]]),
>                   Bipartition([[1, -2], [2], [-1]]),
>                   Bipartition([[1], [2, -2], [-1]]),
>                   Bipartition([[1], [2], [-1], [-2]]));;
gap> NrDClasses(S);;
gap> IsInverseSemigroup(S);
false

# MaximalSubsemigroups, replacement test for manual example which becomes a
#log because of the randomness in the generating sets here.
gap> S := FullTransformationMonoid(4);;
gap> Length(MaximalSubsemigroups(S)) = 9;
true
gap> ForAll(MaximalSubsemigroups(S), M -> M in
> [Semigroup(Transformation([1, 4, 2, 3]),
>            Transformation([4, 2, 3, 4]),
>            Transformation([4, 3, 2, 1])),
>  Semigroup(Transformation([1, 1, 2, 3]),
>            Transformation([1, 2, 4, 3]),
>            Transformation([4, 2, 3, 4]),
>            Transformation([4, 3, 2, 1])),
>  Semigroup(Transformation([1, 1, 2, 3]),
>            Transformation([1, 3, 1, 2]),
>            Transformation([1, 4, 3, 2]),
>            Transformation([2, 1, 4, 3])),
>  Semigroup(Transformation([1, 3, 2]),
>            Transformation([2, 1, 3, 1]),
>            Transformation([3, 4, 1, 2]),
>            Transformation([3, 4, 1, 3])),
>  Semigroup(Transformation([1, 2, 4, 3]),
>            Transformation([1, 4, 2, 3]),
>            Transformation([2, 3, 1, 1]),
>            Transformation([2, 3, 1, 2]),
>            Transformation([4, 2, 3, 4])),
>  Semigroup(Transformation([1, 1, 2, 3]),
>            Transformation([1, 3, 2]),
>            Transformation([3, 1, 2]),
>            Transformation([4, 1, 2, 4])),
>  Semigroup(Transformation([2, 1]),
>            Transformation([2, 3, 1, 1]),
>            Transformation([4, 1, 2, 4]),
>            Transformation([4, 1, 3, 2])),
>  Semigroup(Transformation([2, 1, 3, 1]),
>            Transformation([3, 4, 1, 3]),
>            Transformation([4, 2, 1, 3]),
>            Transformation([4, 2, 3, 1])),
>  Semigroup(Transformation([2, 1]),
>            Transformation([2, 3, 4, 1]),
>            Transformation([3, 1, 3, 3]),
>            Transformation([4, 3, 3, 4]))]);
true

# Test for not being allowed to generate a semigroup with bipartitions of
# different degree
gap> Semigroup(Bipartition([[-1, 1]]), Bipartition([]));
Error, Usage: Semigroup(<gen>,...), Semigroup(<gens>), Semigroup(<D>),

# Issue 150: Bug in RepresentativeOfMinimalIdeal
gap> S := Semigroup([PartialPerm([1, 2], [3, 2])]);;
gap> RepresentativeOfMinimalIdeal(S);
<identity partial perm on [ 2 ]>
gap> IsZeroSimpleSemigroup(S);
false

# Issue 152: Bug in IsomorphismPermGroup for non-perm transformations
gap> S := Semigroup(Transformation([1, 3, 2, 1]),
>                   Transformation([2, 1, 3, 2]));;
gap> iso := IsomorphismPermGroup(S);;
gap> inv := InverseGeneralMapping(iso);;
gap> ForAll(S, x -> (x ^ iso) ^ inv = x);
true

# Issue 160: Bug in IrreundantGeneratingSubset for a semigroup with a single
# repeated generator
gap> S := Semigroup([Transformation([1, 1]), Transformation([1, 1])]);
<transformation semigroup of degree 2 with 2 generators>
gap> IrredundantGeneratingSubset(S);
[ Transformation( [ 1, 1 ] ) ]

# Issue 164: Bug in MatrixEntries for a Rees 0-matrix semigroup
gap> S := Semigroup(SymmetricInverseMonoid(2));;
gap> id := Identity(S);;
gap> R := ReesZeroMatrixSemigroup(S, [[id], [0]]);;
gap> MatrixEntries(R);
[ 0, <identity partial perm on [ 1, 2 ]> ]

# Test for bug in kernel module
gap> AsList(JonesMonoid(1));
[ <block bijection: [ 1, -1 ]> ]

# Kernel-trace methods should only be selected for semigroups with inverse op
gap> S := HallMonoid(2);;
gap> latt := LatticeOfCongruences(S);;
gap> IsIsomorphicDigraph(latt, DigraphFromDigraph6String("&C|E["));
true
gap> IsPartialOrderDigraph(latt);
true

# Test bug in \in for high degree transformation semigroup
gap> S := Semigroup(Transformation([4, 3, 2, 1]),
>                   Transformation([3, 2, 3, 2]),
>                   Transformation([1, 1, 2, 4]),
>                   Transformation([1, 4, 3, 1]));;
gap> T := Range(AntiIsomorphismTransformationSemigroup(S));;
gap> M := MaximalSubsemigroups(T);;
gap> T in M;
false

# Test for Issue 217 (bug in \in for L-class of inverse semigroups)
gap> S := InverseMonoid(PartialPerm([2, 3, 4, 5, 6], [1, 2, 3, 4, 5]),
> PartialPerm([1, 2, 3, 4, 5], [1, 2, 3, 4, 6]),
> PartialPerm([1, 2, 3, 4, 6], [1, 2, 3, 5, 6]),
> PartialPerm([1, 2, 3, 5, 6], [1, 2, 4, 5, 6]),
> PartialPerm([1, 2, 4, 5, 6], [1, 3, 4, 5, 6]),
> PartialPerm([1, 3, 4, 5, 6], [2, 3, 4, 5, 6]),
> PartialPerm([1, 2, 3, 4, 5, 6], [6, 5, 4, 3, 2, 1]));;
gap> NrMaximalSubsemigroups(S);
11
gap> y := PartialPerm([1, 2, 3, 4, 5], [6, 5, 4, 2, 1]);;
gap> D := DClass(S, y);;
gap> y in D;
true
gap> ForAny(LClasses(D), L -> y in L);
true
gap> ForAll(LClasses(D), L -> ForAll(L, x -> x in L));
true

# Bug in AsInverseSemigroupCongruenceByKernelTrace - abnormal trace
gap> S := InverseSemigroup([PartialPerm([1, 2, 3], [5, 1, 2]),
>                           PartialPerm([1, 2, 3, 4, 6], [3, 6, 4, 1, 5]),
>                           PartialPerm([1, 2, 3, 4], [6, 1, 4, 2]),
>                           PartialPerm([1, 3, 6], [4, 5, 2]),
>                           PartialPerm([1, 3, 6], [1, 3, 6])]);;
gap> pairs := [[PartialPerm([5], [1]), PartialPerm([], [])],
>              [PartialPerm([1, 2, 3, 4], [1, 2, 3, 4]),
>               PartialPerm([1, 3, 4, 6], [1, 3, 4, 6])]];;
gap> cong := SemigroupCongruence(S, pairs);;
gap> NrEquivalenceClasses(cong);
32

# Bug in AsInverseSemigroupCongruenceByKernelTrace - abnormal kernel
gap> S := InverseSemigroup([PartialPerm([1, 2], [1, 6]),
>                           PartialPerm([1, 2, 4, 5], [1, 4, 5, 3]),
>                           PartialPerm([1, 3, 4], [1, 2, 5]),
>                           PartialPerm([1, 2, 3, 5, 6], [5, 1, 6, 2, 4]),
>                           PartialPerm([1, 2, 3, 5, 6], [5, 4, 3, 1, 2])]);;
gap> pairs := [[PartialPerm([2, 5, 6], [1, 6, 5]),
>               PartialPerm([2, 5, 6], [5, 1, 6])],
>              [PartialPerm([5], [2]), PartialPerm([1, 5], [5, 4])]];;
gap> cong := SemigroupCongruence(S, pairs);;
gap> NrEquivalenceClasses(cong);
954

# Fix for Issue 230: bug in MaximalSubsemigroups
gap> mat := [
> [(), 0, 0, 0, 0, 0],
> [0, (), 0, 0, 0, 0],
> [0, 0, (), 0, 0, 0],
> [0, 0, 0, (), 0, 0],
> [0, 0, 0, 0, (), 0],
> [0, 0, 0, 0, 0, ()]];;
gap> R := ReesZeroMatrixSemigroup(Group([(1, 5, 4, 3, 2), (1, 5)(2, 4)]),
>                                 mat);;
gap> contain := [
>  RMSElement(R, 1, (1, 5)(2, 4), 6),
>  RMSElement(R, 1, (), 6),
>  RMSElement(R, 2, (1, 2, 3, 4, 5), 1),
>  RMSElement(R, 2, (), 5),
>  RMSElement(R, 3, (), 2),
>  RMSElement(R, 3, (1, 3)(4, 5), 4),
>  RMSElement(R, 4, (), 3),
>  RMSElement(R, 4, (1, 3)(4, 5), 3),
>  RMSElement(R, 5, (1, 3)(4, 5), 4),
>  RMSElement(R, 5, (), 2),
>  RMSElement(R, 6, (1, 5, 4, 3, 2), 5),
>  RMSElement(R, 6, (), 1)];;
gap> MaximalSubsemigroups(R, rec(number := true, contain := contain));
1

# Fix issue with GeneratorsSmallest for a transformation semigroup generated by
# the identity
gap> S := Semigroup(IdentityTransformation);;
gap> GeneratorsSmallest(S);
[ IdentityTransformation ]

# Fix issue with duplicate generators and ClosureSemigroup
gap> gens := [Transformation([1, 2, 1]),
>             IdentityTransformation,
>             Transformation([1, 2, 4, 6, 6, 5]),
>             Transformation([2, 1, 3, 5, 5]),
>             Transformation([5, 4, 4, 2, 1]),
>             Transformation([5, 4, 6, 2, 1, 6]),
>             Transformation([6, 6, 3, 4, 5, 1])];;
gap> S := Semigroup(gens[1], gens[1]);;
gap> Size(S);
1
gap> for x in gens do
>   S := ClosureSemigroup(S, x);
>   Size(S);
> od;
gap> Size(S);
119
gap> Size(Semigroup(S));
119

# Test Issue #237 part 1
gap> T := Semigroup([
>  Transformation([1, 2, 6, 4, 5, 6]),
>  Transformation([1, 2, 3, 4, 6, 6]),
>  Transformation([1, 2, 3, 6, 5, 6]),
>  Transformation([1, 6, 6, 4, 5, 6]),
>  Transformation([1, 6, 3, 4, 6, 6]),
>  Transformation([1, 6, 3, 6, 5, 6]),
>  Transformation([6, 2, 3, 6, 5, 6]),
>  Transformation([6, 2, 6, 4, 5, 6]),
>  Transformation([6, 2, 3, 4, 6, 6])]);;
gap> x := Transformation([1, 6, 3, 6, 6, 6]);;
gap> y := Transformation([6, 6, 3, 6, 6, 6]);;
gap> IsIdempotent(x) and IsIdempotent(y) and x in T and y in T;
true
gap> I := IdempotentGeneratedSubsemigroup(T);;
gap> IsGreensDGreaterThanFunc(I)(x, y);
true
gap> T := Semigroup(T);;
gap> I := IdempotentGeneratedSubsemigroup(T);;
gap> IsInverseSemigroup(I);
true
gap> IsGreensDGreaterThanFunc(I)(x, y);
true

# Test Issue #237 part 2
gap> T := Semigroup([
>  Transformation([1, 2, 6, 4, 5, 6]),
>  Transformation([1, 2, 3, 4, 6, 6]),
>  Transformation([1, 2, 3, 6, 5, 6]),
>  Transformation([1, 6, 6, 4, 5, 6]),
>  Transformation([1, 6, 3, 4, 6, 6]),
>  Transformation([1, 6, 3, 6, 5, 6]),
>  Transformation([6, 2, 3, 6, 5, 6]),
>  Transformation([6, 2, 6, 4, 5, 6]),
>  Transformation([6, 2, 3, 4, 6, 6])]);;
gap> Set(PrimitiveIdempotents(T));
[ Transformation( [ 1, 6, 6, 6, 6, 6 ] ), 
  Transformation( [ 6, 2, 6, 6, 6, 6 ] ), 
  Transformation( [ 6, 6, 3, 6, 6, 6 ] ), 
  Transformation( [ 6, 6, 6, 4, 6, 6 ] ), 
  Transformation( [ 6, 6, 6, 6, 5, 6 ] ) ]

# Issue 253: IsIdempotentGenerated

# Problem with IsIdempotentGenerated for ideals
gap> S := Semigroup([Transformation([3, 2, 1]), Transformation([2, 2, 2])]);
<transformation semigroup of degree 3 with 2 generators>
gap> IsIdempotentGenerated(S);
false
gap> I := SemigroupIdeal(S, S.1 ^ 2);;
gap> IsIdempotentGenerated(I);
false
gap> I = S;
true

# Problem with IsIdempotentGenerated for Rees 0-matrix semigroups over
# non-groups
gap> mat := [[
>  Transformation([2, 3, 1]),
>  Transformation([2, 1]),
>  Transformation([1, 2, 1])]];;
gap> R := ReesZeroMatrixSemigroup(FullTransformationMonoid(3), mat);
<Rees 0-matrix semigroup 3x1 over <full transformation monoid of degree 3>>
gap> IsIdempotentGenerated(R);
false

# Issue #191: part 1 of semicong.tst (congruences on fp semigroups)
gap> n := 3;;
gap> f := FreeSemigroup(n);;
gap> gns := GeneratorsOfSemigroup(f);;
gap> rel := [];;
gap> x := 0;;
gap> for x in [1 .. Length(gns) - 1] do
>   Append(rel, List(gns, y -> [gns[x] * y, y * gns[x]]));
>   Add(rel, [gns[x] ^ (x + 1), gns[x]]);
>   Add(rel, [gns[x] * Last(gns), gns[x]]);
>   Add(rel, [Last(gns) * gns[x], gns[x]]);
> od;
gap> s := f / rel;;
gap> sgns := GeneratorsOfSemigroup(s);;
gap> c := SemigroupCongruenceByGeneratingPairs(s, [[sgns[1], sgns[2]]]);;
gap> EquivalenceRelationPartition(c);
[ [ s1, s2, s1*s2, s2^2, s1*s2^2 ] ]
gap> ##
gap> ## Check to see if elements are in the partition
gap> ##     true and false
gap> ##
gap> ec := EquivalenceClassOfElement(c, sgns[n]);;
gap> Size(ec);
1
gap> ec := EquivalenceClassOfElement(c, sgns[n - 1]);;
gap> sgns[n] in ec;
false
gap> Size(ec);
5

# Infinite congruence classes: part 2 of semicong.tst (fp semigroups)
gap> f := FreeSemigroup(2);;
gap> s := f / [[f.1 ^ 3, f.1], [f.1 * f.2, f.1], [f.2 * f.1, f.1]];;
gap> gns := GeneratorsOfSemigroup(s);;
gap> c := SemigroupCongruenceByGeneratingPairs(s, [[gns[1], gns[1] ^ 2],
>                                                  [gns[2], gns[2] ^ 2]]);;
gap> ec := EquivalenceClassOfElement(c, gns[2]);;
gap> gns[2] ^ 20 in ec;
true
gap> gns[2] ^ 40 in ec;
true
gap> ##
gap> ## We should never get a full closure
gap> ##
gap> HasEquivalenceRelationPartition(c);
false

# Issue 286: Bug in IsSynchronizingSemigroup
gap> S := FullTransformationMonoid(10);
<full transformation monoid of degree 10>
gap> IsSynchronizingSemigroup(S);
true

# Free semigroup congruence test from extreme/cong.tst
gap> F := FreeSemigroup(1);;
gap> x := GeneratorsOfSemigroup(F)[1];;
gap> pair := [x ^ 2, x ^ 4];;
gap> cong := SemigroupCongruence(F, pair);
<2-sided semigroup congruence over <free semigroup on the generators 
[ s1 ]> with 1 generating pairs>
gap> pair in cong;
true
gap> EquivalenceRelationLookup(cong);
Error, the argument (a 2-sided congruence) must have finite range
gap> EquivalenceClasses(cong);
[ <2-sided congruence class of s1>, <2-sided congruence class of s1^2>, 
  <2-sided congruence class of s1^3> ]
gap> NrEquivalenceClasses(cong);
3

# Issue 300: problem with InverseOfSemigroupElement for an acting monoid
gap> S := Monoid(Transformation([1, 2, 4, 4]),
>                Transformation([4, 2, 3, 4]),
>                Transformation([1, 4, 3, 4]));;
gap> IsEUnitaryInverseSemigroup(S);
true

# Issue 167: IsomorphismSemigroups, for an RMS where an arg is not WholeFamily
gap> R := ReesMatrixSemigroup(Group(()), [[(), ()], [(), ()]]);;
gap> W := Semigroup(RMSElement(R, 2, (), 2));;
gap> S := ReesMatrixSemigroup(Group(()), [[()]]);;
gap> IsReesMatrixSemigroup(W);
true
gap> IsomorphismSemigroups(S, S);
((), IdentityMapping( Group( [ () ] ) ), [ (), () ])
gap> IsomorphismSemigroups(W, W) = IdentityMapping(W);
true
gap> IsomorphismSemigroups(W, S);
CompositionMapping( ((), GroupHomomorphismByImages( Group( [ () ] ), Group( 
[ () ] ), [  ], [  ] ), [ (), () ]), 
<Rees matrix semigroup 1x1 over Group(())> -> 
<Rees matrix semigroup 1x1 over Group(())> )

# Issue 363: MultiplicativeNeutralElement, for an ideal
gap> S := SingularFactorisableDualSymmetricInverseMonoid(3);
<inverse bipartition semigroup ideal of degree 3 with 1 generator>
gap> IsMonoidAsSemigroup(S);
false

# Issue 377: IsInverseSemigroup
gap> S := Semigroup([
>   Transformation([1, 1, 1, 2, 1, 5, 3]),
>   Transformation([1, 4, 4, 4, 6, 6, 6])],
> rec(acting := false));;
gap> IsInverseSemigroup(S);
false
gap> S := Semigroup([
>  PBR([[-1], [-1], [-3], [-1], [-3], [-3], [-2]],
>      [[1, 2, 4], [7], [3, 5, 6], [], [], [], []]),
>  PBR([[-1], [-2], [-3], [-4], [-5], [-5], [-4]],
>      [[1], [2], [3], [4, 7], [5, 6], [], []]),
>  PBR([[-1], [-2], [-3], [-6], [-6], [-6], [-6]],
>      [[1], [2], [3], [], [], [4, 5, 6, 7], []])]);;
gap> IsInverseSemigroup(S);
false

# Issue 371: GeneratorsOfSemigroup for a monoid
gap> R := ReesMatrixSemigroup(Group(()), [[()]]);;
gap> S := MonoidByAdjoiningIdentity(R);
<commutative monoid with 1 generator>
gap> GeneratorsOfSemigroup(S);
[ ONE, (1,(),1) ]
gap> Size(S);
2
gap> Elements(S);
[ ONE, (1,(),1) ]

# Issue 393: StructureDescription method in Semigroups inappropriately selected
gap> F := FreeGroup("r", "s");;
gap> r := F.1;;
gap> s := F.2;;
gap> G := F / [s * r * s ^ (- 1) * r ^ (- 1)];;
gap> StructureDescription(G) in ["C0 x C0", "Z x Z"];
true

# Issue 389: NaturalPartialOrder
gap> S := Semigroup(SymmetricInverseMonoid(3), rec(acting := true));;
gap> es := IdempotentGeneratedSubsemigroup(S);;
gap> NaturalPartialOrder(es);
[ [  ], [ 1 ], [ 1 ], [ 1, 2, 3 ], [ 1 ], [ 1, 3, 5 ], [ 1, 2, 5 ], 
  [ 1, 2, 3, 4, 5, 6, 7 ] ]
gap> es := AsSemigroup(IsBlockBijectionSemigroup, es);;
gap> NaturalPartialOrder(es);
[ [  ], [ 1 ], [ 1 ], [ 1 ], [ 1, 2, 3 ], [ 1, 2, 4 ], [ 1, 3, 4 ], 
  [ 1, 2, 3, 4, 5, 6, 7 ] ]

# Issue 251
gap> S := Monoid([
> Transformation([2, 1]),
> Transformation([3, 1, 2]),
> Transformation([4, 2, 3, 4]),
> Transformation([2, 2])]);;
gap> slist := ShallowCopy(AsList(S));;
gap> Sort(slist);
gap> r := MultiplicativeNeutralElement(S);;
gap> for s in S do
> if not r * s in slist then
> Print(s);
> fi;
> od;

# Issue 435 (inappropriate elements added during ClosureMonoid)
gap> S := InverseMonoid(PartialPerm([1, 4], [1, 4]),
>                       PartialPerm([1, 3, 4, 5], [1, 6, 4, 2]),
>                       PartialPerm([1, 2, 4, 6], [1, 5, 4, 3]),
>                       PartialPerm([1, 4], [4, 1]),
>                       PartialPerm([1, 3, 4, 5], [1, 3, 4, 5]),
>                       PartialPerm([1, 2, 4, 6], [1, 2, 4, 6]),
>                       PartialPerm([1, 2, 3, 4, 5, 6, 7],
>                                   [1, 2, 3, 4, 5, 6, 7]));
<inverse partial perm monoid of rank 7 with 7 generators>
gap> I := IdempotentGeneratedSubsemigroup(S);
<inverse partial perm monoid of rank 7 with 3 generators>
gap> I = Semigroup(Idempotents(S));
true
gap> Size(I);
4

# Issue 459: IsTrivial method broken for semigroups with 0 generators
gap> M0 := Magma(FamilyObj([1]), []);;
gap> IsTrivial(M0);
false
gap> Size(M0);
0

# Issue 461: NrEquivalenceClasses gives incorrect answer
gap> tab := [[1, 2, 3, 3], [2, 3, 1, 1], [3, 1, 2, 2], [3, 1, 2, 2]];;
gap> S := SemigroupByMultiplicationTable(tab);;
gap> cong := SemigroupCongruence(S, [[S.3, S.4]]);;
gap> S.1 in EquivalenceClassOfElement(cong, S.3);
false
gap> NrEquivalenceClasses(cong);
3
gap> S.1 in EquivalenceClassOfElement(cong, S.3);
false
gap> IsUniversalSemigroupCongruence(cong);
false

# Issues 676-677
gap> S := FreeBand(2);;
gap> cong := SemigroupCongruence(S, []);;
gap> NonTrivialEquivalenceClasses(cong);
[  ]

# Issue 680
gap> F := FreeSemigroup(2);;
gap> s1 := F.1;; s2 := F.2;;
gap> rels := [[s2 * s1 * s2, s2 * s1], [s1, s1], [s2, s2],
>             [s1 * s2, s1 * s2], [s2 * s1, s2 * s1]];;
gap> cong := SemigroupCongruence(F, rels);
<2-sided semigroup congruence over <free semigroup on the generators 
[ s1, s2 ]> with 1 generating pairs>
gap> NrEquivalenceClasses(cong);
infinity
gap> EquivalenceRelationPartitionWithSingletons(cong);
Error, the argument (a congruence) must have finite range
gap> EquivalenceRelationLookup(cong);
Error, the argument (a 2-sided congruence) must have finite range

# Issue 788
gap> S := GLM(2, 2);
<general linear monoid 2x2 over GF(2)>
gap> Matrix(GF(4), One(S));
[ [ Z(2)^0, 0*Z(2) ], [ 0*Z(2), Z(2)^0 ] ]
gap> Size(Elements(S));
16

# Issue 764, InjectionPrincipalFactor can error for D-classes of semigroups of
# matrices over finite fields
gap> S := Semigroup(
> [Matrix(GF(3),
>    [[Z(3), Z(3), Z(3) ^ 0],
>      [0 * Z(3), Z(3), Z(3)],
>      [Z(3), 0 * Z(3), Z(3) ^ 0]]),
>   Matrix(GF(3),
>    [[Z(3), Z(3), 0 * Z(3)],
>      [Z(3) ^ 0, Z(3) ^ 0, 0 * Z(3)],
>      [Z(3) ^ 0, Z(3) ^ 0, 0 * Z(3)]])]);;
gap> InjectionPrincipalFactor(DClass(S, S.1));;

# Issue 800, Quotient semigroup elements are not associative
gap> T := FullTransformationMonoid(2);;
gap> cong := SemigroupCongruence(T, []);;
gap> IsAssociativeElement(Elements(T / cong)[1]);
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: testinstall.tst");
