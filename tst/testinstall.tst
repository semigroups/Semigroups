#############################################################################
##
#W  testinstall.tst
#Y  Copyright (C) 2011-15                               James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: testinstall.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SemigroupsStartTest();

#T# TestInstall3
gap> S := Semigroup(Transformation([2, 3, 4, 1, 1, 1]));;
gap> IsMonoidAsSemigroup(S);
true
gap> IsMonoid(S);
false
gap> iso := IsomorphismTransformationMonoid(S);;
gap> Range(iso);
<commutative transformation monoid on 4 pts with 1 generator>
gap> RespectsMultiplication(iso);
true
gap> ForAll(S, x -> (x ^ iso) ^ InverseGeneralMapping(iso) = x);
true

#T# TestInstall4
gap> S := Semigroup(Transformation([1, 1, 1]), Transformation([3, 1, 2]));
<transformation semigroup on 3 pts with 2 generators>
gap> IsSimpleSemigroup(S);
false

#T# TestInstall5
gap> S := SingularTransformationSemigroup(6);
<regular transformation semigroup ideal on 6 pts with 1 generator>
gap> Size(S);
45936

#T# TestInstall6
gap> S := Semigroup(IdentityTransformation, rec(generic := false));;
gap> LambdaOrb(S);
<open orbit, 1 points with Schreier tree with log>
gap> Enumerate(last);
<closed orbit, 2 points with Schreier tree with log>

#T# TestInstall7
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
<transformation semigroup on 4 pts with 3 generators>
gap> ForAll(T, x -> x in S);
true
gap> Size(T);
60

#T# TestInstall8: Issue 2
gap> S := Semigroup(Transformation([4, 4, 4, 4]));;
gap> AsList(S);
[ Transformation( [ 4, 4, 4, 4 ] ) ]

#T# TestInstall9: Issue 3
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

#T# TestInstall10: Issue 9
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

#T# TestInstall12
gap> gens := [Transformation([1, 2, 3, 5, 4, 6, 7, 8]),
>             Transformation([4, 4, 3, 1, 5, 6, 3, 8]),
>             Transformation([3, 6, 1, 7, 3, 4, 8, 3]),
>             Transformation([1, 2, 3, 4, 5, 3, 7, 8]),
>             Transformation([1, 2, 3, 4, 1, 6, 7, 8]),
>             Transformation([8, 8, 3, 4, 5, 7, 6, 1])];;
gap> s := Monoid(gens);
<transformation monoid on 8 pts with 6 generators>
gap> t := ClosureSemigroup(s, [Transformation([4, 4, 3, 1, 5, 6, 3, 8])]);
<transformation monoid on 8 pts with 6 generators>
gap> Size(t) = Size(Semigroup(Generators(t)));
true

#T# TestInstall13
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
>                   rec(generic := false));;
gap> x := Transformation([7, 7, 4, 2, 1, 8, 8, 9, 5]);;
gap> D := DClass(S, Transformation([1, 8, 6, 2, 7, 8, 8, 9, 5]));;
gap> L := LClass(D, x);
<Green's L-class: Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] )>
gap> LL := LClass(S, x);
<Green's L-class: Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] )>
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

#T# TestInstall14: IsomorphismTransformationSemigroup/Monoid
gap> G := Group([(5, 9), (1, 7)]);;
gap> IsomorphismTransformationSemigroup(G);;
gap> S := Range(last);
<transformation semigroup on 4 pts with 2 generators>
gap> IsGroupAsSemigroup(S);
true
gap> Generators(S);
[ Transformation( [ 1, 4, 3, 2 ] ), Transformation( [ 3, 2, 1 ] ) ]
gap> T := Range(IsomorphismTransformationMonoid(G));
<transformation monoid on 4 pts with 2 generators>
gap> Generators(T);
[ Transformation( [ 1, 4, 3, 2 ] ), Transformation( [ 3, 2, 1 ] ) ]
gap> H := Range(IsomorphismPermGroup(T));
Group([ (), (2,4), (1,3) ])
gap> IsomorphismGroups(G, H);
[ (5,9), (1,7) ] -> [ (2,4), (1,3) ]

#T# TestInstall15: Issue 22 - takes about 49ms
gap> x := Transformation([2, 12, 10, 7, 6, 11, 8, 3, 4, 5, 1, 11]);;
gap> S := FullTransformationSemigroup(12);;
gap> S := Semigroup(S, rec(generic := false, regular := true));;
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

#T# TestInstall16
gap> file := Concatenation(SemigroupsDir(), "/tst/standard/test.gz");;
gap>  ReadGenerators(file, 1376);
[ <identity partial perm on [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]>, 
  <identity partial perm on [ 1, 2, 3, 4, 5, 6, 7, 9 ]>, 
  <identity partial perm on [ 1, 2, 3, 4, 5, 6, 9 ]>, 
  <identity partial perm on [ 1, 2, 3, 4, 5, 9 ]>, 
  <identity partial perm on [ 1, 2, 3, 4, 9 ]>, 
  <identity partial perm on [ 1, 2, 3, 9 ]>, 
  <identity partial perm on [ 1, 2, 9 ]>, <identity partial perm on [ 1, 9 ]> 
 ]

#T# TestInstall17
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
<inverse partial perm semigroup on 4 pts with 3 generators>
gap> ForAll(T, x -> x in S);
true
gap> Size(T);
98

#T# TestInstall18
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

#T# TestInstall18a
gap> S := InverseSemigroup(PartialPermNC([1, 3, 5, 6, 7], [9, 1, 5, 3, 8]),
>                          PartialPermNC([1, 2, 3, 5, 6, 7, 9, 10],
>                                        [4, 10, 5, 6, 7, 1, 3, 2]));;
gap> D := DClass(S, PartialPerm([2, 10], [2, 10]));
<Green's D-class: <identity partial perm on [ 2, 10 ]>>
gap> F := IsomorphismReesMatrixSemigroup(D);;
gap> G := InverseGeneralMapping(F);;
gap> ForAll(D, x -> (x ^ F) ^ G = x);
true

#T# TestInstall19: from JS' MultiplicativeZero.tst
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

#T# TestInstall20: from JS' PartialPermInjective.tst
gap> PartialPerm([0, 0, 1, 2]);
[3,1][4,2]

#T# TestInstall21: from JS' RestricterPartialPerm.tst
gap> x := PartialPerm([2 .. 7], [1 .. 6]);
> RestrictedPartialPerm(x, [2 .. 7]);
[7,6,5,4,3,2,1]
[7,6,5,4,3,2,1]

#T# TestInstall22: from JS' SizeInverseMonoid.tst
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

#T# TestInstall23: from JS' email
gap> S := InverseMonoid(PartialPerm([1, 3, 2]), PartialPerm([1]));;
gap> [Size(S), Size(AsSet(S))];
[ 3, 3 ]
gap> Elements(S);
[ <identity partial perm on [ 1 ]>, <identity partial perm on [ 1, 2, 3 ]>, 
  (1)(2,3) ]

#T# TestInstall24
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

#T# TestInstall25: Issue 27 in the new numbering...
gap> S := Semigroup(List(GeneratorsOfSemigroup(FullTransformationSemigroup(3)),
>  x -> AsTransformation(x, 4)));;
gap> IsFullTransformationSemigroup(S);
true
gap> IdentityTransformation in S;
true

#T# TestInstall26: Issue 23 in the new numbering...
gap> S := FullTransformationSemigroup(3);;
gap> x := Transformation([4, 3, 1, 2]);;
gap> ClosureSemigroup(S, x);
<transformation monoid on 4 pts with 4 generators>

#T# TestInstall27: Issue 36 in the new numbering...
gap> S := Semigroup(IdentityTransformation);
<trivial transformation group>
gap> SmallGeneratingSet(S);
[  ]

#T# TestInstall28: MaximalSubsemigroups of Rees 0-matrix semigroups
gap> G := Group((1, 2), (3, 4));;
gap> mat := [[(), ()], [(), 0], [(), (1, 2)]];;
gap> R := ReesZeroMatrixSemigroup(G, mat);
<Rees 0-matrix semigroup 2x3 over Group([ (1,2), (3,4) ])>
gap> (IsBound(GAPInfo.PackagesLoaded.grape)
> and Filename(DirectoriesPackagePrograms("grape"), "dreadnautB") <> fail
> and IsDuplicateFreeList(MaximalSubsemigroups(R))
> and ForAll(MaximalSubsemigroups(R), U -> IsMaximalSubsemigroup(R, U))
> and Length(MaximalSubsemigroups(R)) = 6)
> or (not (IsBound(GAPInfo.PackagesLoaded.grape)
> and Filename(DirectoriesPackagePrograms("grape"), "dreadnautB") <> fail));
true

#T# TestInstall29: ClosureSemigroup with an element of higher degree
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

#T# TestInstall30: bug with Elements and IsomorphismPermGroup for group H-class
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

#T# TestInstall31: Issue 47: bug in ClosureSemigroup caused which assumed
# that if the rank of an R-class rep was greater than the maximum rank of the
# collection being added, then we hadn't seen an R-class rep with the same
# rho-value before.
gap> S := Semigroup([Transformation([1, 2, 4, 6, 1, 6]),
> Transformation([1, 6, 1, 1, 6, 5]),
> Transformation([2, 6, 2, 4, 3, 2]),
> Transformation([4, 1, 3, 6, 1, 5]),
> Transformation([4, 1, 4, 2, 4, 2]),
> Transformation([6, 6, 4, 6, 1, 1])]);
<transformation semigroup on 6 pts with 6 generators>
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
<transformation semigroup on 6 pts with 12 generators>
gap> IsMaximalSubsemigroup(S, T);
true
gap> T := Semigroup(T, rec(small := true));;
gap> IsMaximalSubsemigroup(S, T);
true

#T# TestInstall32: From Jack Schmidt 06/02/14 by email
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

#T# TestInstall33: From Jack Schmidt 07/02/14 by email
gap> AsSet(InverseMonoid(PartialPerm([1, 2]), PartialPerm([1])));
[ <identity partial perm on [ 1 ]>, <identity partial perm on [ 1, 2 ]> ]

#T# TestInstall34: Issue #57 (problem in INV_KER_TRANS)
gap> S := Semigroup(Transformation([1, 1, 1]),
>                   Transformation([1, 1, 4, 4, 5]));;
gap> Size(S);
2
gap> IsMonogenicSemigroup(S);
false

#T# TestInstall35: Issue pointed out by WAW caused by
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

#T# TestInstall36: Issue pointed out by WAW, caused by typo in ClosureSemigroup
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

#T# TestInstall37: Issue #63 (problem with Monoid and InverseMonoid when one
# of the arguments is a monoid).
# This only works in GAP 4.7.5 or higher hence the CompareVersionNumbers
gap> S := Semigroup(PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]));
<trivial partial perm group on 5 pts with 0 generators>
gap> T := Monoid(S, PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3]));;
gap> Length(GeneratorsOfMonoid(T)) = 2
> or not CompareVersionNumbers(GAPInfo.Version, "4.7.5");
true
gap> One(S) in T or not CompareVersionNumbers(GAPInfo.Version, "4.7.5");
true
gap> One(S) = One(T);
false
gap> GeneratorsOfSemigroup(T) =
> [PartialPerm([1, 2, 3, 4, 5, 6]),
>  PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]),
>  PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3])]
> or not CompareVersionNumbers(GAPInfo.Version, "4.7.5");
true
gap> GeneratorsOfMonoid(T) =
> [PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]),
>  PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3])]
> or not CompareVersionNumbers(GAPInfo.Version, "4.7.5");
true
gap> S := InverseSemigroup(PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]));;
gap> T := InverseMonoid(S, PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3]));;
gap> Length(GeneratorsOfInverseMonoid(T)) = 2
> or not CompareVersionNumbers(GAPInfo.Version, "4.7.5");
true
gap> GeneratorsOfMonoid(T) =
> [PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]),
>  PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3]),
>  PartialPerm([1, 2, 3, 4, 5], [4, 1, 6, 3, 2])]
> or not CompareVersionNumbers(GAPInfo.Version, "4.7.5");
true
gap> GeneratorsOfSemigroup(T) =
> [PartialPerm([1, 2, 3, 4, 5, 6], [1, 2, 3, 4, 5, 6]),
>  PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]),
>  PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3]),
>  PartialPerm([1, 2, 3, 4, 5], [4, 1, 6, 3, 2])]
>  or not CompareVersionNumbers(GAPInfo.Version, "4.7.5");
true
gap> GeneratorsOfInverseMonoid(T) =
> [PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]),
>   PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3])]
>   or not CompareVersionNumbers(GAPInfo.Version, "4.7.5");
true
gap> GeneratorsOfInverseSemigroup(T) =
> [PartialPerm([1, 2, 4, 5, 6], [1, 2, 4, 5, 6]),
>   PartialPerm([1, 2, 3, 4, 6], [2, 5, 4, 1, 3]),
>   PartialPerm([1, 2, 3, 4, 5, 6], [1, 2, 3, 4, 5, 6])]
>   or not CompareVersionNumbers(GAPInfo.Version, "4.7.5");
true
gap> One(S) in T or not CompareVersionNumbers(GAPInfo.Version, "4.7.5");
true

#T# TestInstall38: Issue 33 (problem with Rees factor semigroups)
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
{Transformation( [ 1, 2, 2, 2 ] )}
gap> t := Image(hom, Transformation([2, 1, 2, 3]));
{Transformation( [ 2, 1, 2, 3 ] )}
gap> u * t;
{Transformation( [ 1, 2, 2, 2 ] )}
gap> t * u;
{Transformation( [ 1, 2, 2, 2 ] )}
gap> S := Semigroup(u, t);
<semigroup with 2 generators>
gap> Size(S);
2

#T# TestInstall39: Issue 56
# (Monoid/InverseMonoid removes One inappropriately sometimes)
gap> M := InverseMonoid(PartialPerm([1, 2]), PartialPerm([1]));
<commutative inverse partial perm monoid on 2 pts with 1 generator>
gap> One(M) in M;
true
gap> AsSet(M);
[ <identity partial perm on [ 1 ]>, <identity partial perm on [ 1, 2 ]> ]
gap> M := InverseMonoid(PartialPerm([1, 2]), PartialPerm([1]));
<commutative inverse partial perm monoid on 2 pts with 1 generator>
gap> AsSet(M);
[ <identity partial perm on [ 1 ]>, <identity partial perm on [ 1, 2 ]> ]

#T# TestInstall40: Issue 4 in Orb (logs are not properly updated if the
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

#T# TestInstall41: Issue 72
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
<transformation monoid on 6 pts with 3 generators>
gap> Size(T);
16
gap> IsMonoid(T);
true

#T# TestInstall42: Issue 82 (couldn't take quotients by ideals!)
gap> S := Monoid(
>  Transformation([3, 3, 3, 3]), Transformation([2, 4, 2, 4]),
>  Transformation([2, 3, 2, 3]), Transformation([4, 1, 4, 3]),
>  Transformation([1, 4, 4, 1]), Transformation([2, 2, 3, 1]),
>  Transformation([2, 4, 3, 4]), Transformation([2, 2, 1, 2]),
>  Transformation([2, 2, 1, 3]), Transformation([1, 2, 2, 3]),
>  Transformation([2, 4, 3, 2]), Transformation([2, 3, 3, 3]));;
gap> I := SemigroupIdeal(S, S.3);;
gap> S / I;
<quotient of Monoid( [ Transformation( [ 3, 3, 3, 3 ] ), 
  Transformation( [ 2, 4, 2, 4 ] ), Transformation( [ 2, 3, 2, 3 ] ), 
  Transformation( [ 4, 1, 4, 3 ] ), Transformation( [ 1, 4, 4, 1 ] ), 
  Transformation( [ 2, 2, 3, 1 ] ), Transformation( [ 2, 4, 3, 4 ] ), 
  Transformation( [ 2, 2, 1, 2 ] ), Transformation( [ 2, 2, 1, 3 ] ), 
  Transformation( [ 1, 2, 2, 3 ] ), Transformation( [ 2, 4, 3, 2 ] ), 
  Transformation( [ 2, 3, 3, 3 ] ) ] ) by ReesCongruenceOfSemigroupIdeal( 
SemigroupIdeal( 
 Monoid( 
  [ Transformation( [ 3, 3, 3, 3 ] ), Transformation( [ 2, 4, 2, 4 ] ), Transf\
ormation( [ 2, 3, 2, 3 ] ), Transformation( [ 4, 1, 4, 3 ] ), Transformation( \
[ 1, 4, 4, 1 ] ), Transformation( [ 2, 2, 3, 1 ] ), Transformation( [ 2, 4, 3,\
 4 ] ), Transformation( [ 2, 2, 1, 2 ] ), Transformation( [ 2, 2, 1, 3 ] ), Tr\
ansformation( [ 1, 2, 2, 3 ] ), Transformation( [ 2, 4, 3, 2 ] ), Transformati\
on( [ 2, 3, 3, 3 ] ) ] ), [ Transformation( [ 2, 3, 2, 3 ] ) ] ) )>

#T# TestInstall43: Issue 89
gap> S := Semigroup(Transformation([2, 1, 3, 1, 4, 3]),
>                   Transformation([2, 2, 2, 2, 1, 2]),
>                   Transformation([5, 3, 4, 3, 5]),
>                   Transformation([6, 4, 1, 4, 5, 3]),
>                   Transformation([6, 5, 2, 6, 3, 4]));;
gap> NrIdempotents(S) = Number(HClasses(S), IsGroupHClass)
> or not CompareVersionNumbers(GAPInfo.Version, "4.7.5");
true

#T# TestInstall44: Issue 96 (problem with using the partial order of D-classes
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
gap> (not (IsBound(GAPInfo.PackagesLoaded.grape)
> and Filename(DirectoriesPackagePrograms("grape"), "dreadnautB") <> fail)) or
> (IsBound(GAPInfo.PackagesLoaded.grape)
> and Filename(DirectoriesPackagePrograms("grape"), "dreadnautB") <> fail
> and IsIsomorphicSemigroup(S, T));
true

#T# TestInstall45: Issue 97
# (bug in normalizer and the kernel function POW_KER_TRANS)
gap> if CompareVersionNumbers(GAPInfo.Version, "4.7.6") then
>   G := Normalizer(SymmetricGroup(3), Semigroup(IdentityTransformation,
>                                                rec(generic := false)));
> else
>   G := SymmetricGroup(3);
> fi;
gap> G;
Sym( [ 1 .. 3 ] )

#T# TestInstall46: Issue 98
# (incorrect definition of partition monoid on 1 point)
gap> GeneratorsOfSemigroup(PartitionMonoid(1));
[ <block bijection: [ 1, -1 ]>, <bipartition: [ 1 ], [ -1 ]> ]

#T# TestInstall47: Issue 101 (incorrect method for
# DegreeOfTransformationSemigroup for a transformation group with 0 generators)
gap> if CompareVersionNumbers(GAPInfo.Version, "4.7.6") then
>   G := GroupOfUnits(FullTransformationSemigroup(1));
> else
>   G := Semigroup(IdentityTransformation);
> fi;
gap> G;
<trivial transformation group>

#T# TestInstall48: Issue 101
# (incorrect method for AsPartialPerm for a perm and zero)
gap> if CompareVersionNumbers(GAPInfo.Version, "4.7.6") then
>   G := GroupOfUnits(Semigroup(PartialPerm([])));
> else
>   G := Semigroup(PartialPerm([]));
> fi;
gap> G;
<trivial partial perm group on 0 pts with 0 generators>

#T# TestInstall49: Issue 103
# (problem with Enumerate(LambdaOrb(I)) when T is an inverse semigroup but
# doesn't know it at the start)
gap> S := POI(5);;
gap> T := Semigroup(S, PartialPerm([1, 2, 3, 4, 5], [2, 3, 4, 5, 1]));;
gap> I := SemigroupIdeal(T, [PartialPerm([1, 2, 4, 5], [1, 2, 3, 5])]);;
gap> IsInverseSemigroup(I);
true
gap> Size(I);
626

#T# TestInstall50: Issue 105 (CyclesOfPartialPerm returned nonsense)
gap> x := PartialPerm([1, 2, 3, 4, 5, 8, 10], [3, 1, 4, 2, 5, 6, 7]);;
gap> CyclesOfPartialPerm(x);
[ [ 3, 4, 2, 1 ], [ 5 ] ]

#T# TestInstall51: Issue 107
# (problems with Green's classes of ideals, and inverse semigroups)
gap> S := Monoid(PartialPermNC([1], [1]),
>                PartialPermNC([1], [2]),
>                PartialPermNC([2], [1]));;
gap> I := SemigroupIdeal(S, PartialPermNC([], []));;
gap> GeneratorsOfSemigroup(I);
[ <empty partial perm> ]

#T# TestInstall52: Issue 107 (problems with Green's classes of ideals, and
# inverse semigroups)
gap> S := [SymmetricInverseMonoid(2)];;
gap> S[2] := MaximalSubsemigroups(S[1]);;
gap> if CompareVersionNumbers(GAPInfo.Version, "4.7.7")
>        and (IsBound(GAPInfo.PackagesLoaded.grape)
>        and Filename(DirectoriesPackagePrograms("grape"),
>                     "dreadnautB") <> fail) then
>      S[3] := List(S[2], MaximalSubsemigroups);;
>    fi;

#T# TestInstall53: Issue 109 (problem with IsReesZeroMatrixSemigroup on the
# subsemigroup generated by 0)
gap> R1 := ReesZeroMatrixSemigroup(Group(()), [[()]]);;
gap> R2 := Semigroup(MultiplicativeZero(R1));;
gap> IsReesZeroMatrixSubsemigroup(R2);
true
gap> (CompareVersionNumbers(GAPInfo.Version, "4.7.7") and
> IsReesZeroMatrixSemigroup(R2));
false

#T# TestInstall54: FreeBand
gap> S := FreeBand("a", "b", "c", "d", "e");
<free band on the generators [ a, b, c, d, e ]>
gap> iter := Iterator(S);
<iterator>
gap> for i in [1 .. 100] do
>      NextIterator(iter);
>    od;
gap> x := NextIterator(iter);
bcabaca
gap> for i in [1 .. 10] do
>      NextIterator(iter);
>    od;
gap> y := NextIterator(iter);
cbcacbab
gap> x * y;
bcacbab
gap> x ^ 2;
bcabaca
gap> y ^ 2;
cbcacbab

#T# TestInstall55: Issue 110 (MaximalSubsemigroups for an non-regular RZMS)
gap> S := ReesZeroMatrixSemigroup(Group(()), [[(), 0], [0, ()]]);;
gap> S := Semigroup(RMSElement(S, 2, (), 2),
>                   RMSElement(S, 1, (), 2));;
gap> (IsBound(GAPInfo.PackagesLoaded.grape)
> and Filename(DirectoriesPackagePrograms("grape"), "dreadnautB") <> fail
> and IsDuplicateFreeList(MaximalSubsemigroups(S))
> and ForAll(MaximalSubsemigroups(S), x -> IsMaximalSubsemigroup(S, x))
> and Length(MaximalSubsemigroups(S)) = 2)
> or (not (IsBound(GAPInfo.PackagesLoaded.grape)
> and Filename(DirectoriesPackagePrograms("grape"), "dreadnautB") <> fail));
true

#T# TestInstall56: Issue 122 (Problem with XClassType for inverse ideals)
gap> S := Semigroup(
> PartialPerm([1, 2, 3, 4], [2, 3, 4, 1]),
> PartialPerm([1, 2, 3, 4], [2, 1, 3, 4]),
> PartialPerm([1, 3], [2, 3]), rec(generic := false));;
gap> x := PartialPerm([], []);;
gap> I := SemigroupIdeal(S, x);;
gap> L := GreensLClassOfElement(I, x);
<Green's L-class: <empty partial perm>>
gap> SchutzenbergerGroup(L);
Group(())

#T# TestInstall57: Issue 123 (Incorrect method for IsZeroSemigroup for
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

#T# TestInstall58: Issue 121:
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

#T# TestInstall59: Issue 88:
# Something called by `JoinIrreducibleDClasses` of an acting semigroup ideal
# of a bipartition semigroup calls `GeneratorsOfSemigroup`
gap> S := InverseMonoid(DualSymmetricInverseMonoid(6), rec(generic := false));;
gap> x := Bipartition([[1, 2, -3], [3, -1, -2], [4, -4],
> [5, -5], [6, -6]]);;
gap> I := SemigroupIdeal(S, x);
<inverse bipartition semigroup ideal on 6 pts with 1 generator>
gap> JoinIrreducibleDClasses(I);
[ <Green's D-class: <block bijection: [ 1, 2, 3, 4, 5, -1, -2, -3, -4, -5 ], 
      [ 6, -6 ]>> ]
gap> I;
<inverse bipartition semigroup ideal on 6 pts with 1 generator>
gap> S := InverseMonoid(DualSymmetricInverseMonoid(3));;
gap> x := Bipartition([[1, 2, -1, -2], [3, -3]]);;
gap> I := SemigroupIdeal(S, x);
<inverse bipartition semigroup ideal on 3 pts with 1 generator>
gap> JoinIrreducibleDClasses(I);
[ <Green's D-class: <block bijection: [ 1, 2, -1, -2 ], [ 3, -3 ]>> ]

#T# TestInstall60: Issue 94:
# EquivalenceClasses of trivial congruence returns empty list
gap> S := FullTransformationSemigroup(6);;
gap> R := PrincipalFactor(MinimalDClass(S));
<Rees matrix semigroup 1x6 over Group(())>
gap> cong := SemigroupCongruenceByGeneratingPairs(R, []);;
gap> EquivalenceClasses(cong);
[ {(1,(),1)}, {(1,(),2)}, {(1,(),3)}, {(1,(),4)}, {(1,(),5)}, {(1,(),6)} ]

#T# TestInstall61: Issue 95:
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
[ {0}, {(2,(),1)}, {(1,(),2)}, {(2,(),2)}, {(1,(),3)}, {(2,(),3)}, 
  {(1,(),4)}, {(2,(),4)}, {(1,(),5)}, {(2,(),5)}, {(1,(),6)}, {(1,(1,3),1)}, 
  {(3,(),1)}, {(4,(),1)}, {(5,(),1)}, {(2,(),6)}, {(3,(),2)}, {(3,(),3)}, 
  {(6,(),1)}, {(3,(),4)}, {(6,(),2)}, {(6,(),3)}, {(3,(),5)}, {(6,(),4)}, 
  {(3,(),6)}, {(6,(),5)}, {(6,(),6)}, {(4,(),2)}, {(4,(),3)}, {(4,(),4)}, 
  {(4,(),5)}, {(7,(),1)}, {(4,(),6)}, {(7,(),2)}, {(7,(),3)}, {(7,(),4)}, 
  {(7,(),5)}, {(7,(),6)}, {(5,(),2)}, {(5,(),3)}, {(5,(),4)}, {(5,(),5)}, 
  {(5,(),6)} ]
gap> ForAny(c, x -> MultiplicativeZero(R) in x);
true

#T# TestInstall62: Issue 119:
# Bug in NrCongruenceClasses for Rees congruences
gap> I := SemigroupIdealByGenerators(FullTransformationSemigroup(4),
> [Transformation([1, 2, 2, 2])]);
<regular transformation semigroup ideal on 4 pts with 1 generator>
gap> cong := ReesCongruenceOfSemigroupIdeal(I);
<Rees congruence of <regular transformation semigroup ideal 
 on 4 pts with 1 generator> over <full transformation semigroup on 4 pts>>
gap> NrCongruenceClasses(cong);
169

#T# TestInstall65: Issue 126:
gap> mat := [
> [ (), 0, 0, 0, 0 ],
> [ 0, (3,4), 0, 0, 0 ],
> [ 0, 0, (), 0, 0 ],
> [ 0, 0, 0, (1,2), 0 ],
> [ 0, 0, 0, 0, (1,2)(3,4) ] ];;
gap> R := ReesZeroMatrixSemigroup(Group([ (1,2), (3,4) ]), mat);;
gap> gens := [
> MultiplicativeZero(R),
> RMSElement(R, 4, (), 4),
> RMSElement(R, 4, (1,2)(3,4), 4),
> RMSElement(R, 5, (1,2)(3,4), 5),
> RMSElement(R, 5, (1,2), 5),
> RMSElement(R, 4, (1,2), 4),
> RMSElement(R, 1, (1,2), 1),
> RMSElement(R, 1, (3,4), 1),
> RMSElement(R, 2, (3,4), 2),
> RMSElement(R, 2, (1,2), 2),
> RMSElement(R, 1, (1,2), 1),
> RMSElement(R, 3, (), 3),
> RMSElement(R, 3, (1,2), 3),
> RMSElement(R, 1, (1,2), 1) ];;
gap> U := Semigroup(gens);;
gap> Filtered(R, x -> x in U);;
gap> x := RMSElement(R, 1, (), 2);;
gap> x in U;
false
gap> IsInverseSemigroup(U);
true
gap> x in U;
false

#T# TestInstall63: Issue 127:
# Bug in Enumerate for an acting semigroup ideal that knows it is regular at its
# point of creation. 
gap> S := Semigroup([[[Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2)], 
>                     [0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2)],
>                     [0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2)], 
>                     [0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0]],
>                    [[Z(2^2), Z(2)^0, Z(2^2), Z(2)^0], 
>                     [Z(2^2)^2, Z(2^2), Z(2^2)^2, Z(2^2)],
>                     [Z(2)^0, Z(2^2)^2, Z(2)^0, Z(2^2)^2],
>                     [Z(2)^0, Z(2^2)^2, Z(2)^0, Z(2^2)^2]],
>                    [[0*Z(2), Z(2^2)^2, 0*Z(2), Z(2)^0], 
>                     [Z(2^2)^2, Z(2)^0, 0*Z(2), Z(2)^0],
>                     [Z(2^2), Z(2)^0, 0*Z(2), 0*Z(2)],
>                     [Z(2^2)^2, Z(2)^0, 0*Z(2), Z(2)^0]]]);
<semigroup with 3 generators>
gap> T := AsTransformationSemigroup(S);
<transformation monoid on 256 pts with 2 generators>
gap> Size(T);
21
gap> I := SemigroupIdeal(T, Idempotents(T));
<regular transformation semigroup ideal on 256 pts with 8 generators>
gap> Size(I);
21

#T# TestInstall64: Bug fixed by changeset 949553d
gap> s := InverseSemigroup(PartialPerm([1], [2]), PartialPerm([2], [1]));
<inverse partial perm semigroup on 2 pts with 2 generators>
gap> Size(s);
5
gap> SemigroupCongruence(s, [s.1, s.1 * s.2]);
<universal semigroup congruence over <0-simple inverse partial perm semigroup 
of size 5, on 2 pts with 2 generators>>

#T# SEMIGROUPS_UnbindVariables
# FIXME redo these!
gap> Unbind(lookingfor);
gap> Unbind(l);
gap> Unbind(L);
gap> Unbind(iter);
gap> Unbind(file);
gap> Unbind(cong);
gap> Unbind(R1);
gap> Unbind(R2);
gap> Unbind(ll);
gap> Unbind(hom);
gap> Unbind(tuples);
gap> Unbind(u);
gap> Unbind(B);
gap> Unbind(mat);
gap> Unbind(G);
gap> Unbind(F);
gap> Unbind(I);
gap> Unbind(H);
gap> Unbind(M);
gap> Unbind(S);
gap> Unbind(R);
gap> Unbind(T);
gap> Unbind(V);
gap> Unbind(d);
gap> Unbind(g);
gap> Unbind(f);
gap> Unbind(i);
gap> Unbind(h);
gap> Unbind(gens);
gap> Unbind(o);
gap> Unbind(s);
gap> Unbind(iso);
gap> Unbind(t);
gap> Unbind(y);
gap> Unbind(x);
gap> Unbind(z);

#E#
gap> STOP_TEST("Semigroups package: testinstall.tst");
