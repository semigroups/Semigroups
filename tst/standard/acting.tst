#############################################################################
##
#W  standard/acting.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/acting.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();
gap> SEMIGROUPS.DefaultOptionsRec.acting := true;;

#T# ActingTest1
gap> s := Semigroup(Transformation([2, 1, 4, 5, 6, 3]),
> Transformation([2, 3, 1, 5, 4, 1]));;
gap> r := GreensRClassOfElement(s,
> Generators(s)[1] * Generators(s)[2] * Generators(s)[1]);
<Green's R-class: Transformation( [ 5, 2, 1, 4, 3, 3 ] )>
gap> Transformation([4, 1, 6, 5, 2, 2]) in r;
true
gap> Representative(r);
Transformation( [ 5, 2, 1, 4, 3, 3 ] )
gap> AsList(LambdaOrb(r)){OrbSCC(LambdaOrb(r))[LambdaOrbSCCIndex(r)]};
[ [ 1, 2, 3, 4, 5 ], [ 1, 2, 4, 5, 6 ], [ 1, 2, 3, 5, 6 ], [ 1, 2, 3, 4, 6 ] ]
gap> LambdaOrbMults(LambdaOrb(r),
> LambdaOrbSCCIndex(r)){OrbSCC(LambdaOrb(r))[LambdaOrbSCCIndex(r)]};
[ [ IdentityTransformation, IdentityTransformation ], 
  [ Transformation( [ 6, 1, 2, 5, 4, 6 ] ), 
      Transformation( [ 2, 3, 1, 5, 4, 1 ] ) ], 
  [ Transformation( [ 1, 2, 5, 6, 3, 6 ] ), 
      Transformation( [ 1, 2, 5, 6, 3, 4 ] ) ], 
  [ Transformation( [ 2, 1, 6, 3, 4, 6 ] ), 
      Transformation( [ 2, 1, 4, 5, 6, 3 ] ) ] ]

#T# ActingTest2
gap> gens := [Transformation([4, 3, 3, 6, 7, 2, 3]),
>   Transformation([6, 6, 4, 4, 2, 1, 4])];;
gap> s := Semigroup(gens);;
gap> Length(GreensRClasses(s));
17
gap> r := GreensRClasses(s)[10];;
gap> Representative(r);
Transformation( [ 3, 3, 3, 3, 3, 2, 3 ] )
gap> AsList(LambdaOrb(r){OrbSCC(LambdaOrb(r))[LambdaOrbSCCIndex(r)]});
[ [ 2, 3 ], [ 4, 6 ], [ 2, 6 ], [ 1, 6 ], [ 2, 4 ], [ 3, 6 ], [ 1, 4 ] ]
gap> LambdaOrbMults(LambdaOrb(r),
> LambdaOrbSCCIndex(r)){OrbSCC(LambdaOrb(r))[LambdaOrbSCCIndex(r)]};
[ [ IdentityTransformation, IdentityTransformation ], 
  [ Transformation( [ 1, 4, 6, 4, 5, 6 ] ), 
      Transformation( [ 6, 3, 3, 2, 3, 3, 3 ] ) ], 
  [ Transformation( [ 1, 6, 2, 4, 5, 6 ] ), 
      Transformation( [ 4, 3, 3, 6, 7, 2, 3 ] ) ], 
  [ Transformation( [ 1, 1, 6, 4, 5, 6 ] ), 
      Transformation( [ 2, 3, 3, 3, 3, 3, 3 ] ) ], 
  [ Transformation( [ 1, 4, 2, 4 ] ), 
      Transformation( [ 6, 3, 3, 2, 3, 3, 3 ] ) ], 
  [ Transformation( [ 1, 6, 3, 4, 5, 6 ] ), 
      Transformation( [ 4, 3, 3, 6, 7, 2, 3 ] ) ], 
  [ Transformation( [ 1, 1, 4, 4 ] ), 
      Transformation( [ 2, 3, 3, 3, 3, 3, 3 ] ) ] ]

#T# ActingTest3
gap> gens := [Transformation([8, 7, 5, 3, 1, 3, 8, 8]),
> Transformation([5, 1, 4, 1, 4, 4, 7, 8])];;
gap> s := Monoid(gens);;
gap> f := Transformation([8, 8, 1, 5, 8, 5, 8, 8]);;
gap> f in SemigroupData(s);
false
gap> iter := IteratorOfRClasses(s);
<iterator of R-classes>
gap> NextIterator(iter);;
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 8, 7, 5, 3, 1, 3, 8, 8 ] )>
gap> f in SemigroupData(s);
false
gap> NextIterator(iter);
<Green's R-class: Transformation( [ 5, 1, 4, 1, 4, 4 ] )>
gap> f in SemigroupData(s);
true

#T# ActingTest4
gap> s := Semigroup([Transformation([2, 4, 1, 2]),
> Transformation([3, 3, 4, 1])]);;
gap> RhoOrb(s);
<open orbit, 1 points with Schreier tree with log>
gap> Enumerate(last);
<closed orbit, 9 points with Schreier tree with log>
gap> AsList(last);
[ [ 0 ], [ 1, 2, 3, 1 ], [ 1, 1, 2, 3 ], [ 1, 2, 2, 1 ], [ 1, 1, 2, 2 ], 
  [ 1, 2, 1, 1 ], [ 1, 1, 1, 2 ], [ 1, 1, 1, 1 ], [ 1, 1, 2, 1 ] ]

#T# acting: \in, wrong family relation 1/2
gap> S := Monoid([Transformation([1, 3, 1]),
> Transformation([2, 1, 3, 1]),
> Transformation([3, 1, 2, 1]),
> Transformation([3, 1, 4, 2]),
> Transformation([4, 4, 4, 4])]);;
gap> Bipartition([[1], [2, -1], [3, -2, -4], [4, -3]]) in S;
false

#T# acting: \in, wrong family relation 2/2
gap> S := Monoid([Transformation([1, 3, 3])]);;
gap> x := Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]);;
gap> x in S;
false

#T# acting: \in, wrong fixed degree
gap> S := Monoid([Bipartition([[1, 2, -1, -2]])]);;
gap> x := Bipartition([[1, -1], [2, -2], [3, -3]]);;
gap> x in S;
false

#T# acting: \in, too large rank
gap> S := Monoid([Transformation([1, 2, 2])]);;
gap> x := Transformation([2, 1, 3]);;
gap> x in S;
false

#T# acting: \in, too small rank
gap> S := RectangularBand(3, 3);
<regular transformation semigroup of size 9, degree 7 with 3 generators>
gap> MinimalIdeal(S);;
gap> x := ConstantTransformation(10, 10);
Transformation( [ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10 ] )
gap> x in S;
false

#T# acting: \in, wrong lambda value
gap> S := Semigroup([Transformation([1, 3, 4, 1, 3]),
> Transformation([5, 5, 1, 1, 3])]);;
gap> x := Transformation([1, 2, 1, 2, 1]);;
gap> x in S;
false

#T# acting: \in, is existing R-class rep
gap> S := Semigroup([Transformation([1, 3, 4, 1, 3]),
> Transformation([5, 5, 1, 1, 3])]);;
gap> RClassReps(S)[1] in S;
true

#T# acting: \in, wrong rho value 1/2
gap> S := Semigroup([Transformation([1, 3, 4, 1, 3]),
> Transformation([5, 5, 1, 1, 3])]);;
gap> Size(S);;
gap> x := Transformation([1, 3, 3, 4, 4]);;
gap> x in S;
false

#T# acting: Position, wrong rho value, lambda not in scc 1st place
gap> S := Semigroup([Transformation([1, 3, 4, 1, 3]),
> Transformation([5, 5, 1, 1, 3])]);;
gap> Enumerate(SemigroupData(S));;
gap> Position(SemigroupData(S), Transformation([5, 1, 1, 1, 3]));
fail
gap> Position(SemigroupData(S), Transformation([1, 5, 1, 5, 1]));
fail

#T# acting: \in, wrong rho value 2/2
gap> S := Semigroup([Transformation([1, 3, 4, 1, 3]),
> Transformation([5, 5, 1, 1, 3])]);;
gap> x := Transformation([1, 3, 3, 4, 4]);;
gap> x in S;
false

#T# acting: \in, wrong rho value 2/2
gap> S := Semigroup([Transformation([1, 3, 4, 1, 3]),
> Transformation([5, 5, 1, 1, 3])]);;
gap> x := Transformation([1, 3, 3, 4, 4]);;
gap> x in S;
false

#T# acting: \in, wrong lambda-rho combination 1/2
gap> S := Semigroup([Transformation([1, 3, 4, 1, 3]),
> Transformation([5, 5, 1, 1, 3])]);;
gap> Size(S);;
gap> x := Transformation([1, 3, 5, 1, 3]);;
gap> x in S;
false

#T# acting: Position, wrong lambda-rho combination 1/2
gap> S := Semigroup([Transformation([1, 3, 4, 1, 3]),
> Transformation([5, 5, 1, 1, 3])]);;
gap> Size(S);;
gap> x := Transformation([1, 3, 5, 1, 3]);;
gap> Position(SemigroupData(S), x);
fail

#T# acting: \in, wrong lambda-rho combination 2/2
gap> S := Semigroup([Transformation([1, 3, 4, 1, 3]),
> Transformation([5, 5, 1, 1, 3])]);;
gap> x := Transformation([1, 3, 5, 1, 3]);;
gap> x in S;
false

#T# acting: \in, Schutzenberger group is symmetric
gap> S := Semigroup([Transformation([3, 5, 2, 4, 1]),
> Transformation([3, 5, 4, 1, 3]),
> Transformation([5, 1, 3, 5, 1]),
> Transformation([5, 4, 5, 2, 4])]);;
gap> x := Transformation([3, 4, 5, 3, 4]);;
gap> x in S;
true

#T# acting: Position, Schutzenberger group is symmetric
gap> S := Semigroup([Transformation([3, 5, 2, 4, 1]),
> Transformation([3, 5, 4, 1, 3]),
> Transformation([5, 1, 3, 5, 1]),
> Transformation([5, 4, 5, 2, 4])]);;
gap> Size(S);;
gap> x := Transformation([3, 4, 5, 3, 4]);;
gap> Position(SemigroupData(S), x);
4

#T# acting: \in, non-regular all possible reps 1/2
gap> S := Monoid([Transformation([1, 3, 2, 3]),
> Transformation([1, 4, 1, 2]),
> Transformation([3, 4, 2, 2]),
> Transformation([4, 1, 2, 1])]);;
gap> Size(S);;
gap> Transformation([1, 2, 4, 2]) in S;
true

#T# acting: \in, check Schutzenberger group
gap> S := Semigroup([Transformation([1, 3, 2, 3]),
> Transformation([1, 4, 1, 2]),
> Transformation([2, 4, 1, 1]),
> Transformation([3, 4, 2, 2])]);;
gap> Transformation([1, 2, 3, 2]) in S;
true

#T# acting: Position, check Schutzenberger group
gap> S := Semigroup([Transformation([1, 3, 2, 3]),
> Transformation([1, 4, 1, 2]),
> Transformation([2, 4, 1, 1]),
> Transformation([3, 4, 2, 2])]);;
gap> Size(S);;
gap> Position(SemigroupData(S), Transformation([1, 2, 3, 2]));
2

#T# acting: \in, new lambda-rho combo and found rep
gap> S := Semigroup([Transformation([1, 3, 4, 1, 3]),
> Transformation([5, 5, 1, 1, 3])]);;
gap> x := Transformation([1, 3, 4, 1, 3]);;
gap> x in S;
true

#T# acting: \in, trivial Schutz group
gap> S := Semigroup([Transformation([3, 1, 1, 3, 1]),
> Transformation([4, 3, 1, 4, 2]),
> Transformation([4, 3, 4, 2, 3]),
> Transformation([5, 2, 1, 2, 1])]);;
gap> Size(S);;
gap> Transformation([1, 3, 4, 1, 2]) in S;
false

#T# acting: \in, enumerated non-trivial Schutzenberger group
gap> S := Monoid([Transformation([2, 6, 7, 2, 6, 1, 1, 5]),
> Transformation([3, 8, 1, 4, 5, 6, 7, 1]),
> Transformation([4, 3, 2, 7, 7, 6, 6, 5]),
> Transformation([7, 1, 7, 4, 2, 5, 6, 3])]);;
gap> Transformation([5, 3, 1, 5, 3, 4, 4, 3]) in S;
true

#T# acting: \in, enumerated trivial Schutzenberger group
gap> S := Semigroup([
>  Transformation([1, 5, 6, 2, 5, 2, 1]),
>  Transformation([1, 7, 5, 4, 3, 5, 7]),
>  Transformation([2, 7, 7, 2, 4, 1, 1]),
>  Transformation([3, 2, 2, 4, 1, 7, 6]),
>  Transformation([3, 3, 5, 1, 7, 1, 6]),
>  Transformation([3, 3, 6, 1, 7, 5, 2]),
>  Transformation([3, 4, 6, 5, 4, 4]),
>  Transformation([5, 2, 4, 5, 1, 4, 5]),
>  Transformation([5, 5, 2, 2, 6, 7, 2]),
>  Transformation([7, 7, 5, 4, 5, 3, 2])]);;
gap> Transformation([2, 6, 6, 7, 6, 1, 3]) in S;
true

#T# acting: \in, non-regular all possible reps 2/2
gap> S := Monoid([Transformation([2, 6, 7, 2, 6, 1, 1, 5]),
>  Transformation([3, 8, 1, 4, 5, 6, 7, 1]),
>  Transformation([4, 3, 2, 7, 7, 6, 6, 5]),
>  Transformation([7, 1, 7, 4, 2, 5, 6, 3])]);;
gap> Transformation([1, 4, 1, 3, 5, 4, 4, 1]) in S;
true

#T# acting: \in final false.
gap> S := Semigroup(Transformation([2, 3, 3, 5, 4]),
>   Transformation([5, 5, 3, 2, 4]));;
gap> Transformation([3, 2, 2, 5, 4]) in S;
false

#T# acting: Position final fail.
gap> S := Semigroup(Transformation([2, 3, 3, 5, 4]),
>   Transformation([5, 5, 3, 2, 4]));;
gap> Size(S);;
gap> Position(SemigroupData(S), Transformation([3, 2, 2, 5, 4]));
fail

#T# acting: Enumerate, 2 args
gap> S := Semigroup([PartialPerm([1, 2, 4, 6, 8, 9],
> [7, 10, 1, 9, 4, 2]),
> PartialPerm([1, 2, 3, 4, 5, 8, 10], [7, 1, 4, 3, 2, 6, 5])]);;
gap> Enumerate(SemigroupData(S), 20);
<open semigroup data with 20 reps, 27 lambda-values, 20 rho-values>

#T# acting: Enumerate, closed and looking
gap> S := Semigroup(IdentityTransformation);;
gap> Enumerate(SemigroupData(S));;
gap> Enumerate(SemigroupData(S), infinity, ReturnTrue);;

#T# acting: Enumerate, innermost if-condition not IsBound(lambdarhoht[l])
gap> S := Semigroup(Transformation([2, 3, 3, 5, 4]),
>   Transformation([5, 5, 3, 2, 4]));;
gap> Enumerate(RhoOrb(S));
<closed orbit, 13 points with Schreier tree with log>
gap> Size(S);
49

#T# acting: OrbitGraph and OrbitGraphAsSets
gap> S := Semigroup([
> Bipartition([[1, 2, 3, -1, -4], [4], [5, -3], [-2, -5]]),
> Bipartition([[1, 3, 5], [2, 4, -1, -2], [-3], [-4], [-5]]),
> Bipartition([[1, -1, -3, -5], [2, 3, 4, -2, -4], [5]])]);;
gap> Size(S);
18
gap> OrbitGraph(SemigroupData(S));
[ [ 2, 3, 4 ], [ 5, 3, 6 ], [ 5, 3, 7 ], [ 5, 3, 6 ], [ 5, 3, 6 ], 
  [ 5, 3, 6 ], [ 5, 3, 6 ] ]
gap> OrbitGraphAsSets(SemigroupData(S));
[ [ 2, 3, 4 ], [ 3, 5, 6 ], [ 3, 5, 7 ], [ 3, 5, 6 ], [ 3, 5, 6 ], 
  [ 3, 5, 6 ], [ 3, 5, 6 ] ]
gap> PositionOfFound(SemigroupData(S));
Error, Semigroups: PositionOfFound: usage,
not looking for anything,

#T# acting: SizeOfSemigroupData
gap> S := Semigroup([PartialPerm([1, 2, 4, 6, 8, 9],
> [7, 10, 1, 9, 4, 2]),
> PartialPerm([1, 2, 3, 4, 5, 8, 10], [7, 1, 4, 3, 2, 6, 5])]);;
gap> Enumerate(SemigroupData(S), 20);
<open semigroup data with 20 reps, 27 lambda-values, 20 rho-values>
gap> SizeOfSemigroupData(SemigroupData(S));
22
gap> SizeOfSemigroupData(S);
Error, Semigroups: SizeOfSemigroupData: usage,
the arg <data> must be semigroup data,
gap> Enumerate(SemigroupData(S));
<closed semigroup data with 46 reps, 27 lambda-values, 28 rho-values>
gap> S := Semigroup(S);;
gap> SizeOfSemigroupData(SemigroupData(S));
0

#T# acting: SEMIGROUPS.UniversalFakeOne
gap> SEMIGROUPS.UniversalFakeOne * RandomMatrix(IsBooleanMat, 4);;
gap> RandomTransformation(4) * SEMIGROUPS.UniversalFakeOne;;
gap> String(SEMIGROUPS.UniversalFakeOne);
"<universal fake one>"
gap> SEMIGROUPS.UniversalFakeOne < SEMIGROUPS.UniversalFakeOne;
false
gap> SEMIGROUPS.UniversalFakeOne < RandomBipartition(4);
true
gap> RandomPartialPerm(4) < SEMIGROUPS.UniversalFakeOne;
false

#T# UnbindVariables
gap> Unbind(S);
gap> Unbind(f);
gap> Unbind(gens);
gap> Unbind(iter);
gap> Unbind(r);
gap> Unbind(s);
gap> Unbind(x);

#E#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/acting.tst");
