#############################################################################
##
#W  testinstall.tst
#Y  Copyright (C) 2011-15                               James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# ReadTest(Filename(DirectoriesPackageLibrary("semigroups","tst"),
# > "testinstall.tst"));
gap> START_TEST("Semigroups package: testinstall.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels and user preferences
gap> SemigroupsStartTest();

#T# TestInstall3
gap> s:=Semigroup(Transformation( [ 2, 3, 4, 1, 1, 1 ] ));;
gap> IsMonoidAsSemigroup(s);
true
gap> IsMonoid(s);
false
gap> iso:=IsomorphismTransformationMonoid(s);
MappingByFunction( <commutative transformation semigroup of degree 6 with 1 
 generator>, <commutative transformation monoid of degree 4 with 1 generator>
 , function( f ) ... end, function( f ) ... end )
gap> RespectsMultiplication(iso);
true
gap> ForAll(s, x-> (x^iso)^InverseGeneralMapping(iso)=x);
true

#T# TestInstall4
gap> s:=Semigroup(Transformation([1,1,1]), Transformation([3,1,2]));
<transformation semigroup of degree 3 with 2 generators>
gap> IsSimpleSemigroup(s);
false

#T# TestInstall5
gap> s:=SingularTransformationSemigroup(6);
<regular transformation semigroup ideal of degree 6 with 1 generator>
gap> Size(s);
45936

#T# TestInstall6
gap> s:=Semigroup(IdentityTransformation);;
gap> LambdaOrb(s);
<open orbit, 1 points with Schreier tree with log>
gap> Enumerate(last);
<closed orbit, 2 points with Schreier tree with log>

#T# TestInstall7 
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
>  Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 3, 4, 2, 2 ] ),
>  Transformation( [ 4, 1, 2, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> Size(s); NrRClasses(s); NrLClasses(s); NrDClasses(s);
69
17
21
9
gap> NrIdempotents(s); NrRegularDClasses(s); IsRegularSemigroup(s);
22
6
false
gap> f:=Transformation( [ 1, 3, 4, 1 ] );;
gap> f in s;
false
gap> f:=Transformation( [ 1, 1, 3, 1 ] );;
gap> f in s;
true
gap> t:=Semigroup(gens{[1..3]});
<transformation semigroup of degree 4 with 3 generators>
gap> ForAll(t, x-> x in s);
true
gap> Size(t);
60

#T# TestInstall8: Issue 2
gap> s:=Semigroup(Transformation([4,4,4,4]));;
gap> AsList(s);
[ Transformation( [ 4, 4, 4, 4 ] ) ]

#T# TestInstall9: Issue 3
gap> s:=Semigroup(Transformation( [ 3, 5, 5, 5, 3 ] ), 
> Transformation( [ 5, 5, 2, 1, 5 ] ));;
gap> f:=Transformation( [ 3, 3, 5, 3, 3 ] );;
gap> IsRegularSemigroupElement(s, f);
true
gap> f:=Transformation( [ 5, 5, 5, 5, 5 ] );;
gap> IsRegularSemigroupElement(s, f);
true
gap> f:=Transformation( [ 3, 5, 5, 5, 3 ] );;
gap> IsRegularSemigroupElement(s, f);
true
gap> IsRegularSemigroup(s);
false
gap> f:=Transformation( [ 5, 5, 2, 1, 5 ] );;
gap> IsRegularSemigroupElement(s, f);
false

#T# TestInstall10: Issue 9
gap> gens:=[ Transformation( [ 1, 2, 3, 9, 5, 11, 7, 8, 9, 10, 11, 12 ] ),
> Transformation( [ 1, 2, 3, 9, 5, 11, 9, 8, 9, 8, 11, 12 ] ), 
> Transformation( [ 1, 2, 5, 7, 8, 11, 9, 12, 9, 12, 11, 10 ] ),
> Transformation( [ 1, 2, 8, 9, 5, 11, 9, 8, 9, 10, 11, 12 ] ), 
> Transformation( [ 1, 2, 8, 11, 12, 7, 11, 8, 11, 10, 9, 12 ] ),
> Transformation( [ 1, 2, 10, 9, 12, 11, 9, 10, 9, 8, 11, 12 ] ),
> Transformation( [ 1, 2, 12, 4, 10, 6, 7, 12, 9, 12, 11, 10 ] ),
> Transformation( [ 1, 5, 3, 11, 5, 9, 11, 8, 11, 10, 9, 12 ] ), 
> Transformation( [ 1, 5, 8, 6, 12, 7, 11, 10, 11, 10, 9, 12 ] ),
> Transformation( [ 1, 5, 8, 11, 12, 7, 11, 8, 11, 10, 9, 12 ] ), 
> Transformation( [ 1, 5, 12, 7, 8, 11, 9, 12, 9, 12, 11, 10 ] ),
> Transformation( [ 1, 8, 3, 9, 5, 11, 9, 8, 9, 10, 11, 12 ] ),
> Transformation( [ 1, 8, 5, 7, 8, 11, 9, 12, 9, 12, 11, 10 ] ), 
> Transformation( [ 1, 12, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] ), 
> Transformation( [ 1, 12, 10, 9, 12, 11, 7, 10, 4, 10, 6, 12 ] ),
> Transformation( [ 2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11 ] ), 
> Transformation( [ 3, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11 ] ),
> Transformation( [ 5, 6, 7, 8, 11, 12, 10, 9, 10, 9, 12, 11 ] ),
> Transformation( [ 5, 6, 11, 8, 7, 12, 10, 11, 10, 11, 12, 9 ] ),
> Transformation( [ 5, 7, 10, 11, 9, 5, 11, 8, 11, 8, 12, 9 ] ),
> Transformation( [ 5, 10, 7, 5, 10, 11, 12, 9, 12, 9, 11, 8 ] ),
> Transformation( [ 7, 3, 11, 9, 8, 5, 9, 11, 9, 11, 5, 3 ] ), 
> Transformation( [ 7, 5, 8, 6, 12, 7, 11, 10, 11, 10, 9, 12 ] ),
> Transformation( [ 7, 12, 11, 10, 5, 9, 10, 11, 8, 11, 9, 12 ] ),
> Transformation( [ 9, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] ), 
> Transformation( [ 9, 11, 8, 5, 11, 9, 12, 3, 5, 8, 9, 11 ] ), 
> Transformation( [ 11, 7, 9, 5, 10, 11, 12, 9, 12, 9, 11, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> NrDClasses(s);
232
gap> Size(s);
11858
gap> NrRClasses(s);
1455
gap> NrLClasses(s);
690
gap> NrHClasses(s);
5356
gap> NrIdempotents(s);
300
gap> Sum(List(GreensDClasses(s), NrRClasses))=NrRClasses(s);
true
gap> ForAll(Concatenation(List(GreensDClasses(s), RClassReps)), 
> x-> x in s);
true

#T# TestInstall11

#gap> ForAll([1..NrRClasses(s)], i->
#> EvaluateWord(Generators(s), TraceRClassRepsTree(s, i))=
#> RClassReps(s)[i]);
#true

#T# TestInstall12
gap> gens:=[ Transformation( [ 1, 2, 3, 5, 4, 6, 7, 8 ] ),
>   Transformation( [ 4, 4, 3, 1, 5, 6, 3, 8 ] ),
>   Transformation( [ 3, 6, 1, 7, 3, 4, 8, 3 ] ),
>   Transformation( [ 1, 2, 3, 4, 5, 3, 7, 8 ] ),
>   Transformation( [ 1, 2, 3, 4, 1, 6, 7, 8 ] ),
>   Transformation( [ 8, 8, 3, 4, 5, 7, 6, 1 ] ) ];;
gap> s:=Monoid(gens);
<transformation monoid of degree 8 with 6 generators>
gap> t:=ClosureSemigroup(s, [Transformation( [ 4, 4, 3, 1, 5, 6, 3, 8 ] )]);
<transformation monoid of degree 8 with 6 generators>
gap> Size(t)=Size(Semigroup(Generators(t)));
true

#T# TestInstall13
gap> s:=Semigroup([ Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] ), 
>  Transformation( [ 1, 2, 3, 4, 5, 6, 7, 9, 8 ] ), 
>  Transformation( [ 7, 2, 8, 4, 5, 6, 1, 9, 8 ] ), 
>  Transformation( [ 5, 5, 3, 4, 1, 6, 7, 8, 9 ] ), 
>  Transformation( [ 5, 7, 3, 4, 1, 6, 7, 8, 9 ] ), 
>  Transformation( [ 1, 2, 8, 6, 5, 4, 7, 9, 8 ] ), 
>  Transformation( [ 1, 8, 6, 2, 7, 8, 8, 9, 5 ] ), 
>  Transformation( [ 1, 2, 3, 8, 8, 7, 7, 9, 5 ] ), 
>  Transformation( [ 1, 2, 3, 1, 8, 7, 7, 5, 9 ] ), 
>  Transformation( [ 7, 7, 2, 7, 8, 8, 9, 5, 1 ] ), 
>  Transformation( [ 7, 2, 5, 2, 8, 8, 1, 9, 5 ] ), 
>  Transformation( [ 7, 2, 8, 1, 8, 7, 1, 9, 5 ] ), 
>  Transformation( [ 1, 1, 4, 8, 9, 9, 8, 5, 7 ] ), 
>  Transformation( [ 1, 1, 1, 2, 5, 5, 7, 8, 9 ] ), 
>  Transformation( [ 1, 2, 1, 1, 8, 7, 7, 5, 9 ] ), 
>  Transformation( [ 1, 2, 8, 8, 8, 2, 7, 9, 5 ] ), 
>  Transformation( [ 7, 2, 7, 1, 8, 8, 1, 5, 9 ] ), 
>  Transformation( [ 8, 8, 2, 8, 5, 5, 9, 7, 1 ] ), 
>  Transformation( [ 1, 2, 1, 1, 5, 5, 7, 8, 9 ] ), 
>  Transformation( [ 5, 5, 4, 5, 8, 8, 9, 7, 1 ] ), 
>  Transformation( [ 1, 2, 8, 8, 8, 1, 7, 9, 5 ] ), 
>  Transformation( [ 7, 2, 7, 2, 5, 5, 1, 8, 9 ] ) ]);;
gap> f:= Transformation( [ 7, 7, 4, 2, 1, 8, 8, 9, 5 ] );;
gap> d:=DClass(s, Transformation( [ 1, 8, 6, 2, 7, 8, 8, 9, 5 ] ));;
gap> l:=LClass(d, f);
<Green's L-class: Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] )>
gap> ll:=LClass(s, f);
<Green's L-class: Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] )>
gap> List(HClassReps(ll), x-> x in ll);
[ true, true, true, true ]
gap> List(HClassReps(l), x-> x in l);
[ true, true, true, true ]
gap> l=ll;
true
gap> ll<l;
false
gap> l<ll;
false
gap> Elements(l)=Elements(ll);
true
gap> Size(l); Size(ll);
8
8
gap> DClassOfLClass(ll)=DClassOfLClass(l);
true
gap> DClassOfLClass(l)=d;
true
gap> NrHClasses(l); NrHClasses(ll);
4
4
gap> HClassReps(l);
[ Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] ), 
  Transformation( [ 7, 7, 4, 2, 1, 8, 8, 9, 5 ] ), 
  Transformation( [ 1, 8, 4, 2, 7, 8, 8, 5 ] ), 
  Transformation( [ 7, 7, 4, 2, 1, 8, 8, 5 ] ) ]
gap> HClassReps(ll);
[ Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] ), 
  Transformation( [ 7, 7, 4, 2, 1, 8, 8, 9, 5 ] ), 
  Transformation( [ 1, 8, 4, 2, 7, 8, 8, 5 ] ), 
  Transformation( [ 7, 7, 4, 2, 1, 8, 8, 5 ] ) ]
gap> Idempotents(l);    
[  ]
gap> Idempotents(ll);
[  ]
gap> IsRegularDClass(d);
false
gap> Size(s); 
6982
gap> Set(HClasses(l))=Set(HClasses(ll));
true
gap> SchutzenbergerGroup(l);
Group([ (5,9), (1,7) ])
gap> g:=SchutzenbergerGroup(ll);
Group([ (5,9), (1,7) ])

#T# TestInstall14: IsomorphismTransformationSemigroup/Monoid
gap> IsomorphismTransformationSemigroup(g);
MappingByFunction( Group([ (5,9), (1,7) ]), <transformation semigroup 
 of size 4, degree 4 with 2 generators>
 , function( x ) ... end, function( x ) ... end )
gap> s:=Range(last);
<transformation semigroup of size 4, degree 4 with 2 generators>
gap> IsGroupAsSemigroup(s);
true
gap> Generators(s);
[ Transformation( [ 1, 4, 3, 2 ] ), Transformation( [ 3, 2, 1 ] ) ]
gap> t:=Range(IsomorphismTransformationMonoid(g));
<transformation monoid of size 4, degree 4 with 2 generators>
gap> Generators(t);
[ Transformation( [ 1, 4, 3, 2 ] ), Transformation( [ 3, 2, 1 ] ) ]
gap> h:=Range(IsomorphismPermGroup(t));
Group([ (), (2,4), (1,3) ])
gap> IsomorphismGroups(g, h);
[ (5,9), (1,7) ] -> [ (2,4), (1,3) ]

#T# TestInstall15: Issue 22 - takes about 49ms
gap> f := Transformation( [ 2, 12, 10, 7, 6, 11, 8, 3, 4, 5, 1, 11 ] );
Transformation( [ 2, 12, 10, 7, 6, 11, 8, 3, 4, 5, 1, 11 ] )
gap> InversesOfSemigroupElement(FullTransformationSemigroup(12),f);
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
gap> file:=Concatenation(SemigroupsDir(), "/tst/test.gz");;
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
gap> s:=InverseSemigroup( 
> [ PartialPermNC( [ 1, 2 ], [ 3, 1 ] ),
>   PartialPermNC( [ 1, 2, 3 ], [ 1, 3, 4 ] ),
>   PartialPermNC( [ 1, 2, 3 ], [ 2, 4, 1 ] ),
>   PartialPermNC( [ 1, 3, 4 ], [ 3, 4, 1 ] ) ]);;
gap> Size(s); NrRClasses(s); NrLClasses(s); NrDClasses(s);
116
14
14
4
gap> NrIdempotents(s); NrRegularDClasses(s); IsRegularSemigroup(s);
14
4
true
gap> ForAll(s, x-> x in s);
true
gap> t:=InverseSemigroup(Generators(s){[1..3]});
<inverse partial perm semigroup of rank 4 with 3 generators>
gap> ForAll(t, x-> x in s);
true
gap> Size(t);
98

#T# TestInstall18
gap> s:=InverseSemigroup(
> [ PartialPermNC( [ 1, 3, 5, 6, 7 ], [ 9, 1, 5, 3, 8 ] ),
> PartialPermNC( [ 1, 2, 3, 5, 6, 7, 9, 10 ], [ 4, 10, 5, 6, 7, 1, 3, 2 ] ) ]);;
gap> f:=PartialPermNC( [ 3, 4, 5, 6 ], [ 1, 3, 6, 5 ] );;
gap> d:=DClass(s, f);;
gap> F:=InjectionPrincipalFactor(d);; G:=InverseGeneralMapping(F);;
gap> (f^F)^G=f;
true
gap> ForAll(d, f-> (f^F)^G=f);
true
gap> s:=InverseSemigroup(
> [ PartialPermNC( [ 1, 3, 5, 6, 7 ], [ 9, 1, 5, 3, 8 ] ),
> PartialPermNC( [ 1, 2, 3, 5, 6, 7, 9, 10 ], [ 4, 10, 5, 6, 7, 1, 3, 2 ] ) ]);;
gap> d:=DClasses(s)[14];
<Green's D-class: <identity partial perm on [ 2, 10 ]>>
gap> F:=IsomorphismReesMatrixSemigroup(d);;
gap> G:=InverseGeneralMapping(F);;
gap> ForAll(d, f-> (f^F)^G=f);        
true

#T# TestInstall19: from JS' MultiplicativeZero.tst
gap> s := InverseMonoid(PartialPerm([1, 2, 3, 4]),
> PartialPerm([1, 3, 2, 4]),
> PartialPerm([1, 2, 0, 0]),
> PartialPerm([1, 0, 0, 4]));;
gap> f := PartialPerm([1, 0, 0, 0]);;
gap> f in s;
true
gap> ForAll(s, x -> f * x = f and x * f = f );
true
gap> f;
<identity partial perm on [ 1 ]>
gap> MultiplicativeZero(s);
<identity partial perm on [ 1 ]>

#T# TestInstall20: from JS' PartialPermInjective.tst
gap> PartialPerm( [0,0,1,2] );
[3,1][4,2]

#T# TestInstall21: from JS' RestricterPartialPerm.tst
gap> x:=PartialPerm([2..7],[1..6]); RestrictedPartialPerm(x,[2..7]);
[7,6,5,4,3,2,1]
[7,6,5,4,3,2,1]

#T# TestInstall22: from JS' SizeInverseMonoid.tst
gap> s:=InverseMonoid( PartialPerm( [1,2,3,4,5,6,7,8] ),
> PartialPerm( [1,6,3,4,8,2,7,5] ),
> PartialPerm( [1,2,7,4,8,6,3,5] ),
> PartialPerm( [1,0,0,4,5,0,0,8] ),
> PartialPerm( [1,2,0,4,0,6,0,0] ),
> PartialPerm( [1,0,3,4,0,0,7,0] ),
> PartialPerm( [1,0,0,0,0,0,0,0] ));;
gap> [ Size( s ), Size( AsSet( s ) ) ];
[ 12, 12 ]

#T# TestInstall23: from JS' email
gap> s:=InverseMonoid( PartialPerm( [1,3,2] ), PartialPerm([1]) );;
gap> [ Size( s ), Size( AsSet( s ) ) ];
[ 3, 3 ]
gap> Elements(s); 
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
gap> u := x^5 * y^3 * z;
x1*x1*x1*x1*x1*x2*x2*x2*x3
gap> u^-1;
x3^-1*x2^-1*x2^-1*x2^-1*x1^-1*x1^-1*x1^-1*x1^-1*x1^-1
gap> x^2 * y = x^2 * y;
true
gap> x * x^-1 = y * y^-1;
false

#T# TestInstall25: Issue 27 in the new numbering...
gap> s:=Semigroup(List(GeneratorsOfSemigroup(FullTransformationSemigroup(3)),
>  x-> AsTransformation(x, 4)));;
gap> IsFullTransformationSemigroup(s);
true
gap>  IdentityTransformation in s; 
true
gap>  IdentityTransformation in s;
true

#T# TestInstall26: Issue 23 in the new numbering...
gap> S:=FullTransformationSemigroup(3);;
gap> f:=Transformation( [ 4, 3, 1, 2 ] );;
gap> ClosureSemigroup(S, f);           
<transformation monoid of degree 4 with 4 generators>

#T# TestInstall27: Issue 36 in the new numbering...
gap> S:=Semigroup(IdentityTransformation);
<trivial transformation group of degree 0 with 1 generator>
gap> SmallGeneratingSet(S);
[ IdentityTransformation ]

#T# TestInstall28: MaximalSubsemigroups of Rees 0-matrix semigroups
gap> G:=Group((1,2),(3,4));;
gap> mat:=[[(), ()], [(), 0], [(), (1,2)]];;
gap> R:=ReesZeroMatrixSemigroup(G, mat);
<Rees 0-matrix semigroup 2x3 over Group([ (1,2), (3,4) ])>
gap> (IsBound(GAPInfo.PackagesLoaded.grape) 
> and Filename(DirectoriesPackagePrograms("grape"),"dreadnautB")<>fail 
> and IsDuplicateFreeList(MaximalSubsemigroups(R))
> and ForAll(MaximalSubsemigroups(R), U-> IsMaximalSubsemigroup(R, U)) 
> and Length(MaximalSubsemigroups(R))=6) 
> or (not (IsBound(GAPInfo.PackagesLoaded.grape) 
> and Filename(DirectoriesPackagePrograms("grape"),"dreadnautB")<>fail));
true

#T# TestInstall29: ClosureSemigroup with an element of higher degree
gap> S:=Semigroup( 
> Transformation( [ 1, 3, 3, 2 ] ), Transformation( [ 4, 1, 4, 2 ] ), 
> Transformation( [ 4, 2, 3, 3 ] ), Transformation( [ 4, 4, 4, 4 ] ) );;
gap> Size(S);
130
gap> f:=Transformation( [ 3, 5, 1, 5, 2 ] );;
gap> T:=ClosureSemigroup(S, f);;
gap> Size(T);
1619

#T# TestInstall30: bug with Elements and IsomorphismPermGroup for group H-class
gap> R:=ReesZeroMatrixSemigroup(Group(()), 
> [ [ (), (), () ], [ (), 0, 0 ], [ (), 0, 0 ] ]);
<Rees 0-matrix semigroup 3x3 over Group(())>
gap> R:=ReesZeroMatrixSubsemigroup(R, [2,3], Group(()), [2,3]);
<Rees 0-matrix semigroup 2x2 over Group(())>
gap> H:=First(HClasses(R), IsGroupHClass);
<Green's H-class: 0>
gap> Elements(H);   
[ 0 ]
gap> f:=IsomorphismPermGroup(H);;
gap> g:=InverseGeneralMapping(f);;
gap> Elements(H)[1]^f;
()
gap> ()^g;            
0

#T# TestInstall31: Issue 47: bug in ClosureSemigroup caused which assumed 
# that if the rank of an R-class rep was greater than the maximum rank of the
# collection being added, then we hadn't seen an R-class rep with the same 
# rho-value before. 
gap> S:=Semigroup([ Transformation( [ 1, 2, 4, 6, 1, 6 ] ),
> Transformation( [ 1, 6, 1, 1, 6, 5 ] ),
> Transformation( [ 2, 6, 2, 4, 3, 2 ] ),
> Transformation( [ 4, 1, 3, 6, 1, 5 ] ),
> Transformation( [ 4, 1, 4, 2, 4, 2 ] ),
> Transformation( [ 6, 6, 4, 6, 1, 1 ] ) ]);
<transformation semigroup of degree 6 with 6 generators>
gap> T:=Semigroup([ Transformation( [ 1, 5, 3, 4, 5 ] ),
> Transformation( [ 6, 4, 3, 5, 4, 1 ] ),
> Transformation( [ 1, 2, 4, 6, 1, 6 ] ),
> Transformation( [ 1, 5, 1, 6, 3, 1 ] ),
> Transformation( [ 4, 1, 6, 5, 4, 5 ] ),
> Transformation( [ 2, 6, 2, 4, 3, 2 ] ),
> Transformation( [ 2, 1, 2, 4, 4, 2 ] ),
> Transformation( [ 4, 5, 4, 4, 5, 3 ] ),
> Transformation( [ 4, 4, 4, 5, 4, 3 ] ),
> Transformation( [ 6, 1, 6, 6, 4, 6 ] ),
> Transformation( [ 5, 6, 6, 6, 6, 1 ] ),
> Transformation( [ 4, 4, 5, 4, 3, 3 ] ) ]);
<transformation semigroup of degree 6 with 12 generators>
gap> IsMaximalSubsemigroup(S, T);
true
gap> T:=Semigroup(T, rec(small:=true));;
gap> IsMaximalSubsemigroup(S, T);
true

#T# TestInstall32: From Jack Schmidt 06/02/14 by email
gap> S:=InverseMonoid([
> PartialPerm([1..32],
> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,30,29,32,31,26,25,28,27,
>  18,17,20,19,22,21,24,23]),
> PartialPerm([1..32], [1,10,16,7,5,14,4,11,9,2,8,15,13,6,12,3,
> 17,22,32,27,21,18,28,31,25,30,20,23,29,26,24,19]),
> PartialPerm([1..16], [1,2,3,4,16,15,13,14,10,9,12,11,7,8,6,5]), 
> PartialPerm([1,2,5,6,9,10,13,14,17,18,21,22,25,26,29,30],
> [1,17,5,29,9,21,13,25,30,14,26,6,18,10,22,2]), 
> PartialPerm([1,2,5,6,9,10,13,14,19,20,23,24,27,28,31,32],
> [1,10,5,14,9,2,13,6,19,24,23,20,27,32,31,28]), 
> PartialPerm([1,6,9,14,18,22,25,29],[1,6,9,14,18,22,25,29]), 
> PartialPerm([1,5,9,13,19,23,27,31],[1,5,9,13,19,23,27,31]), 
> PartialPerm([1,6,9,14,17,21,26,30],[1,6,9,14,17,21,26,30]) ]);; 
gap> ForAll(S,f->ForAll(LClass(S,f),x -> x in S)); 
true

#T# TestInstall33: From Jack Schmidt 07/02/14 by email
gap> AsSet(InverseMonoid( PartialPerm([1,2]), PartialPerm([1])));
[ <identity partial perm on [ 1 ]>, <identity partial perm on [ 1, 2 ]> ]

#T# TestInstall34: Issue #57 (problem in INV_KER_TRANS)
gap> S:=Semigroup(Transformation([1,1,1]), Transformation([1,1,4,4,5]));;
gap> Size(S);
2
gap> IsMonogenicSemigroup(S);
false

#T# TestInstall35: Issue pointed out by WAW caused by
# IsInvLambdaOrb being inherited from the argument of ClosureSemigroup
# by its output, when the output wasn't an InverseOp semigroup... 
gap> S:=Semigroup([
> Bipartition( [ [ 1, -2 ], [ 2, -3 ], [ 3, -1 ] ] ),
> Bipartition( [ [ 1, 2, -2 ], [ 3, -1, -3 ] ] ),
> Bipartition( [ [ 1, -1, -3 ], [ 2, -2 ], [ 3 ] ] ),
> Bipartition( [ [ 1, 3, -1 ], [ 2, -2 ], [ -3 ] ] ) ]);;
gap> gens:=[
> Bipartition( [ [ 1, 3, -1 ], [ 2, -2 ], [ -3 ] ] ),
> Bipartition( [ [ 1, -1, -3 ], [ 2, -2 ], [ 3 ] ] ),
> Bipartition( [ [ 1, 2, -2 ], [ 3, -1, -3 ] ] ) ];;
gap> V:=SemigroupIdealByGenerators(S, gens);
<regular bipartition semigroup ideal of degree 3 with 3 generators>
gap> tuples:=[ Bipartition( [ [ 1, -1 ], [ 2, -2 ], [ 3, -3 ] ] ) ];;
gap> Semigroup(V, tuples, rec(small:=true));;

#T# TestInstall36: Issue pointed out by WAW, caused by typo in ClosureSemigroup 
# (the parent of an R-class was set to be the subsemigroup not the new parent
# semigroup)
gap> for i in [1..6] do 
> V:=Semigroup([ PartialPerm( [ 1, 2, 4, 5, 6 ], [ 1, 5, 3, 4, 6 ] ),
>  PartialPerm( [ 1, 2, 4, 5, 6 ], [ 2, 1, 5, 4, 3 ] ),
>  PartialPerm( [ 1, 3, 4, 5, 6 ], [ 1, 4, 5, 2, 6 ] ),
>  PartialPerm( [ 1, 2, 3, 4, 5 ], [ 2, 1, 6, 5, 4 ] ),
>  PartialPerm( [ 1, 2, 3, 6 ], [ 4, 3, 2, 6 ] ),
>  PartialPerm( [ 1, 2, 4, 6 ], [ 2, 1, 5, 3 ] ),
>  PartialPerm( [ 1, 2, 3, 6 ], [ 5, 2, 1, 3 ] ),
>  PartialPerm( [ 2, 3, 4, 6 ], [ 3, 2, 1, 6 ] ),
>  PartialPerm( [ 1, 2, 6 ], [ 3, 2, 6 ] ) ], rec(small:=true));
> IsInverseSemigroup(V);
> od;

#T# TestInstall37: Issue #63 (problem with Monoid and InverseMonoid when one
# of the arguments is a monoid). 
# This only works in GAP 4.7.5 or higher hence the CompareVersionNumbers
gap> S:=Semigroup(PartialPerm( [ 1, 2, 4, 5, 6 ], [ 1, 2, 4, 5, 6 ] ) );
<trivial partial perm group of rank 5 with 1 generator>
gap> T:=Monoid(S,  PartialPerm( [ 1, 2, 3, 4, 6 ], [ 2, 5, 4, 1, 3 ] ));;
gap> Length(GeneratorsOfMonoid(T))=2 
> or not CompareVersionNumbers(GAPInfo.Version,"4.7.5");
true
gap> One(S) in T or not CompareVersionNumbers(GAPInfo.Version,"4.7.5");
true
gap> One(S)=One(T);
false
gap> GeneratorsOfSemigroup(T)=
> [ PartialPerm( [ 1, 2, 3, 4, 5, 6 ] ), 
> PartialPerm([ 1, 2, 4, 5, 6 ], [ 1, 2, 4, 5, 6 ] ), 
> PartialPerm([ 1, 2, 3, 4, 6 ], [ 2, 5, 4, 1, 3 ] )]
> or not CompareVersionNumbers(GAPInfo.Version,"4.7.5");
true
gap> GeneratorsOfMonoid(T)=
> [ PartialPerm( [ 1, 2, 4, 5, 6 ], [ 1, 2, 4, 5, 6 ] ), 
>   PartialPerm( [ 1, 2, 3, 4, 6 ], [ 2, 5, 4, 1, 3 ] ) ]
> or not CompareVersionNumbers(GAPInfo.Version,"4.7.5");
true
gap> S:=InverseSemigroup(PartialPerm( [ 1, 2, 4, 5, 6 ], [ 1, 2, 4, 5, 6 ] ) );;
gap> T:=InverseMonoid(S,  PartialPerm( [ 1, 2, 3, 4, 6 ], [ 2, 5, 4, 1, 3 ] ));;
gap> Length(GeneratorsOfInverseMonoid(T))=2 
> or not CompareVersionNumbers(GAPInfo.Version,"4.7.5");
true
gap> GeneratorsOfMonoid(T)=
> [ PartialPerm( [ 1, 2, 4, 5, 6 ], [ 1, 2, 4, 5, 6 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 6 ], [ 2, 5, 4, 1, 3 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5 ], [ 4, 1, 6, 3, 2 ] ) ]
> or not CompareVersionNumbers(GAPInfo.Version,"4.7.5");
true
gap> GeneratorsOfSemigroup(T)=
> [ PartialPerm( [ 1, 2, 3, 4, 5, 6 ], [ 1, 2, 3, 4, 5, 6 ] ), 
>  PartialPerm( [ 1, 2, 4, 5, 6 ], [ 1, 2, 4, 5, 6 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 6 ], [ 2, 5, 4, 1, 3 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5 ], [ 4, 1, 6, 3, 2 ] ) ]
>  or not CompareVersionNumbers(GAPInfo.Version,"4.7.5");
true
gap> GeneratorsOfInverseMonoid(T)=
> [ PartialPerm( [ 1, 2, 4, 5, 6 ], [ 1, 2, 4, 5, 6 ] ), 
>   PartialPerm( [ 1, 2, 3, 4, 6 ], [ 2, 5, 4, 1, 3 ] ) ]
>   or not CompareVersionNumbers(GAPInfo.Version,"4.7.5");
true
gap> GeneratorsOfInverseSemigroup(T)=
> [ PartialPerm( [ 1, 2, 4, 5, 6 ], [ 1, 2, 4, 5, 6 ] ), 
>   PartialPerm( [ 1, 2, 3, 4, 6 ], [ 2, 5, 4, 1, 3 ] ), 
>   PartialPerm( [ 1, 2, 3, 4, 5, 6 ], [ 1, 2, 3, 4, 5, 6 ] ) ] 
>   or not CompareVersionNumbers(GAPInfo.Version,"4.7.5");
true
gap> One(S) in T or not CompareVersionNumbers(GAPInfo.Version,"4.7.5");
true

#T# TestInstall38: Issue 33 (problem with Rees factor semigroups)
gap> I := SemigroupIdealByGenerators(FullTransformationSemigroup(4), 
> [Transformation([1,2,2,2])]);
<regular transformation semigroup ideal of degree 4 with 1 generator>
gap> cong := ReesCongruenceOfSemigroupIdeal(I);
<Rees congruence of <regular transformation semigroup ideal of degree 4 with
  1 generator> over <full transformation monoid of degree 4>>
gap> hom := HomomorphismQuotientSemigroup(cong);
MappingByFunction( <full transformation monoid of degree 4>, <quotient of Mono\
id( [ Transformation( [ 2, 3, 4, 1 ] ), Transformation( [ 2, 1 ] ), 
  Transformation( [ 1, 2, 3, 1 ] ) ] ) by ReesCongruenceOfSemigroupIdeal( 
SemigroupIdeal( 
 Monoid( 
  [ Transformation( [ 2, 3, 4, 1 ] ), Transformation( [ 2, 1 ] ), Transformati\
on( [ 1, 2, 3, 1 ] ) ] ), [ Transformation( [ 1, 2, 2, 2 ] ) ] )
  )>, function( x ) ... end )
gap> T := Range(hom);
<quotient of Monoid( [ Transformation( [ 2, 3, 4, 1 ] ), 
  Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 3, 1 ] ) 
 ] ) by ReesCongruenceOfSemigroupIdeal( SemigroupIdeal( 
 Monoid( 
  [ Transformation( [ 2, 3, 4, 1 ] ), Transformation( [ 2, 1 ] ), Transformati\
on( [ 1, 2, 3, 1 ] ) ] ), [ Transformation( [ 1, 2, 2, 2 ] ) ] ) )>
gap> IsSemigroup(T);
true
gap> Size(T);
169
gap> u := Image(hom, Transformation([1,1,1,1]));
{Transformation( [ 1, 2, 2, 2 ] )}
gap> t := Image(hom, Transformation([2,1,2,3]));
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
gap> M:=InverseMonoid( PartialPerm([1,2]), PartialPerm([1]) );
<inverse partial perm monoid of rank 2 with 2 generators>
gap> One(M) in M;
true
gap> AsSet(M);
[ <identity partial perm on [ 1 ]>, <identity partial perm on [ 1, 2 ]> ]
gap> M:=InverseMonoid( PartialPerm([1,2]), PartialPerm([1]) );
<inverse partial perm monoid of rank 2 with 2 generators>
gap> AsSet(M);
[ <identity partial perm on [ 1 ]>, <identity partial perm on [ 1, 2 ]> ]

#T# TestInstall40: Issue 4 in Orb (logs are not properly updated if the 
# enumeration stops early because we find something we are looking for)
gap> gens:=[ PartialPerm( [ 1, 2, 4, 5, 6 ], [ 1, 2, 4, 5, 6 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 6 ], [ 2, 5, 4, 1, 3 ] ) ];;
gap> o:=Orb([gens[1]], [0], OnPosIntSetsPartialPerm, rec(log:=true,   
> lookingfor:=function(o, x) return x=[1,2,4,5,6]; end));
<open orbit, 1 points looking for sth. with log>
gap>  Enumerate(o);
<open orbit, 2 points looking for sth. with log>
gap> o!.looking:=false;
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
gap> B:=Monoid( BinaryRelationOnPoints( [ [ 2 ], [ 1, 2 ], [ 1, 3 ] ] ),
> BinaryRelationOnPoints( [ [ 3 ], [ 1, 2 ], [ 1, 3 ] ] ), 
> BinaryRelationOnPoints( [ [ 1, 2, 3 ], [ 1, 2 ], [ 3 ] ] ) );; 
gap> Size(B);
16
gap> IsMonoid(B);
true
gap> iso:=IsomorphismTransformationSemigroup(B);;
gap> T:=Range(iso);
<transformation monoid of degree 6 with 3 generators>
gap> Size(T);
16
gap> IsMonoid(T);
true

#T# TestInstall42: Issue 82 (couldn't take quotients by ideals!)
gap> S:=Monoid( [ Transformation( [ 3, 3, 3, 3 ] ),
>  Transformation( [ 2, 4, 2, 4 ] ), 
>  Transformation( [ 2, 3, 2, 3 ] ), Transformation( [ 4, 1, 4, 3 ] ), 
>  Transformation( [ 1, 4, 4, 1 ] ), Transformation( [ 2, 2, 3, 1 ] ), 
>  Transformation( [ 2, 4, 3, 4 ] ), Transformation( [ 2, 2, 1, 2 ] ), 
>  Transformation( [ 2, 2, 1, 3 ] ), Transformation( [ 1, 2, 2, 3 ] ), 
>  Transformation( [ 2, 4, 3, 2 ] ), Transformation( [ 2, 3, 3, 3 ] ) ] );;
gap> I:=SemigroupIdeal(S, S.3);;
gap> S/I;
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
gap> S:=Semigroup( [ Transformation( [ 2, 1, 3, 1, 4, 3 ] ), 
>  Transformation( [ 2, 2, 2, 2, 1, 2 ] ), Transformation( [ 5, 3, 4, 3, 5 ] ),
> Transformation( [ 6, 4, 1, 4, 5, 3 ] ), 
>  Transformation( [ 6, 5, 2, 6, 3, 4 ] ) ] );;
gap> NrIdempotents(S)=Number(HClasses(S), IsGroupHClass)
> or not CompareVersionNumbers(GAPInfo.Version,"4.7.5");
true

#T# TestInstall44: Issue 96 (problem with using the partial order of D-classes
# as an isomorphism invariant)
gap> S:=Semigroup( [ Transformation( [ 1, 2, 1 ] ),
> Transformation( [ 1, 2, 3, 2 ] ), 
> Transformation( [ 2, 2, 2 ] ),
> Transformation( [ 4, 2, 1, 4 ] ) ]);;
gap> T:=Semigroup( Transformation( [ 1, 2, 3, 1 ] ),
> Transformation( [ 2, 2, 3, 1 ] ), 
> Transformation( [ 2, 3, 3, 1 ] ), Transformation( [ 1, 3, 3 ] ), 
> Transformation( [ 2, 3, 3, 3 ] ), Transformation( [ 3, 2, 3, 3 ] ));;
gap> (not (IsBound(GAPInfo.PackagesLoaded.grape) 
> and Filename(DirectoriesPackagePrograms("grape"),"dreadnautB")<>fail)) or 
> (IsBound(GAPInfo.PackagesLoaded.grape) 
> and Filename(DirectoriesPackagePrograms("grape"),"dreadnautB")<>fail 
> and IsIsomorphicSemigroup(S, T));
true

#T# TestInstall45: Issue 97
# (bug in normalizer and the kernel function POW_KER_TRANS)
gap> if CompareVersionNumbers(GAPInfo.Version,"4.7.6") then 
> G:=Normalizer(SymmetricGroup(3), Semigroup(IdentityTransformation));
> else 
> G:=SymmetricGroup(3);
> fi;
gap> G;
Group([ (2,3), (1,2,3) ])

#T# TestInstall46: Issue 98
# (incorrect definition of partition monoid on 1 point)
gap> GeneratorsOfSemigroup(PartitionMonoid(1));
[ <block bijection: [ 1, -1 ]>, <bipartition: [ 1 ], [ -1 ]> ]

#T# TestInstall47: Issue 101 (incorrect method for 
# DegreeOfTransformationSemigroup for a transformation group with 0 generators)
gap> if CompareVersionNumbers(GAPInfo.Version,"4.7.6") then 
> G:=GroupOfUnits(FullTransformationSemigroup(1));
> else
> G:=Semigroup(IdentityTransformation);
> fi;
gap> G;
<trivial transformation group of degree 0 with 1 generator>

#T# TestInstall48: Issue 101
# (incorrect method for AsPartialPerm for a perm and zero)
gap> if CompareVersionNumbers(GAPInfo.Version,"4.7.6") then 
> G:=GroupOfUnits(Semigroup(PartialPerm([])));
> else
> G:=Semigroup(PartialPerm([]));
> fi;
gap> G;
<trivial partial perm group of rank 0 with 1 generator>

#T# TestInstall49: Issue 103
# (problem with Enumerate(LambdaOrb(I)) when T is an inverse semigroup but 
# doesn't know it at the start)
gap> S:=POI(5);;
gap> T:=Semigroup(S, PartialPerm([1,2,3,4,5],[2,3,4,5,1]));;
gap> I:=SemigroupIdeal(T, [ PartialPerm( [ 1, 2, 4, 5 ], [ 1, 2, 3, 5 ] )]);
<inverse partial perm semigroup ideal of rank 5 with 1 generator>
gap> Size(I);
626

#T# TestInstall50: Issue 105 (CyclesOfPartialPerm returned nonsense)
gap> x:=PartialPerm( [ 1, 2, 3, 4, 5, 8, 10 ], [ 3, 1, 4, 2, 5, 6, 7 ] );;
gap> CyclesOfPartialPerm(x);
[ [ 3, 4, 2, 1 ], [ 5 ] ]

#T# TestInstall51: Issue 107
# (problems with Green's classes of ideals, and inverse semigroups)
gap> S := Monoid( [ PartialPermNC( [ 1 ], [ 1 ] ),
> PartialPermNC( [ 1 ], [ 2 ] ),
> PartialPermNC( [ 2 ], [ 1 ] ) ] );;
gap> I := SemigroupIdeal( S, PartialPermNC( [ ], [ ] ) );;
gap> GeneratorsOfSemigroup(I);
[ <empty partial perm> ]

#T# TestInstall52: Issue 107 (problems with Green's classes of ideals, and 
# inverse semigroups)
gap> S:=[SymmetricInverseMonoid(2)];;
gap> S[2]:=MaximalSubsemigroups(S[1]);;
gap> if CompareVersionNumbers(GAPInfo.Version,"4.7.7")
> and (IsBound(GAPInfo.PackagesLoaded.grape) 
> and Filename(DirectoriesPackagePrograms("grape"),"dreadnautB")<>fail) then
> S[3]:=List(S[2], MaximalSubsemigroups);;
> fi;

#T# TestInstall53: Issue 109 (problem with IsReesZeroMatrixSemigroup on the
# subsemigroup generated by 0)
gap> R1 := ReesZeroMatrixSemigroup(Group(()), [ [ () ] ]);;
gap> R2 := Semigroup(MultiplicativeZero(R1));;
gap> IsReesZeroMatrixSubsemigroup(R2);
true
gap> (CompareVersionNumbers(GAPInfo.Version,"4.7.7") and
> IsReesZeroMatrixSemigroup(R2));
false

#T# TestInstall54: FreeBand
gap> s := FreeBand("a", "b", "c", "d", "e");
<free band on the generators [ a, b, c, d, e ]>
gap> iter := Iterator(s);
<iterator>
gap> for i in [1 .. 100] do
> NextIterator(iter);
> od;
gap> x := NextIterator(iter);
bcabaca
gap> for i in [1 .. 10] do
> NextIterator(iter);
> od;
gap> y := NextIterator(iter);
cbcacbab
gap> x*y;
bcacbab
gap> x^2;
bcabaca
gap> y^2;
cbcacbab

#T# TestInstall55: Issue 110 (MaximalSubsemigroups for an non-regular RZMS)
gap> S := [ ReesZeroMatrixSemigroup( Group( () ), [ [ (), 0 ], [ 0, () ] ] ) ];;
gap> S[2] := Semigroup(RMSElement(S[1], 2, (), 2), RMSElement(S[1], 1, (), 2));;
gap> (IsBound(GAPInfo.PackagesLoaded.grape) 
> and Filename(DirectoriesPackagePrograms("grape"),"dreadnautB")<>fail 
> and IsDuplicateFreeList(MaximalSubsemigroups(S[2]))
> and ForAll(MaximalSubsemigroups(S[2]), x -> IsMaximalSubsemigroup(S[2], x))
> and Length(MaximalSubsemigroups(S[2])) = 2)
> or (not (IsBound(GAPInfo.PackagesLoaded.grape)
> and Filename(DirectoriesPackagePrograms("grape"),"dreadnautB")<>fail));
true

#T# TestInstall56: Issue 122 (Problem with XClassType for inverse ideals)
gap> S := Semigroup(
> PartialPerm( [ 1, 2, 3, 4 ], [ 2, 3, 4, 1 ] ),
> PartialPerm( [ 1, 2, 3, 4 ], [ 2, 1, 3, 4 ] ),
> PartialPerm( [ 1, 3 ], [ 2, 3 ] ) );;
gap> x := PartialPerm( [  ], [  ] );;
gap> I := SemigroupIdeal(S, x);;
gap> L := GreensLClassOfElement(I, x);
<Green's L-class: <empty partial perm>>
gap> SchutzenbergerGroup(L); 
Group(())

#T# TestInstall57: Issue 123 (Incorrect method for IsZeroSemigroup for
# non-acting semigroup)
gap> x := Transformation([ 1, 1, 2, 3 ]);;
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
gap> t := Transformation( [ 1 ] );;
gap> s := Semigroup(t);;
gap> MultiplicativeZero(s) = t;
true
gap> gens := [ Transformation( [ 1, 2, 1 ] ), Transformation( [ 1, 2, 2 ] ) ];;
gap> S := Semigroup(gens);;
gap> IsRectangularBand(S) and not IsTrivial(S);
true
gap> MultiplicativeZero(S);
fail

#T# TestInstall59: Issue 88:
# Something called by `JoinIrreducibleDClasses` of an ideal of a bipartition
# semigroup calls `GeneratorsOfSemigroup`
gap> S := DualSymmetricInverseMonoid(6);;
gap> x := Bipartition( [ [ 1, 2, -3 ], [ 3, -1, -2 ], [ 4, -4 ], 
> [ 5, -5 ], [ 6, -6 ] ] );;
gap> I := SemigroupIdeal(S, x);
<inverse bipartition semigroup ideal of degree 6 with 1 generator>
gap> JoinIrreducibleDClasses(I);
[ <Green's D-class: <block bijection: [ 1, 2, 3, 4, 5, -1, -2, -3, -4, -5 ], 
      [ 6, -6 ]>> ]
gap> I;
<inverse bipartition semigroup ideal of degree 6 with 1 generator>

#T# TestInstall60: Issue 94:
# EquivalenceClasses of trivial congruence returns empty list
gap> S:=FullTransformationSemigroup(6);;
gap> R:=PrincipalFactor(MinimalDClass(S));
<Rees matrix semigroup 1x6 over Group(())>
gap> cong := SemigroupCongruenceByGeneratingPairs(R, [ ]);;
gap> EquivalenceClasses(cong);
[ {(1,(),1)}, {(1,(),2)}, {(1,(),3)}, {(1,(),4)}, {(1,(),5)}, {(1,(),6)} ]

#T# TestInstall61: Issue 95:
# No zero class in semigroup congruence EquivalenceClasses (generating pairs)
gap> P := [ [ (), 0, (1,3), (1,3), 0, (), 0 ],
>   [ (), (1,3), 0, 0, (1,3), (), 0 ], [ (), (1,3), 0, (), 0, 0, () ],
>   [ 0, (), (1,3), (1,3), (), 0, 0 ], [ 0, 0, 0, (), (), (1,3), () ],
>   [ (), 0, (1,3), 0, (), 0, () ] ];;
gap> R := ReesZeroMatrixSemigroup(Group([(1,3)]), P);;
gap> x := ReesZeroMatrixSemigroupElement(R, 1, (1,3), 1);;
gap> y := ReesZeroMatrixSemigroupElement(R, 1, (), 1);;
gap> cong := SemigroupCongruenceByGeneratingPairs(R, [[x,y]]);;
gap> c := EquivalenceClasses(cong);
[ {0}, {(1,(),1)}, {(1,(),2)}, {(1,(),3)}, {(1,(),4)}, {(1,(),5)}, 
  {(1,(),6)}, {(2,(),1)}, {(2,(),2)}, {(2,(),3)}, {(2,(),4)}, {(2,(),5)}, 
  {(2,(),6)}, {(3,(),1)}, {(3,(),2)}, {(3,(),3)}, {(3,(),4)}, {(3,(),5)}, 
  {(3,(),6)}, {(4,(),1)}, {(4,(),2)}, {(4,(),3)}, {(4,(),4)}, {(4,(),5)}, 
  {(4,(),6)}, {(5,(),1)}, {(5,(),2)}, {(5,(),3)}, {(5,(),4)}, {(5,(),5)}, 
  {(5,(),6)}, {(6,(),1)}, {(6,(),2)}, {(6,(),3)}, {(6,(),4)}, {(6,(),5)}, 
  {(6,(),6)}, {(7,(),1)}, {(7,(),2)}, {(7,(),3)}, {(7,(),4)}, {(7,(),5)}, 
  {(7,(),6)} ]
gap> ForAny(c, x->MultiplicativeZero(R) in x);
true

#T# TestInstall62: Issue 119:
# Bug in NrCongruenceClasses for Rees congruences
gap> I := SemigroupIdealByGenerators(FullTransformationSemigroup(4),
> [Transformation([1,2,2,2])]);
<regular transformation semigroup ideal of degree 4 with 1 generator>
gap> cong := ReesCongruenceOfSemigroupIdeal(I);
<Rees congruence of <regular transformation semigroup ideal of degree 4 with
  1 generator> over <full transformation monoid of degree 4>>
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
<transformation semigroup of degree 22 with 3 generators>
gap> Size(T);
21
gap> I := SemigroupIdeal(T, Idempotents(T));
<regular transformation semigroup ideal of degree 22 with 8 generators>
gap> Size(I);
21

#T# TestInstall64: Bug fixed by changeset 949553d
gap> s := InverseSemigroup(PartialPerm([1], [2]), PartialPerm([2], [1]));
<inverse partial perm semigroup of rank 2 with 2 generators>
gap> Size(s);
5
gap> SemigroupCongruence(s, [s.1, s.1 * s.2]);
<universal semigroup congruence over <0-simple inverse partial perm semigroup 
 of size 5, rank 2 with 2 generators>>

#T# TestInstall65: Issue #131
gap> S := FullTransformationSemigroup(3);;
gap> I := SemigroupIdeal(FullTransformationSemigroup(3), Transformation([1,1,2]));;
gap> T := S/I;;
gap> One(T);
{IdentityTransformation}

#T# TestInstall66: Second bug in Issue #131
gap> I := SemigroupIdeal(FullTransformationSemigroup(3), Transformation([1,1,1]));;
gap> hom:=HomomorphismQuotientSemigroup(ReesCongruenceOfSemigroupIdeal(I));;
gap> map:=IsomorphismTransformationSemigroup(Range(hom));;

#T# Checking for correct non-removal of one from generating sets in
# SemigroupByGenerators JDM
gap> S := Semigroup(PartialPerm([1]));
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
gap> S := Semigroup(IdentityTransformation);
<trivial transformation group of degree 0 with 1 generator>
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

#T# Checking for correct non-removal of one from generating sets in
# MonoidByGenerators JDM
gap> S := Monoid(PartialPerm([1]));
<trivial partial perm group of rank 1 with 1 generator>
gap> S := Monoid(PartialPerm([1]));
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
gap> S := Monoid(IdentityTransformation);
<trivial transformation group of degree 0 with 1 generator>
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

#T# Checking for correct non-removal of one from generating sets in
# InverseSemigroupByGenerators JDM
gap> S := InverseSemigroup(PartialPerm([1]));
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
gap> S := InverseSemigroup(IdentityTransformation);
<trivial transformation group of degree 0 with 1 generator>
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

#T# Checking for correct non-removal of one from generating sets in
# InverseMonoidByGenerators JDM
gap> S := InverseMonoid(PartialPerm([1]));
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
gap> S := InverseMonoid(IdentityTransformation);
<trivial transformation group of degree 0 with 1 generator>
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

#T# Checking GroupByGenerators
gap> S := Group(PartialPerm([1]));
<partial perm group of rank 1 with 1 generator>
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
gap> S := Group(IdentityTransformation);
<transformation group of degree 0 with 1 generator>
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

#T# Test for Issue 136
gap> S := Semigroup(PartialPerm([2, 0]));
<commutative partial perm semigroup of rank 1 with 1 generator>
gap> CyclesOfPartialPermSemigroup(S);
[  ]

#T# Test for Issue 141
gap> S := Semigroup(Bipartition ([[1, 4], [2, 3], [-1, -4], [-2, -3]]),
>                   Bipartition([[1, 2], [3, 4], [-1, -4], [-2, -3]]));;
gap> PartialOrderOfDClasses(S);
[ [ 1 ] ]

#T# Test for Issue 144
gap> S := Semigroup([ Bipartition( [ [ 1, 2 ], [ -1 ], [ -2 ] ] ), 
>   Bipartition( [ [ 1, -1 ], [ 2 ], [ -2 ] ] ), 
>   Bipartition( [ [ 1 ], [ 2, -1 ], [ -2 ] ] ), 
>   Bipartition( [ [ 1, -2 ], [ 2 ], [ -1 ] ] ), 
>   Bipartition( [ [ 1 ], [ 2, -2 ], [ -1 ] ] ), 
>   Bipartition( [ [ 1 ], [ 2 ], [ -1 ], [ -2 ] ] ) ]);;
gap> IsInverseSemigroup(S);
false
gap> S := Semigroup([ Bipartition( [ [ 1, 2 ], [ -1 ], [ -2 ] ] ), 
>   Bipartition( [ [ 1, -1 ], [ 2 ], [ -2 ] ] ), 
>   Bipartition( [ [ 1 ], [ 2, -1 ], [ -2 ] ] ), 
>   Bipartition( [ [ 1, -2 ], [ 2 ], [ -1 ] ] ), 
>   Bipartition( [ [ 1 ], [ 2, -2 ], [ -1 ] ] ), 
>   Bipartition( [ [ 1 ], [ 2 ], [ -1 ], [ -2 ] ] ) ]);;
gap> NrDClasses(S);;
gap> IsInverseSemigroup(S);
false

#T# MaximalSubsemigroups, replacement test for manual example which becomes a
#log because of the randomness in the generating sets here.
gap> S := FullTransformationMonoid(4);;
gap> (not IsGrapeCompiled) or Length(MaximalSubsemigroups(S)) = 9;
true
gap> (not IsGrapeCompiled) or ForAll(MaximalSubsemigroups(S), M -> M in 
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

#T# Test for not being allowed to generate a semigroup with bipartitions of
# different degree
gap> Semigroup(Bipartition([[-1,1]]), Bipartition([]));
Error, Usage: Semigroup(<gen>,...), Semigroup(<gens>), Semigroup(<D>),

#T# Issue 150: Bug in RepresentativeOfMinimalIdeal
gap> S := Semigroup([PartialPerm([1, 2], [3, 2])]);;
gap> RepresentativeOfMinimalIdeal(S);
<identity partial perm on [ 2 ]>
gap> IsZeroSimpleSemigroup(S);
false

#T# SEMIGROUPS_UnbindVariables
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
gap> Unbind(map);
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
gap> Unbind(I);
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
