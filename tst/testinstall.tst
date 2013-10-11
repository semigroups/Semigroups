#############################################################################
##
#W  testinstall.tst
#Y  Copyright (C) 2011-13                               James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# ReadTest(Filename(DirectoriesPackageLibrary("semigroups","tst"),"testinstall.tst"));
gap> START_TEST("Semigroups package: testinstall.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels and user preferences
gap> SemigroupsStartTest();

#
gap> s:=Semigroup(Transformation( [ 2, 3, 4, 1, 1, 1 ] ));;
gap> IsMonoidAsSemigroup(s);
true
gap> IsMonoid(s);
false
gap> iso:=IsomorphismTransformationMonoid(s);
MappingByFunction( <commutative transformation semigroup 
 on 6 pts with 1 generator>, <commutative transformation monoid 
 on 4 pts with 1 generator>, function( f ) ... end, function( f ) ... end )
gap> RespectsMultiplication(iso);
true
gap> ForAll(s, x-> (x^iso)^InverseGeneralMapping(iso)=x);
true

#
gap> s:=Semigroup(Transformation([1,1,1]), Transformation([3,1,2]));
<transformation semigroup on 3 pts with 2 generators>
gap> IsSimpleSemigroup(s);
false

#
gap> s:=SingularTransformationSemigroup(6);
<regular transformation semigroup on 6 pts with 30 generators>
gap> Size(s);
45936

#
gap> s:=Semigroup(IdentityTransformation);;
gap> LambdaOrb(s);
<open orbit, 1 points with Schreier tree with log>
gap> Enumerate(last);
<closed orbit, 2 points with Schreier tree with log>

# 
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
<transformation semigroup on 4 pts with 3 generators>
gap> ForAll(t, x-> x in s);
true
gap> Size(t);
60

# Issue 2
gap> s:=Semigroup(Transformation([4,4,4,4]));;
gap> AsList(s);
[ Transformation( [ 4, 4, 4, 4 ] ) ]

# Issue 3
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

# Issue 9
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

#gap> ForAll([1..NrRClasses(s)], i->
#> EvaluateWord(Generators(s), TraceRClassRepsTree(s, i))=
#> RClassReps(s)[i]);
#true

#
gap> gens:=[ Transformation( [ 1, 2, 3, 5, 4, 6, 7, 8 ] ),
>   Transformation( [ 4, 4, 3, 1, 5, 6, 3, 8 ] ),
>   Transformation( [ 3, 6, 1, 7, 3, 4, 8, 3 ] ),
>   Transformation( [ 1, 2, 3, 4, 5, 3, 7, 8 ] ),
>   Transformation( [ 1, 2, 3, 4, 1, 6, 7, 8 ] ),
>   Transformation( [ 8, 8, 3, 4, 5, 7, 6, 1 ] ) ];;
gap> s:=Monoid(gens);
<transformation monoid on 8 pts with 6 generators>
gap> t:=ClosureSemigroup(s, [Transformation( [ 4, 4, 3, 1, 5, 6, 3, 8 ] )]);
<transformation monoid on 8 pts with 6 generators>
gap> Size(t)=Size(Semigroup(Generators(t)));
true

#gap> s:=Semigroup(ReadGenerators("pkg/semigroups/examples/graph9c.semigroups.gz", 100013));;
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
{Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] )}
gap> ll:=LClass(s, f);
{Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] )}
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
  Transformation( [ 1, 8, 4, 2, 7, 8, 8, 5 ] ), 
  Transformation( [ 7, 7, 4, 2, 1, 8, 8, 9, 5 ] ), 
  Transformation( [ 7, 7, 4, 2, 1, 8, 8, 5 ] ) ]
gap> HClassReps(ll);
[ Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] ), 
  Transformation( [ 1, 8, 4, 2, 7, 8, 8, 5 ] ), 
  Transformation( [ 7, 7, 4, 2, 1, 8, 8, 9, 5 ] ), 
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

# IsomorphismTransformationSemigroup/Monoid
gap> IsomorphismTransformationSemigroup(g);
MappingByFunction( Group([ (5,9), (1,7) ]), <transformation semigroup 
 of size 4, on 4 pts with 2 generators>
 , function( x ) ... end, function( x ) ... end )
gap> s:=Range(last);
<transformation semigroup of size 4, on 4 pts with 2 generators>
gap> IsGroupAsSemigroup(s);
true
gap> Generators(s);
[ Transformation( [ 1, 4, 3, 2 ] ), Transformation( [ 3, 2, 1 ] ) ]
gap> t:=Range(IsomorphismTransformationMonoid(g));
<transformation monoid of size 4, on 4 pts with 2 generators>
gap> Generators(t);
[ Transformation( [ 1, 4, 3, 2 ] ), Transformation( [ 3, 2, 1 ] ) ]
gap> h:=Range(IsomorphismPermGroup(t));
Group([ (2,4), (1,3) ])
gap> IsomorphismGroups(g, h);
[ (5,9), (1,7) ] -> [ (2,4), (1,3) ]

#Issue 22 - takes about 49ms
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

#
gap> file:=Concatenation(SemigroupsDir(), "/examples/munn.semigroups.gz");;
gap>  ReadGenerators(file, 1376);
[ <identity partial perm on [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]>, 
  <identity partial perm on [ 1, 2, 3, 4, 5, 6, 7, 9 ]>, 
  <identity partial perm on [ 1, 2, 3, 4, 5, 6, 9 ]>, 
  <identity partial perm on [ 1, 2, 3, 4, 5, 9 ]>, 
  <identity partial perm on [ 1, 2, 3, 4, 9 ]>, 
  <identity partial perm on [ 1, 2, 3, 9 ]>, 
  <identity partial perm on [ 1, 2, 9 ]>, <identity partial perm on [ 1, 9 ]> 
 ]

#
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
<inverse partial perm semigroup on 4 pts with 3 generators>
gap> ForAll(t, x-> x in s);
true
gap> Size(t);
98

#
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
{PartialPerm( [ 2, 10 ], [ 2, 10 ] )}
gap> F:=IsomorphismReesMatrixSemigroup(d);;
gap> G:=InverseGeneralMapping(F);;
gap> ForAll(d, f-> (f^F)^G=f);        
true

# from JS' MultiplicativeZero.tst
gap> s:=InverseMonoid( PartialPerm( [1,2,3,4] ),
> PartialPerm( [1,3,2,4] ),
> PartialPerm( [1,2,0,0] ),
> PartialPerm( [1,0,0,4] ) );;
gap> f := PartialPerm( [1,0,0,0] );;
gap> f in s;
true
gap> ForAll(s, x -> f*x = f and x*f = f );
true
gap> f;
<identity partial perm on [ 1 ]>
gap> MultiplicativeZero(s);
<identity partial perm on [ 1 ]>

# from JS' PartialPermInjective.tst
gap> PartialPerm( [0,0,1,2] );
[3,1][4,2]

# from JS' RestricterPartialPerm.tst
gap> x:=PartialPerm([2..7],[1..6]); RestrictedPartialPerm(x,[2..7]);
[7,6,5,4,3,2,1]
[7,6,5,4,3,2,1]

# from JS' SizeInverseMonoid.tst
gap> s:=InverseMonoid( PartialPerm( [1,2,3,4,5,6,7,8] ),
> PartialPerm( [1,6,3,4,8,2,7,5] ),
> PartialPerm( [1,2,7,4,8,6,3,5] ),
> PartialPerm( [1,0,0,4,5,0,0,8] ),
> PartialPerm( [1,2,0,4,0,6,0,0] ),
> PartialPerm( [1,0,3,4,0,0,7,0] ),
> PartialPerm( [1,0,0,0,0,0,0,0] ));;
gap> [ Size( s ), Size( AsSet( s ) ) ];
[ 12, 12 ]

# from JS' email
gap> s:=InverseMonoid( PartialPerm( [1,3,2] ), PartialPerm([1]) );;
gap> [ Size( s ), Size( AsSet( s ) ) ];
[ 3, 3 ]
gap> Elements(s); 
[ <identity partial perm on [ 1 ]>, <identity partial perm on [ 1, 2, 3 ]>, 
  (1)(2,3) ]

#
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

# Issue 27 in the new numbering...
gap> s:=Semigroup(List(GeneratorsOfSemigroup(FullTransformationSemigroup(3)),
>  x-> AsTransformation(x, 4)));;
gap> IsFullTransformationSemigroup(s);
true
gap>  IdentityTransformation in s; 
true
gap>  IdentityTransformation in s;
true

# Issue 23 in the new numbering...
gap> S:=FullTransformationSemigroup(3);;
gap> f:=Transformation( [ 4, 3, 1, 2 ] );;
gap> ClosureSemigroup(S, f);           
<transformation monoid on 4 pts with 4 generators>

# Issue 36 in the new numbering...
gap> S:=Semigroup(IdentityTransformation);
<trivial transformation group>
gap> SmallGeneratingSet(S);
[  ]

# MaximalSubsemigroups of Rees 0-matrix semigroups
gap> G:=Group((1,2),(3,4));;
gap> mat:=[[(), ()], [(), 0], [(), (1,2)]];;
gap> R:=ReesZeroMatrixSemigroup(G, mat);
<Rees 0-matrix semigroup 2x3 over Group([ (1,2), (3,4) ])>
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 2x3 Rees 0-matrix semigroup with 4 generators>, 
  <Rees 0-matrix semigroup 2x2 over Group([ (1,2), (3,4) ])>, 
  <Rees 0-matrix semigroup 2x2 over Group([ (3,4), (1,2) ])>, 
  <Rees 0-matrix semigroup 2x2 over Group([ (1,2), (3,4) ])>, 
  <Rees 0-matrix semigroup 1x3 over Group([ (3,4), (1,2) ])>, 
  <subsemigroup of 2x3 Rees 0-matrix semigroup with 9 generators> ]
gap> List(last, U-> IsMaximalSubsemigroup(R, U));
[ true, true, true, true, true, true ]

# ClosureSemigroup with an element of higher degree
gap> S:=Semigroup( 
> Transformation( [ 1, 3, 3, 2 ] ), Transformation( [ 4, 1, 4, 2 ] ), 
> Transformation( [ 4, 2, 3, 3 ] ), Transformation( [ 4, 4, 4, 4 ] ) );;
gap> Size(S);
130
gap> f:=Transformation( [ 3, 5, 1, 5, 2 ] );;
gap> T:=ClosureSemigroup(S, f);;
gap> Size(T);
1619

# bug with Elements and IsomorphismPermGroup for group H-class
gap> R:=ReesZeroMatrixSemigroup(Group(()), 
> [ [ (), (), () ], [ (), 0, 0 ], [ (), 0, 0 ] ]);
<Rees 0-matrix semigroup 3x3 over Group(())>
gap> R:=ReesZeroMatrixSubsemigroup(R, [2,3], Group(()), [2,3]);
<Rees 0-matrix semigroup 2x2 over Group(())>
gap> H:=First(HClasses(R), IsGroupHClass);
{0}
gap> Elements(H);   
[ 0 ]
gap> f:=IsomorphismPermGroup(H);;
gap> g:=InverseGeneralMapping(f);;
gap> Elements(H)[1]^f;
()
gap> ()^g;            
0

#
gap> SemigroupsStopTest();
gap> STOP_TEST( "Semigroups package: testinstall.tst", 10000);
