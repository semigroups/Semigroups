#############################################################################
##
#W  testinstall.tst
#Y  Copyright (C) 2011-12                               James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# ReadTest(Filename(DirectoriesPackageLibrary("semigroups","tst"),"testinstall.tst"));
gap> START_TEST("Semigroups package: testinstall.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels
gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoSemigroups:=InfoLevel(InfoSemigroups);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoSemigroups, 0);
gap> display:=UserPreference("PartialPermDisplayLimit");;
gap> notationpp:=UserPreference("NotationForPartialPerms");;
gap> notationt:=UserPreference("NotationForTransformations");;
gap> SetUserPreference("PartialPermDisplayLimit", 100);;
gap> SetUserPreference("TransformationDisplayLimit", 100);;
gap> SetUserPreference("NotationForPartialPerms", "component");;
gap> SetUserPreference("NotationForTransformations", "input");;

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

#gap> s:=Semigroup(ReadSemigroups("pkg/semigroups/examples/graph9c.semigroups.gz", 100013));;
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
gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoSemigroups, InfoLevelInfoSemigroups);;
gap> SetUserPreference("PartialPermDisplayLimit", display);;
gap> SetUserPreference("NotationForPartialPerm", notationpp);;
gap> SetUserPreference("NotationForTransformations", notationt);;
gap> Unbind(InfoLevelInfoSemigroups);; Unbind(InfoLevelInfoWarning);;
gap> Unbind(notationt);; Unbind(notationpp);;
gap> STOP_TEST( "Semigroups package: testinstall.tst", 10000);
