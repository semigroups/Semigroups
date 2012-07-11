#############################################################################
##
#W  data.tst
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Citrus package: data.tst");
gap> LoadPackage("citrus", false);;

#
gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoCitrus:=InfoLevel(InfoCitrus);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoCitrus, 0);

#
gap> s:=Semigroup(Transformation( [ 2, 1, 4, 5, 6, 3 ] ), 
> Transformation( [ 2, 3, 1, 5, 4, 1 ] ));;
gap> r:=GreensRClassOfElement(s,
> Generators(s)[1]*Generators(s)[2]*Generators(s)[1]);
{Transformation( [ 3, 2, 5, 4, 1, 1 ] )}
gap> Transformation( [ 4, 1, 6, 5, 2, 2 ] ) in r;
true
gap> Representative(r);
Transformation( [ 3, 2, 5, 4, 1, 1 ] )
gap> AsList(LambdaOrb(r)){OrbSCC(LambdaOrb(r))[LambdaOrbSCCIndex(r)]};
[ [ 1, 2, 3, 4, 5 ], [ 1, 2, 4, 5, 6 ], [ 1, 2, 3, 5, 6 ], [ 1, 2, 3, 4, 6 ] ]
gap> LambdaOrbMults(LambdaOrb(r),
> LambdaOrbSCCIndex(r)){OrbSCC(LambdaOrb(r))[LambdaOrbSCCIndex(r)]};
[ (), (1,2,3,6)(4,5), (3,5)(4,6), (1,2)(3,4,5,6) ]

#
gap> gens:= [ Transformation( [ 4, 3, 3, 6, 7, 2, 3 ] ),
>   Transformation( [ 6, 6, 4, 4, 2, 1, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Length(GreensRClasses(s));
17
gap> r:=GreensRClasses(s)[10];;
gap> Representative(r);
Transformation( [ 3, 3, 3, 3, 3, 2, 3 ] )
gap> AsList(LambdaOrb(r){OrbSCC(LambdaOrb(r))[LambdaOrbSCCIndex(r)]});
[ [ 2, 3 ], [ 4, 6 ], [ 2, 6 ], [ 1, 4 ], [ 1, 6 ], [ 2, 4 ], [ 3, 6 ] ]
gap> LambdaOrbMults(LambdaOrb(r),
> LambdaOrbSCCIndex(r)){OrbSCC(LambdaOrb(r))[LambdaOrbSCCIndex(r)]};
[ (), (2,4)(3,5,6), (2,3,4,5,6), (1,2)(3,4), (1,2)(3,4,5,6), (2,3,4), 
  (2,4,5,6) ]

#
gap> gens:=[Transformation( [ 8, 7, 5, 3, 1, 3, 8, 8 ] ),
> Transformation( [ 5, 1, 4, 1, 4, 4, 7, 8 ] )];;
gap> s:=Monoid(gens);;
gap> f:=Transformation( [ 8, 8, 1, 5, 8, 5, 8, 8 ] );;
gap> f in SemigroupData(s);
false
gap> iter:=IteratorOfRClasses(s);
<iterator of R-classes>
gap> NextIterator(iter);;
gap> NextIterator(iter);
{Transformation( [ 8, 7, 5, 3, 1, 3, 8, 8 ] )}
gap> f in SemigroupData(s);
false
gap> NextIterator(iter);
{Transformation( [ 5, 1, 4, 1, 4, 4, 7, 8 ] )}
gap> f in SemigroupData(s);
true

#
gap> S:=Semigroup([ Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 3, 4, 1 ] ) ]);;
gap> RhoOrb(S);
<open orbit, 1 points with Schreier tree with log>
gap> Enumerate(last);
<closed orbit, 9 points with Schreier tree with log>
gap> AsList(last);
[ [ 1, 2, 3, 4 ], [ 1, 2, 3, 1 ], [ 1, 1, 2, 3 ], [ 1, 2, 2, 1 ], 
  [ 1, 1, 2, 2 ], [ 1, 2, 1, 1 ], [ 1, 1, 1, 2 ], [ 1, 1, 1, 1 ], 
  [ 1, 1, 2, 1 ] ]

#
gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;

#
gap> Unbind(gens); Unbind(s); Unbind(f); Unbind(r); Unbind(iter); 
gap> STOP_TEST( "Citrus package: data.tst", 0);
