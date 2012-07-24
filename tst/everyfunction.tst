#############################################################################
##
#W  everyfunction.tst
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

gap> START_TEST("Citrus package: everyfunction.tst");
gap> LoadPackage("citrus", false);;

#
gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoCitrus:=InfoLevel(InfoCitrus);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoCitrus, 0);

#
gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
>   Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
>   Transformation( [ 4, 1, 8, 3, 5, 7, 3, 5 ] ),
>   Transformation( [ 4, 3, 4, 5, 6, 4, 1, 2 ] ),
>   Transformation( [ 5, 4, 8, 8, 5, 6, 1, 5 ] ),
>   Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
>   Transformation( [ 7, 1, 2, 2, 2, 7, 4, 5 ] ),
>   Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);
<semigroup with 8 generators>
gap> Size(s);
597369
gap> f:=Transformation( [ 8, 1, 5, 5, 8, 3, 7, 8 ] );;
gap> l:=LClassNC(s, f);
{Transformation( [ 8, 1, 5, 5, 8, 3, 7, 8 ] )}
gap> RhoOrbStabChain(l);
<stabilizer chain record, Base [ 1, 5, 3, 7 ], Orbit length 5, Size: 120>
gap> Size(l);
4560
gap> RhoOrbSCC(l);
[ 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 
  23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39 ]
gap> SchutzenbergerGroup(l);
Group([ (1,5,7), (1,8,7), (1,5,7,3) ])
gap> ForAll(l, x-> x in l);
true
gap> d:=DClass(s, f);
{Transformation( [ 8, 1, 5, 5, 8, 3, 7, 8 ] )}
gap> iter:=Iterator(d);
<iterator>
gap> for i in iter do od;

#
gap> gens:=[ PartialPermNC( [ 1, 2, 3, 5, 7, 10 ], [ 12, 3, 1, 11, 9, 5 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 7, 8 ], [ 4, 3, 11, 12, 6, 2, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 9, 11 ], [ 11, 6, 9, 2, 4, 8, 12 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 7, 9, 12 ], [ 7, 1, 12, 2, 9, 4, 5 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 7, 8, 9 ], [ 5, 4, 8, 11, 6, 12, 1 ] ),
>  PartialPermNC( [ 1, 2, 4, 6, 8, 9, 10 ], [ 8, 5, 2, 12, 4, 7, 11 ] )];;
gap> s:=Semigroup(gens);
<semigroup with 6 generators>
gap> f:=PartialPermNC([ 3, 4, 5, 11 ], [ 4, 1, 2, 5 ]);;
gap> l:=LClassNC(s, f);
{[ 3, 4, 5, 11 ] -> [ 4, 1, 2, 5 ]}
gap> l:=LClass(s, f);
{[ 3, 4, 5, 11 ] -> [ 4, 1, 2, 5 ]}
gap> d:=DClass(s, f);
{[ 3, 4, 5, 11 ] -> [ 4, 1, 2, 5 ]}
gap> Size(l);
1
gap> Number(d, x-> x in l);
1
gap> Number(s, x-> x in l);
1
gap> s:=Semigroup(gens);
<semigroup with 6 generators>
gap> l:=LClass(s, f);
{[ 3, 4, 5, 11 ] -> [ 4, 1, 2, 5 ]}
gap> d:=DClass(s, f);
{[ 3, 4, 5, 11 ] -> [ 4, 1, 2, 5 ]}
gap> Number(d, x-> x in l);
1
gap> Number(s, x-> x in l);
1
gap> SchutzenbergerGroup(l);
Group(())
gap> ForAll(l, x-> x in l);
true
gap> iter:=IteratorOfLClasses(s);
<iterator of L-classes>
gap> repeat l:=NextIterator(iter); until Size(l)>1;
gap> l;
{[ 4 ] -> [ 3 ]}
gap> Size(l);
11
gap> Elements(l);
[ [ 1 ] -> [ 3 ], [ 2 ] -> [ 3 ], <identity on [ 3 ]>, [ 4 ] -> [ 3 ], 
  [ 5 ] -> [ 3 ], [ 6 ] -> [ 3 ], [ 7 ] -> [ 3 ], [ 8 ] -> [ 3 ], 
  [ 9 ] -> [ 3 ], [ 11 ] -> [ 3 ], [ 12 ] -> [ 3 ] ]
gap> ForAll(last, x-> x in s);
true
gap> ForAll(last2, x-> x in l);
true
gap> d:=DClassNC(s, f);
{[ 3, 4, 5, 11 ] -> [ 4, 1, 2, 5 ]}
gap> d:=DClassNC(s, Representative(l));
{[ 4 ] -> [ 3 ]}
gap> ForAll(l, x-> x in d);
true
gap> Number(d, x-> x in l);
11
gap> Number(s, x-> x in l);
11

#
gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
>   Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
>   Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
>   Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);
<semigroup with 4 generators>
gap> f:=Transformation( [ 5, 2, 7, 2, 7, 2, 5, 8 ] );;
gap> l:=LClassNC(s, f);
{Transformation( [ 5, 2, 7, 2, 7, 2, 5, 8 ] )}
gap> enum:=Enumerator(l);
<enumerator of L-class>
gap> enum[1];
Transformation( [ 5, 2, 7, 2, 7, 2, 5, 8 ] )
gap> enum[2];
Transformation( [ 5, 7, 2, 7, 2, 7, 5, 8 ] )
gap> Position(enum, enum[2]);
2
gap> Position(enum, enum[1]);
1
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
true
gap> ForAll([1..Length(enum)], x-> Position(enum, enum[x])=x);
true
gap> Length(enum);
1728
gap> ForAll(enum, x-> x in s);
true
gap> ForAll(l, x-> x in enum);
true
gap> Number(s, x-> x in enum);
1728
gap> Number(s, x-> x in l);
1728
gap> AsSet(l)=AsSet(enum);
true
gap> f:=Transformation( [ 7, 2, 4, 2, 2, 1, 7, 6 ] );;
gap> Position(enum, f);
fail
gap> GreensHClasses(l);
[ {Transformation( [ 5, 2, 7, 2, 7, 2, 5, 8 ] )}, 
  {Transformation( [ 2, 8, 7, 5, 5, 7, 2, 2 ] )}, 
  {Transformation( [ 7, 7, 5, 2, 7, 2, 7, 8 ] )}, 
  {Transformation( [ 8, 8, 7, 5, 5, 7, 2, 8 ] )}, 
  {Transformation( [ 8, 2, 7, 2, 2, 5, 8, 7 ] )}, 
  {Transformation( [ 7, 8, 5, 7, 7, 7, 7, 2 ] )}, 
  {Transformation( [ 5, 7, 7, 7, 7, 2, 5, 8 ] )}, 
  {Transformation( [ 7, 5, 2, 8, 5, 7, 7, 8 ] )}, 
  {Transformation( [ 7, 2, 5, 8, 5, 8, 7, 8 ] )}, 
  {Transformation( [ 2, 7, 7, 8, 8, 2, 2, 5 ] )}, 
  {Transformation( [ 7, 2, 8, 2, 2, 5, 7, 7 ] )}, 
  {Transformation( [ 8, 2, 5, 7, 7, 7, 8, 7 ] )}, 
  {Transformation( [ 7, 8, 7, 5, 5, 7, 7, 2 ] )}, 
  {Transformation( [ 7, 7, 5, 7, 7, 2, 7, 8 ] )}, 
  {Transformation( [ 5, 8, 2, 7, 7, 5, 5, 7 ] )}, 
  {Transformation( [ 2, 5, 7, 5, 5, 7, 2, 8 ] )}, 
  {Transformation( [ 2, 8, 5, 7, 7, 5, 2, 8 ] )}, 
  {Transformation( [ 7, 5, 7, 2, 2, 8, 7, 2 ] )}, 
  {Transformation( [ 7, 8, 2, 7, 8, 2, 7, 5 ] )}, 
  {Transformation( [ 2, 7, 8, 7, 7, 2, 2, 5 ] )}, 
  {Transformation( [ 2, 7, 5, 8, 8, 7, 2, 7 ] )}, 
  {Transformation( [ 8, 2, 7, 7, 7, 5, 8, 7 ] )}, 
  {Transformation( [ 8, 7, 2, 5, 5, 7, 8, 5 ] )}, 
  {Transformation( [ 2, 7, 5, 8, 7, 5, 2, 7 ] )}, 
  {Transformation( [ 5, 8, 7, 2, 2, 5, 5, 7 ] )}, 
  {Transformation( [ 8, 8, 5, 2, 2, 7, 8, 5 ] )}, 
  {Transformation( [ 5, 2, 7, 7, 7, 2, 5, 8 ] )}, 
  {Transformation( [ 7, 2, 7, 5, 2, 8, 7, 2 ] )}, 
  {Transformation( [ 7, 7, 5, 2, 2, 8, 7, 7 ] )}, 
  {Transformation( [ 5, 8, 2, 7, 8, 7, 5, 7 ] )}, 
  {Transformation( [ 2, 7, 7, 8, 8, 7, 2, 5 ] )}, 
  {Transformation( [ 7, 5, 2, 8, 8, 5, 7, 7 ] )}, 
  {Transformation( [ 2, 5, 8, 7, 5, 7, 2, 5 ] )}, 
  {Transformation( [ 8, 7, 7, 5, 5, 2, 8, 5 ] )}, 
  {Transformation( [ 7, 2, 5, 8, 2, 5, 7, 7 ] )}, 
  {Transformation( [ 8, 5, 5, 8, 8, 2, 8, 7 ] )}, 
  {Transformation( [ 5, 2, 8, 8, 2, 7, 5, 5 ] )}, 
  {Transformation( [ 5, 2, 7, 7, 2, 8, 5, 7 ] )}, 
  {Transformation( [ 8, 7, 2, 5, 5, 8, 8, 7 ] )}, 
  {Transformation( [ 2, 8, 5, 8, 8, 7, 2, 7 ] )}, 
  {Transformation( [ 7, 5, 7, 2, 2, 8, 7, 7 ] )}, 
  {Transformation( [ 7, 8, 2, 7, 8, 7, 7, 5 ] )}, 
  {Transformation( [ 2, 8, 7, 5, 8, 5, 2, 7 ] )}, 
  {Transformation( [ 5, 5, 8, 2, 2, 5, 5, 7 ] )}, 
  {Transformation( [ 8, 5, 2, 5, 5, 7, 8, 5 ] )}, 
  {Transformation( [ 7, 5, 7, 8, 8, 5, 7, 2 ] )}, 
  {Transformation( [ 7, 5, 8, 7, 5, 2, 7, 5 ] )}, 
  {Transformation( [ 5, 7, 5, 8, 8, 8, 5, 2 ] )}, 
  {Transformation( [ 5, 8, 8, 5, 8, 2, 5, 7 ] )}, 
  {Transformation( [ 7, 7, 2, 8, 8, 5, 7, 8 ] )}, 
  {Transformation( [ 2, 5, 8, 7, 5, 8, 2, 7 ] )}, 
  {Transformation( [ 7, 2, 7, 5, 2, 8, 7, 7 ] )}, 
  {Transformation( [ 5, 7, 8, 5, 5, 2, 5, 5 ] )}, 
  {Transformation( [ 8, 2, 5, 5, 2, 5, 8, 7 ] )}, 
  {Transformation( [ 5, 2, 7, 7, 7, 8, 5, 5 ] )}, 
  {Transformation( [ 7, 8, 7, 5, 8, 5, 7, 2 ] )}, 
  {Transformation( [ 7, 2, 5, 5, 5, 8, 7, 8 ] )}, 
  {Transformation( [ 7, 8, 2, 7, 7, 8, 7, 5 ] )}, 
  {Transformation( [ 2, 8, 7, 7, 8, 5, 2, 8 ] )}, 
  {Transformation( [ 7, 5, 8, 5, 5, 5, 7, 2 ] )}, 
  {Transformation( [ 8, 5, 5, 7, 5, 2, 8, 5 ] )}, 
  {Transformation( [ 8, 2, 7, 7, 7, 8, 8, 5 ] )}, 
  {Transformation( [ 7, 8, 7, 8, 8, 5, 7, 2 ] )}, 
  {Transformation( [ 8, 5, 2, 7, 7, 7, 8, 8 ] )}, 
  {Transformation( [ 2, 7, 7, 8, 7, 8, 2, 5 ] )}, 
  {Transformation( [ 5, 2, 8, 7, 7, 5, 5, 5 ] )}, 
  {Transformation( [ 2, 5, 7, 8, 8, 7, 2, 8 ] )}, 
  {Transformation( [ 8, 7, 5, 2, 7, 5, 8, 5 ] )}, 
  {Transformation( [ 5, 8, 7, 2, 2, 8, 5, 7 ] )}, 
  {Transformation( [ 8, 7, 7, 5, 5, 2, 8, 8 ] )}, 
  {Transformation( [ 7, 2, 5, 8, 2, 8, 7, 7 ] )}, 
  {Transformation( [ 7, 5, 8, 7, 5, 2, 7, 8 ] )} ]
gap> Length(last);
72

#
gap> gens:=[ PartialPermNC( [ 1, 2, 3, 5, 7, 10 ], [ 12, 3, 1, 11, 9, 5 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 7, 8 ], [ 4, 3, 11, 12, 6, 2, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 9, 11 ], [ 11, 6, 9, 2, 4, 8, 12 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 7, 9, 12 ], [ 7, 1, 12, 2, 9, 4, 5 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 7, 8, 9 ], [ 5, 4, 8, 11, 6, 12, 1 ] ),
>  PartialPermNC( [ 1, 2, 4, 6, 8, 9, 10 ], [ 8, 5, 2, 12, 4, 7, 11 ] )];;
gap> s:=Semigroup(gens);
<semigroup with 6 generators>
gap> Size(s);
4857
gap> f:=PartialPerm([ 6, 9 ], [ 12, 6 ]);;
gap> l:=LClass(s, f);
{[ 6, 9 ] -> [ 12, 6 ]}
gap> NrHClasses(l);
66
gap> Size(l);
66
gap> SchutzenbergerGroup(l);
Group([ (6,12) ])
gap> o:=RhoOrb(l);
<closed orbit, 40 points with Schreier tree with log with grading>
gap> d:=DClassOfLClass(l);
{[ 6, 9 ] -> [ 12, 6 ]}
gap> Size(d);
66
gap> NrLClasses(d);
1
gap> NrRClasses(d);
66
gap> SchutzenbergerGroup(d);
Group(())
gap> Length(RhoOrbSCC(l));
33
gap> HClasses(l);
[ {[ 6, 9 ] -> [ 12, 6 ]}, {[ 6, 9 ] -> [ 6, 12 ]}, {[ 2, 3 ] -> [ 12, 6 ]}, 
  {[ 2, 3 ] -> [ 6, 12 ]}, {[ 2, 7 ] -> [ 6, 12 ]}, {[ 2, 7 ] -> [ 12, 6 ]}, 
  {[ 1, 4 ] -> [ 12, 6 ]}, {[ 1, 4 ] -> [ 6, 12 ]}, {[ 4, 9 ] -> [ 6, 12 ]}, 
  {[ 4, 9 ] -> [ 12, 6 ]}, {[ 1, 8 ] -> [ 6, 12 ]}, {[ 1, 8 ] -> [ 12, 6 ]}, 
  {[ 2, 9 ] -> [ 12, 6 ]}, {[ 2, 9 ] -> [ 6, 12 ]}, {[ 7, 9 ] -> [ 12, 6 ]}, 
  {[ 7, 9 ] -> [ 6, 12 ]}, {[ 3, 9 ] -> [ 12, 6 ]}, {[ 3, 9 ] -> [ 6, 12 ]}, 
  {[ 3, 4 ] -> [ 6, 12 ]}, {[ 3, 4 ] -> [ 12, 6 ]}, {[ 4, 7 ] -> [ 12, 6 ]}, 
  {[ 4, 7 ] -> [ 6, 12 ]}, {[ 1, 7 ] -> [ 12, 6 ]}, {[ 1, 7 ] -> [ 6, 12 ]}, 
  {[ 1, 2 ] -> [ 12, 6 ]}, {[ 1, 2 ] -> [ 6, 12 ]}, {[ 1, 9 ] -> [ 6, 12 ]}, 
  {[ 1, 9 ] -> [ 12, 6 ]}, {[ 8, 9 ] -> [ 12, 6 ]}, {[ 8, 9 ] -> [ 6, 12 ]}, 
  {[ 7, 8 ] -> [ 6, 12 ]}, {[ 7, 8 ] -> [ 12, 6 ]}, {[ 2, 4 ] -> [ 12, 6 ]}, 
  {[ 2, 4 ] -> [ 6, 12 ]}, {[ 4, 5 ] -> [ 12, 6 ]}, {[ 4, 5 ] -> [ 6, 12 ]}, 
  {[ 4, 8 ] -> [ 12, 6 ]}, {[ 4, 8 ] -> [ 6, 12 ]}, {[ 9, 12 ] -> [ 12, 6 ]}, 
  {[ 9, 12 ] -> [ 6, 12 ]}, {[ 2, 8 ] -> [ 6, 12 ]}, {[ 2, 8 ] -> [ 12, 6 ]}, 
  {[ 5, 9 ] -> [ 12, 6 ]}, {[ 5, 9 ] -> [ 6, 12 ]}, {[ 3, 11 ] -> [ 12, 6 ]}, 
  {[ 3, 11 ] -> [ 6, 12 ]}, {[ 7, 12 ] -> [ 6, 12 ]}, 
  {[ 7, 12 ] -> [ 12, 6 ]}, {[ 2, 5 ] -> [ 12, 6 ]}, {[ 2, 5 ] -> [ 6, 12 ]}, 
  {[ 1, 3 ] -> [ 6, 12 ]}, {[ 1, 3 ] -> [ 12, 6 ]}, {[ 4, 12 ] -> [ 12, 6 ]}, 
  {[ 4, 12 ] -> [ 6, 12 ]}, {[ 5, 11 ] -> [ 12, 6 ]}, 
  {[ 5, 11 ] -> [ 6, 12 ]}, {[ 6, 8 ] -> [ 6, 12 ]}, {[ 6, 8 ] -> [ 12, 6 ]}, 
  {[ 1, 5 ] -> [ 12, 6 ]}, {[ 1, 5 ] -> [ 6, 12 ]}, {[ 2, 12 ] -> [ 12, 6 ]}, 
  {[ 2, 12 ] -> [ 6, 12 ]}, {[ 4, 11 ] -> [ 12, 6 ]}, 
  {[ 4, 11 ] -> [ 6, 12 ]}, {[ 4, 6 ] -> [ 12, 6 ]}, {[ 4, 6 ] -> [ 6, 12 ]} ]
gap> IsDuplicateFreeList(last);
true
gap> IsRegularLClass(l);
false
gap> H:=HClasses(l);;
gap> ForAll(H, x-> Representative(x) in l);
true
gap> ForAll(H, x-> Representative(x) in d);
true
gap> d;
{[ 6, 9 ] -> [ 12, 6 ]}
gap> Representative(l) in d;
true
gap> First(H, x-> not Representative(x) in d);
fail
gap> ForAll(l, x-> x in d);
true
gap>   rep:=Representative(d);
[ 6, 9 ] -> [ 12, 6 ]
gap>   s:=ParentSemigroup(d);
<semigroup with 6 generators>
gap> ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or f[2] <> rep[2];
false
gap> g:=f;
[ 6, 9 ] -> [ 12, 6 ]
gap> 
gap>   m:=LambdaOrbSCCIndex(d); o:=LambdaOrb(d); scc:=OrbSCC(o);
1
<closed orbit, 1 points with Schreier tree with log with grading>
[ [ 1 ] ]
gap> l:=Position(o, LambdaFunc(s)(g));
1
gap>  l = fail or OrbSCCLookup(o)[l]<>m ;
false
gap>  l<>scc[m][1];
false
gap>   m:=RhoOrbSCCIndex(d); o:=RhoOrb(d); scc:=OrbSCC(o);
1
<closed orbit, 40 points with Schreier tree with log with grading>
[ [ 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 21, 22, 23, 
      24, 25, 26, 28, 29, 30, 31, 32, 33, 35, 37, 38, 39 ], [ 8 ], [ 13 ], 
  [ 20 ], [ 27 ], [ 34 ], [ 36 ], [ 40 ] ]
gap> 
gap>   l:=Position(o, RhoFunc(s)(g));
1
gap>  l = fail or OrbSCCLookup(o)[l]<>m;
false
gap>  schutz:=RhoOrbStabChain(d);
<stabilizer chain record, Base [ 6 ], Orbit length 2, Size: 2>
gap> l<>scc[m][1];
false
gap> cosets:=LambdaCosets(d);
[ () ]
gap> LambdaOrbStabChain(LambdaOrb(d), LambdaOrbSCCIndex(d));
false
gap> g:=LambdaPerm(s)(rep, g);
()
gap> schutz<>false;
true
gap> o:=LambdaOrb(d); m:=LambdaOrbSCCIndex(d);
<closed orbit, 1 points with Schreier tree with log with grading>
1
gap> lambda_schutz:=LambdaOrbSchutzGp(o, m);
Group(())
gap>   lambda_stab:=LambdaOrbStabChain(o, m);
false
gap>  o:=RhoOrb(d); m:=RhoOrbSCCIndex(d);
<closed orbit, 40 points with Schreier tree with log with grading>
1
gap>   rho_schutz:=RhoOrbSchutzGp(o, m, infinity);
Group([ (6,12) ])
gap>   rho_stab:=RhoOrbStabChain(o, m);
true
gap> rho_stab=true ;
true
gap> schutz:=lambda_schutz;
Group(())
gap> lambda_stab=true;
false
gap> p:=LambdaConjugator(ParentSemigroup(d))(RhoOrbRep(o, m),
>    Representative(d));;
gap> p;
()
gap> rho_schutz:=rho_schutz^p;
Group([ (6,12) ])
gap> f:=PartialPermNC([ 6, 9 ], [ 12, 6 ]);
[ 6, 9 ] -> [ 12, 6 ]
gap> s:=Semigroup(gens);
<semigroup with 6 generators>
gap> l:=LClass(s, f);
{[ 6, 9 ] -> [ 12, 6 ]}
gap> d:=DClassOfLClass(l);
{[ 6, 9 ] -> [ 12, 6 ]}
gap> ForAll(l, x-> x in d);
true
gap> NrHClasses(l);
66
gap> RhoCosets(d);
<enumerator of perm group>
gap> Length(last);
2
gap> H:=HClasses(l);;
gap> ForAll(H, x-> Representative(x) in l);
true
gap> ForAll(H, x-> Representative(x) in d);
true
gap> ForAll(H, x-> Representative(x) in s);
true
gap> ForAll(l, x-> x in l);
true

#
gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
>   Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
>   Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
>   Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);
<semigroup with 4 generators>
gap> Size(s);
95540
gap> f:=Transformation( [ 2, 2, 7, 7, 7, 1, 2, 7 ] );;
gap> l:=LClassNC(s, f);
{Transformation( [ 2, 2, 7, 7, 7, 1, 2, 7 ] )}
gap> g:=Transformation( [ 2, 2, 7, 7, 7, 1, 2, 1 ] );;
gap> Size(l);
936
gap> h:=GreensHClassOfElement(l, g);
{Transformation( [ 2, 2, 7, 7, 7, 1, 2, 1 ] )}
gap> Size(h);
1
gap> SchutzenbergerGroup(l);
Group([ (2,7), (1,7) ])
gap> IsRegularLClass(l);
false
gap> IsGreensClassNC(h);
true
gap> ForAll(h, x-> x in l);
true
gap> ForAll(h, x-> x in s);
true
gap> SchutzenbergerGroup(h);
Group(())
gap> Idempotents(h);
[  ]
gap> IsGroupHClass(h);
false
gap> IsGreensHClass(h);
true
gap> GreensHRelation(s)=EquivalenceClassRelation(h);
true
gap> gens:=[ PartialPermNC( [ 1, 2, 4, 5, 9 ], [ 3, 6, 2, 10, 5 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 7, 8 ], [ 10, 6, 7, 9, 4, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 9 ], [ 7, 2, 5, 6, 9, 3, 8, 10 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 6, 7, 8, 9 ], [ 10, 3, 7, 1, 5, 9, 2, 6 ] ),
>  PartialPermNC( [ 2, 3, 5, 6, 10 ], [ 4, 1, 9, 2, 5 ] ),
>  PartialPermNC( [ 1, 4, 6, 7, 9, 10 ], [ 8, 7, 2, 3, 4, 1 ] ) ];;
gap> s:=Semigroup(gens);
<semigroup with 6 generators>
gap> 
gap> f:=PartialPerm([]);;
gap> l:=LClass(s, f);
{<empty mapping>}
gap> Size(s);
55279
gap> NrIdempotents(s);
141
gap> NrDClasses(s);
2064
gap> NrRClasses(s);
9568
gap> NrLClasses(s);
8369
gap> NrHClasses(s);
25175
gap> IsRegularSemigroup(s);
false
gap> l:=LClass(s, f);
{<empty mapping>}
gap> h:=HClassNC(l, f);
{<empty mapping>}
gap> Size(h);
1
gap> ForAll(h, x-> x in l);
true
gap> ForAll(h, x-> x in s);
true
gap> IsGreensClassNC(h);
true
gap> f:=PartialPermNC([ 2, 8, 9 ], [ 8, 10, 5 ]);;
gap> l:=LClass(s, f);
{[ 4, 7, 8 ] -> [ 8, 10, 5 ]}
gap> h:=HClassNC(l, f);
{[ 2, 8, 9 ] -> [ 8, 10, 5 ]}
gap> ForAll(h, x-> x in s);
true
gap> ForAll(h, x-> x in l);
true
gap> Size(h);
1

#
gap> gens:=[ Transformation( [ 2, 6, 7, 2, 6, 1, 1, 5 ] ),
>   Transformation( [ 3, 8, 1, 4, 5, 6, 7, 1 ] ),
>   Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] ),
>   Transformation( [ 7, 1, 7, 4, 2, 5, 6, 3 ] ) ];;
gap> s:=Monoid(gens);
<monoid with 4 generators>
gap> f:=Transformation( [ 5, 4, 7, 2, 2, 2, 2, 5 ] );;
gap> f in s;
true
gap> l:=LClass(s, f);
{Transformation( [ 5, 4, 7, 2, 2, 2, 2, 5 ] )}
gap> IsGreensClassNC(l);
false
gap> Size(l);
1
gap> f:=Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] );;
gap> l:=LClass(s, f);
{Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] )}
gap> Size(l);
1
gap> iter:=IteratorOfLClasses(s);
<iterator of L-classes>
gap> repeat l:=NextIterator(iter); until Size(l)>1;
gap> l;
{Transformation( [ 6, 1, 1, 6, 1, 2, 2, 6 ] )}
gap> Size(l);
1494
gap> Idempotents(l);
[ Transformation( [ 1, 2, 2, 1, 2, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 2, 2, 1, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 2, 6, 6, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 1, 2, 6, 6, 1, 6 ] ), 
  Transformation( [ 1, 2, 2, 1, 2, 6, 6, 6 ] ), 
  Transformation( [ 1, 2, 2, 1, 2, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 6, 1, 2, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 1, 2, 2, 6, 6, 6 ] ), 
  Transformation( [ 1, 2, 1, 6, 6, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 1, 2, 2, 6, 1, 6 ] ), 
  Transformation( [ 1, 2, 1, 6, 6, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 1, 2, 2, 6, 1, 1 ] ), 
  Transformation( [ 1, 2, 6, 2, 2, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 1, 2, 6, 6, 1, 1 ] ), 
  Transformation( [ 1, 2, 6, 2, 2, 6, 6, 6 ] ), 
  Transformation( [ 1, 2, 1, 6, 6, 6, 1, 2 ] ), 
  Transformation( [ 1, 2, 2, 6, 6, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 1, 6, 2, 6, 1, 2 ] ), 
  Transformation( [ 1, 2, 6, 6, 1, 6, 6, 6 ] ), 
  Transformation( [ 1, 2, 6, 1, 2, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 1, 2, 2, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 1, 6, 6, 6, 1, 1 ] ), 
  Transformation( [ 1, 2, 6, 6, 6, 6, 6, 6 ] ), 
  Transformation( [ 1, 2, 2, 2, 2, 6, 6, 6 ] ), 
  Transformation( [ 1, 2, 2, 6, 6, 6, 6, 6 ] ), 
  Transformation( [ 1, 2, 6, 6, 6, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 1, 6, 1, 6, 1, 2 ] ), 
  Transformation( [ 1, 2, 1, 6, 2, 6, 1, 1 ] ), 
  Transformation( [ 1, 2, 1, 2, 1, 6, 1, 1 ] ), 
  Transformation( [ 1, 2, 6, 2, 2, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 1, 6, 2, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 2, 2, 2, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 1, 1, 6, 6, 2, 2 ] ), 
  Transformation( [ 1, 2, 1, 1, 1, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 1, 6, 6, 6, 1, 6 ] ), 
  Transformation( [ 1, 2, 6, 1, 2, 6, 6, 6 ] ), 
  Transformation( [ 1, 2, 1, 6, 1, 6, 1, 1 ] ), 
  Transformation( [ 1, 2, 1, 1, 2, 6, 1, 2 ] ), 
  Transformation( [ 1, 2, 2, 1, 1, 6, 6, 6 ] ), 
  Transformation( [ 1, 2, 1, 1, 2, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 1, 2, 2, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 6, 6, 2, 6, 6, 6 ] ), 
  Transformation( [ 1, 2, 1, 6, 2, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 1, 1, 6, 6, 1, 2 ] ), 
  Transformation( [ 1, 2, 2, 1, 1, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 1, 1, 2, 6, 1, 1 ] ), 
  Transformation( [ 1, 2, 1, 1, 6, 6, 1, 1 ] ), 
  Transformation( [ 1, 2, 2, 2, 6, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 1, 1, 6, 6, 2, 1 ] ), 
  Transformation( [ 1, 2, 1, 2, 1, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 1, 1, 2, 6, 2, 2 ] ), 
  Transformation( [ 1, 2, 1, 1, 2, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 1, 2, 6, 6, 1, 2 ] ), 
  Transformation( [ 1, 2, 2, 2, 2, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 1, 6, 2, 6, 2, 2 ] ), 
  Transformation( [ 1, 2, 6, 1, 1, 6, 6, 6 ] ), 
  Transformation( [ 1, 2, 1, 1, 6, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 1, 6, 2, 6, 1, 6 ] ), 
  Transformation( [ 1, 2, 1, 2, 1, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 1, 1, 1, 6, 1, 1 ] ), 
  Transformation( [ 1, 2, 1, 1, 2, 6, 2, 1 ] ), 
  Transformation( [ 1, 2, 1, 6, 1, 6, 1, 6 ] ), 
  Transformation( [ 1, 2, 1, 6, 2, 6, 2, 1 ] ), 
  Transformation( [ 1, 2, 1, 6, 6, 6, 2, 2 ] ), 
  Transformation( [ 1, 2, 1, 2, 2, 6, 1, 2 ] ), 
  Transformation( [ 1, 2, 2, 1, 1, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 1, 2, 6, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 1, 1, 6, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 1, 6, 6, 6, 6, 6 ] ), 
  Transformation( [ 1, 2, 6, 6, 6, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 1, 6, 6, 6, 2, 1 ] ), 
  Transformation( [ 1, 2, 1, 6, 1, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 1, 2, 6, 6, 2, 2 ] ), 
  Transformation( [ 1, 2, 1, 2, 6, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 1, 2, 2, 6, 2, 2 ] ), 
  Transformation( [ 1, 2, 1, 2, 1, 6, 1, 2 ] ), 
  Transformation( [ 1, 2, 1, 6, 1, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 1, 2, 6, 6, 2, 1 ] ), 
  Transformation( [ 1, 2, 1, 1, 1, 6, 6, 6 ] ), 
  Transformation( [ 1, 2, 1, 1, 1, 6, 6, 2 ] ), 
  Transformation( [ 1, 2, 1, 2, 2, 6, 2, 1 ] ), 
  Transformation( [ 1, 2, 1, 1, 1, 6, 1, 2 ] ) ]
gap> Length(last);
82
gap> NrIdempotents(l);
82
gap> ForAll(Idempotents(l), x-> x in l);
true
gap> IsDuplicateFree(Idempotents(l));
true
gap> ForAll(Idempotents(l), x-> x in s);
true
gap> ForAll(l, x-> x in s);
true
gap> IsRegularLClass(l);
true
gap> s:=Monoid(gens);
<monoid with 4 generators>
gap> l:=LClass(s, f);
{Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] )}
gap> IsRegularLClass(l);
false
gap> Size(l);
1
gap> f:= Transformation( [ 1, 2, 2, 1, 2, 6, 6, 1 ] );
Transformation( [ 1, 2, 2, 1, 2, 6, 6, 1 ] )
gap> l:=LClass(s, f);
{Transformation( [ 1, 2, 2, 1, 2, 6, 6, 1 ] )}
gap> IsRegularLClass(l);
true

#
gap> gens:=[ PartialPermNC( [ 1, 2, 3 ], [ 1, 4, 3 ] ),
>  PartialPermNC( [ 1, 2, 3 ], [ 2, 3, 4 ] ),
>  PartialPermNC( [ 1, 2, 3 ], [ 4, 2, 1 ] ),
>  PartialPermNC( [ 1, 2, 4 ], [ 1, 4, 3 ] ) ];;
gap> s:=Semigroup(gens);
<semigroup with 4 generators>
gap> List(LClasses(s), IsRegularLClass);
[ false, false, false, false, true, true, false, false, false, false, true, 
  true, false, false, false, true, true, true, true, true ]
gap> Number(last, x-> x=true);
9
gap> GroupOfUnits(s);
fail
gap> List(LClasses(s), NrIdempotents);
[ 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1 ]
gap> NrIdempotents(s);
9
gap> List(LClasses(s), Size);
[ 1, 1, 1, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 1, 4, 4, 4, 4, 1 ]
gap> Sum(last);
62
gap> Size(s);
62
gap> f:=PartialPerm([ 2, 3 ], [ 2, 4 ]);;
gap> l:=LClassNC(s, f);
{[ 2, 3 ] -> [ 2, 4 ]}
gap> HClassReps(l);
[ [ 2, 3 ] -> [ 2, 4 ], [ 2, 3 ] -> [ 4, 2 ], [ 1, 2 ] -> [ 2, 4 ], 
  [ 1, 2 ] -> [ 4, 2 ] ]
gap> IsRegularLClass(l);
false
gap> NrHClasses(l);
4
gap> l:=LClass(s, f);
{[ 1, 2 ] -> [ 4, 2 ]}
gap> HClassReps(l);
[ [ 1, 2 ] -> [ 4, 2 ], [ 1, 2 ] -> [ 2, 4 ], [ 2, 3 ] -> [ 2, 4 ], 
  [ 2, 3 ] -> [ 4, 2 ] ]
gap> IsRegularLClass(l);
false
gap> ForAll(HClassReps(l), x-> x in l);
true
gap> d:=DClassOfLClass(l);
{[ 1, 2 ] -> [ 4, 2 ]}
gap> Size(d);
4
gap> Size(l);
4
gap> l=d;
true
gap> d=l;
true
gap> l<d;
false
gap> ForAll(l, x-> x in d);
true
gap> ForAll(d, x-> x in l);
true
gap> HClassReps(d)=HClassReps(l);
true
gap> NrRClasses(d);
4
gap> NrLClasses(d);
1
gap> NrHClasses(d);
4

#
gap> gens:=[ Transformation( [ 1, 5, 6, 2, 5, 2, 1 ] ),
>   Transformation( [ 1, 7, 5, 4, 3, 5, 7 ] ),
>   Transformation( [ 2, 7, 7, 2, 4, 1, 1 ] ),
>   Transformation( [ 3, 2, 2, 4, 1, 7, 6 ] ),
>   Transformation( [ 3, 3, 5, 1, 7, 1, 6 ] ),
>   Transformation( [ 3, 3, 6, 1, 7, 5, 2 ] ),
>   Transformation( [ 3, 4, 6, 5, 4, 4, 7 ] ),
>   Transformation( [ 5, 2, 4, 5, 1, 4, 5 ] ),
>   Transformation( [ 5, 5, 2, 2, 6, 7, 2 ] ),
>   Transformation( [ 7, 7, 5, 4, 5, 3, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> l:=LClasses(s)[1154];
{Transformation( [ 7, 2, 2, 3, 6, 1, 2 ] )}
gap> IsRegularLClass(l);
false
gap> d:=DClassOfLClass(l);
{Transformation( [ 7, 2, 2, 3, 6, 1, 2 ] )}
gap> Size(l);
1
gap> Size(d);
1
gap> NrHClasses(d);
1
gap> NrLClasses(d);
1
gap> NrRClasses(d);
1
gap> l:=LClasses(s)[523];
{Transformation( [ 5, 5, 5, 1, 7, 3, 6 ] )}
gap> Size(l);
1
gap> iter:=IteratorOfLClasses(s);
<iterator of L-classes>
gap> repeat l:=NextIterator(iter); until Size(l)>1;
gap> l;
{Transformation( [ 1, 5, 6, 2, 5, 2, 1 ] )}
gap> Size(l);
1872
gap> NrHClasses(d);
1
gap> NrHClasses(l);
78
gap> Length(HClassReps(l));;
gap> Length(HClassReps(l));
78
gap> ForAll(HClasses(l), x-> x in HClassReps(l));
false
gap> ForAll(HClasses(l), x-> Representative(x) in HClassReps(l));
true
gap> Size(l);
1872
gap> d:=DClassOfLClass(l);
{Transformation( [ 1, 5, 6, 2, 5, 2, 1 ] )}
gap> Size(d);
59904
gap> ForAll(l, x-> x in d);
true
gap> Number(d, x-> x in l);
1872
gap> SchutzenbergerGroup(d);
Group([ (1,6), (1,2), (1,5,6,2) ])
gap> d;
{Transformation( [ 1, 5, 6, 2, 5, 2, 1 ] )}

# new stuff from hereJDM
gap> gens:=[ PartialPermNC( [ 1, 2, 3, 5 ], [ 5, 7, 3, 4 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5 ], [ 6, 4, 1, 2, 7 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 7 ], [ 2, 7, 4, 5, 8 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 6 ], [ 5, 6, 1, 4, 3 ] ),
>  PartialPermNC( [ 1, 2, 4, 6, 7 ], [ 2, 1, 6, 7, 4 ] ),
>  PartialPermNC( [ 1, 3, 5, 6, 7 ], [ 6, 2, 3, 5, 7 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 7 ], [ 4, 1, 6, 2, 8, 5 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 8 ], [ 5, 6, 3, 8, 2, 7 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 6, 7 ], [ 1, 5, 2, 6, 7, 4 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6, 8 ], [ 7, 5, 2, 8, 4, 1, 3 ] ) ];;
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> Size(s);
72713
gap> NrRClasses(s);
25643
gap> NrDClasses(s);
4737
gap> NrLClasses(s);
11323
gap> NrIdempotents(s);
121
gap> IsRegularSemigroup(s);
false
gap> f:=Random(s);
[ 3, 4, 7 ] -> [ 4, 7, 8 ]
gap> d:=DClass(s, f);
{[ 3, 4, 7 ] -> [ 4, 7, 8 ]}
gap> Size(d);
282
gap> NrRClasses(d);
282
gap> NrLClasses(d);
1
gap> IsRegularDClass(d);
false
gap> RhoCosets(d);
<enumerator of perm group>
gap> Length(last);
6
gap> AsList(last2);
[ (), (4,7,8), (4,8,7), (7,8), (4,7), (4,8) ]
gap> SchutzenbergerGroup(d);
Group(())
gap> RhoOrbStabChain(d);
<stabilizer chain record, Base [ 8, 4 ], Orbit length 3, Size: 6>
gap> SemigroupDataSCC(d);
fail
gap> Position(DClasses(s), d);
18
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> SemigroupDataSCC(d);
[ 33, 35, 144, 146, 147, 148, 151, 152, 340, 341, 342, 343, 344, 345, 346, 
  353, 355, 356, 357, 358, 503, 519, 539, 540, 544, 553, 560, 561, 568, 571, 
  706, 707, 708, 709, 710, 711, 712, 713, 714, 715, 717, 718, 719, 720, 721, 
  724, 725, 726, 727, 728, 729, 730, 731, 1013, 1014, 1040, 1041, 1043, 1045, 
  1046, 1086, 1087, 1088, 1089, 1090, 1091, 1092, 1102, 1103, 1105, 1124, 
  1126, 1134, 1135, 1136, 1137, 1139, 1140, 1151, 1157, 1158, 1181, 1202, 
  1333, 1334, 1335, 1336, 1337, 1338, 1339, 1340, 1341, 1342, 1343, 1344, 
  1345, 1346, 1347, 1348, 1349, 1350, 1351, 1352, 1353, 1354, 1355, 1356, 
  1357, 1358, 1359, 1360, 1361, 1362, 1363, 1364, 1365, 1366, 1367, 1368, 
  1369, 1370, 1371, 1800, 1801, 1831, 1832, 1833, 1837, 1838, 1839, 1840, 
  1841, 1842, 1898, 1899, 1900, 1901, 1902, 1903, 1904, 1905, 1906, 1907, 
  1908, 1909, 1910, 1911, 1913, 1918, 1928, 1929, 1932, 1933, 1958, 1959, 
  1960, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 2009, 2010, 2037, 2039, 
  2051, 2052, 2057, 2058, 2065, 2188, 2189, 2190, 2191, 2192, 2193, 2194, 
  2195, 2196, 2197, 2198, 2199, 2200, 2201, 2202, 2203, 2204, 2205, 2206, 
  2207, 2208, 2209, 2210, 2211, 2212, 2213, 2214, 2215, 2216, 2217, 2218, 
  2749, 2750, 2782, 2783, 2788, 2789, 2790, 2791, 2792, 2793, 2794, 2795, 
  2855, 2856, 2857, 2858, 2859, 2860, 2861, 2862, 2863, 2864, 2865, 2866, 
  2873, 2884, 2885, 2888, 2889, 2920, 2921, 2936, 2937, 2938, 2939, 2973, 
  2974, 2990, 2997, 3003, 3004, 3019, 3124, 3125, 3126, 3127, 3128, 3129, 
  3130, 3131, 3132, 3133, 3134, 3135, 3136, 3137, 3674, 3704, 3706, 3707, 
  3708, 3765, 3766, 3767, 3831, 3850, 3891, 3910, 3916, 3925, 4040, 4041, 
  4042, 4043, 4044, 4573, 4703, 4807, 5517 ]
gap> LambdaCosets(d);
[ () ]
gap> LambdaOrbSCC(d);
[ 49 ]
gap> RhoOrbSCC(d);
[ 15, 17, 18, 19, 20, 22, 23, 25, 29, 32, 33, 34, 41, 42, 45, 47, 76, 79, 80, 
  83, 84, 85, 86, 87, 89, 90, 91, 93, 98, 101, 102, 103, 105, 106, 107, 108, 
  113, 114, 118, 121, 123, 126, 127, 128, 136, 137, 141 ]
gap> ForAll(d, x-> x in d);
true
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> enum[1];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
gap> Length(enum);
282
gap> Size(d);
282
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Length( lschutz ) * (Position( lscc[lm], ll ) - 1) 
+ Position( lschutz, LambdaPerm( s )( RhoOrbMults( ro, rm )[rl][1] * rep 
      / cosets[j], f ) ) called from
enum!.NumberElement( enum, elm ) called from
Position( enum, x ) called from
func( elm ) called from
ForAllOp( C, func ) called from
...  at line 43 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
0
brk> Length(lschutz);
1
brk> (Position(lscc[lm], ll)-1;
Syntax error: ) expected in *errin* line 3
(Position(lscc[lm], ll)-1;
                         ^
brk> Position(lscc[lm], ll)-1;
0
brk> Position(lschutz, LambdaPerm(s)(RhoOrbMults(ro, rm)[rl][1];
Syntax error: ) expected in *errin* line 5
Position(lschutz, LambdaPerm(s)(RhoOrbMults(ro, rm)[rl][1];
                                                          ^
brk>  Position(lschutz, LambdaPerm(s)(RhoOrbMults(ro, rm)[rl][1]*
>        rep/cosets[j], f))
> ;
fail
brk> f;
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> RhoOrbMults(ro, rm)[rl][1]*
>        rep/cosets[j];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
brk> RhoOrbMults(ro, rm)[rl][1]*rep/cosets[j]^-1*f;
<empty mapping>
brk> (RhoOrbMults(ro, rm)[rl][1]*rep/cosets[j])^-1*f;
[ 4, 7, 8 ] -> [ 8, 4, 7 ]
brk> pp:=last;
Syntax error: warning: unbound global variable in *errin* line 14
pp:=last;
   ^
[ 4, 7, 8 ] -> [ 8, 4, 7 ]
brk> MappingPermListList(DomPP(pp), RanPP(pp));
(4,8,7)
brk> LambdaPerm(s)(RhoOrbMults(ro, rm)[rl][1]*
>        rep/cosets[j], f);
(4,8,7)
brk> lschutz;
[ () ]
brk> LambdaPerm(s)(RhoOrbMults(ro, rm)[rl][1]*
>        rep/cosets[j], RhoOrbMults(ro, rm)[rl][1]*g/cosets[j]);
(1,2,4,3,5,7)(6,8)
brk> g;
(4,8,7)
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
Syntax error: warning: unbound global variable in /Users/jdm/gap/pkg/citrus/ga\
p/greens.gi line 712
      p:=LambdaPerm(s)(rep, g);
        ^
Syntax error: warning: unbound global variable in /Users/jdm/gap/pkg/citrus/ga\
p/greens.gi line 715
        j:=PositionCanonical(cosets, p);
                                      ^
Syntax error: warning: unbound global variable in /Users/jdm/gap/pkg/citrus/ga\
p/greens.gi line 719
          if SiftedPermutation(schutz, p*cosets[j])=() then 
                                        ^
true
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Length( lschutz ) * (Position( lscc[lm], ll ) - 1) 
+ Position( lschutz, LambdaPerm( s )( RhoOrbMults( ro, rm )[rl][1] * rep 
      / cosets[j], RhoOrbMults( ro, rm )[rl][1] * g / cosets[j] ) )
  called from
enum!.NumberElement( enum, elm ) called from
Position( enum, x ) called from
func( elm ) called from
ForAllOp( C, func ) called from
...  at line 48 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> g;
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk>  RhoOrbMults( ro, rm )[rl][1] * g / cosets[j];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> RhoOrbMults(ro, rm)[rl][1]*
>        rep/cosets[j];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
brk> f;
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> coset[j];
Syntax error: warning: unbound global variable in *errin* line 6
coset[j];
     ^
Error, Variable: 'coset' must have a value in
  <compiled or corrupted statement>  called from 
enum!.m * r + Length( lschutz ) * (Position( lscc[lm], ll ) - 1) 
+ Position( lschutz, LambdaPerm( s )( RhoOrbMults( ro, rm )[rl][1] * rep 
      / cosets[j], RhoOrbMults( ro, rm )[rl][1] * g / cosets[j] ) )
  called from
enum!.NumberElement( enum, elm ) called from
Position( enum, x ) called from
func( elm ) called from
ForAllOp( C, func ) called from
...  at line 6 of *errin*
brk> quit;
gap> cosets[j];
Error, Variable: 'cosets' must have a value
not in any function at line 48 of *stdin*
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Length( lschutz ) * (Position( lscc[lm], ll ) - 1) 
+ Position( lschutz, LambdaPerm( s )( RhoOrbMults( ro, rm )[rl][1] * rep 
      / cosets[j], RhoOrbMults( ro, rm )[rl][1] * g / cosets[j] ) )
  called from
enum!.NumberElement( enum, elm ) called from
Position( enum, x ) called from
func( elm ) called from
ForAllOp( C, func ) called from
...  at line 49 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> cosets[j];
()
brk>  RhoOrbMults(ro, rm)[rl][1];
<identity on [ 1 .. 8 ]>
brk> quit
> ;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Position( Enumerator( GreensRClasses( d )[(r + 1)] ), f )
  called from
enum!.NumberElement( enum, elm ) called from
Position( enum, x ) called from
func( elm ) called from
ForAllOp( C, func ) called from
...  at line 53 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk>  Position( Enumerator( GreensRClasses( d )[(r + 1)] ), f );
fail
brk> GreensRClasses( d )[(r + 1)];  f;
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> r;
0
brk> f in GreensRClasses(d)[r+1];
false
brk> x;
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> enum[1];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
gap> enum[2];
Error,  called from
enum!.ElementNumber( enum, nr ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 58 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> GreensRClasses(d)[2];
{[ 1, 3, 6 ] -> [ 4, 8, 7 ]}
gap> Size(last);
1
gap> GreensRClasses(d)[1];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> Size(last);
1
gap> Length(cosets);
Error, Variable: 'cosets' must have a value
not in any function at line 62 of *stdin*
gap> d;
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> cosets:=RhoCosets(d);
<enumerator of perm group>
gap> Length(cosets);
6
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
Error,  called from
enum!.ElementNumber( enum, nr ) called from
<compiled or corrupted statement>  called from
func( elm ) called from
ForAllOp( C, func ) called from
<function "ForAll">( <arguments> )
 called from read-eval loop at line 66 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> f;
[ 3, 4, 7 ] -> [ 4, 7, 8 ]
brk> enum[2];
Error,  called from
enum!.ElementNumber( enum, nr ) called from
Error(  ); called from
enum!.ElementNumber( enum, nr ) called from
<compiled or corrupted statement>  called from
func( elm ) called from
...  at line 2 of *errin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk_2> quit;
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d;
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> enum[2];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Length( lschutz ) * (Position( lscc[lm], ll ) - 1) 
+ Position( lschutz, LambdaPerm( s )( RhoOrbMults( ro, rm )[rl][1] * rep 
      / cosets[j], RhoOrbMults( ro, rm )[rl][1] * g / cosets[j] ) )
  called from
enum!.NumberElement( enum, elm ) called from
Position( enum, x ) called from
func( elm ) called from
ForAllOp( C, func ) called from
...  at line 72 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
0
brk> Length(lschutz)*(Position(lscc[lm], ll)-1);
0
brk> rl;
15
brk> Position(rscc[rm], rl);
1
brk> Length(cosets);
6
brk> j;
1
brk> enum!.m;
1
brk> f;
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> r;
0
brk> GreensRClasses(d)[1];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
brk> f;
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> GreensRClasses(d)[2];
{[ 1, 3, 6 ] -> [ 4, 8, 7 ]}
brk> f in GreensRClasses(d)[2];
true
brk> RereadPackage("citrus/gap/greens.gi");
true
brk> quit;
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> enum[1];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
gap> enum[2];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
gap> Position(enum, enum[2]);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Length( lschutz ) * (Position( lscc[lm], ll ) - 1) 
+ Position( lschutz, LambdaPerm( s )( RhoOrbMults( ro, rm )[rl][1] * rep 
      / cosets[j], RhoOrbMults( ro, rm )[rl][1] * g / cosets[j] ) )
  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 77 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
6
brk> rl;
15
brk> rscc[rm];
[ 15, 17, 18, 19, 20, 22, 23, 25, 29, 32, 33, 34, 41, 42, 45, 47, 76, 79, 80, 
  83, 84, 85, 86, 87, 89, 90, 91, 93, 98, 101, 102, 103, 105, 106, 107, 108, 
  113, 114, 118, 121, 123, 126, 127, 128, 136, 137, 141 ]
brk> r;
6
brk> RhoOrbMults(ro, rm)[rl][1]*
>        rep/cosets[j];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
brk> f;
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> lscc[lm][1];
49
brk> ll;
49
brk> rep;
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
brk> g;
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> LambdaPerm(s)(rep, g);
(4,8,7)
brk> lschutz;
[ () ]
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> enum[1];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
gap> enum[2];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
gap> Position(enum, enum[1]);
1
gap> Position(enum, enum[2]);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Position( Enumerator( GreensRClasses( d )[(r + 1)] ), f )
  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 84 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
0
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[2]);
2
gap> Position(enum, enum[3]);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Position( Enumerator( GreensRClasses( d )[(r + 1)] ), f )
  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 89 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
1
brk> rl;
15
brk> rscc[rm];
[ 15, 17, 18, 19, 20, 22, 23, 25, 29, 32, 33, 34, 41, 42, 45, 47, 76, 79, 80, 
  83, 84, 85, 86, 87, 89, 90, 91, 93, 98, 101, 102, 103, 105, 106, 107, 108, 
  113, 114, 118, 121, 123, 126, 127, 128, 136, 137, 141 ]
brk> j;
1
brk> quit;
gap> enum[3];
[ 1, 3, 6 ] -> [ 8, 7, 4 ]
gap> r;
Error, Variable: 'r' must have a value
not in any function at line 90 of *stdin*
gap> Position(enum, enum[3]);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Position( Enumerator( GreensRClasses( d )[(r + 1)] ), f )
  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 91 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
1
brk> j;
1
brk> RereadPackage("citrus/gap/greens.gi");
true
brk> quit;
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[2]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 94 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> j;
1
brk> cosets[j];
()
brk> o;
Syntax error: warning: unbound global variable in *errin* line 3
o;
 ^
Error, Variable: 'o' must have a value in
  <compiled or corrupted statement>  called from 
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 3 of *errin*
brk> p;
(4,8,7)
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[2]);
Error, Function: number of arguments must be 2 (not 1) in
  schutz := LambdaOrbStabChain( d ); called from 
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 98 of *stdin*
you can replace the argument list <args> via 'return <args>;'
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[3]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 102 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> j;
2
brk> r;
Error, Variable: 'r' must have an assigned value in
  <compiled or corrupted statement>  called from 
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 2 of *errin*
you can 'return;' after assigning a value
brk_2> quit;
brk> r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j;
2
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[3]);
3
gap> Position(enum, enum[4]);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Position( Enumerator( GreensRClasses( d )[(r + 1)] ), f )
  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 107 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> Size(d);
282
gap> enum[4];
[ 1, 3, 6 ] -> [ 8, 4, 7 ]
gap> enum[1];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
gap> enum[2];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
gap> enum[3];
[ 1, 3, 6 ] -> [ 8, 7, 4 ]
gap> enum[4];
[ 1, 3, 6 ] -> [ 8, 4, 7 ]
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:+
Syntax error: ; expected
s:+
 ^
> ;
Syntax error: expression expected
;
^
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[4]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 119 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
Error, Variable: 'r' must have an assigned value in
  <compiled or corrupted statement>  called from 
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 1 of *errin*
you can 'return;' after assigning a value
brk_2> quit;
brk> j;
4
brk> (Position(rscc[rm], rl)-1)*Length(cosets)+j;
4
brk> GreensRClasses(d)[4];
{[ 1, 3, 6 ] -> [ 8, 4, 7 ]}
brk> enum[4];
[ 1, 3, 6 ] -> [ 8, 4, 7 ]
brk> quit;
gap> GreensRClasses(d)[3];
{[ 1, 3, 6 ] -> [ 8, 7, 4 ]}
gap> enum[3];
[ 1, 3, 6 ] -> [ 8, 7, 4 ]
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> enum[1];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
gap> Position(enum, enum[1]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 126 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
1
brk> Position(enum, enum[2]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
Error(  ); called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 2 of *errin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk_2> r;
3
brk_2> quit;
brk> quit;
gap> Position(enum, enum[2]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 126 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
3
brk> enum[2];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> GreensRClasses(d)[2];
{[ 1, 3, 6 ] -> [ 4, 8, 7 ]}
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[1]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 130 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
0
brk> quit;
gap> Position(enum, enum[2]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 130 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
2
brk> j;
3
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[1]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 134 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> j;
1
brk> f;
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
brk> GreensRClasses(d)[1];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
brk> rep/cosets[1];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
brk> rep/cosets[2];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> GreensRClasses(d)[2];
{[ 1, 3, 6 ] -> [ 4, 8, 7 ]}
brk> rep/cosets[3];
[ 1, 3, 6 ] -> [ 8, 7, 4 ]
brk> GreensRClasses(d)[3];
{[ 1, 3, 6 ] -> [ 8, 7, 4 ]}
brk> r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
0
brk> return;
Error, Function Calls: <func> must return a value in
  return enum!.NumberElement( enum, elm ); called from 
<function "unknown">( <arguments> )
 called from read-eval loop at line 134 of *stdin*
you can supply one by 'return <value>;'
brk> enum!.m*r+Position(Enumerator(GreensRClasses(d)[r+1]), f);
Syntax error: warning: unbound global variable in *errin* line 1
enum!.m*r+Position(Enumerator(GreensRClasses(d)[r+1]), f);
         ^
Error, Variable: 'r' must have a value in
  <compiled or corrupted statement>  called from 
<function "unknown">( <arguments> )
 called from read-eval loop at line 1 of *errin*
Syntax error: warning: unbound global variable in *errin* line 1
enum!.m*r+Position(Enumerator(GreensRClasses(d)[r+1]), f);
                                                 ^
brk>  r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
Syntax error: warning: unbound global variable in *errin* line 2
 r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
   ^
Syntax error: warning: unbound global variable in *errin* line 2
 r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
                  ^
Error, Variable: 'rscc' must have a value in
  <compiled or corrupted statement>  called from 
<function "unknown">( <arguments> )
 called from read-eval loop at line 2 of *errin*
Syntax error: warning: unbound global variable in *errin* line 2
 r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
                     ^
Syntax error: warning: unbound global variable in *errin* line 2
 r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
                          ^
Syntax error: warning: unbound global variable in *errin* line 2
 r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
                                               ^
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s;=
<semigroup with 10 generators>
Syntax error: expression expected
s;=
  ^
> ;
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[1]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 140 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
0
brk> enum!.m*r+Position(Enumerator(GreensRClasses(d)[r+1]), f);
1
brk> return;
Error, Function Calls: <func> must return a value in
  return enum!.NumberElement( enum, elm ); called from 
<function "unknown">( <arguments> )
 called from read-eval loop at line 140 of *stdin*
you can supply one by 'return <value>;'
brk> return;
'return' cannot be used in this read-eval-print loop
brk> return;
'return' cannot be used in this read-eval-print loop
brk> return;
'return' cannot be used in this read-eval-print loop
brk> return;
'return' cannot be used in this read-eval-print loop
brk> return;
'return' cannot be used in this read-eval-print loop
brk> return;
'return' cannot be used in this read-eval-print loop
brk> return;
'return' cannot be used in this read-eval-print loop
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> Position(enum, enum[2]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 141 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
2
brk> enum!.m*r+Position(Enumerator(GreensRClasses(d)[r+1]), f);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
Error(  ); called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 2 of *errin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk_2> quit;
brk> rscc;
[ [ 1 ], [ 2, 21, 26, 30, 31, 38, 46, 49, 50, 51, 52, 55, 57, 59, 60, 61, 94, 
      95, 99, 109, 110, 117, 129, 130, 139, 140, 142, 144, 147, 148, 150 ], 
  [ 3, 28 ], [ 4 ], [ 5 ], [ 6 ], [ 7 ], [ 8 ], [ 9 ], [ 10 ], [ 11 ], 
  [ 12, 13, 14, 16, 40, 44, 64, 68, 69, 70, 72, 73, 74, 75, 77, 81, 82, 88, 
      96, 97, 104, 112, 119, 122, 134, 135, 138 ], 
  [ 15, 17, 18, 19, 20, 22, 23, 25, 29, 32, 33, 34, 41, 42, 45, 47, 76, 79, 
      80, 83, 84, 85, 86, 87, 89, 90, 91, 93, 98, 101, 102, 103, 105, 106, 
      107, 108, 113, 114, 118, 121, 123, 126, 127, 128, 136, 137, 141 ], 
  [ 24 ], [ 27 ], [ 35 ], [ 36 ], [ 37 ], [ 39, 111 ], [ 43 ], [ 48 ], 
  [ 53 ], [ 54 ], [ 56 ], [ 58 ], [ 62, 65, 66, 67, 71, 78, 100, 133 ], 
  [ 63 ], [ 92 ], [ 115, 116 ], [ 120 ], [ 124 ], [ 125 ], [ 131 ], [ 132 ], 
  [ 143 ], [ 145 ], [ 146 ], [ 149 ], [ 151 ] ]
brk> rscc[rm];
[ 15, 17, 18, 19, 20, 22, 23, 25, 29, 32, 33, 34, 41, 42, 45, 47, 76, 79, 80, 
  83, 84, 85, 86, 87, 89, 90, 91, 93, 98, 101, 102, 103, 105, 106, 107, 108, 
  113, 114, 118, 121, 123, 126, 127, 128, 136, 137, 141 ]
brk> rl;
15
brk> j;
3
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[1]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 145 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
0
brk> j;
1
brk> return;
1
gap> Position(enum, enum[2]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 146 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> j;
3
brk> cosets[3];
(4,8,7)
brk> p;
(4,8,7)
brk> cosets[3]*p;
(4,7,8)
brk> schutz;
false
brk> p;
(4,8,7)
brk> cosets;
<enumerator of perm group>
brk> PositionCanonical(cosets, p);
3
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> enum[1];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
gap> enum[2];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
gap> Position(enum, enum[1]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 152 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> j;
1
brk> return;
1
gap> Position(enum, enum[2]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 153 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> j;
2
brk> return;
2
gap> Position(enum, enum[3]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 154 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> return;
3
gap> Position(enum, enum[4]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 155 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> return;
4
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
Error,  called from
enum!.NumberElement( enum, elm ) called from
Position( enum, x ) called from
func( elm ) called from
ForAllOp( C, func ) called from
<function "ForAll">( <arguments> )
 called from read-eval loop at line 156 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum[2];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
true
gap> Length(enum);
282
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Length( lschutz ) * (Position( lscc[lm], ll ) - 1) 
+ Position( lschutz, LambdaPerm( s )( RhoOrbMults( ro, rm )[rl][1] * rep 
      / cosets[j], RhoOrbMults( ro, rm )[rl][1] * g / cosets[j] ) )
  called from
enum!.NumberElement( enum, elm ) called from
Position( enum, x ) called from
func( elm ) called from
ForAllOp( C, func ) called from
...  at line 167 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
true
gap> NrDClasses(s);
4737
gap> for d in DClasses(s) do 
> enum:=Enumerator(d);
> ForAll(enum, x-> enum[Position(enum, x)]=x);
> od;
gap> for d in DClasses(s) do
> enum:=Enumerator(d);
> Print(ForAll(enum, x-> enum[Position(enum, x)]=x), "\n");
> od;
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
gap> Size(s);
72713
gap> NrRClasses(s);
25643
gap> NrLClasses(s);
11323
gap> NrDClasses(s);
4737
gap> NrIdempotents(s);
121
gap> quit;
gap> Test("pkg/citrus/tst/everyfunction.tst");
Citrus package: everyfunction.tst
GAP4stones: 0
true
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
>   Transformation( [ 4, 2, 1, 5, 5 ] ),
>   Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> for d in DClasses(s) do
> enum:=Enumerator(d);
> Print(ForAll(enum, x-> enum[Position(enum, x)]=x), "\n");
> od
> ;
true
true
true
true
gap> s:=RandomBlockGroup(3, 10);
<semigroup with 3 generators>
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 3, 6, 8, 10 ], [ 2, 6, 7, 9, 1, 5 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 8, 10 ], [ 7, 1, 4, 3, 2, 6, 5 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 6, 7, 8, 10 ], [ 3, 8, 1, 9, 4, 10, 5, 6 ] ) ]
gap> f:=Random(s);
[ 2, 4 ] -> [ 6, 5 ]
gap> d:=DClassNC(s, f);
{[ 2, 4 ] -> [ 6, 5 ]}
gap> GreensHClasses(d);
[ {[ 2, 4 ] -> [ 6, 5 ]} ]
gap> Size(d):
Syntax error: ; expected
Size(d):
       ^
gap> Size(d);
1
gap> s:=RandomBlockGroup(3, 100);
<semigroup with 3 generators>
gap> f:=Random(s);
[ 12, 27, 37, 40, 46, 50, 51, 53 ] -> [ 98, 3, 84, 99, 100, 21, 70, 89 ]
gap> d:=DClassNC(s, f);
{[ 12, 27, 37, 40, 46, 50, 51, 53 ] -> [ 98, 3, 84, 99, 100, 21, 70, 89 ]}
gap> Size(D);
Error, Variable: 'D' must have a value
not in any function at line 22 of *stdin*
gap> Size(d);
1
gap> GreensHClasses(d);
[ {[ 12, 27, 37, 40, 46, 50, 51, 53 ] -> [ 98, 3, 84, 99, 100, 21, 70, 89 ]} ]
gap> iter:=IteratorOfDClasses(s);
<iterator of D-classes>
gap> repeat d:=NextIterator(iter); until Size(d)>1;
gap> d;
{[ 8, 57 ] -> [ 63, 87 ]}
gap> Size(d);
2036
gap> IsRegularDClass(d);
false
gap> GreensHClasses(d);;
gap> time;
75
gap> NrHClasses(d);
2036
gap> f:=Random(s);
<partial perm on 68 pts>
gap> d:=DClass(s, f);
{<partial perm on 68 pts>}
gap> GreensHClasses(d);;
gap> GreensHClasses(d);
[ {<partial perm on 68 pts>} ]
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 19, 
  20, 24, 25, 26, 27, 28, 29, 31, 32, 34, 35, 36, 37, 38, 40, 43, 45, 46, 49, 
  50, 51, 53, 55, 56, 57, 58, 59, 60, 61, 64, 66, 68, 69, 70, 72, 73, 74, 77, 
  80, 81, 83, 86, 87, 89, 91, 98 ], [ 89, 70, 79, 27, 84, 99, 9, 73, 33, 77, 
  69, 41, 18, 63, 29, 42, 75, 56, 90, 64, 98, 49, 35, 100, 71, 3, 20, 2, 26, 
  11, 39, 7, 48, 85, 8, 10, 61, 25, 55, 92, 62, 21, 34, 57, 44, 14, 53, 59, 
  12, 87, 78, 83, 30, 32, 68, 86, 23, 47, 93, 15, 76, 97, 91 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 
  19, 20, 22, 23, 24, 25, 28, 30, 31, 33, 34, 35, 36, 39, 40, 42, 43, 44, 45, 
  46, 47, 50, 53, 54, 55, 58, 59, 64, 65, 67, 69, 70, 71, 72, 73, 76, 77, 78, 
  81, 82, 84, 85, 86, 87, 89, 92, 94, 95 ], [ 5, 13, 94, 44, 80, 54, 99, 81, 
  31, 7, 90, 30, 46, 68, 36, 11, 100, 17, 87, 72, 14, 29, 9, 61, 91, 32, 43, 
  64, 60, 41, 26, 40, 8, 23, 63, 38, 57, 12, 59, 83, 92, 96, 18, 3, 65, 2, 
  37, 21, 49, 16, 75, 24, 27, 1, 48, 6, 35, 79, 82, 51, 39, 25, 77, 62, 22 
 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
  18, 19, 20, 21, 23, 24, 26, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 
  40, 42, 44, 48, 51, 52, 53, 55, 56, 57, 58, 60, 63, 64, 65, 66, 67, 71, 73, 
  75, 77, 80, 82, 83, 85, 86, 90, 91, 96, 97, 98, 99 ], 
[ 67, 93, 18, 59, 86, 16, 99, 73, 60, 74, 17, 95, 85, 49, 79, 4, 33, 66, 15, 
  44, 77, 41, 55, 84, 68, 69, 94, 31, 2, 29, 5, 42, 10, 63, 58, 34, 72, 53, 
  89, 57, 62, 76, 20, 52, 22, 35, 75, 98, 78, 40, 46, 28, 6, 90, 12, 65, 26, 
  36, 25, 61, 83, 38, 39, 87, 92, 97, 43, 30 ] ) ]
gap> d;
{<partial perm on 68 pts>}
gap> GreensLClasses(d);
[ {<partial perm on 68 pts>} ]
gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ),
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ),
> Transformation( [ 3, 3, 4, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> List(DClasses(s), LClasses);
[ [ {Transformation( [ 1, 3, 4, 1 ] )}, {Transformation( [ 4, 1, 3, 4 ] )}, 
      {Transformation( [ 3, 4, 1, 3 ] )} ], 
  [ {Transformation( [ 2, 4, 1, 2 ] )} ], 
  [ {Transformation( [ 3, 1, 1, 3 ] )}, {Transformation( [ 1, 4, 4, 1 ] )}, 
      {Transformation( [ 2, 1, 1, 2 ] )}, {Transformation( [ 2, 4, 4, 2 ] )}, 
      {Transformation( [ 4, 3, 3, 4 ] )} ], 
  [ {Transformation( [ 3, 3, 4, 1 ] )} ], 
  [ {Transformation( [ 1, 1, 1, 1 ] )}, {Transformation( [ 2, 2, 2, 2 ] )}, 
      {Transformation( [ 3, 3, 3, 3 ] )}, {Transformation( [ 4, 4, 4, 4 ] )} 
     ] ]
gap> Concatenation(last);
[ {Transformation( [ 1, 3, 4, 1 ] )}, {Transformation( [ 4, 1, 3, 4 ] )}, 
  {Transformation( [ 3, 4, 1, 3 ] )}, {Transformation( [ 2, 4, 1, 2 ] )}, 
  {Transformation( [ 3, 1, 1, 3 ] )}, {Transformation( [ 1, 4, 4, 1 ] )}, 
  {Transformation( [ 2, 1, 1, 2 ] )}, {Transformation( [ 2, 4, 4, 2 ] )}, 
  {Transformation( [ 4, 3, 3, 4 ] )}, {Transformation( [ 3, 3, 4, 1 ] )}, 
  {Transformation( [ 1, 1, 1, 1 ] )}, {Transformation( [ 2, 2, 2, 2 ] )}, 
  {Transformation( [ 3, 3, 3, 3 ] )}, {Transformation( [ 4, 4, 4, 4 ] )} ]
gap> IsDuplicateFree(last);
true
gap> List(last2, Size);
[ 1, 1, 1, 1, 10, 10, 10, 10, 10, 3, 1, 1, 1, 1 ]
gap> Sum(last);
61
gap> Size(s);
61
gap> One(s);
Transformation( [ 1, 2, 3, 4 ] )
gap> One(s) in s;
false
gap> SemigroupData(s);
<semigroup data: 10 reps, 9 lambda-rho values>
gap> NrRClasses(s);
9
gap> SemigroupData(s)[1];
[ ,,, Transformation( [ 1, 2, 3, 4 ] ) ]
gap> SemigroupData(s)[2];
[ <monoid with 4 generators>, 2, 
  <closed orbit, 12 points with Schreier tree with log>, 
  Transformation( [ 1, 3, 4, 1 ] ), 2 ]
gap> RereadPackage("citrus/gap/acting.gi");
true
gap> s:=Monoid(gens);;
gap> Size(s);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `[]' on 2 arguments called from
Position( o, lamx ) called from
Enumerate( SemigroupData( s ), infinity, ReturnFalse ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 59 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> o;
<closed orbit, 12 points with Schreier tree with log>
brk> lamx;
[ 1, 2, 3, 4 ]
brk> Position(o, lamx);
fail
brk> o;
<closed orbit, 12 points with Schreier tree with log>
brk> o!.gens;
[ Transformation( [ 1, 3, 4, 1 ] ), Transformation( [ 2, 4, 1, 2 ] ), 
  Transformation( [ 3, 1, 1, 3 ] ), Transformation( [ 3, 3, 4, 1 ] ) ]
brk> quit;
gap> RereadPackage("citrus/gap/acting.gi");
true
gap> s:=Monoid(gens);;
gap> Size(s);
62
gap> RereadPackage("citrus/gap/acting.gi");
true
gap> quit;
gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ),
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ),
> Transformation( [ 3, 3, 4, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> List(GreensDClasses(s), LClasses);
[ [ {Transformation( [ 1, 2, 3, 4 ] )} ], 
  [ {Transformation( [ 1, 3, 4, 1 ] )}, {Transformation( [ 4, 1, 3, 4 ] )}, 
      {Transformation( [ 3, 4, 1, 3 ] )} ], 
  [ {Transformation( [ 2, 4, 1, 2 ] )} ], 
  [ {Transformation( [ 3, 1, 1, 3 ] )}, {Transformation( [ 1, 4, 4, 1 ] )}, 
      {Transformation( [ 2, 1, 1, 2 ] )}, {Transformation( [ 2, 4, 4, 2 ] )}, 
      {Transformation( [ 4, 3, 3, 4 ] )} ], 
  [ {Transformation( [ 3, 3, 4, 1 ] )} ], 
  [ {Transformation( [ 1, 1, 1, 1 ] )}, {Transformation( [ 2, 2, 2, 2 ] )}, 
      {Transformation( [ 3, 3, 3, 3 ] )}, {Transformation( [ 4, 4, 4, 4 ] )} 
     ] ]
gap> List(Concatenation(last), Size);
[ 1, 1, 1, 1, 1, 10, 10, 10, 10, 10, 3, 1, 1, 1, 1 ]
gap> Sum(last);
62
gap> Size(s);
62
gap> l:=Concatenation(List(GreensDClasses(s), LClasses));
[ {Transformation( [ 1, 2, 3, 4 ] )}, {Transformation( [ 1, 3, 4, 1 ] )}, 
  {Transformation( [ 4, 1, 3, 4 ] )}, {Transformation( [ 3, 4, 1, 3 ] )}, 
  {Transformation( [ 2, 4, 1, 2 ] )}, {Transformation( [ 3, 1, 1, 3 ] )}, 
  {Transformation( [ 1, 4, 4, 1 ] )}, {Transformation( [ 2, 1, 1, 2 ] )}, 
  {Transformation( [ 2, 4, 4, 2 ] )}, {Transformation( [ 4, 3, 3, 4 ] )}, 
  {Transformation( [ 3, 3, 4, 1 ] )}, {Transformation( [ 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 2, 2, 2 ] )}, {Transformation( [ 3, 3, 3, 3 ] )}, 
  {Transformation( [ 4, 4, 4, 4 ] )} ]
gap> List(last, Elements);
[ [ Transformation( [ 1, 2, 3, 4 ] ) ], [ Transformation( [ 1, 3, 4, 1 ] ) ], 
  [ Transformation( [ 4, 1, 3, 4 ] ) ], [ Transformation( [ 3, 4, 1, 3 ] ) ], 
  [ Transformation( [ 2, 4, 1, 2 ] ) ], 
  [ Transformation( [ 1, 1, 1, 3 ] ), Transformation( [ 1, 1, 3, 1 ] ), 
      Transformation( [ 1, 1, 3, 3 ] ), Transformation( [ 1, 3, 1, 1 ] ), 
      Transformation( [ 1, 3, 3, 1 ] ), Transformation( [ 3, 1, 1, 3 ] ), 
      Transformation( [ 3, 1, 3, 3 ] ), Transformation( [ 3, 3, 1, 1 ] ), 
      Transformation( [ 3, 3, 1, 3 ] ), Transformation( [ 3, 3, 3, 1 ] ) ], 
  [ Transformation( [ 1, 1, 1, 4 ] ), Transformation( [ 1, 1, 4, 1 ] ), 
      Transformation( [ 1, 1, 4, 4 ] ), Transformation( [ 1, 4, 1, 1 ] ), 
      Transformation( [ 1, 4, 4, 1 ] ), Transformation( [ 4, 1, 1, 4 ] ), 
      Transformation( [ 4, 1, 4, 4 ] ), Transformation( [ 4, 4, 1, 1 ] ), 
      Transformation( [ 4, 4, 1, 4 ] ), Transformation( [ 4, 4, 4, 1 ] ) ], 
  [ Transformation( [ 1, 1, 1, 2 ] ), Transformation( [ 1, 1, 2, 1 ] ), 
      Transformation( [ 1, 1, 2, 2 ] ), Transformation( [ 1, 2, 1, 1 ] ), 
      Transformation( [ 1, 2, 2, 1 ] ), Transformation( [ 2, 1, 1, 2 ] ), 
      Transformation( [ 2, 1, 2, 2 ] ), Transformation( [ 2, 2, 1, 1 ] ), 
      Transformation( [ 2, 2, 1, 2 ] ), Transformation( [ 2, 2, 2, 1 ] ) ], 
  [ Transformation( [ 2, 2, 2, 4 ] ), Transformation( [ 2, 2, 4, 2 ] ), 
      Transformation( [ 2, 2, 4, 4 ] ), Transformation( [ 2, 4, 2, 2 ] ), 
      Transformation( [ 2, 4, 4, 2 ] ), Transformation( [ 4, 2, 2, 4 ] ), 
      Transformation( [ 4, 2, 4, 4 ] ), Transformation( [ 4, 4, 2, 2 ] ), 
      Transformation( [ 4, 4, 2, 4 ] ), Transformation( [ 4, 4, 4, 2 ] ) ], 
  [ Transformation( [ 3, 3, 3, 4 ] ), Transformation( [ 3, 3, 4, 3 ] ), 
      Transformation( [ 3, 3, 4, 4 ] ), Transformation( [ 3, 4, 3, 3 ] ), 
      Transformation( [ 3, 4, 4, 3 ] ), Transformation( [ 4, 3, 3, 4 ] ), 
      Transformation( [ 4, 3, 4, 4 ] ), Transformation( [ 4, 4, 3, 3 ] ), 
      Transformation( [ 4, 4, 3, 4 ] ), Transformation( [ 4, 4, 4, 3 ] ) ], 
  [ Transformation( [ 1, 1, 3, 4 ] ), Transformation( [ 3, 3, 4, 1 ] ), 
      Transformation( [ 4, 4, 1, 3 ] ) ], [ Transformation( [ 1, 1, 1, 1 ] ) ]
    , [ Transformation( [ 2, 2, 2, 2 ] ) ], 
  [ Transformation( [ 3, 3, 3, 3 ] ) ], [ Transformation( [ 4, 4, 4, 4 ] ) ] ]
gap> Union(last);
[ Transformation( [ 1, 1, 1, 1 ] ), Transformation( [ 1, 1, 1, 2 ] ), 
  Transformation( [ 1, 1, 1, 3 ] ), Transformation( [ 1, 1, 1, 4 ] ), 
  Transformation( [ 1, 1, 2, 1 ] ), Transformation( [ 1, 1, 2, 2 ] ), 
  Transformation( [ 1, 1, 3, 1 ] ), Transformation( [ 1, 1, 3, 3 ] ), 
  Transformation( [ 1, 1, 3, 4 ] ), Transformation( [ 1, 1, 4, 1 ] ), 
  Transformation( [ 1, 1, 4, 4 ] ), Transformation( [ 1, 2, 1, 1 ] ), 
  Transformation( [ 1, 2, 2, 1 ] ), Transformation( [ 1, 2, 3, 4 ] ), 
  Transformation( [ 1, 3, 1, 1 ] ), Transformation( [ 1, 3, 3, 1 ] ), 
  Transformation( [ 1, 3, 4, 1 ] ), Transformation( [ 1, 4, 1, 1 ] ), 
  Transformation( [ 1, 4, 4, 1 ] ), Transformation( [ 2, 1, 1, 2 ] ), 
  Transformation( [ 2, 1, 2, 2 ] ), Transformation( [ 2, 2, 1, 1 ] ), 
  Transformation( [ 2, 2, 1, 2 ] ), Transformation( [ 2, 2, 2, 1 ] ), 
  Transformation( [ 2, 2, 2, 2 ] ), Transformation( [ 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 4, 2 ] ), Transformation( [ 2, 2, 4, 4 ] ), 
  Transformation( [ 2, 4, 1, 2 ] ), Transformation( [ 2, 4, 2, 2 ] ), 
  Transformation( [ 2, 4, 4, 2 ] ), Transformation( [ 3, 1, 1, 3 ] ), 
  Transformation( [ 3, 1, 3, 3 ] ), Transformation( [ 3, 3, 1, 1 ] ), 
  Transformation( [ 3, 3, 1, 3 ] ), Transformation( [ 3, 3, 3, 1 ] ), 
  Transformation( [ 3, 3, 3, 3 ] ), Transformation( [ 3, 3, 3, 4 ] ), 
  Transformation( [ 3, 3, 4, 1 ] ), Transformation( [ 3, 3, 4, 3 ] ), 
  Transformation( [ 3, 3, 4, 4 ] ), Transformation( [ 3, 4, 1, 3 ] ), 
  Transformation( [ 3, 4, 3, 3 ] ), Transformation( [ 3, 4, 4, 3 ] ), 
  Transformation( [ 4, 1, 1, 4 ] ), Transformation( [ 4, 1, 3, 4 ] ), 
  Transformation( [ 4, 1, 4, 4 ] ), Transformation( [ 4, 2, 2, 4 ] ), 
  Transformation( [ 4, 2, 4, 4 ] ), Transformation( [ 4, 3, 3, 4 ] ), 
  Transformation( [ 4, 3, 4, 4 ] ), Transformation( [ 4, 4, 1, 1 ] ), 
  Transformation( [ 4, 4, 1, 3 ] ), Transformation( [ 4, 4, 1, 4 ] ), 
  Transformation( [ 4, 4, 2, 2 ] ), Transformation( [ 4, 4, 2, 4 ] ), 
  Transformation( [ 4, 4, 3, 3 ] ), Transformation( [ 4, 4, 3, 4 ] ), 
  Transformation( [ 4, 4, 4, 1 ] ), Transformation( [ 4, 4, 4, 2 ] ), 
  Transformation( [ 4, 4, 4, 3 ] ), Transformation( [ 4, 4, 4, 4 ] ) ]
gap> last=AsSSortedList(s);
true
gap> s:=RandomBlockGroup(4, 7);
<semigroup with 4 generators>
gap> Size(s);
840
gap> Display(Generators(s);
Syntax error: ) expected
Display(Generators(s);
                     ^
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 3, 4 ], [ 5, 7, 1, 6 ] ),
 PartialPermNC( [ 1, 2, 3, 5 ], [ 5, 2, 7, 3 ] ),
 PartialPermNC( [ 1, 2, 3, 6, 7 ], [ 1, 3, 4, 7, 5 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 7 ], [ 3, 2, 4, 6, 1, 5 ] ) ]
gap> NrGreensDClasses(s);
Error, Variable: 'NrGreensDClasses' must have a value
not in any function at line 22 of *stdin*
gap> NrDClasses(s);
176
gap> s:=RandomBlockGroup(4, 6);
<semigroup with 4 generators>
gap> Size(s);
56
gap> NrDClasses(s);
28
gap> List(DClasses(s), RClasse);
Error, Variable: 'RClasse' must have a value
not in any function at line 27 of *stdin*
gap> List(DClasses(s), RClasses);
[ [ {[ 1, 2, 4 ] -> [ 4, 6, 3 ]} ], [ {[ 1 .. 4 ] -> [ 4, 1, 2, 6 ]} ], 
  [ {[ 1, 2, 3, 5 ] -> [ 4, 6, 5, 2 ]} ], 
  [ {[ 1, 2, 3, 5 ] -> [ 6, 4, 5, 1 ]} ], 
  [ {[ 1 ] -> [ 3 ]}, {[ 4 ] -> [ 3 ]}, {[ 2 ] -> [ 3 ]}, {[ 5 ] -> [ 3 ]}, 
      {<identity on [ 3 ]>} ], [ {[ 1 .. 3 ] -> [ 3, 4, 6 ]} ], 
  [ {[ 1, 5 ] -> [ 3, 6 ]} ], [ {[ 2, 5 ] -> [ 3, 4 ]} ], 
  [ {[ 1, 4 ] -> [ 6, 2 ]} ], [ {[ 1 .. 3 ] -> [ 6, 4, 1 ]} ], 
  [ {[ 1, 5 ] -> [ 6, 1 ]} ], [ {[ 2, 5 ] -> [ 6, 4 ]} ], 
  [ {[ 2, 3 ] -> [ 4, 6 ]} ], [ {[ 3, 5 ] -> [ 2, 6 ]} ], 
  [ {[ 3, 5 ] -> [ 2, 4 ]} ], [ {[ 2, 3 ] -> [ 6, 4 ]} ], 
  [ {[ 3, 5 ] -> [ 1, 4 ]} ], [ {[ 3, 5 ] -> [ 1, 6 ]} ], 
  [ {<empty mapping>} ], 
  [ {[ 1 ] -> [ 6 ]}, {[ 4 ] -> [ 6 ]}, {[ 2 ] -> [ 6 ]}, {[ 5 ] -> [ 6 ]}, 
      {[ 3 ] -> [ 6 ]} ], [ {[ 2, 3 ] -> [ 3, 4 ]} ], 
  [ {[ 3, 5 ] -> [ 6, 3 ]} ], [ {[ 3, 5 ] -> [ 4, 3 ]} ], 
  [ {[ 1, 2 ] -> [ 2, 6 ]} ], [ {[ 2, 5 ] -> [ 2, 6 ]} ], 
  [ {[ 3, 5 ] -> [ 4, 6 ]} ], [ {[ 2, 3 ] -> [ 2, 6 ]} ], 
  [ {[ 3, 5 ] -> [ 6, 2 ]} ] ]
gap> ForAll(Union(List(last, Elements)), x-> x in s);
false
gap> Union(List(last2, Elements));
[ {<empty mapping>}, {[ 1 ] -> [ 3 ]}, {[ 1 ] -> [ 6 ]}, {[ 2 ] -> [ 3 ]}, 
  {[ 2 ] -> [ 6 ]}, {<identity on [ 3 ]>}, {[ 3 ] -> [ 6 ]}, {[ 4 ] -> [ 3 ]},
  {[ 4 ] -> [ 6 ]}, {[ 5 ] -> [ 3 ]}, {[ 5 ] -> [ 6 ]}, 
  {[ 1, 2 ] -> [ 2, 6 ]}, {[ 1, 4 ] -> [ 6, 2 ]}, {[ 1, 5 ] -> [ 3, 6 ]}, 
  {[ 1, 5 ] -> [ 6, 1 ]}, {[ 2, 3 ] -> [ 2, 6 ]}, {[ 2, 3 ] -> [ 3, 4 ]}, 
  {[ 2, 3 ] -> [ 4, 6 ]}, {[ 2, 3 ] -> [ 6, 4 ]}, {[ 2, 5 ] -> [ 2, 6 ]}, 
  {[ 2, 5 ] -> [ 3, 4 ]}, {[ 2, 5 ] -> [ 6, 4 ]}, {[ 3, 5 ] -> [ 1, 4 ]}, 
  {[ 3, 5 ] -> [ 1, 6 ]}, {[ 3, 5 ] -> [ 2, 4 ]}, {[ 3, 5 ] -> [ 2, 6 ]}, 
  {[ 3, 5 ] -> [ 4, 3 ]}, {[ 3, 5 ] -> [ 4, 6 ]}, {[ 3, 5 ] -> [ 6, 2 ]}, 
  {[ 3, 5 ] -> [ 6, 3 ]}, {[ 1 .. 3 ] -> [ 3, 4, 6 ]}, 
  {[ 1 .. 3 ] -> [ 6, 4, 1 ]}, {[ 1, 2, 4 ] -> [ 4, 6, 3 ]}, 
  {[ 1 .. 4 ] -> [ 4, 1, 2, 6 ]}, {[ 1, 2, 3, 5 ] -> [ 4, 6, 5, 2 ]}, 
  {[ 1, 2, 3, 5 ] -> [ 6, 4, 5, 1 ]} ]
gap> Union(List(last, Elements));
[ <empty mapping>, <identity on [ 1 ]>, [ 1 ] -> [ 2 ], [ 1 ] -> [ 3 ], 
  [ 1 ] -> [ 4 ], [ 1 ] -> [ 5 ], [ 1 ] -> [ 6 ], [ 2 ] -> [ 1 ], 
  <identity on [ 2 ]>, [ 2 ] -> [ 3 ], [ 2 ] -> [ 4 ], [ 2 ] -> [ 5 ], 
  [ 2 ] -> [ 6 ], [ 3 ] -> [ 1 ], [ 3 ] -> [ 2 ], <identity on [ 3 ]>, 
  [ 3 ] -> [ 4 ], [ 3 ] -> [ 5 ], [ 3 ] -> [ 6 ], [ 4 ] -> [ 1 ], 
  [ 4 ] -> [ 2 ], [ 4 ] -> [ 3 ], <identity on [ 4 ]>, [ 4 ] -> [ 5 ], 
  [ 4 ] -> [ 6 ], [ 5 ] -> [ 1 ], [ 5 ] -> [ 2 ], [ 5 ] -> [ 3 ], 
  [ 5 ] -> [ 4 ], <identity on [ 5 ]>, [ 5 ] -> [ 6 ], [ 1, 2 ] -> [ 2, 6 ], 
  [ 1, 4 ] -> [ 6, 2 ], [ 1, 5 ] -> [ 3, 6 ], [ 1, 5 ] -> [ 6, 1 ], 
  [ 2, 3 ] -> [ 2, 6 ], [ 2, 3 ] -> [ 3, 4 ], [ 2, 3 ] -> [ 4, 6 ], 
  [ 2, 3 ] -> [ 6, 4 ], [ 2, 5 ] -> [ 2, 6 ], [ 2, 5 ] -> [ 3, 4 ], 
  [ 2, 5 ] -> [ 6, 4 ], [ 3, 5 ] -> [ 1, 4 ], [ 3, 5 ] -> [ 1, 6 ], 
  [ 3, 5 ] -> [ 2, 4 ], [ 3, 5 ] -> [ 2, 6 ], [ 3, 5 ] -> [ 4, 3 ], 
  [ 3, 5 ] -> [ 4, 6 ], [ 3, 5 ] -> [ 6, 2 ], [ 3, 5 ] -> [ 6, 3 ], 
  [ 1 .. 3 ] -> [ 3, 4, 6 ], [ 1 .. 3 ] -> [ 6, 4, 1 ], 
  [ 1, 2, 4 ] -> [ 4, 6, 3 ], [ 1 .. 4 ] -> [ 4, 1, 2, 6 ], 
  [ 1, 2, 3, 5 ] -> [ 4, 6, 5, 2 ], [ 1, 2, 3, 5 ] -> [ 6, 4, 5, 1 ] ]
gap> ForAll(last, x-> x in s);
true
gap> Set(last2)=AsSSortedList(s);
true
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
>  Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 2, 4, 1, 1 ] ),
>  Transformation( [ 3, 4, 2, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
114
gap> NrRClasses(s);
11
gap> NrDClasses(s);
5
gap> NrLClasses(s);
19
gap> NrIdempotents(s);
28
gap> IsRegularSemigroup(s);
false
gap> f:=Random(s);
Transformation( [ 4, 2, 4, 3 ] )
gap> d:=First(DClasses(s), x-> f in x);
{Transformation( [ 1, 4, 1, 2 ] )}
gap> h:=HClass(d, f);
{Transformation( [ 4, 2, 4, 3 ] )}
gap> Size(h);
6
gap> IsGroupHClass(h);
true
gap> SchutzenbergerGroup(h);
Group([ (2,4), (2,3,4) ])
gap> ForAll(Elements(h), x-> x in h);
true
gap> ForAll(Elements(h), x-> x in d);
true
gap> IsGreensClassNC(h);
false
gap> s:=RandomBlockGroup(4, 6);
<semigroup with 4 generators>
gap> Size(s);
201
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 4 ], [ 4, 5, 6 ] ),
 PartialPermNC( [ 1, 2, 5 ], [ 2, 1, 3 ] ),
 PartialPermNC( [ 1, 2, 4, 6 ], [ 2, 4, 3, 5 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5 ], [ 4, 3, 6, 5, 1 ] ) ]
gap> f:=Random(s);
[ 1 .. 5 ] -> [ 4, 3, 6, 5, 1 ]
gap> d:=DClassNC(s, f);
{[ 1 .. 5 ] -> [ 4, 3, 6, 5, 1 ]}
gap> h:=HClassNC(d, f);
{[ 1 .. 5 ] -> [ 4, 3, 6, 5, 1 ]}
gap> Size(h);
1
gap> Size(d);
1
gap> h=d;
true
gap> d=h;
true
gap> s:=RandomBlockGroup(4, 100);
<semigroup with 4 generators>
gap> f:=Random(s);
[ 2, 63 ] -> [ 28, 89 ]
gap> d:=DClassNC(s, f);
{[ 2, 63 ] -> [ 28, 89 ]}
gap> Size(d);
4752
gap> time;
43
gap> RhoOrb(d);
<closed orbit, 2874 points with Schreier tree with log with grading>
gap> 2874*2;
5748
gap> LambdaOrb(d);
<closed orbit, 1 points with Schreier tree with log with grading>
gap> NrLClasses(d);
1
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 
  19, 20, 21, 23, 24, 25, 26, 27, 28, 30, 32, 33, 34, 35, 36, 37, 40, 42, 44, 
  46, 47, 51, 53, 54, 58, 59, 60, 61, 63, 65, 66, 67, 69, 71, 72, 76, 79, 84, 
  86, 88, 94, 95, 100 ], [ 46, 47, 33, 32, 70, 97, 29, 30, 34, 11, 37, 89, 
  77, 52, 73, 2, 96, 66, 88, 69, 93, 87, 85, 68, 48, 25, 28, 43, 49, 95, 40, 
  24, 16, 94, 76, 63, 58, 23, 100, 38, 27, 78, 21, 71, 4, 72, 36, 13, 99, 90, 
  17, 41, 98, 10, 35, 91, 53, 45, 82, 42 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 18, 19, 
  21, 22, 24, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 38, 39, 40, 41, 42, 43, 
  44, 46, 48, 49, 51, 52, 54, 56, 58, 59, 61, 64, 65, 67, 68, 70, 73, 74, 76, 
  78, 79, 80, 82, 88, 90, 97 ], [ 63, 38, 57, 12, 9, 91, 59, 32, 54, 83, 92, 
  96, 99, 18, 3, 81, 5, 65, 2, 37, 21, 49, 16, 75, 24, 23, 43, 27, 1, 48, 6, 
  35, 30, 79, 82, 51, 39, 25, 61, 77, 62, 22, 64, 14, 72, 7, 50, 8, 80, 19, 
  94, 69, 10, 40, 67, 28, 88, 93, 66, 36, 70, 56 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
  18, 21, 22, 23, 24, 25, 26, 27, 29, 30, 31, 34, 35, 36, 37, 38, 39, 40, 42, 
  43, 44, 46, 48, 49, 51, 52, 53, 55, 58, 60, 63, 64, 66, 67, 68, 69, 71, 73, 
  75, 80, 86, 87, 88, 90, 91, 94, 95, 97 ], [ 89, 85, 8, 56, 42, 10, 61, 25, 
  98, 55, 39, 92, 62, 21, 34, 57, 44, 14, 53, 64, 59, 84, 12, 87, 78, 83, 30, 
  32, 68, 73, 2, 86, 23, 48, 47, 79, 93, 15, 76, 97, 77, 11, 33, 100, 91, 67, 
  18, 16, 99, 60, 74, 17, 95, 49, 4, 66, 41, 69, 94, 31, 29, 5, 63, 58, 72 
 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 17, 20, 21, 
  22, 23, 24, 26, 28, 29, 30, 32, 34, 35, 37, 39, 40, 42, 43, 44, 45, 46, 47, 
  48, 49, 51, 53, 54, 55, 56, 58, 59, 60, 61, 63, 64, 65, 66, 67, 68, 72, 74, 
  75, 79, 80, 82, 87, 88, 91, 92, 99, 100 ], [ 89, 67, 34, 15, 57, 29, 4, 62, 
  76, 20, 52, 22, 35, 75, 98, 78, 40, 46, 28, 6, 55, 90, 16, 12, 65, 26, 66, 
  36, 25, 61, 83, 38, 41, 93, 2, 39, 87, 85, 17, 92, 97, 43, 30, 5, 13, 94, 
  44, 80, 54, 99, 81, 31, 7, 68, 11, 100, 72, 14, 9, 91, 32, 64, 60, 8, 23 
 ] ) ]
gap> NrRClasses(d);
4752
gap> f:=Random(d);
[ 4, 29 ] -> [ 28, 89 ]
gap> f in d;
true
gap> h:=HClass(d, f);
{[ 4, 29 ] -> [ 28, 89 ]}
gap> hh:=HClassNC(d, f);
{[ 4, 29 ] -> [ 28, 89 ]}
gap> hh=h;
true
gap> Size(h);
1
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
>  Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 3, 4, 2, 2 ] ),
>  Transformation( [ 4, 1, 2, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> List(DClasses(s), RClassReps);
[ [ Transformation( [ 1, 2, 3, 4 ] ) ], [ Transformation( [ 1, 3, 2, 3 ] ) ], 
  [ Transformation( [ 1, 4, 1, 2 ] ), Transformation( [ 1, 2, 4, 4 ] ) ], 
  [ Transformation( [ 4, 1, 2, 1 ] ), Transformation( [ 4, 2, 1, 2 ] ) ], 
  [ Transformation( [ 1, 4, 1, 4 ] ), Transformation( [ 1, 4, 4, 4 ] ), 
      Transformation( [ 1, 1, 4, 1 ] ) ], [ Transformation( [ 1, 4, 2, 4 ] ) ]
    , [ Transformation( [ 4, 4, 2, 2 ] ), Transformation( [ 4, 2, 4, 4 ] ), 
      Transformation( [ 4, 4, 4, 2 ] ) ], [ Transformation( [ 1, 1, 1, 1 ] ) ]
    , [ Transformation( [ 2, 4, 2, 4 ] ), Transformation( [ 2, 4, 4, 4 ] ), 
      Transformation( [ 2, 2, 4, 2 ] ) ] ]
gap> reps:=Concatenation(last0;
Error, Variable: 'last0' must have a value
not in any function at line 87 of *stdin*
Syntax error: ) expected
reps:=Concatenation(last0;
                         ^
gap> reps:=Concatenation(last);
[ Transformation( [ 1, 2, 3, 4 ] ), Transformation( [ 1, 3, 2, 3 ] ), 
  Transformation( [ 1, 4, 1, 2 ] ), Transformation( [ 1, 2, 4, 4 ] ), 
  Transformation( [ 4, 1, 2, 1 ] ), Transformation( [ 4, 2, 1, 2 ] ), 
  Transformation( [ 1, 4, 1, 4 ] ), Transformation( [ 1, 4, 4, 4 ] ), 
  Transformation( [ 1, 1, 4, 1 ] ), Transformation( [ 1, 4, 2, 4 ] ), 
  Transformation( [ 4, 4, 2, 2 ] ), Transformation( [ 4, 2, 4, 4 ] ), 
  Transformation( [ 4, 4, 4, 2 ] ), Transformation( [ 1, 1, 1, 1 ] ), 
  Transformation( [ 2, 4, 2, 4 ] ), Transformation( [ 2, 4, 4, 4 ] ), 
  Transformation( [ 2, 2, 4, 2 ] ) ]
gap> Length(last);
17
gap> IsDuplicateFree(last2);
true
gap> Size(s);
69
gap> NrDClasses(s):
Syntax error: ; expected
NrDClasses(s):
             ^
gap> NrDClasses(s);
9
gap> NrLClasses(s);
21
gap> List(reps, x-> DClass(s, f));
Error, the element does not belong to the semigroup, called from
GreensDClassOfElement( arg[1], arg[2] ) called from
DClass( s, f ) called from
func( C[i] ) called from
<function "List">( <arguments> )
 called from read-eval loop at line 95 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> arg[2];
[ 4, 29 ] -> [ 28, 89 ]
brk> quit;
gap> List(reps, x-> DClass(s, x));
[ {Transformation( [ 1, 2, 3, 4 ] )}, {Transformation( [ 1, 3, 2, 3 ] )}, 
  {Transformation( [ 1, 4, 1, 2 ] )}, {Transformation( [ 1, 2, 4, 4 ] )}, 
  {Transformation( [ 4, 1, 2, 1 ] )}, {Transformation( [ 4, 2, 1, 2 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 4, 4 ] )}, 
  {Transformation( [ 1, 1, 4, 1 ] )}, {Transformation( [ 1, 4, 2, 4 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 4, 2, 4, 4 ] )}, 
  {Transformation( [ 4, 4, 4, 2 ] )}, {Transformation( [ 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )}, {Transformation( [ 2, 4, 4, 4 ] )}, 
  {Transformation( [ 2, 2, 4, 2 ] )} ]
gap> Union(List(last, x-> LClass(x,Representative(x)));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `^' on 2 arguments called from
rho_schutz ^ p called from
SchutzenbergerGroup( d ); called from
RhoOrbStabChain( d ) called from
f in d called from
GreensLClassOfElement( arg[1], arg[2] ) called from
...  at line 96 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> d;
{Transformation( [ 1, 2, 4, 4 ] )}
brk> IsGreensDClass(d);
true
brk> f;
Transformation( [ 1, 2, 4, 4 ] )
brk> f in d;
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `^' on 2 arguments called from
rho_schutz ^ p called from
SchutzenbergerGroup( d ); called from
RhoOrbStabChain( d ) called from
Error( no_method_found ); called from
rho_schutz ^ p called from
...  at line 4 of *errin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk_2> quit;
brk> p;
fail
brk> ParentSemigroup(d);
<monoid with 4 generators>
brk> RhoOrbRep(o, m);
Transformation( [ 1, 4, 1, 2 ] )
brk> o;
<closed orbit, 12 points with Schreier tree with log>
brk> m;
4
brk> d;
{Transformation( [ 1, 2, 4, 4 ] )}
brk> Position(RhoOrb(d), RhoFunc(s)(Representative(d)));
5
brk> OrbSCCLookup(RhoOrb(d))[5];
4
brk> OrbSCC(o)[4];
[ 4, 5 ]
brk> quit;
Syntax error: ) expected
Union(List(last, x-> LClass(x,Representative(x)));
                                                 ^
gap> d:=DClass(s, Transformation( [ 1, 2, 4, 4 ] ));
{Transformation( [ 1, 2, 4, 4 ] )}
gap> f:=Transformation( [ 1, 2, 4, 4 ] );
Transformation( [ 1, 2, 4, 4 ] )
gap> o:=LambdaOrb(s);
<closed orbit, 15 points with Schreier tree with log>
gap> rectify:=RectifyLambda(s, o, f);
rec( l := 4, m := 4, rep := Transformation( [ 1, 2, 4, 4 ] ) )
gap> HasRhoOrb(s) and IsClosed(RhoOrb(s));
true
gap>    o:=RhoOrb(s);
<closed orbit, 12 points with Schreier tree with log>
gap>     rectify:=RectifyRho(s, o, f);
rec( l := 5, m := 4, rep := Transformation( [ 1, 4, 1, 2 ] ) )
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> List(reps, x-> DClass(s, x));
[ {Transformation( [ 1, 2, 3, 4 ] )}, {Transformation( [ 1, 3, 2, 3 ] )}, 
  {Transformation( [ 1, 4, 1, 2 ] )}, {Transformation( [ 1, 4, 1, 2 ] )}, 
  {Transformation( [ 4, 1, 2, 1 ] )}, {Transformation( [ 4, 2, 1, 2 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 1, 4 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 2, 4 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 4, 4, 2, 2 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )}, {Transformation( [ 2, 4, 2, 4 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )} ]
gap> Union(List(last, x-> LClass(x,Representative(x)));
Syntax error: ) expected
Union(List(last, x-> LClass(x,Representative(x)));
                                                 ^
gap> Union(List(last, x-> LClass(x,Representative(x))));
[ Transformation( [ 1, 1, 1, 1 ] ), Transformation( [ 1, 1, 4, 1 ] ), 
  Transformation( [ 1, 2, 1, 4 ] ), Transformation( [ 1, 2, 3, 2 ] ), 
  Transformation( [ 1, 2, 3, 4 ] ), Transformation( [ 1, 2, 4, 2 ] ), 
  Transformation( [ 1, 2, 4, 4 ] ), Transformation( [ 1, 3, 2, 3 ] ), 
  Transformation( [ 1, 4, 1, 2 ] ), Transformation( [ 1, 4, 1, 4 ] ), 
  Transformation( [ 1, 4, 2, 2 ] ), Transformation( [ 1, 4, 2, 4 ] ), 
  Transformation( [ 1, 4, 4, 4 ] ), Transformation( [ 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 4, 2 ] ), Transformation( [ 2, 2, 4, 4 ] ), 
  Transformation( [ 2, 4, 2, 2 ] ), Transformation( [ 2, 4, 2, 4 ] ), 
  Transformation( [ 2, 4, 4, 4 ] ), Transformation( [ 4, 1, 1, 1 ] ), 
  Transformation( [ 4, 1, 2, 1 ] ), Transformation( [ 4, 1, 4, 1 ] ), 
  Transformation( [ 4, 2, 1, 2 ] ), Transformation( [ 4, 2, 2, 2 ] ), 
  Transformation( [ 4, 2, 4, 2 ] ), Transformation( [ 4, 2, 4, 4 ] ), 
  Transformation( [ 4, 4, 1, 4 ] ), Transformation( [ 4, 4, 2, 2 ] ), 
  Transformation( [ 4, 4, 2, 4 ] ), Transformation( [ 4, 4, 4, 2 ] ) ]
gap> Length(last);
30
gap> D:=List(reps, x-> DClass(s, x));
[ {Transformation( [ 1, 2, 3, 4 ] )}, {Transformation( [ 1, 3, 2, 3 ] )}, 
  {Transformation( [ 1, 4, 1, 2 ] )}, {Transformation( [ 1, 4, 1, 2 ] )}, 
  {Transformation( [ 4, 1, 2, 1 ] )}, {Transformation( [ 4, 2, 1, 2 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 1, 4 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 2, 4 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 4, 4, 2, 2 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )}, {Transformation( [ 2, 4, 2, 4 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )} ]
gap> Length(Set(D));
9
gap> List(D, x-> LClass(x, Representative(x)));
[ {Transformation( [ 1, 2, 3, 4 ] )}, {Transformation( [ 1, 3, 2, 3 ] )}, 
  {Transformation( [ 1, 4, 1, 2 ] )}, {Transformation( [ 1, 4, 1, 2 ] )}, 
  {Transformation( [ 4, 1, 2, 1 ] )}, {Transformation( [ 4, 2, 1, 2 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 1, 4 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 2, 4 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 4, 4, 2, 2 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )}, {Transformation( [ 2, 4, 2, 4 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )} ]
gap> Union(last);
[ Transformation( [ 1, 1, 1, 1 ] ), Transformation( [ 1, 1, 4, 1 ] ), 
  Transformation( [ 1, 2, 1, 4 ] ), Transformation( [ 1, 2, 3, 2 ] ), 
  Transformation( [ 1, 2, 3, 4 ] ), Transformation( [ 1, 2, 4, 2 ] ), 
  Transformation( [ 1, 2, 4, 4 ] ), Transformation( [ 1, 3, 2, 3 ] ), 
  Transformation( [ 1, 4, 1, 2 ] ), Transformation( [ 1, 4, 1, 4 ] ), 
  Transformation( [ 1, 4, 2, 2 ] ), Transformation( [ 1, 4, 2, 4 ] ), 
  Transformation( [ 1, 4, 4, 4 ] ), Transformation( [ 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 4, 2 ] ), Transformation( [ 2, 2, 4, 4 ] ), 
  Transformation( [ 2, 4, 2, 2 ] ), Transformation( [ 2, 4, 2, 4 ] ), 
  Transformation( [ 2, 4, 4, 4 ] ), Transformation( [ 4, 1, 1, 1 ] ), 
  Transformation( [ 4, 1, 2, 1 ] ), Transformation( [ 4, 1, 4, 1 ] ), 
  Transformation( [ 4, 2, 1, 2 ] ), Transformation( [ 4, 2, 2, 2 ] ), 
  Transformation( [ 4, 2, 4, 2 ] ), Transformation( [ 4, 2, 4, 4 ] ), 
  Transformation( [ 4, 4, 1, 4 ] ), Transformation( [ 4, 4, 2, 2 ] ), 
  Transformation( [ 4, 4, 2, 4 ] ), Transformation( [ 4, 4, 4, 2 ] ) ]
gap> Set(last2)=Set(LClasses(s));
false
gap> L:=Set(last3);
[ {Transformation( [ 1, 1, 1, 1 ] )}, {Transformation( [ 1, 2, 3, 4 ] )}, 
  {Transformation( [ 1, 3, 2, 3 ] )}, {Transformation( [ 1, 4, 1, 2 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 2, 4 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )}, {Transformation( [ 4, 1, 2, 1 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )} ]
gap> s:=RandomBlockGroup(4, 100);
<semigroup with 4 generators>
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 
  20, 22, 23, 24, 25, 26, 28, 29, 31, 32, 33, 35, 36, 37, 38, 41, 42, 44, 45, 
  50, 51, 52, 54, 55, 60, 62, 64, 65, 66, 68, 71, 73, 75, 77, 78, 79, 83, 84, 
  94, 95, 96, 97 ], [ 30, 56, 33, 17, 43, 34, 28, 78, 91, 24, 44, 84, 71, 81, 
  57, 90, 20, 69, 70, 6, 82, 26, 53, 86, 32, 22, 12, 95, 59, 40, 73, 76, 98, 
  48, 80, 51, 9, 27, 49, 93, 52, 60, 94, 11, 75, 96, 72, 4, 87, 37, 29, 50, 
  39, 45, 88, 67, 14, 99 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 19, 
  20, 21, 23, 24, 25, 26, 28, 30, 32, 35, 36, 37, 41, 42, 43, 47, 48, 49, 50, 
  51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 64, 65, 67, 68, 69, 71, 72, 74, 76, 
  81, 82, 83, 84, 86, 87, 92, 93 ], [ 56, 4, 87, 14, 67, 82, 17, 73, 18, 12, 
  35, 43, 80, 99, 7, 96, 58, 76, 36, 30, 98, 26, 62, 1, 75, 27, 10, 74, 55, 
  47, 37, 95, 39, 52, 84, 72, 50, 53, 77, 24, 59, 66, 9, 49, 70, 6, 51, 89, 
  21, 11, 85, 15, 19, 28, 79, 40, 34, 71, 5, 29, 88, 16, 8 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 
  19, 20, 22, 23, 24, 25, 26, 27, 28, 31, 32, 33, 34, 35, 36, 37, 39, 41, 42, 
  44, 46, 50, 52, 53, 54, 56, 57, 58, 59, 61, 62, 63, 64, 65, 68, 70, 71, 72, 
  77, 81, 84, 88, 89, 91, 93, 95, 97, 99, 100 ], 
[ 53, 10, 43, 41, 57, 14, 68, 20, 54, 62, 5, 49, 86, 56, 91, 48, 9, 87, 33, 
  64, 60, 13, 70, 92, 80, 69, 35, 88, 98, 4, 96, 79, 94, 71, 61, 27, 89, 97, 
  46, 28, 40, 3, 100, 17, 19, 39, 82, 52, 6, 16, 77, 76, 45, 67, 23, 31, 29, 
  12, 95, 72, 85, 7, 26, 38, 18, 24 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 16, 20, 21, 23, 
  24, 25, 26, 27, 28, 29, 31, 32, 33, 34, 35, 36, 38, 40, 41, 42, 43, 44, 47, 
  48, 49, 50, 53, 54, 55, 56, 58, 59, 62, 64, 65, 66, 68, 69, 70, 72, 74, 76, 
  78, 83, 84, 86, 90, 91, 92, 93, 94, 99, 100 ], 
[ 3, 77, 85, 63, 47, 30, 68, 21, 95, 13, 49, 33, 62, 6, 78, 81, 83, 35, 69, 
  50, 26, 61, 27, 93, 56, 39, 48, 5, 19, 52, 73, 12, 8, 89, 25, 86, 84, 14, 
  70, 29, 58, 88, 43, 37, 10, 92, 65, 22, 76, 38, 74, 34, 4, 94, 82, 67, 60, 
  2, 23, 59, 80, 11, 40, 98, 51, 28 ] ) ]
gap> f:=Random(s);
<partial perm on 27 pts>
gap> Display(f);
PartialPermNC( [ 5, 7, 11, 12, 14, 24, 25, 26, 27, 29, 31, 32, 34, 35, 41, 
  42, 44, 47, 48, 49, 50, 53, 62, 69, 70, 86, 92 ], 
[ 23, 52, 39, 62, 11, 47, 94, 34, 70, 50, 73, 89, 2, 86, 14, 81, 74, 83, 77, 
  92, 48, 26, 13, 98, 84, 60, 33 ] )
gap> d:=DClass(s, f);
{<partial perm on 27 pts>}
gap> Size(d);
1
gap> RhoOrb(d);
<closed orbit, 1 points with Schreier tree with log with grading>
gap> LambdaOrb(d);
<closed orbit, 35494 points with Schreier tree with log>
gap> f:=Random(s);
[ 5, 7, 56, 83, 92 ] -> [ 30, 52, 16, 21, 29 ]
gap> d:=DClassNC(s, f);
{[ 5, 7, 56, 83, 92 ] -> [ 30, 52, 16, 21, 29 ]}
gap> Size(d);
1
gap> iter:=IteratorOfDClasses(s);
<iterator of D-classes>
gap> repeat d:=NextIterator(iter); until Size(d)>1;
gap> d;
{[ 74, 84 ] -> [ 26, 6 ]}
gap> Size(d);
6793298
gap> f:=Random(d);
[ 1, 88 ] -> [ 78, 48 ]
gap> f in d;
true
gap> r:=RClass(d, f);
{[ 1, 88 ] -> [ 26, 6 ]}
gap> ForAll(r, x-> x in d);
true
gap> Size(r);
3686
gap> NrLClasses(d)*last;
6793298
gap> SchutzenbergerGroup(r);
Group([ (6,26) ])
gap> SchutzenbergerGroup(d);
Group([ (6,26) ])
gap> IsRegularDClass(d);
true
gap> IsRegularRClass(r);
true
gap> ForAll(r, x-> x in r);
true
gap> f:=Random(s);
<partial perm on 63 pts>
gap> d:=DClassNC(s, f);
{<partial perm on 63 pts>}
gap> Size(d);
1
gap> repeat d:=NextIterator(iter); until Size(d)>1;
gap> d;
{[ 41, 50 ] -> [ 26, 6 ]}
gap> Size(d);
3686
gap> f:=Random(d);
[ 41, 50 ] -> [ 17, 32 ]
gap> RereadPackage("citrus/greens.gi");
false
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> r:=RClassNC(d, f);
Error, Variable: 'l' must have an assigned value in
  return l; called from 
GreensRClassOfElementNC( arg[1], arg[2] ) called from
<function "RClassNC">( <arguments> )
 called from read-eval loop at line 149 of *stdin*
you can 'return;' after assigning a value
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> r:=RClassNC(d, f);
{[ 41, 50 ] -> [ 6, 26 ]}
gap> Size(r);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `LambdaOrb' on 1 arguments called from
LambdaOrb( r ) called from
SchutzenbergerGroup( r ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 151 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> r:+
Syntax error: ; expected
r:+
 ^
> ;
Syntax error: expression expected
;
^
gap> r:=RClassNC(d, f);
{[ 41, 50 ] -> [ 6, 26 ]}
gap> Size(r);
3686
gap> ForAll(r, x-> x in d);
true
gap> d=r;
true
gap> rr:=RClass(s, f);
{[ 41, 50 ] -> [ 26, 6 ]}
gap> rr=r;
true
gap> r=rr;
true
gap> d;
{[ 41, 50 ] -> [ 26, 6 ]}
gap> GroupHClass(d);
fail
gap>   
gap> gens:=[ Transformation( [ 2, 6, 7, 2, 6, 9, 9, 1, 1, 5 ] ),
>   Transformation( [ 3, 1, 4, 2, 5, 2, 1, 6, 1, 7 ] ), 
>   Transformation( [ 3, 8, 1, 9, 9, 4, 10, 5, 10, 6 ] ),
>   Transformation( [ 4, 7, 6, 9, 10, 1, 3, 6, 6, 2 ] ),
>   Transformation( [ 5, 9, 10, 9, 6, 3, 8, 4, 6, 5 ] ),
>   Transformation( [ 6, 2, 2, 7, 8, 8, 2, 10, 2, 4 ] ),
>   Transformation( [ 6, 2, 8, 4, 7, 5, 8, 3, 5, 8 ] ),
>   Transformation( [ 7, 1, 4, 3, 2, 7, 7, 6, 6, 5 ] ),
>   Transformation( [ 7, 10, 10, 1, 7, 9, 10, 4, 2, 10 ] ),
>   Transformation( [ 10, 7, 10, 8, 8, 7, 5, 9, 1, 9 ] ) ];;
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> f:=Random(s);
Transformation( [ 6, 6, 6, 6, 6, 10, 6, 6, 6, 6 ] )
gap> d:=DClassNC(s, f);
{Transformation( [ 6, 6, 6, 6, 6, 10, 6, 6, 6, 6 ] )}
gap> Size(d);
31680
gap> IsRegularDClass(d);
true
gap> GroupHClass(d);
{Transformation( [ 10, 10, 10, 10, 10, 6, 10, 10, 10, 10 ] )}
gap> s:=RandomBlockGroup(4, 10);
<semigroup with 4 generators>
gap> Size(s);
789
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 3, 4, 6, 10 ], [ 3, 4, 1, 6, 10 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6 ], [ 10, 3, 9, 1, 5, 8 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 6, 10 ], [ 1, 8, 2, 3, 4, 9 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 8, 9, 10 ], [ 5, 8, 9, 7, 2, 6, 10 ] ) ]
gap> NrDClasses(s);
251
gap> d:=DClasses(s)[251];
{[ 2, 4 ] -> [ 2, 7 ]}
gap> Size(d);
1
gap> First(DClasses(s), IsRegularDClass);
{[ 1, 3, 4, 6, 10 ] -> [ 3, 4, 1, 6, 10 ]}
gap> d:=last;
{[ 1, 3, 4, 6, 10 ] -> [ 3, 4, 1, 6, 10 ]}
gap> Size(d);
3
gap> GroupHClass(d);
{<identity on [ 1, 3, 4, 6, 10 ]>}
gap> Size(last);
3
gap> h:=last2;
{<identity on [ 1, 3, 4, 6, 10 ]>}
gap> h=d;
true
gap> Elements(h);
[ <identity on [ 1, 3, 4, 6, 10 ]>, [ 1, 3, 4, 6, 10 ] -> [ 3, 4, 1, 6, 10 ], 
  [ 1, 3, 4, 6, 10 ] -> [ 4, 1, 3, 6, 10 ] ]
gap> Number(DClasses(s), IsRegularDClass);
6
gap> List(DClasses(s), Idempotents);
[ [ <identity on [ 1, 3, 4, 6, 10 ]> ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [ <identity on [ 10 ]>, <identity on [ 9 ]>, <identity on [ 3 ]>, 
      <identity on [ 6 ]>, <identity on [ 8 ]>, <identity on [ 4 ]>, 
      <identity on [ 2 ]>, <identity on [ 1 ]> ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [ <identity on [ 5 ]> ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [ <identity on [ 2, 8, 10 ]> ], [  ], [ <empty mapping> ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [ <identity on [ 3, 4 ]>, <identity on [ 1, 4 ]>, <identity on [ 1, 3 ]> ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ] ]
gap> Concatenation(last);
[ <identity on [ 1, 3, 4, 6, 10 ]>, <identity on [ 10 ]>, <identity on [ 9 ]>,
  <identity on [ 3 ]>, <identity on [ 6 ]>, <identity on [ 8 ]>, 
  <identity on [ 4 ]>, <identity on [ 2 ]>, <identity on [ 1 ]>, 
  <identity on [ 5 ]>, <identity on [ 2, 8, 10 ]>, <empty mapping>, 
  <identity on [ 3, 4 ]>, <identity on [ 1, 4 ]>, <identity on [ 1, 3 ]> ]
gap> ForAll(last, x-> x in s);
true
gap> Set(last2)=Idempotents(s);
false
gap> Set(last3)=Set(Idempotents(s));
true
gap> gens:=[Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] ),
> Transformation( [ 2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7 ] )];;
gap> s:=Monoid(gens);;
gap> List(DClasses(s), Idempotents);
[ [ Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] ) ], 
  [ Transformation( [ 1, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ) ], 
  [ Transformation( [ 4, 2, 2, 4, 5, 6, 7, 7, 7, 10, 11 ] ), 
      Transformation( [ 2, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
      Transformation( [ 6, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
      Transformation( [ 7, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
      Transformation( [ 5, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
      Transformation( [ 10, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
      Transformation( [ 11, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
      Transformation( [ 4, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ) ] ]
gap> Concatenation(last);
[ Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] ), 
  Transformation( [ 1, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 4, 2, 2, 4, 5, 6, 7, 7, 7, 10, 11 ] ), 
  Transformation( [ 2, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 6, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 7, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 5, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 10, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 11, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 4, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ) ]
gap> e:=last;
[ Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] ), 
  Transformation( [ 1, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 4, 2, 2, 4, 5, 6, 7, 7, 7, 10, 11 ] ), 
  Transformation( [ 2, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 6, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 7, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 5, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 10, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 11, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 4, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ) ]
gap> IsDuplicateFree(e);
true
gap> ForAll(e, x-> x in s);
true
gap> Set(Idempotents(s))=Set(e);
true
gap> s:=RandomBlockGroup(4, 10);
<semigroup with 4 generators>
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 3, 4, 5, 6 ], [ 7, 10, 8, 6, 4, 2 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 9 ], [ 6, 8, 3, 10, 4, 2 ] ),
 PartialPermNC( [ 1, 2, 3, 5, 6, 7 ], [ 8, 7, 5, 6, 2, 9 ] ),
 PartialPermNC( [ 1, 2, 3, 5, 6, 8 ], [ 9, 3, 4, 7, 8, 6 ] ) ]
gap> Size(s);
489
gap> First(DClasses(s), IsRegularDClass);
{<empty mapping>}
gap> NrRegularDClasses(s);
5
gap> PositionsProperty(DClasses(s), IsRegularDClass);
[ 25, 26, 33, 36, 64 ]
gap> d:=DClasses(s)[26];
{[ 3 ] -> [ 8 ]}
gap> NrLClasses(d);
8
gap> NrRClasses(d);
8
gap> Size(d);
64
gap> Idempotents(d);
[ <identity on [ 8 ]>, <identity on [ 6 ]>, <identity on [ 3 ]>, 
  <identity on [ 5 ]>, <identity on [ 7 ]>, <identity on [ 2 ]>, 
  <identity on [ 9 ]>, <identity on [ 4 ]> ]
gap> ForAll(last, x-> x in d);
true
gap> dd:=DClassNC(s, Random(d));
{[ 8 ] -> [ 9 ]}
gap> dd=d;
true
gap> Size(dd);
64
gap> Idempotents(dd);
[ <identity on [ 9 ]>, <identity on [ 2 ]>, <identity on [ 8 ]>, 
  <identity on [ 7 ]>, <identity on [ 3 ]>, <identity on [ 6 ]>, 
  <identity on [ 5 ]>, <identity on [ 4 ]> ]
gap> Set(LClassReps(dd))=Set(LClassReps(d));
false
gap> LClassReps(dd);
[ [ 8 ] -> [ 9 ], [ 8 ] -> [ 2 ], <identity on [ 8 ]>, [ 8 ] -> [ 7 ], 
  [ 8 ] -> [ 3 ], [ 8 ] -> [ 6 ], [ 8 ] -> [ 5 ], [ 8 ] -> [ 4 ] ]
gap> LClassReps(d);
[ [ 3 ] -> [ 8 ], [ 3 ] -> [ 6 ], <identity on [ 3 ]>, [ 3 ] -> [ 5 ], 
  [ 3 ] -> [ 7 ], [ 3 ] -> [ 2 ], [ 3 ] -> [ 9 ], [ 3 ] -> [ 4 ] ]
gap> Set(List(LClassReps(d), x-> LClass(d, x)))=Set(List(LClassReps(dd), x-> LClass(d, x)));
true
gap> Set(List(LClassReps(d), x-> LClass(d, x)))=Set(List(LClassReps(dd), 
> x-> LClass(dd, x));
Syntax error: ) expected
x-> LClass(dd, x));
                  ^
gap> Set(List(LClassReps(d), x-> LClass(d, x)))=Set(List(LClassReps(dd),
> x-> LClass(dd, x)));
true
gap> ForAll(LClassReps(dd), x-> x in d);
true
gap> ForAll(LClassReps(d), x-> x in dd);
true
gap> quit;
gap> gens:=[ Transformation( [ 2, 6, 7, 2, 6, 9, 9, 1, 1, 5 ] ),
>   Transformation( [ 3, 1, 4, 2, 5, 2, 1, 6, 1, 7 ] ),
>   Transformation( [ 3, 8, 1, 9, 9, 4, 10, 5, 10, 6 ] ),
>   Transformation( [ 4, 7, 6, 9, 10, 1, 3, 6, 6, 2 ] ),
>   Transformation( [ 5, 9, 10, 9, 6, 3, 8, 4, 6, 5 ] ),
>   Transformation( [ 6, 2, 2, 7, 8, 8, 2, 10, 2, 4 ] ),
>   Transformation( [ 6, 2, 8, 4, 7, 5, 8, 3, 5, 8 ] ),
>   Transformation( [ 7, 1, 4, 3, 2, 7, 7, 6, 6, 5 ] ),
>   Transformation( [ 7, 10, 10, 1, 7, 9, 10, 4, 2, 10 ] ),
>   Transformation( [ 10, 7, 10, 8, 8, 7, 5, 9, 1, 9 ] ) ];;
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> s:=RandomBlockGroup(10, 15);
<semigroup with 10 generators>
gap> s:=RandomBlockGroup(20, 10);
<semigroup with 20 generators>
gap> Size(s);
Error, user interrupt in
  val := HTValue( lambdarhoht, rhoy ); called from 
Enumerate( SemigroupData( s ), infinity, ReturnFalse ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 114 of *stdin*
you can 'return;'
brk> SemigroupData(s);
<semigroup data: 192799 reps, 13350 lambda-rho values>
brk> Size(last);
875432
brk> quit;
gap> s:=RandomBlockGroup(100, 10);
<semigroup with 100 generators>
gap> Size(last);
Error, user interrupt in
  x := [ s, m, o, y, nr + 1 ]; called from 
Enumerate( SemigroupData( s ), infinity, ReturnFalse ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 115 of *stdin*
you can 'return;'
brk> s;
<semigroup with 100 generators>
brk> SemigroupData(s);
<semigroup data: 60625 reps, 6612 lambda-rho values>
brk> Size(last);
30381694
brk> SemigroupData(s)!.nr;
Error, Record: '<rec>.nr' must have an assigned value in
  <compiled or corrupted statement>  called from 
Enumerate( SemigroupData( s ), infinity, ReturnFalse ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 4 of *errin*
you can 'return;' after assigning a value
brk_2> quit;
brk> SemigroupData(s)!.pos;
0
brk> pos;
589
brk> quit;
gap> s:=RandomBlockGroup(10, 8);
<semigroup with 10 generators>
gap> Size(s);
17839
gap> s:=RandomBlockGroup(10, 10);
<semigroup with 10 generators>
gap> Size(s);
160088
gap> time;
7903
gap> NrRClasses(s);
104062
gap> List(GeneratorsOfGroup(SymmetricGroup(10)), AsPartialPerm);
[ [ 1 .. 10 ] -> [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1 ], [ 1, 2 ] -> [ 2, 1 ] ]
gap> List(GeneratorsOfGroup(SymmetricGroup(10)), x-> AsPartialPerm(x, 10));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `AsPartialPerm' on 2 arguments called from
AsPartialPerm( x, 10 ) called from
func( C[i] ) called from
<function "List">( <arguments> )
 called from read-eval loop at line 122 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> List(GeneratorsOfGroup(SymmetricGroup(10)), x-> AsPartialPerm(x, [1..10]));
[ [ 1 .. 10 ] -> [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1 ], 
  [ 1 .. 10 ] -> [ 2, 1, 3, 4, 5, 6, 7, 8, 9, 10 ] ]
gap> gens:=last;
[ [ 1 .. 10 ] -> [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1 ], 
  [ 1 .. 10 ] -> [ 2, 1, 3, 4, 5, 6, 7, 8, 9, 10 ] ]
gap> Append(gens, RandomPartialPerm(10));
Error, AppendList: <list2> must be a small list (not a object (data))
not in any function at line 124 of *stdin*
you can replace <list2> via 'return <list2>;'
brk> quit;
gap> Add(gens, RandomPartialPerm(10));
gap> gens;
[ [ 1 .. 10 ] -> [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1 ], 
  [ 1 .. 10 ] -> [ 2, 1, 3, 4, 5, 6, 7, 8, 9, 10 ], 
  [ 1, 2, 4, 7, 10 ] -> [ 8, 5, 9, 6, 7 ] ]
gap> Display(gens);
[ PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], 
[ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], 
[ 2, 1, 3, 4, 5, 6, 7, 8, 9, 10 ] ),
 PartialPermNC( [ 1, 2, 4, 7, 10 ], [ 8, 5, 9, 6, 7 ] ) ]
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> Size(s);
12398231
gap> NrRClasses(s);
639
gap> f:=Random(s);
[ 3, 9 ] -> [ 5, 4 ]
gap> d:=DClass(s, f);
{[ 3, 9 ] -> [ 7, 6 ]}
gap> Position(LambdaOrb(d), RanSetPP(Representative(d)));
7
gap> OrbSCC(RhoOrb(d))[RhoOrbSCCIndex(d)];
[ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 
  22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 
  41, 42, 43, 44, 45 ]
gap> OrbSCC(LambdaOrb(d))[LambdaOrbSCCIndex(d)];
[ 7, 13, 20, 27, 34, 36, 45, 48, 49, 53, 62, 65, 66, 67, 71, 73, 86, 89, 90, 
  92, 95, 110, 113, 114, 115, 116, 119, 120, 137, 139, 142, 143, 147, 148, 
  170, 172, 173, 177, 178, 208, 209, 214, 244, 245, 278 ]
gap> NrIdempotents(d);
45
gap> Number(Idempotents(s), x-> x in d);
45
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> d:=DClass(s, f);
{[ 3, 9 ] -> [ 7, 6 ]}
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> d:=DClassNC(s, f);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> NrIdempotents(d);
45
gap> Number(Idempotents(s), x-> x in d);
45
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> l:=LClass(s, f);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> d:=DClassOfLClass(l);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> NrIdempotents(d);
45
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> l:=LClass(s, f);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> l:=LClassNC(s, f);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> d:=DClassOfLClass(l);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> NrIdempotents(d);
45
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> r:=RClass(s, f);
{[ 3, 9 ] -> [ 6, 7 ]}
gap> d:=DClassOfRClass(r);
{[ 3, 9 ] -> [ 6, 7 ]}
gap> NrIdempotents(d);
45
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> r:=RClassNC(s, f);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> d:=DClassOfRClass(r);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> NrIdempotents(d);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `RhoOrbSCCIndex' on 1 arguments called from
RhoOrbSCCIndex( x ) called from
RhoOrbSCC( d ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 160 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s;=Semigroup(Concatenation(Generators(last)));
<semigroup with 3 generators>
Syntax error: expression expected
s;=Semigroup(Concatenation(Generators(last)));
  ^
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> r:=RClassNC(s, f);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> d:=DClassOfRClass(r);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> NrIdempotents(d);
45
gap> IsGreensClassNC(d);
true
gap> IsGreensClassNC(r);
true
gap> NrRegularDClasses(s);
7
gap> NrDClasses(s);
7
gap> IsRegularSemigroup(s);
true
gap> gens:=[Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] ),
> Transformation( [ 2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7 ] )];;
gap> s:=Monoid(gens);;
gap> 
gap> NrRegularDClasses(s);
3
gap> NrDClasses(s);
3
gap> IsRegularSemigroup(s);
true
gap> d:=Random(DClasses(s));
{Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] )}
gap> NrHClasses(d);
1
gap> GroupHClass(d);
{Transformation( [ 1, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] )}
gap> last=d;
true
gap> Size(d);
7
gap> Size(last3);
7
gap> s:=RandomBlockGroup(2, 12);
<semigroup with 2 generators>
gap> Size(s);
251
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 3, 5, 6, 7, 12 ], [ 11, 10, 3, 4, 6, 2, 8 ] ),
 PartialPermNC( [ 1, 2, 4, 5, 6, 8, 9, 10, 11 ], 
[ 2, 8, 1, 10, 11, 4, 7, 6, 9 ] ) ]
gap> d:=Random(DClasses(s));
{[ 5, 12 ] -> [ 6, 9 ]}
gap> NrHClasses(d);
1
gap> List(DClasses(s), NrHClasses);
[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 81, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  9, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 25, 1, 1, 1, 1, 1, 1, 1, 1, 4, 
  1, 9, 1, 4, 1, 1, 1, 1, 1, 10, 1, 1, 4, 1, 1, 10, 1, 1, 1, 4, 1, 1, 1 ]
gap> Sum(last);
223
gap> NrHClasses(s);
223
gap> s:=RandomBlockGroup(2, 12);
<semigroup with 2 generators>
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 3, 4, 9, 10, 11 ], [ 4, 1, 7, 12, 3, 9, 6 ] ),
 PartialPermNC( [ 1, 3, 4, 5, 7, 8, 11, 12 ], [ 4, 11, 2, 7, 9, 8, 1, 6 ] ) ]
gap> f:=Random(s);
[ 4, 7, 11 ] -> [ 2, 9, 6 ]
gap> d:=DClassNC(s, f);
{[ 4, 7, 11 ] -> [ 2, 9, 6 ]}
gap> NrHClasses(s);
125
gap> d:=DClass(s, f);
{[ 4, 7, 11 ] -> [ 2, 9, 6 ]}
gap> NrHClasses(s);
125
gap> NrHClasses(d);
1
gap> d:=DClassNC(s, f);
{[ 4, 7, 11 ] -> [ 2, 9, 6 ]}
gap> NrHClasses(d);
1
gap> d:=DClass(LClass(s, f));
{[ 4, 7, 11 ] -> [ 2, 9, 6 ]}
gap> NrHClasses(d);
1
gap> d:=DClass(RClass(s, f));
{[ 4, 7, 11 ] -> [ 2, 9, 6 ]}
gap> NrHClasses(d);
1
gap> NrRegularDClasses(s);
4
gap> NrDClasses(s);
65
gap> RClassReps(d);
[ [ 4, 7, 11 ] -> [ 2, 9, 6 ] ]
gap> iter:=IteratorOfDClasses(s);
<iterator of D-classes>
gap> repeat d:=NextIterator(iter); until IsDoneIterator(iter) or Size(d)>1000;
gap> d;
{[ 1, 5 ] -> [ 6, 4 ]}
gap> Size(d);
1
gap> IsDTrivial(s);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `IsRTrivial' on 1 arguments called from
<function "HANDLE_METHOD_NOT_FOUND">( <arguments> )
 called from read-eval loop at line 213 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> List(DClasses(s), Size));
Syntax error: ; expected
List(DClasses(s), Size));
                       ^
gap> List(DClasses(s), Size);
[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 3, 9, 1, 1, 1, 3, 1, 1, 1, 1, 
  1, 3, 9, 1, 3, 3, 3, 3, 1, 3, 3, 1, 3, 1, 3, 1, 1, 1, 1, 1, 3, 3, 1, 3, 1, 
  3, 3, 9, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
gap> Position(last, 9);
17
gap> d:=DClasses(s)[17];
{[ 2, 9 ] -> [ 4, 7 ]}
gap> Size(d);
9
gap> IsRegularDClass(d);
true
gap> RClassReps(d);
[ [ 2, 9 ] -> [ 4, 7 ], [ 1, 3 ] -> [ 4, 7 ], <identity on [ 4, 7 ]> ]
gap> d:=DClassNC(s, Representative(d));
{[ 2, 9 ] -> [ 4, 7 ]}
gap> RClassReps(d);
[ [ 2, 9 ] -> [ 4, 7 ], <identity on [ 4, 7 ]>, [ 1, 3 ] -> [ 4, 7 ] ]
gap> s:=Semigroup(Generators(s));
<semigroup with 2 generators>
gap> d:=DClass(HClass(s, Representative(d)));
{[ 2, 9 ] -> [ 4, 7 ]}
gap> RClassReps(d);
[ [ 2, 9 ] -> [ 4, 7 ], <identity on [ 4, 7 ]>, [ 1, 3 ] -> [ 4, 7 ] ]
gap> Size(d);
9
gap> Number(s, x-> x in d);
9
gap> ForAll(d, x-> x in d);
true
gap> HClassReps(d);
[ [ 2, 9 ] -> [ 4, 7 ], <identity on [ 2, 9 ]>, [ 2, 9 ] -> [ 1, 3 ], 
  <identity on [ 4, 7 ]>, [ 4, 7 ] -> [ 2, 9 ], [ 4, 7 ] -> [ 1, 3 ], 
  [ 1, 3 ] -> [ 4, 7 ], [ 1, 3 ] -> [ 2, 9 ], <identity on [ 1, 3 ]> ]
gap> Set(last)=Elements(d);
true
gap> gens:=[Transformation([2,1,4,5,3,7,8,9,10,6]),
> Transformation([1,2,4,3,5,6,7,8,9,10]),
> Transformation([1,2,3,4,5,6,10,9,8,7]),
> Transformation([9,1,4,3,6,9,3,4,3,9])];;
gap> s:=Monoid(gens);;
gap> 
gap> f:=Random(s);
Transformation( [ 2, 1, 3, 5, 4, 10, 9, 8, 7, 6 ] )
gap> d:=DClass(HClass(s, Representative(d)));
Error, the element does not belong to the semigroup, called from
GreensHClassOfElement( arg[1], arg[2] ) called from
<function "HClass">( <arguments> )
 called from read-eval loop at line 237 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> d:=DClass(HClass(s, f));
{Transformation( [ 2, 1, 3, 5, 4, 10, 9, 8, 7, 6 ] )}
gap> Size(d);
120
gap> HClassReps(d);
[ Transformation( [ 2, 1, 3, 5, 4, 10, 9, 8, 7, 6 ] ) ]
gap> h:=GroupHClass(d);
{Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] )}
gap> h=d;
true
gap> f:=Random(s);
Transformation( [ 1, 2, 5, 3, 4, 8, 9, 10, 6, 7 ] )
gap> Size(s);
491558
gap> f:=Random(s);
Transformation( [ 6, 6, 3, 6, 4, 6, 6, 6, 6, 4 ] )
gap> d:=DClass(HClass(s, f));
{Transformation( [ 9, 9, 4, 9, 3, 9, 9, 9, 9, 3 ] )}
gap> Size(d);
121500
gap> NrHClasses(d);
20250
gap> Length(HClassReps(d));
20250
gap> ForAll(HClassReps(d), x-> x in d);
true
gap> d:=DClass(RClass(s, f));
{Transformation( [ 3, 3, 4, 3, 9, 3, 3, 3, 3, 9 ] )}
gap> Size(d);
121500
gap> ForAll(d, x-> x in d);
true
gap> NrIdempotents(d);
5550
gap> ForAll(Idempotents(d), x-> x in d);
true

#
gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;

#
gap> Unbind(gens); Unbind(s); Unbind(f); Unbind(r); Unbind(l); Unbind(iter);
gap> STOP_TEST( "Citrus package: everyfunction.tst", 0);


