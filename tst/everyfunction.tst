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
{[ 2, 8, 9 ] -> [ 8, 10, 5 ]}
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
gap> Number(last, x-> x=true)
> ;
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
{[ 2, 3 ] -> [ 2, 4 ]}
gap> HClassReps(l);
[ [ 2, 3 ] -> [ 2, 4 ], [ 2, 3 ] -> [ 4, 2 ], [ 1, 2 ] -> [ 2, 4 ], 
  [ 1, 2 ] -> [ 4, 2 ] ]
gap> IsRegularLClass(l);
false
gap> ForAll(HClassReps(l), x-> x in l);
true
gap> d:=DClassOfLClass(l);
{[ 2, 3 ] -> [ 2, 4 ]}
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

#
gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;

#
gap> Unbind(gens); Unbind(s); Unbind(f); Unbind(r); Unbind(l); Unbind(iter);
gap> STOP_TEST( "Citrus package: everyfunction.tst", 0);
