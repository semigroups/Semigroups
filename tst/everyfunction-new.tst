#############################################################################
###
##W  everyfunction.tst
##Y  Copyright (C) 2011-12                                James D. Mitchell
###
###  Licensing information can be found in the README file of this package.
###
##############################################################################
###
gap> START_TEST("Semigroups package: everyfunction.tst"); 
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

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
<transformation semigroup on 8 pts with 8 generators>
gap> Size(s);
597369
gap> f:=Transformation( [ 8, 1, 5, 5, 8, 3, 7, 8 ] );;
gap> l:=LClassNC(s, f);
{Transformation( [ 8, 1, 5, 5, 8, 3, 7, 8 ] )}
gap> RhoOrbStabChain(l);
true
gap> Size(l);
4560
gap> RhoOrbSCC(l);
[ 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 
  23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39 ]
gap> SchutzenbergerGroup(l);
Sym( [ 1, 3, 5, 7, 8 ] )
gap> ForAll(l, x-> x in l);
true
gap> d:=DClass(s, f);
{Transformation( [ 8, 1, 5, 5, 8, 3, 7, 8 ] )}
gap> iter:=Iterator(d);
<iterator of D-class>
gap> for i in iter do od;

#
gap> gens:=[ PartialPermNC( [ 1, 2, 3, 5, 7, 10 ], [ 12, 3, 1, 11, 9, 5 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 7, 8 ], [ 4, 3, 11, 12, 6, 2, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 9, 11 ], [ 11, 6, 9, 2, 4, 8, 12 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 7, 9, 12 ], [ 7, 1, 12, 2, 9, 4, 5 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 7, 8, 9 ], [ 5, 4, 8, 11, 6, 12, 1 ] ),
>  PartialPermNC( [ 1, 2, 4, 6, 8, 9, 10 ], [ 8, 5, 2, 12, 4, 7, 11 ] )];;
gap> s:=Semigroup(gens);
<partial perm semigroup on 12 pts with 6 generators>
gap> f:=PartialPermNC([ 3, 4, 5, 11 ], [ 4, 1, 2, 5 ]);;
gap> l:=LClassNC(s, f);
{PartialPerm( [ 3, 4, 5, 11 ], [ 4, 1, 2, 5 ] )}
gap> l:=LClass(s, f);
{PartialPerm( [ 3, 4, 5, 11 ], [ 4, 1, 2, 5 ] )}
gap> d:=DClass(s, f);
{PartialPerm( [ 3, 4, 5, 11 ], [ 4, 1, 2, 5 ] )}
gap> Size(l);
1
gap> Number(d, x-> x in l);
1
gap> Number(s, x-> x in l);
1
gap> s:=Semigroup(gens);
<partial perm semigroup on 12 pts with 6 generators>
gap> l:=LClass(s, f);
{PartialPerm( [ 3, 4, 5, 11 ], [ 4, 1, 2, 5 ] )}
gap> d:=DClass(s, f);
{PartialPerm( [ 3, 4, 5, 11 ], [ 4, 1, 2, 5 ] )}
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
{PartialPerm( [ 4 ], [ 3 ] )}
gap> Size(l);
11
gap> Elements(l);
[ [1,3], [2,3], <identity partial perm on [ 3 ]>, [4,3], [5,3], [6,3], [7,3], 
  [8,3], [9,3], [11,3], [12,3] ]
gap> ForAll(last, x-> x in s);
true
gap> ForAll(last2, x-> x in l);
true
gap> d:=DClassNC(s, f);
{PartialPerm( [ 3, 4, 5, 11 ], [ 4, 1, 2, 5 ] )}
gap> d:=DClassNC(s, Representative(l));
{PartialPerm( [ 4 ], [ 3 ] )}
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
<transformation semigroup on 8 pts with 4 generators>
gap> f:=Transformation( [ 5, 2, 7, 2, 7, 2, 5, 8 ] );;
gap> l:=LClassNC(s, f);
{Transformation( [ 5, 2, 7, 2, 7, 2, 5 ] )}
gap> enum:=Enumerator(l);
<enumerator of L-class>
gap> enum[1];
Transformation( [ 5, 2, 7, 2, 7, 2, 5 ] )
gap> enum[2];
Transformation( [ 5, 7, 2, 7, 2, 7, 5 ] )
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
[ {Transformation( [ 5, 2, 7, 2, 7, 2, 5 ] )}, 
  {Transformation( [ 2, 8, 7, 5, 5, 7, 2, 2 ] )}, 
  {Transformation( [ 7, 7, 5, 2, 7, 2, 7 ] )}, 
  {Transformation( [ 8, 8, 7, 5, 5, 7, 2 ] )}, 
  {Transformation( [ 8, 2, 7, 2, 2, 5, 8, 7 ] )}, 
  {Transformation( [ 7, 8, 5, 7, 7, 7, 7, 2 ] )}, 
  {Transformation( [ 5, 7, 7, 7, 7, 2, 5 ] )}, 
  {Transformation( [ 7, 5, 2, 8, 5, 7, 7 ] )}, 
  {Transformation( [ 7, 2, 5, 8, 5, 8, 7, 8 ] )}, 
  {Transformation( [ 2, 7, 7, 8, 8, 2, 2, 5 ] )}, 
  {Transformation( [ 7, 2, 8, 2, 2, 5, 7, 7 ] )}, 
  {Transformation( [ 8, 2, 5, 7, 7, 7, 8, 7 ] )}, 
  {Transformation( [ 7, 8, 7, 5, 5, 7, 7, 2 ] )}, 
  {Transformation( [ 7, 7, 5, 7, 7, 2, 7 ] )}, 
  {Transformation( [ 5, 8, 2, 7, 7, 5, 5, 7 ] )}, 
  {Transformation( [ 2, 5, 7, 5, 5, 7, 2 ] )}, 
  {Transformation( [ 2, 8, 5, 7, 7, 5, 2 ] )}, 
  {Transformation( [ 7, 5, 7, 2, 2, 8, 7, 2 ] )}, 
  {Transformation( [ 7, 8, 2, 7, 8, 2, 7, 5 ] )}, 
  {Transformation( [ 2, 7, 8, 7, 7, 2, 2, 5 ] )}, 
  {Transformation( [ 2, 7, 5, 8, 8, 7, 2, 7 ] )}, 
  {Transformation( [ 8, 2, 7, 7, 7, 5, 8, 7 ] )}, 
  {Transformation( [ 8, 7, 2, 5, 5, 7, 8, 5 ] )}, 
  {Transformation( [ 2, 7, 5, 8, 7, 5, 2, 7 ] )}, 
  {Transformation( [ 5, 8, 7, 2, 2, 5, 5, 7 ] )}, 
  {Transformation( [ 8, 8, 5, 2, 2, 7, 8, 5 ] )}, 
  {Transformation( [ 5, 2, 7, 7, 7, 2, 5 ] )}, 
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
  {Transformation( [ 2, 5, 7, 8, 8, 7, 2 ] )}, 
  {Transformation( [ 8, 7, 5, 2, 7, 5, 8, 5 ] )}, 
  {Transformation( [ 5, 8, 7, 2, 2, 8, 5, 7 ] )}, 
  {Transformation( [ 8, 7, 7, 5, 5, 2, 8, 8 ] )}, 
  {Transformation( [ 7, 2, 5, 8, 2, 8, 7, 7 ] )}, 
  {Transformation( [ 7, 5, 8, 7, 5, 2, 7 ] )} ]
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
<partial perm semigroup on 12 pts with 6 generators>
gap> Size(s);
4857
gap> f:=PartialPerm([ 6, 9 ], [ 12, 6 ]);;
gap> l:=LClass(s, f);
{PartialPerm( [ 6, 9 ], [ 12, 6 ] )}
gap> NrHClasses(l);
66
gap> Size(l);
66
gap> SchutzenbergerGroup(l);
Group([ (6,12) ])
gap> o:=RhoOrb(l);
<closed orbit, 40 points with Schreier tree with log with grading>
gap> d:=DClassOfLClass(l);
{PartialPerm( [ 6, 9 ], [ 12, 6 ] )}
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
[ {PartialPerm( [ 6, 9 ], [ 12, 6 ] )}, {PartialPerm( [ 6, 9 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 2, 3 ], [ 12, 6 ] )}, {PartialPerm( [ 2, 3 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 2, 7 ], [ 6, 12 ] )}, {PartialPerm( [ 2, 7 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 1, 4 ], [ 12, 6 ] )}, {PartialPerm( [ 1, 4 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 4, 9 ], [ 6, 12 ] )}, {PartialPerm( [ 4, 9 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 1, 8 ], [ 6, 12 ] )}, {PartialPerm( [ 1, 8 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 2, 9 ], [ 12, 6 ] )}, {PartialPerm( [ 2, 9 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 7, 9 ], [ 12, 6 ] )}, {PartialPerm( [ 7, 9 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 3, 9 ], [ 12, 6 ] )}, {PartialPerm( [ 3, 9 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 3, 4 ], [ 6, 12 ] )}, {PartialPerm( [ 3, 4 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 4, 7 ], [ 12, 6 ] )}, {PartialPerm( [ 4, 7 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 1, 7 ], [ 12, 6 ] )}, {PartialPerm( [ 1, 7 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 12, 6 ] )}, {PartialPerm( [ 1, 2 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 1, 9 ], [ 6, 12 ] )}, {PartialPerm( [ 1, 9 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 8, 9 ], [ 12, 6 ] )}, {PartialPerm( [ 8, 9 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 7, 8 ], [ 6, 12 ] )}, {PartialPerm( [ 7, 8 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 12, 6 ] )}, {PartialPerm( [ 2, 4 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 4, 5 ], [ 12, 6 ] )}, {PartialPerm( [ 4, 5 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 4, 8 ], [ 12, 6 ] )}, {PartialPerm( [ 4, 8 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 9, 12 ], [ 12, 6 ] )}, {PartialPerm( [ 9, 12 ], [ 6, 12 ] )}
    , {PartialPerm( [ 2, 8 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 2, 8 ], [ 12, 6 ] )}, {PartialPerm( [ 5, 9 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 5, 9 ], [ 6, 12 ] )}, {PartialPerm( [ 3, 11 ], [ 12, 6 ] )},
  {PartialPerm( [ 3, 11 ], [ 6, 12 ] )}, {PartialPerm( [ 7, 12 ], [ 6, 12 ] )}
    , {PartialPerm( [ 7, 12 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 2, 5 ], [ 12, 6 ] )}, {PartialPerm( [ 2, 5 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 6, 12 ] )}, {PartialPerm( [ 1, 3 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 4, 12 ], [ 12, 6 ] )}, {PartialPerm( [ 4, 12 ], [ 6, 12 ] )}
    , {PartialPerm( [ 5, 11 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 5, 11 ], [ 6, 12 ] )}, {PartialPerm( [ 6, 8 ], [ 6, 12 ] )},
  {PartialPerm( [ 6, 8 ], [ 12, 6 ] )}, {PartialPerm( [ 1, 5 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 1, 5 ], [ 6, 12 ] )}, {PartialPerm( [ 2, 12 ], [ 12, 6 ] )},
  {PartialPerm( [ 2, 12 ], [ 6, 12 ] )}, {PartialPerm( [ 4, 11 ], [ 12, 6 ] )}
    , {PartialPerm( [ 4, 11 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 4, 6 ], [ 12, 6 ] )}, {PartialPerm( [ 4, 6 ], [ 6, 12 ] )} ]
gap> IsDuplicateFreeList(last);
true
gap> IsRegularClass(l);
false
gap> H:=HClasses(l);;
gap> ForAll(H, x-> Representative(x) in l);
true
gap> ForAll(H, x-> Representative(x) in d);
true
gap> d;
{PartialPerm( [ 6, 9 ], [ 12, 6 ] )}
gap> Representative(l) in d;
true
gap> First(H, x-> not Representative(x) in d);
fail
gap> ForAll(l, x-> x in d);
true
gap> rep:=Representative(d);
[9,6,12]
gap> s:=Parent(d);
<partial perm semigroup of size 4857, on 12 pts with 6 generators>
gap> ElementsFamily(FamilyObj(s)) <> FamilyObj(f) 
> or RankOfPartialPerm(f)<>RankOfPartialPerm(rep);
false
gap> g:=f;
[9,6,12]
gap>   m:=LambdaOrbSCCIndex(d); o:=LambdaOrb(d); scc:=OrbSCC(o);
54
<closed orbit, 184 points with Schreier tree with log>
[ [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 6 ], [ 7 ], [ 8 ], [ 9 ], [ 10 ], 
  [ 11 ], [ 12 ], 
  [ 13, 49, 56, 62, 64, 66, 74, 80, 82, 83, 84, 85, 87, 96, 99, 101, 107, 
      108, 110, 125, 133, 134, 135, 137, 145, 146, 147, 152, 154, 159, 168, 
      171, 175 ], [ 14 ], [ 15 ], [ 16 ], [ 17 ], [ 18 ], [ 19 ], 
  [ 20, 43, 44, 46, 47, 50, 61, 63, 69, 78, 86 ], [ 21 ], [ 22 ], [ 23 ], 
  [ 24 ], [ 25 ], [ 26 ], [ 27 ], [ 28 ], [ 29 ], [ 30 ], [ 31 ], [ 32 ], 
  [ 33 ], [ 34 ], [ 35 ], [ 36 ], [ 37 ], [ 38 ], [ 39 ], [ 40 ], [ 41 ], 
  [ 42 ], [ 45 ], [ 48 ], [ 51 ], [ 52 ], [ 53, 121, 127, 164, 177 ], [ 54 ], 
  [ 55 ], [ 57 ], [ 58 ], [ 59 ], [ 60 ], [ 65 ], [ 67 ], [ 68 ], [ 70 ], 
  [ 71 ], [ 72 ], [ 73 ], [ 75 ], [ 76 ], [ 77 ], [ 79 ], [ 81 ], [ 88 ], 
  [ 89 ], [ 90 ], [ 91 ], [ 92, 142, 150, 173, 179 ], [ 93, 112, 144 ], 
  [ 94 ], [ 95 ], [ 97 ], [ 98 ], [ 100 ], [ 102 ], [ 103 ], [ 104 ], 
  [ 105 ], [ 106 ], [ 109 ], [ 111 ], [ 113 ], [ 114 ], [ 115 ], [ 116 ], 
  [ 117 ], [ 118 ], [ 119 ], [ 120 ], [ 122 ], [ 123 ], [ 124 ], [ 126 ], 
  [ 128 ], [ 129 ], [ 130 ], [ 131 ], [ 132 ], [ 136 ], [ 138 ], [ 139 ], 
  [ 140 ], [ 141 ], [ 143 ], [ 148 ], [ 149 ], [ 151 ], [ 153 ], [ 155 ], 
  [ 156 ], [ 157 ], [ 158 ], [ 160 ], [ 161 ], [ 162 ], [ 163 ], [ 165 ], 
  [ 166 ], [ 167 ], [ 169 ], [ 170 ], [ 172 ], [ 174 ], [ 176 ], [ 178 ], 
  [ 180 ], [ 181 ], [ 182 ], [ 183 ], [ 184 ] ]
gap> l:=Position(o, LambdaFunc(s)(g));
65
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
<closed orbit, 184 points with Schreier tree with log>
54
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
gap> p:=LambdaConjugator(Parent(d))(RhoOrbRep(o, m),
>    Representative(d));
()
gap> rho_schutz:=rho_schutz^p;
Group([ (6,12) ])
gap> f:=PartialPermNC([ 6, 9 ], [ 12, 6 ]);
[9,6,12]
gap> s:=Semigroup(gens);
<partial perm semigroup on 12 pts with 6 generators>
gap> l:=LClass(s, f);
{PartialPerm( [ 6, 9 ], [ 12, 6 ] )}
gap> d:=DClassOfLClass(l);
{PartialPerm( [ 6, 9 ], [ 12, 6 ] )}
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
<transformation semigroup on 8 pts with 4 generators>
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
Sym( [ 1, 2, 7 ] )
gap> IsRegularClass(l);
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
<partial perm semigroup on 10 pts with 6 generators>
gap> 
gap> f:=PartialPerm([]);;
gap> l:=LClass(s, f);
{PartialPerm( [  ], [  ] )}
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
{PartialPerm( [  ], [  ] )}
gap> h:=HClassNC(l, f);
{PartialPerm( [  ], [  ] )}
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
{PartialPerm( [ 4, 7, 8 ], [ 8, 10, 5 ] )}
gap> h:=HClassNC(l, f);
{PartialPerm( [ 2, 8, 9 ], [ 8, 10, 5 ] )}
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
<transformation monoid on 8 pts with 4 generators>
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
gap> IsRegularClass(l);
true
gap> s:=Monoid(gens);
<transformation monoid on 8 pts with 4 generators>
gap> l:=LClass(s, f);
{Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] )}
gap> IsRegularClass(l);
false
gap> Size(l);
1
gap> f:= Transformation( [ 1, 2, 2, 1, 2, 6, 6, 1 ] );
Transformation( [ 1, 2, 2, 1, 2, 6, 6, 1 ] )
gap> l:=LClass(s, f);
{Transformation( [ 1, 2, 2, 1, 2, 6, 6, 1 ] )}
gap> IsRegularClass(l);
true

#
gap> gens:=[ PartialPermNC( [ 1, 2, 3 ], [ 1, 4, 3 ] ),
>  PartialPermNC( [ 1, 2, 3 ], [ 2, 3, 4 ] ),
>  PartialPermNC( [ 1, 2, 3 ], [ 4, 2, 1 ] ),
>  PartialPermNC( [ 1, 2, 4 ], [ 1, 4, 3 ] ) ];;
gap> s:=Semigroup(gens);
<partial perm semigroup on 4 pts with 4 generators>
gap> List(LClasses(s), IsRegularClass);
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
{PartialPerm( [ 2, 3 ], [ 2, 4 ] )}
gap> HClassReps(l);
[ [3,4](2), [3,2,4], [1,2,4], [1,4](2) ]
gap> IsRegularClass(l);
false
gap> NrHClasses(l);
4
gap> l:=LClass(s, f);
{PartialPerm( [ 1, 2 ], [ 4, 2 ] )}
gap> HClassReps(l);
[ [1,4](2), [1,2,4], [3,4](2), [3,2,4] ]
gap> IsRegularClass(l);
false
gap> ForAll(HClassReps(l), x-> x in l);
true
gap> d:=DClassOfLClass(l);
{PartialPerm( [ 1, 2 ], [ 4, 2 ] )}
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
gap> IsRegularClass(l);
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
<partial perm semigroup on 8 pts with 10 generators>
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
gap> f:=PartialPerm([ 3, 4, 7 ], [ 4, 7, 8 ]);;
gap> d:=DClass(s, f);
{PartialPerm( [ 1, 3, 6 ], [ 7, 4, 8 ] )}
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
<stabilizer chain record, Base [ 8, 7 ], Orbit length 3, Size: 6>
gap> SemigroupDataSCC(d);
fail
gap> Position(DClasses(s), d);
18
gap> d:=DClasses(s)[18];
{PartialPerm( [ 1, 3, 6 ], [ 7, 4, 8 ] )}
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

#
gap> SemigroupsStopTest();

#
gap> Unbind(gens); Unbind(s); Unbind(f); Unbind(r); Unbind(l); Unbind(iter);
gap> STOP_TEST( "Semigroups package: everyfunction.tst", 0);
