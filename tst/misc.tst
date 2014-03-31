#############################################################################
###
##W  misc.tst
##Y  Copyright (C) 2011-13                                James D. Mitchell
###
###  Licensing information can be found in the README file of this package.
###
##############################################################################
###
gap> START_TEST("Semigroups package: misc.tst"); 
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
  {Transformation( [ 8, 8, 7, 5, 5, 7, 2, 8 ] )}, 
  {Transformation( [ 8, 2, 7, 2, 2, 5, 8, 7 ] )}, 
  {Transformation( [ 7, 8, 5, 7, 7, 7, 7, 2 ] )}, 
  {Transformation( [ 5, 7, 7, 7, 7, 2, 5 ] )}, 
  {Transformation( [ 7, 5, 2, 8, 5, 7, 7, 8 ] )}, 
  {Transformation( [ 7, 2, 5, 8, 5, 8, 7, 8 ] )}, 
  {Transformation( [ 2, 7, 7, 8, 8, 2, 2, 5 ] )}, 
  {Transformation( [ 7, 2, 8, 2, 2, 5, 7, 7 ] )}, 
  {Transformation( [ 8, 2, 5, 7, 7, 7, 8, 7 ] )}, 
  {Transformation( [ 7, 8, 7, 5, 5, 7, 7, 2 ] )}, 
  {Transformation( [ 7, 7, 5, 7, 7, 2, 7 ] )}, 
  {Transformation( [ 5, 8, 2, 7, 7, 5, 5, 7 ] )}, 
  {Transformation( [ 2, 5, 7, 5, 5, 7, 2 ] )}, 
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
<partial perm semigroup on 12 pts with 6 generators>
gap> Size(s);
4857
gap> f:=PartialPerm([ 6, 9 ], [ 12, 6 ]);;
gap> l:=LClass(s, f);
{PartialPerm( [ 1, 9 ], [ 6, 12 ] )}
gap> NrHClasses(l);
66
gap> Size(l);
66
gap> SchutzenbergerGroup(l);
Group([ (6,12) ])
gap> o:=RhoOrb(l);
<closed orbit, 147 points with Schreier tree with log>
gap> d:=DClassOfLClass(l);
{PartialPerm( [ 1, 9 ], [ 6, 12 ] )}
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
[ {PartialPerm( [ 1, 9 ], [ 6, 12 ] )}, {PartialPerm( [ 1, 9 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 2, 7 ], [ 6, 12 ] )}, {PartialPerm( [ 2, 7 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 4, 9 ], [ 6, 12 ] )}, {PartialPerm( [ 4, 9 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 1, 4 ], [ 12, 6 ] )}, {PartialPerm( [ 1, 4 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 6, 12 ] )}, {PartialPerm( [ 1, 3 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 1, 7 ], [ 12, 6 ] )}, {PartialPerm( [ 1, 7 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 4, 8 ], [ 12, 6 ] )}, {PartialPerm( [ 4, 8 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 7, 8 ], [ 6, 12 ] )}, {PartialPerm( [ 7, 8 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 3, 9 ], [ 12, 6 ] )}, {PartialPerm( [ 3, 9 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 12, 6 ] )}, {PartialPerm( [ 1, 2 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 2, 8 ], [ 6, 12 ] )}, {PartialPerm( [ 2, 8 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 3, 4 ], [ 6, 12 ] )}, {PartialPerm( [ 3, 4 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 2, 3 ], [ 12, 6 ] )}, {PartialPerm( [ 2, 3 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 12, 6 ] )}, {PartialPerm( [ 2, 4 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 1, 5 ], [ 12, 6 ] )}, {PartialPerm( [ 1, 5 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 9, 12 ], [ 12, 6 ] )}, {PartialPerm( [ 9, 12 ], [ 6, 12 ] )}
    , {PartialPerm( [ 2, 9 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 2, 9 ], [ 6, 12 ] )}, {PartialPerm( [ 4, 7 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 4, 7 ], [ 6, 12 ] )}, {PartialPerm( [ 7, 9 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 7, 9 ], [ 6, 12 ] )}, {PartialPerm( [ 1, 8 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 1, 8 ], [ 12, 6 ] )}, {PartialPerm( [ 5, 9 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 5, 9 ], [ 6, 12 ] )}, {PartialPerm( [ 8, 9 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 8, 9 ], [ 6, 12 ] )}, {PartialPerm( [ 4, 5 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 4, 5 ], [ 6, 12 ] )}, {PartialPerm( [ 3, 11 ], [ 12, 6 ] )},
  {PartialPerm( [ 3, 11 ], [ 6, 12 ] )}, {PartialPerm( [ 6, 9 ], [ 12, 6 ] )},
  {PartialPerm( [ 6, 9 ], [ 6, 12 ] )}, {PartialPerm( [ 5, 11 ], [ 12, 6 ] )},
  {PartialPerm( [ 5, 11 ], [ 6, 12 ] )}, {PartialPerm( [ 6, 8 ], [ 6, 12 ] )},
  {PartialPerm( [ 6, 8 ], [ 12, 6 ] )}, {PartialPerm( [ 4, 6 ], [ 12, 6 ] )}, 
  {PartialPerm( [ 4, 6 ], [ 6, 12 ] )}, {PartialPerm( [ 4, 12 ], [ 12, 6 ] )},
  {PartialPerm( [ 4, 12 ], [ 6, 12 ] )}, {PartialPerm( [ 2, 12 ], [ 12, 6 ] )}
    , {PartialPerm( [ 2, 12 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 4, 11 ], [ 12, 6 ] )}, {PartialPerm( [ 4, 11 ], [ 6, 12 ] )}
    , {PartialPerm( [ 7, 12 ], [ 6, 12 ] )}, 
  {PartialPerm( [ 7, 12 ], [ 12, 6 ] )}, {PartialPerm( [ 2, 5 ], [ 12, 6 ] )},
  {PartialPerm( [ 2, 5 ], [ 6, 12 ] )} ]
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
{PartialPerm( [ 1, 9 ], [ 6, 12 ] )}
gap> Representative(l) in d;
true
gap> First(H, x-> not Representative(x) in d);
fail
gap> ForAll(l, x-> x in d);
true
gap> rep:=Representative(d);
[1,6][9,12]
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
12
<closed orbit, 147 points with Schreier tree with log>
[ [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 6 ], [ 7 ], [ 8 ], [ 9 ], 
  [ 10, 42, 45, 48, 50, 51, 52, 53, 115, 134, 144 ], [ 11 ], 
  [ 12, 43, 46, 47, 54, 59, 62, 63, 65, 69, 70, 74, 81, 89, 93, 94, 98, 100, 
      102, 103, 112, 120, 121, 122, 123, 124, 125, 128, 132, 133, 138, 143, 
      145 ], [ 13, 61, 64, 108, 119 ], [ 14 ], [ 15 ], [ 16 ], [ 17 ], 
  [ 18 ], [ 19 ], [ 20 ], [ 21 ], [ 22 ], [ 23 ], [ 24 ], [ 25 ], [ 26 ], 
  [ 27 ], [ 28 ], [ 29 ], [ 30 ], [ 31 ], [ 32 ], [ 33 ], [ 34 ], 
  [ 35, 39, 111, 129, 147 ], [ 36 ], [ 37 ], [ 38 ], [ 40 ], [ 41 ], [ 44 ], 
  [ 49, 75, 84, 85, 113 ], [ 55 ], [ 56, 95, 135 ], [ 57 ], [ 58 ], [ 60 ], 
  [ 66 ], [ 67 ], [ 68 ], [ 71 ], [ 72 ], [ 73 ], [ 76 ], [ 77 ], [ 78 ], 
  [ 79 ], [ 80 ], [ 82 ], [ 83 ], [ 86 ], [ 87 ], [ 88 ], [ 90 ], [ 91 ], 
  [ 92 ], [ 96 ], [ 97 ], [ 99 ], [ 101 ], [ 104 ], [ 105 ], [ 106 ], 
  [ 107 ], [ 109 ], [ 110 ], [ 114 ], [ 116 ], [ 117 ], [ 118 ], [ 126 ], 
  [ 127 ], [ 130 ], [ 131 ], [ 136 ], [ 137 ], [ 139 ], [ 140 ], [ 141 ], 
  [ 142 ], [ 146 ] ]
gap> 
gap>   l:=Position(o, RhoFunc(s)(g));
123
gap>  l = fail or OrbSCCLookup(o)[l]<>m;
false
gap> g:=RhoOrbMult(o, m, l)[2]*g;;
gap>  schutz:=RhoOrbStabChain(d);
<stabilizer chain record, Base [ 6 ], Orbit length 2, Size: 2>
gap> l<>scc[m][1];
true
gap> cosets:=LambdaCosets(d);
<enumerator of perm group>
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
<closed orbit, 147 points with Schreier tree with log>
12
gap>   rho_schutz:=RhoOrbSchutzGp(o, m, infinity);
Group([ (11,12) ])
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
(6,7,8,9,10,11)
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
<enumerator of perm group>
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
[1,7][3,4][6,8]
gap> Length(enum);
282
gap> Size(d);
282
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
true
gap> s:=Semigroup(gens);
<partial perm semigroup on 8 pts with 10 generators>
gap> d:=DClasses(s)[18];
{PartialPerm( [ 1, 3, 6 ], [ 7, 4, 8 ] )}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
true
gap> enum[1];
[1,7][3,4][6,8]
gap> enum[2];
[1,4][3,8][6,7]
gap> Position(enum, enum[2]);
2
gap> Position(enum, enum[3]);
3
gap> enum[3];
[1,8][3,7][6,4]
gap> enum[4];
[1,8][3,4][6,7]
gap> for d in DClasses(s) do
> enum:=Enumerator(d);
> if not ForAll(enum, x-> enum[Position(enum, x)]=x) then 
> Print("problem with enumerator of a D-class 1\n");
> fi;
> od;
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

#
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
>   Transformation( [ 4, 2, 1, 5, 5 ] ),
>   Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> for d in DClasses(s) do
> enum:=Enumerator(d);
> if not ForAll(enum, x-> enum[Position(enum, x)]=x) then 
> Print("problem with enumerator of a D-class 1\n");
> fi;
> od;
gap> gens:=[ PartialPermNC( [ 1, 2, 3, 6, 8, 10 ], [ 2, 6, 7, 9, 1, 5 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5, 8, 10 ], [ 7, 1, 4, 3, 2, 6, 5 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 6, 7, 8, 10 ], [ 3, 8, 1, 9, 4, 10, 5, 6 ] ) ];;
gap> s:=Semigroup(gens);;
gap> f:=PartialPerm([ 2, 4 ], [ 6, 5 ]);;
gap> d:=DClassNC(s, f);
{PartialPerm( [ 2, 4 ], [ 6, 5 ] )}
gap> GreensHClasses(d);
[ {PartialPerm( [ 2, 4 ], [ 6, 5 ] )} ]
gap> Size(d);
1

#
gap> gens:=
> [ PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 19, 
>   20, 24, 25, 26, 27, 28, 29, 31, 32, 34, 35, 36, 37, 38, 40, 43, 45, 46, 49, 
>   50, 51, 53, 55, 56, 57, 58, 59, 60, 61, 64, 66, 68, 69, 70, 72, 73, 74, 77, 
>   80, 81, 83, 86, 87, 89, 91, 98 ], [ 89, 70, 79, 27, 84, 99, 9, 73, 33, 77, 
>   69, 41, 18, 63, 29, 42, 75, 56, 90, 64, 98, 49, 35, 100, 71, 3, 20, 2, 26, 
>   11, 39, 7, 48, 85, 8, 10, 61, 25, 55, 92, 62, 21, 34, 57, 44, 14, 53, 59, 
>   12, 87, 78, 83, 30, 32, 68, 86, 23, 47, 93, 15, 76, 97, 91 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 
>   19, 20, 22, 23, 24, 25, 28, 30, 31, 33, 34, 35, 36, 39, 40, 42, 43, 44, 45, 
>   46, 47, 50, 53, 54, 55, 58, 59, 64, 65, 67, 69, 70, 71, 72, 73, 76, 77, 78, 
>   81, 82, 84, 85, 86, 87, 89, 92, 94, 95 ], [ 5, 13, 94, 44, 80, 54, 99, 81, 
>   31, 7, 90, 30, 46, 68, 36, 11, 100, 17, 87, 72, 14, 29, 9, 61, 91, 32, 43, 
>   64, 60, 41, 26, 40, 8, 23, 63, 38, 57, 12, 59, 83, 92, 96, 18, 3, 65, 2, 
>   37, 21, 49, 16, 75, 24, 27, 1, 48, 6, 35, 79, 82, 51, 39, 25, 77, 62, 22 
>  ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
>   18, 19, 20, 21, 23, 24, 26, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 
>   40, 42, 44, 48, 51, 52, 53, 55, 56, 57, 58, 60, 63, 64, 65, 66, 67, 71, 73, 
>   75, 77, 80, 82, 83, 85, 86, 90, 91, 96, 97, 98, 99 ], 
> [ 67, 93, 18, 59, 86, 16, 99, 73, 60, 74, 17, 95, 85, 49, 79, 4, 33, 66, 15, 
>   44, 77, 41, 55, 84, 68, 69, 94, 31, 2, 29, 5, 42, 10, 63, 58, 34, 72, 53, 
>   89, 57, 62, 76, 20, 52, 22, 35, 75, 98, 78, 40, 46, 28, 6, 90, 12, 65, 26, 
>   36, 25, 61, 83, 38, 39, 87, 92, 97, 43, 30 ] ) ];;
gap> s:=Semigroup(gens);;
gap> f:=PartialPerm([ 12, 27, 37, 40, 46, 50, 51, 53 ],
> [ 98, 3, 84, 99, 100, 21, 70, 89 ]);;
gap> d:=DClassNC(s, f);
{PartialPerm( [ 12, 27, 37, 40, 46, 50, 51, 53 ], 
 [ 98, 3, 84, 99, 100, 21, 70, 89 ] )}
gap> Size(d);
1
gap> GreensHClasses(d);
[ {PartialPerm( [ 12, 27, 37, 40, 46, 50, 51, 53 ], 
     [ 98, 3, 84, 99, 100, 21, 70, 89 ] )} ]
gap> iter:=IteratorOfDClasses(s);
<iterator of D-classes>
gap> repeat d:=NextIterator(iter); until Size(d)>1;
gap> d;
{PartialPerm( [ 8, 57 ], [ 63, 87 ] )}
gap> Size(d);
2036
gap> IsRegularDClass(d);
false
gap> GreensHClasses(d);;
gap> NrHClasses(d);
2036
gap> GreensLClasses(d);
[ {PartialPerm( [ 8, 57 ], [ 63, 87 ] )} ]

#
gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ),
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ),
> Transformation( [ 3, 3, 4, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> List(GreensDClasses(s), LClasses);
[ [ {IdentityTransformation} ], 
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
[ {IdentityTransformation}, {Transformation( [ 1, 3, 4, 1 ] )}, 
  {Transformation( [ 4, 1, 3, 4 ] )}, {Transformation( [ 3, 4, 1, 3 ] )}, 
  {Transformation( [ 2, 4, 1, 2 ] )}, {Transformation( [ 3, 1, 1, 3 ] )}, 
  {Transformation( [ 1, 4, 4, 1 ] )}, {Transformation( [ 2, 1, 1, 2 ] )}, 
  {Transformation( [ 2, 4, 4, 2 ] )}, {Transformation( [ 4, 3, 3, 4 ] )}, 
  {Transformation( [ 3, 3, 4, 1 ] )}, {Transformation( [ 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 2, 2, 2 ] )}, {Transformation( [ 3, 3, 3, 3 ] )}, 
  {Transformation( [ 4, 4, 4, 4 ] )} ]
gap> List(last, Elements);
[ [ IdentityTransformation ], [ Transformation( [ 1, 3, 4, 1 ] ) ], 
  [ Transformation( [ 4, 1, 3, 4 ] ) ], [ Transformation( [ 3, 4, 1, 3 ] ) ], 
  [ Transformation( [ 2, 4, 1, 2 ] ) ], 
  [ Transformation( [ 1, 1, 1, 3 ] ), Transformation( [ 1, 1, 3, 1 ] ), 
      Transformation( [ 1, 1, 3, 3 ] ), Transformation( [ 1, 3, 1, 1 ] ), 
      Transformation( [ 1, 3, 3, 1 ] ), Transformation( [ 3, 1, 1, 3 ] ), 
      Transformation( [ 3, 1, 3, 3 ] ), Transformation( [ 3, 3, 1, 1 ] ), 
      Transformation( [ 3, 3, 1, 3 ] ), Transformation( [ 3, 3, 3, 1 ] ) ], 
  [ Transformation( [ 1, 1, 1 ] ), Transformation( [ 1, 1, 4, 1 ] ), 
      Transformation( [ 1, 1, 4, 4 ] ), Transformation( [ 1, 4, 1, 1 ] ), 
      Transformation( [ 1, 4, 4, 1 ] ), Transformation( [ 4, 1, 1, 4 ] ), 
      Transformation( [ 4, 1, 4, 4 ] ), Transformation( [ 4, 4, 1, 1 ] ), 
      Transformation( [ 4, 4, 1, 4 ] ), Transformation( [ 4, 4, 4, 1 ] ) ], 
  [ Transformation( [ 1, 1, 1, 2 ] ), Transformation( [ 1, 1, 2, 1 ] ), 
      Transformation( [ 1, 1, 2, 2 ] ), Transformation( [ 1, 2, 1, 1 ] ), 
      Transformation( [ 1, 2, 2, 1 ] ), Transformation( [ 2, 1, 1, 2 ] ), 
      Transformation( [ 2, 1, 2, 2 ] ), Transformation( [ 2, 2, 1, 1 ] ), 
      Transformation( [ 2, 2, 1, 2 ] ), Transformation( [ 2, 2, 2, 1 ] ) ], 
  [ Transformation( [ 2, 2, 2 ] ), Transformation( [ 2, 2, 4, 2 ] ), 
      Transformation( [ 2, 2, 4, 4 ] ), Transformation( [ 2, 4, 2, 2 ] ), 
      Transformation( [ 2, 4, 4, 2 ] ), Transformation( [ 4, 2, 2, 4 ] ), 
      Transformation( [ 4, 2, 4, 4 ] ), Transformation( [ 4, 4, 2, 2 ] ), 
      Transformation( [ 4, 4, 2, 4 ] ), Transformation( [ 4, 4, 4, 2 ] ) ], 
  [ Transformation( [ 3, 3, 3 ] ), Transformation( [ 3, 3, 4, 3 ] ), 
      Transformation( [ 3, 3, 4, 4 ] ), Transformation( [ 3, 4, 3, 3 ] ), 
      Transformation( [ 3, 4, 4, 3 ] ), Transformation( [ 4, 3, 3, 4 ] ), 
      Transformation( [ 4, 3, 4, 4 ] ), Transformation( [ 4, 4, 3, 3 ] ), 
      Transformation( [ 4, 4, 3, 4 ] ), Transformation( [ 4, 4, 4, 3 ] ) ], 
  [ Transformation( [ 1, 1 ] ), Transformation( [ 3, 3, 4, 1 ] ), 
      Transformation( [ 4, 4, 1, 3 ] ) ], [ Transformation( [ 1, 1, 1, 1 ] ) ]
    , [ Transformation( [ 2, 2, 2, 2 ] ) ], 
  [ Transformation( [ 3, 3, 3, 3 ] ) ], [ Transformation( [ 4, 4, 4, 4 ] ) ] ]
gap> Union(last);
[ Transformation( [ 1, 1, 1, 1 ] ), Transformation( [ 1, 1, 1, 2 ] ), 
  Transformation( [ 1, 1, 1, 3 ] ), Transformation( [ 1, 1, 1 ] ), 
  Transformation( [ 1, 1, 2, 1 ] ), Transformation( [ 1, 1, 2, 2 ] ), 
  Transformation( [ 1, 1, 3, 1 ] ), Transformation( [ 1, 1, 3, 3 ] ), 
  Transformation( [ 1, 1 ] ), Transformation( [ 1, 1, 4, 1 ] ), 
  Transformation( [ 1, 1, 4, 4 ] ), Transformation( [ 1, 2, 1, 1 ] ), 
  Transformation( [ 1, 2, 2, 1 ] ), IdentityTransformation, 
  Transformation( [ 1, 3, 1, 1 ] ), Transformation( [ 1, 3, 3, 1 ] ), 
  Transformation( [ 1, 3, 4, 1 ] ), Transformation( [ 1, 4, 1, 1 ] ), 
  Transformation( [ 1, 4, 4, 1 ] ), Transformation( [ 2, 1, 1, 2 ] ), 
  Transformation( [ 2, 1, 2, 2 ] ), Transformation( [ 2, 2, 1, 1 ] ), 
  Transformation( [ 2, 2, 1, 2 ] ), Transformation( [ 2, 2, 2, 1 ] ), 
  Transformation( [ 2, 2, 2, 2 ] ), Transformation( [ 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 4, 2 ] ), Transformation( [ 2, 2, 4, 4 ] ), 
  Transformation( [ 2, 4, 1, 2 ] ), Transformation( [ 2, 4, 2, 2 ] ), 
  Transformation( [ 2, 4, 4, 2 ] ), Transformation( [ 3, 1, 1, 3 ] ), 
  Transformation( [ 3, 1, 3, 3 ] ), Transformation( [ 3, 3, 1, 1 ] ), 
  Transformation( [ 3, 3, 1, 3 ] ), Transformation( [ 3, 3, 3, 1 ] ), 
  Transformation( [ 3, 3, 3, 3 ] ), Transformation( [ 3, 3, 3 ] ), 
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

#
gap> gens:= [ PartialPermNC( [ 1, 2, 3, 4 ], [ 5, 7, 1, 6 ] ),
> PartialPermNC( [ 1, 2, 3, 5 ], [ 5, 2, 7, 3 ] ),
> PartialPermNC( [ 1, 2, 3, 6, 7 ], [ 1, 3, 4, 7, 5 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5, 7 ], [ 3, 2, 4, 6, 1, 5 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
840
gap> NrDClasses(s);
176

#
gap> gens:=[ PartialPermNC( [ 1, 2, 3, 4 ], [ 5, 7, 1, 6 ] ),
> PartialPermNC( [ 1, 2, 3, 5 ], [ 5, 2, 7, 3 ] ),
> PartialPermNC( [ 1, 2, 3, 6, 7 ], [ 1, 3, 4, 7, 5 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5, 7 ], [ 3, 2, 4, 6, 1, 5 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
840
gap> NrDClasses(s);
176
gap> List(DClasses(s), RClasses);
[ [ {PartialPerm( [ 1, 2, 3, 4 ], [ 5, 7, 1, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3, 5 ], [ 5, 2, 7, 3 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3, 6, 7 ], [ 1, 3, 4, 7, 5 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3, 4, 5, 7 ], [ 3, 2, 4, 6, 1, 5 ] )} ], 
  [ {PartialPerm( [ 3 ], [ 5 ] )}, {PartialPerm( [ 5 ], [ 5 ] )}, 
      {PartialPerm( [ 1 ], [ 5 ] )}, {PartialPerm( [ 7 ], [ 5 ] )}, 
      {PartialPerm( [ 6 ], [ 5 ] )}, {PartialPerm( [ 4 ], [ 5 ] )} ], 
  [ {PartialPerm( [ 2, 5 ], [ 5, 3 ] )}, {PartialPerm( [ 1, 2 ], [ 3, 5 ] )}, 
      {PartialPerm( [ 2, 7 ], [ 5, 3 ] )}, 
      {PartialPerm( [ 2, 3 ], [ 5, 3 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 5, 1, 6 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 6, 1, 5 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 6, 1, 5 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 5, 1, 6 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 1, 6, 5 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 1, 5, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3, 5 ], [ 1, 7, 6, 5 ] )} ], 
  [ {PartialPerm( [ 1, 3 ], [ 3, 5 ] )}, {PartialPerm( [ 1, 5 ], [ 5, 3 ] )}, 
      {PartialPerm( [ 1, 7 ], [ 5, 3 ] )}, {PartialPerm( [ 1, 6 ], [ 5, 3 ] )}
        , {PartialPerm( [ 3, 4 ], [ 5, 3 ] )}, 
      {PartialPerm( [ 5, 7 ], [ 5, 3 ] )}, {PartialPerm( [ 4, 5 ], [ 3, 5 ] )}
        , {PartialPerm( [ 3, 7 ], [ 3, 5 ] )}, 
      {PartialPerm( [ 3, 5 ], [ 5, 3 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 7, 2, 3 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 3, 2, 7 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 3, 2, 7 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 7, 2, 3 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 3, 7 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 7, 3 ] )} ], 
  [ {PartialPerm( [ 1, 2, 7 ], [ 5, 7, 3 ] )} ], 
  [ {PartialPerm( [ 1, 2, 5, 7 ], [ 7, 2, 5, 3 ] )} ], 
  [ {PartialPerm( [ 2, 3, 4 ], [ 5, 1, 7 ] )} ], 
  [ {PartialPerm( [ 2, 3, 5 ], [ 3, 5, 4 ] )} ], 
  [ {PartialPerm( [ 1, 2, 6 ], [ 1, 4, 5 ] )} ], 
  [ {PartialPerm( [ 1, 2, 4, 5 ], [ 4, 3, 7, 1 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 1, 5, 3 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 3, 5, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 3, 5, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 1, 5, 3 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 5, 3, 1 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 5, 1, 3 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3, 5 ], [ 1, 2, 5, 4 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3, 6, 7 ], [ 3, 4, 6, 5, 1 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3, 5, 7 ], [ 4, 2, 6, 3, 1 ] )} ], 
  [ {PartialPerm( [  ], [  ] )} ], [ {PartialPerm( [ 2 ], [ 5 ] )} ], 
  [ {PartialPerm( [ 2, 5 ], [ 6, 5 ] )}, {PartialPerm( [ 1, 2 ], [ 5, 6 ] )}, 
      {PartialPerm( [ 2, 7 ], [ 6, 5 ] )}, 
      {PartialPerm( [ 2, 3 ], [ 6, 5 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 6, 7, 5 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 5, 7, 6 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 5, 7, 6 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 6, 7, 5 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 7, 5, 6 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 7, 6, 5 ] )} ], 
  [ {PartialPerm( [ 1, 2, 7 ], [ 1, 6, 5 ] )} ], 
  [ {PartialPerm( [ 1, 2, 5, 7 ], [ 6, 7, 1, 5 ] )} ], 
  [ {PartialPerm( [ 2, 5 ], [ 2, 7 ] )}, {PartialPerm( [ 1, 2 ], [ 7, 2 ] )}, 
      {PartialPerm( [ 2, 7 ], [ 2, 7 ] )}, 
      {PartialPerm( [ 2, 3 ], [ 2, 7 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 5, 3, 7 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 7, 3, 5 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 7, 3, 5 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 5, 3, 7 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 3, 7, 5 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 3, 5, 7 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 5, 2, 3 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 3, 2, 5 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 3, 2, 5 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 5, 2, 3 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 3, 5 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 5, 3 ] )} ], 
  [ {PartialPerm( [ 1, 6, 7 ], [ 7, 3, 5 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 5, 2, 7 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 7, 2, 5 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 7, 5 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 1, 5, 7 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 7, 5, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 7, 5, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 1, 5, 7 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 5, 7, 1 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 5, 1, 7 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 5, 3, 4 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 4, 3, 5 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 4, 3, 5 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 5, 3, 4 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 3, 4, 5 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 3, 5, 4 ] )} ], 
  [ {PartialPerm( [ 1, 2, 7 ], [ 5, 3, 4 ] )} ], 
  [ {PartialPerm( [ 2, 4, 5 ], [ 4, 5, 1 ] )} ], 
  [ {PartialPerm( [ 1, 3 ], [ 1, 4 ] )}, {PartialPerm( [ 1, 3 ], [ 4, 1 ] )}, 
      {PartialPerm( [ 1, 5 ], [ 4, 1 ] )}, {PartialPerm( [ 1, 5 ], [ 1, 4 ] )}
        , {PartialPerm( [ 1, 7 ], [ 4, 1 ] )}, 
      {PartialPerm( [ 1, 7 ], [ 1, 4 ] )}, {PartialPerm( [ 1, 6 ], [ 4, 1 ] )}
        , {PartialPerm( [ 1, 6 ], [ 1, 4 ] )}, 
      {PartialPerm( [ 3, 4 ], [ 4, 1 ] )}, {PartialPerm( [ 3, 4 ], [ 1, 4 ] )}
        , {PartialPerm( [ 5, 7 ], [ 4, 1 ] )}, 
      {PartialPerm( [ 5, 7 ], [ 1, 4 ] )}, {PartialPerm( [ 4, 5 ], [ 1, 4 ] )}
        , {PartialPerm( [ 4, 5 ], [ 4, 1 ] )}, 
      {PartialPerm( [ 3, 7 ], [ 1, 4 ] )}, {PartialPerm( [ 3, 7 ], [ 4, 1 ] )}
        , {PartialPerm( [ 3, 5 ], [ 4, 1 ] )}, 
      {PartialPerm( [ 3, 5 ], [ 1, 4 ] )} ], 
  [ {PartialPerm( [ 1, 3, 7 ], [ 4, 7, 1 ] )} ], 
  [ {PartialPerm( [ 2, 3, 5, 7 ], [ 3, 7, 4, 1 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 5, 2, 4 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 4, 2, 5 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 4, 2, 5 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 5, 2, 4 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 4, 5 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 5, 4 ] )} ], 
  [ {PartialPerm( [ 1, 2, 7 ], [ 1, 5, 4 ] )} ], 
  [ {PartialPerm( [ 1, 2, 5, 7 ], [ 5, 2, 1, 4 ] )} ], 
  [ {PartialPerm( [ 2, 3, 4 ], [ 1, 3, 5 ] )} ], 
  [ {PartialPerm( [ 2, 3, 5 ], [ 4, 1, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 6 ], [ 3, 6, 1 ] )} ], 
  [ {PartialPerm( [ 1, 2, 4, 5 ], [ 6, 4, 5, 3 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 3, 1, 4 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 4, 1, 3 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 4, 1, 3 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 3, 1, 4 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 1, 4, 3 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 1, 3, 4 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3, 5 ], [ 3, 2, 1, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 6, 7 ], [ 4, 6, 1, 3 ] )} ], 
  [ {PartialPerm( [ 1, 2, 5, 7 ], [ 6, 2, 4, 3 ] )} ], 
  [ {PartialPerm( [ 1, 3 ], [ 5, 6 ] )}, {PartialPerm( [ 1, 3 ], [ 6, 5 ] )}, 
      {PartialPerm( [ 1, 5 ], [ 6, 5 ] )}, {PartialPerm( [ 1, 5 ], [ 5, 6 ] )}
        , {PartialPerm( [ 1, 7 ], [ 6, 5 ] )}, 
      {PartialPerm( [ 1, 7 ], [ 5, 6 ] )}, {PartialPerm( [ 1, 6 ], [ 6, 5 ] )}
        , {PartialPerm( [ 1, 6 ], [ 5, 6 ] )}, 
      {PartialPerm( [ 3, 4 ], [ 6, 5 ] )}, {PartialPerm( [ 3, 4 ], [ 5, 6 ] )}
        , {PartialPerm( [ 5, 7 ], [ 6, 5 ] )}, 
      {PartialPerm( [ 5, 7 ], [ 5, 6 ] )}, {PartialPerm( [ 4, 5 ], [ 5, 6 ] )}
        , {PartialPerm( [ 4, 5 ], [ 6, 5 ] )}, 
      {PartialPerm( [ 3, 7 ], [ 5, 6 ] )}, {PartialPerm( [ 3, 7 ], [ 6, 5 ] )}
        , {PartialPerm( [ 3, 5 ], [ 6, 5 ] )}, 
      {PartialPerm( [ 3, 5 ], [ 5, 6 ] )} ], 
  [ {PartialPerm( [ 2, 5 ], [ 7, 6 ] )}, {PartialPerm( [ 1, 2 ], [ 6, 7 ] )}, 
      {PartialPerm( [ 2, 7 ], [ 7, 6 ] )}, 
      {PartialPerm( [ 2, 3 ], [ 7, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 1, 5, 6 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 6, 5, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 6, 5, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 1, 5, 6 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 5, 6, 1 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 5, 1, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 1, 7, 5 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 5, 7, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 5, 7, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 1, 7, 5 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 7, 5, 1 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 7, 1, 5 ] )} ], 
  [ {PartialPerm( [ 1, 6, 7 ], [ 6, 5, 1 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 1, 7, 6 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 6, 7, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 6, 7, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 1, 7, 6 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 7, 6, 1 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 7, 1, 6 ] )} ], 
  [ {PartialPerm( [ 2 ], [ 2 ] )} ], [ {PartialPerm( [ 6, 7 ], [ 3, 5 ] )} ], 
  [ {PartialPerm( [ 2, 3, 4 ], [ 5, 7, 3 ] )} ], 
  [ {PartialPerm( [ 2, 5 ], [ 4, 1 ] )}, {PartialPerm( [ 1, 2 ], [ 1, 4 ] )}, 
      {PartialPerm( [ 2, 7 ], [ 4, 1 ] )}, 
      {PartialPerm( [ 2, 3 ], [ 4, 1 ] )} ], 
  [ {PartialPerm( [ 2, 3, 7 ], [ 4, 5, 1 ] )} ], 
  [ {PartialPerm( [ 2, 5 ], [ 1, 4 ] )}, {PartialPerm( [ 1, 2 ], [ 4, 1 ] )}, 
      {PartialPerm( [ 2, 7 ], [ 1, 4 ] )}, 
      {PartialPerm( [ 2, 3 ], [ 1, 4 ] )} ], 
  [ {PartialPerm( [ 1, 2, 6 ], [ 4, 7, 1 ] )} ], 
  [ {PartialPerm( [ 1, 3 ], [ 4, 7 ] )}, {PartialPerm( [ 1, 3 ], [ 7, 4 ] )}, 
      {PartialPerm( [ 1, 5 ], [ 7, 4 ] )}, {PartialPerm( [ 1, 5 ], [ 4, 7 ] )}
        , {PartialPerm( [ 1, 7 ], [ 7, 4 ] )}, 
      {PartialPerm( [ 1, 7 ], [ 4, 7 ] )}, {PartialPerm( [ 1, 6 ], [ 7, 4 ] )}
        , {PartialPerm( [ 1, 6 ], [ 4, 7 ] )}, 
      {PartialPerm( [ 3, 4 ], [ 7, 4 ] )}, {PartialPerm( [ 3, 4 ], [ 4, 7 ] )}
        , {PartialPerm( [ 5, 7 ], [ 7, 4 ] )}, 
      {PartialPerm( [ 5, 7 ], [ 4, 7 ] )}, {PartialPerm( [ 4, 5 ], [ 4, 7 ] )}
        , {PartialPerm( [ 4, 5 ], [ 7, 4 ] )}, 
      {PartialPerm( [ 3, 7 ], [ 4, 7 ] )}, {PartialPerm( [ 3, 7 ], [ 7, 4 ] )}
        , {PartialPerm( [ 3, 5 ], [ 7, 4 ] )}, 
      {PartialPerm( [ 3, 5 ], [ 4, 7 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3, 5 ], [ 4, 3, 1, 7 ] )} ], 
  [ {PartialPerm( [ 2, 6, 7 ], [ 7, 1, 4 ] )} ], 
  [ {PartialPerm( [ 1, 2, 7 ], [ 7, 3, 4 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 1, 4, 5 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 5, 4, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 5, 4, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 1, 4, 5 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 4, 5, 1 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 4, 1, 5 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 1, 2, 4 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 4, 2, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 4, 2, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 1, 2, 4 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 4, 1 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 1, 4 ] )} ], 
  [ {PartialPerm( [ 1, 6, 7 ], [ 5, 4, 1 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 3, 1, 5 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 5, 1, 3 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 5, 1, 3 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 3, 1, 5 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 1, 5, 3 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 1, 3, 5 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 1, 4, 6 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 6, 4, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 6, 4, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 1, 4, 6 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 4, 6, 1 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 4, 1, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 7 ], [ 1, 4, 6 ] )} ], 
  [ {PartialPerm( [ 2, 4, 5 ], [ 6, 1, 3 ] )} ], 
  [ {PartialPerm( [ 1, 3 ], [ 3, 6 ] )}, {PartialPerm( [ 1, 3 ], [ 6, 3 ] )}, 
      {PartialPerm( [ 1, 5 ], [ 6, 3 ] )}, {PartialPerm( [ 1, 5 ], [ 3, 6 ] )}
        , {PartialPerm( [ 1, 7 ], [ 6, 3 ] )}, 
      {PartialPerm( [ 1, 7 ], [ 3, 6 ] )}, {PartialPerm( [ 1, 6 ], [ 6, 3 ] )}
        , {PartialPerm( [ 1, 6 ], [ 3, 6 ] )}, 
      {PartialPerm( [ 3, 4 ], [ 6, 3 ] )}, {PartialPerm( [ 3, 4 ], [ 3, 6 ] )}
        , {PartialPerm( [ 5, 7 ], [ 6, 3 ] )}, 
      {PartialPerm( [ 5, 7 ], [ 3, 6 ] )}, {PartialPerm( [ 4, 5 ], [ 3, 6 ] )}
        , {PartialPerm( [ 4, 5 ], [ 6, 3 ] )}, 
      {PartialPerm( [ 3, 7 ], [ 3, 6 ] )}, {PartialPerm( [ 3, 7 ], [ 6, 3 ] )}
        , {PartialPerm( [ 3, 5 ], [ 6, 3 ] )}, 
      {PartialPerm( [ 3, 5 ], [ 3, 6 ] )} ], 
  [ {PartialPerm( [ 1, 3, 7 ], [ 6, 5, 3 ] )} ], 
  [ {PartialPerm( [ 2, 3, 5, 7 ], [ 4, 5, 6, 3 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 1, 2, 6 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 6, 2, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 6, 2, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 1, 2, 6 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 6, 1 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 1, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 7 ], [ 3, 1, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 5, 7 ], [ 1, 2, 3, 6 ] )} ], 
  [ {PartialPerm( [ 2, 3, 4 ], [ 3, 4, 1 ] )} ], 
  [ {PartialPerm( [ 2, 5 ], [ 6, 3 ] )}, {PartialPerm( [ 1, 2 ], [ 3, 6 ] )}, 
      {PartialPerm( [ 2, 7 ], [ 6, 3 ] )}, 
      {PartialPerm( [ 2, 3 ], [ 6, 3 ] )} ], 
  [ {PartialPerm( [ 2, 4, 5 ], [ 6, 1, 4 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 4, 3, 6 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 6, 3, 4 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 6, 3, 4 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 4, 3, 6 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 3, 6, 4 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 3, 4, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 4, 2, 3 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 3, 2, 4 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 3, 2, 4 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 4, 2, 3 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 3, 4 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 4, 3 ] )} ], 
  [ {PartialPerm( [ 1, 6, 7 ], [ 6, 3, 4 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 4, 2, 6 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 6, 2, 4 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 6, 2, 4 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 4, 2, 6 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 6, 4 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 4, 6 ] )} ], 
  [ {PartialPerm( [ 2, 5 ], [ 5, 6 ] )}, {PartialPerm( [ 1, 2 ], [ 6, 5 ] )}, 
      {PartialPerm( [ 2, 7 ], [ 5, 6 ] )}, 
      {PartialPerm( [ 2, 3 ], [ 5, 6 ] )} ], 
  [ {PartialPerm( [ 6, 7 ], [ 5, 6 ] )} ], 
  [ {PartialPerm( [ 6, 7 ], [ 6, 5 ] )} ], 
  [ {PartialPerm( [ 2, 3, 4 ], [ 1, 6, 5 ] )} ], 
  [ {PartialPerm( [ 2, 4 ], [ 5, 3 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 7, 5, 3 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 3, 5, 7 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 3, 5, 7 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 7, 5, 3 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 5, 3, 7 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 5, 7, 3 ] )} ], 
  [ {PartialPerm( [ 2, 6 ], [ 3, 5 ] )} ], 
  [ {PartialPerm( [ 2, 3, 5 ], [ 4, 1, 5 ] )} ], 
  [ {PartialPerm( [ 2, 4, 5 ], [ 7, 1, 4 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 1, 3, 7 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 7, 3, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 7, 3, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 1, 3, 7 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 3, 7, 1 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 3, 1, 7 ] )} ], 
  [ {PartialPerm( [ 1, 2, 7 ], [ 4, 1, 7 ] )} ], 
  [ {PartialPerm( [ 1, 2, 5, 7 ], [ 1, 3, 4, 7 ] )} ], 
  [ {PartialPerm( [ 2, 4 ], [ 4, 1 ] )} ], 
  [ {PartialPerm( [ 2, 5 ], [ 7, 4 ] )}, {PartialPerm( [ 1, 2 ], [ 4, 7 ] )}, 
      {PartialPerm( [ 2, 7 ], [ 7, 4 ] )}, 
      {PartialPerm( [ 2, 3 ], [ 7, 4 ] )} ], 
  [ {PartialPerm( [ 2, 5 ], [ 4, 7 ] )}, {PartialPerm( [ 1, 2 ], [ 7, 4 ] )}, 
      {PartialPerm( [ 2, 7 ], [ 4, 7 ] )}, 
      {PartialPerm( [ 2, 3 ], [ 4, 7 ] )} ], 
  [ {PartialPerm( [ 2, 5 ], [ 2, 4 ] )}, {PartialPerm( [ 1, 2 ], [ 4, 2 ] )}, 
      {PartialPerm( [ 2, 7 ], [ 2, 4 ] )}, 
      {PartialPerm( [ 2, 3 ], [ 2, 4 ] )} ], 
  [ {PartialPerm( [ 2, 3, 4 ], [ 1, 5, 4 ] )} ], 
  [ {PartialPerm( [ 2, 5 ], [ 4, 6 ] )}, {PartialPerm( [ 1, 2 ], [ 6, 4 ] )}, 
      {PartialPerm( [ 2, 7 ], [ 4, 6 ] )}, 
      {PartialPerm( [ 2, 3 ], [ 4, 6 ] )} ], 
  [ {PartialPerm( [ 2, 3, 7 ], [ 6, 1, 3 ] )} ], 
  [ {PartialPerm( [ 2, 5 ], [ 3, 6 ] )}, {PartialPerm( [ 1, 2 ], [ 6, 3 ] )}, 
      {PartialPerm( [ 2, 7 ], [ 3, 6 ] )}, 
      {PartialPerm( [ 2, 3 ], [ 3, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 6 ], [ 6, 5, 3 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3, 5 ], [ 6, 4, 3, 5 ] )} ], 
  [ {PartialPerm( [ 2, 6, 7 ], [ 5, 3, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 7 ], [ 5, 4, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 3, 6, 1 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 1, 6, 3 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 1, 6, 3 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 3, 6, 1 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 6, 1, 3 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 6, 3, 1 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 3, 2, 6 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 6, 2, 3 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 6, 2, 3 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 3, 2, 6 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 6, 3 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 2, 3, 6 ] )} ], 
  [ {PartialPerm( [ 1, 6, 7 ], [ 1, 6, 3 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 4, 3, 1 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 1, 3, 4 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 1, 3, 4 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 4, 3, 1 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 3, 1, 4 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 3, 4, 1 ] )} ], 
  [ {PartialPerm( [ 2, 5 ], [ 6, 4 ] )}, {PartialPerm( [ 1, 2 ], [ 4, 6 ] )}, 
      {PartialPerm( [ 2, 7 ], [ 6, 4 ] )}, 
      {PartialPerm( [ 2, 3 ], [ 6, 4 ] )} ], 
  [ {PartialPerm( [ 2, 3, 7 ], [ 6, 1, 4 ] )} ], 
  [ {PartialPerm( [ 2, 3, 4 ], [ 4, 6, 3 ] )} ], 
  [ {PartialPerm( [ 1, 3 ], [ 4, 6 ] )}, {PartialPerm( [ 1, 3 ], [ 6, 4 ] )}, 
      {PartialPerm( [ 1, 5 ], [ 6, 4 ] )}, {PartialPerm( [ 1, 5 ], [ 4, 6 ] )}
        , {PartialPerm( [ 1, 7 ], [ 6, 4 ] )}, 
      {PartialPerm( [ 1, 7 ], [ 4, 6 ] )}, {PartialPerm( [ 1, 6 ], [ 6, 4 ] )}
        , {PartialPerm( [ 1, 6 ], [ 4, 6 ] )}, 
      {PartialPerm( [ 3, 4 ], [ 6, 4 ] )}, {PartialPerm( [ 3, 4 ], [ 4, 6 ] )}
        , {PartialPerm( [ 5, 7 ], [ 6, 4 ] )}, 
      {PartialPerm( [ 5, 7 ], [ 4, 6 ] )}, {PartialPerm( [ 4, 5 ], [ 4, 6 ] )}
        , {PartialPerm( [ 4, 5 ], [ 6, 4 ] )}, 
      {PartialPerm( [ 3, 7 ], [ 4, 6 ] )}, {PartialPerm( [ 3, 7 ], [ 6, 4 ] )}
        , {PartialPerm( [ 3, 5 ], [ 6, 4 ] )}, 
      {PartialPerm( [ 3, 5 ], [ 4, 6 ] )} ], 
  [ {PartialPerm( [ 6, 7 ], [ 4, 6 ] )} ], 
  [ {PartialPerm( [ 2, 5 ], [ 2, 6 ] )}, {PartialPerm( [ 1, 2 ], [ 6, 2 ] )}, 
      {PartialPerm( [ 2, 7 ], [ 2, 6 ] )}, 
      {PartialPerm( [ 2, 3 ], [ 2, 6 ] )} ], 
  [ {PartialPerm( [ 2, 4 ], [ 6, 5 ] )} ], 
  [ {PartialPerm( [ 2, 4 ], [ 5, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 7 ], [ 1, 4, 5 ] )} ], 
  [ {PartialPerm( [ 6, 7 ], [ 1, 4 ] )} ], 
  [ {PartialPerm( [ 2, 3, 7 ], [ 7, 1, 4 ] )} ], 
  [ {PartialPerm( [ 6, 7 ], [ 4, 7 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 4, 7, 1 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 1, 7, 4 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 1, 7, 4 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 4, 7, 1 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 7, 1, 4 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 7, 4, 1 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 4, 3, 7 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 7, 3, 4 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 7, 3, 4 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 4, 3, 7 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 3, 7, 4 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 3, 4, 7 ] )} ], 
  [ {PartialPerm( [ 1, 6, 7 ], [ 1, 7, 4 ] )} ], 
  [ {PartialPerm( [ 6, 7 ], [ 4, 1 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 5, 1, 4 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 4, 1, 5 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 4, 1, 5 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 5, 1, 4 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 1, 4, 5 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 1, 5, 4 ] )} ], 
  [ {PartialPerm( [ 2, 3, 5 ], [ 6, 3, 1 ] )} ], 
  [ {PartialPerm( [ 2, 4, 5 ], [ 5, 3, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 3, 4, 5 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 5, 4, 3 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 5, 4, 3 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 3, 4, 5 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 4, 5, 3 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 4, 3, 5 ] )} ], 
  [ {PartialPerm( [ 1, 2, 7 ], [ 6, 3, 5 ] )} ], 
  [ {PartialPerm( [ 1, 2, 5, 7 ], [ 3, 4, 6, 5 ] )} ], 
  [ {PartialPerm( [ 2, 4 ], [ 6, 3 ] )} ], 
  [ {PartialPerm( [ 2, 3, 4 ], [ 3, 1, 6 ] )} ], 
  [ {PartialPerm( [ 2, 6 ], [ 1, 4 ] )} ], 
  [ {PartialPerm( [ 2, 3, 5 ], [ 6, 4, 1 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 6, 4, 3 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 3, 4, 6 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 3, 4, 6 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 6, 4, 3 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 4, 3, 6 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 4, 6, 3 ] )} ], 
  [ {PartialPerm( [ 2, 4 ], [ 6, 4 ] )} ], 
  [ {PartialPerm( [ 2, 6 ], [ 5, 6 ] )} ], 
  [ {PartialPerm( [ 2, 3, 5 ], [ 7, 4, 1 ] )} ], 
  [ {PartialPerm( [ 2, 4 ], [ 7, 4 ] )} ], 
  [ {PartialPerm( [ 2, 3, 4 ], [ 4, 1, 7 ] )} ], 
  [ {PartialPerm( [ 2, 4 ], [ 1, 4 ] )} ], 
  [ {PartialPerm( [ 2, 6 ], [ 4, 1 ] )} ], 
  [ {PartialPerm( [ 1, 2, 7 ], [ 3, 6, 1 ] )} ], 
  [ {PartialPerm( [ 6, 7 ], [ 3, 6 ] )} ], 
  [ {PartialPerm( [ 2, 3, 7 ], [ 5, 3, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 6, 5, 3 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 3, 5, 6 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 3, 5, 6 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 6, 5, 3 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 5, 3, 6 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 5, 6, 3 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 6, 4, 5 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 5, 4, 6 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 5, 4, 6 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 6, 4, 5 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 4, 5, 6 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 4, 6, 5 ] )} ], 
  [ {PartialPerm( [ 1, 6, 7 ], [ 3, 5, 6 ] )} ], 
  [ {PartialPerm( [ 6, 7 ], [ 6, 3 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 1, 3, 6 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 6, 3, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 6, 3, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 1, 3, 6 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 3, 6, 1 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 3, 1, 6 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 4, 6, 1 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 1, 6, 4 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 1, 6, 4 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 4, 6, 1 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 6, 1, 4 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 6, 4, 1 ] )} ], 
  [ {PartialPerm( [ 1, 2, 7 ], [ 4, 6, 1 ] )} ], 
  [ {PartialPerm( [ 2, 6 ], [ 3, 6 ] )} ], 
  [ {PartialPerm( [ 2, 6 ], [ 6, 5 ] )} ], 
  [ {PartialPerm( [ 1, 2, 7 ], [ 4, 7, 1 ] )} ], 
  [ {PartialPerm( [ 6, 7 ], [ 7, 4 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 1, 4, 7 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 7, 4, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 7, 4, 1 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 1, 4, 7 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 4, 7, 1 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 4, 1, 7 ] )} ], 
  [ {PartialPerm( [ 2, 6 ], [ 4, 7 ] )} ], 
  [ {PartialPerm( [ 2, 3, 5 ], [ 5, 6, 3 ] )} ], 
  [ {PartialPerm( [ 2, 3, 4 ], [ 6, 3, 5 ] )} ], 
  [ {PartialPerm( [ 2, 4 ], [ 3, 6 ] )} ], 
  [ {PartialPerm( [ 2, 6 ], [ 6, 3 ] )} ], 
  [ {PartialPerm( [ 2, 6 ], [ 4, 6 ] )} ], 
  [ {PartialPerm( [ 6, 7 ], [ 6, 4 ] )} ], 
  [ {PartialPerm( [ 2, 4 ], [ 4, 7 ] )} ], 
  [ {PartialPerm( [ 2, 6 ], [ 7, 4 ] )} ], 
  [ {PartialPerm( [ 1, 2, 7 ], [ 6, 5, 3 ] )} ], 
  [ {PartialPerm( [ 1, 2, 3 ], [ 3, 6, 5 ] )}, 
      {PartialPerm( [ 1, 2, 3 ], [ 5, 6, 3 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 5, 6, 3 ] )}, 
      {PartialPerm( [ 1, 2, 5 ], [ 3, 6, 5 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 6, 5, 3 ] )}, 
      {PartialPerm( [ 2, 5, 7 ], [ 6, 3, 5 ] )} ], 
  [ {PartialPerm( [ 2, 4 ], [ 4, 6 ] )} ], 
  [ {PartialPerm( [ 2, 6 ], [ 6, 4 ] )} ] ]
gap> ForAll(Union(List(last, Elements)), x-> x in s);
false
gap> Union(List(last2, Elements));
[ {PartialPerm( [  ], [  ] )}, {PartialPerm( [ 1 ], [ 5 ] )}, 
  {PartialPerm( [ 2 ], [ 2 ] )}, {PartialPerm( [ 2 ], [ 5 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 1, 4 ] )}, {PartialPerm( [ 1, 2 ], [ 3, 5 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 3, 6 ] )}, {PartialPerm( [ 1, 2 ], [ 4, 1 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 4, 2 ] )}, {PartialPerm( [ 1, 2 ], [ 4, 6 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 4, 7 ] )}, {PartialPerm( [ 1, 2 ], [ 5, 6 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 6, 2 ] )}, {PartialPerm( [ 1, 2 ], [ 6, 3 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 6, 4 ] )}, {PartialPerm( [ 1, 2 ], [ 6, 5 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 6, 7 ] )}, {PartialPerm( [ 1, 2 ], [ 7, 2 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 7, 4 ] )}, {PartialPerm( [ 3 ], [ 5 ] )}, 
  {PartialPerm( [ 2, 3 ], [ 1, 4 ] )}, {PartialPerm( [ 2, 3 ], [ 2, 4 ] )}, 
  {PartialPerm( [ 2, 3 ], [ 2, 6 ] )}, {PartialPerm( [ 2, 3 ], [ 2, 7 ] )}, 
  {PartialPerm( [ 2, 3 ], [ 3, 6 ] )}, {PartialPerm( [ 2, 3 ], [ 4, 1 ] )}, 
  {PartialPerm( [ 2, 3 ], [ 4, 6 ] )}, {PartialPerm( [ 2, 3 ], [ 4, 7 ] )}, 
  {PartialPerm( [ 2, 3 ], [ 5, 3 ] )}, {PartialPerm( [ 2, 3 ], [ 5, 6 ] )}, 
  {PartialPerm( [ 2, 3 ], [ 6, 3 ] )}, {PartialPerm( [ 2, 3 ], [ 6, 4 ] )}, 
  {PartialPerm( [ 2, 3 ], [ 6, 5 ] )}, {PartialPerm( [ 2, 3 ], [ 7, 4 ] )}, 
  {PartialPerm( [ 2, 3 ], [ 7, 6 ] )}, {PartialPerm( [ 1, 3 ], [ 1, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 2, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 2, 6 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 3, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 3, 6 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 3, 7 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 4, 5 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 4, 6 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 4, 7 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 5, 3 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 5, 6 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 5, 7 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 6, 3 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 6, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 7, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 7, 5 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 7, 6 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 3, 5 ] )}, {PartialPerm( [ 1, 3 ], [ 3, 6 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 3, 1, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 3, 1, 5 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 3, 2, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 3, 2, 5 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 3, 2, 6 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 3, 2, 7 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 3, 4, 5 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 3, 4, 6 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 3, 5, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 3, 5, 6 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 3, 5, 7 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 3, 6, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 3, 6, 5 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 4, 1 ] )}, {PartialPerm( [ 1, 3 ], [ 4, 6 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 4, 7 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 4, 1, 3 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 4, 1, 5 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 4, 2, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 4, 2, 3 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 4, 2, 5 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 4, 2, 6 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 4, 3, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 4, 3, 5 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 4, 3, 6 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 4, 3, 7 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 4, 6, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 4, 7, 1 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 5, 6 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 5, 1, 3 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 5, 1, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 5, 1, 6 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 5, 2, 3 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 5, 2, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 5, 2, 7 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 5, 3, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 5, 3, 7 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 5, 4, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 5, 4, 3 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 5, 4, 6 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 5, 6, 3 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 5, 7, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 5, 7, 6 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 6, 3 ] )}, {PartialPerm( [ 1, 3 ], [ 6, 4 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 6, 5 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 6, 1, 5 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 6, 2, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 6, 2, 3 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 6, 2, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 6, 3, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 6, 3, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 6, 4, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 6, 4, 3 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 6, 4, 5 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 6, 5, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 6, 5, 3 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 6, 7, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 6, 7, 5 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 7, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 7, 2, 3 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 7, 3, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 7, 3, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 7, 3, 5 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 7, 4, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 7, 5, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 7, 5, 3 ] )}, {PartialPerm( [ 4 ], [ 5 ] )}, 
  {PartialPerm( [ 3, 4 ], [ 1, 4 ] )}, {PartialPerm( [ 3, 4 ], [ 3, 6 ] )}, 
  {PartialPerm( [ 3, 4 ], [ 4, 1 ] )}, {PartialPerm( [ 3, 4 ], [ 4, 6 ] )}, 
  {PartialPerm( [ 3, 4 ], [ 4, 7 ] )}, {PartialPerm( [ 3, 4 ], [ 5, 3 ] )}, 
  {PartialPerm( [ 3, 4 ], [ 5, 6 ] )}, {PartialPerm( [ 3, 4 ], [ 6, 3 ] )}, 
  {PartialPerm( [ 3, 4 ], [ 6, 4 ] )}, {PartialPerm( [ 3, 4 ], [ 6, 5 ] )}, 
  {PartialPerm( [ 3, 4 ], [ 7, 4 ] )}, {PartialPerm( [ 2, 4 ], [ 1, 4 ] )}, 
  {PartialPerm( [ 2, 3, 4 ], [ 1, 3, 5 ] )}, 
  {PartialPerm( [ 2, 3, 4 ], [ 1, 5, 4 ] )}, 
  {PartialPerm( [ 2, 3, 4 ], [ 1, 6, 5 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 3, 6 ] )}, 
  {PartialPerm( [ 2, 3, 4 ], [ 3, 1, 6 ] )}, 
  {PartialPerm( [ 2, 3, 4 ], [ 3, 4, 1 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 4, 1 ] )}, {PartialPerm( [ 2, 4 ], [ 4, 6 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 4, 7 ] )}, 
  {PartialPerm( [ 2, 3, 4 ], [ 4, 1, 7 ] )}, 
  {PartialPerm( [ 2, 3, 4 ], [ 4, 6, 3 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 5, 3 ] )}, {PartialPerm( [ 2, 4 ], [ 5, 6 ] )}, 
  {PartialPerm( [ 2, 3, 4 ], [ 5, 1, 7 ] )}, 
  {PartialPerm( [ 2, 3, 4 ], [ 5, 7, 3 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 6, 3 ] )}, {PartialPerm( [ 2, 4 ], [ 6, 4 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 6, 5 ] )}, 
  {PartialPerm( [ 2, 3, 4 ], [ 6, 3, 5 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 7, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3, 4 ], [ 5, 7, 1, 6 ] )}, 
  {PartialPerm( [ 5 ], [ 5 ] )}, {PartialPerm( [ 4, 5 ], [ 1, 4 ] )}, 
  {PartialPerm( [ 4, 5 ], [ 3, 5 ] )}, {PartialPerm( [ 4, 5 ], [ 3, 6 ] )}, 
  {PartialPerm( [ 4, 5 ], [ 4, 1 ] )}, {PartialPerm( [ 4, 5 ], [ 4, 6 ] )}, 
  {PartialPerm( [ 4, 5 ], [ 4, 7 ] )}, {PartialPerm( [ 4, 5 ], [ 5, 6 ] )}, 
  {PartialPerm( [ 4, 5 ], [ 6, 3 ] )}, {PartialPerm( [ 4, 5 ], [ 6, 4 ] )}, 
  {PartialPerm( [ 4, 5 ], [ 6, 5 ] )}, {PartialPerm( [ 4, 5 ], [ 7, 4 ] )}, 
  {PartialPerm( [ 3, 5 ], [ 1, 4 ] )}, {PartialPerm( [ 3, 5 ], [ 3, 6 ] )}, 
  {PartialPerm( [ 3, 5 ], [ 4, 1 ] )}, {PartialPerm( [ 3, 5 ], [ 4, 6 ] )}, 
  {PartialPerm( [ 3, 5 ], [ 4, 7 ] )}, {PartialPerm( [ 3, 5 ], [ 5, 3 ] )}, 
  {PartialPerm( [ 3, 5 ], [ 5, 6 ] )}, {PartialPerm( [ 3, 5 ], [ 6, 3 ] )}, 
  {PartialPerm( [ 3, 5 ], [ 6, 4 ] )}, {PartialPerm( [ 3, 5 ], [ 6, 5 ] )}, 
  {PartialPerm( [ 3, 5 ], [ 7, 4 ] )}, {PartialPerm( [ 2, 5 ], [ 1, 4 ] )}, 
  {PartialPerm( [ 2, 5 ], [ 2, 4 ] )}, {PartialPerm( [ 2, 5 ], [ 2, 6 ] )}, 
  {PartialPerm( [ 2, 5 ], [ 2, 7 ] )}, {PartialPerm( [ 2, 5 ], [ 3, 6 ] )}, 
  {PartialPerm( [ 2, 3, 5 ], [ 3, 5, 4 ] )}, 
  {PartialPerm( [ 2, 5 ], [ 4, 1 ] )}, {PartialPerm( [ 2, 5 ], [ 4, 6 ] )}, 
  {PartialPerm( [ 2, 5 ], [ 4, 7 ] )}, 
  {PartialPerm( [ 2, 4, 5 ], [ 4, 5, 1 ] )}, 
  {PartialPerm( [ 2, 3, 5 ], [ 4, 1, 5 ] )}, 
  {PartialPerm( [ 2, 3, 5 ], [ 4, 1, 6 ] )}, 
  {PartialPerm( [ 2, 5 ], [ 5, 3 ] )}, {PartialPerm( [ 2, 5 ], [ 5, 6 ] )}, 
  {PartialPerm( [ 2, 4, 5 ], [ 5, 3, 6 ] )}, 
  {PartialPerm( [ 2, 3, 5 ], [ 5, 6, 3 ] )}, 
  {PartialPerm( [ 2, 5 ], [ 6, 3 ] )}, {PartialPerm( [ 2, 5 ], [ 6, 4 ] )}, 
  {PartialPerm( [ 2, 5 ], [ 6, 5 ] )}, 
  {PartialPerm( [ 2, 4, 5 ], [ 6, 1, 3 ] )}, 
  {PartialPerm( [ 2, 4, 5 ], [ 6, 1, 4 ] )}, 
  {PartialPerm( [ 2, 3, 5 ], [ 6, 3, 1 ] )}, 
  {PartialPerm( [ 2, 3, 5 ], [ 6, 4, 1 ] )}, 
  {PartialPerm( [ 2, 5 ], [ 7, 4 ] )}, {PartialPerm( [ 2, 5 ], [ 7, 6 ] )}, 
  {PartialPerm( [ 2, 4, 5 ], [ 7, 1, 4 ] )}, 
  {PartialPerm( [ 2, 3, 5 ], [ 7, 4, 1 ] )}, 
  {PartialPerm( [ 1, 5 ], [ 1, 4 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 1, 2, 4 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 1, 2, 6 ] )}, 
  {PartialPerm( [ 1, 2, 3, 5 ], [ 1, 2, 5, 4 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 1, 3, 4 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 1, 3, 6 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 1, 3, 7 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 1, 4, 5 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 1, 4, 6 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 1, 4, 7 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 1, 5, 3 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 1, 5, 6 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 1, 5, 7 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 1, 6, 3 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 1, 6, 4 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 1, 7, 4 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 1, 7, 5 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 1, 7, 6 ] )}, 
  {PartialPerm( [ 1, 2, 3, 5 ], [ 1, 7, 6, 5 ] )}, 
  {PartialPerm( [ 1, 5 ], [ 3, 6 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 3, 1, 4 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 3, 1, 5 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 3, 2, 4 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 3, 2, 5 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 3, 2, 6 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 3, 2, 7 ] )}, 
  {PartialPerm( [ 1, 2, 3, 5 ], [ 3, 2, 1, 6 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 3, 4, 5 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 3, 4, 6 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 3, 5, 1 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 3, 5, 6 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 3, 5, 7 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 3, 6, 1 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 3, 6, 5 ] )}, 
  {PartialPerm( [ 1, 5 ], [ 4, 1 ] )}, {PartialPerm( [ 1, 5 ], [ 4, 6 ] )}, 
  {PartialPerm( [ 1, 5 ], [ 4, 7 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 4, 1, 3 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 4, 1, 5 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 4, 2, 1 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 4, 2, 3 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 4, 2, 5 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 4, 2, 6 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 4, 3, 1 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 4, 3, 5 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 4, 3, 6 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 4, 3, 7 ] )}, 
  {PartialPerm( [ 1, 2, 4, 5 ], [ 4, 3, 7, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3, 5 ], [ 4, 3, 1, 7 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 4, 6, 1 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 4, 7, 1 ] )}, 
  {PartialPerm( [ 1, 5 ], [ 5, 3 ] )}, {PartialPerm( [ 1, 5 ], [ 5, 6 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 5, 1, 3 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 5, 1, 4 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 5, 1, 6 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 5, 2, 3 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 5, 2, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3, 5 ], [ 5, 2, 7, 3 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 5, 3, 4 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 5, 3, 7 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 5, 4, 1 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 5, 4, 3 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 5, 4, 6 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 5, 6, 3 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 5, 7, 1 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 5, 7, 6 ] )}, 
  {PartialPerm( [ 1, 5 ], [ 6, 3 ] )}, {PartialPerm( [ 1, 5 ], [ 6, 4 ] )}, 
  {PartialPerm( [ 1, 5 ], [ 6, 5 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 6, 1, 5 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 6, 2, 1 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 6, 2, 3 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 6, 2, 4 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 6, 3, 1 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 6, 3, 4 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 6, 4, 1 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 6, 4, 3 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 6, 4, 5 ] )}, 
  {PartialPerm( [ 1, 2, 4, 5 ], [ 6, 4, 5, 3 ] )}, 
  {PartialPerm( [ 1, 2, 3, 5 ], [ 6, 4, 3, 5 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 6, 5, 1 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 6, 5, 3 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 6, 7, 1 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 6, 7, 5 ] )}, 
  {PartialPerm( [ 1, 5 ], [ 7, 4 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 7, 2, 3 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 7, 2, 5 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 7, 3, 1 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 7, 3, 4 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 7, 3, 5 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 7, 4, 1 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 7, 5, 1 ] )}, 
  {PartialPerm( [ 1, 2, 5 ], [ 7, 5, 3 ] )}, {PartialPerm( [ 6 ], [ 5 ] )}, 
  {PartialPerm( [ 2, 6 ], [ 1, 4 ] )}, {PartialPerm( [ 2, 6 ], [ 3, 5 ] )}, 
  {PartialPerm( [ 2, 6 ], [ 3, 6 ] )}, {PartialPerm( [ 2, 6 ], [ 4, 1 ] )}, 
  {PartialPerm( [ 2, 6 ], [ 4, 6 ] )}, {PartialPerm( [ 2, 6 ], [ 4, 7 ] )}, 
  {PartialPerm( [ 2, 6 ], [ 5, 6 ] )}, {PartialPerm( [ 2, 6 ], [ 6, 3 ] )}, 
  {PartialPerm( [ 2, 6 ], [ 6, 4 ] )}, {PartialPerm( [ 2, 6 ], [ 6, 5 ] )}, 
  {PartialPerm( [ 2, 6 ], [ 7, 4 ] )}, {PartialPerm( [ 1, 6 ], [ 1, 4 ] )}, 
  {PartialPerm( [ 1, 2, 6 ], [ 1, 4, 5 ] )}, 
  {PartialPerm( [ 1, 6 ], [ 3, 6 ] )}, 
  {PartialPerm( [ 1, 2, 6 ], [ 3, 6, 1 ] )}, 
  {PartialPerm( [ 1, 6 ], [ 4, 1 ] )}, {PartialPerm( [ 1, 6 ], [ 4, 6 ] )}, 
  {PartialPerm( [ 1, 6 ], [ 4, 7 ] )}, 
  {PartialPerm( [ 1, 2, 6 ], [ 4, 7, 1 ] )}, 
  {PartialPerm( [ 1, 6 ], [ 5, 3 ] )}, {PartialPerm( [ 1, 6 ], [ 5, 6 ] )}, 
  {PartialPerm( [ 1, 6 ], [ 6, 3 ] )}, {PartialPerm( [ 1, 6 ], [ 6, 4 ] )}, 
  {PartialPerm( [ 1, 6 ], [ 6, 5 ] )}, 
  {PartialPerm( [ 1, 2, 6 ], [ 6, 5, 3 ] )}, 
  {PartialPerm( [ 1, 6 ], [ 7, 4 ] )}, {PartialPerm( [ 7 ], [ 5 ] )}, 
  {PartialPerm( [ 6, 7 ], [ 1, 4 ] )}, {PartialPerm( [ 6, 7 ], [ 3, 5 ] )}, 
  {PartialPerm( [ 6, 7 ], [ 3, 6 ] )}, {PartialPerm( [ 6, 7 ], [ 4, 1 ] )}, 
  {PartialPerm( [ 6, 7 ], [ 4, 6 ] )}, {PartialPerm( [ 6, 7 ], [ 4, 7 ] )}, 
  {PartialPerm( [ 6, 7 ], [ 5, 6 ] )}, {PartialPerm( [ 6, 7 ], [ 6, 3 ] )}, 
  {PartialPerm( [ 6, 7 ], [ 6, 4 ] )}, {PartialPerm( [ 6, 7 ], [ 6, 5 ] )}, 
  {PartialPerm( [ 6, 7 ], [ 7, 4 ] )}, {PartialPerm( [ 5, 7 ], [ 1, 4 ] )}, 
  {PartialPerm( [ 5, 7 ], [ 3, 6 ] )}, {PartialPerm( [ 5, 7 ], [ 4, 1 ] )}, 
  {PartialPerm( [ 5, 7 ], [ 4, 6 ] )}, {PartialPerm( [ 5, 7 ], [ 4, 7 ] )}, 
  {PartialPerm( [ 5, 7 ], [ 5, 3 ] )}, {PartialPerm( [ 5, 7 ], [ 5, 6 ] )}, 
  {PartialPerm( [ 5, 7 ], [ 6, 3 ] )}, {PartialPerm( [ 5, 7 ], [ 6, 4 ] )}, 
  {PartialPerm( [ 5, 7 ], [ 6, 5 ] )}, {PartialPerm( [ 5, 7 ], [ 7, 4 ] )}, 
  {PartialPerm( [ 3, 7 ], [ 1, 4 ] )}, {PartialPerm( [ 3, 7 ], [ 3, 5 ] )}, 
  {PartialPerm( [ 3, 7 ], [ 3, 6 ] )}, {PartialPerm( [ 3, 7 ], [ 4, 1 ] )}, 
  {PartialPerm( [ 3, 7 ], [ 4, 6 ] )}, {PartialPerm( [ 3, 7 ], [ 4, 7 ] )}, 
  {PartialPerm( [ 3, 7 ], [ 5, 6 ] )}, {PartialPerm( [ 3, 7 ], [ 6, 3 ] )}, 
  {PartialPerm( [ 3, 7 ], [ 6, 4 ] )}, {PartialPerm( [ 3, 7 ], [ 6, 5 ] )}, 
  {PartialPerm( [ 3, 7 ], [ 7, 4 ] )}, {PartialPerm( [ 2, 7 ], [ 1, 4 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 1, 3, 4 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 1, 3, 5 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 1, 4, 3 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 1, 4, 5 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 1, 5, 3 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 1, 5, 4 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 1, 5, 6 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 1, 6, 5 ] )}, 
  {PartialPerm( [ 2, 7 ], [ 2, 4 ] )}, {PartialPerm( [ 2, 7 ], [ 2, 6 ] )}, 
  {PartialPerm( [ 2, 7 ], [ 2, 7 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 1, 4 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 1, 6 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 3, 4 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 3, 5 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 3, 6 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 3, 7 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 4, 1 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 4, 3 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 4, 5 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 4, 6 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 5, 3 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 5, 4 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 6, 1 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 6, 3 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 6, 4 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 7, 3 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 2, 7, 5 ] )}, 
  {PartialPerm( [ 2, 7 ], [ 3, 6 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 3, 1, 4 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 3, 1, 6 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 3, 1, 7 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 3, 4, 1 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 3, 4, 5 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 3, 4, 6 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 3, 4, 7 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 3, 5, 4 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 3, 5, 7 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 3, 6, 1 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 3, 6, 4 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 3, 7, 1 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 3, 7, 4 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 3, 7, 5 ] )}, 
  {PartialPerm( [ 2, 3, 5, 7 ], [ 3, 7, 4, 1 ] )}, 
  {PartialPerm( [ 2, 7 ], [ 4, 1 ] )}, {PartialPerm( [ 2, 7 ], [ 4, 6 ] )}, 
  {PartialPerm( [ 2, 7 ], [ 4, 7 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 4, 1, 5 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 4, 1, 6 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 4, 1, 7 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 4, 3, 5 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 4, 3, 6 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 4, 5, 1 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 4, 5, 3 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 4, 5, 6 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 4, 6, 1 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 4, 6, 3 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 4, 6, 5 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 4, 7, 1 ] )}, 
  {PartialPerm( [ 2, 3, 7 ], [ 4, 5, 1 ] )}, 
  {PartialPerm( [ 2, 3, 5, 7 ], [ 4, 5, 6, 3 ] )}, 
  {PartialPerm( [ 2, 7 ], [ 5, 3 ] )}, {PartialPerm( [ 2, 7 ], [ 5, 6 ] )}, 
  {PartialPerm( [ 2, 6, 7 ], [ 5, 3, 6 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 5, 1, 3 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 5, 1, 6 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 5, 1, 7 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 5, 3, 1 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 5, 3, 6 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 5, 3, 7 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 5, 6, 1 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 5, 6, 3 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 5, 7, 1 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 5, 7, 3 ] )}, 
  {PartialPerm( [ 2, 3, 7 ], [ 5, 3, 6 ] )}, 
  {PartialPerm( [ 2, 7 ], [ 6, 3 ] )}, {PartialPerm( [ 2, 7 ], [ 6, 4 ] )}, 
  {PartialPerm( [ 2, 7 ], [ 6, 5 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 6, 1, 3 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 6, 1, 4 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 6, 3, 1 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 6, 3, 5 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 6, 4, 1 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 6, 5, 3 ] )}, 
  {PartialPerm( [ 2, 3, 7 ], [ 6, 1, 3 ] )}, 
  {PartialPerm( [ 2, 3, 7 ], [ 6, 1, 4 ] )}, 
  {PartialPerm( [ 2, 7 ], [ 7, 4 ] )}, {PartialPerm( [ 2, 7 ], [ 7, 6 ] )}, 
  {PartialPerm( [ 2, 6, 7 ], [ 7, 1, 4 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 7, 1, 4 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 7, 1, 5 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 7, 1, 6 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 7, 4, 1 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 7, 5, 1 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 7, 5, 6 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 7, 6, 1 ] )}, 
  {PartialPerm( [ 2, 5, 7 ], [ 7, 6, 5 ] )}, 
  {PartialPerm( [ 2, 3, 7 ], [ 7, 1, 4 ] )}, 
  {PartialPerm( [ 1, 7 ], [ 1, 4 ] )}, 
  {PartialPerm( [ 1, 6, 7 ], [ 1, 6, 3 ] )}, 
  {PartialPerm( [ 1, 6, 7 ], [ 1, 7, 4 ] )}, 
  {PartialPerm( [ 1, 2, 5, 7 ], [ 1, 2, 3, 6 ] )}, 
  {PartialPerm( [ 1, 2, 5, 7 ], [ 1, 3, 4, 7 ] )}, 
  {PartialPerm( [ 1, 2, 3, 6, 7 ], [ 1, 3, 4, 7, 5 ] )}, 
  {PartialPerm( [ 1, 2, 7 ], [ 1, 4, 5 ] )}, 
  {PartialPerm( [ 1, 2, 7 ], [ 1, 4, 6 ] )}, 
  {PartialPerm( [ 1, 2, 7 ], [ 1, 5, 4 ] )}, 
  {PartialPerm( [ 1, 2, 7 ], [ 1, 6, 5 ] )}, 
  {PartialPerm( [ 1, 7 ], [ 3, 6 ] )}, 
  {PartialPerm( [ 1, 6, 7 ], [ 3, 5, 6 ] )}, 
  {PartialPerm( [ 1, 2, 7 ], [ 3, 1, 6 ] )}, 
  {PartialPerm( [ 1, 2, 3, 4, 5, 7 ], [ 3, 2, 4, 6, 1, 5 ] )}, 
  {PartialPerm( [ 1, 2, 5, 7 ], [ 3, 4, 6, 5 ] )}, 
  {PartialPerm( [ 1, 2, 3, 6, 7 ], [ 3, 4, 6, 5, 1 ] )}, 
  {PartialPerm( [ 1, 2, 7 ], [ 3, 6, 1 ] )}, 
  {PartialPerm( [ 1, 7 ], [ 4, 1 ] )}, {PartialPerm( [ 1, 7 ], [ 4, 6 ] )}, 
  {PartialPerm( [ 1, 7 ], [ 4, 7 ] )}, 
  {PartialPerm( [ 1, 3, 7 ], [ 4, 7, 1 ] )}, 
  {PartialPerm( [ 1, 2, 7 ], [ 4, 1, 7 ] )}, 
  {PartialPerm( [ 1, 2, 3, 5, 7 ], [ 4, 2, 6, 3, 1 ] )}, 
  {PartialPerm( [ 1, 2, 7 ], [ 4, 6, 1 ] )}, 
  {PartialPerm( [ 1, 2, 6, 7 ], [ 4, 6, 1, 3 ] )}, 
  {PartialPerm( [ 1, 2, 7 ], [ 4, 7, 1 ] )}, 
  {PartialPerm( [ 1, 7 ], [ 5, 3 ] )}, {PartialPerm( [ 1, 7 ], [ 5, 6 ] )}, 
  {PartialPerm( [ 1, 6, 7 ], [ 5, 4, 1 ] )}, 
  {PartialPerm( [ 1, 2, 5, 7 ], [ 5, 2, 1, 4 ] )}, 
  {PartialPerm( [ 1, 2, 7 ], [ 5, 3, 4 ] )}, 
  {PartialPerm( [ 1, 2, 7 ], [ 5, 4, 6 ] )}, 
  {PartialPerm( [ 1, 2, 7 ], [ 5, 7, 3 ] )}, 
  {PartialPerm( [ 1, 7 ], [ 6, 3 ] )}, {PartialPerm( [ 1, 7 ], [ 6, 4 ] )}, 
  {PartialPerm( [ 1, 7 ], [ 6, 5 ] )}, 
  {PartialPerm( [ 1, 6, 7 ], [ 6, 3, 4 ] )}, 
  {PartialPerm( [ 1, 6, 7 ], [ 6, 5, 1 ] )}, 
  {PartialPerm( [ 1, 3, 7 ], [ 6, 5, 3 ] )}, 
  {PartialPerm( [ 1, 2, 5, 7 ], [ 6, 2, 4, 3 ] )}, 
  {PartialPerm( [ 1, 2, 7 ], [ 6, 3, 5 ] )}, 
  {PartialPerm( [ 1, 2, 7 ], [ 6, 5, 3 ] )}, 
  {PartialPerm( [ 1, 2, 5, 7 ], [ 6, 7, 1, 5 ] )}, 
  {PartialPerm( [ 1, 7 ], [ 7, 4 ] )}, 
  {PartialPerm( [ 1, 6, 7 ], [ 7, 3, 5 ] )}, 
  {PartialPerm( [ 1, 2, 5, 7 ], [ 7, 2, 5, 3 ] )}, 
  {PartialPerm( [ 1, 2, 7 ], [ 7, 3, 4 ] )} ]
gap> Union(List(last, Elements));
[ <empty partial perm>, <identity partial perm on [ 1 ]>, [1,3], [1,4], 
  [1,5], [1,6], [1,7], [2,1], <identity partial perm on [ 2 ]>, [2,3], [2,4], 
  [2,5], [2,6], [2,7], <identity partial perm on [ 1, 2 ]>, [2,3](1), 
  [2,4](1), [2,5](1), [2,6](1), [2,7](1), [2,1,3], [1,3](2), [1,3][2,4], 
  [1,3][2,5], [1,3][2,6], [1,3][2,7], [2,1,4], [1,4](2), [1,4][2,3], 
  [1,4][2,5], [1,4][2,6], [1,4][2,7], [2,1,5], [1,5](2), [1,5][2,3], 
  [1,5][2,4], [1,5][2,6], [1,5][2,7], [2,1,6], [1,6](2), [1,6][2,3], 
  [1,6][2,4], [1,6][2,5], [1,6][2,7], [2,1,7], [1,7](2), [1,7][2,3], 
  [1,7][2,4], [1,7][2,5], [3,1], <identity partial perm on [ 3 ]>, [3,4], 
  [3,5], [3,6], [3,7], [2,1](3), [2,1][3,4], [2,1][3,5], [2,1][3,6], 
  [2,1][3,7], [3,1](2), <identity partial perm on [ 2, 3 ]>, [3,4](2), 
  [3,5](2), [3,6](2), [3,7](2), [2,3,1], [2,3,4], [2,3,5], [2,3,6], [2,3,7], 
  [2,4][3,1], [2,4](3), [2,4][3,5], [2,4][3,6], [2,4][3,7], [2,5][3,1], 
  [2,5](3), [2,5][3,4], [2,5][3,6], [2,5][3,7], [2,6][3,1], [2,6](3), 
  [2,6][3,4], [2,6][3,5], [2,7][3,1], [2,7](3), [2,7][3,4], [2,7][3,5], 
  [2,7][3,6], <identity partial perm on [ 1, 3 ]>, [3,4](1), [3,5](1), 
  [3,6](1), [3,7](1), <identity partial perm on [ 1, 2, 3 ]>, [3,4](1)(2), 
  [3,5](1)(2), [3,6](1)(2), [2,3,4](1), [2,3,6](1), [2,3,7](1), 
  [2,4][3,5](1), [2,4][3,6](1), [2,4][3,7](1), [2,5](1)(3), [2,5][3,6](1), 
  [2,5][3,7](1), [2,6](1)(3), [2,6][3,4](1), [2,7][3,4](1), [2,7][3,5](1), 
  [2,7][3,6](1), (1,3), [1,3,4], [1,3,5], [1,3,6], [1,3,7], [2,1,3,4], 
  [2,1,3,5], (1,3)(2), [1,3,4](2), [1,3,5](2), [1,3,6](2), [1,3,7](2), 
  [1,3,5][2,4], [1,3,6][2,4], [2,5](1,3), [1,3,6][2,5], [1,3,7][2,5], 
  [2,6](1,3), [1,3,5][2,6], [3,1,4], [1,4](3), [1,4][3,5], [1,4][3,6], 
  [1,4][3,7], [2,1,4](3), [2,1,4][3,5], [3,1,4](2), [1,4](2)(3), 
  [1,4][3,5](2), [1,4][3,6](2), [2,3,1,4], [1,4][2,3,5], [1,4][2,3,6], 
  [1,4][2,3,7], [2,6][3,1,4], [2,7][3,1,4], [3,1,5], [1,5](3), [1,5][3,4], 
  [1,5][3,6], [1,5][3,7], [2,1,5](3), [2,1,5][3,4], [2,1,5][3,6], [3,1,5](2), 
  [1,5](2)(3), [1,5][3,4](2), [1,5][3,7](2), [1,5][2,3,4], [1,5][2,3,7], 
  [2,4][3,1,5], [1,5][2,4](3), [1,5][2,4][3,6], [1,5][2,6](3), [2,7][3,1,5], 
  [1,5][2,7][3,6], [3,1,6], [1,6](3), [1,6][3,4], [1,6][3,5], [2,1,6][3,5], 
  [3,1,6](2), [1,6](2)(3), [1,6][3,4](2), [2,3,1,6], [1,6][2,3,4], 
  [2,4][3,1,6], [1,6][2,4](3), [1,6][2,4][3,5], [2,5][3,1,6], [1,6][2,5](3), 
  [2,7][3,1,6], [1,6][2,7][3,5], [3,1,7], [1,7](3), [1,7][3,4], [1,7][3,5], 
  [1,7](2)(3), [1,7][3,5](2), [2,3,1,7], [1,7][2,3,4], [1,7][2,3,5], 
  [2,4][3,1,7], [2,5][3,1,7], [1,7][2,5](3), [4,1], [4,3], 
  <identity partial perm on [ 4 ]>, [4,5], [4,6], [4,7], [4,3,1], [3,1](4), 
  [3,1][4,5], [3,1][4,6], [3,1][4,7], [4,1](3), 
  <identity partial perm on [ 3, 4 ]>, [4,5](3), [4,6](3), [4,7](3), [3,4,1], 
  (3,4), [3,4,5], [3,4,6], [3,4,7], [3,5][4,1], [4,3,5], [3,5](4), 
  [3,5][4,6], [3,5][4,7], [3,6][4,1], [4,3,6], [3,6](4), [3,6][4,5], 
  [3,7][4,1], [4,3,7], [3,7](4), [3,7][4,5], [2,1][4,3], [2,1](4), 
  [2,1][4,5], [2,1][4,6], [2,1][4,7], [2,1][4,5](3), [2,1][3,5](4), 
  [2,1][3,6][4,5], [2,3][4,1], [2,3](4), [2,3][4,5], [2,3][4,6], [2,3][4,7], 
  [2,3,1][4,6], [2,3,4,1], [2,4,1], [2,4,3], [2,4,5], [2,4,6], [2,4,7], 
  [2,4,7][3,1], [2,4,3,6], [2,5][4,1], [2,5][4,3], [2,5](4), [2,5][4,6], 
  [2,5][4,7], [2,5][3,1][4,7], [2,5][4,3,7], [2,6][4,1], [2,6][4,3], 
  [2,6](4), [2,6][4,5], [2,6][4,5](3), [2,7][4,1], [2,7][4,3], [2,7](4), 
  [2,7][4,5], [2,7][3,1,5][4,6], [5,1], [5,3], [5,4], 
  <identity partial perm on [ 5 ]>, [5,6], [5,7], [4,1][5,3], [5,4,1], 
  [4,1](5), [4,1][5,6], [4,1][5,7], [4,3][5,1], [5,4,3], [4,3](5), 
  [4,3][5,6], [4,3][5,7], [5,1](4), [5,3](4), 
  <identity partial perm on [ 4, 5 ]>, [5,6](4), [5,7](4), [4,5,1], [4,5,3], 
  (4,5), [4,5,6], [4,5,7], [4,6][5,1], [4,6][5,3], [5,4,6], [4,6](5), 
  [4,7][5,1], [4,7][5,3], [5,4,7], [4,7](5), [5,3,1], [3,1][5,4], [3,1](5), 
  [3,1][5,6], [3,1][5,7], [5,1](3), [5,4](3), 
  <identity partial perm on [ 3, 5 ]>, [5,6](3), [5,7](3), [3,4][5,1], 
  [5,3,4], [3,4](5), [3,4][5,6], [3,4][5,7], [3,5,1], (3,5), [3,5,4], 
  [3,5,6], [3,5,7], [3,6][5,1], [5,3,6], [3,6][5,4], [3,6](5), [3,7][5,1], 
  [5,3,7], [3,7][5,4], [3,7](5), [2,1][5,3], [2,1][5,4], [2,1](5), 
  [2,1][5,6], [2,1][5,7], [5,1](2), [5,3](2), [5,4](2), 
  <identity partial perm on [ 2, 5 ]>, [5,6](2), [5,7](2), [2,3][5,1], 
  [2,3][5,4], [2,3](5), [2,3][5,6], [2,3][5,7], [2,3,5,4], [2,4][5,1], 
  [2,4][5,3], [2,4](5), [2,4][5,6], [2,4][5,7], [2,4,5,1], [2,4][3,1](5), 
  [2,4][3,1][5,6], [2,5,1], [2,5,3], [2,5,4], [2,5,6], [2,5,7], [2,5,6][4,3], 
  [2,5,3,6], [2,6][5,1], [2,6][5,3], [2,6][5,4], [2,6](5), [2,6][4,1][5,3], 
  [2,6][5,4,1], [2,6][5,1](3), [2,6][3,4][5,1], [2,7][5,1], [2,7][5,3], 
  [2,7][5,4], [2,7](5), [2,7][5,6], [2,7][5,4,1], [2,7][3,4][5,1], [5,3](1), 
  [5,4](1), <identity partial perm on [ 1, 5 ]>, [5,6](1), [5,7](1), 
  [5,3](1)(2), [5,4](1)(2), <identity partial perm on [ 1, 2, 5 ]>, 
  [5,6](1)(2), [3,5,4](1)(2), [2,3][5,4](1), [2,3][5,6](1), [2,3][5,7](1), 
  [2,4](1)(5), [2,4][5,6](1), [2,4][5,7](1), [2,5,3](1), [2,5,6](1), 
  [2,5,7](1), [2,6][5,3](1), [2,6][5,4](1), [2,7][5,4](1), [2,7](1)(5), 
  [2,7][5,6](1), [2,7][3,6](1)(5), [5,1,3], [1,3][5,4], [1,3](5), [1,3][5,6], 
  [1,3][5,7], [2,1,3][5,4], [2,1,3](5), [5,1,3](2), [1,3][5,4](2), 
  [1,3](2)(5), [1,3][5,6](2), [1,3][5,7](2), [5,6](1,3)(2), [1,3][2,4](5), 
  [1,3][2,4][5,6], [2,5,1,3], [1,3][2,5,6], [1,3][2,5,7], [2,6][5,1,3], 
  [1,3][2,6](5), [5,1,4], [1,4][5,3], [1,4](5), [1,4][5,6], [1,4][5,7], 
  [2,1,4][5,3], [2,1,4](5), [5,1,4](2), [1,4][5,3](2), [1,4](2)(5), 
  [1,4][5,6](2), [2,3][5,1,4], [1,4][2,3](5), [1,4][2,3][5,6], 
  [1,4][2,3][5,7], [2,3][5,1,4,7], [2,3,1,4][5,7], [2,6][5,1,4], 
  [2,7][5,1,4], (1,5), [1,5,3], [1,5,4], [1,5,6], [1,5,7], [2,1,5,3], 
  [2,1,5,4], [2,1,5,6], (1,5)(2), [1,5,3](2), [1,5,4](2), [1,5,7](2), 
  [1,5,3,7](2), [1,5,4][2,3], [1,5,7][2,3], [2,4](1,5), [1,5,3][2,4], 
  [1,5,6][2,4], [1,5,3][2,6], [2,7](1,5), [1,5,6][2,7], [5,1,6], [1,6][5,3], 
  [1,6][5,4], [1,6](5), [2,1,6](5), [5,1,6](2), [1,6][5,3](2), [1,6][5,4](2), 
  [2,3][5,1,6], [1,6][2,3][5,4], [2,4][5,1,6], [1,6][2,4][5,3], 
  [1,6][2,4](5), [1,6][2,4,5,3], [1,6][2,4](3)(5), [2,5,1,6], [1,6][2,5,3], 
  [2,7][5,1,6], [1,6][2,7](5), [5,1,7], [1,7][5,3], [1,7][5,4], [1,7](5), 
  [1,7][5,3](2), [1,7](2)(5), [2,3][5,1,7], [1,7][2,3][5,4], [1,7][2,3](5), 
  [2,4][5,1,7], [2,5,1,7], [1,7][2,5,3], [6,1], [6,3], [6,4], [6,5], 
  <identity partial perm on [ 6 ]>, [6,7], [2,1][6,3], [2,1][6,4], 
  [2,1][6,5], [2,1](6), [2,1][6,7], [2,3][6,1], [2,3][6,4], [2,3][6,5], 
  [2,3](6), [2,3][6,7], [2,4][6,1], [2,4][6,3], [2,4][6,5], [2,4](6), 
  [2,4][6,7], [2,5][6,1], [2,5][6,3], [2,5][6,4], [2,5](6), [2,5][6,7], 
  [2,6,1], [2,6,3], [2,6,4], [2,6,5], [2,7][6,1], [2,7][6,3], [2,7][6,4], 
  [2,7][6,5], [6,3](1), [6,4](1), [6,5](1), <identity partial perm on [ 1, 6 ]
    >, [6,7](1), [2,4][6,5](1), [6,1,3], [1,3][6,4], [1,3][6,5], [1,3](6), 
  [1,3][6,7], [2,6,1,3], [6,1,4], [1,4][6,3], [1,4][6,5], [1,4](6), 
  [1,4][6,7], [2,7][6,1,4], [6,1,5], [1,5][6,3], [1,5][6,4], [1,5](6), 
  [1,5][6,7], (1,6), [1,6,3], [1,6,4], [1,6,5], [1,6,3][2,5], [6,1,7], 
  [1,7][6,3], [1,7][6,4], [1,7][6,5], [7,1], [7,3], [7,4], [7,5], [7,6], 
  <identity partial perm on [ 7 ]>, [6,1][7,3], [6,1][7,4], [6,1][7,5], 
  [7,6,1], [6,1](7), [6,3][7,1], [6,3][7,4], [6,3][7,5], [7,6,3], [6,3](7), 
  [6,4][7,1], [6,4][7,3], [6,4][7,5], [7,6,4], [6,4](7), [6,5][7,1], 
  [6,5][7,3], [6,5][7,4], [7,6,5], [6,5](7), [7,1](6), [7,3](6), [7,4](6), 
  [7,5](6), [6,7,1], [6,7,3], [6,7,4], [6,7,5], [5,1][7,3], [5,1][7,4], 
  [7,5,1], [5,1][7,6], [5,1](7), [5,3][7,1], [5,3][7,4], [7,5,3], [5,3][7,6], 
  [5,3](7), [5,4][7,1], [5,4][7,3], [7,5,4], [5,4][7,6], [5,4](7), [7,1](5), 
  [7,3](5), [7,4](5), [7,6](5), <identity partial perm on [ 5, 7 ]>, 
  [5,6][7,1], [5,6][7,3], [5,6][7,4], [7,5,6], [5,7,1], [5,7,3], [5,7,4], 
  (5,7), [7,3,1], [3,1][7,4], [3,1][7,5], [3,1][7,6], [3,1](7), [7,1](3), 
  [7,4](3), [7,5](3), [7,6](3), <identity partial perm on [ 3, 7 ]>, 
  [3,4][7,1], [7,3,4], [3,4][7,5], [3,4][7,6], [3,4](7), [3,5][7,1], [7,3,5], 
  [3,5][7,4], [3,5][7,6], [3,5](7), [3,6][7,1], [7,3,6], [3,6][7,4], 
  [3,6][7,5], [3,7,1], (3,7), [3,7,4], [3,7,5], [2,1][7,3], [2,1][7,4], 
  [2,1][7,5], [2,1][7,6], [2,1](7), [2,1][5,3][7,4], [2,1][7,5,3], 
  [2,1][5,4][7,3], [2,1][7,5,4], [2,1][7,3](5), [2,1][7,4](5), [2,1][7,6](5), 
  [2,1][7,5,6], [7,1](2), [7,3](2), [7,4](2), [7,5](2), [7,6](2), 
  <identity partial perm on [ 2, 7 ]>, [5,1][7,3](2), [5,1][7,4](2), 
  [7,5,1](2), [5,1][7,6](2), [5,3][7,1](2), [5,3][7,4](2), [7,5,3](2), 
  [5,3][7,6](2), [5,3](2)(7), [5,4][7,1](2), [5,4][7,3](2), [7,5,4](2), 
  [5,4][7,6](2), [7,1](2)(5), [7,3](2)(5), [7,4](2)(5), 
  <identity partial perm on [ 2, 5, 7 ]>, [5,6][7,1](2), [5,6][7,3](2), 
  [5,6][7,4](2), [5,7,3](2), (2)(5,7), [2,3][7,1], [2,3][7,4], [2,3][7,5], 
  [2,3][7,6], [2,3](7), [2,3][5,1][7,4], [2,3][5,1][7,6], [2,3][5,1](7), 
  [2,3][5,4][7,1], [2,3][7,5,4], [2,3][5,4][7,6], [2,3][5,4](7), 
  [2,3][7,4](5), [2,3](5)(7), [2,3][5,6][7,1], [2,3][5,6][7,4], [2,3][5,7,1], 
  [2,3][5,7,4], [2,3](5,7), [2,3,7,1][5,4], [2,4][7,1], [2,4][7,3], 
  [2,4][7,5], [2,4][7,6], [2,4](7), [2,4][7,5,1], [2,4][5,1][7,6], 
  [2,4][5,1](7), [2,4][7,5,3], [2,4][5,3][7,6], [2,4][7,1](5), [2,4][7,3](5), 
  [2,4][7,6](5), [2,4][5,6][7,1], [2,4][5,6][7,3], [2,4][7,5,6], 
  [2,4][5,7,1], [2,4][3,5][7,1], [2,4][7,3,5,6], [2,5][7,1], [2,5][7,3], 
  [2,5][7,4], [2,5][7,6], [2,5](7), [2,5][7,6,3], [2,5,1][7,3], [2,5,1][7,6], 
  [2,5,1](7), [2,5,3][7,1], [2,5,3][7,6], [2,5,3](7), [2,5,6][7,1], 
  [2,5,6][7,3], [2,5,7,1], [2,5,7,3], [2,5][7,6](3), [2,6][7,1], [2,6][7,3], 
  [2,6][7,4], [2,6][7,5], [2,6][5,1][7,3], [2,6][5,1][7,4], [2,6][5,3][7,1], 
  [2,6][7,5,3], [2,6][5,4][7,1], [2,6][7,3](5), [2,6][7,3,1], [2,6][3,1][7,4],
  [2,7,1], [2,7,3], [2,7,4], [2,7,5], [2,7,6], [2,7,4][6,1], [2,7,4][5,1], 
  [2,7,5,1], [2,7,6][5,1], [2,7,1][5,4], [2,7,1](5), [2,7,6](5), 
  [2,7,1][5,6], [2,7,5,6], [2,7,4][3,1], [7,3](1), [7,4](1), [7,5](1), 
  [7,6](1), <identity partial perm on [ 1, 7 ]>, [7,3](1)(6), [6,7,4](1), 
  [5,3][7,6](1)(2), [2,3][5,4](1)(7), [2,3,4][6,7,5](1), [2,4][7,5](1), 
  [2,4][7,6](1), [2,5][7,4](1), [2,6][7,5](1), [7,1,3], [1,3][7,4], 
  [1,3][7,5], [1,3][7,6], [1,3](7), [1,3][7,6,5], [2,1,3][7,6], 
  [7,5,1,3,4,6](2), [1,3][2,4][7,5,6], [2,4][7,1,3,6,5], [2,6][7,1,3], 
  [7,1,4], [1,4][7,3], [1,4][7,5], [1,4][7,6], [1,4](7), [3,7,1,4], 
  [2,1,4](7), [5,3,6][7,1,4](2), [2,6][7,1,4], [2,6,1,4][7,3], [2,7,1,4], 
  [7,1,5], [1,5][7,3], [1,5][7,4], [1,5][7,6], [1,5](7), [6,4][7,1,5], 
  [7,4](1,5)(2), [1,5][2,3][7,4], [1,5][2,4][7,6], [1,5][2,7,3], [7,1,6], 
  [1,6][7,3], [1,6][7,4], [1,6][7,5], [1,6,3][7,4], [7,1,6,5], [1,6][7,3,5], 
  [1,6][5,4][7,3](2), [1,6][2,3][7,5], [1,6][2,5][7,3], [2,7,5,1,6], (1,7), 
  [1,7,3], [1,7,4], [1,7,5], [1,7,5][6,3], [1,7,3](2)(5), [1,7,4][2,3] ]
gap> ForAll(last, x-> x in s);
true
gap> Set(last2)=AsSSortedList(s);
true

#
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
gap> f:=Transformation( [ 4, 2, 4, 3 ] );;
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
gap> gens:=[ PartialPermNC( [ 1, 2, 4 ], [ 4, 5, 6 ] ),
> PartialPermNC( [ 1, 2, 5 ], [ 2, 1, 3 ] ),
> PartialPermNC( [ 1, 2, 4, 6 ], [ 2, 4, 3, 5 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5 ], [ 4, 3, 6, 5, 1 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
201
gap> f:=PartialPerm([ 1 .. 5 ], [ 4, 3, 6, 5, 1 ]);;
gap> d:=DClassNC(s, f);
{PartialPerm( [ 1, 2, 3, 4, 5 ], [ 4, 3, 6, 5, 1 ] )}
gap> h:=HClassNC(d, f);
{PartialPerm( [ 1, 2, 3, 4, 5 ], [ 4, 3, 6, 5, 1 ] )}
gap> Size(h);
1
gap> Size(d);
1
gap> h=d;
true
gap> d=h;
true

#
gap> gens:=
> [ PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 
>   19, 20, 21, 23, 24, 25, 26, 27, 28, 30, 32, 33, 34, 35, 36, 37, 40, 42, 44, 
>   46, 47, 51, 53, 54, 58, 59, 60, 61, 63, 65, 66, 67, 69, 71, 72, 76, 79, 84, 
>   86, 88, 94, 95, 100 ], [ 46, 47, 33, 32, 70, 97, 29, 30, 34, 11, 37, 89, 
>   77, 52, 73, 2, 96, 66, 88, 69, 93, 87, 85, 68, 48, 25, 28, 43, 49, 95, 40, 
>   24, 16, 94, 76, 63, 58, 23, 100, 38, 27, 78, 21, 71, 4, 72, 36, 13, 99, 90, 
>   17, 41, 98, 10, 35, 91, 53, 45, 82, 42 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 18, 19, 
>   21, 22, 24, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 38, 39, 40, 41, 42, 43, 
>   44, 46, 48, 49, 51, 52, 54, 56, 58, 59, 61, 64, 65, 67, 68, 70, 73, 74, 76, 
>   78, 79, 80, 82, 88, 90, 97 ], [ 63, 38, 57, 12, 9, 91, 59, 32, 54, 83, 92, 
>   96, 99, 18, 3, 81, 5, 65, 2, 37, 21, 49, 16, 75, 24, 23, 43, 27, 1, 48, 6, 
>   35, 30, 79, 82, 51, 39, 25, 61, 77, 62, 22, 64, 14, 72, 7, 50, 8, 80, 19, 
>   94, 69, 10, 40, 67, 28, 88, 93, 66, 36, 70, 56 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
>   18, 21, 22, 23, 24, 25, 26, 27, 29, 30, 31, 34, 35, 36, 37, 38, 39, 40, 42, 
>   43, 44, 46, 48, 49, 51, 52, 53, 55, 58, 60, 63, 64, 66, 67, 68, 69, 71, 73, 
>   75, 80, 86, 87, 88, 90, 91, 94, 95, 97 ], [ 89, 85, 8, 56, 42, 10, 61, 25, 
>   98, 55, 39, 92, 62, 21, 34, 57, 44, 14, 53, 64, 59, 84, 12, 87, 78, 83, 30, 
>   32, 68, 73, 2, 86, 23, 48, 47, 79, 93, 15, 76, 97, 77, 11, 33, 100, 91, 67, 
>   18, 16, 99, 60, 74, 17, 95, 49, 4, 66, 41, 69, 94, 31, 29, 5, 63, 58, 72 
>  ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 17, 20, 21, 
>   22, 23, 24, 26, 28, 29, 30, 32, 34, 35, 37, 39, 40, 42, 43, 44, 45, 46, 47, 
>   48, 49, 51, 53, 54, 55, 56, 58, 59, 60, 61, 63, 64, 65, 66, 67, 68, 72, 74, 
>   75, 79, 80, 82, 87, 88, 91, 92, 99, 100 ], [ 89, 67, 34, 15, 57, 29, 4, 62, 
>   76, 20, 52, 22, 35, 75, 98, 78, 40, 46, 28, 6, 55, 90, 16, 12, 65, 26, 66, 
>   36, 25, 61, 83, 38, 41, 93, 2, 39, 87, 85, 17, 92, 97, 43, 30, 5, 13, 94, 
>   44, 80, 54, 99, 81, 31, 7, 68, 11, 100, 72, 14, 9, 91, 32, 64, 60, 8, 23 
>  ] ) ];;
gap> s:=Semigroup(gens);;
gap> f:=PartialPerm([ 2, 63 ], [ 28, 89 ]);;
gap> d:=DClassNC(s, f);
{PartialPerm( [ 2, 63 ], [ 28, 89 ] )}
gap> Size(d);
4752
gap> RhoOrb(d);
<closed orbit, 2874 points with Schreier tree with log with grading>
gap> 2874*2;
5748
gap> LambdaOrb(d);
<closed orbit, 1 points with Schreier tree with log with grading>
gap> NrLClasses(d);
1
gap> NrRClasses(d);
4752
gap> f:=PartialPerm([ 4, 29 ], [ 28, 89 ]);;
gap> f in d;
true
gap> h:=HClass(d, f);
{PartialPerm( [ 4, 29 ], [ 28, 89 ] )}
gap> hh:=HClassNC(d, f);
{PartialPerm( [ 4, 29 ], [ 28, 89 ] )}
gap> hh=h;
true
gap> Size(h);
1

#
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
>  Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 3, 4, 2, 2 ] ),
>  Transformation( [ 4, 1, 2, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> List(DClasses(s), RClassReps);
[ [ IdentityTransformation ], [ Transformation( [ 1, 3, 2, 3 ] ) ], 
  [ Transformation( [ 1, 4, 1, 2 ] ), Transformation( [ 1, 2, 4, 4 ] ) ], 
  [ Transformation( [ 4, 1, 2, 1 ] ), Transformation( [ 4, 2, 1, 2 ] ) ], 
  [ Transformation( [ 1, 4, 1, 4 ] ), Transformation( [ 1, 4, 4, 4 ] ), 
      Transformation( [ 1, 1, 4, 1 ] ) ], [ Transformation( [ 1, 4, 2, 4 ] ) ]
    , [ Transformation( [ 4, 4, 2, 2 ] ), Transformation( [ 4, 2, 4, 4 ] ), 
      Transformation( [ 4, 4, 4, 2 ] ) ], [ Transformation( [ 1, 1, 1, 1 ] ) ]
    , [ Transformation( [ 2, 4, 2, 4 ] ), Transformation( [ 2, 4, 4, 4 ] ), 
      Transformation( [ 2, 2, 4, 2 ] ) ] ]
gap> reps:=Concatenation(last);
[ IdentityTransformation, Transformation( [ 1, 3, 2, 3 ] ), 
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
gap> NrDClasses(s);
9
gap> NrLClasses(s);
21
gap> List(reps, x-> DClass(s, x));
[ {IdentityTransformation}, {Transformation( [ 1, 3, 2, 3 ] )}, 
  {Transformation( [ 1, 4, 1, 2 ] )}, {Transformation( [ 1, 4, 1, 2 ] )}, 
  {Transformation( [ 4, 1, 2, 1 ] )}, {Transformation( [ 4, 2, 1, 2 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 1, 4 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 2, 4 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 4, 4, 2, 2 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )}, {Transformation( [ 2, 4, 2, 4 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )} ]
gap> d:=DClass(s, Transformation( [ 1, 2, 4, 4 ] ));
{Transformation( [ 1, 4, 1, 2 ] )}
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
gap> List(reps, x-> DClass(s, x));
[ {IdentityTransformation}, {Transformation( [ 1, 3, 2, 3 ] )}, 
  {Transformation( [ 1, 4, 1, 2 ] )}, {Transformation( [ 1, 4, 1, 2 ] )}, 
  {Transformation( [ 4, 1, 2, 1 ] )}, {Transformation( [ 4, 2, 1, 2 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 1, 4 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 2, 4 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 4, 4, 2, 2 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )}, {Transformation( [ 2, 4, 2, 4 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )} ]
gap> Union(List(last, x-> LClass(x,Representative(x))));
[ Transformation( [ 1, 1, 1, 1 ] ), Transformation( [ 1, 1, 4, 1 ] ), 
  Transformation( [ 1, 2, 1 ] ), Transformation( [ 1, 2, 3, 2 ] ), 
  IdentityTransformation, Transformation( [ 1, 2, 4, 2 ] ), 
  Transformation( [ 1, 2, 4, 4 ] ), Transformation( [ 1, 3, 2, 3 ] ), 
  Transformation( [ 1, 4, 1, 2 ] ), Transformation( [ 1, 4, 1, 4 ] ), 
  Transformation( [ 1, 4, 2, 2 ] ), Transformation( [ 1, 4, 2, 4 ] ), 
  Transformation( [ 1, 4, 4, 4 ] ), Transformation( [ 2, 2, 2 ] ), 
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
[ {IdentityTransformation}, {Transformation( [ 1, 3, 2, 3 ] )}, 
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
[ {IdentityTransformation}, {Transformation( [ 1, 3, 2, 3 ] )}, 
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
  Transformation( [ 1, 2, 1 ] ), Transformation( [ 1, 2, 3, 2 ] ), 
  IdentityTransformation, Transformation( [ 1, 2, 4, 2 ] ), 
  Transformation( [ 1, 2, 4, 4 ] ), Transformation( [ 1, 3, 2, 3 ] ), 
  Transformation( [ 1, 4, 1, 2 ] ), Transformation( [ 1, 4, 1, 4 ] ), 
  Transformation( [ 1, 4, 2, 2 ] ), Transformation( [ 1, 4, 2, 4 ] ), 
  Transformation( [ 1, 4, 4, 4 ] ), Transformation( [ 2, 2, 2 ] ), 
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
[ {Transformation( [ 1, 1, 1, 1 ] )}, {IdentityTransformation}, 
  {Transformation( [ 1, 3, 2, 3 ] )}, {Transformation( [ 1, 4, 1, 2 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 2, 4 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )}, {Transformation( [ 4, 1, 2, 1 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )} ]

#
gap> gens:=
> [ PartialPermNC( [ 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 
>   20, 22, 23, 24, 25, 26, 28, 29, 31, 32, 33, 35, 36, 37, 38, 41, 42, 44, 45, 
>   50, 51, 52, 54, 55, 60, 62, 64, 65, 66, 68, 71, 73, 75, 77, 78, 79, 83, 84, 
>   94, 95, 96, 97 ], [ 30, 56, 33, 17, 43, 34, 28, 78, 91, 24, 44, 84, 71, 81, 
>   57, 90, 20, 69, 70, 6, 82, 26, 53, 86, 32, 22, 12, 95, 59, 40, 73, 76, 98, 
>   48, 80, 51, 9, 27, 49, 93, 52, 60, 94, 11, 75, 96, 72, 4, 87, 37, 29, 50, 
>   39, 45, 88, 67, 14, 99 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 19, 
>   20, 21, 23, 24, 25, 26, 28, 30, 32, 35, 36, 37, 41, 42, 43, 47, 48, 49, 50, 
>   51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 64, 65, 67, 68, 69, 71, 72, 74, 76, 
>   81, 82, 83, 84, 86, 87, 92, 93 ], [ 56, 4, 87, 14, 67, 82, 17, 73, 18, 12, 
>   35, 43, 80, 99, 7, 96, 58, 76, 36, 30, 98, 26, 62, 1, 75, 27, 10, 74, 55, 
>   47, 37, 95, 39, 52, 84, 72, 50, 53, 77, 24, 59, 66, 9, 49, 70, 6, 51, 89, 
>   21, 11, 85, 15, 19, 28, 79, 40, 34, 71, 5, 29, 88, 16, 8 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 
>   19, 20, 22, 23, 24, 25, 26, 27, 28, 31, 32, 33, 34, 35, 36, 37, 39, 41, 42, 
>   44, 46, 50, 52, 53, 54, 56, 57, 58, 59, 61, 62, 63, 64, 65, 68, 70, 71, 72, 
>   77, 81, 84, 88, 89, 91, 93, 95, 97, 99, 100 ], 
> [ 53, 10, 43, 41, 57, 14, 68, 20, 54, 62, 5, 49, 86, 56, 91, 48, 9, 87, 33, 
>   64, 60, 13, 70, 92, 80, 69, 35, 88, 98, 4, 96, 79, 94, 71, 61, 27, 89, 97, 
>   46, 28, 40, 3, 100, 17, 19, 39, 82, 52, 6, 16, 77, 76, 45, 67, 23, 31, 29, 
>   12, 95, 72, 85, 7, 26, 38, 18, 24 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 16, 20, 21, 23, 
>   24, 25, 26, 27, 28, 29, 31, 32, 33, 34, 35, 36, 38, 40, 41, 42, 43, 44, 47, 
>   48, 49, 50, 53, 54, 55, 56, 58, 59, 62, 64, 65, 66, 68, 69, 70, 72, 74, 76, 
>   78, 83, 84, 86, 90, 91, 92, 93, 94, 99, 100 ], 
> [ 3, 77, 85, 63, 47, 30, 68, 21, 95, 13, 49, 33, 62, 6, 78, 81, 83, 35, 69, 
>   50, 26, 61, 27, 93, 56, 39, 48, 5, 19, 52, 73, 12, 8, 89, 25, 86, 84, 14, 
>   70, 29, 58, 88, 43, 37, 10, 92, 65, 22, 76, 38, 74, 34, 4, 94, 82, 67, 60, 
>   2, 23, 59, 80, 11, 40, 98, 51, 28 ] ) ];;
gap> s:=Semigroup(gens);
<partial perm semigroup on 97 pts with 4 generators>
gap> f:=
> PartialPermNC( [ 5, 7, 11, 12, 14, 24, 25, 26, 27, 29, 31, 32, 34, 35, 41, 
>   42, 44, 47, 48, 49, 50, 53, 62, 69, 70, 86, 92 ], 
> [ 23, 52, 39, 62, 11, 47, 94, 34, 70, 50, 73, 89, 2, 86, 14, 81, 74, 83, 77, 
>   92, 48, 26, 13, 98, 84, 60, 33 ] );;
gap> d:=DClass(s, f);
{PartialPerm( [ 5, 7, 11, 12, 14, 24, 25, 26, 27, 29, 31, 32, 34, 35, 41, 42, 
    44, 47, 48, 49, 50, 53, 62, 69, 70, 86, 92 ], 
 [ 23, 52, 39, 62, 11, 47, 94, 34, 70, 50, 73, 89, 2, 86, 14, 81, 74, 83, 77, 
    92, 48, 26, 13, 98, 84, 60, 33 ] )}
gap> Size(d);
1
gap> RhoOrb(d);
<closed orbit, 1 points with Schreier tree with log with grading>
gap> LambdaOrb(d);
<closed orbit, 35494 points with Schreier tree with log>
gap> f:=PartialPerm([ 5, 7, 56, 83, 92 ], [ 30, 52, 16, 21, 29 ]);;
gap> d:=DClassNC(s, f);
{PartialPerm( [ 5, 7, 56, 83, 92 ], [ 30, 52, 16, 21, 29 ] )}
gap> Size(d);
1
gap> iter:=IteratorOfDClasses(s);
<iterator of D-classes>
gap> repeat d:=NextIterator(iter); until Size(d)>1;
gap> d;
{PartialPerm( [ 74, 84 ], [ 26, 6 ] )}
gap> Size(d);
6793298
gap> f:=PartialPerm([ 1, 88 ], [ 78, 48 ]);;
gap> f in d;
true
gap> r:=RClass(d, f);
{PartialPerm( [ 1, 88 ], [ 26, 6 ] )}
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
gap> IsRegularClass(r);
true
gap> ForAll(r, x-> x in r);
true
gap> repeat d:=NextIterator(iter); until Size(d)>1;
gap> d;
{PartialPerm( [ 41, 50 ], [ 26, 6 ] )}
gap> Size(d);
3686
gap> f:=PartialPerm([ 41, 50 ], [ 17, 32 ]);;
gap> r:=RClassNC(d, f);
{PartialPerm( [ 41, 50 ], [ 6, 26 ] )}
gap> Size(r);
3686
gap> ForAll(r, x-> x in d);
true
gap> d=r;
true
gap> rr:=RClass(s, f);
{PartialPerm( [ 41, 50 ], [ 26, 6 ] )}
gap> rr=r;
true
gap> r=rr;
true
gap> d;
{PartialPerm( [ 41, 50 ], [ 26, 6 ] )}
gap> GroupHClass(d);
fail
gap>   

#
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
<transformation semigroup on 10 pts with 10 generators>
gap> f:=Transformation( [ 6, 6, 6, 6, 6, 10, 6, 6, 6, 6 ] );;
gap> d:=DClassNC(s, f);
{Transformation( [ 6, 6, 6, 6, 6, 10, 6, 6, 6, 6 ] )}
gap> Size(d);
31680
gap> IsRegularDClass(d);
true
gap> GroupHClass(d);
{Transformation( [ 10, 10, 10, 10, 10, 6, 10, 10, 10, 10 ] )}

#
gap> gens:=[ PartialPermNC( [ 1, 3, 4, 6, 10 ], [ 3, 4, 1, 6, 10 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5, 6 ], [ 10, 3, 9, 1, 5, 8 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 6, 10 ], [ 1, 8, 2, 3, 4, 9 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 8, 9, 10 ], [ 5, 8, 9, 7, 2, 6, 10 ] ) ];;
gap> s:=Semigroup(gens);
<partial perm semigroup on 9 pts with 4 generators>
gap> Size(s);
789
gap> NrDClasses(s);
251
gap> d:=DClasses(s)[251];
{PartialPerm( [ 2, 4 ], [ 2, 7 ] )}
gap> Size(d);
1
gap> First(DClasses(s), IsRegularDClass);
{PartialPerm( [ 1, 3, 4, 6, 10 ], [ 3, 4, 1, 6, 10 ] )}
gap> d:=last;
{PartialPerm( [ 1, 3, 4, 6, 10 ], [ 3, 4, 1, 6, 10 ] )}
gap> Size(d);
3
gap> GroupHClass(d);
{PartialPerm( [ 1, 3, 4, 6, 10 ], [ 1, 3, 4, 6, 10 ] )}
gap> Size(last);
3
gap> h:=last2;
{PartialPerm( [ 1, 3, 4, 6, 10 ], [ 1, 3, 4, 6, 10 ] )}
gap> h=d;
true
gap> Elements(h);
[ <identity partial perm on [ 1, 3, 4, 6, 10 ]>, (1,3,4)(6)(10), 
  (1,4,3)(6)(10) ]
gap> Number(DClasses(s), IsRegularDClass);
6
gap> List(DClasses(s), Idempotents);
[ [ <identity partial perm on [ 1, 3, 4, 6, 10 ]> ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], 
  [ <identity partial perm on [ 10 ]>, <identity partial perm on [ 9 ]>, 
      <identity partial perm on [ 3 ]>, <identity partial perm on [ 6 ]>, 
      <identity partial perm on [ 8 ]>, <identity partial perm on [ 4 ]>, 
      <identity partial perm on [ 2 ]>, <identity partial perm on [ 1 ]> ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [ <identity partial perm on [ 5 ]> ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], 
  [ <identity partial perm on [ 2, 8, 10 ]> ], [  ], [ <empty partial perm> ],
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], 
  [ <identity partial perm on [ 3, 4 ]>, <identity partial perm on [ 1, 4 ]>, 
      <identity partial perm on [ 1, 3 ]> ], [  ], [  ], [  ], [  ], [  ], 
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
  [  ], [  ] ]
gap> Concatenation(last);
[ <identity partial perm on [ 1, 3, 4, 6, 10 ]>, 
  <identity partial perm on [ 10 ]>, <identity partial perm on [ 9 ]>, 
  <identity partial perm on [ 3 ]>, <identity partial perm on [ 6 ]>, 
  <identity partial perm on [ 8 ]>, <identity partial perm on [ 4 ]>, 
  <identity partial perm on [ 2 ]>, <identity partial perm on [ 1 ]>, 
  <identity partial perm on [ 5 ]>, <identity partial perm on [ 2, 8, 10 ]>, 
  <empty partial perm>, <identity partial perm on [ 3, 4 ]>, 
  <identity partial perm on [ 1, 4 ]>, <identity partial perm on [ 1, 3 ]> ]
gap> ForAll(last, x-> x in s);
true
gap> Set(last2)=Idempotents(s);
false
gap> Set(last3)=Set(Idempotents(s));
true

#
gap> gens:=[Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] ),
> Transformation( [ 2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7 ] )];;
gap> s:=Monoid(gens);;
gap> List(DClasses(s), Idempotents);
[ [ IdentityTransformation ], 
  [ Transformation( [ 1, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ) ], 
  [ Transformation( [ 4, 2, 2, 4, 5, 6, 7, 7, 7 ] ), 
      Transformation( [ 2, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ), 
      Transformation( [ 6, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ), 
      Transformation( [ 7, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ), 
      Transformation( [ 5, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ), 
      Transformation( [ 10, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ), 
      Transformation( [ 11, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
      Transformation( [ 4, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ) ] ]
gap> Concatenation(last);
[ IdentityTransformation, Transformation( [ 1, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] )
    , Transformation( [ 4, 2, 2, 4, 5, 6, 7, 7, 7 ] ), 
  Transformation( [ 2, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ), 
  Transformation( [ 6, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ), 
  Transformation( [ 7, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ), 
  Transformation( [ 5, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ), 
  Transformation( [ 10, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ), 
  Transformation( [ 11, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 4, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ) ]
gap> e:=last;
[ IdentityTransformation, Transformation( [ 1, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] )
    , Transformation( [ 4, 2, 2, 4, 5, 6, 7, 7, 7 ] ), 
  Transformation( [ 2, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ), 
  Transformation( [ 6, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ), 
  Transformation( [ 7, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ), 
  Transformation( [ 5, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ), 
  Transformation( [ 10, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ), 
  Transformation( [ 11, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 4, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] ) ]
gap> IsDuplicateFree(e);
true
gap> ForAll(e, x-> x in s);
true
gap> Set(Idempotents(s))=Set(e);
true

#
gap> gens:=
> [ PartialPermNC( [ 1, 2, 3, 4, 5, 6 ], [ 7, 10, 8, 6, 4, 2 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 9 ], [ 6, 8, 3, 10, 4, 2 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 6, 7 ], [ 8, 7, 5, 6, 2, 9 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 6, 8 ], [ 9, 3, 4, 7, 8, 6 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
489
gap> First(DClasses(s), IsRegularDClass);
{PartialPerm( [  ], [  ] )}
gap> NrRegularDClasses(s);
5
gap> PositionsProperty(DClasses(s), IsRegularDClass);
[ 25, 26, 33, 36, 64 ]
gap> d:=DClasses(s)[26];
{PartialPerm( [ 3 ], [ 8 ] )}
gap> NrLClasses(d);
8
gap> NrRClasses(d);
8
gap> Size(d);
64
gap> Idempotents(d);
[ <identity partial perm on [ 8 ]>, <identity partial perm on [ 6 ]>, 
  <identity partial perm on [ 3 ]>, <identity partial perm on [ 5 ]>, 
  <identity partial perm on [ 7 ]>, <identity partial perm on [ 2 ]>, 
  <identity partial perm on [ 9 ]>, <identity partial perm on [ 4 ]> ]
gap> ForAll(last, x-> x in d);
true
gap> dd:=DClassNC(s, PartialPermNC([ 8 ], [ 9 ]));
{PartialPerm( [ 8 ], [ 9 ] )}
gap> dd=d;
true
gap> Size(dd);
64
gap> Idempotents(dd);
[ <identity partial perm on [ 9 ]>, <identity partial perm on [ 2 ]>, 
  <identity partial perm on [ 8 ]>, <identity partial perm on [ 7 ]>, 
  <identity partial perm on [ 3 ]>, <identity partial perm on [ 6 ]>, 
  <identity partial perm on [ 5 ]>, <identity partial perm on [ 4 ]> ]
gap> Set(LClassReps(dd))=Set(LClassReps(d));
false
gap> LClassReps(dd);
[ [8,9], [8,2], <identity partial perm on [ 8 ]>, [8,7], [8,3], [8,6], [8,5], 
  [8,4] ]
gap> LClassReps(d);
[ [3,8], [3,6], <identity partial perm on [ 3 ]>, [3,5], [3,7], [3,2], [3,9], 
  [3,4] ]
gap> Set(List(LClassReps(d), x-> LClass(d, x)))=Set(List(LClassReps(dd), x-> LClass(d, x)));
true
gap> Set(List(LClassReps(d), x-> LClass(d, x)))=Set(List(LClassReps(dd),
> x-> LClass(dd, x)));
true
gap> ForAll(LClassReps(dd), x-> x in d);
true
gap> ForAll(LClassReps(d), x-> x in dd);
true

#
gap> gens:=
> [ PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], 
> [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], 
> [ 2, 1, 3, 4, 5, 6, 7, 8, 9, 10 ] ),
>  PartialPermNC( [ 1, 2, 4, 7, 10 ], [ 8, 5, 9, 6, 7 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
12398231
gap> NrRClasses(s);
639
gap> f:=PartialPerm([ 3, 9 ], [ 5, 4 ]);;
gap> d:=DClass(s, f);
{PartialPerm( [ 4, 7 ], [ 6, 7 ] )}
gap> Position(LambdaOrb(d), ImageSetOfPartialPerm(Representative(d)));
7
gap> OrbSCC(RhoOrb(d))[RhoOrbSCCIndex(d)];
[ 8, 15, 24, 38, 39, 40, 60, 61, 62, 63, 64, 92, 93, 94, 95, 96, 132, 133, 
  134, 135, 136, 146, 158, 160, 183, 184, 185, 209, 211, 238, 239, 240, 270, 
  271, 273, 303, 304, 305, 339, 369, 370, 371, 407, 434, 435 ]
gap> OrbSCC(LambdaOrb(d))[LambdaOrbSCCIndex(d)];
[ 7, 13, 20, 27, 34, 36, 45, 48, 49, 53, 62, 65, 66, 67, 71, 73, 86, 89, 90, 
  92, 95, 110, 113, 114, 115, 116, 119, 120, 137, 139, 142, 143, 147, 148, 
  170, 172, 173, 177, 178, 208, 209, 214, 244, 245, 278 ]
gap> NrIdempotents(d);
45
gap> Number(Idempotents(s), x-> x in d);
45
gap> s:=Semigroup(gens);
<partial perm semigroup on 10 pts with 3 generators>
gap> d:=DClass(s, f);
{PartialPerm( [ 3, 9 ], [ 7, 6 ] )}
gap> s:=Semigroup(gens);
<partial perm semigroup on 10 pts with 3 generators>
gap> d:=DClassNC(s, f);
{PartialPerm( [ 3, 9 ], [ 5, 4 ] )}
gap> NrIdempotents(d);
45
gap> Number(Idempotents(s), x-> x in d);
45
gap> s:=Semigroup(gens);
<partial perm semigroup on 10 pts with 3 generators>
gap> l:=LClass(s, f);
{PartialPerm( [ 3, 9 ], [ 5, 4 ] )}
gap> d:=DClassOfLClass(l);
{PartialPerm( [ 3, 9 ], [ 7, 6 ] )}
gap> NrIdempotents(d);
45
gap> s:=Semigroup(gens);
<partial perm semigroup on 10 pts with 3 generators>
gap> l:=LClass(s, f);
{PartialPerm( [ 3, 9 ], [ 5, 4 ] )}
gap> s:=Semigroup(gens);
<partial perm semigroup on 10 pts with 3 generators>
gap> l:=LClassNC(s, f);
{PartialPerm( [ 3, 9 ], [ 5, 4 ] )}
gap> d:=DClassOfLClass(l);
{PartialPerm( [ 3, 9 ], [ 5, 4 ] )}
gap> NrIdempotents(d);
45
gap> s:=Semigroup(gens);
<partial perm semigroup on 10 pts with 3 generators>
gap> r:=RClass(s, f);
{PartialPerm( [ 3, 9 ], [ 6, 7 ] )}
gap> d:=DClassOfRClass(r);
{PartialPerm( [ 3, 9 ], [ 6, 7 ] )}
gap> NrIdempotents(d);
45
gap> s:=Semigroup(gens);
<partial perm semigroup on 10 pts with 3 generators>
gap> r:=RClassNC(s, f);
{PartialPerm( [ 3, 9 ], [ 5, 4 ] )}
gap> d:=DClassOfRClass(r);
{PartialPerm( [ 3, 9 ], [ 5, 4 ] )}
gap> NrIdempotents(d);
45
gap> r:=RClassNC(s, f);
{PartialPerm( [ 3, 9 ], [ 5, 4 ] )}
gap> d:=DClassOfRClass(r);
{PartialPerm( [ 3, 9 ], [ 5, 4 ] )}
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

#
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
gap> d:=DClasses(s)[2];
{Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] )}
gap> NrHClasses(d);
1
gap> GroupHClass(d);
{Transformation( [ 1, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] )}
gap> last=d;
true
gap> Size(d);
7
gap> Size(last3);
7

#
gap> gens:=
> [ PartialPermNC( [ 1, 2, 3, 5, 6, 7, 12 ], [ 11, 10, 3, 4, 6, 2, 8 ] ),
>  PartialPermNC( [ 1, 2, 4, 5, 6, 8, 9, 10, 11 ], 
> [ 2, 8, 1, 10, 11, 4, 7, 6, 9 ] ) ];;
gap> s:=Semigroup(gens);
<partial perm semigroup on 12 pts with 2 generators>
gap> Size(s);
251
gap> d:=DClass(s, PartialPerm([ 5, 12 ], [ 6, 9 ]));;
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

#
gap> gens:=
> [ PartialPermNC( [ 1, 2, 3, 4, 9, 10, 11 ], [ 4, 1, 7, 12, 3, 9, 6 ] ),
> PartialPermNC( [ 1, 3, 4, 5, 7, 8, 11, 12 ], [ 4, 11, 2, 7, 9, 8, 1, 6 ] ) ];;
gap> s:=Semigroup(gens);;
gap> f:=PartialPerm([ 4, 7, 11 ], [ 2, 9, 6 ]);;
gap> d:=DClassNC(s, f);
{PartialPerm( [ 4, 7, 11 ], [ 2, 9, 6 ] )}
gap> NrHClasses(s);
125
gap> d:=DClass(s, f);
{PartialPerm( [ 4, 7, 11 ], [ 2, 9, 6 ] )}
gap> NrHClasses(s);
125
gap> NrHClasses(d);
1
gap> d:=DClassNC(s, f);
{PartialPerm( [ 4, 7, 11 ], [ 2, 9, 6 ] )}
gap> NrHClasses(d);
1
gap> d:=DClass(LClass(s, f));
{PartialPerm( [ 4, 7, 11 ], [ 2, 9, 6 ] )}
gap> NrHClasses(d);
1
gap> d:=DClass(RClass(s, f));
{PartialPerm( [ 4, 7, 11 ], [ 2, 9, 6 ] )}
gap> NrHClasses(d);
1
gap> NrRegularDClasses(s);
4
gap> NrDClasses(s);
65
gap> RClassReps(d);
[ [4,2][7,9][11,6] ]
gap> iter:=IteratorOfDClasses(s);
<iterator of D-classes>
gap> repeat d:=NextIterator(iter); until IsDoneIterator(iter) or Size(d)>1000;
gap> d;
{PartialPerm( [ 1, 5 ], [ 6, 4 ] )}
gap> Size(d);
1
gap> List(DClasses(s), Size);
[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 3, 9, 1, 1, 1, 3, 1, 1, 1, 1, 
  1, 3, 9, 1, 3, 3, 3, 3, 1, 3, 3, 1, 3, 1, 3, 1, 1, 1, 1, 1, 3, 3, 1, 3, 1, 
  3, 3, 9, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
gap> Position(last, 9);
17
gap> d:=DClasses(s)[17];
{PartialPerm( [ 2, 9 ], [ 4, 7 ] )}
gap> Size(d);
9
gap> IsRegularDClass(d);
true
gap> RClassReps(d);
[ [2,4][9,7], [1,4][3,7], <identity partial perm on [ 4, 7 ]> ]
gap> d:=DClassNC(s, Representative(d));
{PartialPerm( [ 2, 9 ], [ 4, 7 ] )}
gap> RClassReps(d);
[ [2,4][9,7], <identity partial perm on [ 4, 7 ]>, [1,4][3,7] ]
gap> s:=Semigroup(Generators(s));
<partial perm semigroup on 11 pts with 2 generators>
gap> d:=DClass(HClass(s, Representative(d)));
{PartialPerm( [ 2, 9 ], [ 4, 7 ] )}
gap> RClassReps(d);
[ [2,4][9,7], <identity partial perm on [ 4, 7 ]>, [1,4][3,7] ]
gap> Size(d);
9
gap> Number(s, x-> x in d);
9
gap> ForAll(d, x-> x in d);
true
gap> HClassReps(d);
[ [2,4][9,7], <identity partial perm on [ 2, 9 ]>, [2,1][9,3], 
  <identity partial perm on [ 4, 7 ]>, [4,2][7,9], [4,1][7,3], [1,4][3,7], 
  [1,2][3,9], <identity partial perm on [ 1, 3 ]> ]
gap> Set(last)=Elements(d);
true

#
gap> gens:=[Transformation([2,1,4,5,3,7,8,9,10,6]),
> Transformation([1,2,4,3,5,6,7,8,9,10]),
> Transformation([1,2,3,4,5,6,10,9,8,7]),
> Transformation([9,1,4,3,6,9,3,4,3,9])];;
gap> s:=Monoid(gens);;
gap> 
gap> f:=Transformation( [ 2, 1, 3, 5, 4, 10, 9, 8, 7, 6 ] );;
gap> d:=DClass(HClass(s, f));
{Transformation( [ 2, 1, 3, 5, 4, 10, 9, 8, 7, 6 ] )}
gap> Size(d);
120
gap> HClassReps(d);
[ Transformation( [ 2, 1, 3, 5, 4, 10, 9, 8, 7, 6 ] ) ]
gap> h:=GroupHClass(d);
{IdentityTransformation}
gap> h=d;
true
gap> Size(s);
491558
gap> f:=Transformation( [ 6, 6, 3, 6, 4, 6, 6, 6, 6, 4 ] );;
gap> d:=DClass(HClass(s, f));
{Transformation( [ 9, 3, 9, 4, 3, 9, 4, 9, 4, 9 ] )}
gap> Size(d);
121500
gap> NrHClasses(d);
20250
gap> Length(HClassReps(d));
20250
gap> ForAll(HClassReps(d), x-> x in d);
true
gap> d:=DClass(RClass(s, f));
{Transformation( [ 3, 9, 3, 4, 9, 3, 4, 3, 4, 3 ] )}
gap> Size(d);
121500
gap> ForAll(d, x-> x in d);
true
gap> NrIdempotents(d);
5550
gap> ForAll(Idempotents(d), x-> x in d);
true

# R-class
gap> gens:=[Transformation([2,2,3,5,5,6,7,8,14,16,16,17,18,14,16,16,17,18]),
> Transformation([1,3,3,4,5,6, 7, 8, 9,10,11,12,13,14,15,16,17,18]),
> Transformation([1,2,4,4,5,6,7,8,9, 10,11,12,13,15,15,16,17,18]),
> Transformation([1,2,3,4,6,6,7,8,9,10,11,12,13,14,15,17,17,18]),
> Transformation([1,2,3,4,5,7,7,8,9,10,11,12,13,14,15,16,18,18]),
> Transformation([1,2,3,4,5,6,8,8,9,10,11,12,13,14,15,16,17,2]),
> Transformation([1,2,9,10,11,12,13,1,9,10,11,12,13,14,15,16,17,18])];;
gap> s := Semigroup(gens);;
gap> f:=Transformation( [ 1, 2, 4, 4, 6, 6, 7, 8, 9, 10, 11, 12, 13, 15, 15,
> 17, 17, 18 ] );;
gap> r:=RClassNC(s, f);
{Transformation( [ 1, 2, 4, 4, 6, 6, 7, 8, 9, 10, 11, 12, 13, 15, 15, 17,
  17 ] )}
gap> Size(r);
1
gap> SchutzenbergerGroup(r);
Group(())
gap> f:=Transformation( [ 1, 2, 10, 10, 11, 12, 13, 1, 9, 10, 11, 12, 13, 15,
> 15, 16, 17, 18 ] );;
gap> r:=RClass(s, f);
{Transformation( [ 1, 2, 10, 10, 11, 12, 13, 1, 9, 10, 11, 12, 13, 15, 15 ] )}
gap> Size(r);
1
gap> SchutzenbergerGroup(r);
Group(())

#
gap> gens:=[ Transformation( [ 2, 4, 1, 5, 4, 4, 7, 3, 8, 1 ] ),
>   Transformation( [ 9, 1, 2, 8, 1, 5, 9, 9, 9, 5 ] ),
>   Transformation( [ 9, 3, 1, 5, 10, 3, 4, 6, 10, 2 ] ),
>   Transformation( [ 10, 7, 3, 7, 1, 9, 8, 8, 4, 10 ] ) ];;
gap> s:=Semigroup(gens);;
gap> f:=Transformation( [ 9, 10, 10, 3, 10, 9, 9, 9, 9, 9 ] );;
gap> r:=RClass(s, f);
{Transformation( [ 9, 5, 5, 2, 5, 9, 9, 9, 9, 9 ] )}
gap> Size(r);
546
gap> SchutzenbergerGroup(r);
Group([ (2,5), (5,9) ])
gap> ForAll(r, x-> x in r);
true
gap> f:=Transformation( [ 8, 8, 8, 8, 8, 8, 7, 7, 8, 8 ] );;
gap> r:=RClass(s, f);
{Transformation( [ 9, 9, 9, 9, 9, 9, 1, 1, 9, 9 ] )}
gap> Size(r);
86
gap> iter:=IteratorOfRClasses(s);
<iterator of R-classes>
gap> repeat r:=NextIterator(iter); until Size(r)>1000;
gap> r;
{Transformation( [ 9, 1, 8, 2, 1, 8, 9, 9, 9, 8 ] )}
gap> Size(r);
1992
gap> SchutzenbergerGroup(r);
Group([ (2,8), (1,8), (1,2,8,9) ])
gap> enum:=Enumerator(r);
<enumerator of R-class>
gap> ForAll(enum, x-> x in r);
true
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
true
gap> ForAll([1..Length(enum)], x-> Position(enum, enum[x])=x);
true
gap> NrHClasses(r);
83
gap> GreensHClasses(r);
[ {Transformation( [ 9, 1, 8, 2, 1, 8, 9, 9, 9, 8 ] )}, 
  {Transformation( [ 8, 2, 4, 3, 2, 4, 8, 8, 8, 4 ] )}, 
  {Transformation( [ 10, 6, 9, 3, 6, 9, 10, 10, 10, 9 ] )}, 
  {Transformation( [ 5, 9, 8, 2, 9, 8, 5, 5, 5, 8 ] )}, 
  {Transformation( [ 8, 4, 7, 10, 4, 7, 8, 8, 8, 7 ] )}, 
  {Transformation( [ 3, 2, 7, 1, 2, 7, 3, 3, 3, 7 ] )}, 
  {Transformation( [ 1, 5, 4, 3, 5, 4, 1, 1, 1, 4 ] )}, 
  {Transformation( [ 1, 5, 3, 6, 5, 3, 1, 1, 1, 3 ] )}, 
  {Transformation( [ 2, 3, 10, 1, 3, 10, 2, 2, 2, 10 ] )}, 
  {Transformation( [ 10, 9, 4, 3, 9, 4, 10, 10, 10, 4 ] )}, 
  {Transformation( [ 3, 4, 2, 5, 4, 2, 3, 3, 3, 2 ] )}, 
  {Transformation( [ 1, 10, 8, 7, 10, 8, 1, 1, 1, 8 ] )}, 
  {Transformation( [ 1, 2, 8, 4, 2, 8, 1, 1, 1, 8 ] )}, 
  {Transformation( [ 9, 1, 2, 5, 1, 2, 9, 9, 9, 2 ] )}, 
  {Transformation( [ 1, 4, 8, 7, 4, 8, 1, 1, 1, 8 ] )}, 
  {Transformation( [ 9, 1, 5, 8, 1, 5, 9, 9, 9, 5 ] )}, 
  {Transformation( [ 3, 5, 7, 1, 5, 7, 3, 3, 3, 7 ] )}, 
  {Transformation( [ 3, 10, 8, 7, 10, 8, 3, 3, 3, 8 ] )}, 
  {Transformation( [ 5, 4, 7, 2, 4, 7, 5, 5, 5, 7 ] )}, 
  {Transformation( [ 1, 4, 7, 2, 4, 7, 1, 1, 1, 7 ] )}, 
  {Transformation( [ 4, 3, 9, 1, 3, 9, 4, 4, 4, 9 ] )}, 
  {Transformation( [ 5, 8, 4, 2, 8, 4, 5, 5, 5, 4 ] )}, 
  {Transformation( [ 10, 5, 9, 3, 5, 9, 10, 10, 10, 9 ] )}, 
  {Transformation( [ 7, 10, 4, 9, 10, 4, 7, 7, 7, 4 ] )}, 
  {Transformation( [ 5, 4, 1, 2, 4, 1, 5, 5, 5, 1 ] )}, 
  {Transformation( [ 10, 1, 9, 5, 1, 9, 10, 10, 10, 9 ] )}, 
  {Transformation( [ 1, 7, 3, 10, 7, 3, 1, 1, 1, 3 ] )}, 
  {Transformation( [ 10, 1, 9, 3, 1, 9, 10, 10, 10, 9 ] )}, 
  {Transformation( [ 3, 1, 2, 9, 1, 2, 3, 3, 3, 2 ] )}, 
  {Transformation( [ 2, 5, 10, 1, 5, 10, 2, 2, 2, 10 ] )}, 
  {Transformation( [ 3, 10, 7, 4, 10, 7, 3, 3, 3, 7 ] )}, 
  {Transformation( [ 1, 4, 9, 7, 4, 9, 1, 1, 1, 9 ] )}, 
  {Transformation( [ 1, 5, 3, 10, 5, 3, 1, 1, 1, 3 ] )}, 
  {Transformation( [ 9, 2, 6, 4, 2, 6, 9, 9, 9, 6 ] )}, 
  {Transformation( [ 9, 3, 6, 5, 3, 6, 9, 9, 9, 6 ] )}, 
  {Transformation( [ 4, 10, 7, 1, 10, 7, 4, 4, 4, 7 ] )}, 
  {Transformation( [ 5, 1, 7, 2, 1, 7, 5, 5, 5, 7 ] )}, 
  {Transformation( [ 7, 2, 4, 3, 2, 4, 7, 7, 7, 4 ] )}, 
  {Transformation( [ 7, 5, 2, 3, 5, 2, 7, 7, 7, 2 ] )}, 
  {Transformation( [ 9, 5, 6, 4, 5, 6, 9, 9, 9, 6 ] )}, 
  {Transformation( [ 4, 1, 8, 10, 1, 8, 4, 4, 4, 8 ] )}, 
  {Transformation( [ 4, 10, 9, 1, 10, 9, 4, 4, 4, 9 ] )}, 
  {Transformation( [ 10, 3, 1, 8, 3, 1, 10, 10, 10, 1 ] )}, 
  {Transformation( [ 7, 10, 1, 9, 10, 1, 7, 7, 7, 1 ] )}, 
  {Transformation( [ 1, 2, 6, 4, 2, 6, 1, 1, 1, 6 ] )}, 
  {Transformation( [ 10, 5, 4, 3, 5, 4, 10, 10, 10, 4 ] )}, 
  {Transformation( [ 5, 1, 8, 4, 1, 8, 5, 5, 5, 8 ] )}, 
  {Transformation( [ 3, 10, 2, 9, 10, 2, 3, 3, 3, 2 ] )}, 
  {Transformation( [ 9, 5, 4, 3, 5, 4, 9, 9, 9, 4 ] )}, 
  {Transformation( [ 5, 1, 8, 2, 1, 8, 5, 5, 5, 8 ] )}, 
  {Transformation( [ 10, 6, 5, 3, 6, 5, 10, 10, 10, 5 ] )}, 
  {Transformation( [ 10, 1, 4, 3, 1, 4, 10, 10, 10, 4 ] )}, 
  {Transformation( [ 7, 1, 5, 8, 1, 5, 7, 7, 7, 5 ] )}, 
  {Transformation( [ 5, 10, 4, 2, 10, 4, 5, 5, 5, 4 ] )}, 
  {Transformation( [ 9, 4, 1, 2, 4, 1, 9, 9, 9, 1 ] )}, 
  {Transformation( [ 2, 9, 10, 1, 9, 10, 2, 2, 2, 10 ] )}, 
  {Transformation( [ 2, 5, 8, 7, 5, 8, 2, 2, 2, 8 ] )}, 
  {Transformation( [ 9, 5, 10, 4, 5, 10, 9, 9, 9, 10 ] )}, 
  {Transformation( [ 1, 4, 5, 7, 4, 5, 1, 1, 1, 5 ] )}, 
  {Transformation( [ 8, 1, 7, 3, 1, 7, 8, 8, 8, 7 ] )}, 
  {Transformation( [ 5, 3, 1, 2, 3, 1, 5, 5, 5, 1 ] )}, 
  {Transformation( [ 5, 9, 6, 2, 9, 6, 5, 5, 5, 6 ] )}, 
  {Transformation( [ 10, 2, 5, 3, 2, 5, 10, 10, 10, 5 ] )}, 
  {Transformation( [ 9, 10, 2, 5, 10, 2, 9, 9, 9, 2 ] )}, 
  {Transformation( [ 2, 1, 9, 6, 1, 9, 2, 2, 2, 9 ] )}, 
  {Transformation( [ 7, 1, 2, 8, 1, 2, 7, 7, 7, 2 ] )}, 
  {Transformation( [ 9, 2, 10, 4, 2, 10, 9, 9, 9, 10 ] )}, 
  {Transformation( [ 9, 10, 6, 5, 10, 6, 9, 9, 9, 6 ] )}, 
  {Transformation( [ 3, 1, 7, 4, 1, 7, 3, 3, 3, 7 ] )}, 
  {Transformation( [ 4, 10, 9, 6, 10, 9, 4, 4, 4, 9 ] )}, 
  {Transformation( [ 3, 10, 6, 4, 10, 6, 3, 3, 3, 6 ] )}, 
  {Transformation( [ 4, 1, 9, 6, 1, 9, 4, 4, 4, 9 ] )}, 
  {Transformation( [ 4, 3, 9, 6, 3, 9, 4, 4, 4, 9 ] )}, 
  {Transformation( [ 9, 1, 4, 5, 1, 4, 9, 9, 9, 4 ] )}, 
  {Transformation( [ 3, 10, 9, 7, 10, 9, 3, 3, 3, 9 ] )}, 
  {Transformation( [ 7, 3, 4, 9, 3, 4, 7, 7, 7, 4 ] )}, 
  {Transformation( [ 1, 2, 10, 4, 2, 10, 1, 1, 1, 10 ] )}, 
  {Transformation( [ 3, 10, 8, 4, 10, 8, 3, 3, 3, 8 ] )}, 
  {Transformation( [ 1, 5, 10, 4, 5, 10, 1, 1, 1, 10 ] )}, 
  {Transformation( [ 3, 4, 8, 7, 4, 8, 3, 3, 3, 8 ] )}, 
  {Transformation( [ 3, 5, 2, 9, 5, 2, 3, 3, 3, 2 ] )}, 
  {Transformation( [ 2, 1, 5, 6, 1, 5, 2, 2, 2, 5 ] )}, 
  {Transformation( [ 1, 5, 6, 4, 5, 6, 1, 1, 1, 6 ] )} ]
gap> List(last, x-> Representative(x) in s);
[ true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true ]
gap> ForAll(last2, x-> Representative(x) in r);
true
gap> Semigroup(gens);;
gap> r:=GreensRClassOfElement(s, f);
{Transformation( [ 9, 9, 9, 9, 9, 9, 1, 1, 9, 9 ] )}
gap> f:=Transformation( [ 9, 9, 5, 9, 5, 9, 5, 5, 5, 5 ] );;
gap> r:=GreensRClassOfElement(s, f);
{Transformation( [ 1, 1, 9, 1, 9, 1, 9, 9, 9, 9 ] )}
gap> Size(r);
86
gap> NrHClasses(r);
43
gap> s:=Semigroup(gens);;
gap> r:=GreensRClassOfElement(s, f);
{Transformation( [ 1, 1, 9, 1, 9, 1, 9, 9, 9, 9 ] )}
gap> GreensHClasses(r);
[ {Transformation( [ 1, 1, 9, 1, 9, 1, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 2, 2, 8, 2, 8, 2, 8, 8, 8, 8 ] )}, 
  {Transformation( [ 10, 10, 9, 10, 9, 10, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 4, 4, 10, 4, 10, 4, 10, 10, 10, 10 ] )}, 
  {Transformation( [ 4, 4, 8, 4, 8, 4, 8, 8, 8, 8 ] )}, 
  {Transformation( [ 10, 10, 3, 10, 3, 10, 3, 3, 3, 3 ] )}, 
  {Transformation( [ 3, 3, 1, 3, 1, 3, 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 2, 9, 2, 9, 2, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 3, 3, 8, 3, 8, 3, 8, 8, 8, 8 ] )}, 
  {Transformation( [ 3, 3, 4, 3, 4, 3, 4, 4, 4, 4 ] )}, 
  {Transformation( [ 6, 6, 3, 6, 3, 6, 3, 3, 3, 3 ] )}, 
  {Transformation( [ 8, 8, 7, 8, 7, 8, 7, 7, 7, 7 ] )}, 
  {Transformation( [ 1, 1, 8, 1, 8, 1, 8, 8, 8, 8 ] )}, 
  {Transformation( [ 5, 5, 9, 5, 9, 5, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 10, 10, 2, 10, 2, 10, 2, 2, 2, 2 ] )}, 
  {Transformation( [ 5, 5, 1, 5, 1, 5, 1, 1, 1, 1 ] )}, 
  {Transformation( [ 5, 5, 8, 5, 8, 5, 8, 8, 8, 8 ] )}, 
  {Transformation( [ 5, 5, 2, 5, 2, 5, 2, 2, 2, 2 ] )}, 
  {Transformation( [ 10, 10, 7, 10, 7, 10, 7, 7, 7, 7 ] )}, 
  {Transformation( [ 8, 8, 9, 8, 9, 8, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 10, 10, 6, 10, 6, 10, 6, 6, 6, 6 ] )}, 
  {Transformation( [ 10, 10, 8, 10, 8, 10, 8, 8, 8, 8 ] )}, 
  {Transformation( [ 4, 4, 1, 4, 1, 4, 1, 1, 1, 1 ] )}, 
  {Transformation( [ 5, 5, 4, 5, 4, 5, 4, 4, 4, 4 ] )}, 
  {Transformation( [ 3, 3, 5, 3, 5, 3, 5, 5, 5, 5 ] )}, 
  {Transformation( [ 5, 5, 6, 5, 6, 5, 6, 6, 6, 6 ] )}, 
  {Transformation( [ 2, 2, 1, 2, 1, 2, 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 2, 4, 2, 4, 2, 4, 4, 4, 4 ] )}, 
  {Transformation( [ 3, 3, 9, 3, 9, 3, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 4, 4, 7, 4, 7, 4, 7, 7, 7, 7 ] )}, 
  {Transformation( [ 6, 6, 1, 6, 1, 6, 1, 1, 1, 1 ] )}, 
  {Transformation( [ 3, 3, 7, 3, 7, 3, 7, 7, 7, 7 ] )}, 
  {Transformation( [ 6, 6, 4, 6, 4, 6, 4, 4, 4, 4 ] )}, 
  {Transformation( [ 3, 3, 2, 3, 2, 3, 2, 2, 2, 2 ] )}, 
  {Transformation( [ 6, 6, 9, 6, 9, 6, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 10, 10, 1, 10, 1, 10, 1, 1, 1, 1 ] )}, 
  {Transformation( [ 1, 1, 7, 1, 7, 1, 7, 7, 7, 7 ] )}, 
  {Transformation( [ 7, 7, 9, 7, 9, 7, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 6, 6, 2, 6, 2, 6, 2, 2, 2, 2 ] )}, 
  {Transformation( [ 10, 10, 5, 10, 5, 10, 5, 5, 5, 5 ] )}, 
  {Transformation( [ 5, 5, 7, 5, 7, 5, 7, 7, 7, 7 ] )}, 
  {Transformation( [ 4, 4, 9, 4, 9, 4, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 2, 2, 7, 2, 7, 2, 7, 7, 7, 7 ] )} ]
gap> Length(last);
43
gap> ForAll(last2, x-> Representative(x) in r);
true
gap> ForAll(last3, x-> Representative(x) in s);
true
gap> h:=Random(GreensHClasses(r));;
gap> f:=Representative(h);;
gap> hh:=HClass(r, f);;
gap> hh=h;
true
gap> h=hh;
true
gap> Elements(h)=Elements(hh);
true
gap> f:=Transformation( [ 10, 1, 9, 10, 2, 1, 5, 3, 2, 3 ] );;
gap> r:=GreensRClassOfElement(s, f);
{Transformation( [ 10, 1, 9, 10, 2, 1, 5, 3, 2, 3 ] )}
gap> Size(r);
1
gap> f:=Transformation( [ 10, 10, 3, 10, 10, 10, 10, 10, 6, 10 ] );;
gap> r:=GreensRClassOfElement(s, f);
{Transformation( [ 5, 5, 2, 5, 5, 5, 5, 5, 9, 5 ] )}
gap> Size(r);
546
gap> f:=Transformation( [ 6, 6, 4, 6, 6, 6, 6, 6, 3, 6 ] );;
gap> f in r;
true
gap> h:=HClass(r, f);
{Transformation( [ 6, 6, 4, 6, 6, 6, 6, 6, 3, 6 ] )}
gap> f in h;
true
gap> ForAll(h, x-> x in r);
true
gap> Size(h);
6
gap> Elements(h);
[ Transformation( [ 3, 3, 4, 3, 3, 3, 3, 3, 6, 3 ] ), 
  Transformation( [ 3, 3, 6, 3, 3, 3, 3, 3, 4, 3 ] ), 
  Transformation( [ 4, 4, 3, 4, 4, 4, 4, 4, 6, 4 ] ), 
  Transformation( [ 4, 4, 6, 4, 4, 4, 4, 4, 3, 4 ] ), 
  Transformation( [ 6, 6, 3, 6, 6, 6, 6, 6, 4, 6 ] ), 
  Transformation( [ 6, 6, 4, 6, 6, 6, 6, 6, 3, 6 ] ) ]

#
gap> gens:=
> [ PartialPermNC( [ 1, 2, 3, 5, 9, 10 ], [ 5, 10, 7, 8, 9, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6, 9 ], [ 9, 3, 1, 4, 2, 5, 6 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 7, 9 ], [ 7, 6, 2, 8, 4, 5, 3 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], 
> [ 8, 7, 4, 3, 10, 9, 5, 6, 1, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
1422787
gap> f:=PartialPermNC([ 1, 4, 7, 9, 10 ], [ 5, 10, 9, 8, 7 ]);;
gap> r:=GreensRClassOfElementNC(s, f);
{PartialPerm( [ 1, 4, 7, 9, 10 ], [ 5, 10, 9, 8, 7 ] )}
gap> Size(r);
4
gap> f in r;
true
gap> f:=PartialPermNC([ 1, 7, 8, 9 ], [ 10, 9, 6, 5 ]);;
gap> r:=GreensRClassOfElementNC(s, f);
{PartialPerm( [ 1, 7, 8, 9 ], [ 5, 6, 8, 7 ] )}
gap> Size(r);
4
gap> iter:=IteratorOfRClasses(s);
<iterator of R-classes>
gap> repeat r:=NextIterator(iter); until Size(r)>1000;
gap> r;
{PartialPerm( [ 1, 3, 9, 10 ], [ 4, 5, 3, 7 ] )}
gap> Size(r);
3792
gap> r:=RClassNC(s, Representative(r));
{PartialPerm( [ 1, 3, 9, 10 ], [ 4, 5, 3, 7 ] )}
gap> h:=HClassNC(r, Random(r));;
gap> Size(h);
24
gap> ForAll(h, x-> x in r);
true
gap> IsRegularClass(r);
true
gap> IsRegularSemigroup(s);
false
gap> NrIdempotents(r);
1
gap> Idempotents(r);
[ <identity partial perm on [ 1, 3, 9, 10 ]> ]
gap> ForAll(last, x-> x in r);
true

#
gap> gens:=[ Transformation( [ 1, 3, 7, 9, 1, 12, 13, 1, 15, 9, 1, 18, 1, 1,
>  13, 1, 1, 21, 1, 1, 1, 1, 1, 25, 26, 1 ] ),
>  Transformation( [ 1, 5, 1, 5, 11, 1, 1, 14, 1, 16, 17, 1, 1, 19, 1, 11, 1,
>       1, 1, 23, 1, 16, 19, 1, 1, 1 ] ),
> Transformation( [ 1, 4, 8, 1, 10, 1, 8, 1, 1, 1, 10, 1, 8, 10, 1, 1, 20, 1,
>       22, 1, 8, 1, 1, 1, 1, 1 ] ),
> Transformation( [ 1, 6, 6, 1, 1, 1, 6, 1, 1, 1, 1, 1, 6, 1, 6, 1, 1, 6, 1,
>       1, 24, 1, 1, 1, 1, 6 ] ) ];;
gap> s:=Semigroup(gens);;
gap> First(DClasses(s), IsRegularDClass);
{Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
 1, 1, 1, 1, 1, 1 ] )}
gap> NrDClasses(s);
31
gap> PositionsProperty(DClasses(s), IsRegularDClass);
[ 6, 7 ]
gap> d:=DClasses(s)[7];
{Transformation( [ 1, 8, 1, 1, 8, 1, 1, 1, 1, 1, 8, 1, 1, 8, 1, 1, 1, 1, 1, 1,
 1, 1, 1, 1, 1, 1 ] )}
gap> r:=RClassNC(s, Representative(d));
{Transformation( [ 1, 8, 1, 1, 8, 1, 1, 1, 1, 1, 8, 1, 1, 8, 1, 1, 1, 1, 1, 1,
 1, 1, 1, 1, 1, 1 ] )}
gap> Size(r);
20
gap> ForAll(Idempotents(r), x-> x in s);
true
gap> ForAll(Idempotents(r), x-> x in r);
true
gap> ForAll(Idempotents(r), x-> x in d);
true
gap> ForAll(r, x-> x in d);
true
gap> Number(GreensRClasses(s), IsRegularClass);
21
gap> NrRegularDClasses(s);
2

#
gap> gens:=[ Transformation( [ 1, 2, 3, 5, 4, 6, 7, 8 ] ),
>   Transformation( [ 4, 4, 3, 1, 5, 6, 3, 8 ] ),
>   Transformation( [ 3, 6, 1, 7, 3, 4, 8, 3 ] ),
>   Transformation( [ 1, 2, 3, 4, 5, 3, 7, 8 ] ),
>   Transformation( [ 1, 2, 3, 4, 1, 6, 7, 8 ] ),
>   Transformation( [ 8, 8, 3, 4, 5, 7, 6, 1 ] ) ];;
gap> s:=Monoid(gens);
<transformation monoid on 8 pts with 6 generators>
gap> f:=Transformation( [ 4, 4, 3, 8, 5, 3, 3, 1 ] );;
gap> Size(s);
998
gap> r:=RClass(s, f);
{Transformation( [ 4, 4, 3, 1, 5, 3, 3 ] )}
gap> IsRegularClass(r);
true
gap> Idempotents(r);
[ Transformation( [ 1, 1, 3, 4, 5, 3, 3 ] ) ]
gap> IsRegularSemigroup(s);
false
gap> ForAll(r, x-> x in s);
true
gap> iter:=Iterator(r);
<iterator of R-class>
gap> for i in iter do od;
gap> Size(r);
24
gap> IsDoneIterator(iter);
true
gap> iter:=Iterator(r);
<iterator of R-class>
gap> for i in [1..23] do NextIterator(iter); od;
gap> IsDoneIterator(iter);
false
gap> NextIterator(iter);
Transformation( [ 5, 5, 3, 8, 4, 3, 3, 1 ] )
gap> IsDoneIterator(iter);
true
gap> Transformation( [ 4, 4, 3, 8, 1, 3, 3, 5 ] ) in r;
true
gap> r;
{Transformation( [ 4, 4, 3, 1, 5, 3, 3 ] )}
gap> NrIdempotents(r);
1

#
gap> gens:=[ PartialPermNC( [ 1, 2, 3, 4, 7 ], [ 8, 3, 5, 7, 4 ] ),
>  PartialPermNC( [ 1, 2, 5, 6, 7 ], [ 4, 1, 6, 2, 8 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6 ], [ 3, 7, 1, 5, 2, 6 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6 ], [ 7, 2, 5, 6, 3, 8 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 6, 7 ], [ 4, 5, 6, 1, 2, 7 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 6, 7 ], [ 5, 1, 7, 2, 8, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
9954
gap> f:=PartialPerm([ 2, 3, 6 ], [ 1, 4, 8 ]);;
gap> r:=RClass(s, f);
{PartialPerm( [ 2, 3, 6 ], [ 1, 4, 8 ] )}
gap> NrIdempotents(r);
0
gap> Sum(List(RClasses(s), NrIdempotents));
53
gap> NrIdempotents(s);
53
gap> gens:=[ Transformation( [ 1, 2, 4, 3, 6, 5 ] ), 
> Transformation( [ 1, 2, 3, 4,   5, 6 ] ),
>    Transformation( [ 6, 4, 3, 2, 5, 3 ] ), 
> Transformation( [ 5, 3, 4, 2, 2, 1 ]  ),
>    Transformation( [ 2, 4, 6, 4, 5, 3 ] ), 
> Transformation( [ 4, 2, 4, 3, 6, 5 ]  ),
>    Transformation( [ 2, 4, 4, 3, 6, 5 ] ), 
> Transformation( [ 5, 6, 4, 4, 3, 2 ]  ),
>    Transformation( [ 2, 2, 3, 4, 5, 6 ] ), 
> Transformation( [ 3, 4, 2, 2, 2, 1 ]  ),
>    Transformation( [ 1, 2, 4, 2, 3, 3 ] ), 
> Transformation( [ 1, 2, 3, 4, 3, 2 ]  ),
>    Transformation( [ 6, 4, 2, 3, 2, 3 ] ), 
> Transformation( [ 6, 4, 2, 2, 1, 1 ]  ),
>    Transformation( [ 6, 4, 2, 3, 4, 4 ] ), 
> Transformation( [ 5, 3, 3, 2, 4, 2 ]  ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
1888
gap> f:=Transformation( [ 2, 4, 6, 6, 5, 6 ] );;
gap> r:=RClass(s, f);
{Transformation( [ 2, 4, 6, 6, 5, 6 ] )}
gap> h:=HClassNC(s, f);
{Transformation( [ 2, 4, 6, 6, 5, 6 ] )}
gap> hh:=HClass(r, f);
{Transformation( [ 2, 4, 6, 6, 5, 6 ] )}
gap> hh=h;
true
gap> ForAll(h, x-> x in r);
true
gap> ForAll(hh, x-> x in r);
true
gap> RClassOfHClass(h)=r;
true
gap> RClassOfHClass(hh)=r;
true
gap> r=RClassOfHClass(hh);
true
gap> Size(r);
2
gap> HClassReps(r);
[ Transformation( [ 2, 4, 6, 6, 5, 6 ] ), 
  Transformation( [ 2, 3, 5, 5, 6, 5 ] ) ]
gap> ForAll(last, x-> x in r);
true
gap> ForAll(last2, x-> x in s);
true

#
gap> gens:=
> [ PartialPermNC( [ 1, 2, 3 ], [ 2, 3, 4 ] ),
>  PartialPermNC( [ 1, 2, 3 ], [ 3, 6, 1 ] ),
>  PartialPermNC( [ 1, 2, 3 ], [ 6, 2, 1 ] ),
>  PartialPermNC( [ 1, 2, 4 ], [ 4, 2, 6 ] ),
>  PartialPermNC( [ 1, 3, 5 ], [ 2, 6, 3 ] ),
>  PartialPermNC( [ 1, 4, 5 ], [ 1, 6, 3 ] ),
>  PartialPermNC( [ 1, 2, 3, 5 ], [ 2, 3, 5, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 5 ], [ 3, 2, 4, 6 ] ),
>  PartialPermNC( [ 1, 2, 4, 6 ], [ 4, 3, 1, 6 ] ),
>  PartialPermNC( [ 1, 3, 5, 6 ], [ 1, 4, 6, 2 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5 ], [ 5, 4, 6, 2, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5 ], [ 6, 2, 3, 5, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5 ], [ 6, 3, 5, 1, 2 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 6 ], [ 4, 1, 5, 2, 3 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 6 ], [ 5, 1, 6, 3, 2 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 6 ], [ 5, 4, 2, 6, 3 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
6741
gap> f:=PartialPermNC( [ 1, 3, 5, 6 ], [ 6, 2, 5, 1 ]);;
gap> r:=RClassNC(s, f);
{PartialPerm( [ 1, 3, 5, 6 ], [ 6, 2, 5, 1 ] )}
gap> HClassReps(r);
[ [3,2](1,6)(5) ]
gap> ForAll(last, x-> x in r);
true
gap> r:=RClass(s, f);
{PartialPerm( [ 1, 3, 5, 6 ], [ 6, 2, 5, 1 ] )}
gap> HClassReps(r);
[ [3,2](1,6)(5) ]
gap> h:=HClass(s, last[1]);
{PartialPerm( [ 1, 3, 5, 6 ], [ 6, 2, 5, 1 ] )}
gap> r:=RClassOfHClass(h);
{PartialPerm( [ 1, 3, 5, 6 ], [ 6, 2, 5, 1 ] )}
gap> HClassReps(r);
[ [3,2](1,6)(5) ]
gap> iter:=IteratorOfRClasses(s);
<iterator of R-classes>
gap> iter:=IteratorOfRClasses(s);
<iterator of R-classes>
gap> repeat r:=NextIterator(iter); until Size(r)>1;
gap> r;
{PartialPerm( [ 1, 2, 3 ], [ 2, 3, 4 ] )}
gap> Size(r);
114
gap> HClassReps(r);
[ [1,2,3,4], [2,3,6](1), [3,6](1)(2), [1,2,4][3,6], [1,3,2,6], [1,5](2)(3), 
  [2,5](1,3), [3,2,1,5], [3,4](1,2), [1,3,5][2,4], [1,2,6][3,5], 
  [1,4][2,3,6], [2,3,4](1), <identity partial perm on [ 1, 2, 3 ]>, 
  [2,6][3,1,4], [2,5][3,4](1), [2,1,5][3,6], [1,6][2,5](3), [1,4][3,2,5] ]
gap> Size(DClass(r));
2166
gap> d:=DClass(r);
{PartialPerm( [ 1, 2, 3 ], [ 2, 3, 4 ] )}
gap> ForAll(r, x-> x in d);
true
gap> Number(d, x-> x in r);
114
gap> Size(r);
114
gap> ForAll(HClassReps(r), x-> x in d);
true
gap> ForAll(HClassReps(r), x-> x in HClassReps(d));
true

#
gap> gens:=[ Transformation( [ 6, 4, 3, 2, 5, 1 ] ),
>   Transformation( [ 1, 2, 3, 4, 5, 6 ] ),
>   Transformation( [ 5, 3, 3, 2, 4, 1 ] ),
>   Transformation( [ 1, 3, 3, 4, 5, 2 ] ),
>   Transformation( [ 4, 5, 2, 3, 3, 1 ] ),
>   Transformation( [ 6, 4, 3, 5, 2, 3 ] ),
>   Transformation( [ 5, 2, 3, 4, 3, 6 ] ),
>   Transformation( [ 1, 3, 2, 5, 4, 5 ] ),
>   Transformation( [ 4, 3, 2, 2, 1, 5 ] ),
>   Transformation( [ 1, 3, 3, 5, 2, 4 ] ),
>   Transformation( [ 6, 3, 3, 2, 1, 5 ] ),
>   Transformation( [ 6, 3, 4, 5, 2, 2 ] ),
>   Transformation( [ 6, 4, 3, 2, 2, 5 ] ),
>   Transformation( [ 1, 3, 2, 3, 5, 4 ] ),
>   Transformation( [ 1, 2, 3, 4, 5, 2 ] ),
>   Transformation( [ 2, 4, 3, 4, 6, 5 ] ),
>   Transformation( [ 2, 4, 3, 3, 6, 1 ] ),
>   Transformation( [ 6, 4, 3, 2, 3, 1 ] ),
>   Transformation( [ 6, 4, 3, 2, 2, 1 ] ) ];;
gap> s:=Semigroup(gens);
<transformation monoid on 6 pts with 18 generators>
gap> Size(s);
7008
gap> NrRClasses(s);
310
gap> IsRegularSemigroup(s);
false
gap> f:=Transformation( [ 3, 2, 3, 4, 3, 5 ] );;
gap> r:=RClassNC(s, f);
{Transformation( [ 3, 5, 3, 4, 3, 2 ] )}
gap> d:=DClassOfRClass(r);
{Transformation( [ 2, 4, 3, 5, 3, 3 ] )}
gap> Size(d);
792
gap> IsRegularDClass(d);
false
gap> NrIdempotents(d);
0
gap> Idempotents(d);
[  ]
gap> HClassReps(d);;
gap> Length(last);
198
gap> Number(HClassReps(d), x-> x in r);
6
gap> NrHClasses(r);
6
gap> enum:=EnumeratorOfRClasses(s);
<enumerator of R-classes of <non-regular transformation monoid of size 7008, 
 on 6 pts with 18 generators>>
gap> enum[1];
{Transformation( [ 6, 4, 3, 2, 5, 1 ] )}
gap> s:=Semigroup(gens);
<transformation monoid on 6 pts with 18 generators>
gap> enum:=EnumeratorOfRClasses(s);
<enumerator of R-classes of <transformation monoid on 6 pts with 18 generators
  >>
gap> enum[1];
{Transformation( [ 6, 4, 3, 2, 5, 1 ] )}
gap> enum[2];
{Transformation( [ 5, 3, 3, 2, 4, 1 ] )}
gap> Position(enum, enum[10]);
10
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
true
gap> ForAll([1..Length(enum)], x-> Position(enum, enum[x])=x);
true
gap> NrHClasses(r);
6

#
gap> gens:=[ PartialPermNC( [ 1, 2, 4 ], [ 2, 5, 3 ] ),
>  PartialPermNC( [ 1, 2, 4 ], [ 5, 6, 1 ] ),
>  PartialPermNC( [ 1, 2, 5 ], [ 5, 3, 2 ] ),
>  PartialPermNC( [ 1, 2, 3, 4 ], [ 5, 1, 2, 4 ] ),
>  PartialPermNC( [ 1, 2, 3, 4 ], [ 5, 1, 2, 6 ] ),
>  PartialPermNC( [ 1, 2, 3, 4 ], [ 5, 6, 4, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 5 ], [ 1, 5, 2, 6 ] ),
>  PartialPermNC( [ 1, 2, 3, 5 ], [ 2, 3, 4, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 5 ], [ 2, 5, 4, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 5 ], [ 5, 1, 2, 3 ] ),
>  PartialPermNC( [ 1, 2, 3, 6 ], [ 1, 4, 6, 5 ] ),
>  PartialPermNC( [ 1, 2, 5, 6 ], [ 6, 4, 2, 5 ] ),
>  PartialPermNC( [ 1, 3, 4, 6 ], [ 2, 3, 1, 6 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5 ], [ 3, 6, 5, 2, 4 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5 ], [ 6, 5, 3, 2, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 6 ], [ 1, 3, 4, 6, 2 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 6 ], [ 1, 3, 6, 4, 5 ] ),
>  PartialPermNC( [ 1, 2, 4, 5, 6 ], [ 5, 4, 2, 1, 6 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6 ], [ 2, 5, 6, 4, 3, 1 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
12612
gap> f:=PartialPermNC([ 1, 4, 6 ], [ 2, 3, 6 ]);;
gap> r:=RClass(s, f);
{PartialPerm( [ 1, 4, 6 ], [ 5, 3, 2 ] )}
gap> Size(r);
120
gap> NrHClasses(r);
20
gap> Number(HClassReps(s), x-> x in r);
20

# H-class tests
gap> gens:=[ Transformation( [ 8, 7, 6, 5, 4, 3, 2, 1 ] ),
>   Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8 ] ),
>   Transformation( [ 7, 6, 5, 4, 3, 2, 1, 2 ] ),
>   Transformation( [ 3, 2, 1, 2, 3, 4, 5, 6 ] ),
>   Transformation( [ 2, 3, 4, 5, 4, 5, 6, 7 ] ),
>   Transformation( [ 1, 2, 3, 4, 5, 4, 5, 6 ] ),
>   Transformation( [ 5, 6, 5, 4, 5, 4, 3, 2 ] ),
>   Transformation( [ 5, 6, 7, 8, 7, 6, 5, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> f:=Transformation( [ 5, 6, 5, 4, 5, 4, 5, 4 ] );;
gap> h:=HClass(s, f);
{Transformation( [ 5, 6, 5, 4, 5, 4, 5, 4 ] )}
gap> ForAll(h, x-> x in h);
true
gap> h:=HClassNC(s, f);
{Transformation( [ 5, 6, 5, 4, 5, 4, 5, 4 ] )}
gap> Enumerator(h);
<enumerator of H-class>
gap> h:=HClassNC(s, f);
{Transformation( [ 5, 6, 5, 4, 5, 4, 5, 4 ] )}
gap> SchutzenbergerGroup(h);
Group([ (4,6) ])

#
gap> s:=FullTransformationSemigroup(7);
<full transformation semigroup on 7 pts>
gap> Factorial(7);
5040
gap> f:=One(s);
IdentityTransformation
gap> h:=HClassNC(s, f);
{IdentityTransformation}
gap> enum:=Enumerator(h);
<enumerator of H-class>
gap> ForAll(enum, x-> x in h);
true
gap> ForAll(enum, x-> x in s);
true
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
true
gap> ForAll([1..Length(enum)], x-> Position(enum, enum[x])=x);
true
gap> Idempotents(h);
[ IdentityTransformation ]
gap> f:=Transformation( [ 3, 2, 4, 5, 6, 1, 1 ] );;
gap> h:=HClassNC(s, f);
{Transformation( [ 3, 2, 4, 5, 6, 1, 1 ] )}
gap> Idempotents(h);
[ Transformation( [ 1, 2, 3, 4, 5, 6, 6 ] ) ]
gap> IsGroupHClass(h);
true
gap> h:=HClass(s, Transformation( [ 5, 1, 3, 3, 5, 5, 3 ] ));;
gap> IsGroupHClass(h);
false
gap> IsRegularClass(h);
false

#
gap> gens:=
> [ PartialPermNC( [ 1, 2, 3 ], [ 1, 5, 2 ] ),
>  PartialPermNC( [ 1, 2, 4 ], [ 1, 3, 6 ] ),
>  PartialPermNC( [ 1, 2, 4 ], [ 3, 1, 6 ] ),
>  PartialPermNC( [ 1, 2, 6 ], [ 6, 4, 1 ] ),
>  PartialPermNC( [ 1, 3, 5 ], [ 5, 2, 3 ] ),
>  PartialPermNC( [ 1, 2, 3, 4 ], [ 5, 3, 2, 4 ] ),
>  PartialPermNC( [ 1, 2, 3, 4 ], [ 6, 1, 5, 3 ] ),
>  PartialPermNC( [ 1, 2, 3, 5 ], [ 1, 4, 6, 3 ] ),
>  PartialPermNC( [ 1, 2, 3, 5 ], [ 2, 3, 4, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 5 ], [ 6, 5, 1, 2 ] ),
>  PartialPermNC( [ 1, 2, 3, 6 ], [ 3, 5, 4, 6 ] ),
>  PartialPermNC( [ 1, 2, 4, 5 ], [ 4, 2, 3, 6 ] ),
>  PartialPermNC( [ 1, 2, 4, 6 ], [ 6, 4, 3, 5 ] ),
>  PartialPermNC( [ 1, 2, 4, 6 ], [ 6, 4, 5, 2 ] ),
>  PartialPermNC( [ 1, 3, 4, 5 ], [ 6, 1, 4, 3 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5 ], [ 3, 4, 1, 2, 6 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 6 ], [ 1, 2, 5, 3, 4 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 6 ], [ 3, 6, 4, 5, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 6 ], [ 4, 3, 5, 1, 6 ] ),
>  PartialPermNC( [ 1, 2, 4, 5, 6 ], [ 2, 3, 1, 5, 6 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
7960
gap> f:=PartialPermNC([ 1, 2, 5, 6 ], [ 5, 3, 6, 4 ]);;
gap> h:=HClass(s, f);
{PartialPerm( [ 1, 2, 5, 6 ], [ 5, 3, 6, 4 ] )}
gap> d:=DClass(s,f);
{PartialPerm( [ 1, 2, 3, 4 ], [ 6, 3, 5, 4 ] )}
gap> h:=HClass(s, f);
{PartialPerm( [ 1, 2, 5, 6 ], [ 5, 3, 6, 4 ] )}
gap> IsGroupHClass(h);
false
gap> Size(h);
1
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until Size(h)>1;
gap> h;
{PartialPerm( [ 1, 2, 3 ], [ 1, 5, 2 ] )}
gap> Size(h);
6
gap> enum:=Enumerator(h);
<enumerator of H-class>
gap> ForAll([1..Length(enum)], x-> Position(enum, enum[x])=x);
true
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
true
gap> d:=DClass(s,Representative(h));
{PartialPerm( [ 1, 2, 3 ], [ 1, 5, 2 ] )}
gap> f:=Representative(h);
[3,2,5](1)
gap> h:=HClass(d, f);
{PartialPerm( [ 1, 2, 3 ], [ 1, 5, 2 ] )}
gap> h=HClass(s, f);
true
gap> Idempotents(h);
[  ]
gap> repeat h:=NextIterator(iter); until Size(h)>1;
gap> h;
{PartialPerm( [ 1, 2, 3 ], [ 6, 3, 1 ] )}
gap> Size(h);
6
gap> f:=Representative(h);
[2,3,1,6]
gap> r:=RClassNC(d, f);
{PartialPerm( [ 1, 2, 3 ], [ 1, 5, 2 ] )}
gap> h:=HClass(r, f);
{PartialPerm( [ 1, 2, 3 ], [ 6, 3, 1 ] )}
gap> h=HClass(s, f);
true
gap> Elements(h)=Elements(HClass(s, f));
true
gap> l:=LClass(s, f);
{PartialPerm( [ 1, 2, 3 ], [ 6, 3, 1 ] )}
gap> h:=HClass(l, f);
{PartialPerm( [ 1, 2, 3 ], [ 6, 3, 1 ] )}
gap> Elements(h)=Elements(HClass(s, f));
true
gap> h:=HClass(l, f);
{PartialPerm( [ 1, 2, 3 ], [ 6, 3, 1 ] )}

#
gap> gens:=[ Transformation( [ 6, 7, 1, 2, 3, 4, 5 ] ),
>   Transformation( [ 7, 6, 5, 4, 3, 2, 1 ] ),
>   Transformation( [ 4, 5, 6, 7, 1, 2, 3 ] ),
>   Transformation( [ 5, 6, 6, 5, 4, 3, 4 ] ),
>   Transformation( [ 5, 4, 3, 2, 3, 3, 4 ] ),
>   Transformation( [ 5, 4, 3, 3, 4, 4, 4 ] ),
>   Transformation( [ 1, 7, 1, 1, 1, 1, 2 ] ),
>   Transformation( [ 5, 6, 6, 5, 4, 4, 5 ] ) ];;
gap> s:=Semigroup(gens);;
gap> First(HClasses(s), IsRegularClass);
{Transformation( [ 6, 7, 1, 2, 3, 4, 5 ] )}
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until IsRegularClass(h);
gap> h;
{Transformation( [ 6, 7, 1, 2, 3, 4, 5 ] )}
gap> repeat h:=NextIterator(iter); until IsRegularClass(h);
gap> h;
{Transformation( [ 5, 6, 6, 5, 4, 3, 4 ] )}
gap> IsGroupHClass(h);
true
gap> IsomorphismPermGroup(h);
MappingByFunction( {Transformation( [ 5, 6, 6, 5, 4, 3, 4 ] )}, Group([ (3,6)
(4,5) ]), function( x ) ... end, function( x ) ... end )
gap> SchutzenbergerGroup(h);
Group([ (3,6)(4,5) ])
gap> repeat h:=NextIterator(iter); until IsRegularClass(h);
gap> h;
{Transformation( [ 1, 2, 2, 1, 7, 6, 7 ] )}
gap> IsGroupHClass(h);
true
gap> h:=GroupHClass(DClass(h));
{Transformation( [ 4, 3, 3, 4, 5, 6, 5 ] )}
gap> IsGroupHClass(h);
true
gap> iso:=IsomorphismPermGroup(h);; inv:=InverseGeneralMapping(iso);;
gap> ForAll(h, x-> (x^iso)^inv=x);
true
gap> First(h, x-> (x^iso)^inv<>x);
fail
gap> One(h);
IdentityTransformation
gap> repeat h:=NextIterator(iter); until IsRegularClass(h);
gap> h;
{Transformation( [ 4, 3, 2, 1, 2, 2, 3 ] )}
gap> IsGroupHClass(h);
true
gap> h:=GroupHClass(DClass(h));
{Transformation( [ 4, 3, 3, 4, 5, 6, 5 ] )}
gap> IsGroupHClass(h);
true
gap> iso:=IsomorphismPermGroup(h); inv:=InverseGeneralMapping(iso);
MappingByFunction( {Transformation( [ 4, 3, 3, 4, 5, 6, 5 ] )}, Group([ (3,6)
(4,5) ]), function( x ) ... end, function( x ) ... end )
MappingByFunction( Group([ (3,6)
(4,5) ]), {Transformation( [ 4, 3, 3, 4, 5, 6, 5 ] )
 }, function( x ) ... end, function( x ) ... end )
gap> ForAll(h, x-> (x^iso)^inv=x);
true

#
gap> gens:=
> [ PartialPermNC( [ 1, 2, 3 ], [ 1, 5, 3 ] ),
>  PartialPermNC( [ 1, 2, 3 ], [ 3, 2, 4 ] ),
>  PartialPermNC( [ 1, 2, 4 ], [ 4, 6, 3 ] ),
>  PartialPermNC( [ 1, 2, 4 ], [ 5, 2, 3 ] ),
>  PartialPermNC( [ 1, 2, 5 ], [ 5, 4, 6 ] ),
>  PartialPermNC( [ 1, 3, 5 ], [ 1, 6, 3 ] ),
>  PartialPermNC( [ 1, 4, 5 ], [ 4, 6, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 4 ], [ 2, 4, 3, 6 ] ),
>  PartialPermNC( [ 1, 2, 3, 4 ], [ 4, 1, 2, 6 ] ),
>  PartialPermNC( [ 1, 2, 3, 4 ], [ 6, 1, 4, 3 ] ),
>  PartialPermNC( [ 1, 2, 3, 5 ], [ 4, 6, 5, 2 ] ),
>  PartialPermNC( [ 1, 2, 3, 5 ], [ 6, 4, 5, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 6 ], [ 1, 3, 4, 5 ] ),
>  PartialPermNC( [ 1, 2, 3, 6 ], [ 5, 1, 6, 3 ] ),
>  PartialPermNC( [ 1, 2, 3, 6 ], [ 5, 2, 1, 3 ] ),
>  PartialPermNC( [ 1, 2, 4, 6 ], [ 3, 4, 1, 6 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 6 ], [ 2, 4, 6, 1, 5 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 6 ], [ 5, 1, 6, 2, 4 ] ),
>  PartialPermNC( [ 1, 2, 4, 5, 6 ], [ 3, 5, 1, 6, 2 ] ),
>  PartialPermNC( [ 1, 3, 4, 5, 6 ], [ 4, 3, 6, 5, 1 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
3941
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until IsRegularClass(h);
gap> h;
{PartialPerm( [ 1, 2, 3 ], [ 1, 2, 3 ] )}
gap> IsGroupHClass(h);
true
gap> h:=GroupHClass(DClass(h));
{PartialPerm( [ 1, 2, 3 ], [ 1, 2, 3 ] )}
gap> iso:=IsomorphismPermGroup(h); inv:=InverseGeneralMapping(iso);
MappingByFunction( {PartialPerm( [ 1, 2, 3 ], [ 1, 2, 3 ] )}, Group([ (1,2),
 (2,3) ]), function( x ) ... end, function( x ) ... end )
MappingByFunction( Group([ (1,2), (2,3) ]), {PartialPerm(
 [ 1, 2, 3 ], [ 1, 2, 3 ] )}, function( x ) ... end, function( x ) ... end )
gap> ForAll(h, x-> (x^iso)^inv=x);
true
gap> repeat h:=NextIterator(iter); until Size(h)>1;
gap> IsGroupHClass(h);
false
gap> h:=GroupHClass(DClass(h));
{PartialPerm( [ 1, 2, 3 ], [ 1, 2, 3 ] )}
gap> Idempotents(h);
[ <identity partial perm on [ 1, 2, 3 ]> ]
gap> h:=GroupHClass(DClass(h));
{PartialPerm( [ 1, 2, 3 ], [ 1, 2, 3 ] )}
gap> One(h);
<identity partial perm on [ 1, 2, 3 ]>
gap> s:=Semigroup(Generators(s));;
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until Size(h)>1;
gap> repeat h:=NextIterator(iter); until Size(h)>1;
gap> repeat h:=NextIterator(iter); until Size(h)>1;
gap> h;
{PartialPerm( [ 1, 2, 3 ], [ 3, 6, 4 ] )}
gap> IsGroupHClass(h);
false
gap> h:=GroupHClass(DClass(h));
{PartialPerm( [ 1, 2, 3 ], [ 1, 2, 3 ] )}
gap> s:=Semigroup(Generators(s));;
gap> f:=PartialPerm([]);;
gap> h:=HClass(s, f);
{PartialPerm( [  ], [  ] )}
gap> One(h);
<empty partial perm>
gap> IsGroupHClass(h);
true
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until Size(h)>1;
gap> h;
{PartialPerm( [ 1, 2, 3 ], [ 1, 5, 3 ] )}
gap> IsGroupHClass(h);
false

#
gap> gens:=[ Transformation( [ 1, 2, 5, 4, 3, 8, 7, 6 ] ),
>   Transformation( [ 1, 6, 3, 4, 7, 2, 5, 8 ] ),
>   Transformation( [ 2, 1, 6, 7, 8, 3, 4, 5 ] ),
>   Transformation( [ 3, 2, 3, 6, 1, 6, 1, 2 ] ),
>   Transformation( [ 5, 2, 3, 6, 3, 4, 7, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
5304

#
gap> gens:=[ Transformation( [ 2, 1, 8, 7, 6, 5, 4, 3 ] ),
>   Transformation( [ 2, 8, 4, 6, 3, 5, 1, 7 ] ),
>   Transformation( [ 8, 7, 2, 1, 4, 3, 6, 5 ] ),
>   Transformation( [ 3, 5, 1, 7, 2, 8, 4, 6 ] ),
>   Transformation( [ 8, 6, 7, 5, 5, 3, 6, 4 ] ),
>   Transformation( [ 2, 1, 1, 3, 2, 4, 8, 2 ] ),
>   Transformation( [ 8, 7, 2, 1, 2, 1, 2, 1 ] ),
>   Transformation( [ 8, 2, 2, 1, 2, 2, 2, 4 ] ),
>   Transformation( [ 3, 4, 5, 3, 6, 5, 4, 6 ] ),
>   Transformation( [ 2, 8, 1, 7, 1, 7, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
15488
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until Size(h)>1;
gap> h;
{Transformation( [ 2, 1, 8, 7, 6, 5, 4, 3 ] )}
gap> repeat h:=NextIterator(iter); until Size(h)>1;
gap> h;
{Transformation( [ 8, 6, 7, 5, 5, 3, 6, 4 ] )}
gap> IsGroupHClass(h);
false
gap> h:=GroupHClass(DClass(h));
{Transformation( [ 1, 2, 3, 4, 4, 6, 2 ] )}
gap> One(h);
IdentityTransformation
gap> IsGroupHClass(h);
true
gap> KnownPropertiesOfObject(h);
[ "IsFinite", "IsDuplicateFree", "IsGroupHClass", "IsGreensClassNC" ]
gap> KnownAttributesOfObject(h);
[ "Representative", "OneImmutable", "ParentAttr", "EquivalenceClassRelation", 
  "LambdaOrbSCCIndex", "RhoOrbSCCIndex", "LambdaOrb", "RhoOrb" ]
gap> iso:=IsomorphismPermGroup(h);; inv:=InverseGeneralMapping(iso);;
gap> ForAll(h, x-> (x^iso)^inv=x);
true
gap> ForAll(Image(iso), x-> (x^inv)^iso=x);
true
gap> Size(h);
4
gap> h:=HClass(s, Transformation( [ 6, 8, 4, 2, 3, 4, 4, 2 ] ));;
gap> NrIdempotents(h);
0

#
gap> gens:=
> [ PartialPermNC( [ 1, 2, 7 ], [ 5, 6, 4 ] ),
>  PartialPermNC( [ 1, 2, 3, 5 ], [ 6, 8, 4, 5 ] ),
>  PartialPermNC( [ 1, 2, 3, 7 ], [ 6, 2, 7, 4 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 8 ], [ 4, 5, 6, 7, 3 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 6 ], [ 3, 4, 1, 6, 5 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 7 ], [ 5, 4, 6, 8, 2 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 8 ], [ 3, 6, 2, 4, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 8 ], [ 7, 4, 3, 6, 8 ] ),
>  PartialPermNC( [ 1, 2, 3, 6, 7 ], [ 1, 6, 8, 5, 3 ] ),
>  PartialPermNC( [ 1, 2, 3, 7, 8 ], [ 1, 2, 4, 5, 8 ] ),
>  PartialPermNC( [ 1, 2, 4, 7, 8 ], [ 7, 1, 4, 5, 8 ] ),
>  PartialPermNC( [ 1, 3, 4, 5, 8 ], [ 6, 2, 5, 7, 8 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6 ], [ 2, 1, 5, 3, 7, 4 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 8 ], [ 6, 7, 3, 8, 2, 5 ] ),
>  PartialPermNC( [ 1, 2, 4, 5, 6, 8 ], [ 2, 1, 3, 6, 8, 5 ] ),
>  PartialPermNC( [ 1, 2, 4, 6, 7, 8 ], [ 3, 1, 4, 5, 2, 7 ] ),
>  PartialPermNC( [ 1, 3, 4, 5, 6, 8 ], [ 5, 2, 3, 1, 7, 8 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6, 8 ], [ 5, 1, 3, 7, 6, 2, 4 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 7, 8 ], [ 7, 3, 4, 8, 5, 2, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 6, 7, 8 ], [ 3, 8, 7, 4, 6, 1, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> NrRClasses(s);
38666
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until Size(h)>24;
gap> h;
{PartialPerm( [ 1, 2, 3, 5, 6 ], [ 3, 4, 1, 6, 5 ] )}
gap> Size(h);
120
gap> l:=LClassOfHClass(h);
{PartialPerm( [ 1, 2, 3, 5, 6 ], [ 3, 4, 1, 6, 5 ] )}
gap> SchutzenbergerGroup(l);
Group([ (3,6,5,4), (1,5,6,4,3), (1,4,5)(3,6) ])
gap> ForAll(h, x-> x in l);
true
gap> h:=GroupHClass(DClass(h));
{PartialPerm( [ 1, 2, 3, 5, 6 ], [ 1, 2, 3, 5, 6 ] )}
gap> l:=LClassOfHClass(h);
{PartialPerm( [ 1, 2, 3, 5, 6 ], [ 1, 2, 3, 5, 6 ] )}
gap> StructureDescription(SchutzenbergerGroup(l));
"S5"
gap> f:=PartialPermNC([ 1, 2, 3, 5, 6 ], [ 3, 4, 1, 6, 5 ]);;
gap> h:=HClass(s, f);
{PartialPerm( [ 1, 2, 3, 5, 6 ], [ 3, 4, 1, 6, 5 ] )}
gap> SchutzenbergerGroup(h);
Group([ (1,3,5), (1,5)(3,4,6) ])
gap> h:=GroupHClass(DClass(h));
{PartialPerm( [ 1, 2, 3, 5, 6 ], [ 1, 2, 3, 5, 6 ] )}
gap> SchutzenbergerGroup(h);
Group([ (1,6,3), (1,2,5)(3,6) ])
gap> l:=LClassOfHClass(h);
{PartialPerm( [ 1, 2, 3, 5, 6 ], [ 1, 2, 3, 5, 6 ] )}
gap> IsGreensClassNC(l);
false
gap> Size(SchutzenbergerGroup(l));
120
gap> h:=GroupHClass(DClass(h));
{PartialPerm( [ 1, 2, 3, 5, 6 ], [ 1, 2, 3, 5, 6 ] )}
gap> h:=HClass(s, f);
{PartialPerm( [ 1, 2, 3, 5, 6 ], [ 3, 4, 1, 6, 5 ] )}
gap> IsGreensClassNC(h);
false
gap> d:=DClass(h);
{PartialPerm( [ 1, 2, 3, 5, 6 ], [ 3, 4, 1, 6, 5 ] )}
gap> IsGreensClassNC(d);
false
gap> h:=GroupHClass(d);
{PartialPerm( [ 1, 2, 3, 5, 6 ], [ 1, 2, 3, 5, 6 ] )}
gap> IsGreensClassNC(h);
false
gap> h:=HClass(s, f);
{PartialPerm( [ 1, 2, 3, 5, 6 ], [ 3, 4, 1, 6, 5 ] )}
gap> d:=DClass(h);
{PartialPerm( [ 1, 2, 3, 5, 6 ], [ 3, 4, 1, 6, 5 ] )}
gap> h:=GroupHClass(d);
{PartialPerm( [ 1, 2, 3, 5, 6 ], [ 1, 2, 3, 5, 6 ] )}
gap> IsGreensClassNC(h);
false
gap> l:=LClassOfHClass(h);
{PartialPerm( [ 1, 2, 3, 5, 6 ], [ 1, 2, 3, 5, 6 ] )}
gap> IsGreensClassNC(l);
false
gap> MovedPoints(SchutzenbergerGroup(l));
[ 1, 2, 3, 5, 6 ]
gap> ForAll(h, x-> x in l);
true
gap> Number(l, x-> x in h);
120
gap> Size(h);
120
gap> f:=PartialPerm([ 1, 2, 3, 6, 8 ], [ 2, 7, 6, 8, 5 ]);;
gap> h:=HClass(s, f);
{PartialPerm( [ 1, 2, 3, 6, 8 ], [ 2, 7, 6, 8, 5 ] )}
gap> IsGroupHClass(h);
false
gap> f:=PartialPerm([ 2, 6, 7, 8 ], [ 3, 5, 7, 6 ]);;
gap> h:=HClassNC(s, f);
{PartialPerm( [ 2, 6, 7, 8 ], [ 3, 5, 7, 6 ] )}
gap> IsGroupHClass(h);
false
gap> h:=GroupHClass(DClass(h));
fail
gap> h:=HClassNC(s, f);
{PartialPerm( [ 2, 6, 7, 8 ], [ 3, 5, 7, 6 ] )}
gap> IsRegularClass(h);
false
gap> d:=DClass(h);
{PartialPerm( [ 2, 6, 7, 8 ], [ 3, 5, 7, 6 ] )}
gap> IsRegularDClass(d);
false

#
gap> gens:=[ Transformation( [ 4, 6, 5, 2, 1, 3 ] ),
>   Transformation( [ 6, 3, 2, 5, 4, 1 ] ),
>   Transformation( [ 1, 2, 4, 3, 5, 6 ] ),
>   Transformation( [ 3, 5, 6, 1, 2, 3 ] ),
>   Transformation( [ 5, 3, 6, 6, 6, 2 ] ),
>   Transformation( [ 2, 3, 2, 6, 4, 6 ] ),
>   Transformation( [ 2, 1, 2, 2, 2, 4 ] ),
>   Transformation( [ 4, 4, 1, 2, 1, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> f:=Transformation( [ 4, 4, 1, 2, 1, 2 ] );;
gap> h:=HClassNC(s, f);
{Transformation( [ 4, 4, 1, 2, 1, 2 ] )}
gap> IsRegularClass(h);
false
gap> IsGroupHClass(h);
false
gap> h:=GroupHClass(DClass(h));
{Transformation( [ 2, 2, 3, 6, 3, 6 ] )}
gap> Size(h);
6
gap> r:=RClassOfHClass(h);
{Transformation( [ 1, 1, 2, 4, 2, 4 ] )}
gap> ForAll(h, x-> x in r);
true
gap> Number(r, x-> x in h);
6
gap> l;
{PartialPerm( [ 1, 2, 3, 5, 6 ], [ 1, 2, 3, 5, 6 ] )}
gap> RhoOrbStabChain(l);
true
gap> g:=SchutzenbergerGroup(l);
Sym( [ 1, 2, 3, 5, 6 ] )
gap> IsSymmetricGroup(g);
true
gap> IsNaturalSymmetricGroup(g);
true

#
gap> a1 := Transformation([2,2,3,5,5,6,7,8,14,16,16,17,18,14,16,16,17,18]);;
gap> a2 := Transformation([1,3,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18]);;
gap> a3 := Transformation([1,2,4,4,5,6,7,8,9, 10,11,12,13,15,15,16,17,18]);;
gap> a4 :=Transformation([1,2,3,4,6,6,7,8,9,10,11,12,13,14,15,17,17,18]);;
gap> a5 := Transformation([1,2,3,4,5,7,7,8,9,10,11,12,13,14,15,16,18,18]);;
gap> a6 :=Transformation([1,2,3,4,5,6,8,8,9,10,11,12,13,14,15,16,17,2]);;
gap> P := Transformation([1,2,9,10,11,12,13,1,9,10,11,12,13,14,15,16,17,18]);;
gap> K18g := [a1,a2,a3,a4,a5,a6,P];;
gap> s := Semigroup(K18g);;
gap> f:=Transformation( [ 1, 2, 4, 4, 6, 6, 7, 8, 9, 10, 11, 12, 13, 15, 15,
> 17, 17, 18 ] );;
gap> r:=RClassNC(s, f);
{Transformation( [ 1, 2, 4, 4, 6, 6, 7, 8, 9, 10, 11, 12, 13, 15, 15, 17,
  17 ] )}
gap> Size(r);
1
gap> SchutzenbergerGroup(r);
Group(())
gap> f:=Transformation( [ 1, 2, 10, 10, 11, 12, 13, 1, 9, 10, 11, 12, 13, 15,
> 15, 16, 17, 18 ] );;
gap> r:=RClass(s, f);
{Transformation( [ 1, 2, 10, 10, 11, 12, 13, 1, 9, 10, 11, 12, 13, 15, 15 ] )}
gap> Size(r);
1
gap> SchutzenbergerGroup(r);
Group(())

#
gap> gens:=[ Transformation( [ 2, 4, 1, 5, 4, 4, 7, 3, 8, 1 ] ),
>   Transformation( [ 9, 1, 2, 8, 1, 5, 9, 9, 9, 5 ] ),
>   Transformation( [ 9, 3, 1, 5, 10, 3, 4, 6, 10, 2 ] ),
>   Transformation( [ 10, 7, 3, 7, 1, 9, 8, 8, 4, 10 ] ) ];;
gap> s:=Semigroup(gens);;
gap> f:=Transformation( [ 9, 10, 10, 3, 10, 9, 9, 9, 9, 9 ] );;
gap> r:=RClass(s, f);
{Transformation( [ 9, 5, 5, 2, 5, 9, 9, 9, 9, 9 ] )}
gap> Size(r);
546
gap> SchutzenbergerGroup(r);
Group([ (2,5), (5,9) ])
gap> ForAll(r, x-> x in r);
true
gap> f:=Transformation( [ 8, 8, 8, 8, 8, 8, 7, 7, 8, 8 ] );;
gap> r:=RClass(s, f);
{Transformation( [ 9, 9, 9, 9, 9, 9, 1, 1, 9, 9 ] )}
gap> Size(r);
86
gap> iter:=IteratorOfRClasses(s);
<iterator of R-classes>
gap> repeat r:=NextIterator(iter); until Size(r)>1000;
gap> r;
{Transformation( [ 9, 1, 8, 2, 1, 8, 9, 9, 9, 8 ] )}
gap> Size(r);
1992
gap> SchutzenbergerGroup(r);
Group([ (2,8), (1,8), (1,2,8,9) ])
gap> enum:=Enumerator(r);
<enumerator of R-class>
gap> ForAll(enum, x-> x in r);
true
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
true
gap> ForAll([1..Length(enum)], x-> Position(enum, enum[x])=x);
true
gap> NrHClasses(r);
83
gap> GreensHClasses(r);
[ {Transformation( [ 9, 1, 8, 2, 1, 8, 9, 9, 9, 8 ] )}, 
  {Transformation( [ 8, 2, 4, 3, 2, 4, 8, 8, 8, 4 ] )}, 
  {Transformation( [ 10, 6, 9, 3, 6, 9, 10, 10, 10, 9 ] )}, 
  {Transformation( [ 5, 9, 8, 2, 9, 8, 5, 5, 5, 8 ] )}, 
  {Transformation( [ 8, 4, 7, 10, 4, 7, 8, 8, 8, 7 ] )}, 
  {Transformation( [ 3, 2, 7, 1, 2, 7, 3, 3, 3, 7 ] )}, 
  {Transformation( [ 1, 5, 4, 3, 5, 4, 1, 1, 1, 4 ] )}, 
  {Transformation( [ 1, 5, 3, 6, 5, 3, 1, 1, 1, 3 ] )}, 
  {Transformation( [ 2, 3, 10, 1, 3, 10, 2, 2, 2, 10 ] )}, 
  {Transformation( [ 10, 9, 4, 3, 9, 4, 10, 10, 10, 4 ] )}, 
  {Transformation( [ 3, 4, 2, 5, 4, 2, 3, 3, 3, 2 ] )}, 
  {Transformation( [ 1, 10, 8, 7, 10, 8, 1, 1, 1, 8 ] )}, 
  {Transformation( [ 1, 2, 8, 4, 2, 8, 1, 1, 1, 8 ] )}, 
  {Transformation( [ 9, 1, 2, 5, 1, 2, 9, 9, 9, 2 ] )}, 
  {Transformation( [ 1, 4, 8, 7, 4, 8, 1, 1, 1, 8 ] )}, 
  {Transformation( [ 9, 1, 5, 8, 1, 5, 9, 9, 9, 5 ] )}, 
  {Transformation( [ 3, 5, 7, 1, 5, 7, 3, 3, 3, 7 ] )}, 
  {Transformation( [ 3, 10, 8, 7, 10, 8, 3, 3, 3, 8 ] )}, 
  {Transformation( [ 5, 4, 7, 2, 4, 7, 5, 5, 5, 7 ] )}, 
  {Transformation( [ 1, 4, 7, 2, 4, 7, 1, 1, 1, 7 ] )}, 
  {Transformation( [ 4, 3, 9, 1, 3, 9, 4, 4, 4, 9 ] )}, 
  {Transformation( [ 5, 8, 4, 2, 8, 4, 5, 5, 5, 4 ] )}, 
  {Transformation( [ 10, 5, 9, 3, 5, 9, 10, 10, 10, 9 ] )}, 
  {Transformation( [ 7, 10, 4, 9, 10, 4, 7, 7, 7, 4 ] )}, 
  {Transformation( [ 5, 4, 1, 2, 4, 1, 5, 5, 5, 1 ] )}, 
  {Transformation( [ 10, 1, 9, 5, 1, 9, 10, 10, 10, 9 ] )}, 
  {Transformation( [ 1, 7, 3, 10, 7, 3, 1, 1, 1, 3 ] )}, 
  {Transformation( [ 10, 1, 9, 3, 1, 9, 10, 10, 10, 9 ] )}, 
  {Transformation( [ 3, 1, 2, 9, 1, 2, 3, 3, 3, 2 ] )}, 
  {Transformation( [ 2, 5, 10, 1, 5, 10, 2, 2, 2, 10 ] )}, 
  {Transformation( [ 3, 10, 7, 4, 10, 7, 3, 3, 3, 7 ] )}, 
  {Transformation( [ 1, 4, 9, 7, 4, 9, 1, 1, 1, 9 ] )}, 
  {Transformation( [ 1, 5, 3, 10, 5, 3, 1, 1, 1, 3 ] )}, 
  {Transformation( [ 9, 2, 6, 4, 2, 6, 9, 9, 9, 6 ] )}, 
  {Transformation( [ 9, 3, 6, 5, 3, 6, 9, 9, 9, 6 ] )}, 
  {Transformation( [ 4, 10, 7, 1, 10, 7, 4, 4, 4, 7 ] )}, 
  {Transformation( [ 5, 1, 7, 2, 1, 7, 5, 5, 5, 7 ] )}, 
  {Transformation( [ 7, 2, 4, 3, 2, 4, 7, 7, 7, 4 ] )}, 
  {Transformation( [ 7, 5, 2, 3, 5, 2, 7, 7, 7, 2 ] )}, 
  {Transformation( [ 9, 5, 6, 4, 5, 6, 9, 9, 9, 6 ] )}, 
  {Transformation( [ 4, 1, 8, 10, 1, 8, 4, 4, 4, 8 ] )}, 
  {Transformation( [ 4, 10, 9, 1, 10, 9, 4, 4, 4, 9 ] )}, 
  {Transformation( [ 10, 3, 1, 8, 3, 1, 10, 10, 10, 1 ] )}, 
  {Transformation( [ 7, 10, 1, 9, 10, 1, 7, 7, 7, 1 ] )}, 
  {Transformation( [ 1, 2, 6, 4, 2, 6, 1, 1, 1, 6 ] )}, 
  {Transformation( [ 10, 5, 4, 3, 5, 4, 10, 10, 10, 4 ] )}, 
  {Transformation( [ 5, 1, 8, 4, 1, 8, 5, 5, 5, 8 ] )}, 
  {Transformation( [ 3, 10, 2, 9, 10, 2, 3, 3, 3, 2 ] )}, 
  {Transformation( [ 9, 5, 4, 3, 5, 4, 9, 9, 9, 4 ] )}, 
  {Transformation( [ 5, 1, 8, 2, 1, 8, 5, 5, 5, 8 ] )}, 
  {Transformation( [ 10, 6, 5, 3, 6, 5, 10, 10, 10, 5 ] )}, 
  {Transformation( [ 10, 1, 4, 3, 1, 4, 10, 10, 10, 4 ] )}, 
  {Transformation( [ 7, 1, 5, 8, 1, 5, 7, 7, 7, 5 ] )}, 
  {Transformation( [ 5, 10, 4, 2, 10, 4, 5, 5, 5, 4 ] )}, 
  {Transformation( [ 9, 4, 1, 2, 4, 1, 9, 9, 9, 1 ] )}, 
  {Transformation( [ 2, 9, 10, 1, 9, 10, 2, 2, 2, 10 ] )}, 
  {Transformation( [ 2, 5, 8, 7, 5, 8, 2, 2, 2, 8 ] )}, 
  {Transformation( [ 9, 5, 10, 4, 5, 10, 9, 9, 9, 10 ] )}, 
  {Transformation( [ 1, 4, 5, 7, 4, 5, 1, 1, 1, 5 ] )}, 
  {Transformation( [ 8, 1, 7, 3, 1, 7, 8, 8, 8, 7 ] )}, 
  {Transformation( [ 5, 3, 1, 2, 3, 1, 5, 5, 5, 1 ] )}, 
  {Transformation( [ 5, 9, 6, 2, 9, 6, 5, 5, 5, 6 ] )}, 
  {Transformation( [ 10, 2, 5, 3, 2, 5, 10, 10, 10, 5 ] )}, 
  {Transformation( [ 9, 10, 2, 5, 10, 2, 9, 9, 9, 2 ] )}, 
  {Transformation( [ 2, 1, 9, 6, 1, 9, 2, 2, 2, 9 ] )}, 
  {Transformation( [ 7, 1, 2, 8, 1, 2, 7, 7, 7, 2 ] )}, 
  {Transformation( [ 9, 2, 10, 4, 2, 10, 9, 9, 9, 10 ] )}, 
  {Transformation( [ 9, 10, 6, 5, 10, 6, 9, 9, 9, 6 ] )}, 
  {Transformation( [ 3, 1, 7, 4, 1, 7, 3, 3, 3, 7 ] )}, 
  {Transformation( [ 4, 10, 9, 6, 10, 9, 4, 4, 4, 9 ] )}, 
  {Transformation( [ 3, 10, 6, 4, 10, 6, 3, 3, 3, 6 ] )}, 
  {Transformation( [ 4, 1, 9, 6, 1, 9, 4, 4, 4, 9 ] )}, 
  {Transformation( [ 4, 3, 9, 6, 3, 9, 4, 4, 4, 9 ] )}, 
  {Transformation( [ 9, 1, 4, 5, 1, 4, 9, 9, 9, 4 ] )}, 
  {Transformation( [ 3, 10, 9, 7, 10, 9, 3, 3, 3, 9 ] )}, 
  {Transformation( [ 7, 3, 4, 9, 3, 4, 7, 7, 7, 4 ] )}, 
  {Transformation( [ 1, 2, 10, 4, 2, 10, 1, 1, 1, 10 ] )}, 
  {Transformation( [ 3, 10, 8, 4, 10, 8, 3, 3, 3, 8 ] )}, 
  {Transformation( [ 1, 5, 10, 4, 5, 10, 1, 1, 1, 10 ] )}, 
  {Transformation( [ 3, 4, 8, 7, 4, 8, 3, 3, 3, 8 ] )}, 
  {Transformation( [ 3, 5, 2, 9, 5, 2, 3, 3, 3, 2 ] )}, 
  {Transformation( [ 2, 1, 5, 6, 1, 5, 2, 2, 2, 5 ] )}, 
  {Transformation( [ 1, 5, 6, 4, 5, 6, 1, 1, 1, 6 ] )} ]
gap> List(last, x-> Representative(x) in s);
[ true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true ]
gap> ForAll(last2, x-> Representative(x) in r);
true
gap> Semigroup(gens);;
gap> r:=GreensRClassOfElement(s, f);
{Transformation( [ 9, 9, 9, 9, 9, 9, 1, 1, 9, 9 ] )}
gap> f:=Transformation( [ 9, 9, 5, 9, 5, 9, 5, 5, 5, 5 ] );;
gap> r:=GreensRClassOfElement(s, f);
{Transformation( [ 1, 1, 9, 1, 9, 1, 9, 9, 9, 9 ] )}
gap> Size(r);
86
gap> NrHClasses(r);
43
gap> s:=Semigroup(gens);;
gap> r:=GreensRClassOfElement(s, f);
{Transformation( [ 1, 1, 9, 1, 9, 1, 9, 9, 9, 9 ] )}
gap> GreensHClasses(r);
[ {Transformation( [ 1, 1, 9, 1, 9, 1, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 2, 2, 8, 2, 8, 2, 8, 8, 8, 8 ] )}, 
  {Transformation( [ 10, 10, 9, 10, 9, 10, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 4, 4, 10, 4, 10, 4, 10, 10, 10, 10 ] )}, 
  {Transformation( [ 4, 4, 8, 4, 8, 4, 8, 8, 8, 8 ] )}, 
  {Transformation( [ 10, 10, 3, 10, 3, 10, 3, 3, 3, 3 ] )}, 
  {Transformation( [ 3, 3, 1, 3, 1, 3, 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 2, 9, 2, 9, 2, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 3, 3, 8, 3, 8, 3, 8, 8, 8, 8 ] )}, 
  {Transformation( [ 3, 3, 4, 3, 4, 3, 4, 4, 4, 4 ] )}, 
  {Transformation( [ 6, 6, 3, 6, 3, 6, 3, 3, 3, 3 ] )}, 
  {Transformation( [ 8, 8, 7, 8, 7, 8, 7, 7, 7, 7 ] )}, 
  {Transformation( [ 1, 1, 8, 1, 8, 1, 8, 8, 8, 8 ] )}, 
  {Transformation( [ 5, 5, 9, 5, 9, 5, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 10, 10, 2, 10, 2, 10, 2, 2, 2, 2 ] )}, 
  {Transformation( [ 5, 5, 1, 5, 1, 5, 1, 1, 1, 1 ] )}, 
  {Transformation( [ 5, 5, 8, 5, 8, 5, 8, 8, 8, 8 ] )}, 
  {Transformation( [ 5, 5, 2, 5, 2, 5, 2, 2, 2, 2 ] )}, 
  {Transformation( [ 10, 10, 7, 10, 7, 10, 7, 7, 7, 7 ] )}, 
  {Transformation( [ 8, 8, 9, 8, 9, 8, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 10, 10, 6, 10, 6, 10, 6, 6, 6, 6 ] )}, 
  {Transformation( [ 10, 10, 8, 10, 8, 10, 8, 8, 8, 8 ] )}, 
  {Transformation( [ 4, 4, 1, 4, 1, 4, 1, 1, 1, 1 ] )}, 
  {Transformation( [ 5, 5, 4, 5, 4, 5, 4, 4, 4, 4 ] )}, 
  {Transformation( [ 3, 3, 5, 3, 5, 3, 5, 5, 5, 5 ] )}, 
  {Transformation( [ 5, 5, 6, 5, 6, 5, 6, 6, 6, 6 ] )}, 
  {Transformation( [ 2, 2, 1, 2, 1, 2, 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 2, 4, 2, 4, 2, 4, 4, 4, 4 ] )}, 
  {Transformation( [ 3, 3, 9, 3, 9, 3, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 4, 4, 7, 4, 7, 4, 7, 7, 7, 7 ] )}, 
  {Transformation( [ 6, 6, 1, 6, 1, 6, 1, 1, 1, 1 ] )}, 
  {Transformation( [ 3, 3, 7, 3, 7, 3, 7, 7, 7, 7 ] )}, 
  {Transformation( [ 6, 6, 4, 6, 4, 6, 4, 4, 4, 4 ] )}, 
  {Transformation( [ 3, 3, 2, 3, 2, 3, 2, 2, 2, 2 ] )}, 
  {Transformation( [ 6, 6, 9, 6, 9, 6, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 10, 10, 1, 10, 1, 10, 1, 1, 1, 1 ] )}, 
  {Transformation( [ 1, 1, 7, 1, 7, 1, 7, 7, 7, 7 ] )}, 
  {Transformation( [ 7, 7, 9, 7, 9, 7, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 6, 6, 2, 6, 2, 6, 2, 2, 2, 2 ] )}, 
  {Transformation( [ 10, 10, 5, 10, 5, 10, 5, 5, 5, 5 ] )}, 
  {Transformation( [ 5, 5, 7, 5, 7, 5, 7, 7, 7, 7 ] )}, 
  {Transformation( [ 4, 4, 9, 4, 9, 4, 9, 9, 9, 9 ] )}, 
  {Transformation( [ 2, 2, 7, 2, 7, 2, 7, 7, 7, 7 ] )} ]
gap> Length(last);
43
gap> ForAll(last2, x-> Representative(x) in r);
true
gap> ForAll(last3, x-> Representative(x) in s);
true
gap> h:=HClass(s, Transformation( [ 4, 4, 9, 4, 9, 4, 9, 9, 9, 9 ] ));;
gap> f:=Representative(h);
Transformation( [ 4, 4, 9, 4, 9, 4, 9, 9, 9, 9 ] )
gap> hh:=HClass(r, f);
{Transformation( [ 4, 4, 9, 4, 9, 4, 9, 9, 9, 9 ] )}
gap> hh=h;
true
gap> h=hh;
true
gap> Elements(h)=Elements(hh);
true
gap> f:=Transformation( [ 10, 1, 9, 10, 2, 1, 5, 3, 2, 3 ] );;
gap> r:=GreensRClassOfElement(s, f);
{Transformation( [ 10, 1, 9, 10, 2, 1, 5, 3, 2, 3 ] )}
gap> Size(r);
1
gap> f:=Transformation( [ 10, 10, 3, 10, 10, 10, 10, 10, 6, 10 ] );;
gap> r:=GreensRClassOfElement(s, f);
{Transformation( [ 5, 5, 2, 5, 5, 5, 5, 5, 9, 5 ] )}
gap> Size(r);
546
gap> f:=Transformation( [ 6, 6, 4, 6, 6, 6, 6, 6, 3, 6 ] );;
gap> f in r;
true
gap> h:=HClass(r, f);
{Transformation( [ 6, 6, 4, 6, 6, 6, 6, 6, 3, 6 ] )}
gap> f in h;
true
gap> ForAll(h, x-> x in r);
true
gap> Size(h);
6
gap> Elements(h);
[ Transformation( [ 3, 3, 4, 3, 3, 3, 3, 3, 6, 3 ] ), 
  Transformation( [ 3, 3, 6, 3, 3, 3, 3, 3, 4, 3 ] ), 
  Transformation( [ 4, 4, 3, 4, 4, 4, 4, 4, 6, 4 ] ), 
  Transformation( [ 4, 4, 6, 4, 4, 4, 4, 4, 3, 4 ] ), 
  Transformation( [ 6, 6, 3, 6, 6, 6, 6, 6, 4, 6 ] ), 
  Transformation( [ 6, 6, 4, 6, 6, 6, 6, 6, 3, 6 ] ) ]

#
gap> gens:=
> [ PartialPermNC( [ 1, 2, 3, 5, 9, 10 ], [ 5, 10, 7, 8, 9, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6, 9 ], [ 9, 3, 1, 4, 2, 5, 6 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 7, 9 ], [ 7, 6, 2, 8, 4, 5, 3 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], 
> [ 8, 7, 4, 3, 10, 9, 5, 6, 1, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
1422787
gap> f:=PartialPerm([ 1, 4, 7, 9, 10 ], [ 5, 10, 9, 8, 7 ]);;
gap> r:=GreensRClassOfElementNC(s, f);
{PartialPerm( [ 1, 4, 7, 9, 10 ], [ 5, 10, 9, 8, 7 ] )}
gap> Size(r);
4
gap> f in r;
true
gap> f:=PartialPerm([ 1, 7, 8, 9 ], [ 10, 9, 6, 5 ]);;
gap> r:=GreensRClassOfElementNC(s, f);
{PartialPerm( [ 1, 7, 8, 9 ], [ 5, 6, 8, 7 ] )}
gap> Size(r);
4
gap> iter:=IteratorOfRClasses(s);
<iterator of R-classes>
gap> repeat r:=NextIterator(iter); until Size(r)>1000;
gap> r;
{PartialPerm( [ 1, 3, 9, 10 ], [ 4, 5, 3, 7 ] )}
gap> Size(r);
3792
gap> r:=RClassNC(s, Representative(r));
{PartialPerm( [ 1, 3, 9, 10 ], [ 4, 5, 3, 7 ] )}
gap> h:=HClassNC(r, PartialPermNC([ 1, 3, 9, 10 ], [ 10, 9, 8, 1 ]));;
gap> Size(h);
24
gap> ForAll(h, x-> x in r);
true
gap> IsRegularClass(r);
true
gap> IsRegularSemigroup(s);
false
gap> NrIdempotents(r);
1
gap> Idempotents(r);
[ <identity partial perm on [ 1, 3, 9, 10 ]> ]
gap> ForAll(last, x-> x in r);
true

#
gap> gens:=[ Transformation( [ 1, 3, 7, 9, 1, 12, 13, 1, 15, 9, 1, 18, 1, 1,
>  13, 1, 1, 21, 1, 1, 1, 1, 1, 25, 26, 1 ] ),
>  Transformation( [ 1, 5, 1, 5, 11, 1, 1, 14, 1, 16, 17, 1, 1, 19, 1, 11, 1,
>       1, 1, 23, 1, 16, 19, 1, 1, 1 ] ),
> Transformation( [ 1, 4, 8, 1, 10, 1, 8, 1, 1, 1, 10, 1, 8, 10, 1, 1, 20, 1,
>       22, 1, 8, 1, 1, 1, 1, 1 ] ),
> Transformation( [ 1, 6, 6, 1, 1, 1, 6, 1, 1, 1, 1, 1, 6, 1, 6, 1, 1, 6, 1,
>       1, 24, 1, 1, 1, 1, 6 ] ) ];;
gap> s:=Semigroup(gens);;
gap> First(DClasses(s), IsRegularDClass);
{Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
 1, 1, 1, 1, 1, 1 ] )}
gap> NrDClasses(s);
31
gap> PositionsProperty(DClasses(s), IsRegularDClass);
[ 6, 7 ]
gap> d:=DClasses(s)[7];
{Transformation( [ 1, 8, 1, 1, 8, 1, 1, 1, 1, 1, 8, 1, 1, 8, 1, 1, 1, 1, 1, 1,
 1, 1, 1, 1, 1, 1 ] )}
gap> r:=RClassNC(s, Representative(d));
{Transformation( [ 1, 8, 1, 1, 8, 1, 1, 1, 1, 1, 8, 1, 1, 8, 1, 1, 1, 1, 1, 1,
 1, 1, 1, 1, 1, 1 ] )}
gap> Size(r);
20
gap> ForAll(Idempotents(r), x-> x in s);
true
gap> ForAll(Idempotents(r), x-> x in r);
true
gap> ForAll(Idempotents(r), x-> x in d);
true
gap> ForAll(r, x-> x in d);
true
gap> Number(GreensRClasses(s), IsRegularClass);
21
gap> NrRegularDClasses(s);
2

#
gap> gens:=[ Transformation( [ 1, 2, 3, 5, 4, 6, 7, 8 ] ),
>   Transformation( [ 4, 4, 3, 1, 5, 6, 3, 8 ] ),
>   Transformation( [ 3, 6, 1, 7, 3, 4, 8, 3 ] ),
>   Transformation( [ 1, 2, 3, 4, 5, 3, 7, 8 ] ),
>   Transformation( [ 1, 2, 3, 4, 1, 6, 7, 8 ] ),
>   Transformation( [ 8, 8, 3, 4, 5, 7, 6, 1 ] ) ];;
gap> s:=Monoid(gens);
<transformation monoid on 8 pts with 6 generators>
gap> f:=Transformation( [ 4, 4, 3, 8, 5, 3, 3, 1 ] );;
gap> Size(s);
998
gap> r:=RClass(s, f);
{Transformation( [ 4, 4, 3, 1, 5, 3, 3 ] )}
gap> IsRegularClass(r);
true
gap> Idempotents(r);
[ Transformation( [ 1, 1, 3, 4, 5, 3, 3 ] ) ]
gap> IsRegularSemigroup(s);
false
gap> ForAll(r, x-> x in s);
true
gap> iter:=Iterator(r);
<iterator of R-class>
gap> for i in iter do od;
gap> Size(r);
24
gap> IsDoneIterator(iter);
true
gap> iter:=Iterator(r);
<iterator of R-class>
gap> for i in [1..23] do NextIterator(iter); od;
gap> IsDoneIterator(iter);
false
gap> NextIterator(iter);
Transformation( [ 5, 5, 3, 8, 4, 3, 3, 1 ] )
gap> IsDoneIterator(iter);
true
gap> Transformation( [ 4, 4, 3, 8, 1, 3, 3, 5 ] ) in r;
true
gap> r;
{Transformation( [ 4, 4, 3, 1, 5, 3, 3 ] )}
gap> NrIdempotents(r);
1

#
gap> gens:= 
> [ PartialPermNC( [ 1, 2, 3, 4, 7 ], [ 8, 3, 5, 7, 4 ] ),
>  PartialPermNC( [ 1, 2, 5, 6, 7 ], [ 4, 1, 6, 2, 8 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6 ], [ 3, 7, 1, 5, 2, 6 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6 ], [ 7, 2, 5, 6, 3, 8 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 6, 7 ], [ 4, 5, 6, 1, 2, 7 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 6, 7 ], [ 5, 1, 7, 2, 8, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
9954
gap> f:=PartialPerm([ 2, 3, 6 ], [ 1, 4, 8 ]);;
gap> r:=RClass(s, f);
{PartialPerm( [ 2, 3, 6 ], [ 1, 4, 8 ] )}
gap> NrIdempotents(r);
0
gap> List(RClasses(s), NrIdempotents);;
gap> Sum(last);
53
gap> NrIdempotents(s);
53
gap> r:=RClassOfHClass(h);
{PartialPerm( [ 1, 3, 9, 10 ], [ 4, 5, 3, 7 ] )}
gap> HClassReps(r);
[ [1,4][9,3,5][10,7], [1,8][3,2][9,5][10,4], [1,3,10,5][9,4], [3,5][9,1,10,8],
  [1,3,2][10,5](9), [9,4][10,3,1,5], [1,6][3,2][9,5][10,8], [3,7][10,1,5](9), 
  [3,4][10,6](1)(9), [1,7][3,5][9,10,8], [1,5][9,2][10,3,4], [3,1,9,7][10,8], 
  [1,6][9,10,3,7], [1,4][9,3,2](10), [1,2][3,10,6][9,8], [1,10,8][3,7](9), 
  [9,6][10,2](1)(3), [1,6][9,4][10,2](3), [3,7][9,1,4](10), [3,9,4][10,1,2], 
  [1,7][3,4][9,8][10,2], [1,8][9,3,4](10), [1,9,10,6][3,7], 
  [1,9,6][3,4][10,5], [10,9,1,8](3), [1,6][3,4][9,5][10,2], 
  [1,5][3,10,6][9,2], [1,10,7][3,9,5], [9,3,4][10,1,2], [1,4][3,6][9,8][10,2],
  [1,7][9,10,4](3), [3,8][9,5][10,6](1), [1,7][3,8][9,4][10,6], 
  [1,9,2][3,5][10,4], [1,3,7][9,4][10,2], [1,7][3,2][10,9,6], 
  [3,5][9,1,2][10,6], [10,3,1,9,5], [1,8][3,4][10,7](9), [1,7][10,4](3,9), 
  [1,3,5][9,8][10,2], [1,4][10,6](3)(9), [1,6][9,8][10,7](3), [9,3,1,7][10,8],
  [1,5][9,6][10,7](3), [3,5][10,9,2](1), 
  <identity partial perm on [ 1, 3, 9, 10 ]>, [3,4][9,8][10,1,6], 
  [1,3,7](9,10), [3,6][9,5][10,4](1), [1,10,9,7][3,2], [3,1,2][9,10,5], 
  [1,8][9,3,7][10,2], [1,2][3,5][9,8][10,7], [10,9,1,3,4], [1,3,8][9,7][10,4],
  [1,3,9,6][10,7], [1,8][3,6](9,10), [1,5][10,9,3,6], [3,7][9,5](1,10), 
  [1,3,4][9,6][10,8], [9,7](1)(3,10), [3,9,2][10,1,6], [3,8][9,10,4](1), 
  [9,1,6][10,5](3), [1,4][3,10,7][9,6], [3,5][9,2][10,1,4], 
  [1,6][3,5][9,4][10,7], [1,5][3,8][9,4][10,6], [1,10,5][3,4](9), 
  [1,9,3,2][10,6], [1,6][9,7][10,3,4], [3,10,1,8][9,7], [3,4][9,1,8][10,2], 
  [1,9,6][10,8](3), [1,8](3,9,10), [3,7][9,5][10,1,2], [1,7][3,8][9,2](10), 
  [1,4][9,3,5][10,8], [1,7][3,10,5][9,6], [1,4][3,9,6][10,2], 
  [9,8][10,1,4](3), [1,4][9,5][10,3,6], [3,1,4][10,5](9), [1,8][9,2][10,3,6], 
  [3,9,4](1,10), [1,2][3,5][9,10,8], [1,9,8][3,4](10), [1,2][3,7][9,4][10,6], 
  [1,7][9,5][10,8](3), [1,4][10,9,3,2], [1,8][9,7](3)(10), [3,1,6][9,2][10,4],
  [1,8][9,2][10,4](3), [1,9,3,10,5], [1,10,9,3,6], [9,1,2](3)(10), 
  [3,7][10,9,4](1), [1,9,5][10,4](3), [1,2][9,7][10,3,5], 
  [1,5][3,6][9,7][10,2], [1,3,10,6][9,4], [1,5][3,2](9)(10), [10,9,1,3,7], 
  [1,10,9,4](3), [9,1,3,8](10), [1,6][3,9,7][10,4], [9,3,1,2][10,8], 
  [1,7][3,10,6][9,2], [1,7][10,9,3,5], [1,5][3,4][9,10,6], [10,3,9,1,2], 
  [9,1,4][10,6](3), [9,4][10,1,3,7], [1,9,7][10,3,8], [3,2][9,4](1)(10), 
  [9,3,5][10,1,8], [9,10,3,4](1), [1,7][3,10,4][9,5], [3,10,1,8](9), 
  [3,7][9,1,10,2], [3,8][9,1,4][10,5], [1,5][3,2][10,9,7], [1,7][9,6][10,3,2],
  [3,10,1,5][9,4], [1,10,9,2](3), [10,1,3,6](9), [3,1,7][9,8][10,4], 
  [1,3,4][10,8](9), [1,6][3,10,8][9,4], [1,5][9,10,3,2], [3,2][9,1,6][10,8], 
  [1,3,6][9,8](10), [1,5][3,7][10,4](9), [9,3,1,5][10,2], [1,10,8][9,3,2], 
  [3,7](1)(9,10), [3,4][9,7][10,1,2], [3,5][10,9,6](1), [3,9,1,4][10,8], 
  [1,5][9,6][10,3,8], [1,10,4][3,7][9,2], [1,9,8][3,7][10,6], [9,1,10,3,5], 
  [3,1,5][9,7][10,8], [1,8][3,4][9,10,7], [3,5][9,2][10,1,8], 
  [1,9,5][3,2][10,6], [9,8][10,6](1,3), [1,10,4][3,6](9), [1,2][3,10,4][9,8], 
  [1,10,6][3,8][9,5], [1,6][9,2][10,5](3), [1,6][3,10,8][9,7], 
  [1,4][3,8][10,9,6], [1,2][3,6](9,10), [1,7][3,9,4](10), [3,9,2][10,1,7] ]
gap> iter:=IteratorOfRClasses(s);
<iterator of R-classes>
gap> repeat r:=NextIterator(iter); until Size(r)>1;
gap> r;
{PartialPerm( [ 1, 2, 5 ], [ 5, 1, 2 ] )}
gap> Size(r);
120
gap> HClassReps(r);
[ (1,5,2), [1,3][2,6](5), [5,2,4](1), [1,4](2)(5), [1,4][2,3](5), 
  [2,6][5,4](1), [1,7][5,3](2), [2,5,4](1), [5,2,6](1), [1,7][5,2,6], 
  [1,7](2)(5), [1,6][5,3](2), [5,7](1,2), [1,3](2)(5), [1,7][2,3][5,6], 
  [2,5,1,6], [1,6][2,5,7], [2,5,1,7], [1,2,5,6], [1,6][5,2,4] ]
gap> ForAll(last, x-> x in r);
true
gap> r;
{PartialPerm( [ 1, 2, 5 ], [ 5, 1, 2 ] )}
gap> Size(DClass(r));
2400
gap> d:=DClass(r);
{PartialPerm( [ 1, 2, 6 ], [ 1, 5, 2 ] )}
gap> ForAll(r, x-> x in d);
true
gap> Number(d, x-> x in r);
120
gap> Size(r);
120
gap> ForAll(HClassReps(r), x-> x in d);
true
gap> ForAll(HClassReps(r), x-> x in HClassReps(d));
true

#
gap> gens:=[ Transformation( [ 6, 4, 3, 2, 5, 1 ] ),
>   Transformation( [ 1, 2, 3, 4, 5, 6 ] ),
>   Transformation( [ 5, 3, 3, 2, 4, 1 ] ),
>   Transformation( [ 1, 3, 3, 4, 5, 2 ] ),
>   Transformation( [ 4, 5, 2, 3, 3, 1 ] ),
>   Transformation( [ 6, 4, 3, 5, 2, 3 ] ),
>   Transformation( [ 5, 2, 3, 4, 3, 6 ] ),
>   Transformation( [ 1, 3, 2, 5, 4, 5 ] ),
>   Transformation( [ 4, 3, 2, 2, 1, 5 ] ),
>   Transformation( [ 1, 3, 3, 5, 2, 4 ] ),
>   Transformation( [ 6, 3, 3, 2, 1, 5 ] ),
>   Transformation( [ 6, 3, 4, 5, 2, 2 ] ),
>   Transformation( [ 6, 4, 3, 2, 2, 5 ] ),
>   Transformation( [ 1, 3, 2, 3, 5, 4 ] ),
>   Transformation( [ 1, 2, 3, 4, 5, 2 ] ),
>   Transformation( [ 2, 4, 3, 4, 6, 5 ] ),
>   Transformation( [ 2, 4, 3, 3, 6, 1 ] ),
>   Transformation( [ 6, 4, 3, 2, 3, 1 ] ),
>   Transformation( [ 6, 4, 3, 2, 2, 1 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
7008
gap> NrRClasses(s);
310
gap> IsRegularSemigroup(s);
false
gap> f:=Transformation( [ 3, 2, 3, 4, 3, 5 ] );;
gap> r:=RClassNC(s, f);
{Transformation( [ 3, 5, 3, 4, 3, 2 ] )}
gap> d:=DClassOfRClass(r);
{Transformation( [ 2, 4, 3, 5, 3, 3 ] )}
gap> Size(d);
792
gap> IsRegularDClass(d);
false
gap> NrIdempotents(d);
0
gap> Idempotents(d);
[  ]
gap> HClassReps(d);
[ Transformation( [ 2, 4, 3, 5, 3, 3 ] ), 
  Transformation( [ 2, 5, 3, 4, 3, 3 ] ), 
  Transformation( [ 2, 5, 4, 3, 4, 4 ] ), 
  Transformation( [ 3, 4, 2, 5, 2, 2 ] ), 
  Transformation( [ 3, 5, 2, 4, 2, 2 ] ), 
  Transformation( [ 4, 3, 2, 5, 2, 2 ] ), 
  Transformation( [ 2, 3, 4, 5, 3, 3 ] ), 
  Transformation( [ 2, 3, 5, 4, 3, 3 ] ), 
  Transformation( [ 2, 4, 5, 3, 4, 4 ] ), 
  Transformation( [ 3, 2, 4, 5, 2, 2 ] ), 
  Transformation( [ 3, 2, 5, 4, 2, 2 ] ), 
  Transformation( [ 4, 2, 3, 5, 2, 2 ] ), 
  Transformation( [ 3, 4, 3, 4, 2, 5 ] ), 
  Transformation( [ 3, 5, 3, 5, 2, 4 ] ), 
  Transformation( [ 4, 5, 4, 5, 2, 3 ] ), 
  Transformation( [ 2, 4, 2, 4, 3, 5 ] ), 
  Transformation( [ 2, 5, 2, 5, 3, 4 ] ), 
  Transformation( [ 2, 3, 2, 3, 4, 5 ] ), 
  Transformation( [ 5, 3, 3, 4, 5, 2 ] ), 
  Transformation( [ 4, 3, 3, 5, 4, 2 ] ), 
  Transformation( [ 3, 4, 4, 5, 3, 2 ] ), 
  Transformation( [ 5, 2, 2, 4, 5, 3 ] ), 
  Transformation( [ 4, 2, 2, 5, 4, 3 ] ), 
  Transformation( [ 5, 2, 2, 3, 5, 4 ] ), 
  Transformation( [ 5, 5, 4, 3, 3, 2 ] ), 
  Transformation( [ 4, 4, 5, 3, 3, 2 ] ), 
  Transformation( [ 3, 3, 5, 4, 4, 2 ] ), 
  Transformation( [ 5, 5, 4, 2, 2, 3 ] ), 
  Transformation( [ 4, 4, 5, 2, 2, 3 ] ), 
  Transformation( [ 5, 5, 3, 2, 2, 4 ] ), 
  Transformation( [ 5, 4, 3, 4, 2, 3 ] ), 
  Transformation( [ 4, 5, 3, 5, 2, 3 ] ), 
  Transformation( [ 3, 5, 4, 5, 2, 4 ] ), 
  Transformation( [ 5, 4, 2, 4, 3, 2 ] ), 
  Transformation( [ 4, 5, 2, 5, 3, 2 ] ), 
  Transformation( [ 5, 3, 2, 3, 4, 2 ] ), 
  Transformation( [ 4, 5, 3, 4, 3, 2 ] ), 
  Transformation( [ 5, 4, 3, 5, 3, 2 ] ), 
  Transformation( [ 5, 3, 4, 5, 4, 2 ] ), 
  Transformation( [ 4, 5, 2, 4, 2, 3 ] ), 
  Transformation( [ 5, 4, 2, 5, 2, 3 ] ), 
  Transformation( [ 3, 5, 2, 3, 2, 4 ] ), 
  Transformation( [ 2, 3, 4, 5, 5, 5 ] ), 
  Transformation( [ 2, 3, 5, 4, 4, 4 ] ), 
  Transformation( [ 2, 4, 5, 3, 3, 3 ] ), 
  Transformation( [ 3, 2, 4, 5, 5, 5 ] ), 
  Transformation( [ 3, 2, 5, 4, 4, 4 ] ), 
  Transformation( [ 4, 2, 3, 5, 5, 5 ] ), 
  Transformation( [ 5, 3, 4, 4, 2, 5 ] ), 
  Transformation( [ 4, 3, 5, 5, 2, 4 ] ), 
  Transformation( [ 3, 4, 5, 5, 2, 3 ] ), 
  Transformation( [ 5, 2, 4, 4, 3, 5 ] ), 
  Transformation( [ 4, 2, 5, 5, 3, 4 ] ), 
  Transformation( [ 5, 2, 3, 3, 4, 5 ] ), 
  Transformation( [ 2, 3, 4, 4, 5, 5 ] ), 
  Transformation( [ 2, 3, 5, 5, 4, 4 ] ), 
  Transformation( [ 2, 4, 5, 5, 3, 3 ] ), 
  Transformation( [ 3, 2, 4, 4, 5, 5 ] ), 
  Transformation( [ 3, 2, 5, 5, 4, 4 ] ), 
  Transformation( [ 4, 2, 3, 3, 5, 5 ] ), 
  Transformation( [ 2, 4, 3, 5, 5, 4 ] ), 
  Transformation( [ 2, 5, 3, 4, 4, 5 ] ), 
  Transformation( [ 2, 5, 4, 3, 3, 5 ] ), 
  Transformation( [ 3, 4, 2, 5, 5, 4 ] ), 
  Transformation( [ 3, 5, 2, 4, 4, 5 ] ), 
  Transformation( [ 4, 3, 2, 5, 5, 3 ] ), 
  Transformation( [ 2, 3, 4, 3, 5, 5 ] ), 
  Transformation( [ 2, 3, 5, 3, 4, 4 ] ), 
  Transformation( [ 2, 4, 5, 4, 3, 3 ] ), 
  Transformation( [ 3, 2, 4, 2, 5, 5 ] ), 
  Transformation( [ 3, 2, 5, 2, 4, 4 ] ), 
  Transformation( [ 4, 2, 3, 2, 5, 5 ] ), 
  Transformation( [ 2, 3, 4, 3, 5, 3 ] ), 
  Transformation( [ 2, 3, 5, 3, 4, 3 ] ), 
  Transformation( [ 2, 4, 5, 4, 3, 4 ] ), 
  Transformation( [ 3, 2, 4, 2, 5, 2 ] ), 
  Transformation( [ 3, 2, 5, 2, 4, 2 ] ), 
  Transformation( [ 4, 2, 3, 2, 5, 2 ] ), 
  Transformation( [ 3, 3, 3, 4, 5, 2 ] ), 
  Transformation( [ 3, 3, 3, 5, 4, 2 ] ), 
  Transformation( [ 4, 4, 4, 5, 3, 2 ] ), 
  Transformation( [ 2, 2, 2, 4, 5, 3 ] ), 
  Transformation( [ 2, 2, 2, 5, 4, 3 ] ), 
  Transformation( [ 2, 2, 2, 3, 5, 4 ] ), 
  Transformation( [ 3, 5, 3, 4, 4, 2 ] ), 
  Transformation( [ 3, 4, 3, 5, 5, 2 ] ), 
  Transformation( [ 4, 3, 4, 5, 5, 2 ] ), 
  Transformation( [ 2, 5, 2, 4, 4, 3 ] ), 
  Transformation( [ 2, 4, 2, 5, 5, 3 ] ), 
  Transformation( [ 2, 5, 2, 3, 3, 4 ] ), 
  Transformation( [ 3, 5, 3, 4, 3, 2 ] ), 
  Transformation( [ 3, 4, 3, 5, 3, 2 ] ), 
  Transformation( [ 4, 3, 4, 5, 4, 2 ] ), 
  Transformation( [ 2, 5, 2, 4, 2, 3 ] ), 
  Transformation( [ 2, 4, 2, 5, 2, 3 ] ), 
  Transformation( [ 2, 5, 2, 3, 2, 4 ] ), 
  Transformation( [ 2, 4, 3, 3, 5, 3 ] ), 
  Transformation( [ 2, 5, 3, 3, 4, 3 ] ), 
  Transformation( [ 2, 5, 4, 4, 3, 4 ] ), 
  Transformation( [ 3, 4, 2, 2, 5, 2 ] ), 
  Transformation( [ 3, 5, 2, 2, 4, 2 ] ), 
  Transformation( [ 4, 3, 2, 2, 5, 2 ] ), 
  Transformation( [ 5, 3, 4, 4, 2, 3 ] ), 
  Transformation( [ 4, 3, 5, 5, 2, 3 ] ), 
  Transformation( [ 3, 4, 5, 5, 2, 4 ] ), 
  Transformation( [ 5, 2, 4, 4, 3, 2 ] ), 
  Transformation( [ 4, 2, 5, 5, 3, 2 ] ), 
  Transformation( [ 5, 2, 3, 3, 4, 2 ] ), 
  Transformation( [ 2, 4, 3, 5, 5, 3 ] ), 
  Transformation( [ 2, 5, 3, 4, 4, 3 ] ), 
  Transformation( [ 2, 5, 4, 3, 3, 4 ] ), 
  Transformation( [ 3, 4, 2, 5, 5, 2 ] ), 
  Transformation( [ 3, 5, 2, 4, 4, 2 ] ), 
  Transformation( [ 4, 3, 2, 5, 5, 2 ] ), 
  Transformation( [ 2, 4, 3, 5, 3, 4 ] ), 
  Transformation( [ 2, 5, 3, 4, 3, 5 ] ), 
  Transformation( [ 2, 5, 4, 3, 4, 5 ] ), 
  Transformation( [ 3, 4, 2, 5, 2, 4 ] ), 
  Transformation( [ 3, 5, 2, 4, 2, 5 ] ), 
  Transformation( [ 4, 3, 2, 5, 2, 3 ] ), 
  Transformation( [ 2, 4, 3, 5, 3, 5 ] ), 
  Transformation( [ 2, 5, 3, 4, 3, 4 ] ), 
  Transformation( [ 2, 5, 4, 3, 4, 3 ] ), 
  Transformation( [ 3, 4, 2, 5, 2, 5 ] ), 
  Transformation( [ 3, 5, 2, 4, 2, 4 ] ), 
  Transformation( [ 4, 3, 2, 5, 2, 5 ] ), 
  Transformation( [ 5, 5, 3, 4, 3, 2 ] ), 
  Transformation( [ 4, 4, 3, 5, 3, 2 ] ), 
  Transformation( [ 3, 3, 4, 5, 4, 2 ] ), 
  Transformation( [ 5, 5, 2, 4, 2, 3 ] ), 
  Transformation( [ 4, 4, 2, 5, 2, 3 ] ), 
  Transformation( [ 5, 5, 2, 3, 2, 4 ] ), 
  Transformation( [ 4, 2, 3, 4, 4, 5 ] ), 
  Transformation( [ 5, 2, 3, 5, 5, 4 ] ), 
  Transformation( [ 5, 2, 4, 5, 5, 3 ] ), 
  Transformation( [ 4, 3, 2, 4, 4, 5 ] ), 
  Transformation( [ 5, 3, 2, 5, 5, 4 ] ), 
  Transformation( [ 3, 4, 2, 3, 3, 5 ] ), 
  Transformation( [ 5, 4, 3, 2, 4, 2 ] ), 
  Transformation( [ 4, 5, 3, 2, 5, 2 ] ), 
  Transformation( [ 3, 5, 4, 2, 5, 2 ] ), 
  Transformation( [ 5, 4, 2, 3, 4, 3 ] ), 
  Transformation( [ 4, 5, 2, 3, 5, 3 ] ), 
  Transformation( [ 5, 3, 2, 4, 3, 4 ] ), 
  Transformation( [ 5, 4, 4, 3, 2, 5 ] ), 
  Transformation( [ 4, 5, 5, 3, 2, 4 ] ), 
  Transformation( [ 3, 5, 5, 4, 2, 3 ] ), 
  Transformation( [ 5, 4, 4, 2, 3, 5 ] ), 
  Transformation( [ 4, 5, 5, 2, 3, 4 ] ), 
  Transformation( [ 5, 3, 3, 2, 4, 5 ] ), 
  Transformation( [ 2, 4, 3, 5, 4, 3 ] ), 
  Transformation( [ 2, 5, 3, 4, 5, 3 ] ), 
  Transformation( [ 2, 5, 4, 3, 5, 4 ] ), 
  Transformation( [ 3, 4, 2, 5, 4, 2 ] ), 
  Transformation( [ 3, 5, 2, 4, 5, 2 ] ), 
  Transformation( [ 4, 3, 2, 5, 3, 2 ] ), 
  Transformation( [ 3, 3, 4, 3, 5, 2 ] ), 
  Transformation( [ 3, 3, 5, 3, 4, 2 ] ), 
  Transformation( [ 4, 4, 5, 4, 3, 2 ] ), 
  Transformation( [ 2, 2, 4, 2, 5, 3 ] ), 
  Transformation( [ 2, 2, 5, 2, 4, 3 ] ), 
  Transformation( [ 2, 2, 3, 2, 5, 4 ] ), 
  Transformation( [ 3, 4, 4, 3, 2, 5 ] ), 
  Transformation( [ 3, 5, 5, 3, 2, 4 ] ), 
  Transformation( [ 4, 5, 5, 4, 2, 3 ] ), 
  Transformation( [ 2, 4, 4, 2, 3, 5 ] ), 
  Transformation( [ 2, 5, 5, 2, 3, 4 ] ), 
  Transformation( [ 2, 3, 3, 2, 4, 5 ] ), 
  Transformation( [ 5, 3, 4, 3, 5, 2 ] ), 
  Transformation( [ 4, 3, 5, 3, 4, 2 ] ), 
  Transformation( [ 3, 4, 5, 4, 3, 2 ] ), 
  Transformation( [ 5, 2, 4, 2, 5, 3 ] ), 
  Transformation( [ 4, 2, 5, 2, 4, 3 ] ), 
  Transformation( [ 5, 2, 3, 2, 5, 4 ] ), 
  Transformation( [ 5, 4, 3, 4, 2, 5 ] ), 
  Transformation( [ 4, 5, 3, 5, 2, 4 ] ), 
  Transformation( [ 3, 5, 4, 5, 2, 3 ] ), 
  Transformation( [ 5, 4, 2, 4, 3, 5 ] ), 
  Transformation( [ 4, 5, 2, 5, 3, 4 ] ), 
  Transformation( [ 5, 3, 2, 3, 4, 5 ] ), 
  Transformation( [ 5, 5, 4, 3, 5, 2 ] ), 
  Transformation( [ 4, 4, 5, 3, 4, 2 ] ), 
  Transformation( [ 3, 3, 5, 4, 3, 2 ] ), 
  Transformation( [ 5, 5, 4, 2, 5, 3 ] ), 
  Transformation( [ 4, 4, 5, 2, 4, 3 ] ), 
  Transformation( [ 5, 5, 3, 2, 5, 4 ] ), 
  Transformation( [ 4, 5, 3, 4, 5, 2 ] ), 
  Transformation( [ 5, 4, 3, 5, 4, 2 ] ), 
  Transformation( [ 5, 3, 4, 5, 3, 2 ] ), 
  Transformation( [ 4, 5, 2, 4, 5, 3 ] ), 
  Transformation( [ 5, 4, 2, 5, 4, 3 ] ), 
  Transformation( [ 3, 5, 2, 3, 5, 4 ] ), 
  Transformation( [ 3, 5, 3, 4, 5, 2 ] ), 
  Transformation( [ 3, 4, 3, 5, 4, 2 ] ), 
  Transformation( [ 4, 3, 4, 5, 3, 2 ] ), 
  Transformation( [ 2, 5, 2, 4, 5, 3 ] ), 
  Transformation( [ 2, 4, 2, 5, 4, 3 ] ), 
  Transformation( [ 2, 5, 2, 3, 5, 4 ] ) ]
gap> Number(HClassReps(d), x-> x in r);
6
gap> NrHClasses(r);
6
gap> enum:=EnumeratorOfRClasses(s);
<enumerator of R-classes of <non-regular transformation monoid of size 7008, 
 on 6 pts with 18 generators>>
gap> enum[1];
{Transformation( [ 6, 4, 3, 2, 5, 1 ] )}
gap> s:=Semigroup(gens);;
gap> enum:=EnumeratorOfRClasses(s);
<enumerator of R-classes of <transformation monoid on 6 pts with 18 generators
  >>
gap> enum[1];
{Transformation( [ 6, 4, 3, 2, 5, 1 ] )}
gap> enum[2];
{Transformation( [ 5, 3, 3, 2, 4, 1 ] )}
gap> Position(enum, enum[10]);
10
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
true
gap> ForAll([1..Length(enum)], x-> Position(enum, enum[x])=x);
true
gap> NrHClasses(r);
6

#
gap> gens:=
> [ PartialPermNC( [ 1, 2, 4 ], [ 2, 5, 3 ] ),
>  PartialPermNC( [ 1, 2, 4 ], [ 5, 6, 1 ] ),
>  PartialPermNC( [ 1, 2, 5 ], [ 5, 3, 2 ] ),
>  PartialPermNC( [ 1, 2, 3, 4 ], [ 5, 1, 2, 4 ] ),
>  PartialPermNC( [ 1, 2, 3, 4 ], [ 5, 1, 2, 6 ] ),
>  PartialPermNC( [ 1, 2, 3, 4 ], [ 5, 6, 4, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 5 ], [ 1, 5, 2, 6 ] ),
>  PartialPermNC( [ 1, 2, 3, 5 ], [ 2, 3, 4, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 5 ], [ 2, 5, 4, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 5 ], [ 5, 1, 2, 3 ] ),
>  PartialPermNC( [ 1, 2, 3, 6 ], [ 1, 4, 6, 5 ] ),
>  PartialPermNC( [ 1, 2, 5, 6 ], [ 6, 4, 2, 5 ] ),
>  PartialPermNC( [ 1, 3, 4, 6 ], [ 2, 3, 1, 6 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5 ], [ 3, 6, 5, 2, 4 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5 ], [ 6, 5, 3, 2, 1 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 6 ], [ 1, 3, 4, 6, 2 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 6 ], [ 1, 3, 6, 4, 5 ] ),
>  PartialPermNC( [ 1, 2, 4, 5, 6 ], [ 5, 4, 2, 1, 6 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6 ], [ 2, 5, 6, 4, 3, 1 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
12612
gap> f:=PartialPerm([ 1, 4, 6 ], [ 2, 3, 6 ]);;
gap> r:=RClass(s, f);
{PartialPerm( [ 1, 4, 6 ], [ 5, 3, 2 ] )}
gap> Size(r);
120
gap> NrHClasses(r);
20
gap> Number(HClassReps(s), x-> x in r);
20

#
gap> SemigroupsStopTest();

#
gap> Unbind(gens); Unbind(s); Unbind(f); Unbind(r); Unbind(l); Unbind(iter);
gap> STOP_TEST( "Semigroups package: misc.tst", 0);
