###########################################################################
##
#W  extreme/misc.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local D, H, K18g, L, P, a1, a2, a3, a4, a5, a6, acting, cosets, d, data, dd
#@local e, enum, f, g, gens, h, hh, i, iter, l, lambda_schutz, lambda_stab, m
#@local o, p, r, rep, reps, rho_schutz, rho_stab, rr, s, scc, schutz
gap> START_TEST("Semigroups package: extreme/misc.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();
gap> SEMIGROUPS.DefaultOptionsRec.acting := true;;

# MiscTest0
gap> gens := [Transformation([2, 8, 3, 7, 1, 5, 2, 6]),
>   Transformation([3, 5, 7, 2, 5, 6, 3, 8]),
>   Transformation([4, 1, 8, 3, 5, 7, 3, 5]),
>   Transformation([4, 3, 4, 5, 6, 4, 1, 2]),
>   Transformation([5, 4, 8, 8, 5, 6, 1, 5]),
>   Transformation([6, 7, 4, 1, 4, 1, 6, 2]),
>   Transformation([7, 1, 2, 2, 2, 7, 4, 5]),
>   Transformation([8, 8, 5, 1, 7, 5, 2, 8])];;
gap> s := Semigroup(gens);
<transformation semigroup of degree 8 with 8 generators>
gap> Size(s);
597369
gap> f := Transformation([8, 1, 5, 5, 8, 3, 7, 8]);;
gap> l := LClassNC(s, f);
<Green's L-class: Transformation( [ 8, 1, 5, 5, 8, 3, 7, 8 ] )>
gap> Transformation([8, 1, 5, 5, 8, 3, 7, 8]) in last;
true
gap> RhoOrbStabChain(l);
true
gap> Size(l);
4560
gap> RhoOrbSCC(l);
[ 1, 2, 5, 9, 10, 13, 14, 18, 24, 25, 22, 28, 34, 21, 11, 16, 12, 33, 32, 36, 
  3, 6, 39, 35, 37, 38, 29, 17, 23, 4, 7, 15, 19, 26, 30, 31, 27, 20 ]
gap> SchutzenbergerGroup(l);
Sym( [ 1, 3, 5, 7, 8 ] )
gap> ForAll(l, x -> x in l);
true
gap> d := DClass(s, f);
<Green's D-class: Transformation( [ 8, 1, 5, 5, 8, 3, 7, 8 ] )>
gap> Transformation([8, 1, 5, 5, 8, 3, 7, 8]) in last;
true
gap> iter := Iterator(d);
<iterator>
gap> for i in iter do od;

# MiscTest1
gap> gens := [PartialPermNC([1, 2, 3, 5, 7, 10], [12, 3, 1, 11, 9, 5]),
>  PartialPermNC([1, 2, 3, 4, 5, 7, 8], [4, 3, 11, 12, 6, 2, 1]),
>  PartialPermNC([1, 2, 3, 4, 5, 9, 11], [11, 6, 9, 2, 4, 8, 12]),
>  PartialPermNC([1, 2, 3, 4, 7, 9, 12], [7, 1, 12, 2, 9, 4, 5]),
>  PartialPermNC([1, 2, 3, 5, 7, 8, 9], [5, 4, 8, 11, 6, 12, 1]),
>  PartialPermNC([1, 2, 4, 6, 8, 9, 10], [8, 5, 2, 12, 4, 7, 11])];;
gap> s := Semigroup(gens);
<partial perm semigroup of rank 12 with 6 generators>
gap> f := PartialPermNC([3, 4, 5, 11], [4, 1, 2, 5]);;
gap> l := LClassNC(s, f);
<Green's L-class: [3,4,1][11,5,2]>
gap> l := LClass(s, f);
<Green's L-class: [3,4,1][11,5,2]>
gap> d := DClass(s, f);
<Green's D-class: [3,4,1][11,5,2]>
gap> Size(l);
1
gap> Number(d, x -> x in l);
1
gap> Number(s, x -> x in l);
1
gap> s := Semigroup(gens);
<partial perm semigroup of rank 12 with 6 generators>
gap> l := LClass(s, f);
<Green's L-class: [3,4,1][11,5,2]>
gap> d := DClass(s, f);
<Green's D-class: [3,4,1][11,5,2]>
gap> Number(d, x -> x in l);
1
gap> Number(s, x -> x in l);
1
gap> SchutzenbergerGroup(l);
Group(())
gap> ForAll(l, x -> x in l);
true
gap> d := DClassNC(s, f);
<Green's D-class: [3,4,1][11,5,2]>
gap> d := DClassNC(s, Representative(l));
<Green's D-class: [3,4,1][11,5,2]>
gap> ForAll(l, x -> x in d);
true
gap> Number(d, x -> x in l);
1
gap> Number(s, x -> x in l);
1

# MiscTest2
gap> gens := [Transformation([2, 8, 3, 7, 1, 5, 2, 6]),
>   Transformation([3, 5, 7, 2, 5, 6, 3, 8]),
>   Transformation([6, 7, 4, 1, 4, 1, 6, 2]),
>   Transformation([8, 8, 5, 1, 7, 5, 2, 8])];;
gap> s := Semigroup(gens);
<transformation semigroup of degree 8 with 4 generators>
gap> f := Transformation([5, 2, 7, 2, 7, 2, 5, 8]);;
gap> l := LClassNC(s, f);
<Green's L-class: Transformation( [ 5, 2, 7, 2, 7, 2, 5 ] )>
gap> Transformation([5, 2, 7, 2, 7, 2, 5]) in last;
true
gap> enum := Enumerator(l);
<enumerator of <Green's L-class: Transformation( [ 5, 2, 7, 2, 7, 2, 5 ] )>>
gap> enum[1];
Transformation( [ 5, 2, 7, 2, 7, 2, 5 ] )
gap> enum[2];
Transformation( [ 5, 8, 7, 8, 7, 8, 5, 2 ] )
gap> Position(enum, enum[2]);
2
gap> Position(enum, enum[1]);
1
gap> ForAll(enum, x -> enum[Position(enum, x)] = x);
true
gap> ForAll([1 .. Length(enum)], x -> Position(enum, enum[x]) = x);
true
gap> Length(enum);
1728
gap> ForAll(enum, x -> x in s);
true
gap> ForAll(l, x -> x in enum);
true
gap> Number(s, x -> x in enum);
1728
gap> Number(s, x -> x in l);
1728
gap> AsSet(l) = AsSet(enum);
true
gap> f := Transformation([7, 2, 4, 2, 2, 1, 7, 6]);;
gap> Position(enum, f);
fail
gap> GreensHClasses(l);
[ <Green's H-class: Transformation( [ 5, 2, 7, 2, 7, 2, 5 ] )>, 
  <Green's H-class: Transformation( [ 2, 8, 7, 5, 5, 7, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 8, 2, 7, 2, 2, 5, 8, 7 ] )>, 
  <Green's H-class: Transformation( [ 2, 7, 7, 8, 8, 2, 2, 5 ] )>, 
  <Green's H-class: Transformation( [ 8, 8, 7, 5, 5, 7, 2, 8 ] )>, 
  <Green's H-class: Transformation( [ 7, 5, 2, 8, 5, 7, 7, 8 ] )>, 
  <Green's H-class: Transformation( [ 5, 8, 2, 7, 7, 5, 5, 7 ] )>, 
  <Green's H-class: Transformation( [ 8, 7, 2, 5, 5, 7, 8, 5 ] )>, 
  <Green's H-class: Transformation( [ 7, 5, 2, 8, 8, 5, 7, 7 ] )>, 
  <Green's H-class: Transformation( [ 7, 2, 8, 2, 2, 5, 7, 7 ] )>, 
  <Green's H-class: Transformation( [ 2, 7, 8, 7, 7, 2, 2, 5 ] )>, 
  <Green's H-class: Transformation( [ 2, 5, 7, 5, 5, 7, 2 ] )>, 
  <Green's H-class: Transformation( [ 5, 8, 7, 2, 2, 5, 5, 7 ] )>, 
  <Green's H-class: Transformation( [ 8, 7, 7, 5, 5, 2, 8, 5 ] )>, 
  <Green's H-class: Transformation( [ 7, 5, 7, 8, 8, 5, 7, 2 ] )>, 
  <Green's H-class: Transformation( [ 5, 2, 7, 7, 7, 8, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 7, 8, 7, 5, 8, 5, 7, 2 ] )>, 
  <Green's H-class: Transformation( [ 8, 2, 7, 7, 7, 8, 8, 5 ] )>, 
  <Green's H-class: Transformation( [ 2, 5, 7, 8, 8, 7, 2, 8 ] )>, 
  <Green's H-class: Transformation( [ 5, 8, 7, 2, 2, 8, 5, 7 ] )>, 
  <Green's H-class: Transformation( [ 8, 7, 7, 5, 5, 2, 8, 8 ] )>, 
  <Green's H-class: Transformation( [ 7, 8, 7, 8, 8, 5, 7, 2 ] )>, 
  <Green's H-class: Transformation( [ 7, 5, 8, 7, 5, 2, 7, 8 ] )>, 
  <Green's H-class: Transformation( [ 7, 2, 5, 8, 2, 8, 7, 7 ] )>, 
  <Green's H-class: Transformation( [ 7, 5, 8, 7, 5, 2, 7, 5 ] )>, 
  <Green's H-class: Transformation( [ 5, 5, 8, 2, 2, 5, 5, 7 ] )>, 
  <Green's H-class: Transformation( [ 5, 7, 8, 5, 5, 2, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 7, 5, 8, 5, 5, 5, 7, 2 ] )>, 
  <Green's H-class: Transformation( [ 5, 2, 8, 7, 7, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 8, 5, 2, 5, 5, 7, 8, 5 ] )>, 
  <Green's H-class: Transformation( [ 8, 7, 5, 2, 7, 5, 8, 5 ] )>, 
  <Green's H-class: Transformation( [ 8, 5, 5, 7, 5, 2, 8, 5 ] )>, 
  <Green's H-class: Transformation( [ 8, 2, 5, 5, 2, 5, 8, 7 ] )>, 
  <Green's H-class: Transformation( [ 7, 2, 5, 8, 2, 5, 7, 7 ] )>, 
  <Green's H-class: Transformation( [ 2, 8, 7, 5, 8, 5, 2, 7 ] )>, 
  <Green's H-class: Transformation( [ 2, 5, 8, 7, 5, 7, 2, 5 ] )>, 
  <Green's H-class: Transformation( [ 2, 7, 5, 8, 7, 5, 2, 7 ] )>, 
  <Green's H-class: Transformation( [ 7, 2, 5, 8, 5, 8, 7, 8 ] )>, 
  <Green's H-class: Transformation( [ 2, 8, 5, 7, 7, 5, 2, 8 ] )>, 
  <Green's H-class: Transformation( [ 8, 8, 5, 2, 2, 7, 8, 5 ] )>, 
  <Green's H-class: Transformation( [ 8, 5, 5, 8, 8, 2, 8, 7 ] )>, 
  <Green's H-class: Transformation( [ 5, 7, 5, 8, 8, 8, 5, 2 ] )>, 
  <Green's H-class: Transformation( [ 7, 2, 5, 5, 5, 8, 7, 8 ] )>, 
  <Green's H-class: Transformation( [ 5, 8, 8, 5, 8, 2, 5, 7 ] )>, 
  <Green's H-class: Transformation( [ 7, 8, 7, 5, 5, 7, 7, 2 ] )>, 
  <Green's H-class: Transformation( [ 8, 2, 7, 7, 7, 5, 8, 7 ] )>, 
  <Green's H-class: Transformation( [ 2, 7, 7, 8, 8, 7, 2, 5 ] )>, 
  <Green's H-class: Transformation( [ 7, 5, 7, 2, 2, 8, 7, 7 ] )>, 
  <Green's H-class: Transformation( [ 5, 7, 7, 7, 7, 2, 5 ] )>, 
  <Green's H-class: Transformation( [ 7, 7, 5, 7, 7, 2, 7 ] )>, 
  <Green's H-class: Transformation( [ 7, 8, 5, 7, 7, 7, 7, 2 ] )>, 
  <Green's H-class: Transformation( [ 8, 2, 5, 7, 7, 7, 8, 7 ] )>, 
  <Green's H-class: Transformation( [ 2, 7, 5, 8, 8, 7, 2, 7 ] )>, 
  <Green's H-class: Transformation( [ 7, 7, 5, 2, 2, 8, 7, 7 ] )>, 
  <Green's H-class: Transformation( [ 5, 2, 7, 7, 2, 8, 5, 7 ] )>, 
  <Green's H-class: Transformation( [ 5, 8, 2, 7, 8, 7, 5, 7 ] )>, 
  <Green's H-class: Transformation( [ 8, 7, 2, 5, 5, 8, 8, 7 ] )>, 
  <Green's H-class: Transformation( [ 7, 7, 2, 8, 8, 5, 7, 8 ] )>, 
  <Green's H-class: Transformation( [ 7, 8, 2, 7, 7, 8, 7, 5 ] )>, 
  <Green's H-class: Transformation( [ 8, 5, 2, 7, 7, 7, 8, 8 ] )>, 
  <Green's H-class: Transformation( [ 2, 8, 5, 8, 8, 7, 2, 7 ] )>, 
  <Green's H-class: Transformation( [ 2, 7, 7, 8, 7, 8, 2, 5 ] )>, 
  <Green's H-class: Transformation( [ 2, 8, 7, 7, 8, 5, 2, 8 ] )>, 
  <Green's H-class: Transformation( [ 2, 5, 8, 7, 5, 8, 2, 7 ] )>, 
  <Green's H-class: Transformation( [ 7, 2, 7, 5, 2, 8, 7, 7 ] )>, 
  <Green's H-class: Transformation( [ 7, 8, 2, 7, 8, 7, 7, 5 ] )>, 
  <Green's H-class: Transformation( [ 5, 2, 8, 8, 2, 7, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 7, 5, 7, 2, 2, 8, 7, 2 ] )>, 
  <Green's H-class: Transformation( [ 5, 2, 7, 7, 7, 2, 5 ] )>, 
  <Green's H-class: Transformation( [ 7, 7, 5, 2, 7, 2, 7 ] )>, 
  <Green's H-class: Transformation( [ 7, 2, 7, 5, 2, 8, 7, 2 ] )>, 
  <Green's H-class: Transformation( [ 7, 8, 2, 7, 8, 2, 7, 5 ] )> ]
gap> Length(last);
72

# MiscTest3
gap> gens := [
>  PartialPermNC([1, 2, 3, 5, 7, 10], [12, 3, 1, 11, 9, 5]),
>  PartialPermNC([1, 2, 3, 4, 5, 7, 8], [4, 3, 11, 12, 6, 2, 1]),
>  PartialPermNC([1, 2, 3, 4, 5, 9, 11], [11, 6, 9, 2, 4, 8, 12]),
>  PartialPermNC([1, 2, 3, 4, 7, 9, 12], [7, 1, 12, 2, 9, 4, 5]),
>  PartialPermNC([1, 2, 3, 5, 7, 8, 9], [5, 4, 8, 11, 6, 12, 1]),
>  PartialPermNC([1, 2, 4, 6, 8, 9, 10], [8, 5, 2, 12, 4, 7, 11])];;
gap> s := Semigroup(gens);
<partial perm semigroup of rank 12 with 6 generators>
gap> Size(s);
4857
gap> f := PartialPerm([6, 9], [12, 6]);;
gap> l := LClass(s, f);
<Green's L-class: [9,6,12]>
gap> NrHClasses(l);
66
gap> Size(l);
66
gap> SchutzenbergerGroup(l);
Group([ (6,12) ])
gap> o := RhoOrb(l);
<closed orbit, 147 points with Schreier tree with log>
gap> d := DClassOfLClass(l);
<Green's D-class: [2,6][7,12]>
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
[ <Green's H-class: [2,6][7,12]>, <Green's H-class: [4,6][9,12]>, 
  <Green's H-class: [7,12][9,6]>, <Green's H-class: [1,12][7,6]>, 
  <Green's H-class: [1,12][2,6]>, <Green's H-class: [7,6][8,12]>, 
  <Green's H-class: [1,6][9,12]>, <Green's H-class: [2,12][4,6]>, 
  <Green's H-class: [4,12][8,6]>, <Green's H-class: [2,12][3,6]>, 
  <Green's H-class: [1,6][8,12]>, <Green's H-class: [3,12][9,6]>, 
  <Green's H-class: [5,12][9,6]>, <Green's H-class: [7,6](12)>, 
  <Green's H-class: [1,6][3,12]>, <Green's H-class: [2,6][8,12]>, 
  <Green's H-class: [1,12][4,6]>, <Green's H-class: [2,12][9,6]>, 
  <Green's H-class: [3,6][4,12]>, <Green's H-class: [4,12][7,6]>, 
  <Green's H-class: [8,12][9,6]>, <Green's H-class: [9,6,12]>, 
  <Green's H-class: [4,12][5,6]>, <Green's H-class: [9,12,6]>, 
  <Green's H-class: [3,12][11,6]>, <Green's H-class: [2,12][5,6]>, 
  <Green's H-class: [4,12,6]>, <Green's H-class: [5,12][11,6]>, 
  <Green's H-class: [1,12][5,6]>, <Green's H-class: [2,12,6]>, 
  <Green's H-class: [4,12](6)>, <Green's H-class: [4,12][11,6]>, 
  <Green's H-class: [8,12](6)>, <Green's H-class: [2,12][7,6]>, 
  <Green's H-class: [4,12][9,6]>, <Green's H-class: [7,6][9,12]>, 
  <Green's H-class: [1,6][7,12]>, <Green's H-class: [1,6][2,12]>, 
  <Green's H-class: [7,12][8,6]>, <Green's H-class: [1,12][9,6]>, 
  <Green's H-class: [2,6][4,12]>, <Green's H-class: [4,6][8,12]>, 
  <Green's H-class: [2,6][3,12]>, <Green's H-class: [1,12][8,6]>, 
  <Green's H-class: [3,6][9,12]>, <Green's H-class: [5,6][9,12]>, 
  <Green's H-class: [7,12,6]>, <Green's H-class: [1,12][3,6]>, 
  <Green's H-class: [2,12][8,6]>, <Green's H-class: [1,6][4,12]>, 
  <Green's H-class: [2,6][9,12]>, <Green's H-class: [3,12][4,6]>, 
  <Green's H-class: [4,6][7,12]>, <Green's H-class: [8,6][9,12]>, 
  <Green's H-class: [9,12](6)>, <Green's H-class: [4,6][5,12]>, 
  <Green's H-class: [9,6](12)>, <Green's H-class: [3,6][11,12]>, 
  <Green's H-class: [2,6][5,12]>, <Green's H-class: [4,6](12)>, 
  <Green's H-class: [5,6][11,12]>, <Green's H-class: [1,6][5,12]>, 
  <Green's H-class: [2,6](12)>, <Green's H-class: [4,6,12]>, 
  <Green's H-class: [4,6][11,12]>, <Green's H-class: [8,6,12]> ]
gap> IsDuplicateFreeList(last);
true
gap> IsRegularGreensClass(l);
false
gap> H := HClasses(l);;
gap> ForAll(H, x -> Representative(x) in l);
true
gap> ForAll(H, x -> Representative(x) in d);
true
gap> d;
<Green's D-class: [2,6][7,12]>
gap> Representative(l) in d;
true
gap> First(H, x -> not Representative(x) in d);
fail
gap> ForAll(l, x -> x in d);
true
gap> rep := Representative(d);
[2,6][7,12]
gap> s := Parent(d);
<partial perm semigroup of size 4857, rank 12 with 6 generators>
gap> ElementsFamily(FamilyObj(s)) <> FamilyObj(f)
> or RankOfPartialPerm(f) <> RankOfPartialPerm(rep);
false
gap> g := f;
[9,6,12]
gap> m := LambdaOrbSCCIndex(d);
54
gap> o := LambdaOrb(d);
<closed orbit, 184 points with Schreier tree with log>
gap> scc := OrbSCC(o);
[ [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 6 ], [ 7 ], [ 8 ], [ 9 ], [ 10 ], 
  [ 11 ], [ 12 ], [ 14 ], [ 15 ], [ 16 ], [ 17 ], [ 18 ], [ 19 ], [ 21 ], 
  [ 22 ], [ 23 ], [ 24 ], [ 25 ], [ 26 ], [ 27 ], [ 28 ], [ 29 ], [ 30 ], 
  [ 31 ], [ 32 ], [ 33 ], [ 34 ], [ 35 ], [ 36 ], [ 37 ], [ 38 ], [ 39 ], 
  [ 40 ], [ 41 ], [ 42 ], [ 43, 46, 44, 50, 20, 47, 69, 61, 63, 86, 78 ], 
  [ 45 ], [ 48 ], 
  [ 49, 83, 66, 82, 108, 87, 99, 80, 145, 84, 56, 64, 133, 147, 74, 96, 85, 
      146, 135, 62, 107, 110, 13, 154, 125, 101, 168, 159, 152, 137, 134, 
      171, 175 ], [ 51 ], [ 52 ], [ 53, 127, 121, 164, 177 ], [ 54 ], [ 55 ], 
  [ 57 ], [ 58 ], [ 59 ], [ 60 ], [ 65 ], [ 67 ], [ 68 ], [ 70 ], [ 71 ], 
  [ 72 ], [ 73 ], [ 75 ], [ 76 ], [ 77 ], [ 79 ], [ 81 ], [ 88 ], [ 89 ], 
  [ 90 ], [ 91 ], [ 94 ], [ 95 ], [ 97 ], [ 98 ], [ 100 ], [ 102 ], [ 103 ], 
  [ 104 ], [ 105 ], [ 106 ], [ 109 ], [ 111 ], [ 113 ], [ 114 ], [ 115 ], 
  [ 116 ], [ 117 ], [ 118 ], [ 119 ], [ 120 ], [ 122 ], [ 123 ], [ 124 ], 
  [ 126 ], [ 128 ], [ 129 ], [ 130 ], [ 131 ], [ 132 ], [ 136 ], [ 138 ], 
  [ 139 ], [ 140 ], [ 141 ], [ 142, 173, 179, 92, 150 ], [ 143 ], 
  [ 144, 112, 93 ], [ 148 ], [ 149 ], [ 151 ], [ 153 ], [ 155 ], [ 156 ], 
  [ 157 ], [ 158 ], [ 160 ], [ 161 ], [ 162 ], [ 163 ], [ 165 ], [ 166 ], 
  [ 167 ], [ 169 ], [ 170 ], [ 172 ], [ 174 ], [ 176 ], [ 178 ], [ 180 ], 
  [ 181 ], [ 182 ], [ 183 ], [ 184 ] ]
gap> l := Position(o, LambdaFunc(s)(g));
65
gap> l = fail or OrbSCCLookup(o)[l] <> m ;
false
gap> l <> scc[m][1];
false
gap> m := RhoOrbSCCIndex(d);
38
gap> o := RhoOrb(d);
<closed orbit, 147 points with Schreier tree with log>
gap> scc := OrbSCC(o);
[ [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 6 ], [ 7 ], [ 8 ], [ 9 ], 
  [ 10, 42, 45, 50, 48, 52, 53, 51, 115, 134, 144 ], [ 11 ], [ 14 ], [ 15 ], 
  [ 16 ], [ 17 ], [ 18 ], [ 19 ], [ 20 ], [ 21 ], [ 22 ], [ 23 ], [ 24 ], 
  [ 25 ], [ 26 ], [ 27 ], [ 28 ], [ 29 ], [ 30 ], [ 31 ], [ 32 ], [ 33 ], 
  [ 34 ], [ 36 ], [ 37 ], [ 38 ], [ 40 ], [ 41 ], 
  [ 43, 46, 102, 59, 69, 63, 12, 89, 62, 81, 103, 65, 112, 143, 54, 70, 47, 
      98, 74, 100, 120, 123, 121, 94, 122, 145, 132, 124, 93, 133, 128, 138, 
      125 ], [ 44 ], [ 49, 113, 85, 84, 75 ], [ 55 ], [ 56, 95, 135 ], 
  [ 57 ], [ 58 ], [ 60 ], [ 61, 119, 108, 64, 13 ], [ 66 ], [ 67 ], [ 68 ], 
  [ 71 ], [ 72 ], [ 73 ], [ 76 ], [ 77 ], [ 78 ], [ 79 ], [ 80 ], [ 82 ], 
  [ 83 ], [ 86 ], [ 87 ], [ 88 ], [ 90 ], [ 91 ], [ 92 ], [ 96 ], [ 97 ], 
  [ 99 ], [ 101 ], [ 104 ], [ 105 ], [ 106 ], [ 107 ], [ 109 ], [ 110 ], 
  [ 111, 129, 147, 35, 39 ], [ 114 ], [ 116 ], [ 117 ], [ 118 ], [ 126 ], 
  [ 127 ], [ 130 ], [ 131 ], [ 136 ], [ 137 ], [ 139 ], [ 140 ], [ 141 ], 
  [ 142 ], [ 146 ] ]
gap> l := Position(o, RhoFunc(s)(g));
123
gap> l = fail or OrbSCCLookup(o)[l] <> m;
false
gap> g := RhoOrbMult(o, m, l)[2] * g;;
gap> schutz := RhoOrbStabChain(d);
<stabilizer chain record, Base [ 12 ], Orbit length 2, Size: 2>
gap> l <> scc[m][1];
true
gap> cosets := LambdaCosets(d);
<enumerator of perm group>
gap> LambdaOrbStabChain(LambdaOrb(d), LambdaOrbSCCIndex(d));
false
gap> g := LambdaPerm(s)(rep, g);
()
gap> schutz <> false;
true
gap> o := LambdaOrb(d);
<closed orbit, 184 points with Schreier tree with log>
gap> m := LambdaOrbSCCIndex(d);
54
gap> lambda_schutz := LambdaOrbSchutzGp(o, m);
Group(())
gap> lambda_stab := LambdaOrbStabChain(o, m);
false
gap> o := RhoOrb(d);
<closed orbit, 147 points with Schreier tree with log>
gap> m := RhoOrbSCCIndex(d);
38
gap> rho_schutz := RhoOrbSchutzGp(o, m);
Group([ (1,12) ])
gap> rho_stab := RhoOrbStabChain(o, m);
true
gap> rho_stab = true;
true
gap> schutz := lambda_schutz;
Group(())
gap> lambda_stab = true;
false
gap> Parent(d) = s;
true
gap> PartialPerm([1, 9], [6, 12]) in d;
true
gap> RhoOrbRep(o, m);
[2,12][7,1]
gap> Representative(d);
[2,6][7,12]
gap> p := LambdaConjugator(Parent(d))(RhoOrbRep(o, m), Representative(d));;
gap> LambdaFunc(s)(RhoOrbRep(o, m));
[ 1, 12 ]
gap> OnSets(last, p);
[ 6, 12 ]
gap> LambdaFunc(s)(Representative(d));
[ 6, 12 ]
gap> rho_schutz := rho_schutz ^ p;
Group([ (6,12) ])
gap> f := PartialPermNC([6, 9], [12, 6]);
[9,6,12]
gap> s := Semigroup(gens);
<partial perm semigroup of rank 12 with 6 generators>
gap> l := LClass(s, f);
<Green's L-class: [9,6,12]>
gap> d := DClassOfLClass(l);
<Green's D-class: [9,6,12]>
gap> ForAll(l, x -> x in d);
true
gap> NrHClasses(l);
66
gap> RhoCosets(d);
<enumerator of perm group>
gap> Length(last);
2
gap> H := HClasses(l);;
gap> ForAll(H, x -> Representative(x) in l);
true
gap> ForAll(H, x -> Representative(x) in d);
true
gap> ForAll(H, x -> Representative(x) in s);
true
gap> ForAll(l, x -> x in l);
true

# MiscTest4
gap> gens := [Transformation([2, 8, 3, 7, 1, 5, 2, 6]),
>   Transformation([3, 5, 7, 2, 5, 6, 3, 8]),
>   Transformation([6, 7, 4, 1, 4, 1, 6, 2]),
>   Transformation([8, 8, 5, 1, 7, 5, 2, 8])];;
gap> s := Semigroup(gens);
<transformation semigroup of degree 8 with 4 generators>
gap> Size(s);
95540
gap> f := Transformation([2, 2, 7, 7, 7, 1, 2, 7]);;
gap> l := LClassNC(s, f);
<Green's L-class: Transformation( [ 2, 2, 7, 7, 7, 1, 2, 7 ] )>
gap> Transformation([2, 2, 7, 7, 7, 1, 2, 7]) in last;
true
gap> g := Transformation([2, 2, 7, 7, 7, 1, 2, 1]);;
gap> Size(l);
936
gap> h := GreensHClassOfElement(l, g);
<Green's H-class: Transformation( [ 2, 2, 7, 7, 7, 1, 2, 1 ] )>
gap> Transformation([2, 2, 7, 7, 7, 1, 2, 1]) in last;
true
gap> Size(h);
1
gap> SchutzenbergerGroup(l);
Sym( [ 1, 2, 7 ] )
gap> IsRegularGreensClass(l);
false
gap> IsGreensClassNC(h);
true
gap> ForAll(h, x -> x in l);
true
gap> ForAll(h, x -> x in s);
true
gap> SchutzenbergerGroup(h);
Group(())
gap> Idempotents(h);
[  ]
gap> IsGroupHClass(h);
false
gap> IsGreensHClass(h);
true
gap> GreensHRelation(s) = EquivalenceClassRelation(h);
true
gap> gens := [PartialPermNC([1, 2, 4, 5, 9], [3, 6, 2, 10, 5]),
>  PartialPermNC([1, 2, 3, 4, 7, 8], [10, 6, 7, 9, 4, 1]),
>  PartialPermNC([1, 2, 3, 4, 5, 6, 7, 9], [7, 2, 5, 6, 9, 3, 8, 10]),
>  PartialPermNC([1, 2, 3, 5, 6, 7, 8, 9], [10, 3, 7, 1, 5, 9, 2, 6]),
>  PartialPermNC([2, 3, 5, 6, 10], [4, 1, 9, 2, 5]),
>  PartialPermNC([1, 4, 6, 7, 9, 10], [8, 7, 2, 3, 4, 1])];;
gap> s := Semigroup(gens);
<partial perm semigroup of rank 10 with 6 generators>
gap> f := PartialPerm([]);;
gap> l := LClass(s, f);
<Green's L-class: <empty partial perm>>
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
gap> l := LClass(s, f);
<Green's L-class: <empty partial perm>>
gap> h := HClassNC(l, f);
<Green's H-class: <empty partial perm>>
gap> Size(h);
1
gap> ForAll(h, x -> x in l);
true
gap> ForAll(h, x -> x in s);
true
gap> IsGreensClassNC(h);
true
gap> f := PartialPermNC([2, 8, 9], [8, 10, 5]);;
gap> l := LClass(s, f);
<Green's L-class: [2,8,10][9,5]>
gap> h := HClassNC(l, f);
<Green's H-class: [2,8,10][9,5]>
gap> ForAll(h, x -> x in s);
true
gap> ForAll(h, x -> x in l);
true
gap> Size(h);
1

# MiscTest5
gap> gens := [Transformation([2, 6, 7, 2, 6, 1, 1, 5]),
>   Transformation([3, 8, 1, 4, 5, 6, 7, 1]),
>   Transformation([4, 3, 2, 7, 7, 6, 6, 5]),
>   Transformation([7, 1, 7, 4, 2, 5, 6, 3])];;
gap> s := Monoid(gens);
<transformation monoid of degree 8 with 4 generators>
gap> f := Transformation([5, 4, 7, 2, 2, 2, 2, 5]);;
gap> f in s;
true
gap> l := LClass(s, f);
<Green's L-class: Transformation( [ 5, 4, 7, 2, 2, 2, 2, 5 ] )>
gap> Transformation([5, 4, 7, 2, 2, 2, 2, 5]) in last;
true
gap> IsGreensClassNC(l);
false
gap> Size(l);
1
gap> f := Transformation([4, 3, 2, 7, 7, 6, 6, 5]);;
gap> l := LClass(s, f);
<Green's L-class: Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] )>
gap> Transformation([4, 3, 2, 7, 7, 6, 6, 5]) in last;
true
gap> Size(l);
1

# MiscTest6
gap> gens := [PartialPermNC([1, 2, 3], [1, 4, 3]),
>  PartialPermNC([1, 2, 3], [2, 3, 4]),
>  PartialPermNC([1, 2, 3], [4, 2, 1]),
>  PartialPermNC([1, 2, 4], [1, 4, 3])];;
gap> s := Semigroup(gens);
<partial perm semigroup of rank 4 with 4 generators>
gap> List(LClasses(s), IsRegularGreensClass);
[ false, false, false, false, true, true, false, false, true, true, false, 
  false, false, false, true, true, true, true, false, true ]
gap> Number(last, x -> x = true);
9
gap> GroupOfUnits(s);
fail
gap> List(LClasses(s), NrIdempotents);
[ 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1 ]
gap> NrIdempotents(s);
9
gap> List(LClasses(s), Size);
[ 1, 1, 1, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 1, 4, 4, 4, 4, 4, 1 ]
gap> Sum(last);
62
gap> Size(s);
62
gap> f := PartialPerm([2, 3], [2, 4]);;
gap> l := LClassNC(s, f);
<Green's L-class: [3,4](2)>
gap> HClassReps(l);
[ [3,4](2), [1,2,4], [3,2,4], [1,4](2) ]
gap> IsRegularGreensClass(l);
false
gap> NrHClasses(l);
4
gap> l := LClass(s, f);
<Green's L-class: [3,4](2)>
gap> HClassReps(l);
[ [1,4](2), [3,4](2), [1,2,4], [3,2,4] ]
gap> IsRegularGreensClass(l);
false
gap> ForAll(HClassReps(l), x -> x in l);
true
gap> d := DClassOfLClass(l);
<Green's D-class: [1,4](2)>
gap> Size(d);
4
gap> Size(l);
4
gap> AsSSortedList(l) = AsSortedList(d);
true
gap> AsSSortedList(d) = AsSSortedList(l);
true
gap> l < d;
false
gap> ForAll(l, x -> x in d);
true
gap> ForAll(d, x -> x in l);
true
gap> HClassReps(d) = HClassReps(l);
true
gap> NrRClasses(d);
4
gap> NrLClasses(d);
1
gap> NrHClasses(d);
4

# MiscTest7
gap> gens := [Transformation([1, 5, 6, 2, 5, 2, 1]),
>   Transformation([1, 7, 5, 4, 3, 5, 7]),
>   Transformation([2, 7, 7, 2, 4, 1, 1]),
>   Transformation([3, 2, 2, 4, 1, 7, 6]),
>   Transformation([3, 3, 5, 1, 7, 1, 6]),
>   Transformation([3, 3, 6, 1, 7, 5, 2]),
>   Transformation([3, 4, 6, 5, 4, 4, 7]),
>   Transformation([5, 2, 4, 5, 1, 4, 5]),
>   Transformation([5, 5, 2, 2, 6, 7, 2]),
>   Transformation([7, 7, 5, 4, 5, 3, 2])];;
gap> s := Semigroup(gens);;
gap> l := LClasses(s)[1154];
<Green's L-class: Transformation( [ 7, 2, 2, 3, 6, 1, 2 ] )>
gap> Transformation([7, 2, 2, 3, 6, 1, 2]) in last;
true
gap> IsRegularGreensClass(l);
false
gap> d := DClassOfLClass(l);
<Green's D-class: Transformation( [ 7, 2, 2, 3, 6, 1, 2 ] )>
gap> Transformation([7, 2, 2, 3, 6, 1, 2]) in last;
true
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
gap> l := LClasses(s)[523];
<Green's L-class: Transformation( [ 5, 5, 5, 1, 7, 3, 6 ] )>
gap> Transformation([5, 5, 5, 1, 7, 3, 6]) in last;
true
gap> Size(l);
1

# MiscTest8
gap> gens := [PartialPermNC([1, 2, 3, 5], [5, 7, 3, 4]),
>  PartialPermNC([1, 2, 3, 4, 5], [6, 4, 1, 2, 7]),
>  PartialPermNC([1, 2, 3, 4, 7], [2, 7, 4, 5, 8]),
>  PartialPermNC([1, 2, 3, 5, 6], [5, 6, 1, 4, 3]),
>  PartialPermNC([1, 2, 4, 6, 7], [2, 1, 6, 7, 4]),
>  PartialPermNC([1, 3, 5, 6, 7], [6, 2, 3, 5, 7]),
>  PartialPermNC([1, 2, 3, 4, 5, 7], [4, 1, 6, 2, 8, 5]),
>  PartialPermNC([1, 2, 3, 4, 5, 8], [5, 6, 3, 8, 2, 7]),
>  PartialPermNC([1, 2, 3, 4, 6, 7], [1, 5, 2, 6, 7, 4]),
>  PartialPermNC([1, 2, 3, 4, 5, 6, 8], [7, 5, 2, 8, 4, 1, 3])];;
gap> s := Semigroup(gens);
<partial perm semigroup of rank 8 with 10 generators>
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
gap> f := PartialPerm([3, 4, 7], [4, 7, 8]);;
gap> d := DClass(s, f);
<Green's D-class: [3,4,7,8]>
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
[ (), (4,8), (4,7,8), (7,8), (4,8,7), (4,7) ]
gap> SchutzenbergerGroup(d);
Group(())
gap> RhoOrbStabChain(d);
<stabilizer chain record, Base [ 7, 8 ], Orbit length 3, Size: 6>
gap> data := SemigroupData(Parent(d));
<closed semigroup data with 25643 reps, 178 lambda-values, 150 rho-values>
gap> OrbSCC(data)[OrbSCCLookup(data)[SemigroupDataIndex(d)]];
[ 33, 144, 340, 568, 35, 151, 353, 540, 1088, 1900, 1342, 2195, 1043, 1151, 
  1336, 2189, 1361, 1902, 713, 1346, 561, 711, 1343, 519, 706, 1333, 553, 
  720, 539, 1086, 571, 1158, 560, 1134, 1973, 1337, 1842, 1040, 725, 1362, 
  1904, 1357, 1202, 1102, 1367, 2196, 1840, 717, 1181, 1339, 2192, 544, 1103, 
  1932, 2888, 1360, 729, 1366, 1124, 1958, 2214, 1126, 715, 1352, 2204, 1340, 
  1013, 1368, 503, 1014, 1801, 2750, 3674, 1335, 2188, 2997, 1344, 356, 727, 
  1090, 1905, 358, 731, 1139, 1911, 1041, 712, 1347, 1371, 721, 1359, 2213, 
  1838, 2788, 1045, 1837, 357, 728, 1364, 1349, 2052, 2009, 1903, 2859, 2873, 
  707, 718, 341, 708, 1338, 345, 719, 147, 344, 1356, 2208, 1978, 2203, 1369, 
  2217, 2862, 1091, 1907, 710, 1341, 2193, 3127, 343, 714, 1351, 2202, 2191, 
  3126, 724, 2037, 4041, 726, 1363, 2215, 3767, 2057, 2198, 2058, 2039, 2990, 
  709, 1137, 1976, 1841, 2794, 2936, 2973, 1974, 1089, 1901, 2855, 2210, 
  1899, 1831, 2209, 3132, 2866, 730, 1370, 2218, 1348, 2199, 1136, 1975, 
  3137, 1358, 2212, 3133, 4044, 1365, 2795, 1928, 2065, 3019, 2884, 1898, 
  1909, 1046, 1839, 342, 1977, 2938, 2792, 148, 346, 1908, 2861, 2939, 3850, 
  2010, 2974, 3891, 2920, 2051, 3003, 1929, 2885, 2216, 3135, 1910, 2863, 
  3004, 3124, 3704, 3916, 2791, 3708, 4573, 5517, 4042, 2190, 3125, 4040, 
  2749, 4043, 4703, 4807, 3925, 2889, 3766, 2783, 2782, 3910, 2789, 2206, 
  3130, 1979, 3706, 1355, 2207, 3131, 2194, 3128, 2856, 3765, 2857, 2201, 
  2937, 2790, 3707, 2205, 2200, 1092, 2864, 1913, 1345, 2197, 1833, 3136, 
  3134, 1960, 2921, 3831, 2865, 1906, 2860, 1832, 1918, 1140, 1800, 1353, 
  1959, 1933, 1354, 2793, 1105, 2211, 1135, 1087, 1334, 1350, 2858, 1157, 
  3129, 355, 152, 146 ]
gap> Position(DClasses(s), d);
17
gap> d := DClasses(s)[18];
<Green's D-class: [1,2][3,7,5][6,8]>
gap> OrbSCC(data)[OrbSCCLookup(data)[SemigroupDataIndex(d)]];
[ 36 ]
gap> LambdaCosets(d);
<enumerator of perm group>
gap> LambdaOrbSCC(d);
[ 22 ]
gap> RhoOrbSCC(d);
[ 35 ]
gap> ForAll(d, x -> x in d);
true
gap> enum := Enumerator(d);
<enumerator of <Green's D-class: [1,2][3,7,5][6,8]>>
gap> enum[1];
[1,2][3,7,5][6,8]
gap> Length(enum);
1
gap> Size(d);
1
gap> ForAll(enum, x -> enum[Position(enum, x)] = x);
true
gap> s := Semigroup(gens);
<partial perm semigroup of rank 8 with 10 generators>
gap> d := DClass(s, PartialPerm([1, 3, 6], [7, 4, 8]));
<Green's D-class: [1,7][3,4][6,8]>
gap> enum := Enumerator(d);
<enumerator of <Green's D-class: [1,7][3,4][6,8]>>
gap> ForAll(enum, x -> enum[Position(enum, x)] = x);
true
gap> ForAll([1 .. Length(enum)], x -> Position(enum, enum[x]) = x);
true
gap> enum[1];
[1,7][3,4][6,8]
gap> enum[2];
[2,8][3,7][6,4]
gap> Position(enum, enum[2]);
2
gap> Position(enum, enum[3]);
3
gap> enum[3];
[1,4][3,8][5,7]
gap> enum[4];
[2,7][6,4](8)
gap> for d in DClasses(s) do
>   enum := Enumerator(d);
>   if not ForAll(enum, x -> enum[Position(enum, x)] = x) then
>     Print("problem with enumerator of a D-class 1\n");
>   fi;
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

# MiscTest9
gap> gens := [Transformation([3, 4, 1, 2, 1]),
>   Transformation([4, 2, 1, 5, 5]),
>   Transformation([4, 2, 2, 2, 4])];;
gap> s := Semigroup(gens);;
gap> for d in DClasses(s) do
> enum := Enumerator(d);
> if not ForAll(enum, x -> enum[Position(enum, x)] = x) then
> Print("problem with enumerator of a D-class 1\n");
> fi;
> od;
gap> gens := [PartialPermNC([1, 2, 3, 6, 8, 10], [2, 6, 7, 9, 1, 5]),
> PartialPermNC([1, 2, 3, 4, 5, 8, 10], [7, 1, 4, 3, 2, 6, 5]),
> PartialPermNC([1, 2, 3, 4, 6, 7, 8, 10], [3, 8, 1, 9, 4, 10, 5, 6])];;
gap> s := Semigroup(gens);;
gap> f := PartialPerm([2, 4], [6, 5]);;
gap> d := DClassNC(s, f);
<Green's D-class: [2,6][4,5]>
gap> GreensHClasses(d);
[ <Green's H-class: [2,6][4,5]> ]
gap> Size(d);
1

# MiscTest10
gap> gens :=
> [PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 19,
>   20, 24, 25, 26, 27, 28, 29, 31, 32, 34, 35, 36, 37, 38, 40, 43, 45, 46, 49,
>   50, 51, 53, 55, 56, 57, 58, 59, 60, 61, 64, 66, 68, 69, 70, 72, 73, 74, 77,
>   80, 81, 83, 86, 87, 89, 91, 98], [89, 70, 79, 27, 84, 99, 9, 73, 33, 77,
>   69, 41, 18, 63, 29, 42, 75, 56, 90, 64, 98, 49, 35, 100, 71, 3, 20, 2, 26,
>   11, 39, 7, 48, 85, 8, 10, 61, 25, 55, 92, 62, 21, 34, 57, 44, 14, 53, 59,
>   12, 87, 78, 83, 30, 32, 68, 86, 23, 47, 93, 15, 76, 97, 91]),
>  PartialPermNC([1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
>   19, 20, 22, 23, 24, 25, 28, 30, 31, 33, 34, 35, 36, 39, 40, 42, 43, 44, 45,
>   46, 47, 50, 53, 54, 55, 58, 59, 64, 65, 67, 69, 70, 71, 72, 73, 76, 77, 78,
>   81, 82, 84, 85, 86, 87, 89, 92, 94, 95], [5, 13, 94, 44, 80, 54, 99, 81,
>   31, 7, 90, 30, 46, 68, 36, 11, 100, 17, 87, 72, 14, 29, 9, 61, 91, 32, 43,
>   64, 60, 41, 26, 40, 8, 23, 63, 38, 57, 12, 59, 83, 92, 96, 18, 3, 65, 2,
>   37, 21, 49, 16, 75, 24, 27, 1, 48, 6, 35, 79, 82, 51, 39, 25, 77, 62, 22]),
>  PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
>   18, 19, 20, 21, 23, 24, 26, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
>   40, 42, 44, 48, 51, 52, 53, 55, 56, 57, 58, 60, 63, 64, 65, 66, 67, 71, 73,
>   75, 77, 80, 82, 83, 85, 86, 90, 91, 96, 97, 98, 99],
> [67, 93, 18, 59, 86, 16, 99, 73, 60, 74, 17, 95, 85, 49, 79, 4, 33, 66, 15,
>   44, 77, 41, 55, 84, 68, 69, 94, 31, 2, 29, 5, 42, 10, 63, 58, 34, 72, 53,
>   89, 57, 62, 76, 20, 52, 22, 35, 75, 98, 78, 40, 46, 28, 6, 90, 12, 65, 26,
>   36, 25, 61, 83, 38, 39, 87, 92, 97, 43, 30])];;
gap> s := Semigroup(gens);;
gap> f := PartialPerm([12, 27, 37, 40, 46, 50, 51, 53],
> [98, 3, 84, 99, 100, 21, 70, 89]);;
gap> d := DClassNC(s, f);
<Green's D-class: [12,98][27,3][37,84][40,99][46,100][50,21][51,70][53,89]>
gap> Size(d);
1
gap> GreensHClasses(d);
[ <Green's H-class: [12,98][27,3][37,84][40,99][46,100][50,21][51,70][53,89]> 
 ]
gap> iter := IteratorOfDClasses(s);
<iterator>
gap> repeat d := NextIterator(iter); until Size(d) > 1;
gap> d;
<Green's D-class: [8,63][57,87]>
gap> Size(d);
2036
gap> IsRegularDClass(d);
false
gap> GreensHClasses(d);;
gap> NrHClasses(d);
2036
gap> GreensLClasses(d);
[ <Green's L-class: [8,63][57,87]> ]

# MiscTest11
gap> gens := [Transformation([1, 3, 4, 1]),
> Transformation([2, 4, 1, 2]),
> Transformation([3, 1, 1, 3]),
> Transformation([3, 3, 4, 1])];;
gap> s := Monoid(gens);;
gap> List(GreensDClasses(s), LClasses);
[ [ <Green's L-class: IdentityTransformation> ], 
  [ <Green's L-class: Transformation( [ 1, 3, 4, 1 ] )>, 
      <Green's L-class: Transformation( [ 4, 1, 3, 4 ] )>, 
      <Green's L-class: Transformation( [ 3, 4, 1, 3 ] )> ], 
  [ <Green's L-class: Transformation( [ 2, 4, 1, 2 ] )> ], 
  [ <Green's L-class: Transformation( [ 3, 1, 1, 3 ] )>, 
      <Green's L-class: Transformation( [ 1, 4, 4, 1 ] )>, 
      <Green's L-class: Transformation( [ 2, 1, 1, 2 ] )>, 
      <Green's L-class: Transformation( [ 2, 4, 4, 2 ] )>, 
      <Green's L-class: Transformation( [ 4, 3, 3, 4 ] )> ], 
  [ <Green's L-class: Transformation( [ 3, 3, 4, 1 ] )> ], 
  [ <Green's L-class: Transformation( [ 1, 1, 1, 1 ] )>, 
      <Green's L-class: Transformation( [ 2, 2, 2, 2 ] )>, 
      <Green's L-class: Transformation( [ 3, 3, 3, 3 ] )>, 
      <Green's L-class: Transformation( [ 4, 4, 4, 4 ] )> ] ]
gap> List(Concatenation(last), Size);
[ 1, 1, 1, 1, 1, 10, 10, 10, 10, 10, 3, 1, 1, 1, 1 ]
gap> Sum(last);
62
gap> Size(s);
62
gap> l := Concatenation(List(GreensDClasses(s), LClasses));
[ <Green's L-class: IdentityTransformation>, 
  <Green's L-class: Transformation( [ 1, 3, 4, 1 ] )>, 
  <Green's L-class: Transformation( [ 4, 1, 3, 4 ] )>, 
  <Green's L-class: Transformation( [ 3, 4, 1, 3 ] )>, 
  <Green's L-class: Transformation( [ 2, 4, 1, 2 ] )>, 
  <Green's L-class: Transformation( [ 3, 1, 1, 3 ] )>, 
  <Green's L-class: Transformation( [ 1, 4, 4, 1 ] )>, 
  <Green's L-class: Transformation( [ 2, 1, 1, 2 ] )>, 
  <Green's L-class: Transformation( [ 2, 4, 4, 2 ] )>, 
  <Green's L-class: Transformation( [ 4, 3, 3, 4 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 4, 1 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 3, 3 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 4, 4 ] )> ]
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
gap> last = AsSSortedList(s);
true

# MiscTest12
gap> gens := [PartialPermNC([1, 2, 3, 4], [5, 7, 1, 6]),
> PartialPermNC([1, 2, 3, 5], [5, 2, 7, 3]),
> PartialPermNC([1, 2, 3, 6, 7], [1, 3, 4, 7, 5]),
> PartialPermNC([1, 2, 3, 4, 5, 7], [3, 2, 4, 6, 1, 5])];;
gap> s := Semigroup(gens);;
gap> Size(s);
840
gap> NrDClasses(s);
176

# MiscTest13
gap> gens := [PartialPermNC([1, 2, 3, 4], [5, 7, 1, 6]),
> PartialPermNC([1, 2, 3, 5], [5, 2, 7, 3]),
> PartialPermNC([1, 2, 3, 6, 7], [1, 3, 4, 7, 5]),
> PartialPermNC([1, 2, 3, 4, 5, 7], [3, 2, 4, 6, 1, 5])];;
gap> s := Semigroup(gens);;
gap> Size(s);
840
gap> NrDClasses(s);
176
gap> List(DClasses(s), RClasses);
[ [ <Green's R-class: [2,7][3,1,5][4,6]> ], 
  [ <Green's R-class: [1,5,3,7](2)> ], 
  [ <Green's R-class: [2,3,4][6,7,5](1)> ], 
  [ <Green's R-class: [7,5,1,3,4,6](2)> ], 
  [ <Green's R-class: [3,5]>, 
      <Green's R-class: <identity partial perm on [ 5 ]>>, 
      <Green's R-class: [1,5]>, <Green's R-class: [7,5]>, 
      <Green's R-class: [6,5]>, <Green's R-class: [4,5]> ], 
  [ <Green's R-class: [1,3][2,5]>, <Green's R-class: [2,5,3]>, 
      <Green's R-class: [2,5][7,3]>, <Green's R-class: [2,5](3)> ], 
  [ <Green's R-class: [2,1,5,6]>, <Green's R-class: [2,1][7,6](5)>, 
      <Green's R-class: [2,1,5][3,6]>, <Green's R-class: [2,1,6](5)>, 
      <Green's R-class: [2,1][7,5,6]>, <Green's R-class: [2,1,6][3,5]> ], 
  [ <Green's R-class: [2,7][3,6](1)(5)> ], 
  [ <Green's R-class: [1,3,5]>, <Green's R-class: [1,5,3]>, 
      <Green's R-class: [1,5][7,3]>, <Green's R-class: [1,5][6,3]>, 
      <Green's R-class: [4,3,5]>, <Green's R-class: [4,3](5)>, 
      <Green's R-class: [7,5](3)>, <Green's R-class: (3,5)>, 
      <Green's R-class: [7,3](5)> ], 
  [ <Green's R-class: [1,3][5,7](2)>, <Green's R-class: [5,3](2)(7)>, 
      <Green's R-class: [1,3,7](2)>, <Green's R-class: [1,7][5,3](2)>, 
      <Green's R-class: [5,7,3](2)>, <Green's R-class: [1,7](2)(3)> ], 
  [ <Green's R-class: [1,5][2,7,3]> ], [ <Green's R-class: [1,7,3](2)(5)> ], 
  [ <Green's R-class: [2,5][3,1][4,7]> ], [ <Green's R-class: [2,3,5,4]> ], 
  [ <Green's R-class: [2,4][6,5](1)> ], [ <Green's R-class: [2,3][5,1,4,7]> ],
  [ <Green's R-class: [2,5,3](1)>, <Green's R-class: [2,5,1][7,3]>, 
      <Green's R-class: [2,5](1)(3)>, <Green's R-class: [2,5,1,3]>, 
      <Green's R-class: [2,5,3][7,1]>, <Green's R-class: [2,5](1,3)> ], 
  [ <Green's R-class: [3,5,4](1)(2)> ], 
  [ <Green's R-class: [2,4][7,1,3,6,5]> ], 
  [ <Green's R-class: [5,3,6][7,1,4](2)> ], 
  [ <Green's R-class: <empty partial perm>> ], [ <Green's R-class: [2,5]> ], 
  [ <Green's R-class: [1,5][2,6]>, <Green's R-class: [2,6](5)>, 
      <Green's R-class: [2,6][7,5]>, <Green's R-class: [2,6][3,5]> ], 
  [ <Green's R-class: [1,5,6][2,7]>, <Green's R-class: [2,7,6](5)>, 
      <Green's R-class: [1,5][2,7][3,6]>, <Green's R-class: [1,6][2,7](5)>, 
      <Green's R-class: [2,7,5,6]>, <Green's R-class: [1,6][2,7][3,5]> ], 
  [ <Green's R-class: [2,6][7,5](1)> ], [ <Green's R-class: [2,7,5,1,6]> ], 
  [ <Green's R-class: [1,7](2)>, <Green's R-class: [5,7](2)>, 
      <Green's R-class: <identity partial perm on [ 2, 7 ]>>, 
      <Green's R-class: [3,7](2)> ], 
  [ <Green's R-class: [1,5,7][2,3]>, <Green's R-class: [2,3](5)(7)>, 
      <Green's R-class: [1,5][2,3,7]>, <Green's R-class: [1,7][2,3](5)>, 
      <Green's R-class: [2,3](5,7)>, <Green's R-class: [1,7][2,3,5]> ], 
  [ <Green's R-class: [1,5,3](2)>, <Green's R-class: [7,3](2)(5)>, 
      <Green's R-class: [1,5](2)(3)>, <Green's R-class: [1,3](2)(5)>, 
      <Green's R-class: [7,5,3](2)>, <Green's R-class: [1,3,5](2)> ], 
  [ <Green's R-class: [1,7,5][6,3]> ], 
  [ <Green's R-class: [1,7](2)(5)>, <Green's R-class: (2)(5,7)>, 
      <Green's R-class: [1,7][3,5](2)> ], 
  [ <Green's R-class: [2,5,7](1)>, <Green's R-class: [2,5,1](7)>, 
      <Green's R-class: [2,5][3,7](1)>, <Green's R-class: [2,5,1,7]>, 
      <Green's R-class: [2,5,7,1]>, <Green's R-class: [2,5][3,1,7]> ], 
  [ <Green's R-class: [1,4][2,3](5)>, <Green's R-class: [2,3][7,5,4]>, 
      <Green's R-class: [1,4][2,3,5]>, <Green's R-class: [1,5,4][2,3]>, 
      <Green's R-class: [2,3][7,4](5)>, <Green's R-class: [1,5][2,3,4]> ], 
  [ <Green's R-class: [1,5][2,3][7,4]> ], [ <Green's R-class: [2,4,5,1]> ], 
  [ <Green's R-class: [3,4](1)>, <Green's R-class: [5,1,4]>, 
      <Green's R-class: [7,1,4]>, <Green's R-class: [6,1,4]>, 
      <Green's R-class: [3,4,1]>, <Green's R-class: [5,4,1]>, 
      <Green's R-class: [3,1][7,4]>, <Green's R-class: [3,4][5,1]>, 
      <Green's R-class: [5,4][7,1]>, <Green's R-class: [3,1,4]>, 
      <Green's R-class: [5,4](1)>, <Green's R-class: [7,4](1)>, 
      <Green's R-class: [6,4](1)>, <Green's R-class: [3,1](4)>, 
      <Green's R-class: [5,1](4)>, <Green's R-class: [3,4][7,1]>, 
      <Green's R-class: [3,1][5,4]>, <Green's R-class: [5,1][7,4]> ], 
  [ <Green's R-class: [3,7,1,4]> ], [ <Green's R-class: [2,3,7,1][5,4]> ], 
  [ <Green's R-class: [1,4](2)(5)>, <Green's R-class: [7,5,4](2)>, 
      <Green's R-class: [1,4][3,5](2)>, <Green's R-class: [1,5,4](2)>, 
      <Green's R-class: [7,4](2)(5)>, <Green's R-class: [1,5][3,4](2)> ], 
  [ <Green's R-class: [2,5][7,4](1)> ], [ <Green's R-class: [7,4](1,5)(2)> ], 
  [ <Green's R-class: [2,1][4,5](3)> ], [ <Green's R-class: [2,4][3,1][5,6]> ]
    , [ <Green's R-class: [2,6,1,3]> ], [ <Green's R-class: [1,6][2,4,5,3]> ],
  [ <Green's R-class: [2,1,3][5,4]>, <Green's R-class: [2,1][5,3][7,4]>, 
      <Green's R-class: [2,1,3,4]>, <Green's R-class: [2,1,4][5,3]>, 
      <Green's R-class: [2,1][5,4][7,3]>, <Green's R-class: [2,1,4](3)> ], 
  [ <Green's R-class: [5,6](1,3)(2)> ], [ <Green's R-class: [2,6,1,4][7,3]> ],
  [ <Green's R-class: [1,6][5,4][7,3](2)> ], 
  [ <Green's R-class: [1,5][3,6]>, <Green's R-class: [1,6](5)>, 
      <Green's R-class: [1,6][7,5]>, <Green's R-class: [1,6,5]>, 
      <Green's R-class: [3,6][4,5]>, <Green's R-class: [4,5,6]>, 
      <Green's R-class: [3,5][7,6]>, <Green's R-class: [3,6](5)>, 
      <Green's R-class: [7,5,6]>, <Green's R-class: [1,6][3,5]>, 
      <Green's R-class: [1,5,6]>, <Green's R-class: [1,5][7,6]>, 
      <Green's R-class: [1,5](6)>, <Green's R-class: [3,5][4,6]>, 
      <Green's R-class: [4,6](5)>, <Green's R-class: [3,6][7,5]>, 
      <Green's R-class: [3,5,6]>, <Green's R-class: [7,6](5)> ], 
  [ <Green's R-class: [1,6][2,7]>, <Green's R-class: [2,7][5,6]>, 
      <Green's R-class: [2,7,6]>, <Green's R-class: [2,7][3,6]> ], 
  [ <Green's R-class: [2,5,6](1)>, <Green's R-class: [2,5,1][7,6]>, 
      <Green's R-class: [2,5][3,6](1)>, <Green's R-class: [2,5,1,6]>, 
      <Green's R-class: [2,5,6][7,1]>, <Green's R-class: [2,5][3,1,6]> ], 
  [ <Green's R-class: [2,7](1)(5)>, <Green's R-class: [2,7,5,1]>, 
      <Green's R-class: [2,7][3,5](1)>, <Green's R-class: [2,7](1,5)>, 
      <Green's R-class: [2,7,1](5)>, <Green's R-class: [2,7][3,1,5]> ], 
  [ <Green's R-class: [7,1,6,5]> ], 
  [ <Green's R-class: [2,7][5,1,6]>, <Green's R-class: [2,7,1][5,6]>, 
      <Green's R-class: [2,7][3,1,6]>, <Green's R-class: [2,7][5,6](1)>, 
      <Green's R-class: [2,7,6][5,1]>, <Green's R-class: [2,7][3,6](1)> ], 
  [ <Green's R-class: <identity partial perm on [ 2 ]>> ], 
  [ <Green's R-class: [6,3][7,5]> ], [ <Green's R-class: [2,5][4,3,7]> ], 
  [ <Green's R-class: [2,4](1)>, <Green's R-class: [2,4][5,1]>, 
      <Green's R-class: [2,4][7,1]>, <Green's R-class: [2,4][3,1]> ], 
  [ <Green's R-class: [2,4][3,5][7,1]> ], 
  [ <Green's R-class: [2,1,4]>, <Green's R-class: [2,1][5,4]>, 
      <Green's R-class: [2,1][7,4]>, <Green's R-class: [2,1][3,4]> ], 
  [ <Green's R-class: [2,7][6,1,4]> ], 
  [ <Green's R-class: [1,4][3,7]>, <Green's R-class: [1,7][5,4]>, 
      <Green's R-class: [1,7,4]>, <Green's R-class: [1,7][6,4]>, 
      <Green's R-class: [3,7](4)>, <Green's R-class: [5,7](4)>, 
      <Green's R-class: [3,4](7)>, <Green's R-class: [3,7][5,4]>, 
      <Green's R-class: [5,7,4]>, <Green's R-class: [1,7][3,4]>, 
      <Green's R-class: [1,4][5,7]>, <Green's R-class: [1,4](7)>, 
      <Green's R-class: [1,4][6,7]>, <Green's R-class: [3,4,7]>, 
      <Green's R-class: [5,4,7]>, <Green's R-class: [3,7,4]>, 
      <Green's R-class: [3,4][5,7]>, <Green's R-class: [5,4](7)> ], 
  [ <Green's R-class: [2,3,1,4][5,7]> ], [ <Green's R-class: [2,7,4][6,1]> ], 
  [ <Green's R-class: [1,7,4][2,3]> ], 
  [ <Green's R-class: [5,4](1)(2)>, <Green's R-class: [5,1][7,4](2)>, 
      <Green's R-class: [3,4](1)(2)>, <Green's R-class: [5,1,4](2)>, 
      <Green's R-class: [5,4][7,1](2)>, <Green's R-class: [3,1,4](2)> ], 
  [ <Green's R-class: [6,4][7,1,5]> ], 
  [ <Green's R-class: [2,1,3](5)>, <Green's R-class: [2,1][7,5,3]>, 
      <Green's R-class: [2,1,3,5]>, <Green's R-class: [2,1,5,3]>, 
      <Green's R-class: [2,1][7,3](5)>, <Green's R-class: [2,1,5](3)> ], 
  [ <Green's R-class: [2,4][5,1,6]>, <Green's R-class: [2,4][5,6][7,1]>, 
      <Green's R-class: [2,4][3,1,6]>, <Green's R-class: [2,4][5,6](1)>, 
      <Green's R-class: [2,4][5,1][7,6]>, <Green's R-class: [2,4][3,6](1)> ], 
  [ <Green's R-class: [2,4][7,6](1)> ], [ <Green's R-class: [2,6][4,1][5,3]> ]
    , 
  [ <Green's R-class: [1,3,6]>, <Green's R-class: [1,6][5,3]>, 
      <Green's R-class: [1,6][7,3]>, <Green's R-class: [1,6,3]>, 
      <Green's R-class: [4,3,6]>, <Green's R-class: [4,3][5,6]>, 
      <Green's R-class: [7,6](3)>, <Green's R-class: [5,3,6]>, 
      <Green's R-class: [5,6][7,3]>, <Green's R-class: [1,6](3)>, 
      <Green's R-class: [1,3][5,6]>, <Green's R-class: [1,3][7,6]>, 
      <Green's R-class: [1,3](6)>, <Green's R-class: [4,6](3)>, 
      <Green's R-class: [4,6][5,3]>, <Green's R-class: [7,3,6]>, 
      <Green's R-class: [5,6](3)>, <Green's R-class: [5,3][7,6]> ], 
  [ <Green's R-class: [1,6][7,3,5]> ], [ <Green's R-class: [2,4][7,3,5,6]> ], 
  [ <Green's R-class: [5,1,6](2)>, <Green's R-class: [5,6][7,1](2)>, 
      <Green's R-class: [3,1,6](2)>, <Green's R-class: [5,6](1)(2)>, 
      <Green's R-class: [5,1][7,6](2)>, <Green's R-class: [3,6](1)(2)> ], 
  [ <Green's R-class: [2,1,3][7,6]> ], [ <Green's R-class: [5,3][7,6](1)(2)> ]
    , [ <Green's R-class: [2,3,4,1]> ], [ <Green's R-class: [2,6][5,4,1]> ], 
  [ <Green's R-class: [1,4][2,3][5,6]>, <Green's R-class: [2,3][5,4][7,6]>, 
      <Green's R-class: [1,4][2,3,6]>, <Green's R-class: [1,6][2,3][5,4]>, 
      <Green's R-class: [2,3][5,6][7,4]>, <Green's R-class: [1,6][2,3,4]> ], 
  [ <Green's R-class: [1,4][5,3](2)>, <Green's R-class: [5,4][7,3](2)>, 
      <Green's R-class: [1,4](2)(3)>, <Green's R-class: [1,3][5,4](2)>, 
      <Green's R-class: [5,3][7,4](2)>, <Green's R-class: [1,3,4](2)> ], 
  [ <Green's R-class: [1,6,3][7,4]> ], 
  [ <Green's R-class: [1,6][5,4](2)>, <Green's R-class: [5,6][7,4](2)>, 
      <Green's R-class: [1,6][3,4](2)>, <Green's R-class: [1,4][5,6](2)>, 
      <Green's R-class: [5,4][7,6](2)>, <Green's R-class: [1,4][3,6](2)> ], 
  [ <Green's R-class: [1,6][2,5]>, <Green's R-class: [2,5,6]>, 
      <Green's R-class: [2,5][7,6]>, <Green's R-class: [2,5][3,6]> ], 
  [ <Green's R-class: [7,6,5]> ], [ <Green's R-class: [7,5](6)> ], 
  [ <Green's R-class: [2,1][3,6][4,5]> ], [ <Green's R-class: [2,5][4,3]> ], 
  [ <Green's R-class: [1,7][2,5,3]>, <Green's R-class: [2,5,7,3]>, 
      <Green's R-class: [1,7][2,5](3)>, <Green's R-class: [1,3][2,5,7]>, 
      <Green's R-class: [2,5,3](7)>, <Green's R-class: [1,3,7][2,5]> ], 
  [ <Green's R-class: [2,3][6,5]> ], [ <Green's R-class: [2,4][3,1](5)> ], 
  [ <Green's R-class: [2,7][5,4,1]> ], 
  [ <Green's R-class: [2,3][5,1,7]>, <Green's R-class: [2,3][5,7,1]>, 
      <Green's R-class: [2,3,1,7]>, <Green's R-class: [2,3][5,7](1)>, 
      <Green's R-class: [2,3][5,1](7)>, <Green's R-class: [2,3,7](1)> ], 
  [ <Green's R-class: [2,1,4](7)> ], [ <Green's R-class: [2,3][5,4](1)(7)> ], 
  [ <Green's R-class: [2,4,1]> ], 
  [ <Green's R-class: [1,7][2,4]>, <Green's R-class: [2,4][5,7]>, 
      <Green's R-class: [2,4](7)>, <Green's R-class: [2,4][3,7]> ], 
  [ <Green's R-class: [2,4](1,5)>, <Green's R-class: [2,4][7,1](5)>, 
      <Green's R-class: [2,4][3,1,5]>, <Green's R-class: [2,4](1)(5)>, 
      <Green's R-class: [2,4][7,5,1]>, <Green's R-class: [2,4][3,5](1)> ], 
  [ <Green's R-class: [2,1][3,5](4)> ], 
  [ <Green's R-class: [1,3][2,6]>, <Green's R-class: [2,6][5,3]>, 
      <Green's R-class: [2,6][7,3]>, <Green's R-class: [2,6](3)> ], 
  [ <Green's R-class: [2,6][7,3,1]> ], 
  [ <Green's R-class: [1,6][2,3]>, <Green's R-class: [2,3][5,6]>, 
      <Green's R-class: [2,3][7,6]>, <Green's R-class: [2,3,6]> ], 
  [ <Green's R-class: [1,6,3][2,5]> ], [ <Green's R-class: [1,6][2,4](3)(5)> ]
    , [ <Green's R-class: [2,5][7,6,3]> ], 
  [ <Green's R-class: [1,5][2,4][7,6]> ], 
  [ <Green's R-class: [1,3][5,6](2)>, <Green's R-class: [5,3][7,6](2)>, 
      <Green's R-class: [1,3,6](2)>, <Green's R-class: [1,6][5,3](2)>, 
      <Green's R-class: [5,6][7,3](2)>, <Green's R-class: [1,6](2)(3)> ], 
  [ <Green's R-class: [7,3](1)(6)> ], 
  [ <Green's R-class: [1,4][2,6]>, <Green's R-class: [2,6][5,4]>, 
      <Green's R-class: [2,6][7,4]>, <Green's R-class: [2,6][3,4]> ], 
  [ <Green's R-class: [2,6][3,1][7,4]> ], [ <Green's R-class: [2,4,3,6]> ], 
  [ <Green's R-class: [1,6][2,4]>, <Green's R-class: [2,4][5,6]>, 
      <Green's R-class: [2,4][7,6]>, <Green's R-class: [2,4][3,6]> ], 
  [ <Green's R-class: [7,6,4]> ], 
  [ <Green's R-class: [1,6](2)>, <Green's R-class: [5,6](2)>, 
      <Green's R-class: [7,6](2)>, <Green's R-class: [3,6](2)> ], 
  [ <Green's R-class: [2,6][4,5]> ], [ <Green's R-class: [2,5][4,6]> ], 
  [ <Green's R-class: [2,4][7,5](1)> ], [ <Green's R-class: [6,1][7,4]> ], 
  [ <Green's R-class: [1,4][2,7]>, <Green's R-class: [2,7][5,4]>, 
      <Green's R-class: [2,7,4]>, <Green's R-class: [2,7][3,4]> ], 
  [ <Green's R-class: [2,7,4][3,1]> ], [ <Green's R-class: [6,4](7)> ], 
  [ <Green's R-class: [1,4][2,3][5,7]>, <Green's R-class: [2,3][5,4](7)>, 
      <Green's R-class: [1,4][2,3,7]>, <Green's R-class: [1,7][2,3][5,4]>, 
      <Green's R-class: [2,3][5,7,4]>, <Green's R-class: [1,7][2,3,4]> ], 
  [ <Green's R-class: [6,7,4](1)> ], 
  [ <Green's R-class: [2,3][5,4](1)>, <Green's R-class: [2,3][5,1][7,4]>, 
      <Green's R-class: [2,3,4](1)>, <Green's R-class: [2,3][5,1,4]>, 
      <Green's R-class: [2,3][5,4][7,1]>, <Green's R-class: [2,3,1,4]> ], 
  [ <Green's R-class: [6,4][7,1]> ], 
  [ <Green's R-class: [1,4](2)>, <Green's R-class: [5,4](2)>, 
      <Green's R-class: [7,4](2)>, <Green's R-class: [3,4](2)> ], 
  [ <Green's R-class: [2,1,5,4]>, <Green's R-class: [2,1][7,4](5)>, 
      <Green's R-class: [2,1,5][3,4]>, <Green's R-class: [2,1,4](5)>, 
      <Green's R-class: [2,1][7,5,4]>, <Green's R-class: [2,1,4][3,5]> ], 
  [ <Green's R-class: [2,6][5,1](3)> ], [ <Green's R-class: [2,5,6][4,3]> ], 
  [ <Green's R-class: [1,5,3][2,4]>, <Green's R-class: [2,4][7,3](5)>, 
      <Green's R-class: [1,5][2,4](3)>, <Green's R-class: [1,3][2,4](5)>, 
      <Green's R-class: [2,4][7,5,3]>, <Green's R-class: [1,3,5][2,4]> ], 
  [ <Green's R-class: [1,6][2,3][7,5]> ], 
  [ <Green's R-class: [1,3][2,4][7,5,6]> ], [ <Green's R-class: [2,6][4,3]> ],
  [ <Green's R-class: [2,6][5,3](1)>, <Green's R-class: [2,6][5,1][7,3]>, 
      <Green's R-class: [2,6](1)(3)>, <Green's R-class: [2,6][5,1,3]>, 
      <Green's R-class: [2,6][5,3][7,1]>, <Green's R-class: [2,6](1,3)> ], 
  [ <Green's R-class: [2,3,1][4,6]> ], [ <Green's R-class: [2,1][6,4]> ], 
  [ <Green's R-class: [2,6][3,4][5,1]> ], 
  [ <Green's R-class: [1,4][3,6]>, <Green's R-class: [1,6][5,4]>, 
      <Green's R-class: [1,6][7,4]>, <Green's R-class: [1,6,4]>, 
      <Green's R-class: [3,6](4)>, <Green's R-class: [5,6](4)>, 
      <Green's R-class: [3,4][7,6]>, <Green's R-class: [3,6][5,4]>, 
      <Green's R-class: [5,6][7,4]>, <Green's R-class: [1,6][3,4]>, 
      <Green's R-class: [1,4][5,6]>, <Green's R-class: [1,4][7,6]>, 
      <Green's R-class: [1,4](6)>, <Green's R-class: [3,4,6]>, 
      <Green's R-class: [5,4,6]>, <Green's R-class: [3,6][7,4]>, 
      <Green's R-class: [3,4][5,6]>, <Green's R-class: [5,4][7,6]> ], 
  [ <Green's R-class: [2,6](4)> ], [ <Green's R-class: [2,5](6)> ], 
  [ <Green's R-class: [2,7][3,4][5,1]> ], [ <Green's R-class: [2,7](4)> ], 
  [ <Green's R-class: [2,7][5,4](1)>, <Green's R-class: [2,7,4][5,1]>, 
      <Green's R-class: [2,7][3,4](1)>, <Green's R-class: [2,7][5,1,4]>, 
      <Green's R-class: [2,7,1][5,4]>, <Green's R-class: [2,7][3,1,4]> ], 
  [ <Green's R-class: [2,4,7][3,1]> ], [ <Green's R-class: [2,1](4)> ], 
  [ <Green's R-class: [2,4][6,1]> ], [ <Green's R-class: [2,6][7,1,3]> ], 
  [ <Green's R-class: [7,6,3]> ], [ <Green's R-class: [2,5][7,6](3)> ], 
  [ <Green's R-class: [1,6][2,4](5)>, <Green's R-class: [2,4][7,5,6]>, 
      <Green's R-class: [1,6][2,4][3,5]>, <Green's R-class: [1,5,6][2,4]>, 
      <Green's R-class: [2,4][7,6](5)>, <Green's R-class: [1,5][2,4][3,6]> ], 
  [ <Green's R-class: [1,3][7,6,5]> ], 
  [ <Green's R-class: [1,3][2,4][5,6]>, <Green's R-class: [2,4][5,3][7,6]>, 
      <Green's R-class: [1,3,6][2,4]>, <Green's R-class: [1,6][2,4][5,3]>, 
      <Green's R-class: [2,4][5,6][7,3]>, <Green's R-class: [1,6][2,4](3)> ], 
  [ <Green's R-class: [7,3](6)> ], 
  [ <Green's R-class: [2,3][5,6](1)>, <Green's R-class: [2,3][5,1][7,6]>, 
      <Green's R-class: [2,3,6](1)>, <Green's R-class: [2,3][5,1,6]>, 
      <Green's R-class: [2,3][5,6][7,1]>, <Green's R-class: [2,3,1,6]> ], 
  [ <Green's R-class: [2,6][5,4](1)>, <Green's R-class: [2,6][5,1][7,4]>, 
      <Green's R-class: [2,6][3,4](1)>, <Green's R-class: [2,6][5,1,4]>, 
      <Green's R-class: [2,6][5,4][7,1]>, <Green's R-class: [2,6][3,1,4]> ], 
  [ <Green's R-class: [2,6][7,1,4]> ], [ <Green's R-class: [2,3](6)> ], 
  [ <Green's R-class: [2,6,5]> ], [ <Green's R-class: [2,7,1,4]> ], 
  [ <Green's R-class: [6,7,4]> ], 
  [ <Green's R-class: [2,4][5,7](1)>, <Green's R-class: [2,4][5,1](7)>, 
      <Green's R-class: [2,4][3,7](1)>, <Green's R-class: [2,4][5,1,7]>, 
      <Green's R-class: [2,4][5,7,1]>, <Green's R-class: [2,4][3,1,7]> ], 
  [ <Green's R-class: [2,4][6,7]> ], [ <Green's R-class: [2,5,3,6]> ], 
  [ <Green's R-class: [1,3][2,5,6]>, <Green's R-class: [2,5,3][7,6]>, 
      <Green's R-class: [1,3,6][2,5]>, <Green's R-class: [1,6][2,5,3]>, 
      <Green's R-class: [2,5,6][7,3]>, <Green's R-class: [1,6][2,5](3)> ], 
  [ <Green's R-class: [2,6][4,5](3)> ], [ <Green's R-class: [2,3][4,6]> ], 
  [ <Green's R-class: [2,6,3]> ], [ <Green's R-class: [2,4](6)> ], 
  [ <Green's R-class: [7,4](6)> ], [ <Green's R-class: [2,4,7]> ], 
  [ <Green's R-class: [2,7][6,4]> ], [ <Green's R-class: [1,6][2,5][7,3]> ], 
  [ <Green's R-class: [1,3][2,6](5)>, <Green's R-class: [2,6][7,5,3]>, 
      <Green's R-class: [1,3,5][2,6]>, <Green's R-class: [1,5,3][2,6]>, 
      <Green's R-class: [2,6][7,3](5)>, <Green's R-class: [1,5][2,6](3)> ], 
  [ <Green's R-class: [2,4,6]> ], [ <Green's R-class: [2,6,4]> ] ]
gap> ForAll(Union(List(Union(last), Elements)), x -> x in s);
true
gap> Union(List(last2, Elements));
[ <Green's R-class: [2,7][3,1,5][4,6]>, <Green's R-class: [1,5,3,7](2)>, 
  <Green's R-class: [2,3,4][6,7,5](1)>, <Green's R-class: [7,5,1,3,4,6](2)>, 
  <Green's R-class: [3,5]>, <Green's R-class: [2,5,3]>, 
  <Green's R-class: [2,1,5][3,6]>, <Green's R-class: [2,7][3,6](1)(5)>, 
  <Green's R-class: [1,3,5]>, <Green's R-class: [1,3][5,7](2)>, 
  <Green's R-class: [1,5][2,7,3]>, <Green's R-class: [1,7,3](2)(5)>, 
  <Green's R-class: [2,5][3,1][4,7]>, <Green's R-class: [2,3,5,4]>, 
  <Green's R-class: [2,4][6,5](1)>, <Green's R-class: [2,3][5,1,4,7]>, 
  <Green's R-class: [2,5](1)(3)>, <Green's R-class: [3,5,4](1)(2)>, 
  <Green's R-class: [2,4][7,1,3,6,5]>, <Green's R-class: [5,3,6][7,1,4](2)>, 
  <Green's R-class: <empty partial perm>>, 
  <Green's R-class: <identity partial perm on [ 5 ]>>, 
  <Green's R-class: [2,5]>, <Green's R-class: [1,5]>, 
  <Green's R-class: [1,3][2,5]>, <Green's R-class: [7,5]>, 
  <Green's R-class: [2,5][7,3]>, <Green's R-class: [1,5][2,6]>, 
  <Green's R-class: [2,1,6](5)>, <Green's R-class: [1,5,6][2,7]>, 
  <Green's R-class: [2,6][7,5](1)>, <Green's R-class: [2,7,5,1,6]>, 
  <Green's R-class: [1,5,3]>, <Green's R-class: [1,7](2)>, 
  <Green's R-class: [1,5][7,3]>, <Green's R-class: [5,3](2)(7)>, 
  <Green's R-class: [2,5](3)>, <Green's R-class: [1,5][6,3]>, 
  <Green's R-class: [1,5][2,3,7]>, <Green's R-class: [1,5](2)(3)>, 
  <Green's R-class: [1,7,5][6,3]>, <Green's R-class: (2)(5,7)>, 
  <Green's R-class: [2,5][3,7](1)>, <Green's R-class: [1,4][2,3](5)>, 
  <Green's R-class: [1,5][2,3][7,4]>, <Green's R-class: [4,3,5]>, 
  <Green's R-class: [2,4,5,1]>, <Green's R-class: [3,4](1)>, 
  <Green's R-class: [3,7,1,4]>, <Green's R-class: [2,3,7,1][5,4]>, 
  <Green's R-class: [2,5,1,3]>, <Green's R-class: [3,1,4]>, 
  <Green's R-class: [1,4](2)(5)>, <Green's R-class: [2,5][7,4](1)>, 
  <Green's R-class: [7,4](1,5)(2)>, <Green's R-class: [2,1][4,5](3)>, 
  <Green's R-class: [2,4][3,1][5,6]>, <Green's R-class: [2,6,1,3]>, 
  <Green's R-class: [1,6][2,4,5,3]>, <Green's R-class: [2,1,3,4]>, 
  <Green's R-class: [5,6](1,3)(2)>, <Green's R-class: [2,6,1,4][7,3]>, 
  <Green's R-class: [1,6][5,4][7,3](2)>, <Green's R-class: [6,5]>, 
  <Green's R-class: [2,6](5)>, <Green's R-class: [1,5][3,6]>, 
  <Green's R-class: [1,6][7,5]>, <Green's R-class: [2,1][7,5,6]>, 
  <Green's R-class: [1,6][3,5]>, <Green's R-class: [1,6][2,7]>, 
  <Green's R-class: [1,5][7,6]>, <Green's R-class: [2,7,6](5)>, 
  <Green's R-class: [2,6][3,5]>, <Green's R-class: [2,5][3,6](1)>, 
  <Green's R-class: [2,7][3,5](1)>, <Green's R-class: [7,1,6,5]>, 
  <Green's R-class: [2,7,1][5,6]>, <Green's R-class: [7,3](5)>, 
  <Green's R-class: <identity partial perm on [ 2 ]>>, 
  <Green's R-class: [5,7](2)>, <Green's R-class: [1,3,7](2)>, 
  <Green's R-class: [6,3][7,5]>, 
  <Green's R-class: <identity partial perm on [ 2, 7 ]>>, 
  <Green's R-class: [4,3](5)>, <Green's R-class: [1,7][2,3](5)>, 
  <Green's R-class: [1,3](2)(5)>, <Green's R-class: [2,5][4,3,7]>, 
  <Green's R-class: [1,7][3,5](2)>, <Green's R-class: [2,5,1,7]>, 
  <Green's R-class: [2,3][7,5,4]>, <Green's R-class: [2,4](1)>, 
  <Green's R-class: [7,5](3)>, <Green's R-class: [2,4][3,5][7,1]>, 
  <Green's R-class: [5,1,4]>, <Green's R-class: [2,1][3,4]>, 
  <Green's R-class: (3,5)>, <Green's R-class: [2,7][6,1,4]>, 
  <Green's R-class: [1,7][5,4]>, <Green's R-class: [2,1,4]>, 
  <Green's R-class: [2,3,1,4][5,7]>, <Green's R-class: [2,7,4][6,1]>, 
  <Green's R-class: [1,7,4][2,3]>, <Green's R-class: [2,5,3][7,1]>, 
  <Green's R-class: [5,4](1)>, <Green's R-class: [7,5,4](2)>, 
  <Green's R-class: [2,4][3,1]>, <Green's R-class: [6,4](1)>, 
  <Green's R-class: [2,4][3,5](1)>, <Green's R-class: [3,4](1)(2)>, 
  <Green's R-class: [6,4][7,1,5]>, <Green's R-class: [2,1,3,5]>, 
  <Green's R-class: [2,4][5,1,6]>, <Green's R-class: [2,4][7,6](1)>, 
  <Green's R-class: [2,6][4,1][5,3]>, <Green's R-class: [1,3,6]>, 
  <Green's R-class: [1,6][7,3,5]>, <Green's R-class: [2,4][7,3,5,6]>, 
  <Green's R-class: [2,1][5,4]>, <Green's R-class: [2,1,4][5,3]>, 
  <Green's R-class: [1,6](3)>, <Green's R-class: [5,1,6](2)>, 
  <Green's R-class: [2,1,3][7,6]>, <Green's R-class: [5,3][7,6](1)(2)>, 
  <Green's R-class: [2,3,4,1]>, <Green's R-class: [2,6](3)>, 
  <Green's R-class: [2,6][5,4,1]>, <Green's R-class: [1,4][2,3,6]>, 
  <Green's R-class: [1,4](2)(3)>, <Green's R-class: [1,6,3][7,4]>, 
  <Green's R-class: [5,6][7,4](2)>, <Green's R-class: [4,5]>, 
  <Green's R-class: [2,6][7,5]>, <Green's R-class: [1,6](5)>, 
  <Green's R-class: [2,5][3,6]>, <Green's R-class: [1,6,5]>, 
  <Green's R-class: [1,6][2,5]>, <Green's R-class: [2,1,6][3,5]>, 
  <Green's R-class: [7,6,5]>, <Green's R-class: [1,5,6]>, 
  <Green's R-class: [2,7][5,6]>, <Green's R-class: [1,5](6)>, 
  <Green's R-class: [1,5][2,7][3,6]>, <Green's R-class: [7,5](6)>, 
  <Green's R-class: [2,5,6]>, <Green's R-class: [2,5,1,6]>, 
  <Green's R-class: [2,7](1,5)>, <Green's R-class: [2,1][3,6][4,5]>, 
  <Green's R-class: [4,5,6]>, <Green's R-class: [2,7][3,1,6]>, 
  <Green's R-class: [2,7,6]>, <Green's R-class: [1,7][5,3](2)>, 
  <Green's R-class: [2,5][4,3]>, <Green's R-class: [3,7](2)>, 
  <Green's R-class: [2,3](5,7)>, <Green's R-class: [7,5,3](2)>, 
  <Green's R-class: [1,7][2,5](3)>, <Green's R-class: [1,7](2)(5)>, 
  <Green's R-class: [2,5,7,1]>, <Green's R-class: [1,4][2,3,5]>, 
  <Green's R-class: [2,4][5,1]>, <Green's R-class: [2,3][6,5]>, 
  <Green's R-class: [2,4][3,1](5)>, <Green's R-class: [7,1,4]>, 
  <Green's R-class: [5,4][7,1]>, <Green's R-class: [3,4,1]>, 
  <Green's R-class: [2,7][5,4,1]>, <Green's R-class: [1,4][3,7]>, 
  <Green's R-class: [1,7,4]>, <Green's R-class: [5,7,4]>, 
  <Green's R-class: [1,7][3,4]>, <Green's R-class: [2,3][5,1,7]>, 
  <Green's R-class: [2,1,4](7)>, <Green's R-class: [2,3][5,4](1)(7)>, 
  <Green's R-class: [2,4,1]>, <Green's R-class: [2,7][3,4]>, 
  <Green's R-class: [2,4][3,7]>, <Green's R-class: [1,7][6,4]>, 
  <Green's R-class: [2,5](1,3)>, <Green's R-class: [7,4](1)>, 
  <Green's R-class: [5,1][7,4]>, <Green's R-class: [1,4][3,5](2)>, 
  <Green's R-class: [7,4](2)>, <Green's R-class: [3,1](4)>, 
  <Green's R-class: [5,1](4)>, <Green's R-class: [2,4](1,5)>, 
  <Green's R-class: [5,4](2)>, <Green's R-class: [5,1,4](2)>, 
  <Green's R-class: [2,1][3,5](4)>, <Green's R-class: [2,1,5,3]>, 
  <Green's R-class: [2,4][5,6][7,1]>, <Green's R-class: [2,4][3,6]>, 
  <Green's R-class: [1,3][2,6]>, <Green's R-class: [2,6][7,3,1]>, 
  <Green's R-class: [1,6][5,3]>, <Green's R-class: [2,3,6]>, 
  <Green's R-class: [1,6,3][2,5]>, <Green's R-class: [1,6][2,3]>, 
  <Green's R-class: [1,6][2,4](3)(5)>, <Green's R-class: [2,5][7,6,3]>, 
  <Green's R-class: [1,5][2,4][7,6]>, <Green's R-class: [2,1][7,4]>, 
  <Green's R-class: [2,1][5,4][7,3]>, <Green's R-class: [1,3][5,6]>, 
  <Green's R-class: [5,6][7,1](2)>, <Green's R-class: [1,3](6)>, 
  <Green's R-class: [2,6](1,3)>, <Green's R-class: [1,3,6](2)>, 
  <Green's R-class: [7,3](1)(6)>, <Green's R-class: [2,3,1,4]>, 
  <Green's R-class: [2,6][5,3]>, <Green's R-class: [1,4][2,6]>, 
  <Green's R-class: [3,1][7,4]>, <Green's R-class: [2,6][3,1][7,4]>, 
  <Green's R-class: [2,3][5,6]>, <Green's R-class: [1,6][2,3][5,4]>, 
  <Green's R-class: [1,3][5,4](2)>, <Green's R-class: [2,4,3,6]>, 
  <Green's R-class: [1,6,4]>, <Green's R-class: [4,3][5,6]>, 
  <Green's R-class: [1,6][2,4]>, <Green's R-class: [1,6][3,4](2)>, 
  <Green's R-class: [7,6,4]>, <Green's R-class: [7,6](2)>, 
  <Green's R-class: [7,5,6]>, <Green's R-class: [3,6][4,5]>, 
  <Green's R-class: [2,1,5,6]>, <Green's R-class: [2,6][4,5]>, 
  <Green's R-class: [7,6](5)>, <Green's R-class: [3,5][4,6]>, 
  <Green's R-class: [4,6](5)>, <Green's R-class: [1,6][2,7](5)>, 
  <Green's R-class: [2,5][4,6]>, <Green's R-class: [2,5][7,6]>, 
  <Green's R-class: [2,5,6][7,1]>, <Green's R-class: [2,7,1](5)>, 
  <Green's R-class: [3,5][7,6]>, <Green's R-class: [2,7][5,6](1)>, 
  <Green's R-class: [2,7][3,6]>, <Green's R-class: [5,7,3](2)>, 
  <Green's R-class: [1,7][2,3,5]>, <Green's R-class: [1,3,5](2)>, 
  <Green's R-class: [1,3][2,5,7]>, <Green's R-class: [2,5][3,1,7]>, 
  <Green's R-class: [1,5,4][2,3]>, <Green's R-class: [2,4][7,1]>, 
  <Green's R-class: [2,4][7,5](1)>, <Green's R-class: [6,1,4]>, 
  <Green's R-class: [6,1][7,4]>, <Green's R-class: [1,4][2,7]>, 
  <Green's R-class: [2,7,4][3,1]>, <Green's R-class: [1,7][2,4]>, 
  <Green's R-class: [6,4](7)>, <Green's R-class: [1,4][5,7]>, 
  <Green's R-class: [2,3][5,7,1]>, <Green's R-class: [1,4][6,7]>, 
  <Green's R-class: [2,7][3,1,4]>, <Green's R-class: [1,4][2,3,7]>, 
  <Green's R-class: [6,7,4](1)>, <Green's R-class: [2,3][5,1][7,4]>, 
  <Green's R-class: [2,7][5,4]>, <Green's R-class: [2,4][5,7]>, 
  <Green's R-class: [3,7](4)>, <Green's R-class: [5,7](4)>, 
  <Green's R-class: [2,5,3](1)>, <Green's R-class: [6,4][7,1]>, 
  <Green's R-class: [1,5,4](2)>, <Green's R-class: [3,4](2)>, 
  <Green's R-class: [3,4][7,1]>, <Green's R-class: [2,4][7,1](5)>, 
  <Green's R-class: [1,4](2)>, <Green's R-class: [5,4][7,1](2)>, 
  <Green's R-class: [2,1,5][3,4]>, <Green's R-class: [2,1][7,3](5)>, 
  <Green's R-class: [2,4][3,1,6]>, <Green's R-class: [2,4][7,6]>, 
  <Green's R-class: [2,4][5,6]>, <Green's R-class: [2,6][5,1](3)>, 
  <Green's R-class: [1,6][7,3]>, <Green's R-class: [5,6][7,3]>, 
  <Green's R-class: [4,3,6]>, <Green's R-class: [2,5,6][4,3]>, 
  <Green's R-class: [1,5,3][2,4]>, <Green's R-class: [1,6][2,3][7,5]>, 
  <Green's R-class: [1,3][2,4][7,5,6]>, <Green's R-class: [2,6][4,3]>, 
  <Green's R-class: [2,1,4](3)>, <Green's R-class: [1,3][7,6]>, 
  <Green's R-class: [5,3][7,6]>, <Green's R-class: [3,1,6](2)>, 
  <Green's R-class: [4,6](3)>, <Green's R-class: [4,6][5,3]>, 
  <Green's R-class: [2,6][5,3](1)>, <Green's R-class: [5,6](2)>, 
  <Green's R-class: [1,6][5,3](2)>, <Green's R-class: [2,3,1][4,6]>, 
  <Green's R-class: [2,3][5,4](1)>, <Green's R-class: [2,6][7,3]>, 
  <Green's R-class: [2,6][5,4]>, <Green's R-class: [3,4][5,1]>, 
  <Green's R-class: [2,1][6,4]>, <Green's R-class: [2,6][3,4][5,1]>, 
  <Green's R-class: [2,3][7,6]>, <Green's R-class: [1,4][3,6]>, 
  <Green's R-class: [1,6][7,4]>, <Green's R-class: [2,3][5,6][7,4]>, 
  <Green's R-class: [5,3][7,4](2)>, <Green's R-class: [1,6][2,4](3)>, 
  <Green's R-class: [3,6](4)>, <Green's R-class: [5,6](4)>, 
  <Green's R-class: [7,6](3)>, <Green's R-class: [1,4][5,6](2)>, 
  <Green's R-class: [2,6](4)>, <Green's R-class: [3,6](2)>, 
  <Green's R-class: [2,1][7,6](5)>, <Green's R-class: [3,6][7,5]>, 
  <Green's R-class: [2,7,5,6]>, <Green's R-class: [2,5][3,1,6]>, 
  <Green's R-class: [2,7][3,1,5]>, <Green's R-class: [3,6](5)>, 
  <Green's R-class: [2,5](6)>, <Green's R-class: [2,7,6][5,1]>, 
  <Green's R-class: [1,7](2)(3)>, <Green's R-class: [1,5,7][2,3]>, 
  <Green's R-class: [1,5,3](2)>, <Green's R-class: [2,5,3](7)>, 
  <Green's R-class: [2,5,7](1)>, <Green's R-class: [2,3][7,4](5)>, 
  <Green's R-class: [5,4,1]>, <Green's R-class: [2,7][3,4][5,1]>, 
  <Green's R-class: [2,7](4)>, <Green's R-class: [1,4](7)>, 
  <Green's R-class: [5,4](7)>, <Green's R-class: [2,3,1,7]>, 
  <Green's R-class: [3,4,7]>, <Green's R-class: [5,4,7]>, 
  <Green's R-class: [2,7][5,4](1)>, <Green's R-class: [1,7][2,3][5,4]>, 
  <Green's R-class: [2,4,7][3,1]>, <Green's R-class: [2,3,4](1)>, 
  <Green's R-class: [2,7,4]>, <Green's R-class: [2,4](7)>, 
  <Green's R-class: [3,4](7)>, <Green's R-class: [2,5,1][7,3]>, 
  <Green's R-class: [2,1](4)>, <Green's R-class: [7,4](2)(5)>, 
  <Green's R-class: [3,1][5,4]>, <Green's R-class: [2,4][6,1]>, 
  <Green's R-class: [2,4][3,1,5]>, <Green's R-class: [3,1,4](2)>, 
  <Green's R-class: [2,1,4](5)>, <Green's R-class: [2,1,5](3)>, 
  <Green's R-class: [2,4][5,6](1)>, <Green's R-class: [2,6][7,1,3]>, 
  <Green's R-class: [1,6,3]>, <Green's R-class: [7,6,3]>, 
  <Green's R-class: [2,5][7,6](3)>, <Green's R-class: [2,4][7,3](5)>, 
  <Green's R-class: [1,6][2,5](3)>, <Green's R-class: [1,6][2,4][3,5]>, 
  <Green's R-class: [1,3][7,6,5]>, <Green's R-class: [2,4][5,3][7,6]>, 
  <Green's R-class: [2,1,3][5,4]>, <Green's R-class: [7,3](6)>, 
  <Green's R-class: [5,6](1)(2)>, <Green's R-class: [7,3,6]>, 
  <Green's R-class: [2,6][5,1][7,3]>, <Green's R-class: [1,6](2)>, 
  <Green's R-class: [5,6][7,3](2)>, <Green's R-class: [2,3,6](1)>, 
  <Green's R-class: [2,6][7,4]>, <Green's R-class: [2,6][5,4](1)>, 
  <Green's R-class: [2,6][7,1,4]>, <Green's R-class: [1,6][5,4]>, 
  <Green's R-class: [1,6][2,3,4]>, <Green's R-class: [1,3,4](2)>, 
  <Green's R-class: [1,3][2,4][5,6]>, <Green's R-class: [2,6][3,4]>, 
  <Green's R-class: [1,6][3,4]>, <Green's R-class: [3,4][7,6]>, 
  <Green's R-class: [5,3,6]>, <Green's R-class: [2,3](6)>, 
  <Green's R-class: [1,4][7,6]>, <Green's R-class: [5,4][7,6](2)>, 
  <Green's R-class: [3,5,6]>, <Green's R-class: [2,6,5]>, 
  <Green's R-class: [1,6][2,7][3,5]>, <Green's R-class: [2,5,6](1)>, 
  <Green's R-class: [2,7](1)(5)>, <Green's R-class: [2,7][3,6](1)>, 
  <Green's R-class: [2,3](5)(7)>, <Green's R-class: [7,3](2)(5)>, 
  <Green's R-class: [1,3,7][2,5]>, <Green's R-class: [2,5,1](7)>, 
  <Green's R-class: [1,5][2,3,4]>, <Green's R-class: [2,7,1,4]>, 
  <Green's R-class: [6,7,4]>, <Green's R-class: [2,3][5,7](1)>, 
  <Green's R-class: [3,7,4]>, <Green's R-class: [2,7,4][5,1]>, 
  <Green's R-class: [2,3][5,7,4]>, <Green's R-class: [2,4][3,7](1)>, 
  <Green's R-class: [2,3][5,1,4]>, <Green's R-class: [3,7][5,4]>, 
  <Green's R-class: [2,4][6,7]>, <Green's R-class: [1,5][3,4](2)>, 
  <Green's R-class: [2,4](1)(5)>, <Green's R-class: [5,4](1)(2)>, 
  <Green's R-class: [2,1][7,5,4]>, <Green's R-class: [2,1,3](5)>, 
  <Green's R-class: [2,4][5,1][7,6]>, <Green's R-class: [2,5,3,6]>, 
  <Green's R-class: [1,5][2,4](3)>, <Green's R-class: [1,3][2,5,6]>, 
  <Green's R-class: [1,5,6][2,4]>, <Green's R-class: [2,6][4,5](3)>, 
  <Green's R-class: [1,3,6][2,4]>, <Green's R-class: [2,1][5,3][7,4]>, 
  <Green's R-class: [2,3][4,6]>, <Green's R-class: [5,1][7,6](2)>, 
  <Green's R-class: [5,6](3)>, <Green's R-class: [2,6,3]>, 
  <Green's R-class: [2,6](1)(3)>, <Green's R-class: [1,6](2)(3)>, 
  <Green's R-class: [2,3][5,1,6]>, <Green's R-class: [2,6][5,1][7,4]>, 
  <Green's R-class: [5,6][7,4]>, <Green's R-class: [1,4][2,3][5,6]>, 
  <Green's R-class: [1,4][5,3](2)>, <Green's R-class: [1,4][5,6]>, 
  <Green's R-class: [3,6][5,4]>, <Green's R-class: [2,4](6)>, 
  <Green's R-class: [1,4](6)>, <Green's R-class: [1,4][3,6](2)>, 
  <Green's R-class: [7,4](6)>, <Green's R-class: [2,5,1][7,6]>, 
  <Green's R-class: [2,7,5,1]>, <Green's R-class: [2,7][5,1,6]>, 
  <Green's R-class: [1,7][2,5,3]>, <Green's R-class: [2,4,7]>, 
  <Green's R-class: [2,3][5,1](7)>, <Green's R-class: [3,4][5,7]>, 
  <Green's R-class: [2,7][6,4]>, <Green's R-class: [2,7][3,4](1)>, 
  <Green's R-class: [1,7][2,3,4]>, <Green's R-class: [2,4][5,1,7]>, 
  <Green's R-class: [2,3][5,4][7,1]>, <Green's R-class: [2,4][7,5,1]>, 
  <Green's R-class: [5,1][7,4](2)>, <Green's R-class: [2,1,4][3,5]>, 
  <Green's R-class: [2,1][7,5,3]>, <Green's R-class: [2,4][3,6](1)>, 
  <Green's R-class: [1,6][2,5][7,3]>, <Green's R-class: [1,3][2,4](5)>, 
  <Green's R-class: [2,5,3][7,6]>, <Green's R-class: [2,4][7,6](5)>, 
  <Green's R-class: [1,3,5][2,6]>, <Green's R-class: [1,6][2,4][5,3]>, 
  <Green's R-class: [3,6](1)(2)>, <Green's R-class: [2,6][5,1,3]>, 
  <Green's R-class: [1,3][5,6](2)>, <Green's R-class: [2,3][5,6][7,1]>, 
  <Green's R-class: [2,6][3,4](1)>, <Green's R-class: [2,3][5,4][7,6]>, 
  <Green's R-class: [5,4][7,3](2)>, <Green's R-class: [5,4][7,6]>, 
  <Green's R-class: [2,4,6]>, <Green's R-class: [3,4,6]>, 
  <Green's R-class: [5,4,6]>, <Green's R-class: [1,6][5,4](2)>, 
  <Green's R-class: [2,5,7,3]>, <Green's R-class: [2,3,7](1)>, 
  <Green's R-class: [2,7][5,1,4]>, <Green's R-class: [1,4][2,3][5,7]>, 
  <Green's R-class: [2,4][5,7,1]>, <Green's R-class: [2,1,5,4]>, 
  <Green's R-class: [2,4][7,5,3]>, <Green's R-class: [1,3,6][2,5]>, 
  <Green's R-class: [1,5][2,4][3,6]>, <Green's R-class: [1,5,3][2,6]>, 
  <Green's R-class: [2,4][5,6][7,3]>, <Green's R-class: [2,6][5,3][7,1]>, 
  <Green's R-class: [5,3][7,6](2)>, <Green's R-class: [2,3,1,6]>, 
  <Green's R-class: [2,6][5,1,4]>, <Green's R-class: [3,6][7,4]>, 
  <Green's R-class: [2,7,1][5,4]>, <Green's R-class: [2,3][5,4](7)>, 
  <Green's R-class: [2,4][3,1,7]>, <Green's R-class: [2,1][7,4](5)>, 
  <Green's R-class: [1,3,5][2,4]>, <Green's R-class: [1,6][2,5,3]>, 
  <Green's R-class: [1,6][2,4](5)>, <Green's R-class: [2,6][7,3](5)>, 
  <Green's R-class: [2,3][5,6](1)>, <Green's R-class: [2,6][5,4][7,1]>, 
  <Green's R-class: [3,4][5,6]>, <Green's R-class: [2,6,4]>, 
  <Green's R-class: [2,4][5,7](1)>, <Green's R-class: [2,5,6][7,3]>, 
  <Green's R-class: [2,4][7,5,6]>, <Green's R-class: [1,5][2,6](3)>, 
  <Green's R-class: [2,3][5,1][7,6]>, <Green's R-class: [2,6][3,1,4]>, 
  <Green's R-class: [2,4][5,1](7)>, <Green's R-class: [1,3][2,6](5)>, 
  <Green's R-class: [2,6][7,5,3]> ]
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
gap> ForAll(last, x -> x in s);
true
gap> Set(last2) = AsSSortedList(s);
true

# MiscTest14
gap> gens := [Transformation([1, 3, 2, 3]),
>  Transformation([1, 4, 1, 2]),
>  Transformation([2, 4, 1, 1]),
>  Transformation([3, 4, 2, 2])];;
gap> s := Semigroup(gens);;
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
gap> f := Transformation([4, 2, 4, 3]);;
gap> d := First(DClasses(s), x -> f in x);
<Green's D-class: Transformation( [ 1, 4, 1, 2 ] )>
gap> Transformation([1, 4, 1, 2]) in last;
true
gap> h := HClass(d, f);
<Green's H-class: Transformation( [ 4, 2, 4, 3 ] )>
gap> Transformation([4, 2, 4, 3]) in last;
true
gap> Size(h);
6
gap> IsGroupHClass(h);
true
gap> SchutzenbergerGroup(h);
Group([ (2,4), (2,3,4) ])
gap> ForAll(Elements(h), x -> x in h);
true
gap> ForAll(Elements(h), x -> x in d);
true
gap> IsGreensClassNC(h);
false
gap> gens := [PartialPermNC([1, 2, 4], [4, 5, 6]),
> PartialPermNC([1, 2, 5], [2, 1, 3]),
> PartialPermNC([1, 2, 4, 6], [2, 4, 3, 5]),
> PartialPermNC([1, 2, 3, 4, 5], [4, 3, 6, 5, 1])];;
gap> s := Semigroup(gens);;
gap> Size(s);
201
gap> f := PartialPerm([1 .. 5], [4, 3, 6, 5, 1]);;
gap> d := DClassNC(s, f);
<Green's D-class: [2,3,6](1,4,5)>
gap> h := HClassNC(d, f);
<Green's H-class: [2,3,6](1,4,5)>
gap> Size(h);
1
gap> Size(d);
1
gap> AsSSortedList(h) = AsSSortedList(d);
true

# MiscTest15
gap> gens :=
> [PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18,
>   19, 20, 21, 23, 24, 25, 26, 27, 28, 30, 32, 33, 34, 35, 36, 37, 40, 42, 44,
>   46, 47, 51, 53, 54, 58, 59, 60, 61, 63, 65, 66, 67, 69, 71, 72, 76, 79, 84,
>   86, 88, 94, 95, 100], [46, 47, 33, 32, 70, 97, 29, 30, 34, 11, 37, 89,
>   77, 52, 73, 2, 96, 66, 88, 69, 93, 87, 85, 68, 48, 25, 28, 43, 49, 95, 40,
>   24, 16, 94, 76, 63, 58, 23, 100, 38, 27, 78, 21, 71, 4, 72, 36, 13, 99, 90,
>   17, 41, 98, 10, 35, 91, 53, 45, 82, 42]),
>  PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 18, 19,
>   21, 22, 24, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 38, 39, 40, 41, 42, 43,
>   44, 46, 48, 49, 51, 52, 54, 56, 58, 59, 61, 64, 65, 67, 68, 70, 73, 74, 76,
>   78, 79, 80, 82, 88, 90, 97], [63, 38, 57, 12, 9, 91, 59, 32, 54, 83, 92,
>   96, 99, 18, 3, 81, 5, 65, 2, 37, 21, 49, 16, 75, 24, 23, 43, 27, 1, 48, 6,
>   35, 30, 79, 82, 51, 39, 25, 61, 77, 62, 22, 64, 14, 72, 7, 50, 8, 80, 19,
>   94, 69, 10, 40, 67, 28, 88, 93, 66, 36, 70, 56]),
>  PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
>   18, 21, 22, 23, 24, 25, 26, 27, 29, 30, 31, 34, 35, 36, 37, 38, 39, 40, 42,
>   43, 44, 46, 48, 49, 51, 52, 53, 55, 58, 60, 63, 64, 66, 67, 68, 69, 71, 73,
>   75, 80, 86, 87, 88, 90, 91, 94, 95, 97], [89, 85, 8, 56, 42, 10, 61, 25,
>   98, 55, 39, 92, 62, 21, 34, 57, 44, 14, 53, 64, 59, 84, 12, 87, 78, 83, 30,
>   32, 68, 73, 2, 86, 23, 48, 47, 79, 93, 15, 76, 97, 77, 11, 33, 100, 91, 67,
>   18, 16, 99, 60, 74, 17, 95, 49, 4, 66, 41, 69, 94, 31, 29, 5, 63, 58, 72]),
>  PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 17, 20, 21,
>   22, 23, 24, 26, 28, 29, 30, 32, 34, 35, 37, 39, 40, 42, 43, 44, 45, 46, 47,
>   48, 49, 51, 53, 54, 55, 56, 58, 59, 60, 61, 63, 64, 65, 66, 67, 68, 72, 74,
>   75, 79, 80, 82, 87, 88, 91, 92, 99, 100], [89, 67, 34, 15, 57, 29, 4, 62,
>   76, 20, 52, 22, 35, 75, 98, 78, 40, 46, 28, 6, 55, 90, 16, 12, 65, 26, 66,
>   36, 25, 61, 83, 38, 41, 93, 2, 39, 87, 85, 17, 92, 97, 43, 30, 5, 13, 94,
>   44, 80, 54, 99, 81, 31, 7, 68, 11, 100, 72, 14, 9, 91, 32, 64, 60, 8,
>   23])];;
gap> s := Semigroup(gens);;
gap> f := PartialPerm([2, 63], [28, 89]);;
gap> d := DClassNC(s, f);
<Green's D-class: [2,28][63,89]>
gap> Size(d);
4752
gap> RhoOrb(d);
<closed orbit, 2874 points with Schreier tree with log with grading>
gap> 2874 * 2;
5748
gap> LambdaOrb(d);
<closed orbit, 1 points with Schreier tree with log with grading>
gap> NrLClasses(d);
1
gap> NrRClasses(d);
4752
gap> f := PartialPerm([4, 29], [28, 89]);;
gap> f in d;
true
gap> h := HClass(d, f);
<Green's H-class: [4,28][29,89]>
gap> hh := HClassNC(d, f);
<Green's H-class: [4,28][29,89]>
gap> hh = h;
true
gap> Size(h);
1

# MiscTest16
gap> gens := [Transformation([1, 3, 2, 3]),
>  Transformation([1, 4, 1, 2]),
>  Transformation([3, 4, 2, 2]),
>  Transformation([4, 1, 2, 1])];;
gap> s := Monoid(gens);;
gap> List(DClasses(s), RClassReps);
[ [ IdentityTransformation ], [ Transformation( [ 1, 3, 2, 3 ] ) ], 
  [ Transformation( [ 1, 4, 1, 2 ] ), Transformation( [ 1, 2, 4, 4 ] ) ], 
  [ Transformation( [ 4, 1, 2, 1 ] ), Transformation( [ 4, 2, 1, 2 ] ) ], 
  [ Transformation( [ 3, 2, 3, 2 ] ), Transformation( [ 3, 2, 2, 2 ] ), 
      Transformation( [ 3, 3, 2, 3 ] ) ], [ Transformation( [ 1, 4, 2, 4 ] ) ]
    , [ Transformation( [ 4, 4, 2, 2 ] ), Transformation( [ 4, 2, 4, 4 ] ), 
      Transformation( [ 4, 4, 4, 2 ] ) ], [ Transformation( [ 1, 1, 1, 1 ] ) ]
    , [ Transformation( [ 4, 2, 4, 2 ] ), Transformation( [ 4, 2, 2, 2 ] ), 
      Transformation( [ 4, 4, 2, 4 ] ) ] ]
gap> reps := Concatenation(last);
[ IdentityTransformation, Transformation( [ 1, 3, 2, 3 ] ), 
  Transformation( [ 1, 4, 1, 2 ] ), Transformation( [ 1, 2, 4, 4 ] ), 
  Transformation( [ 4, 1, 2, 1 ] ), Transformation( [ 4, 2, 1, 2 ] ), 
  Transformation( [ 3, 2, 3, 2 ] ), Transformation( [ 3, 2, 2, 2 ] ), 
  Transformation( [ 3, 3, 2, 3 ] ), Transformation( [ 1, 4, 2, 4 ] ), 
  Transformation( [ 4, 4, 2, 2 ] ), Transformation( [ 4, 2, 4, 4 ] ), 
  Transformation( [ 4, 4, 4, 2 ] ), Transformation( [ 1, 1, 1, 1 ] ), 
  Transformation( [ 4, 2, 4, 2 ] ), Transformation( [ 4, 2, 2, 2 ] ), 
  Transformation( [ 4, 4, 2, 4 ] ) ]
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
gap> List(reps, x -> DClass(s, x));
[ <Green's D-class: IdentityTransformation>, 
  <Green's D-class: Transformation( [ 1, 3, 2, 3 ] )>, 
  <Green's D-class: Transformation( [ 1, 4, 1, 2 ] )>, 
  <Green's D-class: Transformation( [ 1, 2, 4, 4 ] )>, 
  <Green's D-class: Transformation( [ 4, 1, 2, 1 ] )>, 
  <Green's D-class: Transformation( [ 4, 2, 1, 2 ] )>, 
  <Green's D-class: Transformation( [ 3, 2, 3, 2 ] )>, 
  <Green's D-class: Transformation( [ 3, 2, 2, 2 ] )>, 
  <Green's D-class: Transformation( [ 3, 3, 2, 3 ] )>, 
  <Green's D-class: Transformation( [ 1, 4, 2, 4 ] )>, 
  <Green's D-class: Transformation( [ 4, 4, 2, 2 ] )>, 
  <Green's D-class: Transformation( [ 4, 2, 4, 4 ] )>, 
  <Green's D-class: Transformation( [ 4, 4, 4, 2 ] )>, 
  <Green's D-class: Transformation( [ 1, 1, 1, 1 ] )>, 
  <Green's D-class: Transformation( [ 4, 2, 4, 2 ] )>, 
  <Green's D-class: Transformation( [ 4, 2, 2, 2 ] )>, 
  <Green's D-class: Transformation( [ 4, 4, 2, 4 ] )> ]
gap> d := DClass(s, Transformation([1, 2, 4, 4]));
<Green's D-class: Transformation( [ 1, 2, 4, 4 ] )>
gap> Transformation([1, 4, 1, 2]) in last;
true
gap> f := Transformation([1, 2, 4, 4]);
Transformation( [ 1, 2, 4, 4 ] )
gap> o := LambdaOrb(s);
<closed orbit, 15 points with Schreier tree with log>
gap> HasRhoOrb(s) and IsClosedOrbit(RhoOrb(s));
true
gap> o := RhoOrb(s);
<closed orbit, 12 points with Schreier tree with log>
gap> List(reps, x -> DClass(s, x));
[ <Green's D-class: IdentityTransformation>, 
  <Green's D-class: Transformation( [ 1, 3, 2, 3 ] )>, 
  <Green's D-class: Transformation( [ 1, 4, 1, 2 ] )>, 
  <Green's D-class: Transformation( [ 1, 2, 4, 4 ] )>, 
  <Green's D-class: Transformation( [ 4, 1, 2, 1 ] )>, 
  <Green's D-class: Transformation( [ 4, 2, 1, 2 ] )>, 
  <Green's D-class: Transformation( [ 3, 2, 3, 2 ] )>, 
  <Green's D-class: Transformation( [ 3, 2, 2, 2 ] )>, 
  <Green's D-class: Transformation( [ 3, 3, 2, 3 ] )>, 
  <Green's D-class: Transformation( [ 1, 4, 2, 4 ] )>, 
  <Green's D-class: Transformation( [ 4, 4, 2, 2 ] )>, 
  <Green's D-class: Transformation( [ 4, 2, 4, 4 ] )>, 
  <Green's D-class: Transformation( [ 4, 4, 4, 2 ] )>, 
  <Green's D-class: Transformation( [ 1, 1, 1, 1 ] )>, 
  <Green's D-class: Transformation( [ 4, 2, 4, 2 ] )>, 
  <Green's D-class: Transformation( [ 4, 2, 2, 2 ] )>, 
  <Green's D-class: Transformation( [ 4, 4, 2, 4 ] )> ]
gap> Union(List(last, x -> LClass(x, Representative(x))));
[ Transformation( [ 1, 1, 1, 1 ] ), Transformation( [ 1, 2, 1 ] ), 
  Transformation( [ 1, 2, 3, 2 ] ), IdentityTransformation, 
  Transformation( [ 1, 2, 4, 2 ] ), Transformation( [ 1, 2, 4, 4 ] ), 
  Transformation( [ 1, 3, 2, 3 ] ), Transformation( [ 1, 4, 1, 2 ] ), 
  Transformation( [ 1, 4, 2, 2 ] ), Transformation( [ 1, 4, 2, 4 ] ), 
  Transformation( [ 2, 2, 2 ] ), Transformation( [ 2, 2, 3, 2 ] ), 
  Transformation( [ 2, 2, 4, 2 ] ), Transformation( [ 2, 2, 4, 4 ] ), 
  Transformation( [ 2, 3, 2, 3 ] ), Transformation( [ 2, 3, 3, 3 ] ), 
  Transformation( [ 2, 4, 2, 2 ] ), Transformation( [ 2, 4, 2, 4 ] ), 
  Transformation( [ 2, 4, 4, 4 ] ), Transformation( [ 3, 2, 2, 2 ] ), 
  Transformation( [ 3, 2, 3, 2 ] ), Transformation( [ 3, 3, 2, 3 ] ), 
  Transformation( [ 4, 1, 2, 1 ] ), Transformation( [ 4, 2, 1, 2 ] ), 
  Transformation( [ 4, 2, 2, 2 ] ), Transformation( [ 4, 2, 4, 2 ] ), 
  Transformation( [ 4, 2, 4, 4 ] ), Transformation( [ 4, 4, 2, 2 ] ), 
  Transformation( [ 4, 4, 2, 4 ] ), Transformation( [ 4, 4, 4, 2 ] ) ]
gap> Length(last);
30
gap> D := List(reps, x -> DClass(s, x));
[ <Green's D-class: IdentityTransformation>, 
  <Green's D-class: Transformation( [ 1, 3, 2, 3 ] )>, 
  <Green's D-class: Transformation( [ 1, 4, 1, 2 ] )>, 
  <Green's D-class: Transformation( [ 1, 2, 4, 4 ] )>, 
  <Green's D-class: Transformation( [ 4, 1, 2, 1 ] )>, 
  <Green's D-class: Transformation( [ 4, 2, 1, 2 ] )>, 
  <Green's D-class: Transformation( [ 3, 2, 3, 2 ] )>, 
  <Green's D-class: Transformation( [ 3, 2, 2, 2 ] )>, 
  <Green's D-class: Transformation( [ 3, 3, 2, 3 ] )>, 
  <Green's D-class: Transformation( [ 1, 4, 2, 4 ] )>, 
  <Green's D-class: Transformation( [ 4, 4, 2, 2 ] )>, 
  <Green's D-class: Transformation( [ 4, 2, 4, 4 ] )>, 
  <Green's D-class: Transformation( [ 4, 4, 4, 2 ] )>, 
  <Green's D-class: Transformation( [ 1, 1, 1, 1 ] )>, 
  <Green's D-class: Transformation( [ 4, 2, 4, 2 ] )>, 
  <Green's D-class: Transformation( [ 4, 2, 2, 2 ] )>, 
  <Green's D-class: Transformation( [ 4, 4, 2, 4 ] )> ]
gap> Length(Set(D));
9
gap> List(D, x -> LClass(x, Representative(x)));
[ <Green's L-class: IdentityTransformation>, 
  <Green's L-class: Transformation( [ 1, 3, 2, 3 ] )>, 
  <Green's L-class: Transformation( [ 1, 4, 1, 2 ] )>, 
  <Green's L-class: Transformation( [ 1, 2, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 4, 1, 2, 1 ] )>, 
  <Green's L-class: Transformation( [ 4, 2, 1, 2 ] )>, 
  <Green's L-class: Transformation( [ 3, 2, 3, 2 ] )>, 
  <Green's L-class: Transformation( [ 3, 2, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 2, 3 ] )>, 
  <Green's L-class: Transformation( [ 1, 4, 2, 4 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 4, 2, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 4, 2 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 4, 2, 4, 2 ] )>, 
  <Green's L-class: Transformation( [ 4, 2, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 2, 4 ] )> ]
gap> Union(last);
[ Transformation( [ 1, 1, 1, 1 ] ), Transformation( [ 1, 2, 1 ] ), 
  Transformation( [ 1, 2, 3, 2 ] ), IdentityTransformation, 
  Transformation( [ 1, 2, 4, 2 ] ), Transformation( [ 1, 2, 4, 4 ] ), 
  Transformation( [ 1, 3, 2, 3 ] ), Transformation( [ 1, 4, 1, 2 ] ), 
  Transformation( [ 1, 4, 2, 2 ] ), Transformation( [ 1, 4, 2, 4 ] ), 
  Transformation( [ 2, 2, 2 ] ), Transformation( [ 2, 2, 3, 2 ] ), 
  Transformation( [ 2, 2, 4, 2 ] ), Transformation( [ 2, 2, 4, 4 ] ), 
  Transformation( [ 2, 3, 2, 3 ] ), Transformation( [ 2, 3, 3, 3 ] ), 
  Transformation( [ 2, 4, 2, 2 ] ), Transformation( [ 2, 4, 2, 4 ] ), 
  Transformation( [ 2, 4, 4, 4 ] ), Transformation( [ 3, 2, 2, 2 ] ), 
  Transformation( [ 3, 2, 3, 2 ] ), Transformation( [ 3, 3, 2, 3 ] ), 
  Transformation( [ 4, 1, 2, 1 ] ), Transformation( [ 4, 2, 1, 2 ] ), 
  Transformation( [ 4, 2, 2, 2 ] ), Transformation( [ 4, 2, 4, 2 ] ), 
  Transformation( [ 4, 2, 4, 4 ] ), Transformation( [ 4, 4, 2, 2 ] ), 
  Transformation( [ 4, 4, 2, 4 ] ), Transformation( [ 4, 4, 4, 2 ] ) ]
gap> Set(last2) = Set(LClasses(s));
false
gap> L := Set(last3);
[ <Green's L-class: Transformation( [ 1, 1, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 1, 4, 1, 2 ] )>, 
  <Green's L-class: Transformation( [ 1, 3, 2, 3 ] )>, 
  <Green's L-class: IdentityTransformation>, 
  <Green's L-class: Transformation( [ 1, 4, 2, 4 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 3, 2, 3, 2 ] )>, 
  <Green's L-class: Transformation( [ 4, 2, 4, 2 ] )>, 
  <Green's L-class: Transformation( [ 4, 1, 2, 1 ] )> ]

# MiscTest17
gap> gens :=
> [PartialPermNC([1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18,
>   20, 22, 23, 24, 25, 26, 28, 29, 31, 32, 33, 35, 36, 37, 38, 41, 42, 44, 45,
>   50, 51, 52, 54, 55, 60, 62, 64, 65, 66, 68, 71, 73, 75, 77, 78, 79, 83, 84,
>   94, 95, 96, 97], [30, 56, 33, 17, 43, 34, 28, 78, 91, 24, 44, 84, 71, 81,
>   57, 90, 20, 69, 70, 6, 82, 26, 53, 86, 32, 22, 12, 95, 59, 40, 73, 76, 98,
>   48, 80, 51, 9, 27, 49, 93, 52, 60, 94, 11, 75, 96, 72, 4, 87, 37, 29, 50,
>   39, 45, 88, 67, 14, 99]),
>  PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 19,
>   20, 21, 23, 24, 25, 26, 28, 30, 32, 35, 36, 37, 41, 42, 43, 47, 48, 49, 50,
>   51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 64, 65, 67, 68, 69, 71, 72, 74, 76,
>   81, 82, 83, 84, 86, 87, 92, 93], [56, 4, 87, 14, 67, 82, 17, 73, 18, 12,
>   35, 43, 80, 99, 7, 96, 58, 76, 36, 30, 98, 26, 62, 1, 75, 27, 10, 74, 55,
>   47, 37, 95, 39, 52, 84, 72, 50, 53, 77, 24, 59, 66, 9, 49, 70, 6, 51, 89,
>   21, 11, 85, 15, 19, 28, 79, 40, 34, 71, 5, 29, 88, 16, 8]),
>  PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18,
>   19, 20, 22, 23, 24, 25, 26, 27, 28, 31, 32, 33, 34, 35, 36, 37, 39, 41, 42,
>   44, 46, 50, 52, 53, 54, 56, 57, 58, 59, 61, 62, 63, 64, 65, 68, 70, 71, 72,
>   77, 81, 84, 88, 89, 91, 93, 95, 97, 99, 100],
> [53, 10, 43, 41, 57, 14, 68, 20, 54, 62, 5, 49, 86, 56, 91, 48, 9, 87, 33,
>   64, 60, 13, 70, 92, 80, 69, 35, 88, 98, 4, 96, 79, 94, 71, 61, 27, 89, 97,
>   46, 28, 40, 3, 100, 17, 19, 39, 82, 52, 6, 16, 77, 76, 45, 67, 23, 31, 29,
>   12, 95, 72, 85, 7, 26, 38, 18, 24]),
>  PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 16, 20, 21, 23,
>   24, 25, 26, 27, 28, 29, 31, 32, 33, 34, 35, 36, 38, 40, 41, 42, 43, 44, 47,
>   48, 49, 50, 53, 54, 55, 56, 58, 59, 62, 64, 65, 66, 68, 69, 70, 72, 74, 76,
>   78, 83, 84, 86, 90, 91, 92, 93, 94, 99, 100],
> [3, 77, 85, 63, 47, 30, 68, 21, 95, 13, 49, 33, 62, 6, 78, 81, 83, 35, 69,
>   50, 26, 61, 27, 93, 56, 39, 48, 5, 19, 52, 73, 12, 8, 89, 25, 86, 84, 14,
>   70, 29, 58, 88, 43, 37, 10, 92, 65, 22, 76, 38, 74, 34, 4, 94, 82, 67, 60,
>   2, 23, 59, 80, 11, 40, 98, 51, 28])];;
gap> s := Semigroup(gens);
<partial perm semigroup of rank 97 with 4 generators>
gap> f :=
> PartialPermNC([5, 7, 11, 12, 14, 24, 25, 26, 27, 29, 31, 32, 34, 35, 41,
>   42, 44, 47, 48, 49, 50, 53, 62, 69, 70, 86, 92],
> [23, 52, 39, 62, 11, 47, 94, 34, 70, 50, 73, 89, 2, 86, 14, 81, 74, 83, 77,
>   92, 48, 26, 13, 98, 84, 60, 33]);;
gap> d := DClass(s, f);
<Green's D-class: [5,23][7,52][12,62,13][24,47,83][25,94][27,70,84]
 [29,50,48,77][31,73][32,89][35,86,60][41,14,11,39][42,81][44,74][49,92,33]
 [53,26,34,2][69,98]>
gap> Size(d);
1
gap> RhoOrb(d);
<closed orbit, 1 points with Schreier tree with log with grading>
gap> LambdaOrb(d);
<closed orbit, 35494 points with Schreier tree with log>
gap> f := PartialPerm([5, 7, 56, 83, 92], [30, 52, 16, 21, 29]);;
gap> d := DClassNC(s, f);
<Green's D-class: [5,30][7,52][56,16][83,21][92,29]>
gap> Size(d);
1
gap> iter := IteratorOfDClasses(s);
<iterator>
gap> repeat d := NextIterator(iter); until Size(d) > 1;
gap> d;
<Green's D-class: [74,16][84,34]>
gap> Size(d);
6793298
gap> f := PartialPerm([1, 88], [78, 48]);;
gap> f in d;
true
gap> r := RClass(d, f);
<Green's R-class: [1,78][88,48]>
gap> ForAll(r, x -> x in d);
true
gap> Size(r);
3686
gap> NrLClasses(d) * last;
6793298
gap> SchutzenbergerGroup(r);
Group([ (16,34) ])
gap> SchutzenbergerGroup(d);
Group([ (16,34) ])
gap> IsRegularDClass(d);
true
gap> IsRegularGreensClass(r);
true
gap> ForAll(r, x -> x in r);
true
gap> repeat d := NextIterator(iter); until Size(d) > 1;
gap> d;
<Green's D-class: [41,34][50,16]>
gap> Size(d);
3686
gap> f := PartialPerm([41, 50], [17, 32]);;
gap> r := RClassNC(d, f);
<Green's R-class: [41,17][50,32]>
gap> Size(r);
3686
gap> ForAll(r, x -> x in d);
true
gap> ForAll(d, x -> x in r);
true
gap> rr := RClass(s, f);
<Green's R-class: [41,17][50,32]>
gap> AsSSortedList(rr) = AsSSortedList(r);
true
gap> d;
<Green's D-class: [41,34][50,16]>
gap> GroupHClass(d);
fail

# MiscTest18
gap> gens := [Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>   Transformation([3, 1, 4, 2, 5, 2, 1, 6, 1, 7]),
>   Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]),
>   Transformation([4, 7, 6, 9, 10, 1, 3, 6, 6, 2]),
>   Transformation([5, 9, 10, 9, 6, 3, 8, 4, 6, 5]),
>   Transformation([6, 2, 2, 7, 8, 8, 2, 10, 2, 4]),
>   Transformation([6, 2, 8, 4, 7, 5, 8, 3, 5, 8]),
>   Transformation([7, 1, 4, 3, 2, 7, 7, 6, 6, 5]),
>   Transformation([7, 10, 10, 1, 7, 9, 10, 4, 2, 10]),
>   Transformation([10, 7, 10, 8, 8, 7, 5, 9, 1, 9])];;
gap> s := Semigroup(gens);
<transformation semigroup of degree 10 with 10 generators>
gap> f := Transformation([6, 6, 6, 6, 6, 10, 6, 6, 6, 6]);;
gap> d := DClassNC(s, f);
<Green's D-class: Transformation( [ 6, 6, 6, 6, 6, 10, 6, 6, 6, 6 ] )>
gap> Transformation([6, 6, 6, 6, 6, 10, 6, 6, 6, 6]) in last;
true
gap> Size(d);
31680
gap> IsRegularDClass(d);
true
gap> GroupHClass(d);
<Green's H-class: Transformation( [ 10, 10, 10, 10, 10, 6, 10, 10, 10, 10 ] )>
gap> Transformation([10, 10, 10, 10, 10, 6, 10, 10, 10, 10]) in last;
true

# MiscTest19
gap> gens := [PartialPermNC([1, 3, 4, 6, 10], [3, 4, 1, 6, 10]),
> PartialPermNC([1, 2, 3, 4, 5, 6], [10, 3, 9, 1, 5, 8]),
> PartialPermNC([1, 2, 3, 4, 6, 10], [1, 8, 2, 3, 4, 9]),
> PartialPermNC([1, 2, 3, 4, 8, 9, 10], [5, 8, 9, 7, 2, 6, 10])];;
gap> s := Semigroup(gens);
<partial perm semigroup of rank 9 with 4 generators>
gap> Size(s);
789
gap> NrDClasses(s);
251
gap> d := DClasses(s)[251];
<Green's D-class: [4,7](2)>
gap> Size(d);
1
gap> First(DClasses(s), IsRegularDClass);
<Green's D-class: (1,3,4)(6)(10)>
gap> d := last;
<Green's D-class: (1,3,4)(6)(10)>
gap> Size(d);
3
gap> h := GroupHClass(d);
<Green's H-class: <identity partial perm on [ 1, 3, 4, 6, 10 ]>>
gap> PartialPerm([1, 3, 4, 6, 10], [1, 3, 4, 6, 10]) in last;
true
gap> Size(h);
3
gap> AsSSortedList(h) = AsSSortedList(d);
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
  [ <identity partial perm on [ 8 ]>, <identity partial perm on [ 2 ]>, 
      <identity partial perm on [ 3 ]>, <identity partial perm on [ 4 ]>, 
      <identity partial perm on [ 6 ]>, <identity partial perm on [ 9 ]>, 
      <identity partial perm on [ 10 ]>, <identity partial perm on [ 1 ]> ], 
  [  ], [  ], [  ], [  ], [  ], [ <identity partial perm on [ 5 ]> ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [ <identity partial perm on [ 2, 8, 10 ]> ], 
  [ <empty partial perm> ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [ <identity partial perm on [ 3, 4 ]>, <identity partial perm on [ 1, 3 ]>, 
      <identity partial perm on [ 1, 4 ]> ], [  ], [  ], [  ], [  ], [  ], 
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
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ] ]
gap> Concatenation(last);
[ <identity partial perm on [ 1, 3, 4, 6, 10 ]>, 
  <identity partial perm on [ 8 ]>, <identity partial perm on [ 2 ]>, 
  <identity partial perm on [ 3 ]>, <identity partial perm on [ 4 ]>, 
  <identity partial perm on [ 6 ]>, <identity partial perm on [ 9 ]>, 
  <identity partial perm on [ 10 ]>, <identity partial perm on [ 1 ]>, 
  <identity partial perm on [ 5 ]>, <identity partial perm on [ 2, 8, 10 ]>, 
  <empty partial perm>, <identity partial perm on [ 3, 4 ]>, 
  <identity partial perm on [ 1, 3 ]>, <identity partial perm on [ 1, 4 ]> ]
gap> ForAll(last, x -> x in s);
true
gap> Set(last2) = Idempotents(s);
false
gap> Set(last3) = Set(Idempotents(s));
true

# MiscTest20
gap> gens := [Transformation([1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10]),
> Transformation([2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7])];;
gap> s := Monoid(gens);;
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
gap> e := last;
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
gap> ForAll(e, x -> x in s);
true
gap> Set(Idempotents(s)) = Set(e);
true

# MiscTest21
gap> gens :=
> [PartialPermNC([1, 2, 3, 4, 5, 6], [7, 10, 8, 6, 4, 2]),
>  PartialPermNC([1, 2, 3, 4, 5, 9], [6, 8, 3, 10, 4, 2]),
>  PartialPermNC([1, 2, 3, 5, 6, 7], [8, 7, 5, 6, 2, 9]),
>  PartialPermNC([1, 2, 3, 5, 6, 8], [9, 3, 4, 7, 8, 6])];;
gap> s := Semigroup(gens);;
gap> Size(s);
489
gap> First(DClasses(s), IsRegularDClass);
<Green's D-class: <empty partial perm>>
gap> NrRegularDClasses(s);
5
gap> PositionsProperty(DClasses(s), IsRegularDClass);
[ 25, 26, 32, 35, 63 ]
gap> d := DClasses(s)[26];
<Green's D-class: [3,8]>
gap> NrLClasses(d);
8
gap> NrRClasses(d);
8
gap> Size(d);
64
gap> Idempotents(d);
[ <identity partial perm on [ 3 ]>, <identity partial perm on [ 2 ]>, 
  <identity partial perm on [ 6 ]>, <identity partial perm on [ 4 ]>, 
  <identity partial perm on [ 5 ]>, <identity partial perm on [ 8 ]>, 
  <identity partial perm on [ 9 ]>, <identity partial perm on [ 7 ]> ]
gap> ForAll(last, x -> x in d);
true
gap> dd := DClassNC(s, PartialPermNC([8], [9]));
<Green's D-class: [8,9]>
gap> dd = d;
true
gap> Size(dd);
64
gap> Idempotents(dd);
[ <identity partial perm on [ 8 ]>, <identity partial perm on [ 3 ]>, 
  <identity partial perm on [ 2 ]>, <identity partial perm on [ 6 ]>, 
  <identity partial perm on [ 4 ]>, <identity partial perm on [ 5 ]>, 
  <identity partial perm on [ 9 ]>, <identity partial perm on [ 7 ]> ]
gap> Set(LClassReps(dd)) = Set(LClassReps(d));
false
gap> LClassReps(dd);
[ [8,9], [8,2], <identity partial perm on [ 8 ]>, [8,6], [8,7], [8,3], [8,5], 
  [8,4] ]
gap> LClassReps(d);
[ [3,8], [3,6], [3,2], <identity partial perm on [ 3 ]>, [3,5], [3,7], [3,9], 
  [3,4] ]
gap> Set(List(LClassReps(d), x -> LClass(d, x))) =
> Set(List(LClassReps(dd), x -> LClass(d, x)));
true
gap> Set(List(LClassReps(d), x -> LClass(d, x))) = Set(List(LClassReps(dd),
> x -> LClass(dd, x)));
true
gap> ForAll(LClassReps(dd), x -> x in d);
true
gap> ForAll(LClassReps(d), x -> x in dd);
true

# MiscTest22
gap> gens :=
> [PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
> [2, 3, 4, 5, 6, 7, 8, 9, 10, 1]),
>  PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
> [2, 1, 3, 4, 5, 6, 7, 8, 9, 10]),
>  PartialPermNC([1, 2, 4, 7, 10], [8, 5, 9, 6, 7])];;
gap> s := Semigroup(gens);;
gap> Size(s);
12398231
gap> NrRClasses(s);
639
gap> f := PartialPerm([3, 9], [5, 4]);;
gap> d := DClass(s, f);
<Green's D-class: [3,5][9,4]>
gap> Position(LambdaOrb(d), ImageSetOfPartialPerm(d!.rep));
7
gap> OrbSCC(RhoOrb(d))[RhoOrbSCCIndex(d)];
[ 61, 38, 60, 92, 39, 62, 94, 93, 133, 184, 64, 96, 136, 134, 160, 211, 8, 
  15, 24, 40, 273, 146, 185, 158, 209, 270, 339, 407, 271, 240, 305, 63, 95, 
  135, 371, 435, 239, 304, 370, 434, 132, 183, 238, 303, 369 ]
gap> OrbSCC(LambdaOrb(d))[LambdaOrbSCCIndex(d)];
[ 7, 13, 20, 27, 36, 48, 65, 67, 90, 53, 73, 95, 45, 62, 86, 110, 49, 34, 
  139, 170, 115, 143, 172, 208, 66, 71, 92, 116, 119, 147, 177, 173, 209, 
  244, 278, 245, 120, 137, 148, 178, 214, 114, 142, 89, 113 ]
gap> NrIdempotents(d);
45
gap> Number(Idempotents(s), x -> x in d);
45
gap> s := Semigroup(gens);
<partial perm semigroup of rank 10 with 3 generators>
gap> d := DClass(s, f);
<Green's D-class: [3,5][9,4]>
gap> s := Semigroup(gens);
<partial perm semigroup of rank 10 with 3 generators>
gap> d := DClassNC(s, f);
<Green's D-class: [3,5][9,4]>
gap> NrIdempotents(d);
45
gap> Number(Idempotents(s), x -> x in d);
45
gap> s := Semigroup(gens);
<partial perm semigroup of rank 10 with 3 generators>
gap> l := LClass(s, f);
<Green's L-class: [3,5][9,4]>
gap> d := DClassOfLClass(l);
<Green's D-class: [3,5][9,4]>
gap> NrIdempotents(d);
45
gap> s := Semigroup(gens);
<partial perm semigroup of rank 10 with 3 generators>
gap> l := LClass(s, f);
<Green's L-class: [3,5][9,4]>
gap> s := Semigroup(gens);
<partial perm semigroup of rank 10 with 3 generators>
gap> l := LClassNC(s, f);
<Green's L-class: [3,5][9,4]>
gap> d := DClassOfLClass(l);
<Green's D-class: [3,5][9,4]>
gap> NrIdempotents(d);
45
gap> s := Semigroup(gens);
<partial perm semigroup of rank 10 with 3 generators>
gap> r := RClass(s, f);
<Green's R-class: [3,5][9,4]>
gap> d := DClassOfRClass(r);
<Green's D-class: [3,7][9,6]>
gap> NrIdempotents(d);
45
gap> s := Semigroup(gens);
<partial perm semigroup of rank 10 with 3 generators>
gap> r := RClassNC(s, f);
<Green's R-class: [3,5][9,4]>
gap> d := DClassOfRClass(r);
<Green's D-class: [3,5][9,4]>
gap> NrIdempotents(d);
45
gap> r := RClassNC(s, f);
<Green's R-class: [3,5][9,4]>
gap> d := DClassOfRClass(r);
<Green's D-class: [3,5][9,4]>
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

# MiscTest23
gap> gens := [Transformation([1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10]),
> Transformation([2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7])];;
gap> s := Monoid(gens);;
gap> NrRegularDClasses(s);
3
gap> NrDClasses(s);
3
gap> IsRegularSemigroup(s);
true
gap> d := DClasses(s)[2];
<Green's D-class: Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] )>
gap> Transformation([1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10]) in last;
true
gap> NrHClasses(d);
1
gap> h := GroupHClass(d);
<Green's H-class: Transformation( [ 1, 2, 4, 4, 5, 6, 7, 6, 10, 10 ] )>
gap> Transformation([1, 2, 4, 4, 5, 6, 7, 6, 10, 10]) in last;
true
gap> AsSSortedList(h) = AsSSortedList(d);
true
gap> Size(d);
7
gap> Size(h);
7

# MiscTest24
gap> gens :=
> [PartialPermNC([1, 2, 3, 5, 6, 7, 12], [11, 10, 3, 4, 6, 2, 8]),
>  PartialPermNC([1, 2, 4, 5, 6, 8, 9, 10, 11],
> [2, 8, 1, 10, 11, 4, 7, 6, 9])];;
gap> s := Semigroup(gens);
<partial perm semigroup of rank 12 with 2 generators>
gap> Size(s);
251
gap> d := DClass(s, PartialPerm([5, 12], [6, 9]));;
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

# MiscTest25
gap> gens :=
> [PartialPermNC([1, 2, 3, 4, 9, 10, 11], [4, 1, 7, 12, 3, 9, 6]),
> PartialPermNC([1, 3, 4, 5, 7, 8, 11, 12], [4, 11, 2, 7, 9, 8, 1, 6])];;
gap> s := Semigroup(gens);;
gap> f := PartialPerm([4, 7, 11], [2, 9, 6]);;
gap> d := DClassNC(s, f);
<Green's D-class: [4,2][7,9][11,6]>
gap> NrHClasses(s);
125
gap> d := DClass(s, f);
<Green's D-class: [4,2][7,9][11,6]>
gap> NrHClasses(s);
125
gap> NrHClasses(d);
1
gap> d := DClassNC(s, f);
<Green's D-class: [4,2][7,9][11,6]>
gap> NrHClasses(d);
1
gap> d := DClass(LClass(s, f));
<Green's D-class: [4,2][7,9][11,6]>
gap> NrHClasses(d);
1
gap> d := DClass(RClass(s, f));
<Green's D-class: [4,2][7,9][11,6]>
gap> NrHClasses(d);
1
gap> NrRegularDClasses(s);
4
gap> NrDClasses(s);
65
gap> RClassReps(d);
[ [4,2][7,9][11,6] ]
gap> iter := IteratorOfDClasses(s);
<iterator>
gap> repeat
>   d := NextIterator(iter);
> until IsDoneIterator(iter) or Size(d) > 1000;
gap> d;
<Green's D-class: [1,6][5,4]>
gap> Size(d);
1
gap> List(DClasses(s), Size);
[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 3, 9, 1, 1, 1, 3, 1, 1, 1, 1, 
  1, 3, 1, 3, 3, 3, 3, 1, 9, 3, 1, 3, 1, 3, 1, 1, 1, 1, 1, 3, 3, 1, 3, 1, 3, 
  9, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1 ]
gap> Position(last, 9);
17
gap> d := DClasses(s)[17];
<Green's D-class: [1,4][3,7]>
gap> Size(d);
9
gap> IsRegularDClass(d);
true
gap> RClassReps(d);
[ [1,4][3,7], [2,4][9,7], <identity partial perm on [ 4, 7 ]> ]
gap> d := DClassNC(s, Representative(d));
<Green's D-class: [1,4][3,7]>
gap> RClassReps(d);
[ [1,4][3,7], [2,4][9,7], <identity partial perm on [ 4, 7 ]> ]
gap> s := Semigroup(Generators(s));
<partial perm semigroup of rank 11 with 2 generators>
gap> d := DClass(HClass(s, Representative(d)));
<Green's D-class: [1,4][3,7]>
gap> RClassReps(d);
[ [1,4][3,7], [2,4][9,7], <identity partial perm on [ 4, 7 ]> ]
gap> Size(d);
9
gap> Number(s, x -> x in d);
9
gap> ForAll(d, x -> x in d);
true
gap> HClassReps(d);
[ [1,4][3,7], [1,2][3,9], <identity partial perm on [ 1, 3 ]>, [2,4][9,7], 
  <identity partial perm on [ 2, 9 ]>, [2,1][9,3], 
  <identity partial perm on [ 4, 7 ]>, [4,2][7,9], [4,1][7,3] ]
gap> Set(last) = Elements(d);
true

# MiscTest26
gap> gens := [Transformation([2, 1, 4, 5, 3, 7, 8, 9, 10, 6]),
> Transformation([1, 2, 4, 3, 5, 6, 7, 8, 9, 10]),
> Transformation([1, 2, 3, 4, 5, 6, 10, 9, 8, 7]),
> Transformation([9, 1, 4, 3, 6, 9, 3, 4, 3, 9])];;
gap> s := Monoid(gens);;
gap> f := Transformation([2, 1, 3, 5, 4, 10, 9, 8, 7, 6]);;
gap> d := DClass(HClass(s, f));
<Green's D-class: Transformation( [ 2, 1, 3, 5, 4, 10, 9, 8, 7, 6 ] )>
gap> Transformation([2, 1, 3, 5, 4, 10, 9, 8, 7, 6]) in last;
true
gap> Size(d);
120
gap> HClassReps(d);
[ Transformation( [ 2, 1, 3, 5, 4, 10, 9, 8, 7, 6 ] ) ]
gap> h := GroupHClass(d);
<Green's H-class: IdentityTransformation>
gap> ForAll(h, x -> x in d) and ForAll(d, x -> x in h);
true
gap> Size(s);
491558
gap> f := Transformation([6, 6, 3, 6, 4, 6, 6, 6, 6, 4]);;
gap> d := DClass(HClass(s, f));
<Green's D-class: Transformation( [ 6, 6, 3, 6, 4, 6, 6, 6, 6, 4 ] )>
gap> Transformation([9, 4, 9, 6, 4, 9, 6, 9, 6, 9]) in last;
true
gap> Size(d);
121500
gap> NrHClasses(d);
20250
gap> Length(HClassReps(d));
20250
gap> ForAll(HClassReps(d), x -> x in d);
true
gap> d := DClass(RClass(s, f));
<Green's D-class: Transformation( [ 9, 9, 6, 9, 4, 9, 9, 9, 9, 4 ] )>
gap> Transformation([9, 4, 9, 6, 4, 9, 6, 9, 6, 9]) in last;
true
gap> Size(d);
121500
gap> ForAll(d, x -> x in d);
true
gap> NrIdempotents(d);
5550
gap> ForAll(Idempotents(d), x -> x in d);
true

# MiscTest27: R-class
gap> gens := [
> Transformation([2, 2, 3, 5, 5, 6, 7, 8, 14, 16, 16, 17, 18, 14, 16, 16, 17,
>                 18]),
> Transformation([1, 3, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
>                 18]),
> Transformation([1, 2, 4, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 15, 16, 17,
>                 18]),
> Transformation([1, 2, 3, 4, 6, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 17,
>                 18]),
> Transformation([1, 2, 3, 4, 5, 7, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18,
>                 18]),
> Transformation([1, 2, 3, 4, 5, 6, 8, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
>                 2]),
> Transformation([1, 2, 9, 10, 11, 12, 13, 1, 9, 10, 11, 12, 13, 14, 15, 16,
>                 17, 18])];;
gap> s := Semigroup(gens);;
gap> f := Transformation([1, 2, 4, 4, 6, 6, 7, 8, 9, 10, 11, 12, 13, 15, 15,
>                         17, 17, 18]);;
gap> r := RClassNC(s, f);
<Green's R-class: Transformation( [ 1, 2, 4, 4, 6, 6, 7, 8, 9, 10, 11, 12, 13,
  15, 15, 17, 17 ] )>
gap> Size(r);
1
gap> SchutzenbergerGroup(r);
Group(())
gap> f := Transformation([1, 2, 10, 10, 11, 12, 13, 1, 9, 10, 11, 12, 13, 15,
>                         15, 16, 17, 18]);;
gap> r := RClass(s, f);
<Green's R-class: Transformation( [ 1, 2, 10, 10, 11, 12, 13, 1, 9, 10, 11,
   12, 13, 15, 15 ] )>
gap> Size(r);
1
gap> SchutzenbergerGroup(r);
Group(())

# MiscTest28
gap> gens := [
> Transformation([2, 4, 1, 5, 4, 4, 7, 3, 8, 1]),
> Transformation([9, 1, 2, 8, 1, 5, 9, 9, 9, 5]),
> Transformation([9, 3, 1, 5, 10, 3, 4, 6, 10, 2]),
> Transformation([10, 7, 3, 7, 1, 9, 8, 8, 4, 10])];;
gap> s := Semigroup(gens);;
gap> f := Transformation([9, 10, 10, 3, 10, 9, 9, 9, 9, 9]);;
gap> r := RClass(s, f);
<Green's R-class: Transformation( [ 9, 10, 10, 3, 10, 9, 9, 9, 9, 9 ] )>
gap> Transformation([9, 8, 8, 1, 8, 9, 9, 9, 9, 9]) in last;
true
gap> Size(r);
546
gap> SchutzenbergerGroup(r);
Group([ (1,9,8), (1,8) ])
gap> ForAll(r, x -> x in r);
true
gap> f := Transformation([8, 8, 8, 8, 8, 8, 7, 7, 8, 8]);;
gap> r := RClass(s, f);
<Green's R-class: Transformation( [ 8, 8, 8, 8, 8, 8, 7, 7, 8, 8 ] )>
gap> Transformation([1, 1, 1, 1, 1, 1, 9, 9, 1, 1]) in last;
true
gap> Size(r);
86
gap> iter := IteratorOfRClasses(s);
<iterator>
gap> repeat r := NextIterator(iter); until Size(r) > 1000;
gap> r;
<Green's R-class: Transformation( [ 9, 1, 8, 2, 1, 8, 9, 9, 9, 8 ] )>
gap> Transformation([9, 1, 8, 2, 1, 8, 9, 9, 9, 8]) in last;
true
gap> Size(r);
1992
gap> SchutzenbergerGroup(r);
Group([ (2,8), (1,8), (1,2,8,9) ])
gap> enum := Enumerator(r);
<enumerator of <Green's R-class: Transformation( [ 9, 1, 8, 2, 1, 8, 9, 9, 9,
   8 ] )>>
gap> ForAll(enum, x -> x in r);
true
gap> ForAll(enum, x -> enum[Position(enum, x)] = x);
true
gap> ForAll([1 .. Length(enum)], x -> Position(enum, enum[x]) = x);
true
gap> NrHClasses(r);
83
gap> GreensHClasses(r);
[ <Green's H-class: Transformation( [ 9, 1, 8, 2, 1, 8, 9, 9, 9, 8 ] )>, 
  <Green's H-class: Transformation( [ 8, 2, 4, 3, 2, 4, 8, 8, 8, 4 ] )>, 
  <Green's H-class: Transformation( [ 1, 5, 4, 3, 5, 4, 1, 1, 1, 4 ] )>, 
  <Green's H-class: Transformation( [ 5, 4, 1, 2, 4, 1, 5, 5, 5, 1 ] )>, 
  <Green's H-class: Transformation( [ 10, 5, 9, 3, 5, 9, 10, 10, 10, 9 ] )>, 
  <Green's H-class: Transformation( [ 9, 1, 2, 5, 1, 2, 9, 9, 9, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 10, 7, 1, 10, 7, 4, 4, 4, 7 ] )>, 
  <Green's H-class: Transformation( [ 5, 1, 7, 2, 1, 7, 5, 5, 5, 7 ] )>, 
  <Green's H-class: Transformation( [ 10, 9, 4, 3, 9, 4, 10, 10, 10, 4 ] )>, 
  <Green's H-class: Transformation( [ 5, 9, 8, 2, 9, 8, 5, 5, 5, 8 ] )>, 
  <Green's H-class: Transformation( [ 1, 4, 8, 7, 4, 8, 1, 1, 1, 8 ] )>, 
  <Green's H-class: Transformation( [ 7, 5, 2, 3, 5, 2, 7, 7, 7, 2 ] )>, 
  <Green's H-class: Transformation( [ 10, 1, 4, 3, 1, 4, 10, 10, 10, 4 ] )>, 
  <Green's H-class: Transformation( [ 8, 1, 7, 3, 1, 7, 8, 8, 8, 7 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 7, 1, 2, 7, 3, 3, 3, 7 ] )>, 
  <Green's H-class: Transformation( [ 1, 4, 7, 2, 4, 7, 1, 1, 1, 7 ] )>, 
  <Green's H-class: Transformation( [ 5, 4, 7, 2, 4, 7, 5, 5, 5, 7 ] )>, 
  <Green's H-class: Transformation( [ 10, 5, 4, 3, 5, 4, 10, 10, 10, 4 ] )>, 
  <Green's H-class: Transformation( [ 1, 7, 3, 10, 7, 3, 1, 1, 1, 3 ] )>, 
  <Green's H-class: Transformation( [ 9, 4, 1, 2, 4, 1, 9, 9, 9, 1 ] )>, 
  <Green's H-class: Transformation( [ 5, 8, 4, 2, 8, 4, 5, 5, 5, 4 ] )>, 
  <Green's H-class: Transformation( [ 10, 6, 5, 3, 6, 5, 10, 10, 10, 5 ] )>, 
  <Green's H-class: Transformation( [ 2, 3, 10, 1, 3, 10, 2, 2, 2, 10 ] )>, 
  <Green's H-class: Transformation( [ 3, 1, 2, 9, 1, 2, 3, 3, 3, 2 ] )>, 
  <Green's H-class: Transformation( [ 10, 1, 9, 3, 1, 9, 10, 10, 10, 9 ] )>, 
  <Green's H-class: Transformation( [ 2, 9, 10, 1, 9, 10, 2, 2, 2, 10 ] )>, 
  <Green's H-class: Transformation( [ 2, 4, 1, 8, 4, 1, 2, 2, 2, 1 ] )>, 
  <Green's H-class: Transformation( [ 5, 3, 4, 2, 3, 4, 5, 5, 5, 4 ] )>, 
  <Green's H-class: Transformation( [ 10, 1, 5, 3, 1, 5, 10, 10, 10, 5 ] )>, 
  <Green's H-class: Transformation( [ 3, 5, 9, 6, 5, 9, 3, 3, 3, 9 ] )>, 
  <Green's H-class: Transformation( [ 3, 1, 4, 9, 1, 4, 3, 3, 3, 4 ] )>, 
  <Green's H-class: Transformation( [ 1, 5, 10, 9, 5, 10, 1, 1, 1, 10 ] )>, 
  <Green's H-class: Transformation( [ 3, 10, 7, 4, 10, 7, 3, 3, 3, 7 ] )>, 
  <Green's H-class: Transformation( [ 3, 10, 8, 7, 10, 8, 3, 3, 3, 8 ] )>, 
  <Green's H-class: Transformation( [ 1, 2, 6, 4, 2, 6, 1, 1, 1, 6 ] )>, 
  <Green's H-class: Transformation( [ 9, 1, 5, 8, 1, 5, 9, 9, 9, 5 ] )>, 
  <Green's H-class: Transformation( [ 4, 1, 8, 10, 1, 8, 4, 4, 4, 8 ] )>, 
  <Green's H-class: Transformation( [ 5, 3, 1, 2, 3, 1, 5, 5, 5, 1 ] )>, 
  <Green's H-class: Transformation( [ 5, 9, 6, 2, 9, 6, 5, 5, 5, 6 ] )>, 
  <Green's H-class: Transformation( [ 1, 4, 9, 7, 4, 9, 1, 1, 1, 9 ] )>, 
  <Green's H-class: Transformation( [ 8, 4, 7, 10, 4, 7, 8, 8, 8, 7 ] )>, 
  <Green's H-class: Transformation( [ 3, 5, 7, 1, 5, 7, 3, 3, 3, 7 ] )>, 
  <Green's H-class: Transformation( [ 4, 10, 9, 1, 10, 9, 4, 4, 4, 9 ] )>, 
  <Green's H-class: Transformation( [ 5, 1, 8, 2, 1, 8, 5, 5, 5, 8 ] )>, 
  <Green's H-class: Transformation( [ 10, 6, 9, 3, 6, 9, 10, 10, 10, 9 ] )>, 
  <Green's H-class: Transformation( [ 1, 10, 8, 7, 10, 8, 1, 1, 1, 8 ] )>, 
  <Green's H-class: Transformation( [ 9, 2, 6, 4, 2, 6, 9, 9, 9, 6 ] )>, 
  <Green's H-class: Transformation( [ 9, 10, 2, 5, 10, 2, 9, 9, 9, 2 ] )>, 
  <Green's H-class: Transformation( [ 10, 3, 1, 8, 3, 1, 10, 10, 10, 1 ] )>, 
  <Green's H-class: Transformation( [ 2, 1, 9, 6, 1, 9, 2, 2, 2, 9 ] )>, 
  <Green's H-class: Transformation( [ 7, 10, 4, 9, 10, 4, 7, 7, 7, 4 ] )>, 
  <Green's H-class: Transformation( [ 7, 1, 5, 8, 1, 5, 7, 7, 7, 5 ] )>, 
  <Green's H-class: Transformation( [ 7, 2, 4, 3, 2, 4, 7, 7, 7, 4 ] )>, 
  <Green's H-class: Transformation( [ 1, 4, 5, 7, 4, 5, 1, 1, 1, 5 ] )>, 
  <Green's H-class: Transformation( [ 9, 5, 10, 4, 5, 10, 9, 9, 9, 10 ] )>, 
  <Green's H-class: Transformation( [ 5, 1, 8, 4, 1, 8, 5, 5, 5, 8 ] )>, 
  <Green's H-class: Transformation( [ 9, 10, 6, 5, 10, 6, 9, 9, 9, 6 ] )>, 
  <Green's H-class: Transformation( [ 4, 10, 9, 6, 10, 9, 4, 4, 4, 9 ] )>, 
  <Green's H-class: Transformation( [ 10, 2, 5, 3, 2, 5, 10, 10, 10, 5 ] )>, 
  <Green's H-class: Transformation( [ 5, 10, 4, 2, 10, 4, 5, 5, 5, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 5, 8, 7, 5, 8, 2, 2, 2, 8 ] )>, 
  <Green's H-class: Transformation( [ 3, 10, 6, 4, 10, 6, 3, 3, 3, 6 ] )>, 
  <Green's H-class: Transformation( [ 3, 10, 9, 7, 10, 9, 3, 3, 3, 9 ] )>, 
  <Green's H-class: Transformation( [ 1, 2, 10, 4, 2, 10, 1, 1, 1, 10 ] )>, 
  <Green's H-class: Transformation( [ 3, 5, 2, 9, 5, 2, 3, 3, 3, 2 ] )>, 
  <Green's H-class: Transformation( [ 3, 1, 7, 4, 1, 7, 3, 3, 3, 7 ] )>, 
  <Green's H-class: Transformation( [ 9, 1, 4, 5, 1, 4, 9, 9, 9, 4 ] )>, 
  <Green's H-class: Transformation( [ 3, 10, 8, 4, 10, 8, 3, 3, 3, 8 ] )>, 
  <Green's H-class: Transformation( [ 2, 1, 5, 6, 1, 5, 2, 2, 2, 5 ] )>, 
  <Green's H-class: Transformation( [ 7, 10, 1, 9, 10, 1, 7, 7, 7, 1 ] )>, 
  <Green's H-class: Transformation( [ 7, 1, 2, 8, 1, 2, 7, 7, 7, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 3, 9, 6, 3, 9, 4, 4, 4, 9 ] )>, 
  <Green's H-class: Transformation( [ 7, 3, 4, 9, 3, 4, 7, 7, 7, 4 ] )>, 
  <Green's H-class: Transformation( [ 1, 5, 10, 4, 5, 10, 1, 1, 1, 10 ] )>, 
  <Green's H-class: Transformation( [ 3, 4, 8, 7, 4, 8, 3, 3, 3, 8 ] )>, 
  <Green's H-class: Transformation( [ 1, 5, 6, 4, 5, 6, 1, 1, 1, 6 ] )>, 
  <Green's H-class: Transformation( [ 9, 2, 10, 4, 2, 10, 9, 9, 9, 10 ] )>, 
  <Green's H-class: Transformation( [ 3, 10, 2, 9, 10, 2, 3, 3, 3, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 5, 10, 1, 5, 10, 2, 2, 2, 10 ] )>, 
  <Green's H-class: Transformation( [ 9, 5, 4, 3, 5, 4, 9, 9, 9, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 1, 9, 6, 1, 9, 4, 4, 4, 9 ] )>, 
  <Green's H-class: Transformation( [ 9, 5, 6, 4, 5, 6, 9, 9, 9, 6 ] )>, 
  <Green's H-class: Transformation( [ 1, 5, 3, 6, 5, 3, 1, 1, 1, 3 ] )> ]
gap> List(last, x -> Representative(x) in s);
[ true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true ]
gap> ForAll(last2, x -> Representative(x) in r);
true
gap> Semigroup(gens);;
gap> r := GreensRClassOfElement(s, f);
<Green's R-class: Transformation( [ 8, 8, 8, 8, 8, 8, 7, 7, 8, 8 ] )>
gap> Transformation([1, 1, 1, 1, 1, 1, 9, 9, 1, 1]) in last;
true
gap> f := Transformation([9, 9, 5, 9, 5, 9, 5, 5, 5, 5]);;
gap> r := GreensRClassOfElement(s, f);
<Green's R-class: Transformation( [ 9, 9, 5, 9, 5, 9, 5, 5, 5, 5 ] )>
gap> Transformation([9, 9, 1, 9, 1, 9, 1, 1, 1, 1]) in last;
true
gap> Size(r);
86
gap> NrHClasses(r);
43
gap> s := Semigroup(gens);;
gap> r := GreensRClassOfElement(s, f);
<Green's R-class: Transformation( [ 9, 9, 5, 9, 5, 9, 5, 5, 5, 5 ] )>
gap> Transformation([9, 9, 1, 9, 1, 9, 1, 1, 1, 1]) in last;
true
gap> GreensHClasses(r);
[ <Green's H-class: Transformation( [ 9, 9, 1, 9, 1, 9, 1, 1, 1, 1 ] )>, 
  <Green's H-class: Transformation( [ 8, 8, 2, 8, 2, 8, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 4, 3, 4, 3, 4, 3, 3, 3, 3 ] )>, 
  <Green's H-class: Transformation( [ 1, 1, 5, 1, 5, 1, 5, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 9, 9, 10, 9, 10, 9, 10, 10, 10, 10 ] )>,
  <Green's H-class: Transformation( [ 10, 10, 4, 10, 4, 10, 4, 4, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 8, 8, 5, 8, 5, 8, 5, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 8, 8, 1, 8, 1, 8, 1, 1, 1, 1 ] )>, 
  <Green's H-class: Transformation( [ 8, 8, 10, 8, 10, 8, 10, 10, 10, 10 ] )>,
  <Green's H-class: Transformation( [ 1, 1, 3, 1, 3, 1, 3, 3, 3, 3 ] )>, 
  <Green's H-class: Transformation( [ 3, 3, 10, 3, 10, 3, 10, 10, 10, 10 ] )>,
  <Green's H-class: Transformation( [ 2, 2, 5, 2, 5, 2, 5, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 7, 7, 1, 7, 1, 7, 1, 1, 1, 1 ] )>, 
  <Green's H-class: Transformation( [ 9, 9, 4, 9, 4, 9, 4, 4, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 9, 9, 8, 9, 8, 9, 8, 8, 8, 8 ] )>, 
  <Green's H-class: Transformation( [ 8, 8, 4, 8, 4, 8, 4, 4, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 7, 7, 8, 7, 8, 7, 8, 8, 8, 8 ] )>, 
  <Green's H-class: Transformation( [ 7, 7, 3, 7, 3, 7, 3, 3, 3, 3 ] )>, 
  <Green's H-class: Transformation( [ 9, 9, 2, 9, 2, 9, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 7, 7, 4, 7, 4, 7, 4, 4, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 4, 5, 4, 5, 4, 5, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 5, 5, 10, 5, 10, 5, 10, 10, 10, 10 ] )>,
  <Green's H-class: Transformation( [ 2, 2, 10, 2, 10, 2, 10, 10, 10, 10 ] )>,
  <Green's H-class: Transformation( [ 7, 7, 10, 7, 10, 7, 10, 10, 10, 10 ] )>,
  <Green's H-class: Transformation( [ 9, 9, 5, 9, 5, 9, 5, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 1, 1, 4, 1, 4, 1, 4, 4, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 4, 2, 4, 2, 4, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 5, 5, 3, 5, 3, 5, 3, 3, 3, 3 ] )>, 
  <Green's H-class: Transformation( [ 1, 1, 2, 1, 2, 1, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 9, 9, 3, 9, 3, 9, 3, 3, 3, 3 ] )>, 
  <Green's H-class: Transformation( [ 1, 1, 10, 1, 10, 1, 10, 10, 10, 10 ] )>,
  <Green's H-class: Transformation( [ 2, 2, 3, 2, 3, 2, 3, 3, 3, 3 ] )>, 
  <Green's H-class: Transformation( [ 7, 7, 5, 7, 5, 7, 5, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 8, 8, 3, 8, 3, 8, 3, 3, 3, 3 ] )>, 
  <Green's H-class: Transformation( [ 1, 1, 6, 1, 6, 1, 6, 6, 6, 6 ] )>, 
  <Green's H-class: Transformation( [ 4, 4, 6, 4, 6, 4, 6, 6, 6, 6 ] )>, 
  <Green's H-class: Transformation( [ 9, 9, 7, 9, 7, 9, 7, 7, 7, 7 ] )>, 
  <Green's H-class: Transformation( [ 6, 6, 5, 6, 5, 6, 5, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 6, 6, 10, 6, 10, 6, 10, 10, 10, 10 ] )>,
  <Green's H-class: Transformation( [ 7, 7, 2, 7, 2, 7, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 6, 2, 6, 2, 6, 6, 6, 6 ] )>, 
  <Green's H-class: Transformation( [ 9, 9, 6, 9, 6, 9, 6, 6, 6, 6 ] )>, 
  <Green's H-class: Transformation( [ 3, 3, 6, 3, 6, 3, 6, 6, 6, 6 ] )> ]
gap> Length(last);
43
gap> ForAll(last2, x -> Representative(x) in r);
true
gap> ForAll(last3, x -> Representative(x) in s);
true
gap> h := Random(GreensHClasses(r));;
gap> f := Representative(h);;
gap> hh := HClass(r, f);;
gap> hh = h;
true
gap> h = hh;
true
gap> Elements(h) = Elements(hh);
true
gap> f := Transformation([10, 1, 9, 10, 2, 1, 5, 3, 2, 3]);;
gap> r := GreensRClassOfElement(s, f);
<Green's R-class: Transformation( [ 10, 1, 9, 10, 2, 1, 5, 3, 2, 3 ] )>
gap> Transformation([10, 1, 9, 10, 2, 1, 5, 3, 2, 3]) in last;
true
gap> Size(r);
1
gap> f := Transformation([10, 10, 3, 10, 10, 10, 10, 10, 6, 10]);;
gap> r := GreensRClassOfElement(s, f);
<Green's R-class: Transformation( [ 10, 10, 3, 10, 10, 10, 10, 10, 6, 10 ] )>
gap> Transformation([8, 8, 1, 8, 8, 8, 8, 8, 9, 8]) in last;
true
gap> Size(r);
546
gap> f := Transformation([6, 6, 4, 6, 6, 6, 6, 6, 3, 6]);;
gap> f in r;
true
gap> h := HClass(r, f);
<Green's H-class: Transformation( [ 6, 6, 4, 6, 6, 6, 6, 6, 3, 6 ] )>
gap> Transformation([6, 6, 4, 6, 6, 6, 6, 6, 3, 6]) in last;
true
gap> f in h;
true
gap> ForAll(h, x -> x in r);
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

# MiscTest29
gap> gens :=
> [PartialPermNC([1, 2, 3, 5, 9, 10], [5, 10, 7, 8, 9, 1]),
>  PartialPermNC([1, 2, 3, 4, 5, 6, 9], [9, 3, 1, 4, 2, 5, 6]),
>  PartialPermNC([1, 2, 3, 4, 5, 7, 9], [7, 6, 2, 8, 4, 5, 3]),
>  PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
> [8, 7, 4, 3, 10, 9, 5, 6, 1, 2])];;
gap> s := Semigroup(gens);;
gap> Size(s);
1422787
gap> f := PartialPermNC([1, 4, 7, 9, 10], [5, 10, 9, 8, 7]);;
gap> r := GreensRClassOfElementNC(s, f);
<Green's R-class: [1,5][4,10,7,9,8]>
gap> Size(r);
4
gap> f in r;
true
gap> f := PartialPermNC([1, 7, 8, 9], [10, 9, 6, 5]);;
gap> r := GreensRClassOfElementNC(s, f);
<Green's R-class: [1,10][7,9,5][8,6]>
gap> Size(r);
4
gap> iter := IteratorOfRClasses(s);
<iterator>
gap> repeat r := NextIterator(iter); until Size(r) > 1000;
gap> r;
<Green's R-class: [1,4][9,3,5][10,7]>
gap> Size(r);
3792
gap> r := RClassNC(s, Representative(r));
<Green's R-class: [1,4][9,3,5][10,7]>
gap> h := HClassNC(r, Random(r));;
gap> Size(h);
24
gap> ForAll(h, x -> x in r);
true
gap> IsRegularGreensClass(r);
true
gap> IsRegularSemigroup(s);
false
gap> NrIdempotents(r);
1
gap> Idempotents(r);
[ <identity partial perm on [ 1, 3, 9, 10 ]> ]
gap> ForAll(last, x -> x in r);
true

# MiscTest30
gap> gens := [Transformation([1, 3, 7, 9, 1, 12, 13, 1, 15, 9, 1, 18, 1, 1,
>  13, 1, 1, 21, 1, 1, 1, 1, 1, 25, 26, 1]),
>  Transformation([1, 5, 1, 5, 11, 1, 1, 14, 1, 16, 17, 1, 1, 19, 1, 11, 1,
>       1, 1, 23, 1, 16, 19, 1, 1, 1]),
> Transformation([1, 4, 8, 1, 10, 1, 8, 1, 1, 1, 10, 1, 8, 10, 1, 1, 20, 1,
>       22, 1, 8, 1, 1, 1, 1, 1]),
> Transformation([1, 6, 6, 1, 1, 1, 6, 1, 1, 1, 1, 1, 6, 1, 6, 1, 1, 6, 1,
>       1, 24, 1, 1, 1, 1, 6])];;
gap> s := Semigroup(gens);;
gap> First(DClasses(s), IsRegularDClass);
<Green's D-class: Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] )>
gap> NrDClasses(s);
31
gap> PositionsProperty(DClasses(s), IsRegularDClass);
[ 6, 7 ]
gap> d := DClasses(s)[7];
<Green's D-class: Transformation( [ 1, 6, 1, 1, 6, 1, 1, 1, 1, 1, 6, 1, 1, 6,
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] )>
gap> r := RClassNC(s, Representative(d));
<Green's R-class: Transformation( [ 1, 6, 1, 1, 6, 1, 1, 1, 1, 1, 6, 1, 1, 6,
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] )>
gap> Size(r);
20
gap> ForAll(Idempotents(r), x -> x in s);
true
gap> ForAll(Idempotents(r), x -> x in r);
true
gap> ForAll(Idempotents(r), x -> x in d);
true
gap> ForAll(r, x -> x in d);
true
gap> Number(GreensRClasses(s), IsRegularGreensClass);
21
gap> NrRegularDClasses(s);
2

# MiscTest31
gap> gens := [Transformation([1, 2, 3, 5, 4, 6, 7, 8]),
>   Transformation([4, 4, 3, 1, 5, 6, 3, 8]),
>   Transformation([3, 6, 1, 7, 3, 4, 8, 3]),
>   Transformation([1, 2, 3, 4, 5, 3, 7, 8]),
>   Transformation([1, 2, 3, 4, 1, 6, 7, 8]),
>   Transformation([8, 8, 3, 4, 5, 7, 6, 1])];;
gap> s := Monoid(gens);
<transformation monoid of degree 8 with 6 generators>
gap> f := Transformation([4, 4, 3, 8, 5, 3, 3, 1]);;
gap> Size(s);
998
gap> r := RClass(s, f);
<Green's R-class: Transformation( [ 4, 4, 3, 8, 5, 3, 3, 1 ] )>
gap> Transformation([4, 4, 3, 8, 5, 3, 3, 1]) in last;
true
gap> IsRegularGreensClass(r);
true
gap> Idempotents(r);
[ Transformation( [ 1, 1, 3, 4, 5, 3, 3 ] ) ]
gap> IsRegularSemigroup(s);
false
gap> ForAll(r, x -> x in s);
true
gap> iter := Iterator(r);
<iterator>
gap> for i in iter do od;
gap> Size(r);
24
gap> IsDoneIterator(iter);
true
gap> iter := Iterator(r);
<iterator>
gap> for i in [1 .. 23] do NextIterator(iter); od;
gap> IsDoneIterator(iter);
false
gap> NextIterator(iter);
Transformation( [ 5, 5, 3, 1, 4, 3, 3 ] )
gap> IsDoneIterator(iter);
true
gap> Transformation([4, 4, 3, 8, 1, 3, 3, 5]) in r;
true
gap> r;
<Green's R-class: Transformation( [ 4, 4, 3, 8, 5, 3, 3, 1 ] )>
gap> Transformation([4, 4, 3, 8, 5, 3, 3, 1]) in last;
true
gap> NrIdempotents(r);
1

# MiscTest32
gap> gens := [PartialPermNC([1, 2, 3, 4, 7], [8, 3, 5, 7, 4]),
>  PartialPermNC([1, 2, 5, 6, 7], [4, 1, 6, 2, 8]),
>  PartialPermNC([1, 2, 3, 4, 5, 6], [3, 7, 1, 5, 2, 6]),
>  PartialPermNC([1, 2, 3, 4, 5, 6], [7, 2, 5, 6, 3, 8]),
>  PartialPermNC([1, 2, 3, 5, 6, 7], [4, 5, 6, 1, 2, 7]),
>  PartialPermNC([1, 2, 3, 5, 6, 7], [5, 1, 7, 2, 8, 4])];;
gap> s := Semigroup(gens);;
gap> Size(s);
9954
gap> f := PartialPerm([2, 3, 6], [1, 4, 8]);;
gap> r := RClass(s, f);
<Green's R-class: [2,1][3,4][6,8]>
gap> NrIdempotents(r);
0
gap> Sum(List(RClasses(s), NrIdempotents));
53
gap> NrIdempotents(s);
53
gap> gens := [Transformation([1, 2, 4, 3, 6, 5]),
> Transformation([1, 2, 3, 4, 5, 6]),
>    Transformation([6, 4, 3, 2, 5, 3]),
> Transformation([5, 3, 4, 2, 2, 1]),
>    Transformation([2, 4, 6, 4, 5, 3]),
> Transformation([4, 2, 4, 3, 6, 5]),
>    Transformation([2, 4, 4, 3, 6, 5]),
> Transformation([5, 6, 4, 4, 3, 2]),
>    Transformation([2, 2, 3, 4, 5, 6]),
> Transformation([3, 4, 2, 2, 2, 1]),
>    Transformation([1, 2, 4, 2, 3, 3]),
> Transformation([1, 2, 3, 4, 3, 2]),
>    Transformation([6, 4, 2, 3, 2, 3]),
> Transformation([6, 4, 2, 2, 1, 1]),
>    Transformation([6, 4, 2, 3, 4, 4]),
> Transformation([5, 3, 3, 2, 4, 2])];;
gap> s := Semigroup(gens);;
gap> Size(s);
1888
gap> f := Transformation([2, 4, 6, 6, 5, 6]);;
gap> r := RClass(s, f);
<Green's R-class: Transformation( [ 2, 4, 6, 6, 5, 6 ] )>
gap> Transformation([2, 4, 6, 6, 5, 6]) in last;
true
gap> h := HClassNC(s, f);
<Green's H-class: Transformation( [ 2, 4, 6, 6, 5, 6 ] )>
gap> Transformation([2, 4, 6, 6, 5, 6]) in last;
true
gap> hh := HClass(r, f);
<Green's H-class: Transformation( [ 2, 4, 6, 6, 5, 6 ] )>
gap> Transformation([2, 4, 6, 6, 5, 6]) in last;
true
gap> hh = h;
true
gap> ForAll(h, x -> x in r);
true
gap> ForAll(hh, x -> x in r);
true
gap> RClassOfHClass(h) = r;
true
gap> RClassOfHClass(hh) = r;
true
gap> r = RClassOfHClass(hh);
true
gap> Size(r);
2
gap> HClassReps(r);
[ Transformation( [ 2, 4, 6, 6, 5, 6 ] ), 
  Transformation( [ 2, 3, 5, 5, 6, 5 ] ) ]
gap> ForAll(last, x -> x in r);
true
gap> ForAll(last2, x -> x in s);
true

# MiscTest33
gap> gens :=
> [PartialPermNC([1, 2, 3], [2, 3, 4]),
>  PartialPermNC([1, 2, 3], [3, 6, 1]),
>  PartialPermNC([1, 2, 3], [6, 2, 1]),
>  PartialPermNC([1, 2, 4], [4, 2, 6]),
>  PartialPermNC([1, 3, 5], [2, 6, 3]),
>  PartialPermNC([1, 4, 5], [1, 6, 3]),
>  PartialPermNC([1, 2, 3, 5], [2, 3, 5, 1]),
>  PartialPermNC([1, 2, 3, 5], [3, 2, 4, 6]),
>  PartialPermNC([1, 2, 4, 6], [4, 3, 1, 6]),
>  PartialPermNC([1, 3, 5, 6], [1, 4, 6, 2]),
>  PartialPermNC([1, 2, 3, 4, 5], [5, 4, 6, 2, 1]),
>  PartialPermNC([1, 2, 3, 4, 5], [6, 2, 3, 5, 1]),
>  PartialPermNC([1, 2, 3, 4, 5], [6, 3, 5, 1, 2]),
>  PartialPermNC([1, 2, 3, 4, 6], [4, 1, 5, 2, 3]),
>  PartialPermNC([1, 2, 3, 4, 6], [5, 1, 6, 3, 2]),
>  PartialPermNC([1, 2, 3, 5, 6], [5, 4, 2, 6, 3])];;
gap> s := Semigroup(gens);;
gap> Size(s);
6741
gap> f := PartialPermNC([1, 3, 5, 6], [6, 2, 5, 1]);;
gap> r := RClassNC(s, f);
<Green's R-class: [3,2](1,6)(5)>
gap> HClassReps(r);
[ [3,2](1,6)(5) ]
gap> ForAll(last, x -> x in r);
true
gap> r := RClass(s, f);
<Green's R-class: [3,2](1,6)(5)>
gap> HClassReps(r);
[ [3,2](1,6)(5) ]
gap> h := HClass(s, last[1]);
<Green's H-class: [3,2](1,6)(5)>
gap> r := RClassOfHClass(h);
<Green's R-class: [3,2](1,6)(5)>
gap> HClassReps(r);
[ [3,2](1,6)(5) ]
gap> iter := IteratorOfRClasses(s);
<iterator>
gap> iter := IteratorOfRClasses(s);
<iterator>
gap> repeat r := NextIterator(iter); until Size(r) > 1;
gap> r;
<Green's R-class: [1,2,3,4]>
gap> Size(r);
114
gap> HClassReps(r);
[ [1,2,3,4], [1,2,4][3,6], [2,3,6](1), [1,5](2)(3), [2,5](1,3), [3,2,1,5], 
  <identity partial perm on [ 1, 2, 3 ]>, [1,3,2,6], [3,6](1)(2), 
  [1,4][2,3,6], [1,3,5][2,4], [2,3,4](1), [2,1,5][3,6], [1,6][2,5](3), 
  [1,2,6][3,5], [1,4][3,2,5], [3,4](1,2), [2,5][3,4](1), [2,6][3,1,4] ]
gap> Size(DClass(r));
2166
gap> d := DClass(r);
<Green's D-class: [1,2,3,4]>
gap> ForAll(r, x -> x in d);
true
gap> Number(d, x -> x in r);
114
gap> Size(r);
114
gap> ForAll(HClassReps(r), x -> x in d);
true
gap> ForAll(HClassReps(r), x -> x in HClassReps(d));
true

# MiscTest34
gap> gens := [Transformation([6, 4, 3, 2, 5, 1]),
>   Transformation([1, 2, 3, 4, 5, 6]),
>   Transformation([5, 3, 3, 2, 4, 1]),
>   Transformation([1, 3, 3, 4, 5, 2]),
>   Transformation([4, 5, 2, 3, 3, 1]),
>   Transformation([6, 4, 3, 5, 2, 3]),
>   Transformation([5, 2, 3, 4, 3, 6]),
>   Transformation([1, 3, 2, 5, 4, 5]),
>   Transformation([4, 3, 2, 2, 1, 5]),
>   Transformation([1, 3, 3, 5, 2, 4]),
>   Transformation([6, 3, 3, 2, 1, 5]),
>   Transformation([6, 3, 4, 5, 2, 2]),
>   Transformation([6, 4, 3, 2, 2, 5]),
>   Transformation([1, 3, 2, 3, 5, 4]),
>   Transformation([1, 2, 3, 4, 5, 2]),
>   Transformation([2, 4, 3, 4, 6, 5]),
>   Transformation([2, 4, 3, 3, 6, 1]),
>   Transformation([6, 4, 3, 2, 3, 1]),
>   Transformation([6, 4, 3, 2, 2, 1])];;
gap> s := Semigroup(gens);
<transformation monoid of degree 6 with 18 generators>
gap> Size(s);
7008
gap> NrRClasses(s);
310
gap> IsRegularSemigroup(s);
false
gap> f := Transformation([3, 2, 3, 4, 3, 5]);;
gap> r := RClassNC(s, f);
<Green's R-class: Transformation( [ 3, 2, 3, 4, 3, 5 ] )>
gap> Transformation([3, 2, 3, 4, 3, 5]) in last;
true
gap> d := DClassOfRClass(r);
<Green's D-class: Transformation( [ 3, 2, 3, 4, 3, 5 ] )>
gap> Transformation([3, 2, 3, 4, 3, 5]) in last;
true
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
gap> Number(HClassReps(d), x -> x in r);
6
gap> NrHClasses(r);
6

# MiscTest35
gap> gens := [PartialPermNC([1, 2, 4], [2, 5, 3]),
>  PartialPermNC([1, 2, 4], [5, 6, 1]),
>  PartialPermNC([1, 2, 5], [5, 3, 2]),
>  PartialPermNC([1, 2, 3, 4], [5, 1, 2, 4]),
>  PartialPermNC([1, 2, 3, 4], [5, 1, 2, 6]),
>  PartialPermNC([1, 2, 3, 4], [5, 6, 4, 1]),
>  PartialPermNC([1, 2, 3, 5], [1, 5, 2, 6]),
>  PartialPermNC([1, 2, 3, 5], [2, 3, 4, 1]),
>  PartialPermNC([1, 2, 3, 5], [2, 5, 4, 1]),
>  PartialPermNC([1, 2, 3, 5], [5, 1, 2, 3]),
>  PartialPermNC([1, 2, 3, 6], [1, 4, 6, 5]),
>  PartialPermNC([1, 2, 5, 6], [6, 4, 2, 5]),
>  PartialPermNC([1, 3, 4, 6], [2, 3, 1, 6]),
>  PartialPermNC([1, 2, 3, 4, 5], [3, 6, 5, 2, 4]),
>  PartialPermNC([1, 2, 3, 4, 5], [6, 5, 3, 2, 1]),
>  PartialPermNC([1, 2, 3, 4, 6], [1, 3, 4, 6, 2]),
>  PartialPermNC([1, 2, 3, 5, 6], [1, 3, 6, 4, 5]),
>  PartialPermNC([1, 2, 4, 5, 6], [5, 4, 2, 1, 6]),
>  PartialPermNC([1, 2, 3, 4, 5, 6], [2, 5, 6, 4, 3, 1])];;
gap> s := Semigroup(gens);;
gap> Size(s);
12612
gap> f := PartialPermNC([1, 4, 6], [2, 3, 6]);;
gap> r := RClass(s, f);
<Green's R-class: [1,2][4,3](6)>
gap> Size(r);
120
gap> NrHClasses(r);
20
gap> Number(HClassReps(s), x -> x in r);
20

# MiscTest36: H-class tests
gap> gens := [Transformation([8, 7, 6, 5, 4, 3, 2, 1]),
>   Transformation([1, 2, 3, 4, 5, 6, 7, 8]),
>   Transformation([7, 6, 5, 4, 3, 2, 1, 2]),
>   Transformation([3, 2, 1, 2, 3, 4, 5, 6]),
>   Transformation([2, 3, 4, 5, 4, 5, 6, 7]),
>   Transformation([1, 2, 3, 4, 5, 4, 5, 6]),
>   Transformation([5, 6, 5, 4, 5, 4, 3, 2]),
>   Transformation([5, 6, 7, 8, 7, 6, 5, 4])];;
gap> s := Semigroup(gens);;
gap> f := Transformation([5, 6, 5, 4, 5, 4, 5, 4]);;
gap> h := HClass(s, f);
<Green's H-class: Transformation( [ 5, 6, 5, 4, 5, 4, 5, 4 ] )>
gap> Transformation([5, 6, 5, 4, 5, 4, 5, 4]) in last;
true
gap> ForAll(h, x -> x in h);
true
gap> h := HClassNC(s, f);
<Green's H-class: Transformation( [ 5, 6, 5, 4, 5, 4, 5, 4 ] )>
gap> Transformation([5, 6, 5, 4, 5, 4, 5, 4]) in last;
true
gap> Enumerator(h);
<enumerator of <Green's H-class: Transformation( [ 5, 6, 5, 4, 5, 4, 5, 4 ] )>
 >
gap> h := HClassNC(s, f);
<Green's H-class: Transformation( [ 5, 6, 5, 4, 5, 4, 5, 4 ] )>
gap> Transformation([5, 6, 5, 4, 5, 4, 5, 4]) in last;
true
gap> SchutzenbergerGroup(h);
Group([ (4,6) ])

# MiscTest37
gap> s := FullTransformationSemigroup(7);
<full transformation monoid of degree 7>
gap> Factorial(7);
5040
gap> f := One(s);
IdentityTransformation
gap> h := HClassNC(s, f);
<Green's H-class: IdentityTransformation>
gap> enum := Enumerator(h);
<enumerator of <Green's H-class: IdentityTransformation>>
gap> ForAll(enum, x -> x in h);
true
gap> ForAll(enum, x -> x in s);
true
gap> ForAll(enum, x -> enum[Position(enum, x)] = x);
true
gap> ForAll([1 .. Length(enum)], x -> Position(enum, enum[x]) = x);
true
gap> Idempotents(h);
[ IdentityTransformation ]
gap> f := Transformation([3, 2, 4, 5, 6, 1, 1]);;
gap> h := HClassNC(s, f);
<Green's H-class: Transformation( [ 3, 2, 4, 5, 6, 1, 1 ] )>
gap> Transformation([3, 2, 4, 5, 6, 1, 1]) in last;
true
gap> Idempotents(h);
[ Transformation( [ 1, 2, 3, 4, 5, 6, 6 ] ) ]
gap> IsGroupHClass(h);
true
gap> h := HClass(s, Transformation([5, 1, 3, 3, 5, 5, 3]));;
gap> IsGroupHClass(h);
false
gap> IsRegularGreensClass(h);
false

# MiscTest38
gap> gens :=
> [PartialPermNC([1, 2, 3], [1, 5, 2]),
>  PartialPermNC([1, 2, 4], [1, 3, 6]),
>  PartialPermNC([1, 2, 4], [3, 1, 6]),
>  PartialPermNC([1, 2, 6], [6, 4, 1]),
>  PartialPermNC([1, 3, 5], [5, 2, 3]),
>  PartialPermNC([1, 2, 3, 4], [5, 3, 2, 4]),
>  PartialPermNC([1, 2, 3, 4], [6, 1, 5, 3]),
>  PartialPermNC([1, 2, 3, 5], [1, 4, 6, 3]),
>  PartialPermNC([1, 2, 3, 5], [2, 3, 4, 1]),
>  PartialPermNC([1, 2, 3, 5], [6, 5, 1, 2]),
>  PartialPermNC([1, 2, 3, 6], [3, 5, 4, 6]),
>  PartialPermNC([1, 2, 4, 5], [4, 2, 3, 6]),
>  PartialPermNC([1, 2, 4, 6], [6, 4, 3, 5]),
>  PartialPermNC([1, 2, 4, 6], [6, 4, 5, 2]),
>  PartialPermNC([1, 3, 4, 5], [6, 1, 4, 3]),
>  PartialPermNC([1, 2, 3, 4, 5], [3, 4, 1, 2, 6]),
>  PartialPermNC([1, 2, 3, 4, 6], [1, 2, 5, 3, 4]),
>  PartialPermNC([1, 2, 3, 4, 6], [3, 6, 4, 5, 1]),
>  PartialPermNC([1, 2, 3, 5, 6], [4, 3, 5, 1, 6]),
>  PartialPermNC([1, 2, 4, 5, 6], [2, 3, 1, 5, 6])];;
gap> s := Semigroup(gens);;
gap> Size(s);
7960
gap> f := PartialPermNC([1, 2, 5, 6], [5, 3, 6, 4]);;
gap> h := HClass(s, f);
<Green's H-class: [1,5,6,4][2,3]>
gap> d := DClass(s, f);
<Green's D-class: [1,5,6,4][2,3]>
gap> h := HClass(s, f);
<Green's H-class: [1,5,6,4][2,3]>
gap> IsGroupHClass(h);
false
gap> Size(h);
1
gap> h;
<Green's H-class: [1,5,6,4][2,3]>
gap> Size(h);
1
gap> enum := Enumerator(h);
<enumerator of <Green's H-class: [1,5,6,4][2,3]>>
gap> ForAll([1 .. Length(enum)], x -> Position(enum, enum[x]) = x);
true
gap> ForAll(enum, x -> enum[Position(enum, x)] = x);
true
gap> d := DClass(s, Representative(h));
<Green's D-class: [1,5,6,4][2,3]>
gap> f := Representative(h);
[1,5,6,4][2,3]
gap> h := HClass(d, f);
<Green's H-class: [1,5,6,4][2,3]>
gap> h = HClass(s, f);
true
gap> Idempotents(h);
[  ]
gap> repeat h := NextIterator(iter); until Size(h) > 1;
gap> h;
<Green's R-class: [1,3](2)(4)>
gap> Size(h);
114
gap> f := Representative(h);
[1,3](2)(4)
gap> r := RClassNC(d, f);
<Green's R-class: [1,3](2)(4)>
gap> h := HClass(r, f);
Error, the 2nd argument (a mult. elt.) does not belong to the 1st argument (a \
Green's class)
gap> h = HClass(s, f);
false
gap> Elements(h) = Elements(HClass(s, f));
false
gap> l := LClass(s, f);
<Green's L-class: [1,3](2)(4)>
gap> h := HClass(l, f);
<Green's H-class: [1,3](2)(4)>
gap> Elements(h) = Elements(HClass(s, f));
true
gap> h := HClass(l, f);
<Green's H-class: [1,3](2)(4)>

# MiscTest41
gap> gens := [Transformation([1, 2, 5, 4, 3, 8, 7, 6]),
>   Transformation([1, 6, 3, 4, 7, 2, 5, 8]),
>   Transformation([2, 1, 6, 7, 8, 3, 4, 5]),
>   Transformation([3, 2, 3, 6, 1, 6, 1, 2]),
>   Transformation([5, 2, 3, 6, 3, 4, 7, 4])];;
gap> s := Semigroup(gens);;
gap> Size(s);
5304

# MiscTest44
gap> gens := [Transformation([4, 6, 5, 2, 1, 3]),
>   Transformation([6, 3, 2, 5, 4, 1]),
>   Transformation([1, 2, 4, 3, 5, 6]),
>   Transformation([3, 5, 6, 1, 2, 3]),
>   Transformation([5, 3, 6, 6, 6, 2]),
>   Transformation([2, 3, 2, 6, 4, 6]),
>   Transformation([2, 1, 2, 2, 2, 4]),
>   Transformation([4, 4, 1, 2, 1, 2])];;
gap> s := Semigroup(gens);;
gap> f := Transformation([4, 4, 1, 2, 1, 2]);;
gap> h := HClassNC(s, f);
<Green's H-class: Transformation( [ 4, 4, 1, 2, 1, 2 ] )>
gap> Transformation([4, 4, 1, 2, 1, 2]) in last;
true
gap> IsRegularGreensClass(h);
false
gap> IsGroupHClass(h);
false
gap> h := GroupHClass(DClass(h));
<Green's H-class: Transformation( [ 2, 2, 3, 6, 3, 6 ] )>
gap> Transformation([2, 2, 3, 6, 3, 6]) in last;
true
gap> Size(h);
6
gap> r := RClassOfHClass(h);
<Green's R-class: Transformation( [ 2, 2, 3, 6, 3, 6 ] )>
gap> Transformation([1, 1, 2, 4, 2, 4]) in last;
true
gap> ForAll(h, x -> x in r);
true
gap> Number(r, x -> x in h);
6
gap> l;
<Green's L-class: [1,3](2)(4)>
gap> RhoOrbStabChain(l);
true
gap> g := SchutzenbergerGroup(l);
Sym( [ 2 .. 4 ] )
gap> IsSymmetricGroup(g);
true
gap> IsNaturalSymmetricGroup(g);
true

# MiscTest45
gap> a1 := Transformation([2, 2, 3, 5, 5, 6, 7, 8, 14, 16, 16, 17, 18, 14,
>                          16, 16, 17, 18]);;
gap> a2 := Transformation([1, 3, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
>                          16, 17, 18]);;
gap> a3 := Transformation([1, 2, 4, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 15,
>                          16, 17, 18]);;
gap> a4 := Transformation([1, 2, 3, 4, 6, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
>                          17, 17, 18]);;
gap> a5 := Transformation([1, 2, 3, 4, 5, 7, 7, 8, 9, 10, 11, 12, 13, 14, 15,
>                          16, 18, 18]);;
gap> a6 := Transformation([1, 2, 3, 4, 5, 6, 8, 8, 9, 10, 11, 12, 13, 14, 15,
>                          16, 17, 2]);;
gap> P := Transformation([1, 2, 9, 10, 11, 12, 13, 1, 9, 10, 11, 12, 13, 14,
>                         15, 16, 17, 18]);;
gap> K18g := [a1, a2, a3, a4, a5, a6, P];;
gap> s := Semigroup(K18g);;
gap> f := Transformation([1, 2, 4, 4, 6, 6, 7, 8, 9, 10, 11, 12, 13, 15, 15,
>                         17, 17, 18]);;
gap> r := RClassNC(s, f);
<Green's R-class: Transformation( [ 1, 2, 4, 4, 6, 6, 7, 8, 9, 10, 11, 12, 13,
  15, 15, 17, 17 ] )>
gap> Size(r);
1
gap> SchutzenbergerGroup(r);
Group(())
gap> f := Transformation([1, 2, 10, 10, 11, 12, 13, 1, 9, 10, 11, 12, 13, 15,
> 15, 16, 17, 18]);;
gap> r := RClass(s, f);
<Green's R-class: Transformation( [ 1, 2, 10, 10, 11, 12, 13, 1, 9, 10, 11,
   12, 13, 15, 15 ] )>
gap> Size(r);
1
gap> SchutzenbergerGroup(r);
Group(())

# MiscTest46
gap> gens := [Transformation([2, 4, 1, 5, 4, 4, 7, 3, 8, 1]),
>   Transformation([9, 1, 2, 8, 1, 5, 9, 9, 9, 5]),
>   Transformation([9, 3, 1, 5, 10, 3, 4, 6, 10, 2]),
>   Transformation([10, 7, 3, 7, 1, 9, 8, 8, 4, 10])];;
gap> s := Semigroup(gens);;
gap> f := Transformation([9, 10, 10, 3, 10, 9, 9, 9, 9, 9]);;
gap> r := RClass(s, f);
<Green's R-class: Transformation( [ 9, 10, 10, 3, 10, 9, 9, 9, 9, 9 ] )>
gap> Transformation([9, 8, 8, 1, 8, 9, 9, 9, 9, 9]) in last;
true
gap> Size(r);
546
gap> SchutzenbergerGroup(r);
Group([ (1,9,8), (1,8) ])
gap> ForAll(r, x -> x in r);
true
gap> f := Transformation([8, 8, 8, 8, 8, 8, 7, 7, 8, 8]);;
gap> r := RClass(s, f);
<Green's R-class: Transformation( [ 8, 8, 8, 8, 8, 8, 7, 7, 8, 8 ] )>
gap> Transformation([1, 1, 1, 1, 1, 1, 9, 9, 1, 1]) in last;
true
gap> Size(r);
86
gap> iter := IteratorOfRClasses(s);
<iterator>
gap> repeat r := NextIterator(iter); until Size(r) > 1000;
gap> r;
<Green's R-class: Transformation( [ 9, 1, 8, 2, 1, 8, 9, 9, 9, 8 ] )>
gap> Transformation([9, 1, 8, 2, 1, 8, 9, 9, 9, 8]) in last;
true
gap> Size(r);
1992
gap> SchutzenbergerGroup(r);
Group([ (2,8), (1,8), (1,2,8,9) ])
gap> enum := Enumerator(r);
<enumerator of <Green's R-class: Transformation( [ 9, 1, 8, 2, 1, 8, 9, 9, 9,
   8 ] )>>
gap> ForAll(enum, x -> x in r);
true
gap> ForAll(enum, x -> enum[Position(enum, x)] = x);
true
gap> ForAll([1 .. Length(enum)], x -> Position(enum, enum[x]) = x);
true
gap> NrHClasses(r);
83
gap> GreensHClasses(r);
[ <Green's H-class: Transformation( [ 9, 1, 8, 2, 1, 8, 9, 9, 9, 8 ] )>, 
  <Green's H-class: Transformation( [ 8, 2, 4, 3, 2, 4, 8, 8, 8, 4 ] )>, 
  <Green's H-class: Transformation( [ 1, 5, 4, 3, 5, 4, 1, 1, 1, 4 ] )>, 
  <Green's H-class: Transformation( [ 5, 4, 1, 2, 4, 1, 5, 5, 5, 1 ] )>, 
  <Green's H-class: Transformation( [ 10, 5, 9, 3, 5, 9, 10, 10, 10, 9 ] )>, 
  <Green's H-class: Transformation( [ 9, 1, 2, 5, 1, 2, 9, 9, 9, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 10, 7, 1, 10, 7, 4, 4, 4, 7 ] )>, 
  <Green's H-class: Transformation( [ 5, 1, 7, 2, 1, 7, 5, 5, 5, 7 ] )>, 
  <Green's H-class: Transformation( [ 10, 9, 4, 3, 9, 4, 10, 10, 10, 4 ] )>, 
  <Green's H-class: Transformation( [ 5, 9, 8, 2, 9, 8, 5, 5, 5, 8 ] )>, 
  <Green's H-class: Transformation( [ 1, 4, 8, 7, 4, 8, 1, 1, 1, 8 ] )>, 
  <Green's H-class: Transformation( [ 7, 5, 2, 3, 5, 2, 7, 7, 7, 2 ] )>, 
  <Green's H-class: Transformation( [ 10, 1, 4, 3, 1, 4, 10, 10, 10, 4 ] )>, 
  <Green's H-class: Transformation( [ 8, 1, 7, 3, 1, 7, 8, 8, 8, 7 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 7, 1, 2, 7, 3, 3, 3, 7 ] )>, 
  <Green's H-class: Transformation( [ 1, 4, 7, 2, 4, 7, 1, 1, 1, 7 ] )>, 
  <Green's H-class: Transformation( [ 5, 4, 7, 2, 4, 7, 5, 5, 5, 7 ] )>, 
  <Green's H-class: Transformation( [ 10, 5, 4, 3, 5, 4, 10, 10, 10, 4 ] )>, 
  <Green's H-class: Transformation( [ 1, 7, 3, 10, 7, 3, 1, 1, 1, 3 ] )>, 
  <Green's H-class: Transformation( [ 9, 4, 1, 2, 4, 1, 9, 9, 9, 1 ] )>, 
  <Green's H-class: Transformation( [ 5, 8, 4, 2, 8, 4, 5, 5, 5, 4 ] )>, 
  <Green's H-class: Transformation( [ 10, 6, 5, 3, 6, 5, 10, 10, 10, 5 ] )>, 
  <Green's H-class: Transformation( [ 2, 3, 10, 1, 3, 10, 2, 2, 2, 10 ] )>, 
  <Green's H-class: Transformation( [ 3, 1, 2, 9, 1, 2, 3, 3, 3, 2 ] )>, 
  <Green's H-class: Transformation( [ 10, 1, 9, 3, 1, 9, 10, 10, 10, 9 ] )>, 
  <Green's H-class: Transformation( [ 2, 9, 10, 1, 9, 10, 2, 2, 2, 10 ] )>, 
  <Green's H-class: Transformation( [ 2, 4, 1, 8, 4, 1, 2, 2, 2, 1 ] )>, 
  <Green's H-class: Transformation( [ 5, 3, 4, 2, 3, 4, 5, 5, 5, 4 ] )>, 
  <Green's H-class: Transformation( [ 10, 1, 5, 3, 1, 5, 10, 10, 10, 5 ] )>, 
  <Green's H-class: Transformation( [ 3, 5, 9, 6, 5, 9, 3, 3, 3, 9 ] )>, 
  <Green's H-class: Transformation( [ 3, 1, 4, 9, 1, 4, 3, 3, 3, 4 ] )>, 
  <Green's H-class: Transformation( [ 1, 5, 10, 9, 5, 10, 1, 1, 1, 10 ] )>, 
  <Green's H-class: Transformation( [ 3, 10, 7, 4, 10, 7, 3, 3, 3, 7 ] )>, 
  <Green's H-class: Transformation( [ 3, 10, 8, 7, 10, 8, 3, 3, 3, 8 ] )>, 
  <Green's H-class: Transformation( [ 1, 2, 6, 4, 2, 6, 1, 1, 1, 6 ] )>, 
  <Green's H-class: Transformation( [ 9, 1, 5, 8, 1, 5, 9, 9, 9, 5 ] )>, 
  <Green's H-class: Transformation( [ 4, 1, 8, 10, 1, 8, 4, 4, 4, 8 ] )>, 
  <Green's H-class: Transformation( [ 5, 3, 1, 2, 3, 1, 5, 5, 5, 1 ] )>, 
  <Green's H-class: Transformation( [ 5, 9, 6, 2, 9, 6, 5, 5, 5, 6 ] )>, 
  <Green's H-class: Transformation( [ 1, 4, 9, 7, 4, 9, 1, 1, 1, 9 ] )>, 
  <Green's H-class: Transformation( [ 8, 4, 7, 10, 4, 7, 8, 8, 8, 7 ] )>, 
  <Green's H-class: Transformation( [ 3, 5, 7, 1, 5, 7, 3, 3, 3, 7 ] )>, 
  <Green's H-class: Transformation( [ 4, 10, 9, 1, 10, 9, 4, 4, 4, 9 ] )>, 
  <Green's H-class: Transformation( [ 5, 1, 8, 2, 1, 8, 5, 5, 5, 8 ] )>, 
  <Green's H-class: Transformation( [ 10, 6, 9, 3, 6, 9, 10, 10, 10, 9 ] )>, 
  <Green's H-class: Transformation( [ 1, 10, 8, 7, 10, 8, 1, 1, 1, 8 ] )>, 
  <Green's H-class: Transformation( [ 9, 2, 6, 4, 2, 6, 9, 9, 9, 6 ] )>, 
  <Green's H-class: Transformation( [ 9, 10, 2, 5, 10, 2, 9, 9, 9, 2 ] )>, 
  <Green's H-class: Transformation( [ 10, 3, 1, 8, 3, 1, 10, 10, 10, 1 ] )>, 
  <Green's H-class: Transformation( [ 2, 1, 9, 6, 1, 9, 2, 2, 2, 9 ] )>, 
  <Green's H-class: Transformation( [ 7, 10, 4, 9, 10, 4, 7, 7, 7, 4 ] )>, 
  <Green's H-class: Transformation( [ 7, 1, 5, 8, 1, 5, 7, 7, 7, 5 ] )>, 
  <Green's H-class: Transformation( [ 7, 2, 4, 3, 2, 4, 7, 7, 7, 4 ] )>, 
  <Green's H-class: Transformation( [ 1, 4, 5, 7, 4, 5, 1, 1, 1, 5 ] )>, 
  <Green's H-class: Transformation( [ 9, 5, 10, 4, 5, 10, 9, 9, 9, 10 ] )>, 
  <Green's H-class: Transformation( [ 5, 1, 8, 4, 1, 8, 5, 5, 5, 8 ] )>, 
  <Green's H-class: Transformation( [ 9, 10, 6, 5, 10, 6, 9, 9, 9, 6 ] )>, 
  <Green's H-class: Transformation( [ 4, 10, 9, 6, 10, 9, 4, 4, 4, 9 ] )>, 
  <Green's H-class: Transformation( [ 10, 2, 5, 3, 2, 5, 10, 10, 10, 5 ] )>, 
  <Green's H-class: Transformation( [ 5, 10, 4, 2, 10, 4, 5, 5, 5, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 5, 8, 7, 5, 8, 2, 2, 2, 8 ] )>, 
  <Green's H-class: Transformation( [ 3, 10, 6, 4, 10, 6, 3, 3, 3, 6 ] )>, 
  <Green's H-class: Transformation( [ 3, 10, 9, 7, 10, 9, 3, 3, 3, 9 ] )>, 
  <Green's H-class: Transformation( [ 1, 2, 10, 4, 2, 10, 1, 1, 1, 10 ] )>, 
  <Green's H-class: Transformation( [ 3, 5, 2, 9, 5, 2, 3, 3, 3, 2 ] )>, 
  <Green's H-class: Transformation( [ 3, 1, 7, 4, 1, 7, 3, 3, 3, 7 ] )>, 
  <Green's H-class: Transformation( [ 9, 1, 4, 5, 1, 4, 9, 9, 9, 4 ] )>, 
  <Green's H-class: Transformation( [ 3, 10, 8, 4, 10, 8, 3, 3, 3, 8 ] )>, 
  <Green's H-class: Transformation( [ 2, 1, 5, 6, 1, 5, 2, 2, 2, 5 ] )>, 
  <Green's H-class: Transformation( [ 7, 10, 1, 9, 10, 1, 7, 7, 7, 1 ] )>, 
  <Green's H-class: Transformation( [ 7, 1, 2, 8, 1, 2, 7, 7, 7, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 3, 9, 6, 3, 9, 4, 4, 4, 9 ] )>, 
  <Green's H-class: Transformation( [ 7, 3, 4, 9, 3, 4, 7, 7, 7, 4 ] )>, 
  <Green's H-class: Transformation( [ 1, 5, 10, 4, 5, 10, 1, 1, 1, 10 ] )>, 
  <Green's H-class: Transformation( [ 3, 4, 8, 7, 4, 8, 3, 3, 3, 8 ] )>, 
  <Green's H-class: Transformation( [ 1, 5, 6, 4, 5, 6, 1, 1, 1, 6 ] )>, 
  <Green's H-class: Transformation( [ 9, 2, 10, 4, 2, 10, 9, 9, 9, 10 ] )>, 
  <Green's H-class: Transformation( [ 3, 10, 2, 9, 10, 2, 3, 3, 3, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 5, 10, 1, 5, 10, 2, 2, 2, 10 ] )>, 
  <Green's H-class: Transformation( [ 9, 5, 4, 3, 5, 4, 9, 9, 9, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 1, 9, 6, 1, 9, 4, 4, 4, 9 ] )>, 
  <Green's H-class: Transformation( [ 9, 5, 6, 4, 5, 6, 9, 9, 9, 6 ] )>, 
  <Green's H-class: Transformation( [ 1, 5, 3, 6, 5, 3, 1, 1, 1, 3 ] )> ]
gap> List(last, x -> Representative(x) in s);
[ true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true ]
gap> ForAll(last2, x -> Representative(x) in r);
true
gap> Semigroup(gens);;
gap> r := GreensRClassOfElement(s, f);
<Green's R-class: Transformation( [ 8, 8, 8, 8, 8, 8, 7, 7, 8, 8 ] )>
gap> Transformation([1, 1, 1, 1, 1, 1, 9, 9, 1, 1]) in last;
true
gap> f := Transformation([9, 9, 5, 9, 5, 9, 5, 5, 5, 5]);;
gap> r := GreensRClassOfElement(s, f);
<Green's R-class: Transformation( [ 9, 9, 5, 9, 5, 9, 5, 5, 5, 5 ] )>
gap> Transformation([9, 9, 1, 9, 1, 9, 1, 1, 1, 1]) in last;
true
gap> Size(r);
86
gap> NrHClasses(r);
43
gap> s := Semigroup(gens);;
gap> r := GreensRClassOfElement(s, f);
<Green's R-class: Transformation( [ 9, 9, 5, 9, 5, 9, 5, 5, 5, 5 ] )>
gap> Transformation([9, 9, 1, 9, 1, 9, 1, 1, 1, 1]) in last;
true
gap> GreensHClasses(r);
[ <Green's H-class: Transformation( [ 9, 9, 1, 9, 1, 9, 1, 1, 1, 1 ] )>, 
  <Green's H-class: Transformation( [ 8, 8, 2, 8, 2, 8, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 4, 3, 4, 3, 4, 3, 3, 3, 3 ] )>, 
  <Green's H-class: Transformation( [ 1, 1, 5, 1, 5, 1, 5, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 9, 9, 10, 9, 10, 9, 10, 10, 10, 10 ] )>,
  <Green's H-class: Transformation( [ 10, 10, 4, 10, 4, 10, 4, 4, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 8, 8, 5, 8, 5, 8, 5, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 8, 8, 1, 8, 1, 8, 1, 1, 1, 1 ] )>, 
  <Green's H-class: Transformation( [ 8, 8, 10, 8, 10, 8, 10, 10, 10, 10 ] )>,
  <Green's H-class: Transformation( [ 1, 1, 3, 1, 3, 1, 3, 3, 3, 3 ] )>, 
  <Green's H-class: Transformation( [ 3, 3, 10, 3, 10, 3, 10, 10, 10, 10 ] )>,
  <Green's H-class: Transformation( [ 2, 2, 5, 2, 5, 2, 5, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 7, 7, 1, 7, 1, 7, 1, 1, 1, 1 ] )>, 
  <Green's H-class: Transformation( [ 9, 9, 4, 9, 4, 9, 4, 4, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 9, 9, 8, 9, 8, 9, 8, 8, 8, 8 ] )>, 
  <Green's H-class: Transformation( [ 8, 8, 4, 8, 4, 8, 4, 4, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 7, 7, 8, 7, 8, 7, 8, 8, 8, 8 ] )>, 
  <Green's H-class: Transformation( [ 7, 7, 3, 7, 3, 7, 3, 3, 3, 3 ] )>, 
  <Green's H-class: Transformation( [ 9, 9, 2, 9, 2, 9, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 7, 7, 4, 7, 4, 7, 4, 4, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 4, 5, 4, 5, 4, 5, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 5, 5, 10, 5, 10, 5, 10, 10, 10, 10 ] )>,
  <Green's H-class: Transformation( [ 2, 2, 10, 2, 10, 2, 10, 10, 10, 10 ] )>,
  <Green's H-class: Transformation( [ 7, 7, 10, 7, 10, 7, 10, 10, 10, 10 ] )>,
  <Green's H-class: Transformation( [ 9, 9, 5, 9, 5, 9, 5, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 1, 1, 4, 1, 4, 1, 4, 4, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 4, 2, 4, 2, 4, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 5, 5, 3, 5, 3, 5, 3, 3, 3, 3 ] )>, 
  <Green's H-class: Transformation( [ 1, 1, 2, 1, 2, 1, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 9, 9, 3, 9, 3, 9, 3, 3, 3, 3 ] )>, 
  <Green's H-class: Transformation( [ 1, 1, 10, 1, 10, 1, 10, 10, 10, 10 ] )>,
  <Green's H-class: Transformation( [ 2, 2, 3, 2, 3, 2, 3, 3, 3, 3 ] )>, 
  <Green's H-class: Transformation( [ 7, 7, 5, 7, 5, 7, 5, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 8, 8, 3, 8, 3, 8, 3, 3, 3, 3 ] )>, 
  <Green's H-class: Transformation( [ 1, 1, 6, 1, 6, 1, 6, 6, 6, 6 ] )>, 
  <Green's H-class: Transformation( [ 4, 4, 6, 4, 6, 4, 6, 6, 6, 6 ] )>, 
  <Green's H-class: Transformation( [ 9, 9, 7, 9, 7, 9, 7, 7, 7, 7 ] )>, 
  <Green's H-class: Transformation( [ 6, 6, 5, 6, 5, 6, 5, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 6, 6, 10, 6, 10, 6, 10, 10, 10, 10 ] )>,
  <Green's H-class: Transformation( [ 7, 7, 2, 7, 2, 7, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 6, 2, 6, 2, 6, 6, 6, 6 ] )>, 
  <Green's H-class: Transformation( [ 9, 9, 6, 9, 6, 9, 6, 6, 6, 6 ] )>, 
  <Green's H-class: Transformation( [ 3, 3, 6, 3, 6, 3, 6, 6, 6, 6 ] )> ]
gap> Length(last);
43
gap> ForAll(last2, x -> Representative(x) in r);
true
gap> ForAll(last3, x -> Representative(x) in s);
true
gap> h := HClass(s, Transformation([4, 4, 9, 4, 9, 4, 9, 9, 9, 9]));;
gap> f := Representative(h);
Transformation( [ 4, 4, 9, 4, 9, 4, 9, 9, 9, 9 ] )
gap> hh := HClass(r, f);
<Green's H-class: Transformation( [ 4, 4, 9, 4, 9, 4, 9, 9, 9, 9 ] )>
gap> Transformation([4, 4, 9, 4, 9, 4, 9, 9, 9, 9]) in last;
true
gap> hh = h;
true
gap> h = hh;
true
gap> Elements(h) = Elements(hh);
true
gap> f := Transformation([10, 1, 9, 10, 2, 1, 5, 3, 2, 3]);;
gap> r := GreensRClassOfElement(s, f);
<Green's R-class: Transformation( [ 10, 1, 9, 10, 2, 1, 5, 3, 2, 3 ] )>
gap> Transformation([10, 1, 9, 10, 2, 1, 5, 3, 2, 3]) in last;
true
gap> Size(r);
1
gap> f := Transformation([10, 10, 3, 10, 10, 10, 10, 10, 6, 10]);;
gap> r := GreensRClassOfElement(s, f);
<Green's R-class: Transformation( [ 10, 10, 3, 10, 10, 10, 10, 10, 6, 10 ] )>
gap> Transformation([8, 8, 1, 8, 8, 8, 8, 8, 9, 8]) in last;
true
gap> Size(r);
546
gap> f := Transformation([6, 6, 4, 6, 6, 6, 6, 6, 3, 6]);;
gap> f in r;
true
gap> h := HClass(r, f);
<Green's H-class: Transformation( [ 6, 6, 4, 6, 6, 6, 6, 6, 3, 6 ] )>
gap> Transformation([6, 6, 4, 6, 6, 6, 6, 6, 3, 6]) in last;
true
gap> f in h;
true
gap> ForAll(h, x -> x in r);
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

# MiscTest47
gap> gens :=
> [PartialPermNC([1, 2, 3, 5, 9, 10], [5, 10, 7, 8, 9, 1]),
>  PartialPermNC([1, 2, 3, 4, 5, 6, 9], [9, 3, 1, 4, 2, 5, 6]),
>  PartialPermNC([1, 2, 3, 4, 5, 7, 9], [7, 6, 2, 8, 4, 5, 3]),
>  PartialPermNC([1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
> [8, 7, 4, 3, 10, 9, 5, 6, 1, 2])];;
gap> s := Semigroup(gens);;
gap> Size(s);
1422787
gap> f := PartialPerm([1, 4, 7, 9, 10], [5, 10, 9, 8, 7]);;
gap> r := GreensRClassOfElementNC(s, f);
<Green's R-class: [1,5][4,10,7,9,8]>
gap> Size(r);
4
gap> f in r;
true
gap> f := PartialPerm([1, 7, 8, 9], [10, 9, 6, 5]);;
gap> r := GreensRClassOfElementNC(s, f);
<Green's R-class: [1,10][7,9,5][8,6]>
gap> Size(r);
4
gap> iter := IteratorOfRClasses(s);
<iterator>
gap> repeat r := NextIterator(iter); until Size(r) > 1000;
gap> r;
<Green's R-class: [1,4][9,3,5][10,7]>
gap> Size(r);
3792
gap> r := RClassNC(s, Representative(r));
<Green's R-class: [1,4][9,3,5][10,7]>
gap> h := HClassNC(r, PartialPermNC([1, 3, 9, 10], [10, 9, 8, 1]));;
gap> Size(h);
24
gap> ForAll(h, x -> x in r);
true
gap> IsRegularGreensClass(r);
true
gap> IsRegularSemigroup(s);
false
gap> NrIdempotents(r);
1
gap> Idempotents(r);
[ <identity partial perm on [ 1, 3, 9, 10 ]> ]
gap> ForAll(last, x -> x in r);
true

# MiscTest48
gap> gens := [Transformation([1, 3, 7, 9, 1, 12, 13, 1, 15, 9, 1, 18, 1, 1,
>  13, 1, 1, 21, 1, 1, 1, 1, 1, 25, 26, 1]),
>  Transformation([1, 5, 1, 5, 11, 1, 1, 14, 1, 16, 17, 1, 1, 19, 1, 11, 1,
>       1, 1, 23, 1, 16, 19, 1, 1, 1]),
> Transformation([1, 4, 8, 1, 10, 1, 8, 1, 1, 1, 10, 1, 8, 10, 1, 1, 20, 1,
>       22, 1, 8, 1, 1, 1, 1, 1]),
> Transformation([1, 6, 6, 1, 1, 1, 6, 1, 1, 1, 1, 1, 6, 1, 6, 1, 1, 6, 1,
>       1, 24, 1, 1, 1, 1, 6])];;
gap> s := Semigroup(gens);;
gap> First(DClasses(s), IsRegularDClass);
<Green's D-class: Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] )>
gap> NrDClasses(s);
31
gap> PositionsProperty(DClasses(s), IsRegularDClass);
[ 6, 7 ]
gap> d := DClasses(s)[7];
<Green's D-class: Transformation( [ 1, 6, 1, 1, 6, 1, 1, 1, 1, 1, 6, 1, 1, 6,
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] )>
gap> r := RClassNC(s, Representative(d));
<Green's R-class: Transformation( [ 1, 6, 1, 1, 6, 1, 1, 1, 1, 1, 6, 1, 1, 6,
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] )>
gap> Size(r);
20
gap> ForAll(Idempotents(r), x -> x in s);
true
gap> ForAll(Idempotents(r), x -> x in r);
true
gap> ForAll(Idempotents(r), x -> x in d);
true
gap> ForAll(r, x -> x in d);
true
gap> Number(GreensRClasses(s), IsRegularGreensClass);
21
gap> NrRegularDClasses(s);
2

# MiscTest49
gap> gens := [Transformation([1, 2, 3, 5, 4, 6, 7, 8]),
>   Transformation([4, 4, 3, 1, 5, 6, 3, 8]),
>   Transformation([3, 6, 1, 7, 3, 4, 8, 3]),
>   Transformation([1, 2, 3, 4, 5, 3, 7, 8]),
>   Transformation([1, 2, 3, 4, 1, 6, 7, 8]),
>   Transformation([8, 8, 3, 4, 5, 7, 6, 1])];;
gap> s := Monoid(gens);
<transformation monoid of degree 8 with 6 generators>
gap> f := Transformation([4, 4, 3, 8, 5, 3, 3, 1]);;
gap> Size(s);
998
gap> r := RClass(s, f);
<Green's R-class: Transformation( [ 4, 4, 3, 8, 5, 3, 3, 1 ] )>
gap> Transformation([4, 4, 3, 8, 5, 3, 3, 1]) in last;
true
gap> IsRegularGreensClass(r);
true
gap> Idempotents(r);
[ Transformation( [ 1, 1, 3, 4, 5, 3, 3 ] ) ]
gap> IsRegularSemigroup(s);
false
gap> ForAll(r, x -> x in s);
true
gap> iter := Iterator(r);
<iterator>
gap> for i in iter do od;
gap> Size(r);
24
gap> IsDoneIterator(iter);
true
gap> iter := Iterator(r);
<iterator>
gap> for i in [1 .. 23] do NextIterator(iter); od;
gap> IsDoneIterator(iter);
false
gap> NextIterator(iter);
Transformation( [ 5, 5, 3, 1, 4, 3, 3 ] )
gap> IsDoneIterator(iter);
true
gap> Transformation([4, 4, 3, 8, 1, 3, 3, 5]) in r;
true
gap> r;
<Green's R-class: Transformation( [ 4, 4, 3, 8, 5, 3, 3, 1 ] )>
gap> Transformation([4, 4, 3, 8, 5, 3, 3, 1]) in last;
true
gap> NrIdempotents(r);
1

# MiscTest50
gap> gens :=
> [PartialPermNC([1, 2, 3, 4, 7], [8, 3, 5, 7, 4]),
>  PartialPermNC([1, 2, 5, 6, 7], [4, 1, 6, 2, 8]),
>  PartialPermNC([1, 2, 3, 4, 5, 6], [3, 7, 1, 5, 2, 6]),
>  PartialPermNC([1, 2, 3, 4, 5, 6], [7, 2, 5, 6, 3, 8]),
>  PartialPermNC([1, 2, 3, 5, 6, 7], [4, 5, 6, 1, 2, 7]),
>  PartialPermNC([1, 2, 3, 5, 6, 7], [5, 1, 7, 2, 8, 4])];;
gap> s := Semigroup(gens);;
gap> Size(s);
9954
gap> f := PartialPerm([2, 3, 6], [1, 4, 8]);;
gap> r := RClass(s, f);
<Green's R-class: [2,1][3,4][6,8]>
gap> NrIdempotents(r);
0
gap> List(RClasses(s), NrIdempotents);;
gap> Sum(last);
53
gap> NrIdempotents(s);
53
gap> r := RClassOfHClass(h);
<Green's R-class: [1,4][9,3,5][10,7]>
gap> HClassReps(r);
[ [1,4][9,3,5][10,7], [1,8][3,2][9,5][10,4], [1,6][9,10,3,7], 
  [1,9,2][3,5][10,4], [1,6][9,4][10,2](3), [9,4][10,3,1,5], [3,9,4][10,1,2], 
  [1,4][10,6](3)(9), [3,6][9,5][10,4](1), [1,8](3,9,10), [3,1,6][9,2][10,4], 
  [1,9,5][10,4](3), [1,8][9,2][10,4](3), [1,6][9,7][10,3,4], [9,10,3,4](1), 
  [1,9,7][10,3,8], [10,9,1,3,4], [3,4][10,6](1)(9), [1,9,6][3,4][10,5], 
  [1,6][3,4][9,5][10,2], [1,5][9,2][10,3,4], [9,3,4][10,1,2], 
  [1,3,8][9,7][10,4], [1,4][9,5][10,3,6], [3,5][9,2][10,1,4], [1,4][10,9,3,2],
  [1,8][9,2][10,3,6], [1,6][3,9,7][10,4], [10,3,1,9,5], [1,3,7][9,4][10,2], 
  [1,6][3,2][9,5][10,8], [1,9,10,6][3,7], [3,5][10,9,2](1), [1,9,3,2][10,6], 
  [9,1,6][10,5](3), [1,9,8][3,4](10), [9,6][10,2](1)(3), [1,8][3,4][10,7](9), 
  [3,7][10,9,4](1), [1,7][9,5][10,8](3), [1,5][3,4][9,10,6], [1,10,9,2](3), 
  [3,7](1)(9,10), [3,5][9,2][10,1,8], [1,6][3,10,8][9,7], [1,9,5][3,2][10,6], 
  [1,6][9,2][10,5](3), [9,3,1,5][10,2], [1,7][3,5][9,10,8], 
  [1,5][3,10,6][9,2], [1,10,9,7][3,2], [3,7][9,5][10,1,2], 
  [1,6][3,5][9,4][10,7], [1,9,3,10,5], [3,1,9,7][10,8], [3,8][9,5][10,6](1), 
  [1,8][3,6](9,10), [3,9,2][10,1,6], [1,5][10,9,3,6], [3,5][9,1,2][10,6], 
  [1,3,2][10,5](9), [1,10,8][3,7](9), [3,7][9,1,4](10), [1,3,5][9,8][10,2], 
  [1,4][3,10,7][9,6], [3,9,4](1,10), [9,3,1,2][10,8], [1,7][3,8][9,4][10,6], 
  [3,2][9,4](1)(10), [1,8][9,3,7][10,2], [10,3,9,1,2], [1,10,7][3,9,5], 
  [3,1,2][9,10,5], [3,5][9,1,10,8], [1,2][3,10,6][9,8], [1,7][3,2][10,9,6], 
  [3,7][10,1,5](9), [1,7][3,8][9,2](10), [1,5][3,6][9,7][10,2], 
  [10,1,3,6](9), [3,5][10,9,6](1), [3,10,1,8](9), [3,2][9,1,6][10,8], 
  [1,9,8][3,7][10,6], [3,9,1,4][10,8], [9,8][10,6](1,3), [1,4][3,8][10,9,6], 
  [1,7][9,6][10,3,2], [1,5][3,7][10,4](9), [1,4][9,3,5][10,8], 
  [1,3,10,6][9,4], [9,1,10,3,5], [3,1,5][9,7][10,8], [1,10,6][3,8][9,5], 
  [1,2][3,6](9,10), [3,9,2][10,1,7], [1,5][9,6][10,7](3), [1,10,5][3,4](9), 
  [9,1,2](3)(10), [1,7][3,4][9,8][10,2], [3,7][9,5](1,10), [1,2][3,5][9,10,8],
  [1,7][3,10,6][9,2], [1,5][3,2][10,9,7], [1,2][3,10,4][9,8], 
  [3,1,7][9,8][10,4], [1,5][9,6][10,3,8], [1,10,4][3,6](9), 
  [1,2][3,7][9,4][10,6], [1,7][10,9,3,5], [3,10,1,5][9,4], [1,10,8][9,3,2], 
  [1,8][3,4][9,10,7], [1,7][3,9,4](10), [3,4][9,7][10,1,2], [9,3,5][10,1,8], 
  [1,6][3,10,8][9,4], [3,10,1,8][9,7], [3,8][9,10,4](1), [9,1,4][10,6](3), 
  [3,1,4][10,5](9), [1,4][3,9,6][10,2], [10,9,1,3,7], [1,2][9,7][10,3,5], 
  [1,7][3,10,4][9,5], [1,5][9,10,3,2], [1,10,4][3,7][9,2], [3,8][9,1,4][10,5],
  [1,3,6][9,8](10), [9,1,3,8](10), [1,4][3,6][9,8][10,2], [1,3,9,6][10,7], 
  [1,3,4][10,8](9), [9,4][10,1,3,7], [1,2][3,5][9,8][10,7], 
  [1,7][3,10,5][9,6], [1,5][3,2](9)(10), [3,7][9,1,10,2], [1,8][9,7](3)(10), 
  [1,10,9,4](3), [1,7][9,10,4](3), [1,3,7](9,10), 
  <identity partial perm on [ 1, 3, 9, 10 ]>, [3,4][9,1,8][10,2], 
  [1,6][9,8][10,7](3), [10,9,1,8](3), [3,4][9,8][10,1,6], [1,9,6][10,8](3), 
  [9,8][10,1,4](3), [1,3,4][9,6][10,8], [9,3,1,7][10,8], 
  [1,5][3,8][9,4][10,6], [1,10,9,3,6], [1,8][9,3,4](10), [1,7][10,4](3,9), 
  [9,7](1)(3,10), [1,3,10,5][9,4], [1,4][9,3,2](10) ]
gap> iter := IteratorOfRClasses(s);
<iterator>
gap> repeat r := NextIterator(iter); until Size(r) > 1;
gap> r;
<Green's R-class: (1,5,2)>
gap> Size(r);
120
gap> HClassReps(r);
[ (1,5,2), [2,6][5,4](1), [1,3][2,6](5), [5,2,6](1), [5,2,4](1), 
  [1,7][5,2,6], [1,7](2)(5), [2,5,1,7], [1,4](2)(5), [1,6][5,3](2), 
  [1,2,5,6], [1,7][2,3][5,6], [1,7][5,3](2), [1,4][2,3](5), [1,6][2,5,7], 
  [5,7](1,2), [2,5,4](1), [1,3](2)(5), [2,5,1,6], [1,6][5,2,4] ]
gap> ForAll(last, x -> x in r);
true
gap> r;
<Green's R-class: (1,5,2)>
gap> Size(DClass(r));
2400
gap> d := DClass(r);
<Green's D-class: (1,5,2)>
gap> ForAll(r, x -> x in d);
true
gap> Number(d, x -> x in r);
120
gap> Size(r);
120
gap> ForAll(HClassReps(r), x -> x in d);
true
gap> ForAll(HClassReps(r), x -> x in HClassReps(d));
true

# MiscTest51
gap> gens := [Transformation([6, 4, 3, 2, 5, 1]),
>   Transformation([1, 2, 3, 4, 5, 6]),
>   Transformation([5, 3, 3, 2, 4, 1]),
>   Transformation([1, 3, 3, 4, 5, 2]),
>   Transformation([4, 5, 2, 3, 3, 1]),
>   Transformation([6, 4, 3, 5, 2, 3]),
>   Transformation([5, 2, 3, 4, 3, 6]),
>   Transformation([1, 3, 2, 5, 4, 5]),
>   Transformation([4, 3, 2, 2, 1, 5]),
>   Transformation([1, 3, 3, 5, 2, 4]),
>   Transformation([6, 3, 3, 2, 1, 5]),
>   Transformation([6, 3, 4, 5, 2, 2]),
>   Transformation([6, 4, 3, 2, 2, 5]),
>   Transformation([1, 3, 2, 3, 5, 4]),
>   Transformation([1, 2, 3, 4, 5, 2]),
>   Transformation([2, 4, 3, 4, 6, 5]),
>   Transformation([2, 4, 3, 3, 6, 1]),
>   Transformation([6, 4, 3, 2, 3, 1]),
>   Transformation([6, 4, 3, 2, 2, 1])];;
gap> s := Semigroup(gens);;
gap> Size(s);
7008
gap> NrRClasses(s);
310
gap> IsRegularSemigroup(s);
false
gap> f := Transformation([3, 2, 3, 4, 3, 5]);;
gap> r := RClassNC(s, f);
<Green's R-class: Transformation( [ 3, 2, 3, 4, 3, 5 ] )>
gap> Transformation([3, 2, 3, 4, 3, 5]) in last;
true
gap> d := DClassOfRClass(r);
<Green's D-class: Transformation( [ 3, 2, 3, 4, 3, 5 ] )>
gap> Transformation([3, 2, 3, 4, 3, 5]) in last;
true
gap> Size(d);
792
gap> IsRegularDClass(d);
false
gap> NrIdempotents(d);
0
gap> Idempotents(d);
[  ]
gap> HClassReps(d);
[ Transformation( [ 3, 2, 3, 4, 3, 5 ] ), 
  Transformation( [ 3, 2, 3, 5, 3, 4 ] ), 
  Transformation( [ 4, 2, 4, 5, 4, 3 ] ), 
  Transformation( [ 2, 3, 2, 4, 2, 5 ] ), 
  Transformation( [ 2, 3, 2, 5, 2, 4 ] ), 
  Transformation( [ 2, 4, 2, 3, 2, 5 ] ), 
  Transformation( [ 5, 4, 3, 2, 3, 3 ] ), 
  Transformation( [ 4, 5, 3, 2, 3, 3 ] ), 
  Transformation( [ 3, 5, 4, 2, 4, 4 ] ), 
  Transformation( [ 5, 4, 2, 3, 2, 2 ] ), 
  Transformation( [ 4, 5, 2, 3, 2, 2 ] ), 
  Transformation( [ 5, 3, 2, 4, 2, 2 ] ), 
  Transformation( [ 3, 3, 3, 4, 2, 5 ] ), 
  Transformation( [ 3, 3, 3, 5, 2, 4 ] ), 
  Transformation( [ 4, 4, 4, 5, 2, 3 ] ), 
  Transformation( [ 2, 2, 2, 4, 3, 5 ] ), 
  Transformation( [ 2, 2, 2, 5, 3, 4 ] ), 
  Transformation( [ 2, 2, 2, 3, 4, 5 ] ), 
  Transformation( [ 5, 4, 3, 3, 2, 3 ] ), 
  Transformation( [ 4, 5, 3, 3, 2, 3 ] ), 
  Transformation( [ 3, 5, 4, 4, 2, 4 ] ), 
  Transformation( [ 5, 4, 2, 2, 3, 2 ] ), 
  Transformation( [ 4, 5, 2, 2, 3, 2 ] ), 
  Transformation( [ 5, 3, 2, 2, 4, 2 ] ), 
  Transformation( [ 5, 3, 4, 3, 2, 3 ] ), 
  Transformation( [ 4, 3, 5, 3, 2, 3 ] ), 
  Transformation( [ 3, 4, 5, 4, 2, 4 ] ), 
  Transformation( [ 5, 2, 4, 2, 3, 2 ] ), 
  Transformation( [ 4, 2, 5, 2, 3, 2 ] ), 
  Transformation( [ 5, 2, 3, 2, 4, 2 ] ), 
  Transformation( [ 2, 4, 3, 4, 5, 3 ] ), 
  Transformation( [ 2, 5, 3, 5, 4, 3 ] ), 
  Transformation( [ 2, 5, 4, 5, 3, 4 ] ), 
  Transformation( [ 3, 4, 2, 4, 5, 2 ] ), 
  Transformation( [ 3, 5, 2, 5, 4, 2 ] ), 
  Transformation( [ 4, 3, 2, 3, 5, 2 ] ), 
  Transformation( [ 3, 2, 3, 4, 4, 5 ] ), 
  Transformation( [ 3, 2, 3, 5, 5, 4 ] ), 
  Transformation( [ 4, 2, 4, 5, 5, 3 ] ), 
  Transformation( [ 2, 3, 2, 4, 4, 5 ] ), 
  Transformation( [ 2, 3, 2, 5, 5, 4 ] ), 
  Transformation( [ 2, 4, 2, 3, 3, 5 ] ), 
  Transformation( [ 5, 4, 3, 2, 2, 3 ] ), 
  Transformation( [ 4, 5, 3, 2, 2, 3 ] ), 
  Transformation( [ 3, 5, 4, 2, 2, 4 ] ), 
  Transformation( [ 5, 4, 2, 3, 3, 2 ] ), 
  Transformation( [ 4, 5, 2, 3, 3, 2 ] ), 
  Transformation( [ 5, 3, 2, 4, 4, 2 ] ), 
  Transformation( [ 5, 3, 4, 3, 2, 2 ] ), 
  Transformation( [ 4, 3, 5, 3, 2, 2 ] ), 
  Transformation( [ 3, 4, 5, 4, 2, 2 ] ), 
  Transformation( [ 5, 2, 4, 2, 3, 3 ] ), 
  Transformation( [ 4, 2, 5, 2, 3, 3 ] ), 
  Transformation( [ 5, 2, 3, 2, 4, 4 ] ), 
  Transformation( [ 2, 3, 4, 3, 2, 5 ] ), 
  Transformation( [ 2, 3, 5, 3, 2, 4 ] ), 
  Transformation( [ 2, 4, 5, 4, 2, 3 ] ), 
  Transformation( [ 3, 2, 4, 2, 3, 5 ] ), 
  Transformation( [ 3, 2, 5, 2, 3, 4 ] ), 
  Transformation( [ 4, 2, 3, 2, 4, 5 ] ), 
  Transformation( [ 3, 3, 4, 3, 2, 5 ] ), 
  Transformation( [ 3, 3, 5, 3, 2, 4 ] ), 
  Transformation( [ 4, 4, 5, 4, 2, 3 ] ), 
  Transformation( [ 2, 2, 4, 2, 3, 5 ] ), 
  Transformation( [ 2, 2, 5, 2, 3, 4 ] ), 
  Transformation( [ 2, 2, 3, 2, 4, 5 ] ), 
  Transformation( [ 5, 4, 3, 2, 4, 3 ] ), 
  Transformation( [ 4, 5, 3, 2, 5, 3 ] ), 
  Transformation( [ 3, 5, 4, 2, 5, 4 ] ), 
  Transformation( [ 5, 4, 2, 3, 4, 2 ] ), 
  Transformation( [ 4, 5, 2, 3, 5, 2 ] ), 
  Transformation( [ 5, 3, 2, 4, 3, 2 ] ), 
  Transformation( [ 5, 3, 4, 2, 3, 3 ] ), 
  Transformation( [ 4, 3, 5, 2, 3, 3 ] ), 
  Transformation( [ 3, 4, 5, 2, 4, 4 ] ), 
  Transformation( [ 5, 2, 4, 3, 2, 2 ] ), 
  Transformation( [ 4, 2, 5, 3, 2, 2 ] ), 
  Transformation( [ 5, 2, 3, 4, 2, 2 ] ), 
  Transformation( [ 3, 2, 4, 3, 3, 5 ] ), 
  Transformation( [ 3, 2, 5, 3, 3, 4 ] ), 
  Transformation( [ 4, 2, 5, 4, 4, 3 ] ), 
  Transformation( [ 2, 3, 4, 2, 2, 5 ] ), 
  Transformation( [ 2, 3, 5, 2, 2, 4 ] ), 
  Transformation( [ 2, 4, 3, 2, 2, 5 ] ), 
  Transformation( [ 2, 3, 4, 4, 5, 3 ] ), 
  Transformation( [ 2, 3, 5, 5, 4, 3 ] ), 
  Transformation( [ 2, 4, 5, 5, 3, 4 ] ), 
  Transformation( [ 3, 2, 4, 4, 5, 2 ] ), 
  Transformation( [ 3, 2, 5, 5, 4, 2 ] ), 
  Transformation( [ 4, 2, 3, 3, 5, 2 ] ), 
  Transformation( [ 5, 3, 4, 2, 3, 2 ] ), 
  Transformation( [ 4, 3, 5, 2, 3, 2 ] ), 
  Transformation( [ 3, 4, 5, 2, 4, 2 ] ), 
  Transformation( [ 5, 2, 4, 3, 2, 3 ] ), 
  Transformation( [ 4, 2, 5, 3, 2, 3 ] ), 
  Transformation( [ 5, 2, 3, 4, 2, 4 ] ), 
  Transformation( [ 2, 2, 4, 3, 3, 5 ] ), 
  Transformation( [ 2, 2, 5, 3, 3, 4 ] ), 
  Transformation( [ 2, 2, 5, 4, 4, 3 ] ), 
  Transformation( [ 3, 3, 4, 2, 2, 5 ] ), 
  Transformation( [ 3, 3, 5, 2, 2, 4 ] ), 
  Transformation( [ 4, 4, 3, 2, 2, 5 ] ), 
  Transformation( [ 5, 4, 3, 2, 3, 2 ] ), 
  Transformation( [ 4, 5, 3, 2, 3, 2 ] ), 
  Transformation( [ 3, 5, 4, 2, 4, 2 ] ), 
  Transformation( [ 5, 4, 2, 3, 2, 3 ] ), 
  Transformation( [ 4, 5, 2, 3, 2, 3 ] ), 
  Transformation( [ 5, 3, 2, 4, 2, 4 ] ), 
  Transformation( [ 5, 4, 3, 2, 3, 4 ] ), 
  Transformation( [ 4, 5, 3, 2, 3, 5 ] ), 
  Transformation( [ 3, 5, 4, 2, 4, 5 ] ), 
  Transformation( [ 5, 4, 2, 3, 2, 4 ] ), 
  Transformation( [ 4, 5, 2, 3, 2, 5 ] ), 
  Transformation( [ 5, 3, 2, 4, 2, 3 ] ), 
  Transformation( [ 4, 2, 3, 4, 3, 5 ] ), 
  Transformation( [ 5, 2, 3, 5, 3, 4 ] ), 
  Transformation( [ 5, 2, 4, 5, 4, 3 ] ), 
  Transformation( [ 4, 3, 2, 4, 2, 5 ] ), 
  Transformation( [ 5, 3, 2, 5, 2, 4 ] ), 
  Transformation( [ 3, 4, 2, 3, 2, 5 ] ), 
  Transformation( [ 5, 4, 3, 2, 2, 4 ] ), 
  Transformation( [ 4, 5, 3, 2, 2, 5 ] ), 
  Transformation( [ 3, 5, 4, 2, 2, 5 ] ), 
  Transformation( [ 5, 4, 2, 3, 3, 4 ] ), 
  Transformation( [ 4, 5, 2, 3, 3, 5 ] ), 
  Transformation( [ 5, 3, 2, 4, 4, 3 ] ), 
  Transformation( [ 2, 3, 3, 4, 2, 5 ] ), 
  Transformation( [ 2, 3, 3, 5, 2, 4 ] ), 
  Transformation( [ 2, 4, 4, 5, 2, 3 ] ), 
  Transformation( [ 3, 2, 2, 4, 3, 5 ] ), 
  Transformation( [ 3, 2, 2, 5, 3, 4 ] ), 
  Transformation( [ 4, 2, 2, 3, 4, 5 ] ), 
  Transformation( [ 3, 4, 3, 4, 5, 2 ] ), 
  Transformation( [ 3, 5, 3, 5, 4, 2 ] ), 
  Transformation( [ 4, 5, 4, 5, 3, 2 ] ), 
  Transformation( [ 2, 4, 2, 4, 5, 3 ] ), 
  Transformation( [ 2, 5, 2, 5, 4, 3 ] ), 
  Transformation( [ 2, 3, 2, 3, 5, 4 ] ), 
  Transformation( [ 5, 3, 4, 4, 2, 2 ] ), 
  Transformation( [ 4, 3, 5, 5, 2, 2 ] ), 
  Transformation( [ 3, 4, 5, 5, 2, 2 ] ), 
  Transformation( [ 5, 2, 4, 4, 3, 3 ] ), 
  Transformation( [ 4, 2, 5, 5, 3, 3 ] ), 
  Transformation( [ 5, 2, 3, 3, 4, 4 ] ), 
  Transformation( [ 2, 4, 4, 3, 5, 2 ] ), 
  Transformation( [ 2, 5, 5, 3, 4, 2 ] ), 
  Transformation( [ 2, 5, 5, 4, 3, 2 ] ), 
  Transformation( [ 3, 4, 4, 2, 5, 3 ] ), 
  Transformation( [ 3, 5, 5, 2, 4, 3 ] ), 
  Transformation( [ 4, 3, 3, 2, 5, 4 ] ), 
  Transformation( [ 2, 3, 4, 4, 5, 2 ] ), 
  Transformation( [ 2, 3, 5, 5, 4, 2 ] ), 
  Transformation( [ 2, 4, 5, 5, 3, 2 ] ), 
  Transformation( [ 3, 2, 4, 4, 5, 3 ] ), 
  Transformation( [ 3, 2, 5, 5, 4, 3 ] ), 
  Transformation( [ 4, 2, 3, 3, 5, 4 ] ), 
  Transformation( [ 5, 3, 4, 2, 2, 2 ] ), 
  Transformation( [ 4, 3, 5, 2, 2, 2 ] ), 
  Transformation( [ 3, 4, 5, 2, 2, 2 ] ), 
  Transformation( [ 5, 2, 4, 3, 3, 3 ] ), 
  Transformation( [ 4, 2, 5, 3, 3, 3 ] ), 
  Transformation( [ 5, 2, 3, 4, 4, 4 ] ), 
  Transformation( [ 2, 2, 4, 3, 4, 5 ] ), 
  Transformation( [ 2, 2, 5, 3, 5, 4 ] ), 
  Transformation( [ 2, 2, 5, 4, 5, 3 ] ), 
  Transformation( [ 3, 3, 4, 2, 4, 5 ] ), 
  Transformation( [ 3, 3, 5, 2, 5, 4 ] ), 
  Transformation( [ 4, 4, 3, 2, 3, 5 ] ), 
  Transformation( [ 2, 2, 4, 3, 2, 5 ] ), 
  Transformation( [ 2, 2, 5, 3, 2, 4 ] ), 
  Transformation( [ 2, 2, 5, 4, 2, 3 ] ), 
  Transformation( [ 3, 3, 4, 2, 3, 5 ] ), 
  Transformation( [ 3, 3, 5, 2, 3, 4 ] ), 
  Transformation( [ 4, 4, 3, 2, 4, 5 ] ), 
  Transformation( [ 2, 4, 3, 4, 5, 2 ] ), 
  Transformation( [ 2, 5, 3, 5, 4, 2 ] ), 
  Transformation( [ 2, 5, 4, 5, 3, 2 ] ), 
  Transformation( [ 3, 4, 2, 4, 5, 3 ] ), 
  Transformation( [ 3, 5, 2, 5, 4, 3 ] ), 
  Transformation( [ 4, 3, 2, 3, 5, 4 ] ), 
  Transformation( [ 3, 4, 4, 3, 5, 2 ] ), 
  Transformation( [ 3, 5, 5, 3, 4, 2 ] ), 
  Transformation( [ 4, 5, 5, 4, 3, 2 ] ), 
  Transformation( [ 2, 4, 4, 2, 5, 3 ] ), 
  Transformation( [ 2, 5, 5, 2, 4, 3 ] ), 
  Transformation( [ 2, 3, 3, 2, 5, 4 ] ), 
  Transformation( [ 4, 2, 3, 4, 2, 5 ] ), 
  Transformation( [ 5, 2, 3, 5, 2, 4 ] ), 
  Transformation( [ 5, 2, 4, 5, 2, 3 ] ), 
  Transformation( [ 4, 3, 2, 4, 3, 5 ] ), 
  Transformation( [ 5, 3, 2, 5, 3, 4 ] ), 
  Transformation( [ 3, 4, 2, 3, 4, 5 ] ), 
  Transformation( [ 3, 2, 3, 4, 2, 5 ] ), 
  Transformation( [ 3, 2, 3, 5, 2, 4 ] ), 
  Transformation( [ 4, 2, 4, 5, 2, 3 ] ), 
  Transformation( [ 2, 3, 2, 4, 3, 5 ] ), 
  Transformation( [ 2, 3, 2, 5, 3, 4 ] ), 
  Transformation( [ 2, 4, 2, 3, 4, 5 ] ) ]
gap> Number(HClassReps(d), x -> x in r);
6
gap> NrHClasses(r);
6

# MiscTest52
gap> gens :=
> [PartialPermNC([1, 2, 4], [2, 5, 3]),
>  PartialPermNC([1, 2, 4], [5, 6, 1]),
>  PartialPermNC([1, 2, 5], [5, 3, 2]),
>  PartialPermNC([1, 2, 3, 4], [5, 1, 2, 4]),
>  PartialPermNC([1, 2, 3, 4], [5, 1, 2, 6]),
>  PartialPermNC([1, 2, 3, 4], [5, 6, 4, 1]),
>  PartialPermNC([1, 2, 3, 5], [1, 5, 2, 6]),
>  PartialPermNC([1, 2, 3, 5], [2, 3, 4, 1]),
>  PartialPermNC([1, 2, 3, 5], [2, 5, 4, 1]),
>  PartialPermNC([1, 2, 3, 5], [5, 1, 2, 3]),
>  PartialPermNC([1, 2, 3, 6], [1, 4, 6, 5]),
>  PartialPermNC([1, 2, 5, 6], [6, 4, 2, 5]),
>  PartialPermNC([1, 3, 4, 6], [2, 3, 1, 6]),
>  PartialPermNC([1, 2, 3, 4, 5], [3, 6, 5, 2, 4]),
>  PartialPermNC([1, 2, 3, 4, 5], [6, 5, 3, 2, 1]),
>  PartialPermNC([1, 2, 3, 4, 6], [1, 3, 4, 6, 2]),
>  PartialPermNC([1, 2, 3, 5, 6], [1, 3, 6, 4, 5]),
>  PartialPermNC([1, 2, 4, 5, 6], [5, 4, 2, 1, 6]),
>  PartialPermNC([1, 2, 3, 4, 5, 6], [2, 5, 6, 4, 3, 1])];;
gap> s := Semigroup(gens);;
gap> Size(s);
12612
gap> f := PartialPerm([1, 4, 6], [2, 3, 6]);;
gap> r := RClass(s, f);
<Green's R-class: [1,2][4,3](6)>
gap> Size(r);
120
gap> NrHClasses(r);
20
gap> Number(HClassReps(s), x -> x in r);
20

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/misc.tst");
