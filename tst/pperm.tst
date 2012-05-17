#############################################################################
##
#W  pperm.tst
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Citrus package: pperm.tst");
gap> LoadPackage("citrus", false);;

#
gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoCitrus:=InfoLevel(InfoCitrus);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoCitrus, 0);

#
gap> f:=PartialPermNC([0,1,0,20]);
[ 2, 4 ] -> [ 1, 20 ]
gap> InternalRepOfPartialPerm(f);
[ 4, 2, 1, 20, 1, 20, 0, 1, 0, 20, 2, 4, 1, 20, 0, 0 ]
gap> f^2;
<empty mapping>
gap> f^-1;
[ 1, 20 ] -> [ 2, 4 ]
gap> f:=PartialPermNC([0,20,0,1]); 
[ 2, 4 ] -> [ 20, 1 ]
gap> f^2;
<empty mapping>
gap> f^-1;
[ 1, 20 ] -> [ 4, 2 ]
gap> RangeSetOfPartialPerm(f^-1);
[ 2, 4 ]
gap> RangeSetOfPartialPerm(f);   
[ 1, 20 ]
gap> f^-1<f;
true
gap> f<f^-1;
false
gap> f:=PartialPermNC([2,4], [20,1]);
[ 2, 4 ] -> [ 20, 1 ]
gap> f^-1=f;
false
gap> f=f^-1;
false
gap> f^-1;
[ 1, 20 ] -> [ 4, 2 ]
gap> f*f^-1;
<identity on [ 2, 4 ]>
gap> f^-1*f;   
<identity on [ 1, 20 ]>
gap> f^2;
<empty mapping>
gap> f:=[PartialPermNC([ 1, 2, 5 ], [ 1, 7, 4 ]), 
> PartialPermNC([ 1, 2, 3, 4, 8 ], [ 1, 7, 5, 6, 8 ]),
> PartialPermNC([ 1, 2, 4, 5, 7 ], [ 3, 4, 5, 2, 7 ]),
> PartialPermNC([ 1, 2, 3, 5, 6, 7 ], [ 5, 2, 6, 8, 1, 7 ])];;
gap> f[1]*f[2];
[ 1, 5 ] -> [ 1, 6 ]
gap> f[1]*f[2]*f[3]; 
[ 1 ] -> [ 3 ]
gap> f[4]*f[1]*f[3];
[ 1, 2, 6 ] -> [ 5, 7, 3 ]
gap> f[4]^4;
[ 2, 3, 7 ] -> [ 2, 8, 7 ]
gap> f[4]^5;
<identity on [ 2, 7 ]>
gap> f[4]^10;
<identity on [ 2, 7 ]>
gap> f[4]^-4; 
[ 2, 7, 8 ] -> [ 2, 7, 3 ]
gap> 

#
gap> file:=Concatenation(CitrusDir(), "/examples/inverse.citrus.gz");;
gap> f:=ReadCitrus(file, 1)[1];
<partial perm on 6326 pts>
gap> f^6*f^-6=LeftOne(f);
false
gap> f^6*f^-6=LeftOne(f^6);
true
gap> (f^-1)^6=f^-6;
true
gap> (f^-1)^16=f^-16;
true
gap> i:=7541;;
gap> i in DomPP(f);
false
gap> i^f;
fail
gap> i:=9912;;
gap> i in DomPP(f);
true
gap> i^f;
3545
gap> i^f in RanPP(f);
true
gap> (i^f)^(f^-1);
9912
gap> g:=f^-1;;
gap> ForAll(DomPP(f), i-> (i^f)^g=i);
true
gap> f:=RandomPartialPerm(10000);;
gap> g:=f^-1;;
gap> ForAll(DomPP(f), i-> (i^f)^g=i);
true
gap> ForAll(RanSetPP(f), i-> (i^g)^f=i);     
true
gap> f:=ReadCitrus(file, 3)[1];
<partial perm on 6317 pts>
gap> g:=f^-1;;
gap> ForAll(RanSetPP(f), i-> (i^g)^f=i);
true
gap> p:=(1,3,2)(4,5);;
gap> f:=PartialPermNC([ 1, 3, 4, 5, 6, 7, 9, 10 ], [ 6, 1, 4, 9, 2, 3, 7, 5 ]);
[ 1, 3, 4, 5, 6, 7, 9, 10 ] -> [ 6, 1, 4, 9, 2, 3, 7, 5 ]
gap> p*f;
[ 1, 2, 4, 5, 6, 7, 9, 10 ] -> [ 1, 6, 9, 4, 2, 3, 7, 5 ]
gap> f*p;
[ 1, 3, 4, 5, 6, 7, 9, 10 ] -> [ 6, 3, 5, 9, 1, 2, 7, 4 ]
gap> FixedPointsPP(f*p);
[ 3 ]
gap> f:=ReadCitrus(file, 4)[1];
<partial perm on 12675 pts>
gap> p:=Random(SymmetricGroup(65000));;
gap> f*p;
<partial perm on 12675 pts>
gap> p*f;  
<partial perm on 12675 pts>
gap> f*p*p^-1=f;
true
gap> p^2*p^-2*f*p*p^-1*f=f^2;
true
gap> f<f;
false
gap> f/p;
<partial perm on 12675 pts>
gap> f/p*p=f;
true
gap> f:=PartialPermNC([ 1, 2, 3, 5, 6, 7, 8, 10 ],
> [ 6, 2, 5, 4, 7, 8, 9, 10 ]);
[ 1, 2, 3, 5, 6, 7, 8, 10 ] -> [ 6, 2, 5, 4, 7, 8, 9, 10 ]
gap> g:=PartialPermNC([ 1, 2, 3, 5, 7, 8 ], [ 8, 10, 9, 1, 5, 6 ]);
[ 1, 2, 3, 5, 7, 8 ] -> [ 8, 10, 9, 1, 5, 6 ]
gap> f<g;
false
gap> g<f;
true
gap> f=g;
false
gap> f/g;
[ 1, 3, 7, 8, 10 ] -> [ 8, 7, 1, 3, 2 ]
gap> f/g=f*g^-1;
true
gap> f/g^-1=f*g;
true
gap> AsPermutation(f);
fail
gap> AsTransformation(f);
Transformation( [ 6, 2, 5, 11, 4, 7, 8, 9, 11, 10, 11 ] )
gap> AsTransformation(f, 12);
Transformation( [ 6, 2, 5, 12, 4, 7, 8, 9, 12, 10, 12, 12 ] )
gap> AsPartialPermNC(last);
[ 1, 2, 3, 5, 6, 7, 8, 10 ] -> [ 6, 2, 5, 4, 7, 8, 9, 10 ]
gap> f;
[ 1, 2, 3, 5, 6, 7, 8, 10 ] -> [ 6, 2, 5, 4, 7, 8, 9, 10 ]
gap> f:=PartialPermNC([ 1, 3, 4, 5, 6, 9 ], [ 9, 10, 5, 7, 2, 8 ]);;
gap> AsTransformation(f);
Transformation( [ 9, 11, 10, 5, 7, 2, 11, 11, 8, 11, 11 ] )
gap> AsTransformationNC(f);
Transformation( [ 9, 11, 10, 5, 7, 2, 11, 11, 8, 11, 11 ] )
gap> AsPartialPermNC(last)=f;
true
gap> DenseRangeList(f);
[ 9, 0, 10, 5, 7, 2, 0, 0, 8 ]
gap> g:=PartialPermNC([ 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 19 ],
> [ 3, 17, 12, 13, 6, 1, 2, 20, 9, 16, 4, 15, 8 ]);;
gap> DenseRangeList(f);
[ 9, 0, 10, 5, 7, 2, 0, 0, 8 ]
gap> DenseRangeList(g);
[ 3, 17, 12, 13, 6, 1, 2, 20, 0, 9, 16, 4, 15, 0, 0, 0, 0, 0, 8 ]
gap> DomPP(f)=RanSetPP(f^-1);
true
gap> RanSetPP(f)=DomPP(f^-1);   
true
gap> FixedPointsPP(f);
[  ]
gap> FixedPointsPP(f^-1);
[  ]
gap> FixedPointsPP(g);   
[  ]
gap> f:=RandomPartialPerm(100000);
Error, usage: can only create partial perms on at most 65535 pts,
gap> f;                                                               
[ 1, 3, 4, 5, 6, 9 ] -> [ 9, 10, 5, 7, 2, 8 ]
gap> InternalRepOfPartialPerm(f);
[ 9, 6, 2, 10, 1, 10, 9, 0, 10, 5, 7, 2, 0, 0, 8, 1, 3, 4, 5, 6, 9, 9, 10, 5, 
  7, 2, 8, 2, 5, 7, 8, 9, 10 ]
gap> InternalRepOfPartialPerm(g);
[ 19, 13, 1, 20, 1, 20, 3, 17, 12, 13, 6, 1, 2, 20, 0, 9, 16, 4, 15, 0, 0, 0, 
  0, 0, 8, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 19, 3, 17, 12, 13, 6, 1, 
  2, 20, 9, 16, 4, 15, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
gap> RanSetPP(g);
[ 1, 2, 3, 4, 6, 8, 9, 12, 13, 15, 16, 17, 20 ]
gap> InternalRepOfPartialPerm(g);
[ 19, 13, 1, 20, 1, 20, 3, 17, 12, 13, 6, 1, 2, 20, 0, 9, 16, 4, 15, 0, 0, 0, 
  0, 0, 8, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 19, 3, 17, 12, 13, 6, 1, 
  2, 20, 9, 16, 4, 15, 8, 1, 2, 3, 4, 6, 8, 9, 12, 13, 15, 16, 17, 20 ]
gap> OnSets(DomPP(f), f)=RanPP(f);
false
gap> OnSets(DomPP(f), f)=RanSetPP(f);
true
gap> OnTuples(DomPP(f), f)=RanSetPP(f);
false
gap> OnTuples(DomPP(f), f)=RanPP(f);   
true
gap> OnTuples(RanPP(f), f^-1)=DomPP(f);
true
gap> OnSets(RanSetPP(f), f^-1)=DomPP(f);
true
gap> OnSets(RanSetPP(g), g^-1)=DomPP(g);
true
gap> OnTuples(RanPP(g), g^-1)=DomPP(f);
false
gap> OnTuples(RanPP(g), g^-1)=DomPP(g);
true
gap> f:=ReadCitrus(file, 5)[1];
<partial perm on 627 pts>
gap> f[1]; #max dom
1000
gap> f[6]; #max dom ran
1000
gap> f[4]; #max ran
999
gap> f[3]; #min ran
1
gap> f[5]; #min dom ran
1
gap> f[f[1]+7]; # min dom
1
gap> f:=ReadCitrus(file, 6)[1];   
<partial perm on 31542 pts>
gap> f[6];
50000
gap> f[4];   
50000
gap> f[f[1]+7];
1
gap> f[1];
49997
gap> f[5];
1
gap> f[3];
4
gap> f[50007];
4

#
gap> f:=PartialPermNC([ 1, 2, 3, 4, 5, 6, 7, 11, 12, 13, 14, 19 ],
> [ 5, 13, 7, 6, 10, 15, 9, 14, 4, 20, 19, 2 ]);
[ 1, 2, 3, 4, 5, 6, 7, 11, 12, 13, 14, 19 ] -> 
[ 5, 13, 7, 6, 10, 15, 9, 14, 4, 20, 19, 2 ]
gap> One(f);
<identity on [ 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 19, 20 ]>
gap> One(f)*f=f*One(f);
true
gap> One(f)*f=f;
true
gap> OnIntegerSetsWithPartialPerm([1..100], f);
[ 2, 4, 5, 6, 7, 9, 10, 13, 14, 15, 19, 20 ]
gap> f:=PartialPermNC([2039, 2149, 21443, 13431, 1, 2, 3]);
[ 1 .. 7 ] -> [ 2039, 2149, 21443, 13431, 1, 2, 3 ]
gap> OnIntegerSetsWithPartialPerm([1..10000], f); 
[ 1, 2, 3, 2039, 2149, 13431, 21443 ]
gap> RanPP(f);
[ 2039, 2149, 21443, 13431, 1, 2, 3 ]
gap> RangeOfPartialPerm(f);
[ 2039, 2149, 21443, 13431, 1, 2, 3 ]
gap> RangeSetOfPartialPerm(f);
[ 1, 2, 3, 2039, 2149, 13431, 21443 ]
gap> RanSetPP(f);
[ 1, 2, 3, 2039, 2149, 13431, 21443 ]
gap> RankOfPartialPerm(f);
7
gap> f:=ReadCitrus(file, 4)[1];
<partial perm on 12675 pts>
gap> RestrictedPP(f, [1..100]);
<partial perm on 100 pts>
gap> g:=last;
<partial perm on 100 pts>
gap> DomPP(g);
[ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 
  22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 
  41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 
  60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 
  79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 
  98, 99, 100 ]
gap> RanSetPP(g);
[ 308, 382, 730, 999, 1469, 1488, 1628, 1715, 1759, 1908, 2062, 2149, 2192, 
  2339, 2427, 2778, 2971, 3137, 3364, 3369, 3412, 3622, 3811, 4445, 4522, 
  4874, 5058, 5191, 5706, 5856, 6322, 6532, 6759, 7182, 7282, 7590, 7989, 
  8151, 8357, 8362, 8562, 8611, 8795, 8829, 9042, 9371, 9760, 9946, 10435, 
  10601, 10689, 10701, 10743, 11117, 11192, 11284, 11743, 11798, 12164, 
  12190, 12263, 12323, 12421, 13061, 13110, 13339, 13344, 13529, 13542, 
  13869, 13974, 14325, 14326, 14410, 14904, 14955, 15581, 15782, 15977, 
  16047, 16189, 16381, 16525, 16754, 17197, 17458, 17717, 17770, 17835, 
  17895, 17957, 18166, 18456, 18483, 18788, 19090, 19456, 19522, 19745, 19795 
 ]
gap> g^2;
<empty mapping>
gap> g^-1;
<partial perm on 100 pts>
gap> g*g^-1=LeftOne(g);
true
gap> g^-1*g=RightOne(g);  
true
gap> RanPP(g);
[ 2192, 11284, 13542, 3622, 10601, 18166, 16754, 1628, 1469, 9042, 5058, 
  14904, 1488, 17895, 16525, 7182, 19745, 9371, 18788, 12164, 13110, 999, 
  8151, 4522, 3137, 13869, 12323, 10701, 11743, 8795, 13061, 1715, 13529, 
  17835, 6759, 2149, 18456, 8357, 17717, 19522, 17458, 10435, 4445, 15977, 
  7282, 10689, 8362, 19090, 14326, 16047, 16381, 12421, 8829, 17957, 730, 
  17770, 4874, 382, 6532, 2778, 9946, 2062, 1759, 12263, 1908, 14325, 10743, 
  2427, 15581, 6322, 13974, 9760, 15782, 5191, 8562, 14410, 11192, 3412, 
  3369, 13344, 16189, 14955, 2971, 19795, 2339, 7590, 7989, 13339, 11117, 
  17197, 18483, 308, 5856, 12190, 11798, 3364, 3811, 5706, 19456, 8611 ]
gap> f<f^-1;                                             
true
gap> g<g^-1; 
true
gap> g^-1<g;   
false
gap> NaturalLeqPP(g, f);
true
gap> NaturalLeqPP(f, g);
false
gap> f:=PartialPermNC(
> [ 9, 45, 53, 15, 42, 97, 71, 66, 7, 88, 6, 98, 95, 36, 20, 59, 94, 0, 81, 70,
>  65, 29, 78, 37, 74, 48, 52, 4, 32, 93, 18, 13, 55, 0, 49, 0, 99, 46, 35,
>  84, 0, 79, 80, 0, 85, 0, 89, 0, 0, 27, 0, 0, 0, 73, 33, 0, 77, 69, 41, 0,
>  63, 0, 0, 0, 75, 56, 0, 0, 0, 90, 64, 0, 0, 0, 100, 0, 0, 3, 0, 0, 2, 26,
>  11, 39, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 10, 61, 25 ]);
<partial perm on 62 pts>
gap> OnPointsPP(50, f);
27
gap> OnPointsPP(27, f);
52
gap> OnPointsPP(52, f);
fail
gap> 50^f;
27
gap> 27^f;
52

#
gap> f:=PartialPermNC( [ 1, 2, 3, 6, 8, 10 ], [ 2, 6, 7, 9, 1, 5 ] );;
gap> f<f;
false

#
gap> f:=PartialPermNC([]);;
gap> f<f;
false

#
gap> S:=InverseSemigroup([ PartialPermNC( [ 1, 3 ], [ 1, 3 ] ),
> PartialPermNC( [ 1, 2 ], [ 3, 2 ] ) ] );;
gap> Size(S);
11
gap> NaturalPartialOrder(S);
[ [  ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1, 2, 4, 5 ], [ 1, 3, 4, 5 ], 
  [ 1, 2, 6 ], [ 1, 4, 5 ], [ 1, 4, 6 ] ]
gap> NaturalLeqPartialPerm(Elements(S)[4], Elements(S)[10]);
true
gap> NaturalLeqPartialPerm(Elements(S)[4], Elements(S)[9]);
false
gap> List(Elements(S), x-> NaturalLeqPartialPerm(Elements(S)[1], x));
[ true, true, true, true, true, true, true, true, true, true, true ]
gap> List(Elements(S), x-> NaturalLeqPartialPerm(Elements(S)[2], x));
[ false, true, false, false, false, false, true, false, true, false, false ]
gap> PositionsProperty([1..11], x-> NaturalLeqPartialPerm(Elements(S)[4], 
> Elements(S)[x]));
[ 4, 7, 8, 10, 11 ]
gap> PositionsProperty([1..11], x-> NaturalLeqPartialPerm(Elements(S)[2],
> Elements(S)[x]));
[ 2, 7, 9 ]

#
gap> s:=InverseSemigroup( 
> PartialPermNC( [ 1, 2, 3, 6, 8, 10 ], [ 2, 6, 7, 9, 1, 5 ] ),
> PartialPermNC( [ 1, 2, 4, 5, 8, 10 ], [ 6, 2, 7, 8, 10, 4 ] ),
> PartialPermNC( [ 1, 2, 4, 6, 8, 9 ], [ 7, 10, 1, 9, 4, 2 ] ),
> PartialPermNC( [ 1, 2, 4, 7, 8, 9 ], [ 10, 7, 8, 5, 9, 1 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5, 6, 8 ], [ 6, 2, 8, 4, 7, 5, 3 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5, 8, 10 ], [ 3, 1, 4, 2, 5, 6, 7 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5, 8, 10 ], [ 7, 1, 4, 3, 2, 6, 5 ] ),
> PartialPermNC( [ 1, 2, 3, 5, 6, 7, 8 ], [ 5, 9, 10, 6, 3, 8, 4 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 10 ], [ 4, 7, 6, 9, 10, 1, 3, 2 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 6, 7, 8, 10 ], [ 3, 8, 1, 9, 4, 10, 5, 6 ] ) );;
gap> NrMovedPoints(s);
10
gap> MovedPoints(s);
[ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
gap> LargestMovedPoint(s);
10
gap> SmallestMovedPoint(s); 
1
gap> f:=PartialPermNC([ 9 ], [ 3 ]);;
gap> NrMovedPoints(f);
1
gap> MovedPoints(f);
[ 9 ]
gap> SmallestMovedPoint(f);
9
gap> LargestMovedPoint(f);
9
gap> coll:=[ PartialPermNC([]), PartialPermNC([ 2, 3, 6 ], [ 2, 4, 5 ]), 
> PartialPermNC([ 9, 10 ], [ 7, 8 ]), PartialPermNC([ 6, 9 ], [ 5, 4 ]) ];;
gap> NrMovedPoints(coll);
4
gap> MovedPoints(coll);
[ 3, 6, 9, 10 ]
gap> LargestMovedPoint(coll);
10
gap> SmallestMovedPoint(coll);
3

#
gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;

#
gap> STOP_TEST("Citrus package: pperm.tst", 10000);
