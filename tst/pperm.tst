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

gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoCitrus:=InfoLevel(InfoCitrus);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoCitrus, 0);

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

gap> f:=ReadCitrus("pkg/citrus/examples/inverse.citrus.gz", 1)[1];
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
gap> ForAll(DomPP(f), i-> (i^f)^(f^-1)=i);
true
gap> f:=RandomPartialPerm(10000);;
gap> ForAll(DomPP(f), i-> (i^f)^(f^-1)=i);
true
gap> ForAll(RanSetPP(f), i-> (i^(f^-1))^f=i);     
true
gap> f:=ReadCitrus("pkg/citrus/examples/inverse.citrus.gz", 3)[1];
<partial perm on 6317 pts>
gap> ForAll(RanSetPP(f), i-> (i^(f^-1))^f=i);
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
gap> f:=ReadCitrus("pkg/citrus/examples/inverse.citrus.gz", 4)[1];
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
true
gap> g<f;
false
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
gap> f:=ReadCitrus("pkg/citrus/examples/inverse.citrus.gz", 5)[1];
<partial perm on 627 pts>
gap> MaxDomPP(f);
1000
gap> MaxDomRanPP(f);
1000
gap> MaxRanPP(f);
999
gap> MinRanPP(f);
1
gap> MinDomRanPP(f);
1
gap> MinRanPP(f);
1
gap> MinDomPP(f);
1
gap> f:=ReadCitrus("pkg/citrus/examples/inverse.citrus.gz", 6)[1];   
<partial perm on 31542 pts>
gap> MaxDomRanPP(f);
50000
gap> MaxRanPP(f);   
50000
gap> MinDomPP(f); #something wrong here!
1
gap> f[1];
49997
gap> MinDomRanPP(f);
1
gap> MinRanPP(f);
4
gap> f[50007];
4

gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;

gap> STOP_TEST("Citrus package: pperm.tst", 10000);
