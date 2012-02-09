#############################################################################
##
#W  inverse.tst
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

gap> START_TEST("Citrus package: inverse.tst");
gap> LoadPackage("citrus", false);;

gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoCitrus:=InfoLevel(InfoCitrus);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoCitrus, 0);

gap> f:=PartialPermNC([0,1,0,20]);
[ 2, 4 ] -> [ 1, 20 ]
gap> InternalRepOfPartialPerm(f);
[ 4, 2, 1, 20, 1, 20, 0, 1, 0, 20, 2, 4, 1, 20 ]
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
false
gap> f<f^-1;
true
gap> f:=PartialPermNC([2,4], [20,1]);
[ 2, 4 ] -> [ 20, 1 ]
gap> f^-1=f;
false
gap> f=f^-1;
false
gap> f^01;
[ 2, 4 ] -> [ 20, 1 ]
gap> f^-1;
[ 1, 20 ] -> [ 4, 2 ]
gap> f*f^-1;
[ 2, 4 ] -> [ 2, 4 ]
gap> f^-1*f;   
[ 1, 20 ] -> [ 1, 20 ]
gap> f^2;
<empty mapping>
gap> f:=ReadCitrus("pkg/citrus/examples/inverse.citrus.gz", 1);
[ [ 1, 2, 5 ] -> [ 1, 7, 4 ], [ 1, 2, 3, 4, 8 ] -> [ 1, 7, 5, 6, 8 ], 
  [ 1, 2, 4, 5, 7 ] -> [ 3, 4, 5, 2, 7 ], 
  [ 1, 2, 3, 5, 6, 7 ] -> [ 5, 2, 6, 8, 1, 7 ] ]
gap> f[1]*f[2];
[ 1, 5 ] -> [ 1, 6 ]
gap> f[1]*f[2]*f[3]; 
[ 1 ] -> [ 3 ]
gap> f[4]*f[1]*f[3];
[ 1, 2, 6 ] -> [ 5, 7, 3 ]
gap> f[4]^4;
[ 2, 3, 7 ] -> [ 2, 8, 7 ]
gap> f[4]^5;
[ 2, 7 ] -> [ 2, 7 ]
gap> f[4]^10;
[ 2, 7 ] -> [ 2, 7 ]
gap> f[4]^-4; 
[ 2, 7, 8 ] -> [ 2, 7, 3 ]
gap> 

gap> s:=InverseSemigroup(PartialPermNC([ 1, 2 ], [ 1, 2 ]),
> PartialPermNC([ 1, 2 ], [ 1, 3 ]));;
gap> GreensHClasses(s);
[ {[ 1, 2 ] -> [ 1, 2 ]}, {[ 1, 2 ] -> [ 1, 3 ]}, {[ 1, 3 ] -> [ 1, 2 ]}, 
  {[ 1, 3 ] -> [ 1, 3 ]}, {[ 1 ] -> [ 1 ]} ]
gap> s:=InverseSemigroup(Generators(s));;
gap> HClassReps(s);
[ [ 1, 2 ] -> [ 1, 2 ], [ 1, 2 ] -> [ 1, 3 ], [ 1, 3 ] -> [ 1, 2 ], 
  [ 1, 3 ] -> [ 1, 3 ], [ 1 ] -> [ 1 ] ]
gap> GreensHClasses(s);
[ {[ 1, 2 ] -> [ 1, 2 ]}, {[ 1, 2 ] -> [ 1, 3 ]}, {[ 1, 3 ] -> [ 1, 2 ]}, 
  {[ 1, 3 ] -> [ 1, 3 ]}, {[ 1 ] -> [ 1 ]} ]

gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;

gap> STOP_TEST("Citrus package: inverse.tst", 10000);
