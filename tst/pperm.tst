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



gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;

gap> STOP_TEST("Citrus package: pperm.tst", 10000);
