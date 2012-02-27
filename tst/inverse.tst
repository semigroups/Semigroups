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


gap> gens:=[PartialPermNC( [ 1, 2, 4 ], [ 1, 5, 2 ] ),
> PartialPermNC( [ 1, 2, 3 ], [ 2, 3, 5 ] ),
> PartialPermNC( [ 1, 3, 4 ], [ 2, 5, 4 ] ),
> PartialPermNC( [ 1, 2, 4 ], [ 3, 1, 2 ] ),
> PartialPermNC( [ 1, 2, 3 ], [ 3, 1, 4 ] ),
> PartialPermNC( [ 1, 2, 4 ], [ 3, 5, 2 ] ),
> PartialPermNC( [ 1, 2, 3, 4 ], [ 4, 1, 5, 2 ] ),
> PartialPermNC( [ 1, 2, 4, 5 ], [ 4, 3, 5, 2 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5 ], [ 5, 2, 4, 3, 1 ] ),
> PartialPermNC( [ 1, 3, 5 ], [ 5, 4, 1 ] )];;
gap> s:=InverseSemigroup(gens);                 
<inverse semigroup with 10 generators>
gap> NrRClasses(s);
30
gap> RClassReps(s);
[ <identity on [ 1, 2, 5 ]>, [ 2, 3, 5 ] -> [ 2, 1, 5 ], 
  [ 2, 4, 5 ] -> [ 2, 1, 5 ], [ 1 .. 3 ] -> [ 5, 2, 1 ], 
  [ 1, 3, 4 ] -> [ 2, 5, 1 ], [ 1, 4, 5 ] -> [ 2, 5, 1 ], 
  [ 1, 2, 4 ] -> [ 1, 5, 2 ], [ 1, 3 .. 5 ] -> [ 1, 5, 2 ], 
  [ 2 .. 4 ] -> [ 5, 2, 1 ], [ 3 .. 5 ] -> [ 5, 1, 2 ], 
  <identity on [ 1, 2, 4, 5 ]>, [ 2 .. 5 ] -> [ 5, 2, 1, 4 ], 
  [ 1 .. 4 ] -> [ 4, 1, 5, 2 ], [ 1, 2, 3, 5 ] -> [ 5, 2, 4, 1 ], 
  <identity on [ 1, 5 ]>, [ 2, 3 ] -> [ 1, 5 ], [ 1, 3 ] -> [ 5, 1 ], 
  [ 3, 5 ] -> [ 5, 1 ], [ 1, 4 ] -> [ 5, 1 ], [ 2, 4 ] -> [ 5, 1 ], 
  [ 4, 5 ] -> [ 5, 1 ], [ 3, 4 ] -> [ 5, 1 ], [ 2, 5 ] -> [ 1, 5 ], 
  [ 1, 2 ] -> [ 1, 5 ], <identity on [ 2 ]>, [ 5 ] -> [ 2 ], [ 1 ] -> [ 2 ], 
  [ 3 ] -> [ 2 ], [ 4 ] -> [ 2 ], <empty mapping> ]
gap> List(last, DomPP);
[ [ 1, 2, 5 ], [ 2, 3, 5 ], [ 2, 4, 5 ], [ 1, 2, 3 ], [ 1, 3, 4 ], 
  [ 1, 4, 5 ], [ 1, 2, 4 ], [ 1, 3, 5 ], [ 2, 3, 4 ], [ 3, 4, 5 ], 
  [ 1, 2, 4, 5 ], [ 2, 3, 4, 5 ], [ 1, 2, 3, 4 ], [ 1, 2, 3, 5 ], [ 1, 5 ], 
  [ 2, 3 ], [ 1, 3 ], [ 3, 5 ], [ 1, 4 ], [ 2, 4 ], [ 4, 5 ], [ 3, 4 ], 
  [ 2, 5 ], [ 1, 2 ], [ 2 ], [ 5 ], [ 1 ], [ 3 ], [ 4 ], [  ] ]
gap> IsDuplicateFreeList(last);
true

gap> s:=InverseSemigroup(PartialPermNC([ 1, 2 ], [ 1, 2 ]),
> PartialPermNC([ 1, 2 ], [ 1, 3 ]));;
gap> GreensHClasses(s);
[ {<identity on [ 1, 2 ]>}, {[ 1, 2 ] -> [ 1, 3 ]}, {[ 1, 3 ] -> [ 1, 2 ]},
  {<identity on [ 1, 3 ]>}, {<identity on [ 1 ]>} ]
gap> s:=InverseSemigroup(Generators(s));;
gap> HClassReps(s);
[ <identity on [ 1, 2 ]>, [ 1, 2 ] -> [ 1, 3 ], [ 1, 3 ] -> [ 1, 2 ],
  <identity on [ 1, 3 ]>, <identity on [ 1 ]> ]
gap> GreensHClasses(s);
[ {<identity on [ 1, 2 ]>}, {[ 1, 2 ] -> [ 1, 3 ]}, {[ 1, 3 ] -> [ 1, 2 ]},
  {<identity on [ 1, 3 ]>}, {<identity on [ 1 ]>} ]

gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;

gap> STOP_TEST("Citrus package: inverse.tst", 10000);
