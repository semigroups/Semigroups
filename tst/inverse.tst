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
gap> Size(s);
860
gap> NrRClasses(s);
31
gap> RClassReps(s);
[ <identity on [ 1 .. 5 ]>, <identity on [ 1, 2, 5 ]>, 
  [ 2, 3, 5 ] -> [ 2, 1, 5 ], [ 2, 4, 5 ] -> [ 2, 1, 5 ], 
  [ 1 .. 3 ] -> [ 5, 2, 1 ], [ 1, 3, 4 ] -> [ 2, 5, 1 ], 
  [ 1, 4, 5 ] -> [ 2, 5, 1 ], [ 1, 2, 4 ] -> [ 1, 5, 2 ], 
  [ 1, 3 .. 5 ] -> [ 1, 5, 2 ], [ 2 .. 4 ] -> [ 5, 2, 1 ], 
  [ 3 .. 5 ] -> [ 5, 1, 2 ], <identity on [ 1, 2, 4, 5 ]>, 
  [ 2 .. 5 ] -> [ 5, 2, 1, 4 ], [ 1 .. 4 ] -> [ 4, 1, 5, 2 ], 
  [ 1, 2, 3, 5 ] -> [ 5, 2, 4, 1 ], <identity on [ 1, 5 ]>, 
  [ 2, 3 ] -> [ 1, 5 ], [ 1, 3 ] -> [ 5, 1 ], [ 3, 5 ] -> [ 5, 1 ], 
  [ 1, 4 ] -> [ 5, 1 ], [ 2, 4 ] -> [ 5, 1 ], [ 4, 5 ] -> [ 5, 1 ], 
  [ 3, 4 ] -> [ 5, 1 ], [ 2, 5 ] -> [ 1, 5 ], [ 1, 2 ] -> [ 1, 5 ], 
  <identity on [ 2 ]>, [ 5 ] -> [ 2 ], [ 1 ] -> [ 2 ], [ 3 ] -> [ 2 ], 
  [ 4 ] -> [ 2 ], <empty mapping> ]
gap> List(last, DomPP);
[ [ 1, 2, 3, 4, 5 ], [ 1, 2, 5 ], [ 2, 3, 5 ], [ 2, 4, 5 ], [ 1, 2, 3 ], 
  [ 1, 3, 4 ], [ 1, 4, 5 ], [ 1, 2, 4 ], [ 1, 3, 5 ], [ 2, 3, 4 ], 
  [ 3, 4, 5 ], [ 1, 2, 4, 5 ], [ 2, 3, 4, 5 ], [ 1, 2, 3, 4 ], 
  [ 1, 2, 3, 5 ], [ 1, 5 ], [ 2, 3 ], [ 1, 3 ], [ 3, 5 ], [ 1, 4 ], [ 2, 4 ], 
  [ 4, 5 ], [ 3, 4 ], [ 2, 5 ], [ 1, 2 ], [ 2 ], [ 5 ], [ 1 ], [ 3 ], [ 4 ], 
  [  ] ]
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

gap> gens:=[PartialPermNC( [ 1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19 ], 
> [ 9, 18, 20, 11, 5, 16, 8, 19, 14, 13, 1 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 14, 16, 18, 19, 20 ], 
> [ 13, 1, 8, 5, 4, 14, 11, 12, 9, 20, 2, 18, 7, 3, 19 ] )];;
gap> s:=InverseSemigroup(gens);;
gap> d:=DClass(s, Generators(s)[1]);
{<identity on [ 1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19 ]>}
gap> Size(s);
60880
gap> h:=HClass(d, Generators(s)[1]);
{[ 1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19 ] ->
[ 9, 18, 20, 11, 5, 16, 8, 19, 14, 13, 1 ]}
gap> Generators(s)[1] in h;
true
gap> DClassOfHClass(h)=d;
true
gap> DClassOfHClass(h);  
{<identity on [ 1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19 ]>}
gap> r:=RClassOfHClass(h);
{<identity on [ 1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19 ]>}
gap> ForAll(h, x-> x in r);   
true
gap> l:=LClass(h);   
{[ 1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19 ] ->
[ 9, 18, 20, 11, 5, 16, 8, 19, 14, 13, 1 ]}
gap> ForAll(h, x-> x in l);
true
gap> Representative(l) in l;
true
gap> IsGreensLClass(l);
true
gap> DClassOfLClass(l)=d;
true
gap> DClassOfRClass(r)=d;
true
gap> S:=InverseSemigroup(PartialPermNC([ 1, 2, 3, 6, 8, 10 ], 
> [ 2, 6, 7, 9, 1, 5 ]), PartialPermNC([ 1, 2, 3, 4, 6, 7, 8, 10 ], 
> [ 3, 8, 1, 9, 4, 10, 5, 6 ]));
<inverse semigroup with 2 generators>
gap> f:=Generators(S)[1];
[ 1, 2, 3, 6, 8, 10 ] -> [ 2, 6, 7, 9, 1, 5 ]
gap> h:=HClass(S, f);
{[ 1, 2, 3, 6, 8, 10 ] -> [ 2, 6, 7, 9, 1, 5 ]}
gap> IsGreensHClass(h);
true
gap> RClassOfHClass(h);
{<identity on [ 1, 2, 3, 6, 8, 10 ]>}
gap> LClassOfHClass(h);
{[ 1, 2, 3, 6, 8, 10 ] -> [ 2, 6, 7, 9, 1, 5 ]}
gap> r:=RClassOfHClass(h);
{<identity on [ 1, 2, 3, 6, 8, 10 ]>}
gap> l:=LClass(h);
{[ 1, 2, 3, 6, 8, 10 ] -> [ 2, 6, 7, 9, 1, 5 ]}
gap> DClass(r)=DClass(l);
true
gap> DClass(h)=DClass(l);
true
gap> f:=PartialPermNC([ 1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19 ], 
> [ 9, 18, 20, 11, 5, 16, 8, 19, 14, 13, 1 ]);;
gap> h:=HClass(s, f);
{[ 1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19 ] -> 
[ 9, 18, 20, 11, 5, 16, 8, 19, 14, 13, 1 ]}
gap> ForAll(h, x-> x in RClassOfHClass(h));
true
gap> Size(h);
1
gap> IsGroupHClass(h);
false
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until Size(h)>1 or IsDoneIterator(iter);
gap> h;
{<identity on [ 8, 20 ]>}
gap> IsDoneIterator(iter);
false
gap> ForAll(HClasses(s), IsTrivial);
false
gap> First(HClasses(s), x-> not IsTrivial(x));
{<identity on [ 8, 20 ]>}
gap> Size(last);
2
gap> iter:=IteratorOfHClasses(s);                                          
<iterator>
gap> repeat h:=NextIterator(iter); until Size(h)>1 or IsDoneIterator(iter);
gap> h;
{<identity on [ 8, 20 ]>}
gap> IsDoneIterator(iter);
false
gap> s:=InverseSemigroup(Generators(s));
<inverse semigroup with 2 generators>
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until Size(h)>1 or IsDoneIterator(iter);
gap> h;
{<identity on [ 8, 20 ]>}
gap> Size(h);
2
gap> IsDoneIterator(iter);
false
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> h:=NextIterator(iter);
{<identity on [ 1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20 ]>}
gap> Size(h);
1
gap> h:=NextIterator(iter);
{[ 1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20 ] -> 
[ 19, 6, 8, 1, 5, 16, 12, 7, 2, 11, 3 ]}
gap> Size(h);
1
gap> h:=NextIterator(iter);
{[ 1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19 ] ->
[ 9, 18, 20, 11, 5, 16, 8, 19, 14, 13, 1 ]}
gap> Size(h);
1
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> Size(h);
1
gap> f:=PartialPermNC([8,20], [8,20]);
<identity on [ 8, 20 ]>
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until Size(h)>1 or IsDoneIterator(iter);
gap> h;
{<identity on [ 8, 20 ]>}
gap> f in h;
true
gap> f in s;
true
gap> hh:=HClass(s, f);
{<identity on [ 8, 20 ]>}
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until h=hh or IsDoneIterator(iter);     
gap> h;
{<identity on [ 8, 20 ]>}
gap> IsDoneIterator(iter);
false
gap> NrHClasses(s);
40715
gap> i:=0;                                                         
0
gap> iter:=IteratorOfHClasses(s);;
gap> repeat i:=i+1; NextIterator(iter); until IsDoneIterator(iter);
gap> i;
40715
gap> IsGreensHClass(h);
true
gap> IsGreensHClass(hh);
true
gap> f in HClassReps(s);
true
gap> Representative(h) in HClassReps(s);
true
gap> iter:=IteratorOfHClassReps(s);
<iterator>
gap> s:=InverseSemigroup(Generators(s));
<inverse semigroup with 2 generators>
gap> iter:=IteratorOfHClassReps(s);
<iterator of H-class reps>
gap> for f in iter do if not f in s then Print("yikes"); break; fi; od;
#gap> time;
#329
gap> iter:=IteratorOfHClasses(s);                                  
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until Size(h)>1 or IsDoneIterator(iter);
gap> h;
{<identity on [ 8, 20 ]>}
gap> Size(h);
2
gap> Size(DClass(h));
40328
gap> RClass(h);
{<identity on [ 8, 20 ]>}
gap> Size(last);
284
gap> 284^2;
80656
gap> d:=DClass(h);
{<identity on [ 8, 20 ]>}
gap> IsGreensDClass(d);
true
gap> d;
{<identity on [ 8, 20 ]>}
gap> Size(DClass(h))=Size(RClass(h))^2/2;
true

gap> s:=Semigroup(ReadCitrus("pkg/citrus/examples/graph7c.citrus.gz", 600));
<semigroup with 2 generators>
gap> iso:=IsomorphismPartialPermSemigroup(s);;
gap> inv:=InverseGeneralMapping(iso);;
gap> f:=Transformation( [ 1, 7, 3, 4, 5, 6, 7 ] );;
gap> f^iso;
<identity on [ 1, 3, 4, 5, 6, 7 ]>
gap> (f^iso)^inv;
Transformation( [ 1, 7, 3, 4, 5, 6, 7 ] )
gap> ForAll(s, f-> (f^iso)^inv=f);
true

gap> s:=Semigroup( Transformation( [ 2, 5, 1, 7, 3, 7, 7 ] ), 
> Transformation( [ 3, 6, 5, 7, 2, 1, 7 ] ) );;
gap> iso:=IsomorphismPartialPermSemigroup(s);;
gap> inv:=InverseGeneralMapping(iso);;
gap> f:=Transformation( [ 7, 1, 7, 7, 7, 7, 7 ] );;
gap> f^iso;
[ 2, 7 ] -> [ 1, 7 ]
gap> ForAll(s, f-> (f^iso)^inv=f);
true
gap> Size(Range(iso));
631
gap> ForAll(s, f-> f^iso in Range(iso));
true
gap> Size(s);
631

gap> s:=InverseSemigroup( PartialPermNC( [ 1, 2, 3 ], [ 2, 4, 1 ] ),
> PartialPermNC( [ 1, 3, 4 ], [ 3, 4, 1 ] ) );;
gap> GreensDClasses(s);
[ {<identity on [ 1, 2, 4 ]>}, {<identity on [ 1, 3, 4 ]>}, 
  {<identity on [ 2, 4 ]>}, {<identity on [ 4 ]>}, {<empty mapping>} ]
gap> GreensHClasses(s);
[ {<identity on [ 1, 2, 4 ]>}, {[ 1, 2, 4 ] -> [ 3, 1, 2 ]}, 
  {[ 1 .. 3 ] -> [ 2, 4, 1 ]}, {<identity on [ 1 .. 3 ]>}, 
  {<identity on [ 1, 3, 4 ]>}, {<identity on [ 2, 4 ]>}, 
  {[ 2, 4 ] -> [ 3, 1 ]}, {[ 2, 4 ] -> [ 1, 2 ]}, {[ 2, 4 ] -> [ 3, 2 ]}, 
  {[ 2, 4 ] -> [ 4, 3 ]}, {[ 2, 4 ] -> [ 1, 4 ]}, {[ 1, 3 ] -> [ 4, 2 ]}, 
  {<identity on [ 1, 3 ]>}, {[ 1, 3 ] -> [ 2, 1 ]}, {[ 1, 3 ] -> [ 2, 3 ]}, 
  {[ 1, 3 ] -> [ 3, 4 ]}, {[ 1, 3 ] -> [ 4, 1 ]}, {[ 1, 2 ] -> [ 2, 4 ]}, 
  {[ 1, 2 ] -> [ 3, 1 ]}, {<identity on [ 1, 2 ]>}, {[ 1, 2 ] -> [ 3, 2 ]}, 
  {[ 1, 2 ] -> [ 4, 3 ]}, {[ 1, 2 ] -> [ 1, 4 ]}, {[ 2, 3 ] -> [ 4, 2 ]}, 
  {[ 2, 3 ] -> [ 1, 3 ]}, {[ 2, 3 ] -> [ 2, 1 ]}, {<identity on [ 2, 3 ]>}, 
  {[ 2, 3 ] -> [ 3, 4 ]}, {[ 2, 3 ] -> [ 4, 1 ]}, {[ 3, 4 ] -> [ 4, 2 ]}, 
  {[ 3, 4 ] -> [ 1, 3 ]}, {[ 3, 4 ] -> [ 2, 1 ]}, {[ 3, 4 ] -> [ 2, 3 ]}, 
  {<identity on [ 3, 4 ]>}, {[ 3, 4 ] -> [ 4, 1 ]}, {[ 1, 4 ] -> [ 2, 4 ]}, 
  {[ 1, 4 ] -> [ 3, 1 ]}, {[ 1, 4 ] -> [ 1, 2 ]}, {[ 1, 4 ] -> [ 3, 2 ]}, 
  {[ 1, 4 ] -> [ 4, 3 ]}, {<identity on [ 1, 4 ]>}, {<identity on [ 4 ]>}, 
  {[ 4 ] -> [ 1 ]}, {[ 4 ] -> [ 3 ]}, {[ 4 ] -> [ 2 ]}, {[ 1 ] -> [ 4 ]}, 
  {<identity on [ 1 ]>}, {[ 1 ] -> [ 3 ]}, {[ 1 ] -> [ 2 ]}, {[ 3 ] -> [ 4 ]},
  {[ 3 ] -> [ 1 ]}, {<identity on [ 3 ]>}, {[ 3 ] -> [ 2 ]}, {[ 2 ] -> [ 4 ]},
  {[ 2 ] -> [ 1 ]}, {[ 2 ] -> [ 3 ]}, {<identity on [ 2 ]>}, 
  {<empty mapping>} ]
gap> IsDuplicateFree(last);
true
gap> GreensLClasses(s);
[ {<identity on [ 1, 2, 4 ]>}, {[ 1, 2, 4 ] -> [ 3, 1, 2 ]}, 
  {<identity on [ 1, 3, 4 ]>}, {<identity on [ 2, 4 ]>}, 
  {[ 2, 4 ] -> [ 3, 1 ]}, {[ 2, 4 ] -> [ 1, 2 ]}, {[ 2, 4 ] -> [ 3, 2 ]}, 
  {[ 2, 4 ] -> [ 4, 3 ]}, {[ 2, 4 ] -> [ 1, 4 ]}, {<identity on [ 4 ]>}, 
  {[ 4 ] -> [ 1 ]}, {[ 4 ] -> [ 3 ]}, {[ 4 ] -> [ 2 ]}, {<empty mapping>} ]
gap> GreensRClasses(s);
[ {<identity on [ 1, 2, 4 ]>}, {[ 1 .. 3 ] -> [ 2, 4, 1 ]}, 
  {<identity on [ 1, 3, 4 ]>}, {<identity on [ 2, 4 ]>}, 
  {[ 1, 3 ] -> [ 4, 2 ]}, {[ 1, 2 ] -> [ 2, 4 ]}, {[ 2, 3 ] -> [ 4, 2 ]}, 
  {[ 3, 4 ] -> [ 4, 2 ]}, {[ 1, 4 ] -> [ 2, 4 ]}, {<identity on [ 4 ]>}, 
  {[ 1 ] -> [ 4 ]}, {[ 3 ] -> [ 4 ]}, {[ 2 ] -> [ 4 ]}, {<empty mapping>} ]
gap> D:=GreensDClasses(s)[2];
{<identity on [ 1, 3, 4 ]>}
gap> GreensLClasses(D);
[ {<identity on [ 1, 3, 4 ]>} ]
gap> GreensRClasses(D);
[ {<identity on [ 1, 3, 4 ]>} ]
gap> GreensHClasses(D);
[ {<identity on [ 1, 3, 4 ]>} ]
gap> D:=GreensDClasses(s)[3];
{<identity on [ 2, 4 ]>}
gap> GreensLClasses(D);
[ {<identity on [ 2, 4 ]>}, {[ 2, 4 ] -> [ 3, 1 ]}, {[ 2, 4 ] -> [ 1, 2 ]}, 
  {[ 2, 4 ] -> [ 3, 2 ]}, {[ 2, 4 ] -> [ 4, 3 ]}, {[ 2, 4 ] -> [ 1, 4 ]} ]
gap> GreensRClasses(D);
[ {<identity on [ 2, 4 ]>}, {[ 1, 3 ] -> [ 4, 2 ]}, {[ 1, 2 ] -> [ 2, 4 ]}, 
  {[ 2, 3 ] -> [ 4, 2 ]}, {[ 3, 4 ] -> [ 4, 2 ]}, {[ 1, 4 ] -> [ 2, 4 ]} ]
gap> GreensHClasses(D);
[ {<identity on [ 2, 4 ]>}, {[ 2, 4 ] -> [ 3, 1 ]}, {[ 2, 4 ] -> [ 1, 2 ]}, 
  {[ 2, 4 ] -> [ 3, 2 ]}, {[ 2, 4 ] -> [ 4, 3 ]}, {[ 2, 4 ] -> [ 1, 4 ]}, 
  {[ 1, 3 ] -> [ 4, 2 ]}, {<identity on [ 1, 3 ]>}, {[ 1, 3 ] -> [ 2, 1 ]}, 
  {[ 1, 3 ] -> [ 2, 3 ]}, {[ 1, 3 ] -> [ 3, 4 ]}, {[ 1, 3 ] -> [ 4, 1 ]}, 
  {[ 1, 2 ] -> [ 2, 4 ]}, {[ 1, 2 ] -> [ 3, 1 ]}, {<identity on [ 1, 2 ]>}, 
  {[ 1, 2 ] -> [ 3, 2 ]}, {[ 1, 2 ] -> [ 4, 3 ]}, {[ 1, 2 ] -> [ 1, 4 ]}, 
  {[ 2, 3 ] -> [ 4, 2 ]}, {[ 2, 3 ] -> [ 1, 3 ]}, {[ 2, 3 ] -> [ 2, 1 ]}, 
  {<identity on [ 2, 3 ]>}, {[ 2, 3 ] -> [ 3, 4 ]}, {[ 2, 3 ] -> [ 4, 1 ]}, 
  {[ 3, 4 ] -> [ 4, 2 ]}, {[ 3, 4 ] -> [ 1, 3 ]}, {[ 3, 4 ] -> [ 2, 1 ]}, 
  {[ 3, 4 ] -> [ 2, 3 ]}, {<identity on [ 3, 4 ]>}, {[ 3, 4 ] -> [ 4, 1 ]}, 
  {[ 1, 4 ] -> [ 2, 4 ]}, {[ 1, 4 ] -> [ 3, 1 ]}, {[ 1, 4 ] -> [ 1, 2 ]}, 
  {[ 1, 4 ] -> [ 3, 2 ]}, {[ 1, 4 ] -> [ 4, 3 ]}, {<identity on [ 1, 4 ]>} ]
gap> h:=last[9];;
gap> L:=LClass(D, Representative(h));
{[ 2, 4 ] -> [ 1, 2 ]}
gap> Position(HClasses(L), h);
2
gap> DClassOfLClass(L)=D;
true
gap> LClassOfHClass(h)=L;
true
gap> R:=RClassOfHClass(h);
{[ 1, 3 ] -> [ 4, 2 ]}
gap> Position(HClasses(R), h);
3
gap> DClassOfRClass(R)=D;
true

gap> s:=InverseSemigroup(
> PartialPermNC( [ 1, 2, 3, 5 ], [ 1, 4, 6, 3 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 6 ], [ 3, 6, 4, 5, 1 ] ) );;
gap> f:=PartialPermNC([ 1, 4, 6 ], [ 6, 3, 1 ]);;
gap> D:=DClass(s, f);
{<identity on [ 1, 4, 6 ]>}
gap> LClass(s, f)=LClass(D, f);
true
gap> RClass(s, f)=RClass(D, f);
true
gap> R:=RClass(s, f);             
{<identity on [ 1, 4, 6 ]>}
gap> HClass(s, f)=HClass(R, f);
true
gap> HClass(D, f)=HClass(R, f);
true
gap> L:=LClass(s, f);                
{[ 1, 4, 6 ] -> [ 6, 3, 1 ]}
gap> HClass(D, f)=HClass(L, f);
true
gap> HClass(s, f)=HClass(L, f);
true
gap> 

gap> s:=POI(10);
<inverse monoid with 10 generators>
gap> f:=PartialPermNC([ 2, 4, 5, 7 ], [ 2, 3, 5, 7 ]);;
gap> l:=LClassNC(s, f);
{[ 2, 4, 5, 7 ] -> [ 2, 3, 5, 7 ]}
gap> l:=LClass(s,f);
{[ 2, 4, 5, 7 ] -> [ 2, 3, 5, 7 ]}
gap> s:=POI(15);
<inverse monoid with 15 generators>
gap> f:=PartialPermNC( [ 1, 3, 5, 8, 9, 10, 12, 13, 14 ],
> [ 2, 3, 4, 7, 9, 11, 12, 13, 15 ] ) ;;
gap> l:=LClass(s,f);
{[ 1, 3, 5, 8, 9, 10, 12, 13, 14 ] -> [ 2, 3, 4, 7, 9, 11, 12, 13, 15 ]}
gap> l:=LClassNC(s,f);
{[ 1, 3, 5, 8, 9, 10, 12, 13, 14 ] -> [ 2, 3, 4, 7, 9, 11, 12, 13, 15 ]}
gap> s:=POI(15);;
gap> l:=LClassNC(s,f);
{[ 1, 3, 5, 8, 9, 10, 12, 13, 14 ] -> [ 2, 3, 4, 7, 9, 11, 12, 13, 15 ]}
gap> l=LClass(s,f);
true
gap> f:=PartialPermNC([ 1, 2, 4, 7, 8, 11, 12 ], [ 1, 2, 6, 7, 9, 10, 11 ]);;
gap> l:=LClass(POI(12), f);
{[ 1, 2, 4, 7, 8, 11, 12 ] -> [ 1, 2, 6, 7, 9, 10, 11 ]}
gap> f:=PartialPermNC( [ 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13 ],
> [ 1, 2, 3, 4, 5, 7, 8, 9, 10, 12, 13 ] );;
gap> l:=LClass(POI(13), f);
{[ 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13 ] -> [ 1, 2, 3, 4, 5, 7, 8, 9, 10, 12, 
  13 ]}
gap> f:=PartialPermNC([ 1, 2, 3, 4, 7, 8, 9, 10 ], 
> [ 2, 3, 4, 5, 6, 8, 10, 11 ]);;
gap> l:=LClass(POI(13), f);
{[ 1, 2, 3, 4, 7, 8, 9, 10 ] -> [ 2, 3, 4, 5, 6, 8, 10, 11 ]}
gap> LClassNC(POI(13), f);
{[ 1, 2, 3, 4, 7, 8, 9, 10 ] -> [ 2, 3, 4, 5, 6, 8, 10, 11 ]}
gap> RClassNC(POI(13), f);
{<identity on [ 1, 2, 3, 4, 7, 8, 9, 10 ]>}
gap> HClassNC(POI(13), f);       
{[ 1, 2, 3, 4, 7, 8, 9, 10 ] -> [ 2, 3, 4, 5, 6, 8, 10, 11 ]}
gap> DClassNC(POI(13), f);
{<identity on [ 1, 2, 3, 4, 7, 8, 9, 10 ]>}
gap> s:=POI(13);
<inverse monoid with 13 generators>
gap> D:=DClassNC(s, f);
{<identity on [ 1, 2, 3, 4, 7, 8, 9, 10 ]>}
gap> LClassNC(s, f)=LClass(D, f);
true
gap> LClass(s, f)=LClassNC(D, f);
true
gap> LClassNC(s, f)=LClassNC(D, f);
true
gap> LClassNC(s, f)=LClassNC(D, f);
true
gap> RClass(s, f)=RClassNC(D, f);
true
gap> RClassNC(s, f)=RClassNC(D, f);
true
gap> RClassNC(s, f)=RClass(D, f);  
true
gap> R:=RClassNC(s, f);
{<identity on [ 1, 2, 3, 4, 7, 8, 9, 10 ]>}
gap> HClass(s, f)=HClass(R, f);
true
gap> HClassNC(s, f)=HClass(R, f);
true
gap> HClassNC(s, f)=HClassNC(R, f);
true
gap> HClass(s, f)=HClassNC(R, f);  
true
gap> L:=LClassNC(s, f);
{[ 1, 2, 3, 4, 7, 8, 9, 10 ] -> [ 2, 3, 4, 5, 6, 8, 10, 11 ]}
gap> HClass(s, f)=HClassNC(L, f);
true
gap> HClass(s, f)=HClass(L, f);
true
gap> HClassNC(L, f)=HClass(D, f);
true
gap> HClassNC(L, f)=HClassNC(s, f);
true
gap> HClass(D, f)=HClassNC(s, f);  
true

gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;

gap> STOP_TEST("Citrus package: inverse.tst", 10000);
