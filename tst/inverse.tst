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

#
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

#
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

#
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

#
gap> file:=Concatenation(CitrusDir(), "/examples/graph7c.citrus.gz");;
gap> s:=Semigroup(ReadCitrus(file, 600));
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

#
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

#
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

#
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

#
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

#
gap> m:=InverseSemigroup(
> PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
>  18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 
>  37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55,
>  56, 57, 58, 59, 60, 61, 62, 63, 64 ], [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
>  12, 13, 14, 15, 16, 33, 34, 35, 36, 37, 39, 40, 41, 43, 44, 46, 49, 50, 52, 
>  55, 59, 17, 18, 19, 20, 21, 38, 22, 23, 24, 42, 25, 26, 45, 27, 47, 48, 28, 
>  29, 51, 30, 53, 54, 31, 56, 57, 58, 32, 60, 61, 62, 63, 64 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
>  18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 
>  37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 
>  56, 57, 58, 59, 60, 61, 62, 63, 64 ], [ 1, 2, 3, 4, 9, 10, 11, 13, 5, 6, 7, 
>  12, 8, 14, 15, 16, 17, 18, 19, 21, 20, 22, 24, 23, 26, 25, 27, 29, 28, 30, 
>  31, 32, 33, 34, 35, 37, 36, 38, 39, 41, 40, 42, 44, 43, 45, 46, 48, 47, 50, 
>  49, 51, 52, 54, 53, 55, 57, 56, 58, 59, 61, 60, 62, 63, 64 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
>  18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 
>  37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 
>  56, 57, 58, 59, 60, 61, 62, 63, 64 ], [ 1, 2, 3, 4, 5, 6, 7, 8, 17, 18, 19, 
>  20, 22, 23, 25, 28, 9, 10, 11, 12, 21, 13, 14, 24, 15, 26, 27, 16, 29, 30, 
>  31, 32, 33, 34, 35, 36, 38, 37, 39, 40, 42, 41, 43, 45, 44, 47, 46, 48, 49, 
>  51, 50, 53, 52, 54, 56, 55, 57, 58, 60, 59, 61, 62, 63, 64 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
>  18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 
>  37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 
>  56, 57, 58, 59, 60, 61, 62, 63, 64 ], [ 1, 3, 2, 4, 5, 7, 6, 8, 9, 11, 10, 
>  12, 13, 15, 14, 16, 17, 19, 18, 20, 21, 22, 25, 26, 23, 24, 27, 28, 29, 31, 
>  30, 32, 33, 35, 34, 36, 37, 38, 39, 43, 44, 45, 40, 41, 42, 46, 47, 48, 49, 
>  50, 51, 55, 56, 57, 52, 53, 54, 58, 59, 60, 61, 63, 62, 64 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
>  18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 
>  37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 
>  56, 57, 58, 59, 60, 61, 62, 63, 64 ], [ 1, 2, 5, 6, 3, 4, 7, 8, 9, 10, 12, 
>  11, 14, 13, 15, 16, 17, 18, 20, 19, 21, 23, 22, 24, 25, 27, 26, 28, 30, 29, 
>  31, 32, 33, 34, 36, 35, 37, 38, 40, 39, 41, 42, 43, 46, 47, 44, 45, 48, 49, 
>  52, 53, 50, 51, 54, 55, 56, 58, 57, 59, 60, 62, 61, 63, 64 ] ),
> PartialPermNC( [ 2, 4, 6, 8, 10, 13, 14, 16, 18, 22, 23, 24, 28, 29, 30, 32, 
>  34, 39, 40, 41, 42, 49, 50, 51, 52, 53, 54, 59, 60, 61, 62, 64 ], 
> [ 3, 4, 7, 8, 11, 13, 15, 16, 19, 22, 25, 26, 28, 29, 31, 32, 35, 39, 43, 44, 
>  45, 49, 50, 51, 55, 56, 57, 59, 60, 61, 63, 64 ] ) );;
gap> DClassReps(m);
[ <partial perm on 64 pts>, <partial perm on 32 pts>, 
  <identity on [ 4, 8, 13, 16, 22, 28, 29, 32, 39, 49, 50, 51, 59, 60, 61, 64 
     ]>, <identity on [ 8, 16, 28, 32, 49, 59, 60, 64 ]>, 
  <identity on [ 16, 32, 59, 64 ]>, <identity on [ 32, 64 ]>, 
  <identity on [ 64 ]> ]
gap> NrLClasses(m);
64
gap> IsRTrivial(m);
false
gap> Size(m);
13327
gap> f:=PartialPermNC([ 27, 30, 31, 32, 58, 62, 63, 64 ],
> [ 8, 16, 28, 60, 49, 59, 32, 64 ]);;
gap> d:=DClassNC(m, f);
{<identity on [ 8, 16, 28, 32, 49, 59, 60, 64 ]>}
gap> LClassReps(d);
[ <identity on [ 8, 16, 28, 32, 49, 59, 60, 64 ]>, 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 13, 16, 29, 32, 50, 59, 61, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 22, 28, 29, 32, 51, 60, 61, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 15, 16, 31, 32, 55, 59, 63, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 14, 16, 30, 32, 52, 59, 62, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 39, 49, 50, 59, 51, 60, 61, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 25, 28, 31, 32, 56, 60, 63, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 23, 28, 30, 32, 53, 60, 62, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 43, 49, 55, 59, 56, 60, 63, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 40, 49, 52, 59, 53, 60, 62, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 26, 29, 31, 32, 57, 61, 63, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 24, 29, 30, 32, 54, 61, 62, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 44, 50, 55, 59, 57, 61, 63, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 41, 50, 52, 59, 54, 61, 62, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 27, 30, 31, 32, 58, 62, 63, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 45, 51, 56, 60, 57, 61, 63, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 42, 51, 53, 60, 54, 61, 62, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 46, 52, 55, 59, 58, 62, 63, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 47, 53, 56, 60, 58, 62, 63, 64 ], 
  [ 8, 16, 28, 32, 49, 59, 60, 64 ] -> [ 48, 54, 57, 61, 58, 62, 63, 64 ] ]
gap> List(DClasses(m), NrRClasses);
[ 1, 6, 15, 20, 15, 6, 1 ]
gap> d:=DClasses(m)[6]; 
{<identity on [ 32, 64 ]>}
gap> LClassReps(d);
[ <identity on [ 32, 64 ]>, [ 32, 64 ] -> [ 59, 64 ], 
  [ 32, 64 ] -> [ 60, 64 ], [ 32, 64 ] -> [ 61, 64 ], 
  [ 32, 64 ] -> [ 63, 64 ], [ 32, 64 ] -> [ 62, 64 ] ]
gap> RClassReps(d);                              
[ <identity on [ 32, 64 ]>, [ 59, 64 ] -> [ 32, 64 ], 
  [ 60, 64 ] -> [ 32, 64 ], [ 61, 64 ] -> [ 32, 64 ], 
  [ 63, 64 ] -> [ 32, 64 ], [ 62, 64 ] -> [ 32, 64 ] ]
gap> d:=DClassNC(m, Representative(d));
{<identity on [ 32, 64 ]>}
gap> LClassReps(d);
[ <identity on [ 32, 64 ]>, [ 32, 64 ] -> [ 59, 64 ], 
  [ 32, 64 ] -> [ 60, 64 ], [ 32, 64 ] -> [ 61, 64 ], 
  [ 32, 64 ] -> [ 63, 64 ], [ 32, 64 ] -> [ 62, 64 ] ]
gap> RClassReps(m);             
[ <partial perm on 64 pts>, <partial perm on 32 pts>, 
  <partial perm on 32 pts>, <partial perm on 32 pts>, 
  <partial perm on 32 pts>, <partial perm on 32 pts>, 
  <partial perm on 32 pts>, 
  <identity on [ 4, 8, 13, 16, 22, 28, 29, 32, 39, 49, 50, 51, 59, 60, 61, 64 
     ]>, [ 7, 8, 15, 16, 25, 28, 31, 32, 43, 49, 55, 56, 59, 60, 63, 64 ] -> 
    [ 4, 8, 13, 16, 22, 28, 29, 32, 39, 49, 50, 51, 59, 60, 61, 64 ], 
  [ 6, 8, 14, 16, 23, 28, 30, 32, 40, 49, 52, 53, 59, 60, 62, 64 ] -> 
    [ 4, 8, 13, 16, 22, 28, 29, 32, 39, 49, 50, 51, 59, 60, 61, 64 ], 
  [ 11, 13, 15, 16, 26, 29, 31, 32, 44, 50, 55, 57, 59, 61, 63, 64 ] -> 
    [ 4, 8, 13, 16, 22, 28, 29, 32, 39, 49, 50, 51, 59, 60, 61, 64 ], 
  [ 10, 13, 14, 16, 24, 29, 30, 32, 41, 50, 52, 54, 59, 61, 62, 64 ] -> 
    [ 4, 8, 13, 16, 22, 28, 29, 32, 39, 49, 50, 51, 59, 60, 61, 64 ], 
  [ 19, 22, 25, 26, 28, 29, 31, 32, 45, 51, 56, 57, 60, 61, 63, 64 ] -> 
    [ 4, 8, 13, 22, 16, 28, 29, 32, 39, 49, 50, 51, 59, 60, 61, 64 ], 
  [ 18, 22, 23, 24, 28, 29, 30, 32, 42, 51, 53, 54, 60, 61, 62, 64 ] -> 
    [ 4, 8, 13, 22, 16, 28, 29, 32, 39, 49, 50, 51, 59, 60, 61, 64 ], 
  [ 12, 14, 15, 16, 27, 30, 31, 32, 46, 52, 55, 58, 59, 62, 63, 64 ] -> 
    [ 4, 8, 13, 16, 22, 28, 29, 32, 39, 49, 50, 51, 59, 60, 61, 64 ], 
  [ 35, 39, 43, 44, 45, 49, 50, 51, 55, 56, 57, 59, 60, 61, 63, 64 ] -> 
    [ 4, 8, 13, 22, 39, 16, 28, 49, 29, 50, 51, 32, 59, 60, 61, 64 ], 
  [ 34, 39, 40, 41, 42, 49, 50, 51, 52, 53, 54, 59, 60, 61, 62, 64 ] -> 
    [ 4, 8, 13, 22, 39, 16, 28, 49, 29, 50, 51, 32, 59, 60, 61, 64 ], 
  [ 20, 23, 25, 27, 28, 30, 31, 32, 47, 53, 56, 58, 60, 62, 63, 64 ] -> 
    [ 4, 8, 13, 22, 16, 28, 29, 32, 39, 49, 50, 51, 59, 60, 61, 64 ], 
  [ 36, 40, 43, 46, 47, 49, 52, 53, 55, 56, 58, 59, 60, 62, 63, 64 ] -> 
    [ 4, 8, 13, 22, 39, 16, 28, 49, 29, 50, 51, 32, 59, 60, 61, 64 ], 
  [ 21, 24, 26, 27, 29, 30, 31, 32, 48, 54, 57, 58, 61, 62, 63, 64 ] -> 
    [ 4, 8, 13, 22, 16, 28, 29, 32, 39, 49, 50, 51, 59, 60, 61, 64 ], 
  [ 37, 41, 44, 46, 48, 50, 52, 54, 55, 57, 58, 59, 61, 62, 63, 64 ] -> 
    [ 4, 8, 13, 22, 39, 16, 28, 49, 29, 50, 51, 32, 59, 60, 61, 64 ], 
  [ 38, 42, 45, 47, 48, 51, 53, 54, 56, 57, 58, 60, 61, 62, 63, 64 ] -> 
    [ 4, 8, 13, 22, 39, 16, 28, 49, 29, 50, 51, 32, 59, 60, 61, 64 ], 
  <identity on [ 8, 16, 28, 32, 49, 59, 60, 64 ]>, 
  [ 13, 16, 29, 32, 50, 59, 61, 64 ] -> [ 8, 16, 28, 32, 49, 59, 60, 64 ], 
  [ 22, 28, 29, 32, 51, 60, 61, 64 ] -> [ 8, 16, 28, 32, 49, 59, 60, 64 ], 
  [ 15, 16, 31, 32, 55, 59, 63, 64 ] -> [ 8, 16, 28, 32, 49, 59, 60, 64 ], 
  [ 14, 16, 30, 32, 52, 59, 62, 64 ] -> [ 8, 16, 28, 32, 49, 59, 60, 64 ], 
  [ 39, 49, 50, 51, 59, 60, 61, 64 ] -> [ 8, 16, 28, 49, 32, 59, 60, 64 ], 
  [ 25, 28, 31, 32, 56, 60, 63, 64 ] -> [ 8, 16, 28, 32, 49, 59, 60, 64 ], 
  [ 23, 28, 30, 32, 53, 60, 62, 64 ] -> [ 8, 16, 28, 32, 49, 59, 60, 64 ], 
  [ 43, 49, 55, 56, 59, 60, 63, 64 ] -> [ 8, 16, 28, 49, 32, 59, 60, 64 ], 
  [ 40, 49, 52, 53, 59, 60, 62, 64 ] -> [ 8, 16, 28, 49, 32, 59, 60, 64 ], 
  [ 26, 29, 31, 32, 57, 61, 63, 64 ] -> [ 8, 16, 28, 32, 49, 59, 60, 64 ], 
  [ 24, 29, 30, 32, 54, 61, 62, 64 ] -> [ 8, 16, 28, 32, 49, 59, 60, 64 ], 
  [ 44, 50, 55, 57, 59, 61, 63, 64 ] -> [ 8, 16, 28, 49, 32, 59, 60, 64 ], 
  [ 41, 50, 52, 54, 59, 61, 62, 64 ] -> [ 8, 16, 28, 49, 32, 59, 60, 64 ], 
  [ 27, 30, 31, 32, 58, 62, 63, 64 ] -> [ 8, 16, 28, 32, 49, 59, 60, 64 ], 
  [ 45, 51, 56, 57, 60, 61, 63, 64 ] -> [ 8, 16, 28, 49, 32, 59, 60, 64 ], 
  [ 42, 51, 53, 54, 60, 61, 62, 64 ] -> [ 8, 16, 28, 49, 32, 59, 60, 64 ], 
  [ 46, 52, 55, 58, 59, 62, 63, 64 ] -> [ 8, 16, 28, 49, 32, 59, 60, 64 ], 
  [ 47, 53, 56, 58, 60, 62, 63, 64 ] -> [ 8, 16, 28, 49, 32, 59, 60, 64 ], 
  [ 48, 54, 57, 58, 61, 62, 63, 64 ] -> [ 8, 16, 28, 49, 32, 59, 60, 64 ], 
  <identity on [ 16, 32, 59, 64 ]>, [ 28, 32, 60, 64 ] -> [ 16, 32, 59, 64 ], 
  [ 49, 59, 60, 64 ] -> [ 16, 32, 59, 64 ], 
  [ 29, 32, 61, 64 ] -> [ 16, 32, 59, 64 ], 
  [ 50, 59, 61, 64 ] -> [ 16, 32, 59, 64 ], 
  [ 31, 32, 63, 64 ] -> [ 16, 32, 59, 64 ], 
  [ 30, 32, 62, 64 ] -> [ 16, 32, 59, 64 ], 
  [ 51, 60, 61, 64 ] -> [ 16, 32, 59, 64 ], 
  [ 55, 59, 63, 64 ] -> [ 16, 32, 59, 64 ], 
  [ 52, 59, 62, 64 ] -> [ 16, 32, 59, 64 ], 
  [ 56, 60, 63, 64 ] -> [ 16, 32, 59, 64 ], 
  [ 53, 60, 62, 64 ] -> [ 16, 32, 59, 64 ], 
  [ 57, 61, 63, 64 ] -> [ 16, 32, 59, 64 ], 
  [ 54, 61, 62, 64 ] -> [ 16, 32, 59, 64 ], 
  [ 58, 62, 63, 64 ] -> [ 16, 32, 59, 64 ], <identity on [ 32, 64 ]>, 
  [ 59, 64 ] -> [ 32, 64 ], [ 60, 64 ] -> [ 32, 64 ], 
  [ 61, 64 ] -> [ 32, 64 ], [ 63, 64 ] -> [ 32, 64 ], 
  [ 62, 64 ] -> [ 32, 64 ], <identity on [ 64 ]> ]
gap> RClassReps(d);
[ <identity on [ 32, 64 ]>, [ 59, 64 ] -> [ 32, 64 ], 
  [ 60, 64 ] -> [ 32, 64 ], [ 61, 64 ] -> [ 32, 64 ], 
  [ 63, 64 ] -> [ 32, 64 ], [ 62, 64 ] -> [ 32, 64 ] ]
gap> Size(d);
36
gap> Size(DClasses(m)[6]);
36

#
gap> s:=InverseSemigroup( [ PartialPermNC( [ 1, 2, 3, 5 ], [ 2, 1, 6, 3 ] ),
> PartialPermNC( [ 1, 2, 3, 6 ], [ 3, 5, 2, 6 ] ) ]);;
gap> f:=PartialPermNC([ 1 .. 3 ], [ 6, 3, 1 ]);;
gap> d:=DClassNC(s, f);
{<identity on [ 1 .. 3 ]>}
gap> GroupHClass(d);
{<identity on [ 1 .. 3 ]>}
gap> StructureDescription(last);
"1"
gap> ForAny(DClasses(s), x-> not IsTrivial(GroupHClass(x)));
true
gap> First(DClasses(s), x-> not IsTrivial(GroupHClass(x)));
{<identity on [ 1, 2 ]>}
gap> StructureDescription(GroupHClass(last));
"C2"

#
gap> s:=InverseSemigroup(
> [ PartialPermNC( [ 1, 2, 3, 4, 5, 7 ], [ 10, 6, 3, 4, 9, 1 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8 ], [ 6, 10, 7, 4, 8, 2, 9, 1 ] ) ]);;
gap> Idempotents(s, 1);
[ <identity on [ 4 ]> ]
gap> Idempotents(s, 0);
[  ]
gap> PartialPermNC([]) in s; 
false
gap> Idempotents(s, 2);
[ <identity on [ 3, 4 ]>, <identity on [ 4, 7 ]>, <identity on [ 2, 4 ]>, 
  <identity on [ 4, 10 ]>, <identity on [ 1, 4 ]>, <identity on [ 4, 9 ]>, 
  <identity on [ 4, 8 ]>, <identity on [ 4, 6 ]>, <identity on [ 4, 5 ]> ]
gap> Idempotents(s, 10);
[  ]
gap> f:=PartialPermNC( [ 2, 4, 9, 10 ], [ 7, 4, 3, 2 ] );;
gap> r:=RClassNC(s, f);
{[ 2, 4, 9, 10 ] -> [ 6, 4, 7, 2 ]}
gap> Idempotents(r);
[ <identity on [ 2, 4, 9, 10 ]> ]

#
gap> s:=RandomInverseSemigroup(2,10);
<inverse semigroup with 2 generators>
gap> ForAll(RClasses(s), IsRegularRClass);
true

#
gap> s:=InverseSemigroup( 
> PartialPermNC( [ 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 15 ], 
> [ 6, 4, 18, 3, 11, 8, 5, 14, 19, 13, 12, 20, 1 ] ),
> PartialPermNC( [ 1, 2, 4, 5, 6, 7, 9, 10, 11, 12, 15, 16, 18, 20 ], 
> [ 1, 18, 3, 7, 4, 9, 19, 5, 14, 16, 12, 17, 15, 6 ] ) );;
gap> iter:=IteratorOfDClassReps(s);
<iterator of D-class reps>
gap> NextIterator(iter);
<identity on [ 1, 3, 4, 5, 6, 8, 11, 12, 13, 14, 18, 19, 20 ]>
gap> NextIterator(iter);
<identity on [ 1, 3, 4, 5, 6, 7, 9, 12, 14, 15, 16, 17, 18, 19 ]>
gap> NextIterator(iter);
<identity on [ 3, 5, 6, 11, 12, 13, 18, 20 ]>
gap> NextIterator(iter);
<identity on [ 1, 3, 4, 6, 7, 14, 15, 16 ]>
gap> NextIterator(iter);
<identity on [ 1, 2, 4, 6, 9, 10, 11, 15, 20 ]>
gap> NextIterator(iter);
<identity on [ 1, 3, 6, 8, 11, 14, 18, 20 ]>
gap> NextIterator(iter);
<identity on [ 1, 3, 4, 7, 9, 12, 15, 16, 17, 19 ]>
gap> NextIterator(iter);
<identity on [ 1, 3, 4, 5, 9, 12, 14, 18, 19 ]>
gap> NextIterator(iter);
<identity on [ 11, 12, 13, 18 ]>
gap> NextIterator(iter);
<identity on [ 4, 6, 7, 14, 15, 16 ]>
gap> NextIterator(iter);
<identity on [ 2, 4, 10, 15, 20 ]>
gap> NextIterator(iter);
<identity on [ 1, 3, 6, 8, 11, 18, 20 ]>
gap> NextIterator(iter);
<identity on [ 1, 3, 4, 9, 12, 17 ]>
gap> NextIterator(iter);
<identity on [ 1, 2, 4, 9, 15 ]>
gap> NextIterator(iter);
<identity on [ 1, 3, 4, 6, 11, 13, 14, 19 ]>
gap> NextIterator(iter);
<identity on [ 1, 6, 7, 18, 20 ]>
gap> NextIterator(iter);
<identity on [ 5, 6, 11, 13, 18, 20 ]>
gap> NextIterator(iter);
<identity on [ 1, 4, 6, 14, 15 ]>
gap> NextIterator(iter);
<identity on [ 1, 2, 4, 11, 20 ]>
gap> NextIterator(iter);
<identity on [ 1, 3, 6, 8, 14, 18 ]>
gap> NextIterator(iter);
<identity on [ 1, 3, 9, 12, 16, 17, 19 ]>
gap> NextIterator(iter);
<identity on [ 1, 3, 5, 12, 18, 19 ]>
gap> NextIterator(iter);
<identity on [ 3, 6, 14, 18, 20 ]>
gap> NextIterator(iter);
<identity on [ 1, 3, 7, 15, 16, 19 ]>
gap> NextIterator(iter);
<identity on [ 12, 13 ]>
gap> NextIterator(iter);
<identity on [ 14 .. 16 ]>
gap> NextIterator(iter);
<identity on [ 1, 3, 8, 11, 20 ]>
gap> NextIterator(iter);
<identity on [ 3, 4, 9, 12, 17 ]>
gap> NextIterator(iter);
<identity on [ 1, 2, 9 ]>
gap> NextIterator(iter);
<identity on [ 1, 3, 4, 19 ]>
gap> NextIterator(iter);
<identity on [ 5, 6, 11, 13, 18 ]>
gap> NextIterator(iter);
<identity on [ 1, 2, 4, 20 ]>
gap> NextIterator(iter);
<identity on [ 3, 6, 14, 18 ]>
gap> NextIterator(iter);
<identity on [ 1, 3, 16, 19 ]>
gap> NextIterator(iter);
<identity on [ 2, 4, 13, 15 ]>
gap> NextIterator(iter);
<identity on [ 1, 6, 7, 18 ]>
gap> NextIterator(iter);
<identity on [ 1, 2, 6, 10, 15, 20 ]>
gap> NextIterator(iter);
<identity on [ 3, 6, 11, 12, 13, 18, 20 ]>
gap> NextIterator(iter);
<identity on [ 1, 11, 18, 20 ]>
gap> NextIterator(iter);
<identity on [ 11 .. 13 ]>
gap> NextIterator(iter);
<identity on [ 2, 10, 20 ]>
gap> NextIterator(iter);
<identity on [ 3, 4, 6, 13 ]>
gap> NextIterator(iter);
<identity on [ 1, 4, 15 ]>
gap> NextIterator(iter);
<identity on [ 6, 14, 18 ]>
gap> NextIterator(iter);
<identity on [ 1, 16, 17, 19 ]>
gap> NextIterator(iter);
<identity on [ 1, 7, 15, 16 ]>
gap> s:=RandomInverseSemigroup(2,20);
<inverse semigroup with 2 generators>
gap> iter:=IteratorOfDClassReps(s);
<iterator of D-class reps>
gap> s:=RandomInverseSemigroup(2,100);
<inverse semigroup with 2 generators>
gap> iter:=IteratorOfLClassReps(s);
<iterator of L-class reps>
gap> for i in [1..10000] do NextIterator(iter); od;
gap> s:=RandomInverseSemigroup(2,10);        
<inverse semigroup with 2 generators>
gap> iter:=IteratorOfLClassReps(s);
<iterator of L-class reps>
gap> for i in iter do od;
gap> iter:=IteratorOfDClassReps(s);
<iterator of D-class reps>
gap> for i in iter do od;
gap> iter:=IteratorOfRClassReps(s);
<iterator of R-class reps>
gap> for i in iter do od;

#
gap> s:=RandomInverseSemigroup(100,100);
<inverse semigroup with 100 generators>
gap> iter:=IteratorOfRClasses(s);       
<iterator of R-classes>
gap> for i in [1..100] do NextIterator(iter); od;
gap> iter:=IteratorOfLClasses(s);      
<iterator of L-classes>
gap> for i in [1..100] do NextIterator(iter); od;

#
gap> s:=InverseSemigroup(
> [ PartialPermNC( [ 1, 2, 3, 5, 7, 9, 10 ], [ 6, 7, 2, 9, 1, 5, 3 ] ),
> PartialPermNC( [ 1, 2, 3, 5, 6, 7, 9, 10 ], [ 8, 1, 9, 4, 10, 5, 6, 7 ] ) ]);;
gap> NrIdempotents(s);
236
gap> f:=PartialPermNC([ 2, 3, 7, 9, 10 ], [ 7, 2, 1, 5, 3 ]);;
gap> d:=DClassNC(s, f);;
gap> NrIdempotents(d);
13
gap> l:=LClass(d, f);
{[ 2, 3, 7, 9, 10 ] -> [ 7, 2, 1, 5, 3 ]}
gap> NrIdempotents(l);
1
gap> DClass(l);
{<identity on [ 2, 3, 7, 9, 10 ]>}
gap> last=d;
true
gap> NrIdempotents(last2);
13

#
gap> S:=InverseSemigroup(
> PartialPermNC( [ 1, 2, 3 ], [ 1, 3, 5 ] ),
> PartialPermNC( [ 1, 2, 4 ], [ 1, 2, 3 ] ),
> PartialPermNC( [ 1, 2, 5 ], [ 4, 5, 2 ] ) );;
gap> f:=PartialPermNC( [ 1, 5 ], [ 3, 2 ] );;
gap> SchutzenbergerGroup(LClass(S, f));
Group(())
gap> SchutzenbergerGroup(RClass(S, f));
Group(())
gap> SchutzenbergerGroup(HClass(S, f));
Group(())
gap> SchutzenbergerGroup(DClass(S, f));
Group(())
gap> List(DClasses(S), SchutzenbergerGroup);
[ Group(()), Group(()), Group(()), Group(()), Group([ (2,5) ]), Group(()) ]

#
gap> file:=Concatenation(CitrusDir(), "/examples/munn.citrus.gz");;
gap> ReadCitrus(file, 1078);;
gap> s:=InverseSemigroup(last);
<inverse semigroup with 6 generators>
gap> Size(s);
12
gap> IsDTrivial(s);
false

#
gap> IsIsometryPP:=function(f)
> local n, i, j, k, l;
>  n:=f[2];
>  for i in [1..n-1] do
>    k:=DomPP(f)[i];
>    for j in [i+1..n] do
>      l:=DomPP(f)[j];
>      if not AbsInt(k^f-l^f)=AbsInt(k-l) then
>        return false;
>      fi;
>    od;
>  od;
>  return true;
> end;;
gap> s:=SubsemigroupByProperty(SymmetricInverseSemigp(5), IsIsometryPP);;
gap> Size(s);
142
gap> s:=SubsemigroupByProperty(SymmetricInverseSemigp(6), IsIsometryPP);;
gap> Size(s);
319
gap> s:=SubsemigroupByProperty(SymmetricInverseSemigp(7), IsIsometryPP);;
gap> Size(s);
686

#
gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;

#
gap> STOP_TEST("Citrus package: inverse.tst", 10000);
