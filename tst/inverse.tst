#############################################################################
##
#W  inverse.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: inverse.tst");
gap> LoadPackage("semigroups", false);;

# 
gap> SemigroupsStartTest();

#T# InverseTest1
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
<inverse partial perm semigroup on 5 pts with 10 generators>
gap> Size(s);
860
gap> NrRClasses(s);
31
gap> RClassReps(s);
[ <identity partial perm on [ 1, 2, 5 ]>, [3,2,5,1], [4,1](2)(5), [3,1,5](2), 
  [3,5][4,1,2], [4,5,1,2], [4,2,5](1), [3,5,2](1), [3,2,5][4,1], 
  [3,5,2][4,1], <identity partial perm on [ 1, 2, 4, 5 ]>, [3,2,5,4,1], 
  [3,5](1,4,2), [3,4](1,5)(2), <identity partial perm on [ 1, 2, 3, 4, 5 ]>, 
  <identity partial perm on [ 1, 5 ]>, [2,1][3,5], [3,1,5], [3,1](5), 
  [4,5](1), [2,5][4,1], [4,1](5), [3,5][4,1], [2,5,1], [2,5](1), 
  <identity partial perm on [ 2 ]>, [5,2], [1,2], [3,2], [4,2], 
  <empty partial perm> ]
gap> List(last, DomainOfPartialPerm);
[ [ 1, 2, 5 ], [ 2, 3, 5 ], [ 2, 4, 5 ], [ 1, 2, 3 ], [ 1, 3, 4 ], 
  [ 1, 4, 5 ], [ 1, 2, 4 ], [ 1, 3, 5 ], [ 2, 3, 4 ], [ 3, 4, 5 ], 
  [ 1, 2, 4, 5 ], [ 2, 3, 4, 5 ], [ 1, 2, 3, 4 ], [ 1, 2, 3, 5 ], 
  [ 1, 2, 3, 4, 5 ], [ 1, 5 ], [ 2, 3 ], [ 1, 3 ], [ 3, 5 ], [ 1, 4 ], 
  [ 2, 4 ], [ 4, 5 ], [ 3, 4 ], [ 2, 5 ], [ 1, 2 ], [ 2 ], [ 5 ], [ 1 ], 
  [ 3 ], [ 4 ], [  ] ]
gap> IsDuplicateFreeList(last);
true

#T# InverseTest2
gap> s:=InverseSemigroup(PartialPermNC([ 1, 2 ], [ 1, 2 ]),
> PartialPermNC([ 1, 2 ], [ 1, 3 ]));;
gap> GreensHClasses(s);
[ {PartialPerm( [ 1, 2 ], [ 1, 2 ] )}, {PartialPerm( [ 1, 2 ], [ 1, 3 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 1, 2 ] )}, {PartialPerm( [ 1, 3 ], [ 1, 3 ] )}, 
  {PartialPerm( [ 1 ], [ 1 ] )} ]
gap> s:=InverseSemigroup(Generators(s));;
gap> HClassReps(s);
[ <identity partial perm on [ 1, 2 ]>, [2,3](1), [3,2](1), 
  <identity partial perm on [ 1, 3 ]>, <identity partial perm on [ 1 ]> ]
gap> GreensHClasses(s);
[ {PartialPerm( [ 1, 2 ], [ 1, 2 ] )}, {PartialPerm( [ 1, 2 ], [ 1, 3 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 1, 2 ] )}, {PartialPerm( [ 1, 3 ], [ 1, 3 ] )}, 
  {PartialPerm( [ 1 ], [ 1 ] )} ]

#T# InverseTest3
gap> gens:=[PartialPermNC( [ 1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19 ], 
> [ 9, 18, 20, 11, 5, 16, 8, 19, 14, 13, 1 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 14, 16, 18, 19, 20 ], 
> [ 13, 1, 8, 5, 4, 14, 11, 12, 9, 20, 2, 18, 7, 3, 19 ] )];;
gap> s:=InverseSemigroup(gens);;
gap> d:=DClass(s, Generators(s)[1]);
{PartialPerm( [ 1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20 ], 
 [ 1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20 ] )}
gap> Size(s);
60880
gap> h:=HClass(d, Generators(s)[1]);
{PartialPerm( [ 1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19 ], 
 [ 9, 18, 20, 11, 5, 16, 8, 19, 14, 13, 1 ] )}
gap> Generators(s)[1] in h;
true
gap> DClassOfHClass(h)=d;
true
gap> DClassOfHClass(h);  
{PartialPerm( [ 1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20 ], 
 [ 1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20 ] )}
gap> r:=RClassOfHClass(h);
{PartialPerm( [ 1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19 ], 
 [ 9, 18, 20, 11, 5, 16, 8, 19, 14, 13, 1 ] )}
gap> ForAll(h, x-> x in r);   
true
gap> l:=LClass(h);   
{PartialPerm( [ 1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20 ], 
 [ 1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20 ] )}
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
<inverse partial perm semigroup on 10 pts with 2 generators>
gap> f:=Generators(S)[1];
[3,7][8,1,2,6,9][10,5]
gap> h:=HClass(S, f);
{PartialPerm( [ 1, 2, 3, 6, 8, 10 ], [ 2, 6, 7, 9, 1, 5 ] )}
gap> IsGreensHClass(h);
true
gap> RClassOfHClass(h);
{PartialPerm( [ 1, 2, 3, 6, 8, 10 ], [ 2, 6, 7, 9, 1, 5 ] )}
gap> LClassOfHClass(h);
{PartialPerm( [ 1, 2, 5, 6, 7, 9 ], [ 1, 2, 5, 6, 7, 9 ] )}
gap> r:=RClassOfHClass(h);
{PartialPerm( [ 1, 2, 3, 6, 8, 10 ], [ 2, 6, 7, 9, 1, 5 ] )}
gap> l:=LClass(h);
{PartialPerm( [ 1, 2, 5, 6, 7, 9 ], [ 1, 2, 5, 6, 7, 9 ] )}
gap> DClass(r)=DClass(l);
true
gap> DClass(h)=DClass(l);
true
gap> f:=PartialPermNC([ 1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19 ], 
> [ 9, 18, 20, 11, 5, 16, 8, 19, 14, 13, 1 ]);;
gap> h:=HClass(s, f);
{PartialPerm( [ 1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19 ], 
 [ 9, 18, 20, 11, 5, 16, 8, 19, 14, 13, 1 ] )}
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
{PartialPerm( [ 8, 20 ], [ 8, 20 ] )}
gap> IsDoneIterator(iter);
false
gap> ForAll(HClasses(s), IsTrivial);
false
gap> First(HClasses(s), x-> not IsTrivial(x));
{PartialPerm( [ 8, 20 ], [ 8, 20 ] )}
gap> Size(last);
2
gap> iter:=IteratorOfHClasses(s);                                          
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until Size(h)>1 or IsDoneIterator(iter);
gap> h;
{PartialPerm( [ 8, 20 ], [ 8, 20 ] )}
gap> IsDoneIterator(iter);
false
gap> s:=InverseSemigroup(Generators(s));
<inverse partial perm semigroup on 18 pts with 2 generators>
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until Size(h)>1 or IsDoneIterator(iter);
gap> h;
{PartialPerm( [ 8, 20 ], [ 8, 20 ] )}
gap> Size(h);
2
gap> IsDoneIterator(iter);
false
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> h:=NextIterator(iter);
{PartialPerm( [ 1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20 ], 
 [ 1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20 ] )}
gap> Size(h);
1
gap> h:=NextIterator(iter);
{PartialPerm( [ 1, 5, 8, 9, 11, 13, 14, 16, 18, 19, 20 ], 
 [ 19, 6, 8, 1, 5, 16, 12, 7, 2, 11, 3 ] )}
gap> Size(h);
1
gap> h:=NextIterator(iter);
{PartialPerm( [ 1, 2, 3, 5, 6, 7, 8, 11, 12, 16, 19 ], 
 [ 9, 18, 20, 11, 5, 16, 8, 19, 14, 13, 1 ] )}
gap> Size(h);
1
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> Size(h);
1
gap> f:=PartialPermNC([8,20], [8,20]);
<identity partial perm on [ 8, 20 ]>
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until Size(h)>1 or IsDoneIterator(iter);
gap> h;
{PartialPerm( [ 8, 20 ], [ 8, 20 ] )}
gap> f in h;
true
gap> f in s;
true
gap> hh:=HClass(s, f);
{PartialPerm( [ 8, 20 ], [ 8, 20 ] )}
gap> iter:=IteratorOfHClasses(s);
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until h=hh or IsDoneIterator(iter);     
gap> h;
{PartialPerm( [ 8, 20 ], [ 8, 20 ] )}
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
<iterator of H-class reps>
gap> s:=InverseSemigroup(Generators(s));
<inverse partial perm semigroup on 18 pts with 2 generators>
gap> iter:=IteratorOfHClassReps(s);
<iterator of H-class reps>
gap> i:=0;
0

#T# InverseTest4 
#JDM this breaks when i=38360 fairly reliably, it seems to be a problem in
# IsDoneIterator of iter. 

#gap> for f in iter do
#>   i := i + 1;
#>   if not f in s then
#>     Print("yikes");
#>     break;
#>   fi;
#>   if i = 38360 then
#>     break;
#>   fi;
#> od;
gap> iter:=IteratorOfHClasses(s);                                  
<iterator of H-classes>
gap> repeat h:=NextIterator(iter); until Size(h)>1 or IsDoneIterator(iter);
gap> h;
{PartialPerm( [ 8, 20 ], [ 8, 20 ] )}
gap> Size(h);
2
gap> Size(DClass(h));
40328
gap> RClass(h);
{PartialPerm( [ 8, 20 ], [ 8, 20 ] )}
gap> Size(last);
284
gap> 284^2;
80656
gap> d:=DClass(h);
{PartialPerm( [ 8, 20 ], [ 8, 20 ] )}
gap> IsGreensDClass(d);
true
gap> d;
{PartialPerm( [ 8, 20 ], [ 8, 20 ] )}
gap> Size(DClass(h))=Size(RClass(h))^2/2;
true

#T# InverseTest5 
gap> s:=Semigroup( [ Transformation( [ 3, 2, 1, 6, 5, 4 ] ), 
> Transformation( [ 4, 7, 3, 1, 6, 5, 7 ] ) ] );
<transformation semigroup on 7 pts with 2 generators>
gap> iso:=IsomorphismPartialPermSemigroup(s);;
gap> inv:=InverseGeneralMapping(iso);;
gap> f:=Transformation( [ 1, 7, 3, 4, 5, 6, 7 ] );;
gap> f^iso;
<identity partial perm on [ 1, 3, 4, 5, 6, 7 ]>
gap> (f^iso)^inv;
Transformation( [ 1, 7, 3, 4, 5, 6, 7 ] )
gap> ForAll(s, f-> (f^iso)^inv=f);
true

#T# InverseTest6 
gap> s:=Semigroup( Transformation( [ 2, 5, 1, 7, 3, 7, 7 ] ), 
> Transformation( [ 3, 6, 5, 7, 2, 1, 7 ] ) );;
gap> iso:=IsomorphismPartialPermSemigroup(s);;
gap> inv:=InverseGeneralMapping(iso);;
gap> f:=Transformation( [ 7, 1, 7, 7, 7, 7, 7 ] );;
gap> f^iso;
[2,1](7)
gap> (f^iso)^inv=f;
true
gap> f:=Random(s);;
gap> (f^iso)^inv=f;
true
gap> f:=Random(s);;
gap> (f^iso)^inv=f;
true
gap> f:=Random(s);;
gap> (f^iso)^inv=f;
true
gap> f:=Random(s);;
gap> (f^iso)^inv=f;
true
gap> f:=Random(s);;
gap> (f^iso)^inv=f;
true
gap> f:=Random(s);;
gap> (f^iso)^inv=f;
true

#T# InverseTest7  
# this is too slow, it used to work better! but the method was incorrect in
# general FIXME

#gap> ForAll(s, f-> (f^iso)^inv=f);
#true
gap> Size(Range(iso));
631
gap> ForAll(s, f-> f^iso in Range(iso));
true
gap> Size(s);
631

#T# InverseTest8 
gap> s:=InverseSemigroup( PartialPermNC( [ 1, 2, 3 ], [ 2, 4, 1 ] ),
> PartialPermNC( [ 1, 3, 4 ], [ 3, 4, 1 ] ) );;
gap> GreensDClasses(s);
[ {PartialPerm( [ 1, 2, 4 ], [ 1, 2, 4 ] )}, 
  {PartialPerm( [ 1, 3, 4 ], [ 1, 3, 4 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 2, 4 ] )}, {PartialPerm( [ 4 ], [ 4 ] )}, 
  {PartialPerm( [  ], [  ] )} ]
gap> GreensHClasses(s);
[ {PartialPerm( [ 1, 2, 4 ], [ 1, 2, 4 ] )}, 
  {PartialPerm( [ 1, 2, 4 ], [ 3, 1, 2 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 2, 4, 1 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 2, 3 ] )}, 
  {PartialPerm( [ 1, 3, 4 ], [ 1, 3, 4 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 2, 4 ] )}, {PartialPerm( [ 2, 4 ], [ 3, 1 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 1, 2 ] )}, {PartialPerm( [ 2, 4 ], [ 3, 2 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 4, 3 ] )}, {PartialPerm( [ 2, 4 ], [ 1, 4 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 4, 2 ] )}, {PartialPerm( [ 1, 3 ], [ 1, 3 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 2, 1 ] )}, {PartialPerm( [ 1, 3 ], [ 2, 3 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 3, 4 ] )}, {PartialPerm( [ 1, 3 ], [ 4, 1 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 2, 4 ] )}, {PartialPerm( [ 1, 2 ], [ 3, 1 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 1, 2 ] )}, {PartialPerm( [ 1, 2 ], [ 3, 2 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 4, 3 ] )}, {PartialPerm( [ 1, 2 ], [ 1, 4 ] )}, 
  {PartialPerm( [ 2, 3 ], [ 4, 2 ] )}, {PartialPerm( [ 2, 3 ], [ 1, 3 ] )}, 
  {PartialPerm( [ 2, 3 ], [ 2, 1 ] )}, {PartialPerm( [ 2, 3 ], [ 2, 3 ] )}, 
  {PartialPerm( [ 2, 3 ], [ 3, 4 ] )}, {PartialPerm( [ 2, 3 ], [ 4, 1 ] )}, 
  {PartialPerm( [ 3, 4 ], [ 4, 2 ] )}, {PartialPerm( [ 3, 4 ], [ 1, 3 ] )}, 
  {PartialPerm( [ 3, 4 ], [ 2, 1 ] )}, {PartialPerm( [ 3, 4 ], [ 2, 3 ] )}, 
  {PartialPerm( [ 3, 4 ], [ 3, 4 ] )}, {PartialPerm( [ 3, 4 ], [ 4, 1 ] )}, 
  {PartialPerm( [ 1, 4 ], [ 2, 4 ] )}, {PartialPerm( [ 1, 4 ], [ 3, 1 ] )}, 
  {PartialPerm( [ 1, 4 ], [ 1, 2 ] )}, {PartialPerm( [ 1, 4 ], [ 3, 2 ] )}, 
  {PartialPerm( [ 1, 4 ], [ 4, 3 ] )}, {PartialPerm( [ 1, 4 ], [ 1, 4 ] )}, 
  {PartialPerm( [ 4 ], [ 4 ] )}, {PartialPerm( [ 4 ], [ 1 ] )}, 
  {PartialPerm( [ 4 ], [ 3 ] )}, {PartialPerm( [ 4 ], [ 2 ] )}, 
  {PartialPerm( [ 1 ], [ 4 ] )}, {PartialPerm( [ 1 ], [ 1 ] )}, 
  {PartialPerm( [ 1 ], [ 3 ] )}, {PartialPerm( [ 1 ], [ 2 ] )}, 
  {PartialPerm( [ 3 ], [ 4 ] )}, {PartialPerm( [ 3 ], [ 1 ] )}, 
  {PartialPerm( [ 3 ], [ 3 ] )}, {PartialPerm( [ 3 ], [ 2 ] )}, 
  {PartialPerm( [ 2 ], [ 4 ] )}, {PartialPerm( [ 2 ], [ 1 ] )}, 
  {PartialPerm( [ 2 ], [ 3 ] )}, {PartialPerm( [ 2 ], [ 2 ] )}, 
  {PartialPerm( [  ], [  ] )} ]
gap> IsDuplicateFree(last);
true
gap> GreensLClasses(s);
[ {PartialPerm( [ 1, 2, 4 ], [ 1, 2, 4 ] )}, 
  {PartialPerm( [ 1, 2, 4 ], [ 3, 1, 2 ] )}, 
  {PartialPerm( [ 1, 3, 4 ], [ 1, 3, 4 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 2, 4 ] )}, {PartialPerm( [ 2, 4 ], [ 3, 1 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 1, 2 ] )}, {PartialPerm( [ 2, 4 ], [ 3, 2 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 4, 3 ] )}, {PartialPerm( [ 2, 4 ], [ 1, 4 ] )}, 
  {PartialPerm( [ 4 ], [ 4 ] )}, {PartialPerm( [ 4 ], [ 1 ] )}, 
  {PartialPerm( [ 4 ], [ 3 ] )}, {PartialPerm( [ 4 ], [ 2 ] )}, 
  {PartialPerm( [  ], [  ] )} ]
gap> GreensRClasses(s);
[ {PartialPerm( [ 1, 2, 4 ], [ 1, 2, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 2, 4, 1 ] )}, 
  {PartialPerm( [ 1, 3, 4 ], [ 1, 3, 4 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 2, 4 ] )}, {PartialPerm( [ 1, 3 ], [ 4, 2 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 2, 4 ] )}, {PartialPerm( [ 2, 3 ], [ 4, 2 ] )}, 
  {PartialPerm( [ 3, 4 ], [ 4, 2 ] )}, {PartialPerm( [ 1, 4 ], [ 2, 4 ] )}, 
  {PartialPerm( [ 4 ], [ 4 ] )}, {PartialPerm( [ 1 ], [ 4 ] )}, 
  {PartialPerm( [ 3 ], [ 4 ] )}, {PartialPerm( [ 2 ], [ 4 ] )}, 
  {PartialPerm( [  ], [  ] )} ]
gap> D:=GreensDClasses(s)[2];
{PartialPerm( [ 1, 3, 4 ], [ 1, 3, 4 ] )}
gap> GreensLClasses(D);
[ {PartialPerm( [ 1, 3, 4 ], [ 1, 3, 4 ] )} ]
gap> GreensRClasses(D);
[ {PartialPerm( [ 1, 3, 4 ], [ 1, 3, 4 ] )} ]
gap> GreensHClasses(D);
[ {PartialPerm( [ 1, 3, 4 ], [ 1, 3, 4 ] )} ]
gap> D:=GreensDClasses(s)[3];
{PartialPerm( [ 2, 4 ], [ 2, 4 ] )}
gap> GreensLClasses(D);
[ {PartialPerm( [ 2, 4 ], [ 2, 4 ] )}, {PartialPerm( [ 2, 4 ], [ 3, 1 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 1, 2 ] )}, {PartialPerm( [ 2, 4 ], [ 3, 2 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 4, 3 ] )}, {PartialPerm( [ 2, 4 ], [ 1, 4 ] )} ]
gap> GreensRClasses(D);
[ {PartialPerm( [ 2, 4 ], [ 2, 4 ] )}, {PartialPerm( [ 1, 3 ], [ 4, 2 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 2, 4 ] )}, {PartialPerm( [ 2, 3 ], [ 4, 2 ] )}, 
  {PartialPerm( [ 3, 4 ], [ 4, 2 ] )}, {PartialPerm( [ 1, 4 ], [ 2, 4 ] )} ]
gap> GreensHClasses(D);
[ {PartialPerm( [ 2, 4 ], [ 2, 4 ] )}, {PartialPerm( [ 2, 4 ], [ 3, 1 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 1, 2 ] )}, {PartialPerm( [ 2, 4 ], [ 3, 2 ] )}, 
  {PartialPerm( [ 2, 4 ], [ 4, 3 ] )}, {PartialPerm( [ 2, 4 ], [ 1, 4 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 4, 2 ] )}, {PartialPerm( [ 1, 3 ], [ 1, 3 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 2, 1 ] )}, {PartialPerm( [ 1, 3 ], [ 2, 3 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 3, 4 ] )}, {PartialPerm( [ 1, 3 ], [ 4, 1 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 2, 4 ] )}, {PartialPerm( [ 1, 2 ], [ 3, 1 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 1, 2 ] )}, {PartialPerm( [ 1, 2 ], [ 3, 2 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 4, 3 ] )}, {PartialPerm( [ 1, 2 ], [ 1, 4 ] )}, 
  {PartialPerm( [ 2, 3 ], [ 4, 2 ] )}, {PartialPerm( [ 2, 3 ], [ 1, 3 ] )}, 
  {PartialPerm( [ 2, 3 ], [ 2, 1 ] )}, {PartialPerm( [ 2, 3 ], [ 2, 3 ] )}, 
  {PartialPerm( [ 2, 3 ], [ 3, 4 ] )}, {PartialPerm( [ 2, 3 ], [ 4, 1 ] )}, 
  {PartialPerm( [ 3, 4 ], [ 4, 2 ] )}, {PartialPerm( [ 3, 4 ], [ 1, 3 ] )}, 
  {PartialPerm( [ 3, 4 ], [ 2, 1 ] )}, {PartialPerm( [ 3, 4 ], [ 2, 3 ] )}, 
  {PartialPerm( [ 3, 4 ], [ 3, 4 ] )}, {PartialPerm( [ 3, 4 ], [ 4, 1 ] )}, 
  {PartialPerm( [ 1, 4 ], [ 2, 4 ] )}, {PartialPerm( [ 1, 4 ], [ 3, 1 ] )}, 
  {PartialPerm( [ 1, 4 ], [ 1, 2 ] )}, {PartialPerm( [ 1, 4 ], [ 3, 2 ] )}, 
  {PartialPerm( [ 1, 4 ], [ 4, 3 ] )}, {PartialPerm( [ 1, 4 ], [ 1, 4 ] )} ]
gap> h:=last[9];;
gap> L:=LClass(D, Representative(h));
{PartialPerm( [ 2, 4 ], [ 1, 2 ] )}
gap> Position(HClasses(L), h);
2
gap> DClassOfLClass(L)=D;
true
gap> LClassOfHClass(h)=L;
true
gap> R:=RClassOfHClass(h);
{PartialPerm( [ 1, 3 ], [ 4, 2 ] )}
gap> Position(HClasses(R), h);
3
gap> DClassOfRClass(R)=D;
true

#T# InverseTest9 
gap> s:=InverseSemigroup(
> PartialPermNC( [ 1, 2, 3, 5 ], [ 1, 4, 6, 3 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 6 ], [ 3, 6, 4, 5, 1 ] ) );;
gap> f:=PartialPermNC([ 1, 4, 6 ], [ 6, 3, 1 ]);;
gap> D:=DClass(s, f);
{PartialPerm( [ 1, 3, 6 ], [ 1, 3, 6 ] )}
gap> LClass(s, f)=LClass(D, f);
true
gap> RClass(s, f)=RClass(D, f);
true
gap> R:=RClass(s, f);             
{PartialPerm( [ 1, 4, 6 ], [ 6, 3, 1 ] )}
gap> HClass(s, f)=HClass(R, f);
true
gap> HClass(D, f)=HClass(R, f);
true
gap> L:=LClass(s, f);                
{PartialPerm( [ 1, 3, 6 ], [ 1, 3, 6 ] )}
gap> HClass(D, f)=HClass(L, f);
true
gap> HClass(s, f)=HClass(L, f);
true
gap> 

#T# InverseTest10
gap> s:=POI(10);
<inverse partial perm monoid on 10 pts with 10 generators>
gap> f:=PartialPermNC([ 2, 4, 5, 7 ], [ 2, 3, 5, 7 ]);;
gap> l:=LClassNC(s, f);
{PartialPerm( [ 2, 3, 5, 7 ], [ 2, 3, 5, 7 ] )}
gap> l:=LClass(s,f);
{PartialPerm( [ 1, 2, 3, 4 ], [ 2, 3, 5, 7 ] )}
gap> s:=POI(15);
<inverse partial perm monoid on 15 pts with 15 generators>
gap> f:=PartialPermNC( [ 1, 3, 5, 8, 9, 10, 12, 13, 14 ],
> [ 2, 3, 4, 7, 9, 11, 12, 13, 15 ] ) ;;
gap> l:=LClass(s,f);
{PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 2, 3, 4, 7, 9, 11, 12, 13, 15 ]
 )}
gap> l:=LClassNC(s,f);
{PartialPerm( [ 2, 3, 4, 7, 9, 11, 12, 13, 15 ], 
 [ 2, 3, 4, 7, 9, 11, 12, 13, 15 ] )}
gap> s:=POI(15);;
gap> l:=LClassNC(s,f);
{PartialPerm( [ 2, 3, 4, 7, 9, 11, 12, 13, 15 ], 
 [ 2, 3, 4, 7, 9, 11, 12, 13, 15 ] )}
gap> l=LClass(s,f);
true
gap> f:=PartialPermNC([ 1, 2, 4, 7, 8, 11, 12 ], [ 1, 2, 6, 7, 9, 10, 11 ]);;
gap> l:=LClass(POI(12), f);
{PartialPerm( [ 1, 2, 3, 4, 5, 6, 7 ], [ 1, 2, 6, 7, 9, 10, 11 ] )}
gap> f:=PartialPermNC( [ 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13 ],
> [ 1, 2, 3, 4, 5, 7, 8, 9, 10, 12, 13 ] );;
gap> l:=LClass(POI(13), f);
{PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ], 
 [ 1, 2, 3, 4, 5, 7, 8, 9, 10, 12, 13 ] )}
gap> f:=PartialPermNC([ 1, 2, 3, 4, 7, 8, 9, 10 ], 
> [ 2, 3, 4, 5, 6, 8, 10, 11 ]);;
gap> l:=LClass(POI(13), f);
{PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 8 ], [ 2, 3, 4, 5, 6, 8, 10, 11 ] )}
gap> LClassNC(POI(13), f);
{PartialPerm( [ 2, 3, 4, 5, 6, 8, 10, 11 ], [ 2, 3, 4, 5, 6, 8, 10, 11 ] )}
gap> RClassNC(POI(13), f);
{PartialPerm( [ 1, 2, 3, 4, 7, 8, 9, 10 ], [ 2, 3, 4, 5, 6, 8, 10, 11 ] )}
gap> HClassNC(POI(13), f);       
{PartialPerm( [ 1, 2, 3, 4, 7, 8, 9, 10 ], [ 2, 3, 4, 5, 6, 8, 10, 11 ] )}
gap> DClassNC(POI(13), f);
{PartialPerm( [ 2, 3, 4, 5, 6, 8, 10, 11 ], [ 2, 3, 4, 5, 6, 8, 10, 11 ] )}
gap> s:=POI(13);
<inverse partial perm monoid on 13 pts with 13 generators>
gap> D:=DClassNC(s, f);
{PartialPerm( [ 2, 3, 4, 5, 6, 8, 10, 11 ], [ 2, 3, 4, 5, 6, 8, 10, 11 ] )}
gap> l:=LClassNC(s, f);
{PartialPerm( [ 2, 3, 4, 5, 6, 8, 10, 11 ], [ 2, 3, 4, 5, 6, 8, 10, 11 ] )}
gap> l:=LClass(s,f);
{PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 8 ], [ 2, 3, 4, 5, 6, 8, 10, 11 ] )}
gap> s:=POI(15);
<inverse partial perm monoid on 15 pts with 15 generators>
gap> f:=PartialPermNC( [ 1, 3, 5, 8, 9, 10, 12, 13, 14 ],
> [ 2, 3, 4, 7, 9, 11, 12, 13, 15 ] ) ;;
gap> l:=LClass(s,f);
{PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 2, 3, 4, 7, 9, 11, 12, 13, 15 ]
 )}
gap> l:=LClassNC(s,f);
{PartialPerm( [ 2, 3, 4, 7, 9, 11, 12, 13, 15 ], 
 [ 2, 3, 4, 7, 9, 11, 12, 13, 15 ] )}
gap> s:=POI(15);;
gap> l:=LClassNC(s,f);
{PartialPerm( [ 2, 3, 4, 7, 9, 11, 12, 13, 15 ], 
 [ 2, 3, 4, 7, 9, 11, 12, 13, 15 ] )}
gap> l=LClass(s,f);
true
gap> f:=PartialPermNC([ 1, 2, 4, 7, 8, 11, 12 ], [ 1, 2, 6, 7, 9, 10, 11 ]);;
gap> l:=LClass(POI(12), f);
{PartialPerm( [ 1, 2, 3, 4, 5, 6, 7 ], [ 1, 2, 6, 7, 9, 10, 11 ] )}
gap> f:=PartialPermNC( [ 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13 ],
> [ 1, 2, 3, 4, 5, 7, 8, 9, 10, 12, 13 ] );;
gap> l:=LClass(POI(13), f);
{PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ], 
 [ 1, 2, 3, 4, 5, 7, 8, 9, 10, 12, 13 ] )}
gap> f:=PartialPermNC([ 1, 2, 3, 4, 7, 8, 9, 10 ], 
> [ 2, 3, 4, 5, 6, 8, 10, 11 ]);;
gap> l:=LClass(POI(13), f);
{PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 8 ], [ 2, 3, 4, 5, 6, 8, 10, 11 ] )}
gap> LClassNC(POI(13), f);
{PartialPerm( [ 2, 3, 4, 5, 6, 8, 10, 11 ], [ 2, 3, 4, 5, 6, 8, 10, 11 ] )}
gap> RClassNC(POI(13), f);
{PartialPerm( [ 1, 2, 3, 4, 7, 8, 9, 10 ], [ 2, 3, 4, 5, 6, 8, 10, 11 ] )}
gap> HClassNC(POI(13), f);       
{PartialPerm( [ 1, 2, 3, 4, 7, 8, 9, 10 ], [ 2, 3, 4, 5, 6, 8, 10, 11 ] )}
gap> DClassNC(POI(13), f);
{PartialPerm( [ 2, 3, 4, 5, 6, 8, 10, 11 ], [ 2, 3, 4, 5, 6, 8, 10, 11 ] )}
gap> s:=POI(13);
<inverse partial perm monoid on 13 pts with 13 generators>
gap> D:=DClassNC(s, f);
{PartialPerm( [ 2, 3, 4, 5, 6, 8, 10, 11 ], [ 2, 3, 4, 5, 6, 8, 10, 11 ] )}
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
{PartialPerm( [ 1, 2, 3, 4, 7, 8, 9, 10 ], [ 2, 3, 4, 5, 6, 8, 10, 11 ] )}
gap> HClass(s, f)=HClass(R, f);
true
gap> HClassNC(s, f)=HClass(R, f);
true
gap> HClassNC(s, f)=HClassNC(R, f);
true
gap> HClass(s, f)=HClassNC(R, f);  
true
gap> L:=LClassNC(s, f);
{PartialPerm( [ 2, 3, 4, 5, 6, 8, 10, 11 ], [ 2, 3, 4, 5, 6, 8, 10, 11 ] )}
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

#T# InverseTest11
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
[ <identity partial perm on 
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 2\
1, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,\
 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 6\
0, 61, 62, 63, 64 ]>, 
  <identity partial perm on 
    [ 3, 4, 7, 8, 11, 13, 15, 16, 19, 22, 25, 26, 28, 29, 31, 32, 35, 39, 43, \
44, 45, 49, 50, 51, 55, 56, 57, 59, 60, 61, 63, 64 ]>, 
  <identity partial perm on 
    [ 4, 8, 13, 16, 22, 28, 29, 32, 39, 49, 50, 51, 59, 60, 61, 64 ]>, 
  <identity partial perm on [ 8, 16, 28, 32, 49, 59, 60, 64 ]>, 
  <identity partial perm on [ 16, 32, 59, 64 ]>, 
  <identity partial perm on [ 32, 64 ]>, <identity partial perm on [ 64 ]> ]
gap> NrLClasses(m);
64
gap> IsRTrivial(m);
false
gap> Size(m);
13327
gap> f:=PartialPermNC([ 27, 30, 31, 32, 58, 62, 63, 64 ],
> [ 8, 16, 28, 60, 49, 59, 32, 64 ]);;
gap> d:=DClassNC(m, f);
{PartialPerm( [ 8, 16, 28, 32, 49, 59, 60, 64 ], 
 [ 8, 16, 28, 32, 49, 59, 60, 64 ] )}
gap> LClassReps(d);
[ <identity partial perm on [ 8, 16, 28, 32, 49, 59, 60, 64 ]>, 
  [8,13][28,29][49,50][60,61](16)(32)(59)(64), 
  [8,22][16,28,29][49,51][59,60,61](32)(64), 
  [8,14][28,30][49,52][60,62](16)(32)(59)(64), 
  [8,39][16,49,51][28,50][32,59,60,61](64), [8,23][16,28,30][49,53][59,60,62]
    (32)(64), [8,15][28,31][49,55][60,63](16)(32)(59)(64), 
  [8,40][16,49,53][28,52][32,59,60,62](64), [8,24][16,29][28,30][49,54][59,61]
    [60,62](32)(64), [8,25][16,28,31][49,56][59,60,63](32)(64), 
  [8,41][16,50][28,52][32,59,61][49,54][60,62](64), 
  [8,43][16,49,56][28,55][32,59,60,63](64), [8,26][16,29][28,31][49,57][59,61]
    [60,63](32)(64), [8,42][16,51][28,53][32,60,62][49,54][59,61](64), 
  [8,44][16,50][28,55][32,59,61][49,57][60,63](64), 
  [8,27][16,30][28,31][49,58][59,62][60,63](32)(64), 
  [8,45][16,51][28,56][32,60,63][49,57][59,61](64), 
  [8,46][16,52][28,55][32,59,62][49,58][60,63](64), 
  [8,47][16,53][28,56][32,60,63][49,58][59,62](64), 
  [8,48][16,54][28,57][32,61][49,58][59,62][60,63](64) ]
gap> List(DClasses(m), NrRClasses);
[ 1, 6, 15, 20, 15, 6, 1 ]
gap> d:=DClasses(m)[6]; 
{PartialPerm( [ 32, 64 ], [ 32, 64 ] )}
gap> LClassReps(d);
[ <identity partial perm on [ 32, 64 ]>, [32,59](64), [32,60](64), 
  [32,61](64), [32,63](64), [32,62](64) ]
gap> RClassReps(d);                              
[ <identity partial perm on [ 32, 64 ]>, [59,32](64), [60,32](64), 
  [61,32](64), [63,32](64), [62,32](64) ]
gap> d:=DClassNC(m, Representative(d));
{PartialPerm( [ 32, 64 ], [ 32, 64 ] )}
gap> LClassReps(d);
[ <identity partial perm on [ 32, 64 ]>, [32,59](64), [32,60](64), 
  [32,61](64), [32,62](64), [32,63](64) ]
gap> RClassReps(m);             
[ <identity partial perm on 
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 2\
1, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,\
 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 6\
0, 61, 62, 63, 64 ]>, 
  <identity partial perm on 
    [ 3, 4, 7, 8, 11, 13, 15, 16, 19, 22, 25, 26, 28, 29, 31, 32, 35, 39, 43, \
44, 45, 49, 50, 51, 55, 56, 57, 59, 60, 61, 63, 64 ]>, 
  [2,3][6,7][10,11][14,15][18,19][23,25][24,26][30,31][34,35][40,43][41,44]
    [42,45][52,55][53,56][54,57][62,63](4)(8)(13)(16)(22)(28)(29)(32)(39)(49)
    (50)(51)(59)(60)(61)(64), [5,3][6,4][12,11][14,13][20,19][23,22][27,26]
    [30,29][36,35][40,39][46,44][47,45][52,50][53,51][58,57][62,61](7)(8)(15)
    (16)(25)(28)(31)(32)(43)(49)(55)(56)(59)(60)(63)(64), 
  [9,3][10,4][12,11,7][14,13,8][21,19][24,22][27,26,25][30,29,28][37,35]
    [41,39][46,44,43][48,45][52,50,49][54,51][58,57,56][62,61,60](15)(16)(31)
    (32)(55)(59)(63)(64), [17,3][18,4][20,11][21,19,7][23,13][24,22,8]
    [27,26,25,15][30,29,28,16][38,35][42,39][47,44][48,45,43][53,50][54,51,49]
    [58,57,56,55][62,61,60,59](31)(32)(63)(64), 
  [33,3][34,4][36,11][37,19][38,35,7][40,13][41,22][42,39,8][46,26][47,44,25]
    [48,45,43,15][52,29][53,50,28][54,51,49,16][58,57,56,55,31]
    [62,61,60,59,32](63)(64), 
  <identity partial perm on 
    [ 4, 8, 13, 16, 22, 28, 29, 32, 39, 49, 50, 51, 59, 60, 61, 64 ]>, 
  [7,4][15,13][25,22][31,29][43,39][55,50][56,51][63,61](8)(16)(28)(32)(49)
    (59)(60)(64), [6,4][14,13][23,22][30,29][40,39][52,50][53,51][62,61](8)
    (16)(28)(32)(49)(59)(60)(64), [11,4][15,13,8][26,22][31,29,28][44,39]
    [55,50,49][57,51][63,61,60](16)(32)(59)(64), 
  [10,4][14,13,8][24,22][30,29,28][41,39][52,50,49][54,51][62,61,60](16)(32)
    (59)(64), [19,4][25,13][26,22,8][31,29,28,16][45,39][56,50][57,51,49]
    [63,61,60,59](32)(64), [18,4][23,13][24,22,8][30,29,28,16][42,39][53,50]
    [54,51,49][62,61,60,59](32)(64), [12,4][14,8][15,13][27,22][30,28][31,29]
    [46,39][52,49][55,50][58,51][62,60][63,61](16)(32)(59)(64), 
  [35,4][43,13][44,22][45,39,8][55,29][56,50,28][57,51,49,16][63,61,60,59,32]
    (64), [34,4][40,13][41,22][42,39,8][52,29][53,50,28][54,51,49,16]
    [62,61,60,59,32](64), [20,4][23,8][25,13][27,22][30,28,16][31,29][47,39]
    [53,49][56,50][58,51][62,60,59][63,61](32)(64), 
  [36,4][40,8][43,13][46,22][47,39][52,28][53,49,16][55,29][56,50][58,51]
    [62,60,59,32][63,61](64), [21,4][24,8][26,13][27,22][30,28][31,29,16]
    [48,39][54,49][57,50][58,51][62,60][63,61,59](32)(64), 
  [37,4][41,8][44,13][46,22][48,39][52,28][54,49][55,29][57,50,16][58,51]
    [62,60][63,61,59,32](64), [38,4][42,8][45,13][47,22][48,39][53,28][54,49]
    [56,29][57,50][58,51,16][62,60,32][63,61,59](64), 
  <identity partial perm on [ 8, 16, 28, 32, 49, 59, 60, 64 ]>, 
  [13,8][29,28][50,49][61,60](16)(32)(59)(64), 
  [22,8][29,28,16][51,49][61,60,59](32)(64), 
  [15,8][31,28][55,49][63,60](16)(32)(59)(64), 
  [14,8][30,28][52,49][62,60](16)(32)(59)(64), 
  [39,8][50,28][51,49,16][61,60,59,32](64), [25,8][31,28,16][56,49][63,60,59]
    (32)(64), [23,8][30,28,16][53,49][62,60,59](32)(64), 
  [43,8][55,28][56,49,16][63,60,59,32](64), 
  [40,8][52,28][53,49,16][62,60,59,32](64), [26,8][29,16][31,28][57,49][61,59]
    [63,60](32)(64), [24,8][29,16][30,28][54,49][61,59][62,60](32)(64), 
  [44,8][50,16][55,28][57,49][61,59,32][63,60](64), 
  [41,8][50,16][52,28][54,49][61,59,32][62,60](64), 
  [27,8][30,16][31,28][58,49][62,59][63,60](32)(64), 
  [45,8][51,16][56,28][57,49][61,59][63,60,32](64), 
  [42,8][51,16][53,28][54,49][61,59][62,60,32](64), 
  [46,8][52,16][55,28][58,49][62,59,32][63,60](64), 
  [47,8][53,16][56,28][58,49][62,59][63,60,32](64), 
  [48,8][54,16][57,28][58,49][61,32][62,59][63,60](64), 
  <identity partial perm on [ 16, 32, 59, 64 ]>, [28,16][60,59](32)(64), 
  [49,16][60,59,32](64), [29,16][61,59](32)(64), [50,16][61,59,32](64), 
  [31,16][63,59](32)(64), [30,16][62,59](32)(64), [51,16][60,32][61,59](64), 
  [55,16][63,59,32](64), [52,16][62,59,32](64), [56,16][60,32][63,59](64), 
  [53,16][60,32][62,59](64), [57,16][61,32][63,59](64), 
  [54,16][61,32][62,59](64), [58,16][62,32][63,59](64), 
  <identity partial perm on [ 32, 64 ]>, [59,32](64), [60,32](64), 
  [61,32](64), [63,32](64), [62,32](64), <identity partial perm on [ 64 ]> ]
gap> RClassReps(d);
[ <identity partial perm on [ 32, 64 ]>, [59,32](64), [60,32](64), 
  [61,32](64), [62,32](64), [63,32](64) ]
gap> Size(d);
36
gap> Size(DClasses(m)[6]);
36

#T# InverseTest12
gap> s:=InverseSemigroup( [ PartialPermNC( [ 1, 2, 3, 5 ], [ 2, 1, 6, 3 ] ),
> PartialPermNC( [ 1, 2, 3, 6 ], [ 3, 5, 2, 6 ] ) ]);;
gap> f:=PartialPermNC([ 1 .. 3 ], [ 6, 3, 1 ]);;
gap> d:=DClassNC(s, f);
{PartialPerm( [ 1, 3, 6 ], [ 1, 3, 6 ] )}
gap> GroupHClass(d);
{PartialPerm( [ 1, 3, 6 ], [ 1, 3, 6 ] )}
gap> StructureDescription(last);
"1"
gap> ForAny(DClasses(s), x-> not IsTrivial(GroupHClass(x)));
true
gap> First(DClasses(s), x-> not IsTrivial(GroupHClass(x)));
{PartialPerm( [ 1, 2 ], [ 1, 2 ] )}
gap> StructureDescription(GroupHClass(last));
"C2"

#T# InverseTest13
gap> s:=InverseSemigroup(
> [ PartialPermNC( [ 1, 2, 3, 4, 5, 7 ], [ 10, 6, 3, 4, 9, 1 ] ),
> PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8 ], [ 6, 10, 7, 4, 8, 2, 9, 1 ] ) ]);;
gap> Idempotents(s, 1);
[ <identity partial perm on [ 4 ]> ]
gap> Idempotents(s, 0);
[  ]
gap> PartialPermNC([]) in s; 
false
gap> Idempotents(s, 2);
[ <identity partial perm on [ 3, 4 ]>, <identity partial perm on [ 4, 7 ]>, 
  <identity partial perm on [ 2, 4 ]>, <identity partial perm on [ 4, 10 ]>, 
  <identity partial perm on [ 1, 4 ]>, <identity partial perm on [ 4, 9 ]>, 
  <identity partial perm on [ 4, 8 ]>, <identity partial perm on [ 4, 6 ]>, 
  <identity partial perm on [ 4, 5 ]> ]
gap> Idempotents(s, 10);
[  ]
gap> f:=PartialPermNC( [ 2, 4, 9, 10 ], [ 7, 4, 3, 2 ] );;
gap> r:=RClassNC(s, f);
{PartialPerm( [ 2, 4, 9, 10 ], [ 7, 4, 3, 2 ] )}
gap> Idempotents(r);
[ <identity partial perm on [ 2, 4, 9, 10 ]> ]

#T# InverseTest14
gap> s := InverseSemigroup([
> PartialPerm( [ 1, 2, 3, 4, 5, 6, 9 ], [ 1, 5, 9, 2, 6, 10, 7 ] ),
> PartialPerm( [ 1, 3, 4, 7, 8, 9 ], [ 9, 4, 1, 6, 2, 8 ] )]);
<inverse partial perm semigroup on 10 pts with 2 generators>
gap> ForAll(RClasses(s), IsRegularClass);
true

#T# InverseTest15
gap> s:=InverseSemigroup( 
> PartialPermNC( [ 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 15 ], 
> [ 6, 4, 18, 3, 11, 8, 5, 14, 19, 13, 12, 20, 1 ] ),
> PartialPermNC( [ 1, 2, 4, 5, 6, 7, 9, 10, 11, 12, 15, 16, 18, 20 ], 
> [ 1, 18, 3, 7, 4, 9, 19, 5, 14, 16, 12, 17, 15, 6 ] ) );;
gap> iter:=IteratorOfDClassReps(s);
<iterator of D-class reps>
gap> NextIterator(iter);
<identity partial perm on [ 1, 3, 4, 5, 6, 8, 11, 12, 13, 14, 18, 19, 20 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 3, 4, 5, 6, 7, 9, 12, 14, 15, 16, 17, 18, 19 ]>
gap> NextIterator(iter);
<identity partial perm on [ 3, 5, 6, 11, 12, 13, 18, 20 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 3, 4, 6, 7, 14, 15, 16 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 2, 4, 6, 9, 10, 11, 15, 20 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 3, 6, 8, 11, 14, 18, 20 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 3, 4, 7, 9, 12, 15, 16, 17, 19 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 3, 4, 5, 9, 12, 14, 18, 19 ]>
gap> NextIterator(iter);
<identity partial perm on [ 11, 12, 13, 18 ]>
gap> NextIterator(iter);
<identity partial perm on [ 4, 6, 7, 14, 15, 16 ]>
gap> NextIterator(iter);
<identity partial perm on [ 2, 4, 10, 15, 20 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 3, 6, 8, 11, 18, 20 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 3, 4, 9, 12, 17 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 2, 4, 9, 15 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 3, 4, 6, 11, 13, 14, 19 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 6, 7, 18, 20 ]>
gap> NextIterator(iter);
<identity partial perm on [ 5, 6, 11, 13, 18, 20 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 4, 6, 14, 15 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 2, 4, 11, 20 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 3, 6, 8, 14, 18 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 3, 9, 12, 16, 17, 19 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 3, 5, 12, 18, 19 ]>
gap> NextIterator(iter);
<identity partial perm on [ 3, 6, 14, 18, 20 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 3, 7, 15, 16, 19 ]>
gap> NextIterator(iter);
<identity partial perm on [ 12, 13 ]>
gap> NextIterator(iter);
<identity partial perm on [ 14, 15, 16 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 3, 8, 11, 20 ]>
gap> NextIterator(iter);
<identity partial perm on [ 3, 4, 9, 12, 17 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 2, 9 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 3, 4, 19 ]>
gap> NextIterator(iter);
<identity partial perm on [ 5, 6, 11, 13, 18 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 2, 4, 20 ]>
gap> NextIterator(iter);
<identity partial perm on [ 3, 6, 14, 18 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 3, 16, 19 ]>
gap> NextIterator(iter);
<identity partial perm on [ 2, 4, 13, 15 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 6, 7, 18 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 2, 6, 10, 15, 20 ]>
gap> NextIterator(iter);
<identity partial perm on [ 3, 6, 11, 12, 13, 18, 20 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 11, 18, 20 ]>
gap> NextIterator(iter);
<identity partial perm on [ 11, 12, 13 ]>
gap> NextIterator(iter);
<identity partial perm on [ 2, 10, 20 ]>
gap> NextIterator(iter);
<identity partial perm on [ 3, 4, 6, 13 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 4, 15 ]>
gap> NextIterator(iter);
<identity partial perm on [ 6, 14, 18 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 16, 17, 19 ]>
gap> NextIterator(iter);
<identity partial perm on [ 1, 7, 15, 16 ]>
gap> s:=RandomInverseSemigroup(2,20);;
gap> iter:=IteratorOfDClassReps(s);
<iterator of D-class reps>
gap> s:=RandomInverseSemigroup(2,100);;
gap> iter:=IteratorOfLClassReps(s);
<iterator of L-class reps>
gap> for i in [1..10000] do NextIterator(iter); od;
gap> s:=RandomInverseSemigroup(2,10);;
gap> iter:=IteratorOfLClassReps(s);
<iterator of L-class reps>
gap> for i in iter do od;
gap> iter:=IteratorOfDClassReps(s);
<iterator of D-class reps>
gap> for i in iter do od;
gap> iter:=IteratorOfRClassReps(s);
<iterator of R-class reps>
gap> for i in iter do od;

#T# InverseTest16
gap> s:=RandomInverseSemigroup(100,100);
<inverse partial perm semigroup on 100 pts with 100 generators>
gap> iter:=IteratorOfRClasses(s);       
<iterator of R-classes>
gap> for i in [1..100] do NextIterator(iter); od;
gap> iter:=IteratorOfLClasses(s);      
<iterator of L-classes>
gap> for i in [1..100] do NextIterator(iter); od;

#T# InverseTest17
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
{PartialPerm( [ 1, 2, 3, 5, 7 ], [ 1, 2, 3, 5, 7 ] )}
gap> NrIdempotents(l);
1
gap> DClass(l);
{PartialPerm( [ 1, 2, 3, 5, 7 ], [ 1, 2, 3, 5, 7 ] )}
gap> last=d;
true
gap> NrIdempotents(last2);
13

#T# InverseTest18
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

#T# InverseTest19
gap> s:=InverseSemigroup(
> [ PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] ),
>  PartialPerm( [ 1, 2, 3, 4, 5, 7, 8, 9 ], [ 1, 2, 3, 4, 5, 7, 8, 9 ] ), 
>  PartialPerm( [ 1, 3, 4, 7, 8, 9 ], [ 1, 3, 4, 7, 8, 9 ] ), 
>  PartialPerm( [ 3, 7, 8, 9 ], [ 2, 7, 8, 9 ] ), 
>  PartialPerm( [ 1, 7, 9 ], [ 1, 7, 9 ] ), 
>  PartialPerm( [ 1, 7, 9 ], [ 8, 7, 9 ] ) ]);;
gap> Size(s);
12
gap> IsDTrivial(s);
false

#T# InverseTest20 
gap> IsIsometryPP:=function(f)
> local n, i, j, k, l;
>  n:=RankOfPartialPerm(f);
>  for i in [1..n-1] do
>    k:=DomainOfPartialPerm(f)[i];
>    for j in [i+1..n] do
>      l:=DomainOfPartialPerm(f)[j];
>      if not AbsInt(k^f-l^f)=AbsInt(k-l) then
>        return false;
>      fi;
>    od;
>  od;
>  return true;
> end;;
gap> s:=InverseSubsemigroupByProperty(SymmetricInverseSemigroup(5),
> IsIsometryPP);;
gap> Size(s);
142
gap> s:=InverseSubsemigroupByProperty(SymmetricInverseSemigroup(6),
> IsIsometryPP);;
gap> Size(s);
319
gap> s:=InverseSubsemigroupByProperty(SymmetricInverseSemigroup(7),
> IsIsometryPP);;
gap> Size(s);
686

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(d);
gap> Unbind(f);
gap> Unbind(S);
gap> Unbind(h);
gap> Unbind(inv);
gap> Unbind(j);
gap> Unbind(m);
gap> Unbind(l);
gap> Unbind(iter);
gap> Unbind(gens);
gap> Unbind(i);
gap> Unbind(s);
gap> Unbind(r);
gap> Unbind(iso);
gap> Unbind(n);
gap> Unbind(R);
gap> Unbind(IsIsometryPP);
gap> Unbind(hh);
gap> Unbind(k);
gap> Unbind(L);
gap> Unbind(D);

#E# 
gap> STOP_TEST("Semigroups package: inverse.tst");
