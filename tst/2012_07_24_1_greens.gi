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
<semigroup with 10 generators>
gap> s:=RandomBlockGroup(10, 15);
<semigroup with 10 generators>
gap> s:=RandomBlockGroup(20, 10);
<semigroup with 20 generators>
gap> Size(s);
Error, user interrupt in
  val := HTValue( lambdarhoht, rhoy ); called from 
Enumerate( SemigroupData( s ), infinity, ReturnFalse ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 114 of *stdin*
you can 'return;'
brk> SemigroupData(s);
<semigroup data: 192799 reps, 13350 lambda-rho values>
brk> Size(last);
875432
brk> quit;
gap> s:=RandomBlockGroup(100, 10);
<semigroup with 100 generators>
gap> Size(last);
Error, user interrupt in
  x := [ s, m, o, y, nr + 1 ]; called from 
Enumerate( SemigroupData( s ), infinity, ReturnFalse ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 115 of *stdin*
you can 'return;'
brk> s;
<semigroup with 100 generators>
brk> SemigroupData(s);
<semigroup data: 60625 reps, 6612 lambda-rho values>
brk> Size(last);
30381694
brk> SemigroupData(s)!.nr;
Error, Record: '<rec>.nr' must have an assigned value in
  <compiled or corrupted statement>  called from 
Enumerate( SemigroupData( s ), infinity, ReturnFalse ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 4 of *errin*
you can 'return;' after assigning a value
brk_2> quit;
brk> SemigroupData(s)!.pos;
0
brk> pos;
589
brk> quit;
gap> s:=RandomBlockGroup(10, 8);
<semigroup with 10 generators>
gap> Size(s);
17839
gap> s:=RandomBlockGroup(10, 10);
<semigroup with 10 generators>
gap> Size(s);
160088
gap> time;
7903
gap> NrRClasses(s);
104062
gap> List(GeneratorsOfGroup(SymmetricGroup(10)), AsPartialPerm);
[ [ 1 .. 10 ] -> [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1 ], [ 1, 2 ] -> [ 2, 1 ] ]
gap> List(GeneratorsOfGroup(SymmetricGroup(10)), x-> AsPartialPerm(x, 10));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `AsPartialPerm' on 2 arguments called from
AsPartialPerm( x, 10 ) called from
func( C[i] ) called from
<function "List">( <arguments> )
 called from read-eval loop at line 122 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> List(GeneratorsOfGroup(SymmetricGroup(10)), x-> AsPartialPerm(x, [1..10]));
[ [ 1 .. 10 ] -> [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1 ], 
  [ 1 .. 10 ] -> [ 2, 1, 3, 4, 5, 6, 7, 8, 9, 10 ] ]
gap> gens:=last;
[ [ 1 .. 10 ] -> [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1 ], 
  [ 1 .. 10 ] -> [ 2, 1, 3, 4, 5, 6, 7, 8, 9, 10 ] ]
gap> Append(gens, RandomPartialPerm(10));
Error, AppendList: <list2> must be a small list (not a object (data))
not in any function at line 124 of *stdin*
you can replace <list2> via 'return <list2>;'
brk> quit;
gap> Add(gens, RandomPartialPerm(10));
gap> gens;
[ [ 1 .. 10 ] -> [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1 ], 
  [ 1 .. 10 ] -> [ 2, 1, 3, 4, 5, 6, 7, 8, 9, 10 ], 
  [ 1, 2, 4, 7, 10 ] -> [ 8, 5, 9, 6, 7 ] ]
gap> Display(gens);
[ PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], 
[ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], 
[ 2, 1, 3, 4, 5, 6, 7, 8, 9, 10 ] ),
 PartialPermNC( [ 1, 2, 4, 7, 10 ], [ 8, 5, 9, 6, 7 ] ) ]
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> Size(s);
12398231
gap> NrRClasses(s);
639
gap> f:=Random(s);
[ 3, 9 ] -> [ 5, 4 ]
gap> d:=DClass(s, f);
{[ 3, 9 ] -> [ 7, 6 ]}
gap> Position(LambdaOrb(d), RanSetPP(Representative(d)));
7
gap> OrbSCC(RhoOrb(d))[RhoOrbSCCIndex(d)];
[ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 
  22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 
  41, 42, 43, 44, 45 ]
gap> OrbSCC(LambdaOrb(d))[LambdaOrbSCCIndex(d)];
[ 7, 13, 20, 27, 34, 36, 45, 48, 49, 53, 62, 65, 66, 67, 71, 73, 86, 89, 90, 
  92, 95, 110, 113, 114, 115, 116, 119, 120, 137, 139, 142, 143, 147, 148, 
  170, 172, 173, 177, 178, 208, 209, 214, 244, 245, 278 ]
gap> NrIdempotents(d);
45
gap> Number(Idempotents(s), x-> x in d);
45
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> d:=DClass(s, f);
{[ 3, 9 ] -> [ 7, 6 ]}
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> d:=DClassNC(s, f);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> NrIdempotents(d);
45
gap> Number(Idempotents(s), x-> x in d);
45
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> l:=LClass(s, f);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> d:=DClassOfLClass(l);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> NrIdempotents(d);
45
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> l:=LClass(s, f);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> l:=LClassNC(s, f);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> d:=DClassOfLClass(l);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> NrIdempotents(d);
45
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> r:=RClass(s, f);
{[ 3, 9 ] -> [ 6, 7 ]}
gap> d:=DClassOfRClass(r);
{[ 3, 9 ] -> [ 6, 7 ]}
gap> NrIdempotents(d);
45
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> r:=RClassNC(s, f);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> d:=DClassOfRClass(r);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> NrIdempotents(d);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `RhoOrbSCCIndex' on 1 arguments called from
RhoOrbSCCIndex( x ) called from
RhoOrbSCC( d ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 160 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s;=Semigroup(Concatenation(Generators(last)));
<semigroup with 3 generators>
Syntax error: expression expected
s;=Semigroup(Concatenation(Generators(last)));
  ^
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> r:=RClassNC(s, f);
{[ 3, 9 ] -> [ 5, 4 ]}
gap> d:=DClassOfRClass(r);
{[ 3, 9 ] -> [ 5, 4 ]}
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
gap> d:=Random(DClasses(s));
{Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] )}
gap> NrHClasses(d);
1
gap> GroupHClass(d);
{Transformation( [ 1, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] )}
gap> last=d;
true
gap> Size(d);
7
gap> Size(last3);
7
gap> s:=RandomBlockGroup(2, 12);
<semigroup with 2 generators>
gap> Size(s);
251
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 3, 5, 6, 7, 12 ], [ 11, 10, 3, 4, 6, 2, 8 ] ),
 PartialPermNC( [ 1, 2, 4, 5, 6, 8, 9, 10, 11 ], 
[ 2, 8, 1, 10, 11, 4, 7, 6, 9 ] ) ]
gap> d:=Random(DClasses(s));
{[ 5, 12 ] -> [ 6, 9 ]}
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
gap> s:=RandomBlockGroup(2, 12);
<semigroup with 2 generators>
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 3, 4, 9, 10, 11 ], [ 4, 1, 7, 12, 3, 9, 6 ] ),
 PartialPermNC( [ 1, 3, 4, 5, 7, 8, 11, 12 ], [ 4, 11, 2, 7, 9, 8, 1, 6 ] ) ]
gap> f:=Random(s);
[ 4, 7, 11 ] -> [ 2, 9, 6 ]
gap> d:=DClassNC(s, f);
{[ 4, 7, 11 ] -> [ 2, 9, 6 ]}
gap> NrHClasses(s);
125
gap> d:=DClass(s, f);
{[ 4, 7, 11 ] -> [ 2, 9, 6 ]}
gap> NrHClasses(s);
125
gap> NrHClasses(d);
1
gap> d:=DClassNC(s, f);
{[ 4, 7, 11 ] -> [ 2, 9, 6 ]}
gap> NrHClasses(d);
1
gap> d:=DClass(LClass(s, f));
{[ 4, 7, 11 ] -> [ 2, 9, 6 ]}
gap> NrHClasses(d);
1
gap> d:=DClass(RClass(s, f));
{[ 4, 7, 11 ] -> [ 2, 9, 6 ]}
gap> NrHClasses(d);
1
gap> NrRegularDClasses(s);
4
gap> NrDClasses(s);
65
gap> RClassReps(d);
[ [ 4, 7, 11 ] -> [ 2, 9, 6 ] ]
gap> iter:=IteratorOfDClasses(s);
<iterator of D-classes>
gap> repeat d:=NextIterator(iter); until IsDoneIterator(iter) or Size(d)>1000;
gap> d;
{[ 1, 5 ] -> [ 6, 4 ]}
gap> Size(d);
1
gap> IsDTrivial(s);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `IsRTrivial' on 1 arguments called from
<function "HANDLE_METHOD_NOT_FOUND">( <arguments> )
 called from read-eval loop at line 213 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> List(DClasses(s), Size));
Syntax error: ; expected
List(DClasses(s), Size));
                       ^
gap> List(DClasses(s), Size);
[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 3, 9, 1, 1, 1, 3, 1, 1, 1, 1, 
  1, 3, 9, 1, 3, 3, 3, 3, 1, 3, 3, 1, 3, 1, 3, 1, 1, 1, 1, 1, 3, 3, 1, 3, 1, 
  3, 3, 9, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
gap> Position(last, 9);
17
gap> d:=DClasses(s)[17];
{[ 2, 9 ] -> [ 4, 7 ]}
gap> Size(d);
9
gap> IsRegularDClass(d);
true
gap> RClassReps(d);
[ [ 2, 9 ] -> [ 4, 7 ], [ 1, 3 ] -> [ 4, 7 ], <identity on [ 4, 7 ]> ]
gap> d:=DClassNC(s, Representative(d));
{[ 2, 9 ] -> [ 4, 7 ]}
gap> RClassReps(d);
[ [ 2, 9 ] -> [ 4, 7 ], <identity on [ 4, 7 ]>, [ 1, 3 ] -> [ 4, 7 ] ]
gap> s:=Semigroup(Generators(s));
<semigroup with 2 generators>
gap> d:=DClass(HClass(s, Representative(d)));
{[ 2, 9 ] -> [ 4, 7 ]}
gap> RClassReps(d);
[ [ 2, 9 ] -> [ 4, 7 ], <identity on [ 4, 7 ]>, [ 1, 3 ] -> [ 4, 7 ] ]
gap> Size(d);
9
gap> Number(s, x-> x in d);
9
gap> ForAll(d, x-> x in d);
true
gap> HClassReps(d);
[ [ 2, 9 ] -> [ 4, 7 ], <identity on [ 2, 9 ]>, [ 2, 9 ] -> [ 1, 3 ], 
  <identity on [ 4, 7 ]>, [ 4, 7 ] -> [ 2, 9 ], [ 4, 7 ] -> [ 1, 3 ], 
  [ 1, 3 ] -> [ 4, 7 ], [ 1, 3 ] -> [ 2, 9 ], <identity on [ 1, 3 ]> ]
gap> Set(last)=Elements(d);
true
gap> gens:=[Transformation([2,1,4,5,3,7,8,9,10,6]),
> Transformation([1,2,4,3,5,6,7,8,9,10]),
> Transformation([1,2,3,4,5,6,10,9,8,7]),
> Transformation([9,1,4,3,6,9,3,4,3,9])];;
gap> s:=Monoid(gens);;
gap> 
gap> f:=Random(s);
Transformation( [ 2, 1, 3, 5, 4, 10, 9, 8, 7, 6 ] )
gap> d:=DClass(HClass(s, Representative(d)));
Error, the element does not belong to the semigroup, called from
GreensHClassOfElement( arg[1], arg[2] ) called from
<function "HClass">( <arguments> )
 called from read-eval loop at line 237 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> d:=DClass(HClass(s, f));
{Transformation( [ 2, 1, 3, 5, 4, 10, 9, 8, 7, 6 ] )}
gap> Size(d);
120
gap> HClassReps(d);
[ Transformation( [ 2, 1, 3, 5, 4, 10, 9, 8, 7, 6 ] ) ]
gap> h:=GroupHClass(d);
{Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] )}
gap> h=d;
true
gap> f:=Random(s);
Transformation( [ 1, 2, 5, 3, 4, 8, 9, 10, 6, 7 ] )
gap> Size(s);
491558
gap> f:=Random(s);
Transformation( [ 6, 6, 3, 6, 4, 6, 6, 6, 6, 4 ] )
gap> d:=DClass(HClass(s, f));
{Transformation( [ 9, 9, 4, 9, 3, 9, 9, 9, 9, 3 ] )}
gap> Size(d);
121500
gap> NrHClasses(d);
20250
gap> Length(HClassReps(d));
20250
gap> ForAll(HClassReps(d), x-> x in d);
true
gap> d:=DClass(RClass(s, f));
{Transformation( [ 3, 3, 4, 3, 9, 3, 3, 3, 3, 9 ] )}
gap> Size(d);
121500
gap> ForAll(d, x-> x in d);
true
gap> NrIdempotents(d);
5550
gap> ForAll(Idempotents(d), x-> x in d);
true
gap> exit
> ;
Error, Variable: 'exit' must have a value
not in any function at line 256 of *stdin*
gap> quit;
