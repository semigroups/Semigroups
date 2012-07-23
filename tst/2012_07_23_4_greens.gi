gap> Test("pkg/citrus/tst/everyfunction.tst");
Citrus package: everyfunction.tst
GAP4stones: 0
true
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
>   Transformation( [ 4, 2, 1, 5, 5 ] ),
>   Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> for d in DClasses(s) do
> enum:=Enumerator(d);
> Print(ForAll(enum, x-> enum[Position(enum, x)]=x), "\n");
> od
> ;
true
true
true
true
gap> s:=RandomBlockGroup(3, 10);
<semigroup with 3 generators>
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 3, 6, 8, 10 ], [ 2, 6, 7, 9, 1, 5 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 8, 10 ], [ 7, 1, 4, 3, 2, 6, 5 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 6, 7, 8, 10 ], [ 3, 8, 1, 9, 4, 10, 5, 6 ] ) ]
gap> f:=Random(s);
[ 2, 4 ] -> [ 6, 5 ]
gap> d:=DClassNC(s, f);
{[ 2, 4 ] -> [ 6, 5 ]}
gap> GreensHClasses(d);
[ {[ 2, 4 ] -> [ 6, 5 ]} ]
gap> Size(d):
Syntax error: ; expected
Size(d):
       ^
gap> Size(d);
1
gap> s:=RandomBlockGroup(3, 100);
<semigroup with 3 generators>
gap> f:=Random(s);
[ 12, 27, 37, 40, 46, 50, 51, 53 ] -> [ 98, 3, 84, 99, 100, 21, 70, 89 ]
gap> d:=DClassNC(s, f);
{[ 12, 27, 37, 40, 46, 50, 51, 53 ] -> [ 98, 3, 84, 99, 100, 21, 70, 89 ]}
gap> Size(D);
Error, Variable: 'D' must have a value
not in any function at line 22 of *stdin*
gap> Size(d);
1
gap> GreensHClasses(d);
[ {[ 12, 27, 37, 40, 46, 50, 51, 53 ] -> [ 98, 3, 84, 99, 100, 21, 70, 89 ]} ]
gap> iter:=IteratorOfDClasses(s);
<iterator of D-classes>
gap> repeat d:=NextIterator(iter); until Size(d)>1;
gap> d;
{[ 8, 57 ] -> [ 63, 87 ]}
gap> Size(d);
2036
gap> IsRegularDClass(d);
false
gap> GreensHClasses(d);;
gap> time;
75
gap> NrHClasses(d);
2036
gap> f:=Random(s);
<partial perm on 68 pts>
gap> d:=DClass(s, f);
{<partial perm on 68 pts>}
gap> GreensHClasses(d);;
gap> GreensHClasses(d);
[ {<partial perm on 68 pts>} ]
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 19, 
  20, 24, 25, 26, 27, 28, 29, 31, 32, 34, 35, 36, 37, 38, 40, 43, 45, 46, 49, 
  50, 51, 53, 55, 56, 57, 58, 59, 60, 61, 64, 66, 68, 69, 70, 72, 73, 74, 77, 
  80, 81, 83, 86, 87, 89, 91, 98 ], [ 89, 70, 79, 27, 84, 99, 9, 73, 33, 77, 
  69, 41, 18, 63, 29, 42, 75, 56, 90, 64, 98, 49, 35, 100, 71, 3, 20, 2, 26, 
  11, 39, 7, 48, 85, 8, 10, 61, 25, 55, 92, 62, 21, 34, 57, 44, 14, 53, 59, 
  12, 87, 78, 83, 30, 32, 68, 86, 23, 47, 93, 15, 76, 97, 91 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 
  19, 20, 22, 23, 24, 25, 28, 30, 31, 33, 34, 35, 36, 39, 40, 42, 43, 44, 45, 
  46, 47, 50, 53, 54, 55, 58, 59, 64, 65, 67, 69, 70, 71, 72, 73, 76, 77, 78, 
  81, 82, 84, 85, 86, 87, 89, 92, 94, 95 ], [ 5, 13, 94, 44, 80, 54, 99, 81, 
  31, 7, 90, 30, 46, 68, 36, 11, 100, 17, 87, 72, 14, 29, 9, 61, 91, 32, 43, 
  64, 60, 41, 26, 40, 8, 23, 63, 38, 57, 12, 59, 83, 92, 96, 18, 3, 65, 2, 
  37, 21, 49, 16, 75, 24, 27, 1, 48, 6, 35, 79, 82, 51, 39, 25, 77, 62, 22 
 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
  18, 19, 20, 21, 23, 24, 26, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 
  40, 42, 44, 48, 51, 52, 53, 55, 56, 57, 58, 60, 63, 64, 65, 66, 67, 71, 73, 
  75, 77, 80, 82, 83, 85, 86, 90, 91, 96, 97, 98, 99 ], 
[ 67, 93, 18, 59, 86, 16, 99, 73, 60, 74, 17, 95, 85, 49, 79, 4, 33, 66, 15, 
  44, 77, 41, 55, 84, 68, 69, 94, 31, 2, 29, 5, 42, 10, 63, 58, 34, 72, 53, 
  89, 57, 62, 76, 20, 52, 22, 35, 75, 98, 78, 40, 46, 28, 6, 90, 12, 65, 26, 
  36, 25, 61, 83, 38, 39, 87, 92, 97, 43, 30 ] ) ]
gap> d;
{<partial perm on 68 pts>}
gap> GreensLClasses(d);
[ {<partial perm on 68 pts>} ]
gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ),
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ),
> Transformation( [ 3, 3, 4, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> List(DClasses(s), LClasses);
[ [ {Transformation( [ 1, 3, 4, 1 ] )}, {Transformation( [ 4, 1, 3, 4 ] )}, 
      {Transformation( [ 3, 4, 1, 3 ] )} ], 
  [ {Transformation( [ 2, 4, 1, 2 ] )} ], 
  [ {Transformation( [ 3, 1, 1, 3 ] )}, {Transformation( [ 1, 4, 4, 1 ] )}, 
      {Transformation( [ 2, 1, 1, 2 ] )}, {Transformation( [ 2, 4, 4, 2 ] )}, 
      {Transformation( [ 4, 3, 3, 4 ] )} ], 
  [ {Transformation( [ 3, 3, 4, 1 ] )} ], 
  [ {Transformation( [ 1, 1, 1, 1 ] )}, {Transformation( [ 2, 2, 2, 2 ] )}, 
      {Transformation( [ 3, 3, 3, 3 ] )}, {Transformation( [ 4, 4, 4, 4 ] )} 
     ] ]
gap> Concatenation(last);
[ {Transformation( [ 1, 3, 4, 1 ] )}, {Transformation( [ 4, 1, 3, 4 ] )}, 
  {Transformation( [ 3, 4, 1, 3 ] )}, {Transformation( [ 2, 4, 1, 2 ] )}, 
  {Transformation( [ 3, 1, 1, 3 ] )}, {Transformation( [ 1, 4, 4, 1 ] )}, 
  {Transformation( [ 2, 1, 1, 2 ] )}, {Transformation( [ 2, 4, 4, 2 ] )}, 
  {Transformation( [ 4, 3, 3, 4 ] )}, {Transformation( [ 3, 3, 4, 1 ] )}, 
  {Transformation( [ 1, 1, 1, 1 ] )}, {Transformation( [ 2, 2, 2, 2 ] )}, 
  {Transformation( [ 3, 3, 3, 3 ] )}, {Transformation( [ 4, 4, 4, 4 ] )} ]
gap> IsDuplicateFree(last);
true
gap> List(last2, Size);
[ 1, 1, 1, 1, 10, 10, 10, 10, 10, 3, 1, 1, 1, 1 ]
gap> Sum(last);
61
gap> Size(s);
61
gap> One(s);
Transformation( [ 1, 2, 3, 4 ] )
gap> One(s) in s;
false
gap> SemigroupData(s);
<semigroup data: 10 reps, 9 lambda-rho values>
gap> NrRClasses(s);
9
gap> SemigroupData(s)[1];
[ ,,, Transformation( [ 1, 2, 3, 4 ] ) ]
gap> SemigroupData(s)[2];
[ <monoid with 4 generators>, 2, 
  <closed orbit, 12 points with Schreier tree with log>, 
  Transformation( [ 1, 3, 4, 1 ] ), 2 ]
gap> RereadPackage("citrus/gap/acting.gi");
true
gap> s:=Monoid(gens);;
gap> Size(s);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `[]' on 2 arguments called from
Position( o, lamx ) called from
Enumerate( SemigroupData( s ), infinity, ReturnFalse ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 59 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> o;
<closed orbit, 12 points with Schreier tree with log>
brk> lamx;
[ 1, 2, 3, 4 ]
brk> Position(o, lamx);
fail
brk> o;
<closed orbit, 12 points with Schreier tree with log>
brk> o!.gens;
[ Transformation( [ 1, 3, 4, 1 ] ), Transformation( [ 2, 4, 1, 2 ] ), 
  Transformation( [ 3, 1, 1, 3 ] ), Transformation( [ 3, 3, 4, 1 ] ) ]
brk> quit;
gap> RereadPackage("citrus/gap/acting.gi");
true
gap> s:=Monoid(gens);;
gap> Size(s);
62
gap> RereadPackage("citrus/gap/acting.gi");
true
gap> quit;
