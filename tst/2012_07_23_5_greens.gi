gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ),
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ),
> Transformation( [ 3, 3, 4, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> List(GreensDClasses(s), LClasses);
[ [ {Transformation( [ 1, 2, 3, 4 ] )} ], 
  [ {Transformation( [ 1, 3, 4, 1 ] )}, {Transformation( [ 4, 1, 3, 4 ] )}, 
      {Transformation( [ 3, 4, 1, 3 ] )} ], 
  [ {Transformation( [ 2, 4, 1, 2 ] )} ], 
  [ {Transformation( [ 3, 1, 1, 3 ] )}, {Transformation( [ 1, 4, 4, 1 ] )}, 
      {Transformation( [ 2, 1, 1, 2 ] )}, {Transformation( [ 2, 4, 4, 2 ] )}, 
      {Transformation( [ 4, 3, 3, 4 ] )} ], 
  [ {Transformation( [ 3, 3, 4, 1 ] )} ], 
  [ {Transformation( [ 1, 1, 1, 1 ] )}, {Transformation( [ 2, 2, 2, 2 ] )}, 
      {Transformation( [ 3, 3, 3, 3 ] )}, {Transformation( [ 4, 4, 4, 4 ] )} 
     ] ]
gap> List(Concatenation(last), Size);
[ 1, 1, 1, 1, 1, 10, 10, 10, 10, 10, 3, 1, 1, 1, 1 ]
gap> Sum(last);
62
gap> Size(s);
62
gap> l:=Concatenation(List(GreensDClasses(s), LClasses));
[ {Transformation( [ 1, 2, 3, 4 ] )}, {Transformation( [ 1, 3, 4, 1 ] )}, 
  {Transformation( [ 4, 1, 3, 4 ] )}, {Transformation( [ 3, 4, 1, 3 ] )}, 
  {Transformation( [ 2, 4, 1, 2 ] )}, {Transformation( [ 3, 1, 1, 3 ] )}, 
  {Transformation( [ 1, 4, 4, 1 ] )}, {Transformation( [ 2, 1, 1, 2 ] )}, 
  {Transformation( [ 2, 4, 4, 2 ] )}, {Transformation( [ 4, 3, 3, 4 ] )}, 
  {Transformation( [ 3, 3, 4, 1 ] )}, {Transformation( [ 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 2, 2, 2 ] )}, {Transformation( [ 3, 3, 3, 3 ] )}, 
  {Transformation( [ 4, 4, 4, 4 ] )} ]
gap> List(last, Elements);
[ [ Transformation( [ 1, 2, 3, 4 ] ) ], [ Transformation( [ 1, 3, 4, 1 ] ) ], 
  [ Transformation( [ 4, 1, 3, 4 ] ) ], [ Transformation( [ 3, 4, 1, 3 ] ) ], 
  [ Transformation( [ 2, 4, 1, 2 ] ) ], 
  [ Transformation( [ 1, 1, 1, 3 ] ), Transformation( [ 1, 1, 3, 1 ] ), 
      Transformation( [ 1, 1, 3, 3 ] ), Transformation( [ 1, 3, 1, 1 ] ), 
      Transformation( [ 1, 3, 3, 1 ] ), Transformation( [ 3, 1, 1, 3 ] ), 
      Transformation( [ 3, 1, 3, 3 ] ), Transformation( [ 3, 3, 1, 1 ] ), 
      Transformation( [ 3, 3, 1, 3 ] ), Transformation( [ 3, 3, 3, 1 ] ) ], 
  [ Transformation( [ 1, 1, 1, 4 ] ), Transformation( [ 1, 1, 4, 1 ] ), 
      Transformation( [ 1, 1, 4, 4 ] ), Transformation( [ 1, 4, 1, 1 ] ), 
      Transformation( [ 1, 4, 4, 1 ] ), Transformation( [ 4, 1, 1, 4 ] ), 
      Transformation( [ 4, 1, 4, 4 ] ), Transformation( [ 4, 4, 1, 1 ] ), 
      Transformation( [ 4, 4, 1, 4 ] ), Transformation( [ 4, 4, 4, 1 ] ) ], 
  [ Transformation( [ 1, 1, 1, 2 ] ), Transformation( [ 1, 1, 2, 1 ] ), 
      Transformation( [ 1, 1, 2, 2 ] ), Transformation( [ 1, 2, 1, 1 ] ), 
      Transformation( [ 1, 2, 2, 1 ] ), Transformation( [ 2, 1, 1, 2 ] ), 
      Transformation( [ 2, 1, 2, 2 ] ), Transformation( [ 2, 2, 1, 1 ] ), 
      Transformation( [ 2, 2, 1, 2 ] ), Transformation( [ 2, 2, 2, 1 ] ) ], 
  [ Transformation( [ 2, 2, 2, 4 ] ), Transformation( [ 2, 2, 4, 2 ] ), 
      Transformation( [ 2, 2, 4, 4 ] ), Transformation( [ 2, 4, 2, 2 ] ), 
      Transformation( [ 2, 4, 4, 2 ] ), Transformation( [ 4, 2, 2, 4 ] ), 
      Transformation( [ 4, 2, 4, 4 ] ), Transformation( [ 4, 4, 2, 2 ] ), 
      Transformation( [ 4, 4, 2, 4 ] ), Transformation( [ 4, 4, 4, 2 ] ) ], 
  [ Transformation( [ 3, 3, 3, 4 ] ), Transformation( [ 3, 3, 4, 3 ] ), 
      Transformation( [ 3, 3, 4, 4 ] ), Transformation( [ 3, 4, 3, 3 ] ), 
      Transformation( [ 3, 4, 4, 3 ] ), Transformation( [ 4, 3, 3, 4 ] ), 
      Transformation( [ 4, 3, 4, 4 ] ), Transformation( [ 4, 4, 3, 3 ] ), 
      Transformation( [ 4, 4, 3, 4 ] ), Transformation( [ 4, 4, 4, 3 ] ) ], 
  [ Transformation( [ 1, 1, 3, 4 ] ), Transformation( [ 3, 3, 4, 1 ] ), 
      Transformation( [ 4, 4, 1, 3 ] ) ], [ Transformation( [ 1, 1, 1, 1 ] ) ]
    , [ Transformation( [ 2, 2, 2, 2 ] ) ], 
  [ Transformation( [ 3, 3, 3, 3 ] ) ], [ Transformation( [ 4, 4, 4, 4 ] ) ] ]
gap> Union(last);
[ Transformation( [ 1, 1, 1, 1 ] ), Transformation( [ 1, 1, 1, 2 ] ), 
  Transformation( [ 1, 1, 1, 3 ] ), Transformation( [ 1, 1, 1, 4 ] ), 
  Transformation( [ 1, 1, 2, 1 ] ), Transformation( [ 1, 1, 2, 2 ] ), 
  Transformation( [ 1, 1, 3, 1 ] ), Transformation( [ 1, 1, 3, 3 ] ), 
  Transformation( [ 1, 1, 3, 4 ] ), Transformation( [ 1, 1, 4, 1 ] ), 
  Transformation( [ 1, 1, 4, 4 ] ), Transformation( [ 1, 2, 1, 1 ] ), 
  Transformation( [ 1, 2, 2, 1 ] ), Transformation( [ 1, 2, 3, 4 ] ), 
  Transformation( [ 1, 3, 1, 1 ] ), Transformation( [ 1, 3, 3, 1 ] ), 
  Transformation( [ 1, 3, 4, 1 ] ), Transformation( [ 1, 4, 1, 1 ] ), 
  Transformation( [ 1, 4, 4, 1 ] ), Transformation( [ 2, 1, 1, 2 ] ), 
  Transformation( [ 2, 1, 2, 2 ] ), Transformation( [ 2, 2, 1, 1 ] ), 
  Transformation( [ 2, 2, 1, 2 ] ), Transformation( [ 2, 2, 2, 1 ] ), 
  Transformation( [ 2, 2, 2, 2 ] ), Transformation( [ 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 4, 2 ] ), Transformation( [ 2, 2, 4, 4 ] ), 
  Transformation( [ 2, 4, 1, 2 ] ), Transformation( [ 2, 4, 2, 2 ] ), 
  Transformation( [ 2, 4, 4, 2 ] ), Transformation( [ 3, 1, 1, 3 ] ), 
  Transformation( [ 3, 1, 3, 3 ] ), Transformation( [ 3, 3, 1, 1 ] ), 
  Transformation( [ 3, 3, 1, 3 ] ), Transformation( [ 3, 3, 3, 1 ] ), 
  Transformation( [ 3, 3, 3, 3 ] ), Transformation( [ 3, 3, 3, 4 ] ), 
  Transformation( [ 3, 3, 4, 1 ] ), Transformation( [ 3, 3, 4, 3 ] ), 
  Transformation( [ 3, 3, 4, 4 ] ), Transformation( [ 3, 4, 1, 3 ] ), 
  Transformation( [ 3, 4, 3, 3 ] ), Transformation( [ 3, 4, 4, 3 ] ), 
  Transformation( [ 4, 1, 1, 4 ] ), Transformation( [ 4, 1, 3, 4 ] ), 
  Transformation( [ 4, 1, 4, 4 ] ), Transformation( [ 4, 2, 2, 4 ] ), 
  Transformation( [ 4, 2, 4, 4 ] ), Transformation( [ 4, 3, 3, 4 ] ), 
  Transformation( [ 4, 3, 4, 4 ] ), Transformation( [ 4, 4, 1, 1 ] ), 
  Transformation( [ 4, 4, 1, 3 ] ), Transformation( [ 4, 4, 1, 4 ] ), 
  Transformation( [ 4, 4, 2, 2 ] ), Transformation( [ 4, 4, 2, 4 ] ), 
  Transformation( [ 4, 4, 3, 3 ] ), Transformation( [ 4, 4, 3, 4 ] ), 
  Transformation( [ 4, 4, 4, 1 ] ), Transformation( [ 4, 4, 4, 2 ] ), 
  Transformation( [ 4, 4, 4, 3 ] ), Transformation( [ 4, 4, 4, 4 ] ) ]
gap> last=AsSSortedList(s);
true
gap> s:=RandomBlockGroup(4, 7);
<semigroup with 4 generators>
gap> Size(s);
840
gap> Display(Generators(s);
Syntax error: ) expected
Display(Generators(s);
                     ^
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 3, 4 ], [ 5, 7, 1, 6 ] ),
 PartialPermNC( [ 1, 2, 3, 5 ], [ 5, 2, 7, 3 ] ),
 PartialPermNC( [ 1, 2, 3, 6, 7 ], [ 1, 3, 4, 7, 5 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 7 ], [ 3, 2, 4, 6, 1, 5 ] ) ]
gap> NrGreensDClasses(s);
Error, Variable: 'NrGreensDClasses' must have a value
not in any function at line 22 of *stdin*
gap> NrDClasses(s);
176
gap> s:=RandomBlockGroup(4, 6);
<semigroup with 4 generators>
gap> Size(s);
56
gap> NrDClasses(s);
28
gap> List(DClasses(s), RClasse);
Error, Variable: 'RClasse' must have a value
not in any function at line 27 of *stdin*
gap> List(DClasses(s), RClasses);
[ [ {[ 1, 2, 4 ] -> [ 4, 6, 3 ]} ], [ {[ 1 .. 4 ] -> [ 4, 1, 2, 6 ]} ], 
  [ {[ 1, 2, 3, 5 ] -> [ 4, 6, 5, 2 ]} ], 
  [ {[ 1, 2, 3, 5 ] -> [ 6, 4, 5, 1 ]} ], 
  [ {[ 1 ] -> [ 3 ]}, {[ 4 ] -> [ 3 ]}, {[ 2 ] -> [ 3 ]}, {[ 5 ] -> [ 3 ]}, 
      {<identity on [ 3 ]>} ], [ {[ 1 .. 3 ] -> [ 3, 4, 6 ]} ], 
  [ {[ 1, 5 ] -> [ 3, 6 ]} ], [ {[ 2, 5 ] -> [ 3, 4 ]} ], 
  [ {[ 1, 4 ] -> [ 6, 2 ]} ], [ {[ 1 .. 3 ] -> [ 6, 4, 1 ]} ], 
  [ {[ 1, 5 ] -> [ 6, 1 ]} ], [ {[ 2, 5 ] -> [ 6, 4 ]} ], 
  [ {[ 2, 3 ] -> [ 4, 6 ]} ], [ {[ 3, 5 ] -> [ 2, 6 ]} ], 
  [ {[ 3, 5 ] -> [ 2, 4 ]} ], [ {[ 2, 3 ] -> [ 6, 4 ]} ], 
  [ {[ 3, 5 ] -> [ 1, 4 ]} ], [ {[ 3, 5 ] -> [ 1, 6 ]} ], 
  [ {<empty mapping>} ], 
  [ {[ 1 ] -> [ 6 ]}, {[ 4 ] -> [ 6 ]}, {[ 2 ] -> [ 6 ]}, {[ 5 ] -> [ 6 ]}, 
      {[ 3 ] -> [ 6 ]} ], [ {[ 2, 3 ] -> [ 3, 4 ]} ], 
  [ {[ 3, 5 ] -> [ 6, 3 ]} ], [ {[ 3, 5 ] -> [ 4, 3 ]} ], 
  [ {[ 1, 2 ] -> [ 2, 6 ]} ], [ {[ 2, 5 ] -> [ 2, 6 ]} ], 
  [ {[ 3, 5 ] -> [ 4, 6 ]} ], [ {[ 2, 3 ] -> [ 2, 6 ]} ], 
  [ {[ 3, 5 ] -> [ 6, 2 ]} ] ]
gap> ForAll(Union(List(last, Elements)), x-> x in s);
false
gap> Union(List(last2, Elements));
[ {<empty mapping>}, {[ 1 ] -> [ 3 ]}, {[ 1 ] -> [ 6 ]}, {[ 2 ] -> [ 3 ]}, 
  {[ 2 ] -> [ 6 ]}, {<identity on [ 3 ]>}, {[ 3 ] -> [ 6 ]}, {[ 4 ] -> [ 3 ]},
  {[ 4 ] -> [ 6 ]}, {[ 5 ] -> [ 3 ]}, {[ 5 ] -> [ 6 ]}, 
  {[ 1, 2 ] -> [ 2, 6 ]}, {[ 1, 4 ] -> [ 6, 2 ]}, {[ 1, 5 ] -> [ 3, 6 ]}, 
  {[ 1, 5 ] -> [ 6, 1 ]}, {[ 2, 3 ] -> [ 2, 6 ]}, {[ 2, 3 ] -> [ 3, 4 ]}, 
  {[ 2, 3 ] -> [ 4, 6 ]}, {[ 2, 3 ] -> [ 6, 4 ]}, {[ 2, 5 ] -> [ 2, 6 ]}, 
  {[ 2, 5 ] -> [ 3, 4 ]}, {[ 2, 5 ] -> [ 6, 4 ]}, {[ 3, 5 ] -> [ 1, 4 ]}, 
  {[ 3, 5 ] -> [ 1, 6 ]}, {[ 3, 5 ] -> [ 2, 4 ]}, {[ 3, 5 ] -> [ 2, 6 ]}, 
  {[ 3, 5 ] -> [ 4, 3 ]}, {[ 3, 5 ] -> [ 4, 6 ]}, {[ 3, 5 ] -> [ 6, 2 ]}, 
  {[ 3, 5 ] -> [ 6, 3 ]}, {[ 1 .. 3 ] -> [ 3, 4, 6 ]}, 
  {[ 1 .. 3 ] -> [ 6, 4, 1 ]}, {[ 1, 2, 4 ] -> [ 4, 6, 3 ]}, 
  {[ 1 .. 4 ] -> [ 4, 1, 2, 6 ]}, {[ 1, 2, 3, 5 ] -> [ 4, 6, 5, 2 ]}, 
  {[ 1, 2, 3, 5 ] -> [ 6, 4, 5, 1 ]} ]
gap> Union(List(last, Elements));
[ <empty mapping>, <identity on [ 1 ]>, [ 1 ] -> [ 2 ], [ 1 ] -> [ 3 ], 
  [ 1 ] -> [ 4 ], [ 1 ] -> [ 5 ], [ 1 ] -> [ 6 ], [ 2 ] -> [ 1 ], 
  <identity on [ 2 ]>, [ 2 ] -> [ 3 ], [ 2 ] -> [ 4 ], [ 2 ] -> [ 5 ], 
  [ 2 ] -> [ 6 ], [ 3 ] -> [ 1 ], [ 3 ] -> [ 2 ], <identity on [ 3 ]>, 
  [ 3 ] -> [ 4 ], [ 3 ] -> [ 5 ], [ 3 ] -> [ 6 ], [ 4 ] -> [ 1 ], 
  [ 4 ] -> [ 2 ], [ 4 ] -> [ 3 ], <identity on [ 4 ]>, [ 4 ] -> [ 5 ], 
  [ 4 ] -> [ 6 ], [ 5 ] -> [ 1 ], [ 5 ] -> [ 2 ], [ 5 ] -> [ 3 ], 
  [ 5 ] -> [ 4 ], <identity on [ 5 ]>, [ 5 ] -> [ 6 ], [ 1, 2 ] -> [ 2, 6 ], 
  [ 1, 4 ] -> [ 6, 2 ], [ 1, 5 ] -> [ 3, 6 ], [ 1, 5 ] -> [ 6, 1 ], 
  [ 2, 3 ] -> [ 2, 6 ], [ 2, 3 ] -> [ 3, 4 ], [ 2, 3 ] -> [ 4, 6 ], 
  [ 2, 3 ] -> [ 6, 4 ], [ 2, 5 ] -> [ 2, 6 ], [ 2, 5 ] -> [ 3, 4 ], 
  [ 2, 5 ] -> [ 6, 4 ], [ 3, 5 ] -> [ 1, 4 ], [ 3, 5 ] -> [ 1, 6 ], 
  [ 3, 5 ] -> [ 2, 4 ], [ 3, 5 ] -> [ 2, 6 ], [ 3, 5 ] -> [ 4, 3 ], 
  [ 3, 5 ] -> [ 4, 6 ], [ 3, 5 ] -> [ 6, 2 ], [ 3, 5 ] -> [ 6, 3 ], 
  [ 1 .. 3 ] -> [ 3, 4, 6 ], [ 1 .. 3 ] -> [ 6, 4, 1 ], 
  [ 1, 2, 4 ] -> [ 4, 6, 3 ], [ 1 .. 4 ] -> [ 4, 1, 2, 6 ], 
  [ 1, 2, 3, 5 ] -> [ 4, 6, 5, 2 ], [ 1, 2, 3, 5 ] -> [ 6, 4, 5, 1 ] ]
gap> ForAll(last, x-> x in s);
true
gap> Set(last2)=AsSSortedList(s);
true
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
>  Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 2, 4, 1, 1 ] ),
>  Transformation( [ 3, 4, 2, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
114
gap> NrRClasses(s);
11
gap> NrDClasses(s);
5
gap> NrLClasses(s);
19
gap> NrIdempotents(s);
28
gap> IsRegularSemigroup(s);
false
gap> f:=Random(s);
Transformation( [ 4, 2, 4, 3 ] )
gap> d:=First(DClasses(s), x-> f in x);
{Transformation( [ 1, 4, 1, 2 ] )}
gap> h:=HClass(d, f);
{Transformation( [ 4, 2, 4, 3 ] )}
gap> Size(h);
6
gap> IsGroupHClass(h);
true
gap> SchutzenbergerGroup(h);
Group([ (2,4), (2,3,4) ])
gap> ForAll(Elements(h), x-> x in h);
true
gap> ForAll(Elements(h), x-> x in d);
true
gap> IsGreensClassNC(h);
false
gap> s:=RandomBlockGroup(4, 6);
<semigroup with 4 generators>
gap> Size(s);
201
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 4 ], [ 4, 5, 6 ] ),
 PartialPermNC( [ 1, 2, 5 ], [ 2, 1, 3 ] ),
 PartialPermNC( [ 1, 2, 4, 6 ], [ 2, 4, 3, 5 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5 ], [ 4, 3, 6, 5, 1 ] ) ]
gap> f:=Random(s);
[ 1 .. 5 ] -> [ 4, 3, 6, 5, 1 ]
gap> d:=DClassNC(s, f);
{[ 1 .. 5 ] -> [ 4, 3, 6, 5, 1 ]}
gap> h:=HClassNC(d, f);
{[ 1 .. 5 ] -> [ 4, 3, 6, 5, 1 ]}
gap> Size(h);
1
gap> Size(d);
1
gap> h=d;
true
gap> d=h;
true
gap> s:=RandomBlockGroup(4, 100);
<semigroup with 4 generators>
gap> f:=Random(s);
[ 2, 63 ] -> [ 28, 89 ]
gap> d:=DClassNC(s, f);
{[ 2, 63 ] -> [ 28, 89 ]}
gap> Size(d);
4752
gap> time;
43
gap> RhoOrb(d);
<closed orbit, 2874 points with Schreier tree with log with grading>
gap> 2874*2;
5748
gap> LambdaOrb(d);
<closed orbit, 1 points with Schreier tree with log with grading>
gap> NrLClasses(d);
1
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 
  19, 20, 21, 23, 24, 25, 26, 27, 28, 30, 32, 33, 34, 35, 36, 37, 40, 42, 44, 
  46, 47, 51, 53, 54, 58, 59, 60, 61, 63, 65, 66, 67, 69, 71, 72, 76, 79, 84, 
  86, 88, 94, 95, 100 ], [ 46, 47, 33, 32, 70, 97, 29, 30, 34, 11, 37, 89, 
  77, 52, 73, 2, 96, 66, 88, 69, 93, 87, 85, 68, 48, 25, 28, 43, 49, 95, 40, 
  24, 16, 94, 76, 63, 58, 23, 100, 38, 27, 78, 21, 71, 4, 72, 36, 13, 99, 90, 
  17, 41, 98, 10, 35, 91, 53, 45, 82, 42 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 18, 19, 
  21, 22, 24, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 38, 39, 40, 41, 42, 43, 
  44, 46, 48, 49, 51, 52, 54, 56, 58, 59, 61, 64, 65, 67, 68, 70, 73, 74, 76, 
  78, 79, 80, 82, 88, 90, 97 ], [ 63, 38, 57, 12, 9, 91, 59, 32, 54, 83, 92, 
  96, 99, 18, 3, 81, 5, 65, 2, 37, 21, 49, 16, 75, 24, 23, 43, 27, 1, 48, 6, 
  35, 30, 79, 82, 51, 39, 25, 61, 77, 62, 22, 64, 14, 72, 7, 50, 8, 80, 19, 
  94, 69, 10, 40, 67, 28, 88, 93, 66, 36, 70, 56 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
  18, 21, 22, 23, 24, 25, 26, 27, 29, 30, 31, 34, 35, 36, 37, 38, 39, 40, 42, 
  43, 44, 46, 48, 49, 51, 52, 53, 55, 58, 60, 63, 64, 66, 67, 68, 69, 71, 73, 
  75, 80, 86, 87, 88, 90, 91, 94, 95, 97 ], [ 89, 85, 8, 56, 42, 10, 61, 25, 
  98, 55, 39, 92, 62, 21, 34, 57, 44, 14, 53, 64, 59, 84, 12, 87, 78, 83, 30, 
  32, 68, 73, 2, 86, 23, 48, 47, 79, 93, 15, 76, 97, 77, 11, 33, 100, 91, 67, 
  18, 16, 99, 60, 74, 17, 95, 49, 4, 66, 41, 69, 94, 31, 29, 5, 63, 58, 72 
 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 17, 20, 21, 
  22, 23, 24, 26, 28, 29, 30, 32, 34, 35, 37, 39, 40, 42, 43, 44, 45, 46, 47, 
  48, 49, 51, 53, 54, 55, 56, 58, 59, 60, 61, 63, 64, 65, 66, 67, 68, 72, 74, 
  75, 79, 80, 82, 87, 88, 91, 92, 99, 100 ], [ 89, 67, 34, 15, 57, 29, 4, 62, 
  76, 20, 52, 22, 35, 75, 98, 78, 40, 46, 28, 6, 55, 90, 16, 12, 65, 26, 66, 
  36, 25, 61, 83, 38, 41, 93, 2, 39, 87, 85, 17, 92, 97, 43, 30, 5, 13, 94, 
  44, 80, 54, 99, 81, 31, 7, 68, 11, 100, 72, 14, 9, 91, 32, 64, 60, 8, 23 
 ] ) ]
gap> NrRClasses(d);
4752
gap> f:=Random(d);
[ 4, 29 ] -> [ 28, 89 ]
gap> f in d;
true
gap> h:=HClass(d, f);
{[ 4, 29 ] -> [ 28, 89 ]}
gap> hh:=HClassNC(d, f);
{[ 4, 29 ] -> [ 28, 89 ]}
gap> hh=h;
true
gap> Size(h);
1
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
>  Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 3, 4, 2, 2 ] ),
>  Transformation( [ 4, 1, 2, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> List(DClasses(s), RClassReps);
[ [ Transformation( [ 1, 2, 3, 4 ] ) ], [ Transformation( [ 1, 3, 2, 3 ] ) ], 
  [ Transformation( [ 1, 4, 1, 2 ] ), Transformation( [ 1, 2, 4, 4 ] ) ], 
  [ Transformation( [ 4, 1, 2, 1 ] ), Transformation( [ 4, 2, 1, 2 ] ) ], 
  [ Transformation( [ 1, 4, 1, 4 ] ), Transformation( [ 1, 4, 4, 4 ] ), 
      Transformation( [ 1, 1, 4, 1 ] ) ], [ Transformation( [ 1, 4, 2, 4 ] ) ]
    , [ Transformation( [ 4, 4, 2, 2 ] ), Transformation( [ 4, 2, 4, 4 ] ), 
      Transformation( [ 4, 4, 4, 2 ] ) ], [ Transformation( [ 1, 1, 1, 1 ] ) ]
    , [ Transformation( [ 2, 4, 2, 4 ] ), Transformation( [ 2, 4, 4, 4 ] ), 
      Transformation( [ 2, 2, 4, 2 ] ) ] ]
gap> reps:=Concatenation(last0;
Error, Variable: 'last0' must have a value
not in any function at line 87 of *stdin*
Syntax error: ) expected
reps:=Concatenation(last0;
                         ^
gap> reps:=Concatenation(last);
[ Transformation( [ 1, 2, 3, 4 ] ), Transformation( [ 1, 3, 2, 3 ] ), 
  Transformation( [ 1, 4, 1, 2 ] ), Transformation( [ 1, 2, 4, 4 ] ), 
  Transformation( [ 4, 1, 2, 1 ] ), Transformation( [ 4, 2, 1, 2 ] ), 
  Transformation( [ 1, 4, 1, 4 ] ), Transformation( [ 1, 4, 4, 4 ] ), 
  Transformation( [ 1, 1, 4, 1 ] ), Transformation( [ 1, 4, 2, 4 ] ), 
  Transformation( [ 4, 4, 2, 2 ] ), Transformation( [ 4, 2, 4, 4 ] ), 
  Transformation( [ 4, 4, 4, 2 ] ), Transformation( [ 1, 1, 1, 1 ] ), 
  Transformation( [ 2, 4, 2, 4 ] ), Transformation( [ 2, 4, 4, 4 ] ), 
  Transformation( [ 2, 2, 4, 2 ] ) ]
gap> Length(last);
17
gap> IsDuplicateFree(last2);
true
gap> Size(s);
69
gap> NrDClasses(s):
Syntax error: ; expected
NrDClasses(s):
             ^
gap> NrDClasses(s);
9
gap> NrLClasses(s);
21
gap> List(reps, x-> DClass(s, f));
Error, the element does not belong to the semigroup, called from
GreensDClassOfElement( arg[1], arg[2] ) called from
DClass( s, f ) called from
func( C[i] ) called from
<function "List">( <arguments> )
 called from read-eval loop at line 95 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> arg[2];
[ 4, 29 ] -> [ 28, 89 ]
brk> quit;
gap> List(reps, x-> DClass(s, x));
[ {Transformation( [ 1, 2, 3, 4 ] )}, {Transformation( [ 1, 3, 2, 3 ] )}, 
  {Transformation( [ 1, 4, 1, 2 ] )}, {Transformation( [ 1, 2, 4, 4 ] )}, 
  {Transformation( [ 4, 1, 2, 1 ] )}, {Transformation( [ 4, 2, 1, 2 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 4, 4 ] )}, 
  {Transformation( [ 1, 1, 4, 1 ] )}, {Transformation( [ 1, 4, 2, 4 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 4, 2, 4, 4 ] )}, 
  {Transformation( [ 4, 4, 4, 2 ] )}, {Transformation( [ 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )}, {Transformation( [ 2, 4, 4, 4 ] )}, 
  {Transformation( [ 2, 2, 4, 2 ] )} ]
gap> Union(List(last, x-> LClass(x,Representative(x)));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `^' on 2 arguments called from
rho_schutz ^ p called from
SchutzenbergerGroup( d ); called from
RhoOrbStabChain( d ) called from
f in d called from
GreensLClassOfElement( arg[1], arg[2] ) called from
...  at line 96 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> d;
{Transformation( [ 1, 2, 4, 4 ] )}
brk> IsGreensDClass(d);
true
brk> f;
Transformation( [ 1, 2, 4, 4 ] )
brk> f in d;
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `^' on 2 arguments called from
rho_schutz ^ p called from
SchutzenbergerGroup( d ); called from
RhoOrbStabChain( d ) called from
Error( no_method_found ); called from
rho_schutz ^ p called from
...  at line 4 of *errin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk_2> quit;
brk> p;
fail
brk> ParentSemigroup(d);
<monoid with 4 generators>
brk> RhoOrbRep(o, m);
Transformation( [ 1, 4, 1, 2 ] )
brk> o;
<closed orbit, 12 points with Schreier tree with log>
brk> m;
4
brk> d;
{Transformation( [ 1, 2, 4, 4 ] )}
brk> Position(RhoOrb(d), RhoFunc(s)(Representative(d)));
5
brk> OrbSCCLookup(RhoOrb(d))[5];
4
brk> OrbSCC(o)[4];
[ 4, 5 ]
brk> quit;
Syntax error: ) expected
Union(List(last, x-> LClass(x,Representative(x)));
                                                 ^
gap> d:=DClass(s, Transformation( [ 1, 2, 4, 4 ] ));
{Transformation( [ 1, 2, 4, 4 ] )}
gap> f:=Transformation( [ 1, 2, 4, 4 ] );
Transformation( [ 1, 2, 4, 4 ] )
gap> o:=LambdaOrb(s);
<closed orbit, 15 points with Schreier tree with log>
gap> rectify:=RectifyLambda(s, o, f);
rec( l := 4, m := 4, rep := Transformation( [ 1, 2, 4, 4 ] ) )
gap> HasRhoOrb(s) and IsClosed(RhoOrb(s));
true
gap>    o:=RhoOrb(s);
<closed orbit, 12 points with Schreier tree with log>
gap>     rectify:=RectifyRho(s, o, f);
rec( l := 5, m := 4, rep := Transformation( [ 1, 4, 1, 2 ] ) )
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> List(reps, x-> DClass(s, x));
[ {Transformation( [ 1, 2, 3, 4 ] )}, {Transformation( [ 1, 3, 2, 3 ] )}, 
  {Transformation( [ 1, 4, 1, 2 ] )}, {Transformation( [ 1, 4, 1, 2 ] )}, 
  {Transformation( [ 4, 1, 2, 1 ] )}, {Transformation( [ 4, 2, 1, 2 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 1, 4 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 2, 4 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 4, 4, 2, 2 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )}, {Transformation( [ 2, 4, 2, 4 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )} ]
gap> Union(List(last, x-> LClass(x,Representative(x)));
Syntax error: ) expected
Union(List(last, x-> LClass(x,Representative(x)));
                                                 ^
gap> Union(List(last, x-> LClass(x,Representative(x))));
[ Transformation( [ 1, 1, 1, 1 ] ), Transformation( [ 1, 1, 4, 1 ] ), 
  Transformation( [ 1, 2, 1, 4 ] ), Transformation( [ 1, 2, 3, 2 ] ), 
  Transformation( [ 1, 2, 3, 4 ] ), Transformation( [ 1, 2, 4, 2 ] ), 
  Transformation( [ 1, 2, 4, 4 ] ), Transformation( [ 1, 3, 2, 3 ] ), 
  Transformation( [ 1, 4, 1, 2 ] ), Transformation( [ 1, 4, 1, 4 ] ), 
  Transformation( [ 1, 4, 2, 2 ] ), Transformation( [ 1, 4, 2, 4 ] ), 
  Transformation( [ 1, 4, 4, 4 ] ), Transformation( [ 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 4, 2 ] ), Transformation( [ 2, 2, 4, 4 ] ), 
  Transformation( [ 2, 4, 2, 2 ] ), Transformation( [ 2, 4, 2, 4 ] ), 
  Transformation( [ 2, 4, 4, 4 ] ), Transformation( [ 4, 1, 1, 1 ] ), 
  Transformation( [ 4, 1, 2, 1 ] ), Transformation( [ 4, 1, 4, 1 ] ), 
  Transformation( [ 4, 2, 1, 2 ] ), Transformation( [ 4, 2, 2, 2 ] ), 
  Transformation( [ 4, 2, 4, 2 ] ), Transformation( [ 4, 2, 4, 4 ] ), 
  Transformation( [ 4, 4, 1, 4 ] ), Transformation( [ 4, 4, 2, 2 ] ), 
  Transformation( [ 4, 4, 2, 4 ] ), Transformation( [ 4, 4, 4, 2 ] ) ]
gap> Length(last);
30
gap> D:=List(reps, x-> DClass(s, x));
[ {Transformation( [ 1, 2, 3, 4 ] )}, {Transformation( [ 1, 3, 2, 3 ] )}, 
  {Transformation( [ 1, 4, 1, 2 ] )}, {Transformation( [ 1, 4, 1, 2 ] )}, 
  {Transformation( [ 4, 1, 2, 1 ] )}, {Transformation( [ 4, 2, 1, 2 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 1, 4 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 2, 4 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 4, 4, 2, 2 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )}, {Transformation( [ 2, 4, 2, 4 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )} ]
gap> Length(Set(D));
9
gap> List(D, x-> LClass(x, Representative(x)));
[ {Transformation( [ 1, 2, 3, 4 ] )}, {Transformation( [ 1, 3, 2, 3 ] )}, 
  {Transformation( [ 1, 4, 1, 2 ] )}, {Transformation( [ 1, 4, 1, 2 ] )}, 
  {Transformation( [ 4, 1, 2, 1 ] )}, {Transformation( [ 4, 2, 1, 2 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 1, 4 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 2, 4 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 4, 4, 2, 2 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )}, {Transformation( [ 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )}, {Transformation( [ 2, 4, 2, 4 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )} ]
gap> Union(last);
[ Transformation( [ 1, 1, 1, 1 ] ), Transformation( [ 1, 1, 4, 1 ] ), 
  Transformation( [ 1, 2, 1, 4 ] ), Transformation( [ 1, 2, 3, 2 ] ), 
  Transformation( [ 1, 2, 3, 4 ] ), Transformation( [ 1, 2, 4, 2 ] ), 
  Transformation( [ 1, 2, 4, 4 ] ), Transformation( [ 1, 3, 2, 3 ] ), 
  Transformation( [ 1, 4, 1, 2 ] ), Transformation( [ 1, 4, 1, 4 ] ), 
  Transformation( [ 1, 4, 2, 2 ] ), Transformation( [ 1, 4, 2, 4 ] ), 
  Transformation( [ 1, 4, 4, 4 ] ), Transformation( [ 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 4, 2 ] ), Transformation( [ 2, 2, 4, 4 ] ), 
  Transformation( [ 2, 4, 2, 2 ] ), Transformation( [ 2, 4, 2, 4 ] ), 
  Transformation( [ 2, 4, 4, 4 ] ), Transformation( [ 4, 1, 1, 1 ] ), 
  Transformation( [ 4, 1, 2, 1 ] ), Transformation( [ 4, 1, 4, 1 ] ), 
  Transformation( [ 4, 2, 1, 2 ] ), Transformation( [ 4, 2, 2, 2 ] ), 
  Transformation( [ 4, 2, 4, 2 ] ), Transformation( [ 4, 2, 4, 4 ] ), 
  Transformation( [ 4, 4, 1, 4 ] ), Transformation( [ 4, 4, 2, 2 ] ), 
  Transformation( [ 4, 4, 2, 4 ] ), Transformation( [ 4, 4, 4, 2 ] ) ]
gap> Set(last2)=Set(LClasses(s));
false
gap> L:=Set(last3);
[ {Transformation( [ 1, 1, 1, 1 ] )}, {Transformation( [ 1, 2, 3, 4 ] )}, 
  {Transformation( [ 1, 3, 2, 3 ] )}, {Transformation( [ 1, 4, 1, 2 ] )}, 
  {Transformation( [ 1, 4, 1, 4 ] )}, {Transformation( [ 1, 4, 2, 4 ] )}, 
  {Transformation( [ 2, 4, 2, 4 ] )}, {Transformation( [ 4, 1, 2, 1 ] )}, 
  {Transformation( [ 4, 4, 2, 2 ] )} ]
gap> s:=RandomBlockGroup(4, 100);
<semigroup with 4 generators>
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 
  20, 22, 23, 24, 25, 26, 28, 29, 31, 32, 33, 35, 36, 37, 38, 41, 42, 44, 45, 
  50, 51, 52, 54, 55, 60, 62, 64, 65, 66, 68, 71, 73, 75, 77, 78, 79, 83, 84, 
  94, 95, 96, 97 ], [ 30, 56, 33, 17, 43, 34, 28, 78, 91, 24, 44, 84, 71, 81, 
  57, 90, 20, 69, 70, 6, 82, 26, 53, 86, 32, 22, 12, 95, 59, 40, 73, 76, 98, 
  48, 80, 51, 9, 27, 49, 93, 52, 60, 94, 11, 75, 96, 72, 4, 87, 37, 29, 50, 
  39, 45, 88, 67, 14, 99 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 19, 
  20, 21, 23, 24, 25, 26, 28, 30, 32, 35, 36, 37, 41, 42, 43, 47, 48, 49, 50, 
  51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 64, 65, 67, 68, 69, 71, 72, 74, 76, 
  81, 82, 83, 84, 86, 87, 92, 93 ], [ 56, 4, 87, 14, 67, 82, 17, 73, 18, 12, 
  35, 43, 80, 99, 7, 96, 58, 76, 36, 30, 98, 26, 62, 1, 75, 27, 10, 74, 55, 
  47, 37, 95, 39, 52, 84, 72, 50, 53, 77, 24, 59, 66, 9, 49, 70, 6, 51, 89, 
  21, 11, 85, 15, 19, 28, 79, 40, 34, 71, 5, 29, 88, 16, 8 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 
  19, 20, 22, 23, 24, 25, 26, 27, 28, 31, 32, 33, 34, 35, 36, 37, 39, 41, 42, 
  44, 46, 50, 52, 53, 54, 56, 57, 58, 59, 61, 62, 63, 64, 65, 68, 70, 71, 72, 
  77, 81, 84, 88, 89, 91, 93, 95, 97, 99, 100 ], 
[ 53, 10, 43, 41, 57, 14, 68, 20, 54, 62, 5, 49, 86, 56, 91, 48, 9, 87, 33, 
  64, 60, 13, 70, 92, 80, 69, 35, 88, 98, 4, 96, 79, 94, 71, 61, 27, 89, 97, 
  46, 28, 40, 3, 100, 17, 19, 39, 82, 52, 6, 16, 77, 76, 45, 67, 23, 31, 29, 
  12, 95, 72, 85, 7, 26, 38, 18, 24 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 16, 20, 21, 23, 
  24, 25, 26, 27, 28, 29, 31, 32, 33, 34, 35, 36, 38, 40, 41, 42, 43, 44, 47, 
  48, 49, 50, 53, 54, 55, 56, 58, 59, 62, 64, 65, 66, 68, 69, 70, 72, 74, 76, 
  78, 83, 84, 86, 90, 91, 92, 93, 94, 99, 100 ], 
[ 3, 77, 85, 63, 47, 30, 68, 21, 95, 13, 49, 33, 62, 6, 78, 81, 83, 35, 69, 
  50, 26, 61, 27, 93, 56, 39, 48, 5, 19, 52, 73, 12, 8, 89, 25, 86, 84, 14, 
  70, 29, 58, 88, 43, 37, 10, 92, 65, 22, 76, 38, 74, 34, 4, 94, 82, 67, 60, 
  2, 23, 59, 80, 11, 40, 98, 51, 28 ] ) ]
gap> f:=Random(s);
<partial perm on 27 pts>
gap> Display(f);
PartialPermNC( [ 5, 7, 11, 12, 14, 24, 25, 26, 27, 29, 31, 32, 34, 35, 41, 
  42, 44, 47, 48, 49, 50, 53, 62, 69, 70, 86, 92 ], 
[ 23, 52, 39, 62, 11, 47, 94, 34, 70, 50, 73, 89, 2, 86, 14, 81, 74, 83, 77, 
  92, 48, 26, 13, 98, 84, 60, 33 ] )
gap> d:=DClass(s, f);
{<partial perm on 27 pts>}
gap> Size(d);
1
gap> RhoOrb(d);
<closed orbit, 1 points with Schreier tree with log with grading>
gap> LambdaOrb(d);
<closed orbit, 35494 points with Schreier tree with log>
gap> f:=Random(s);
[ 5, 7, 56, 83, 92 ] -> [ 30, 52, 16, 21, 29 ]
gap> d:=DClassNC(s, f);
{[ 5, 7, 56, 83, 92 ] -> [ 30, 52, 16, 21, 29 ]}
gap> Size(d);
1
gap> iter:=IteratorOfDClasses(s);
<iterator of D-classes>
gap> repeat d:=NextIterator(iter); until Size(d)>1;
gap> d;
{[ 74, 84 ] -> [ 26, 6 ]}
gap> Size(d);
6793298
gap> f:=Random(d);
[ 1, 88 ] -> [ 78, 48 ]
gap> f in d;
true
gap> r:=RClass(d, f);
{[ 1, 88 ] -> [ 26, 6 ]}
gap> ForAll(r, x-> x in d);
true
gap> Size(r);
3686
gap> NrLClasses(d)*last;
6793298
gap> SchutzenbergerGroup(r);
Group([ (6,26) ])
gap> SchutzenbergerGroup(d);
Group([ (6,26) ])
gap> IsRegularDClass(d);
true
gap> IsRegularRClass(r);
true
gap> ForAll(r, x-> x in r);
true
gap> f:=Random(s);
<partial perm on 63 pts>
gap> d:=DClassNC(s, f);
{<partial perm on 63 pts>}
gap> Size(d);
1
gap> repeat d:=NextIterator(iter); until Size(d)>1;
gap> d;
{[ 41, 50 ] -> [ 26, 6 ]}
gap> Size(d);
3686
gap> f:=Random(d);
[ 41, 50 ] -> [ 17, 32 ]
gap> RereadPackage("citrus/greens.gi");
false
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> r:=RClassNC(d, f);
Error, Variable: 'l' must have an assigned value in
  return l; called from 
GreensRClassOfElementNC( arg[1], arg[2] ) called from
<function "RClassNC">( <arguments> )
 called from read-eval loop at line 149 of *stdin*
you can 'return;' after assigning a value
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> r:=RClassNC(d, f);
{[ 41, 50 ] -> [ 6, 26 ]}
gap> Size(r);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `LambdaOrb' on 1 arguments called from
LambdaOrb( r ) called from
SchutzenbergerGroup( r ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 151 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> r:+
Syntax error: ; expected
r:+
 ^
> ;
Syntax error: expression expected
;
^
gap> r:=RClassNC(d, f);
{[ 41, 50 ] -> [ 6, 26 ]}
gap> Size(r);
3686
gap> ForAll(r, x-> x in d);
true
gap> d=r;
true
gap> rr:=RClass(s, f);
{[ 41, 50 ] -> [ 26, 6 ]}
gap> rr=r;
true
gap> r=rr;
true
gap> d;
{[ 41, 50 ] -> [ 26, 6 ]}
gap> GroupHClass(d);
fail
gap>   
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
gap> f:=Random(s);
Transformation( [ 6, 6, 6, 6, 6, 10, 6, 6, 6, 6 ] )
gap> d:=DClassNC(s, f);
{Transformation( [ 6, 6, 6, 6, 6, 10, 6, 6, 6, 6 ] )}
gap> Size(d);
31680
gap> IsRegularDClass(d);
true
gap> GroupHClass(d);
{Transformation( [ 10, 10, 10, 10, 10, 6, 10, 10, 10, 10 ] )}
gap> s:=RandomBlockGroup(4, 10);
<semigroup with 4 generators>
gap> Size(s);
789
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 3, 4, 6, 10 ], [ 3, 4, 1, 6, 10 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 6 ], [ 10, 3, 9, 1, 5, 8 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 6, 10 ], [ 1, 8, 2, 3, 4, 9 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 8, 9, 10 ], [ 5, 8, 9, 7, 2, 6, 10 ] ) ]
gap> NrDClasses(s);
251
gap> d:=DClasses(s)[251];
{[ 2, 4 ] -> [ 2, 7 ]}
gap> Size(d);
1
gap> First(DClasses(s), IsRegularDClass);
{[ 1, 3, 4, 6, 10 ] -> [ 3, 4, 1, 6, 10 ]}
gap> d:=last;
{[ 1, 3, 4, 6, 10 ] -> [ 3, 4, 1, 6, 10 ]}
gap> Size(d);
3
gap> GroupHClass(d);
{<identity on [ 1, 3, 4, 6, 10 ]>}
gap> Size(last);
3
gap> h:=last2;
{<identity on [ 1, 3, 4, 6, 10 ]>}
gap> h=d;
true
gap> Elements(h);
[ <identity on [ 1, 3, 4, 6, 10 ]>, [ 1, 3, 4, 6, 10 ] -> [ 3, 4, 1, 6, 10 ], 
  [ 1, 3, 4, 6, 10 ] -> [ 4, 1, 3, 6, 10 ] ]
gap> Number(DClasses(s), IsRegularDClass);
6
gap> List(DClasses(s), Idempotents);
[ [ <identity on [ 1, 3, 4, 6, 10 ]> ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [ <identity on [ 10 ]>, <identity on [ 9 ]>, <identity on [ 3 ]>, 
      <identity on [ 6 ]>, <identity on [ 8 ]>, <identity on [ 4 ]>, 
      <identity on [ 2 ]>, <identity on [ 1 ]> ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [ <identity on [ 5 ]> ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [ <identity on [ 2, 8, 10 ]> ], [  ], [ <empty mapping> ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [ <identity on [ 3, 4 ]>, <identity on [ 1, 4 ]>, <identity on [ 1, 3 ]> ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], [  ], 
  [  ], [  ], [  ], [  ], [  ], [  ], [  ] ]
gap> Concatenation(last);
[ <identity on [ 1, 3, 4, 6, 10 ]>, <identity on [ 10 ]>, <identity on [ 9 ]>,
  <identity on [ 3 ]>, <identity on [ 6 ]>, <identity on [ 8 ]>, 
  <identity on [ 4 ]>, <identity on [ 2 ]>, <identity on [ 1 ]>, 
  <identity on [ 5 ]>, <identity on [ 2, 8, 10 ]>, <empty mapping>, 
  <identity on [ 3, 4 ]>, <identity on [ 1, 4 ]>, <identity on [ 1, 3 ]> ]
gap> ForAll(last, x-> x in s);
true
gap> Set(last2)=Idempotents(s);
false
gap> Set(last3)=Set(Idempotents(s));
true
gap> gens:=[Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] ),
> Transformation( [ 2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7 ] )];;
gap> s:=Monoid(gens);;
gap> List(DClasses(s), Idempotents);
[ [ Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] ) ], 
  [ Transformation( [ 1, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ) ], 
  [ Transformation( [ 4, 2, 2, 4, 5, 6, 7, 7, 7, 10, 11 ] ), 
      Transformation( [ 2, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
      Transformation( [ 6, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
      Transformation( [ 7, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
      Transformation( [ 5, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
      Transformation( [ 10, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
      Transformation( [ 11, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
      Transformation( [ 4, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ) ] ]
gap> Concatenation(last);
[ Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] ), 
  Transformation( [ 1, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 4, 2, 2, 4, 5, 6, 7, 7, 7, 10, 11 ] ), 
  Transformation( [ 2, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 6, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 7, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 5, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 10, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 11, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 4, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ) ]
gap> e:=last;
[ Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] ), 
  Transformation( [ 1, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 4, 2, 2, 4, 5, 6, 7, 7, 7, 10, 11 ] ), 
  Transformation( [ 2, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 6, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 7, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 5, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 10, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 11, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ), 
  Transformation( [ 4, 2, 4, 4, 5, 6, 7, 6, 10, 10, 11 ] ) ]
gap> IsDuplicateFree(e);
true
gap> ForAll(e, x-> x in s);
true
gap> Set(Idempotents(s))=Set(e);
true
gap> s:=RandomBlockGroup(4, 10);
<semigroup with 4 generators>
gap> Display(Generators(s));
[ PartialPermNC( [ 1, 2, 3, 4, 5, 6 ], [ 7, 10, 8, 6, 4, 2 ] ),
 PartialPermNC( [ 1, 2, 3, 4, 5, 9 ], [ 6, 8, 3, 10, 4, 2 ] ),
 PartialPermNC( [ 1, 2, 3, 5, 6, 7 ], [ 8, 7, 5, 6, 2, 9 ] ),
 PartialPermNC( [ 1, 2, 3, 5, 6, 8 ], [ 9, 3, 4, 7, 8, 6 ] ) ]
gap> Size(s);
489
gap> First(DClasses(s), IsRegularDClass);
{<empty mapping>}
gap> NrRegularDClasses(s);
5
gap> PositionsProperty(DClasses(s), IsRegularDClass);
[ 25, 26, 33, 36, 64 ]
gap> d:=DClasses(s)[26];
{[ 3 ] -> [ 8 ]}
gap> NrLClasses(d);
8
gap> NrRClasses(d);
8
gap> Size(d);
64
gap> Idempotents(d);
[ <identity on [ 8 ]>, <identity on [ 6 ]>, <identity on [ 3 ]>, 
  <identity on [ 5 ]>, <identity on [ 7 ]>, <identity on [ 2 ]>, 
  <identity on [ 9 ]>, <identity on [ 4 ]> ]
gap> ForAll(last, x-> x in d);
true
gap> dd:=DClassNC(s, Random(d));
{[ 8 ] -> [ 9 ]}
gap> dd=d;
true
gap> Size(dd);
64
gap> Idempotents(dd);
[ <identity on [ 9 ]>, <identity on [ 2 ]>, <identity on [ 8 ]>, 
  <identity on [ 7 ]>, <identity on [ 3 ]>, <identity on [ 6 ]>, 
  <identity on [ 5 ]>, <identity on [ 4 ]> ]
gap> Set(LClassReps(dd))=Set(LClassReps(d));
false
gap> LClassReps(dd);
[ [ 8 ] -> [ 9 ], [ 8 ] -> [ 2 ], <identity on [ 8 ]>, [ 8 ] -> [ 7 ], 
  [ 8 ] -> [ 3 ], [ 8 ] -> [ 6 ], [ 8 ] -> [ 5 ], [ 8 ] -> [ 4 ] ]
gap> LClassReps(d);
[ [ 3 ] -> [ 8 ], [ 3 ] -> [ 6 ], <identity on [ 3 ]>, [ 3 ] -> [ 5 ], 
  [ 3 ] -> [ 7 ], [ 3 ] -> [ 2 ], [ 3 ] -> [ 9 ], [ 3 ] -> [ 4 ] ]
gap> Set(List(LClassReps(d), x-> LClass(d, x)))=Set(List(LClassReps(dd), x-> LClass(d, x)));
true
gap> Set(List(LClassReps(d), x-> LClass(d, x)))=Set(List(LClassReps(dd), 
> x-> LClass(dd, x));
Syntax error: ) expected
x-> LClass(dd, x));
                  ^
gap> Set(List(LClassReps(d), x-> LClass(d, x)))=Set(List(LClassReps(dd),
> x-> LClass(dd, x)));
true
gap> ForAll(LClassReps(dd), x-> x in d);
true
gap> ForAll(LClassReps(d), x-> x in dd);
true
gap> quit;
