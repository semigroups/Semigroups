gens:=[ Transformation( [ 1, 3, 7, 9, 1, 12, 13, 1, 15, 9, 1, 18, 1, 1, 
 13, 1, 1, 21, 1, 1, 1, 1, 1, 25, 26, 1 ] ), 
 Transformation( [ 1, 5, 1, 5, 11, 1, 1, 14, 1, 16, 17, 1, 1, 19, 1, 11, 1, 
      1, 1, 23, 1, 16, 19, 1, 1, 1 ] ), 
Transformation( [ 1, 4, 8, 1, 10, 1, 8, 1, 1, 1, 10, 1, 8, 10, 1, 1, 20, 1, 
      22, 1, 8, 1, 1, 1, 1, 1 ] ), 
Transformation( [ 1, 6, 6, 1, 1, 1, 6, 1, 1, 1, 1, 1, 6, 1, 6, 1, 1, 6, 1, 
      1, 24, 1, 1, 1, 1, 6 ] ) ];;
s:=Semigroup(gens);
f:=Transformation( [ 1, 9, 1, 1, 9, 1, 1, 1, 1, 1, 9, 1, 1, 9, 1, 1, 1, 1, 1, 
1, 1, 1, 1, 1, 1, 1 ] );;


gens:=[ Transformation( [ 6, 4, 3, 2, 5, 1 ] ),
  Transformation( [ 1, 2, 3, 4, 5, 6 ] ), 
  Transformation( [ 5, 3, 3, 2, 4, 1 ] ), 
  Transformation( [ 1, 3, 3, 4, 5, 2 ] ), 
  Transformation( [ 4, 5, 2, 3, 3, 1 ] ), 
  Transformation( [ 6, 4, 3, 5, 2, 3 ] ), 
  Transformation( [ 5, 2, 3, 4, 3, 6 ] ), 
  Transformation( [ 1, 3, 2, 5, 4, 5 ] ), 
  Transformation( [ 4, 3, 2, 2, 1, 5 ] ), 
  Transformation( [ 1, 3, 3, 5, 2, 4 ] ), 
  Transformation( [ 6, 3, 3, 2, 1, 5 ] ), 
  Transformation( [ 6, 3, 4, 5, 2, 2 ] ), 
  Transformation( [ 6, 4, 3, 2, 2, 5 ] ),
  Transformation( [ 1, 3, 2, 3, 5, 4 ] ), 
  Transformation( [ 1, 2, 3, 4, 5, 2 ] ), 
  Transformation( [ 2, 4, 3, 4, 6, 5 ] ), 
  Transformation( [ 2, 4, 3, 3, 6, 1 ] ), 
  Transformation( [ 6, 4, 3, 2, 3, 1 ] ), 
  Transformation( [ 6, 4, 3, 2, 2, 1 ] ) ];;
s:=Semigroup(gens);
f:=Transformation( [ 4, 3, 2, 3, 3, 5 ] );
h:=GreensHClassOfElement(s, f);

gap> s:=Semigroup(gens);     
<semigroup with 4 generators>
gap> f;
Transformation( [ 1, 8, 2, 6, 2, 6, 1, 6 ] )
gap> l:=GreensLClassOfElement(s, f);
{Transformation( [ 1, 8, 2, 6, 2, 6, 1, 6 ] )}

