gap> s:=SingularSemigroup(8);
<semigroup with 56 generators>
gap> Size(s);
16736896
gap> HasIsRegularSemigroup(s);
true
gap> IsRegularSemigroup(s);
true
gap> 8^8-Factorial(8);
16736896
gap> ReadCitrus("pkg/citrus/examples/cycle.citrus.gz");;
gap> x:=last;;
gap> Length(x);
19
gap> x[1];
[ Transformation( [ 2, 1 ] ) ]
gap> semis:=List(x, Semigroup);
[ <semigroup with 1 generator>, <semigroup with 2 generators>, 
  <semigroup with 3 generators>, <semigroup with 2 generators>, 
  <semigroup with 3 generators>, <semigroup with 2 generators>, 
  <semigroup with 4 generators>, <semigroup with 2 generators>, 
  <semigroup with 5 generators>, <semigroup with 2 generators>, 
  <semigroup with 10 generators>, <semigroup with 2 generators>, 
  <semigroup with 19 generators>, <semigroup with 2 generators>, 
  <semigroup with 56 generators>, <semigroup with 2 generators>, 
  <semigroup with 154 generators>, <semigroup with 2 generators>, 
  <semigroup with 503 generators> ]
gap> List(semis, IsRegularSemigroup);
[ true, true, true, true, true, true, true, true, false, true, false, true, 
  false, true, false, true, false, true, false ]
gap> semis:=Filtered(semis, IsRegularSemigroup);
[ <semigroup with 1 generator>, <semigroup with 2 generators>, 
  <semigroup with 3 generators>, <semigroup with 2 generators>, 
  <semigroup with 3 generators>, <semigroup with 2 generators>, 
  <semigroup with 4 generators>, <semigroup with 2 generators>, 
  <semigroup with 2 generators>, <semigroup with 2 generators>, 
  <semigroup with 2 generators>, <semigroup with 2 generators>, 
  <semigroup with 2 generators> ]
gap> quit;
