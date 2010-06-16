


gap> s:=RandomSemilatticeAsSemigroup(10,6);
<semigroup with 10 generators>
gap> Size(s);
13
gap> CongruencesSemilatticeByCayleyGraph(s);;
gap> time;
913
gap> Length(last2);
1086
gap> GeneratorsOfSemigroup(s);
gap> gens:=[ Transformation( [ 1, 2, 6, 4, 5, 6 ] ), Transformation( [ 1, 2, 6, 4, 6, 6 ] ), 
>  Transformation( [ 1, 2, 6, 6, 6, 6 ] ), Transformation( [ 1, 6, 3, 4, 6, 6 ] ), 
>  Transformation( [ 1, 6, 6, 4, 6, 6 ] ), Transformation( [ 1, 6, 6, 6, 6, 6 ] ), 
>  Transformation( [ 6, 2, 3, 4, 6, 6 ] ), Transformation( [ 6, 2, 6, 4, 6, 6 ] ), 
>  Transformation( [ 6, 2, 6, 6, 6, 6 ] ), Transformation( [ 6, 6, 3, 4, 6, 6 ] ), 
>  Transformation( [ 6, 6, 3, 6, 6, 6 ] ), Transformation( [ 6, 6, 6, 4, 6, 6 ] ), 
>  Transformation( [ 6, 6, 6, 6, 6, 6 ] ) ]
gap> s:=Semigroup(gens);
gap> CongruencesSemilatticeByCayleyGraph(s);;
#gap> time;
#913
gap> Length(last2);
1086
gap> ForAll(CongruencesSemilattice(s), x-> IsCongruenceOfSemilattice(s, x));
true;