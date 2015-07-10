############################################################################
##
#W  standard/attributes/maximal.tst
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## TODO: improve code coverage!!

gap> START_TEST("Semigroups package: standard/attributes/maximal.tst");
gap> LoadPackage("semigroups", false);;

#  
gap> SemigroupsStartTest();;

#T# MaximalTest1: IsMaximalSubsemigroup
gap> S := Semigroup([
>   Transformation( [ 1, 2, 4, 4 ] ),
>   Transformation( [ 4, 4, 1, 4 ] ),
>   Transformation( [ 2, 1, 4, 2 ] ) ]);
<transformation semigroup of degree 4 with 3 generators>
gap> T := Semigroup( [ 
> Transformation( [ 2, 1, 4, 2 ] ), 
> Transformation( [ 2, 1, 2, 2 ] ),
> Transformation( [ 4, 4, 1, 4 ] ) ] );
<transformation semigroup of degree 4 with 3 generators>
gap> IsMaximalSubsemigroup(S, T);
true
gap> U := Semigroup([
>   Transformation( [ 2, 2, 1, 1 ] ),
>   Transformation( [ 2, 2, 3, 4 ] ),
>   Transformation( [ 3, 4, 2, 4 ] ) ]);
<transformation semigroup of degree 4 with 3 generators>
gap> IsSubsemigroup(S, U);
false
gap> IsMaximalSubsemigroup(S, U);
false
gap> IsSubsemigroup(U, S);
false
gap> IsMaximalSubsemigroup(U, S);
false
gap> IsSubsemigroup(S, S);
true
gap> S <> S;
false
gap> IsMaximalSubsemigroup(S, S);
false

#T# maximal: MaximalSubsemigroups, for RMS and group, error 1/3
gap> U := BrauerMonoid(2);
<regular bipartition monoid of degree 2 with 2 generators>
gap> R := ReesMatrixSemigroup(U, [[One(U)]]);
<Rees matrix semigroup 1x1 over <regular bipartition monoid of degree 2 with
  2 generators>>
gap> MaximalSubsemigroups(R, Group(()));
Error, Semigroups: MaximalSubsemigroups: usage,
the first argument <R> must be a Rees matrix semigroup whose underlying
semigroup is a group,

#T# maximal: MaximalSubsemigroups, for RMS and group, error, 2/3
gap> R := RectangularBand(2, 2);
<Rees matrix semigroup 2x2 over Group(())>
gap> MaximalSubsemigroups(R, Group((1,2)));
Error, Semigroups: MaximalSubsemigroups: usage,
the second argument <H> must be a subgroup of the underlying
group of the Rees matrix semigroup in the first argument, <R>,

#T# maximal: MaximalSubsemigroups, for RMS and group, error, 3/3
gap> R := ReesMatrixSemigroup( SymmetricGroup(3), [ [ (), () ], [ (), () ] ] );
<Rees matrix semigroup 2x2 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R, Group(()));
Error, Semigroups: MaximalSubsemigroups: usage,
the second argument <H> must be a maximal subgroup of the underlying
group of the Rees matrix semigroup in the first argument, <R>,

#T# maximal: MaximalSubsemigroups, for RMS and group, 1/3
gap> R := ReesMatrixSemigroup( SymmetricGroup(3), [ [ (), () ], [ (), () ] ] );
<Rees matrix semigroup 2x2 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R, MaximalSubgroups(SymmetricGroup(3))[1]);
[ <subsemigroup of 2x2 Rees matrix semigroup with 3 generators> ]

#T# maximal: MaximalSubsemigroups, for RMS and group, 2/3
gap> R := ReesMatrixSemigroup( SymmetricGroup(3), [ [ (), () , (), () ] ] );
<Rees matrix semigroup 4x1 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R, MaximalSubgroups(SymmetricGroup(3))[1]);
[ <subsemigroup of 4x1 Rees matrix semigroup with 5 generators> ]

#T# maximal: MaximalSubsemigroups, for RMS and group, 3/3
gap> R := ReesMatrixSemigroup( SymmetricGroup(3), [ [()], [()] , [()], [()] ] );
<Rees matrix semigroup 1x4 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R, MaximalSubgroups(SymmetricGroup(3))[1]);
[ <subsemigroup of 1x4 Rees matrix semigroup with 5 generators> ]

#T# maximal: MaximalSubsemigroupsNC, for RMS and group, no result, 1/1
gap> G := SymmetricGroup(3);;
gap> R := ReesMatrixSemigroup(G, [ [ (1,2,3), () ], [ (), () ] ] );
<Rees matrix semigroup 2x2 over Sym( [ 1 .. 3 ] )>
gap> List(MaximalSubgroups(G), H -> MaximalSubsemigroups(R, H));
[ [ <subsemigroup of 2x2 Rees matrix semigroup with 3 generators> ], [  ], 
  [  ], [  ] ]

#T# maximal: MaximalSubsemigroups, for non-simple, 1/?
gap> MaximalSubsemigroups(BrauerMonoid(3));
[ <bipartition semigroup of degree 3 with 2 generators>, 
<bipartition semigroup of degree 3 with 4 generators>, 
<bipartition semigroup of degree 3 with 3 generators>, 
<bipartition semigroup of degree 3 with 3 generators>, 
<bipartition semigroup of degree 3 with 2 generators> ]

#T# maximal: MaximalSubsemigroups, for non-simple, 2/?
gap> MaximalSubsemigroups(ZeroSemigroup(5));
[ <partial perm semigroup on 3 pts with 3 generators>, 
  <partial perm semigroup on 3 pts with 3 generators>, 
  <partial perm semigroup on 3 pts with 3 generators>, 
  <partial perm semigroup on 3 pts with 3 generators> ]

#T# maximal: MaximalSubsemigroups, for non-simple, 3/?
gap> S := SymmetricInverseMonoid(3);
<inverse partial perm monoid of rank 3 with 3 generators>
gap> MaximalSubsemigroups(S)[1];
<partial perm semigroup on 3 pts with 3 generators>
gap> MaximalSubsemigroups(S);
[ <partial perm semigroup on 3 pts with 3 generators>, 
  <partial perm semigroup on 3 pts with 3 generators>, 
  <partial perm semigroup on 3 pts with 5 generators>, 
  <partial perm semigroup on 3 pts with 5 generators>, 
  <partial perm semigroup on 3 pts with 3 generators> ]

#T# SEMIGROUPS_UnbindVariables

#E#
gap> STOP_TEST("Semigroups package: standard/attributes/maximal.tst");
