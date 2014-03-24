#############################################################################
##
#W  ideals.tst
#Y  Copyright (C) 2013-14                                James D. Mitchell
##                                                       Julius Jonusas 
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: ideals.tst");
gap> LoadPackage("semigroups", false);;

#
gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoSemigroups:=InfoLevel(InfoSemigroups);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoSemigroups, 0);

#
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ), 
>   Transformation( [ 4, 2, 1, 5, 5 ] ), 
>   Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> I := SemigroupIdeal(s, gens);
<transformation semigroup ideal on 5 pts with 3 generators>
gap> data := SemigroupData(I);
<open semigroup ideal data with 0 reps, 0 lambda-values, 0 rho-values>
gap> Size(I);
731
gap> NrDClasses(I);
4
gap> GreensDClasses(I);
[ {Transformation( [ 3, 4, 1, 2, 1 ] )}, {Transformation( [ 4, 2, 2, 2, 4 ] )}
    , {Transformation( [ 4, 5, 2, 4, 4 ] )}, 
  {Transformation( [ 2, 2, 2, 2, 2 ] )} ]

#
gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ), 
> Transformation( [ 2, 4, 1, 2 ] ), 
> Transformation( [ 3, 1, 1, 3 ] ), 
> Transformation( [ 3, 3, 4, 1 ] ) ];;
gap> s:=Semigroup(gens);;
gap> I := SemigroupIdeal(s, gens{[1,2]});
<transformation semigroup ideal on 4 pts with 2 generators>
gap> o := LambdaOrb(I);
<open ideal lambda orbit with 0 points in 0 components>
gap> I.1 in o;
false
gap> UpdateIdealLambdaOrb(o, LambdaFunc(I)(I.1), I.1, fail, fail, 1);
2

#
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ), 
>  Transformation( [ 1, 4, 1, 2 ] ), 
>  Transformation( [ 2, 4, 1, 1 ] ), 
>  Transformation( [ 3, 4, 2, 2 ] ) ];; 
gap> s:=Semigroup(gens);;
gap> I := SemigroupIdeal(s, [gens[2]* gens[1], gens[3]^3]);
<transformation semigroup ideal on 4 pts with 2 generators>
gap> o := RhoOrb(I);
<open ideal rho orbit with 0 points in 0 components>
gap> I.1 in o;
false
gap> UpdateIdealRhoOrb(o, RhoFunc(I)(I.1), I.1, fail, fail, 1);
2

#
gap> gens := [                                                              
> PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 10 ], [ 4, 6, 7, 3, 8, 2, 9, 5 ] ),
> PartialPerm( [ 1, 2, 7, 9 ], [ 5, 6, 4, 3 ] ),
> PartialPerm( [ 1, 2, 6, 7, 8 ], [ 5, 1, 6, 2, 3 ] )];;
gap> s := Semigroup(gens);
<partial perm semigroup on 10 pts with 3 generators>
gap> I := SemigroupIdeal(s, [gens[1]^2, gens[2]]);
<partial perm semigroup ideal on 10 pts with 2 generators>
gap> R := GreensRClassOfElement(I, gens[1]^2);
{PartialPerm( [ 1, 2, 3, 4, 6, 10 ], [ 3, 2, 9, 7, 6, 8 ] )}
gap> DClassOfRClass(R);
{PartialPerm( [ 1, 2, 3, 4, 6, 10 ], [ 3, 2, 9, 7, 6, 8 ] )}
gap> L := GreensLClassOfElement(I, gens[1]^2);
{PartialPerm( [ 1, 2, 3, 4, 6, 10 ], [ 3, 2, 9, 7, 6, 8 ] )}
gap> DClassOfLClass(L);
{PartialPerm( [ 1, 2, 3, 4, 6, 10 ], [ 3, 2, 9, 7, 6, 8 ] )}

# \in for an inverse op semigroup ideal

#
gap> STOP_TEST( "Semigroups package: ideals.tst", 10000);
