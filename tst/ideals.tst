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
<regular transformation semigroup ideal on 5 pts with 3 generators>
gap> data := SemigroupData(I);
<closed semigroup ideal data with 26 reps, 23 lambda-values, 26 rho-values>
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
<non-regular transformation semigroup ideal on 4 pts with 2 generators>
gap> o := LambdaOrb(I);
<closed ideal lambda orbit with 11 points in 2 components>

#
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ), 
>  Transformation( [ 1, 4, 1, 2 ] ), 
>  Transformation( [ 2, 4, 1, 1 ] ), 
>  Transformation( [ 3, 4, 2, 2 ] ) ];; 
gap> s:=Semigroup(gens);;
gap> I := SemigroupIdeal(s, [gens[2]* gens[1], gens[3]^3]);
<non-regular transformation semigroup ideal on 4 pts with 2 generators>
gap> o := RhoOrb(I);
<closed ideal rho orbit with 10 points in 2 components>

#
gap> gens := [                                                              
> PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 10 ], [ 4, 6, 7, 3, 8, 2, 9, 5 ] ),
> PartialPerm( [ 1, 2, 7, 9 ], [ 5, 6, 4, 3 ] ),
> PartialPerm( [ 1, 2, 6, 7, 8 ], [ 5, 1, 6, 2, 3 ] )];;
gap> s := Semigroup(gens);
<partial perm semigroup on 10 pts with 3 generators>
gap> I := SemigroupIdeal(s, [gens[1]^2, gens[2]]);
<non-regular partial perm semigroup ideal on 10 pts with 2 generators>
gap> R := GreensRClassOfElement(I, gens[1]^2);
{PartialPerm( [ 1, 2, 3, 4, 6, 10 ], [ 3, 2, 9, 7, 6, 8 ] )}
gap> DClassOfRClass(R);
{PartialPerm( [ 1, 2, 3, 4, 6, 10 ], [ 3, 2, 9, 7, 6, 8 ] )}
gap> L := GreensLClassOfElement(I, gens[1]^2);
{PartialPerm( [ 1, 2, 3, 4, 6, 10 ], [ 3, 2, 9, 7, 6, 8 ] )}
gap> DClassOfLClass(L);
{PartialPerm( [ 1, 2, 3, 4, 6, 10 ], [ 3, 2, 9, 7, 6, 8 ] )}

# \in for an inverse op semigroup ideal
gap> S:=InverseSemigroup(
> PartialPerm( [ 1, 2, 3, 5, 6, 7, 8 ], [ 5, 9, 10, 6, 3, 8, 4 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5, 6, 8 ], [ 6, 2, 8, 4, 7, 5, 3 ] ), 
>  PartialPerm( [ 1, 2, 4, 6, 8, 9 ], [ 7, 10, 1, 9, 4, 2 ] ), 
>  PartialPerm( [ 1, 2, 4, 7, 8, 9 ], [ 10, 7, 8, 5, 9, 1 ] ), 
>  PartialPerm( [ 1, 2, 3, 6, 8, 10 ], [ 2, 6, 7, 9, 1, 5 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5, 8, 10 ], [ 3, 1, 4, 2, 5, 6, 7 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 6, 7, 8, 10 ], [ 3, 8, 1, 9, 4, 10, 5, 6 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 10 ], [ 4, 7, 6, 9, 10, 1, 3, 2 ] ), 
>  PartialPerm( [ 1, 2, 4, 5, 8, 10 ], [ 6, 2, 7, 8, 10, 4 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5, 8, 10 ], [ 7, 1, 4, 3, 2, 6, 5 ] ) );;
gap> I:=SemigroupIdeal(S, 
> [ PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 10 ], [ 4, 7, 6, 9, 10, 1, 3, 2 ] ), 
>   PartialPerm( [ 4 ], [ 3 ] ), PartialPerm( [ 6, 7 ], [ 5, 8 ] ) ]);;
gap> Size(I);
4626941

# attributes.gi
gap> S:=Semigroup([ Transformation( [ 1, 3, 4, 1, 3, 5 ] ), 
>  Transformation( [ 1, 5, 3, 5, 5, 5 ] ), 
>  Transformation( [ 2, 4, 6, 1, 6, 5 ] ), 
>  Transformation( [ 3, 2, 4, 2, 3, 3 ] ), 
>  Transformation( [ 4, 1, 2, 6, 2, 1 ] ), 
>  Transformation( [ 4, 6, 4, 3, 3, 3 ] ), 
>  Transformation( [ 4, 6, 5, 5, 2, 6 ] ), 
>  Transformation( [ 5, 1, 6, 1, 6, 3 ] ), 
>  Transformation( [ 5, 2, 5, 3, 5, 3 ] ), 
>  Transformation( [ 6, 4, 5, 5, 1, 6 ] ) ]);;
gap> I:=SemigroupIdeal(S, Transformation( [ 1, 3, 4, 1, 3, 5 ] ));;
gap> J:=SemigroupIdeal(S, Transformation( [ 1, 5, 3, 5, 5, 5 ] ));;
gap> IsGreensDLeq(I); IsGreensDLeq(J);
function( x, y ) ... end
function( x, y ) ... end
gap> Length(MaximalDClasses(I)); 
265
gap> MaximalDClasses(J);
[ {Transformation( [ 1, 5, 3, 5, 5, 5 ] )} ]
gap> StructureDescriptionSchutzenbergerGroups(I);
[ "1", "C2", "S3" ]
gap> StructureDescriptionSchutzenbergerGroups(J);
[ "1", "C2", "S3" ]
gap> StructureDescriptionMaximalSubgroups(J);
[ "1", "C2", "S3" ]
gap> StructureDescriptionMaximalSubgroups(I);
[ "1", "C2", "S3" ]
gap> GroupOfUnits(I);
fail
gap> GroupOfUnits(J);
fail
gap> IdempotentGeneratedSubsemigroup(I);;
gap> IdempotentGeneratedSubsemigroup(J);;
gap> last=last2;
true
gap> x:=Transformation( [ 5, 5, 1, 4, 1, 1 ] );;
gap> x in I; 
true
gap> x in J;
true
gap> Length(InversesOfSemigroupElement(I, x))=84;
true
gap> InversesOfSemigroupElement(J, x)=InversesOfSemigroupElement(I, x);
true
gap> MultiplicativeNeutralElement(I);
fail
gap> MultiplicativeNeutralElement(J);
fail
gap> MultiplicativeNeutralElement(I);
fail
gap> MultiplicativeNeutralElement(J);
fail
gap> MultiplicativeZero(I);
fail
gap> MultiplicativeZero(J);
fail
gap> MinimalIdeal(I);
<simple transformation semigroup ideal on 6 pts with 1 generator>
gap> MinimalIdeal(J);
<simple transformation semigroup ideal on 6 pts with 1 generator>
gap> last=last2;
true
gap> MinimalDClass(I);
{Transformation( [ 1, 1, 1, 1, 1, 1 ] )}
gap> MinimalDClass(J);
{Transformation( [ 5, 5, 5, 5, 5, 5 ] )}
gap> 

# attributes
gap> S:=InverseSemigroup(
>  PartialPerm( [ 1, 2, 3, 5, 6, 7, 8 ], [ 5, 9, 10, 6, 3, 8, 4 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5, 6, 8 ], [ 6, 2, 8, 4, 7, 5, 3 ] ), 
>  PartialPerm( [ 1, 2, 4, 6, 8, 9 ], [ 7, 10, 1, 9, 4, 2 ] ), 
>  PartialPerm( [ 1, 2, 4, 7, 8, 9 ], [ 10, 7, 8, 5, 9, 1 ] ), 
>  PartialPerm( [ 1, 2, 3, 6, 8, 10 ], [ 2, 6, 7, 9, 1, 5 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5, 8, 10 ], [ 3, 1, 4, 2, 5, 6, 7 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 6, 7, 8, 10 ], [ 3, 8, 1, 9, 4, 10, 5, 6 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 10 ], [ 4, 7, 6, 9, 10, 1, 3, 2 ] ), 
>  PartialPerm( [ 1, 2, 4, 5, 8, 10 ], [ 6, 2, 7, 8, 10, 4 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5, 8, 10 ], [ 7, 1, 4, 3, 2, 6, 5 ] ));;
gap> K:=SemigroupIdeal(S, S.1*S.2^2*S.8);
<inverse partial perm semigroup ideal on 10 pts with 1 generator>
gap> MaximalDClasses(K);
[ {PartialPerm( [ 3, 6, 9 ], [ 3, 6, 9 ] )} ]
gap> StructureDescriptionMaximalSubgroups(K);
[ "1", "C2", "S3" ]
gap> StructureDescriptionSchutzenbergerGroups(K);
[ "1", "C2", "S3" ]
gap> GroupOfUnits(K);
fail
gap> Size(IdempotentGeneratedSubsemigroup(K));
176
gap> x:=PartialPerm( [ 2, 4, 6, 8, 9 ], [ 10, 1, 9, 4, 2 ] );;
gap> x in K;
false
gap> x:=PartialPerm([9],[9]);;
gap> x in K;
true
gap> InversesOfSemigroupElement(K, x);
[ <identity partial perm on [ 9 ]> ]
gap> MultiplicativeZero(K);
<empty partial perm>
gap> MultiplicativeNeutralElement(K);
fail
gap> MinimalIdeal(K);
<partial perm group on 10 pts with 1 generator>
gap> MinimalDClass(K);
{PartialPerm( [  ], [  ] )}
gap> I:=MinimalIdeal(K);
<partial perm group on 10 pts with 1 generator>
gap> IsomorphismPermGroup(I);
MappingByFunction( <trivial partial perm group on 10 pts with 1 generator>
 , Group(()), <Attribute "AsPermutation">, function( x ) ... end )

# attributes.gi
gap> S:=Monoid( Transformation( [ 2, 6, 7, 2, 6, 1, 1, 5 ] ), 
>   Transformation( [ 3, 8, 1, 4, 5, 6, 7, 1 ] ), 
>   Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] ), 
>   Transformation( [ 7, 1, 7, 4, 2, 5, 6, 3 ] ) );;
gap> L:=SemigroupIdeal(S, GeneratorsOfSemigroup(S));
<non-regular transformation semigroup ideal on 8 pts with 5 generators>
gap> Length(MaximalDClasses(L));
1
gap> L=S;
true
gap> MaximalDClasses(L);
[ {IdentityTransformation} ]
gap> StructureDescriptionSchutzenbergerGroups(L);
[ "1", "C2", "C4", "C5", "S3", "S4" ]
gap> StructureDescriptionMaximalSubgroups(L);
[ "1", "C2", "C4", "C5", "S3", "S4" ]
gap> GroupOfUnits(L);
<trivial transformation group>
gap> IdempotentGeneratedSubsemigroup(L);;
gap> x:=Transformation( [ 1, 4, 4, 5, 5, 3, 3, 1 ] );;
gap> InversesOfSemigroupElement(L, x);
[  ]
gap> InversesOfSemigroupElement(S, x);
[  ]
gap> MultiplicativeNeutralElement(L);
IdentityTransformation
gap> MultiplicativeZero(L);
fail
gap> MinimalIdeal(L);
<simple transformation semigroup ideal on 8 pts with 1 generator>
gap> L:=SemigroupIdeal(S, GeneratorsOfSemigroup(S));
<transformation semigroup ideal on 8 pts with 5 generators>
gap> MinimalIdeal(J);
<simple transformation semigroup ideal on 6 pts with 1 generator>
gap> MinimalIdeal(L);
<simple transformation semigroup ideal on 8 pts with 1 generator>
gap> MinimalDClass(L);
{Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1 ] )}
gap> MinimalDClass(S);
{Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1 ] )}

# 
gap> R:=Semigroup( [ Bipartition( [ [ 1, 2, 3, 5, -3 ], [ 4, 6, 7, -5 ], [ -1 ],
> [ -2, -4, -7 ], [ -6 ] ] ), Bipartition( [ [ 1, 2, 6, -5 ], [ 3 ], [ 4, 5, -2 ],
> [ 7, -1, -3, -4, -7 ], [ -6 ] ] ), Bipartition( [ [ 1, 3, 4, 5, -2, -3, -5 ], [
> 2, 6, -1, -6, -7 ], [ 7 ], [ -4 ] ] ), Bipartition( [ [ 1, 3, 4, 6, -7 ], [ 2,
> 5, -1, -5 ], [ 7, -2, -3, -4 ], [ -6 ] ] ), Bipartition( [ [ 1, 3 ], [ 2, 5, 6,
> -1, -2, -3 ], [ 4, 7, -4, -7 ], [ -5 ], [ -6 ] ] ), Bipartition( [ [ 1, 4, 5,
> -1, -2, -4, -6 ], [ 2, 3, 7, -3, -5, -7 ], [ 6 ] ] ), Bipartition( [ [ 1, -1, -4
> ], [ 2, 3, 4, 5, 6, 7, -2, -6 ], [ -3, -5, -7 ] ] ), Bipartition( [ [ 1, 7, -6
> ], [ 2, 3, 4, 5, -1, -2, -4 ], [ 6, -3, -5 ], [ -7 ] ] ), Bipartition( [ [ 1, 5,
> -2, -7 ], [ 2, 3, 6, -4 ], [ 4, -1, -5, -6 ], [ 7 ], [ -3 ] ] ), Bipartition( [
> [ 1, -3, -4 ], [ 2 ], [ 3, 7, -1, -7 ], [ 4, 5, -6 ], [ 6, -5 ], [ -2 ] ] ) ]
> );; 
gap> gens:= [ Bipartition( [ [ 1, 2, 3, 5, -3 ], [ 4, 6, 7, -5 ], [ -1 ], [
> -2, -4, -7 ], [ -6 ] ] ), Bipartition( [ [ 1, 2, 6, -5 ], [ 3 ], [ 4, 5, -2 ], [
> 7, -1, -3, -4, -7 ], [ -6 ] ] ), Bipartition( [ [ 1, 3 ], [ 2, 5, 6, -1, -2, -3
> ], [ 4, 7, -4, -7 ], [ -5 ], [ -6 ] ] ), Bipartition( [ [ 1, -3, -4 ], [ 2 ], [
> 3, 7, -1, -7 ], [ 4, 5, -6 ], [ 6, -5 ], [ -2 ] ] ) ];;
gap> M:=SemigroupIdeal(R, gens);
<non-regular bipartition semigroup ideal on 7 pts with 4 generators>
gap> Length(MaximalDClasses(M));
10
gap> Length(GeneratorsOfSemigroup(M));
95
gap> StructureDescriptionSchutzenbergerGroups(M);
[ "1" ]
gap> StructureDescriptionMaximalSubgroups(M);
[ "1" ]
gap> Size(IdempotentGeneratedSubsemigroup(M));
1441
gap> GroupOfUnits(M);
fail
gap> x:=Bipartition( [ [ 1, 2, 3, 4, 5, 6, 7, -3, -5 ], [ -1 ], 
> [ -2, -4, -7 ], [ -6 ] ] );;
gap> x in M;
true
gap> Length(InversesOfSemigroupElement(M, x));
875
gap> ForAll(InversesOfSemigroupElement(M, x), y-> y in M);
true
gap> MultiplicativeNeutralElement(M);
fail
gap> MultiplicativeZero(M);
fail
gap> MinimalIdeal(M);
<simple bipartition semigroup ideal on 7 pts with 1 generator>
gap> MinimalDClass(M);
{Bipartition( [ [ 1, 2, 3, 4, 5, 6, 7 ], [ -1 ], [ -2, -4, -7 ], [ -3, -5 ], [\
 -6 ] ] )}
gap> MinimalDClass(R);
{Bipartition( [ [ 1, 2, 3, 4, 5, 6, 7 ], [ -1, -2, -3, -4, -5, -7 ], [ -6 ] ] \
)}

#
gap> STOP_TEST( "Semigroups package: ideals.tst", 10000);
