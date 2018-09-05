#############################################################################
##
#W  standard/isomorph.tst
#Y  Copyright (C) 2015-17                               James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/isomorph.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# helper functions
gap> BruteForceIsoCheck := function(iso)
>   local x, y;
>   if not IsInjective(iso) or not IsSurjective(iso) then
>     return false;
>   fi;
>   for x in Generators(Source(iso)) do
>     for y in Generators(Source(iso)) do
>       if x ^ iso * y ^ iso <> (x * y) ^ iso then
>         return false;
>       fi;
>     od;
>   od;
>   return true;
> end;;
gap> BruteForceInverseCheck := function(map)
> local inv;
>   inv := InverseGeneralMapping(map);
>   return ForAll(Source(map), x -> x = (x ^ map) ^ inv)
>     and ForAll(Range(map), x -> x = (x ^ inv) ^ map);
> end;;

# isomorph: SmallestMultiplicationTable, 1/2
gap> S := DualSymmetricInverseMonoid(2);
<inverse block bijection monoid of degree 2 with 2 generators>
gap> Size(S);
3
gap> SmallestMultiplicationTable(S);
[ [ 1, 2, 3 ], [ 2, 1, 3 ], [ 3, 3, 3 ] ]

# isomorph: SmallestMultiplicationTable, 2/2
gap> S := Semigroup(
> [PBR([[-4, 1, 2, 3], [-4, 1, 2, 4], [-2, -1, 1], [-4, -1, 1, 2, 4]],
>      [[-4, -3, 1, 4], [-3, -1, 3], [-4, -1, 1, 2, 4], [-4, -3, -2, 3, 4]]),
>  PBR([[-3, -2, -1, 1, 3, 4], [-4, -3, -2, -1, 2, 3], [-3, -2, -1, 1],
>       [-1, 1, 2, 3]],
>      [[-3, -2, -1, 2, 3, 4], [-3, 1, 4],
>       [-3, 1, 2], [-4, -3, 1, 2, 3, 4]]),
>  PBR([[-3, -1, 1, 3], [-1, 1, 2], [-2, -1, 1], [-4, -3, -1, 1, 2, 3, 4]],
>      [[-4, -3, -2], [], [-4, -1, 1, 2], [-4, -3, -2, -1, 2, 3, 4]]),
>  PBR([[-3, -2, -1, 2, 3], [-2, -1, 2, 4], [-3, -2, 1, 3],
>       [-4, -3, -2, -1, 1, 2, 3, 4]],
>      [[-4, -2, -1, 4], [-4, -3, -2, 2, 3], [-3, -2, -1, 1, 3, 4], [-3, 2]]),
>  PBR([[-4, -3, -2, -1, 2, 3], [-4, -1, 1, 2, 3, 4], [-3, 1, 2, 3],
>       [-4, -3, -2, -1, 1, 4]],
>      [[-4, -3, -1, 2, 4], [-3, -2, 2, 3, 4], [-4, -2, -1, 1],
>       [-4, -2, 1, 4]]),
>  PBR([[-4, -2, -1, 2, 3], [-4, -3, -1, 1, 3], [-4, 2, 4], [-3, -1, 1]],
>      [[-4, -3, -1, 1, 3], [-4, -3, 2, 3], [-4, -3, -2, -1, 2, 4],
>       [-4, -1, 1, 3, 4]])]);
<pbr semigroup of degree 4 with 6 generators>
gap> Size(S);
11
gap> SmallestMultiplicationTable(S);
[ [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], 
  [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], 
  [ 1, 1, 1, 1, 1, 6, 6, 7, 9, 9, 11 ], [ 1, 1, 1, 11, 1, 6, 6, 6, 9, 9, 11 ],
  [ 1, 1, 1, 11, 1, 6, 6, 6, 9, 9, 11 ], [ 1, 1, 1, 11, 1, 6, 6, 6, 9, 9, 11 ]
    , [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], 
  [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ] ]

# isomorph: IsIsomorphicSemigroup, 1/2
gap> S := DualSymmetricInverseMonoid(2);;
gap> T := Semigroup([Transformation([2, 1, 2, 2]),
>                    Transformation([1, 2, 3, 3])]);;
gap> IsIsomorphicSemigroup(S, T);
false

# isomorph: IsIsomorphicSemigroup, 2/2
gap> S := Semigroup([
>  Matrix(IsNTPMatrix, [[0, 1, 2], [4, 3, 0], [0, 2, 0]], 9, 4),
>  Matrix(IsNTPMatrix, [[1, 1, 0], [4, 1, 1], [0, 0, 0]], 9, 4)]);
<semigroup of 3x3 ntp matrices with 2 generators>
gap> IsIsomorphicSemigroup(S, S);
true
gap> T := AsSemigroup(IsTransformationSemigroup, S);
<transformation semigroup of size 46, degree 47 with 2 generators>
gap> IsIsomorphicSemigroup(S, T);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `IsomorphismSemigroups' on 2 arguments
gap> S := Semigroup(IdentityTransformation);
<trivial transformation group of degree 0 with 1 generator>
gap> T := Semigroup(PartialPerm([]));
<trivial partial perm group of rank 0 with 1 generator>
gap> IsIsomorphicSemigroup(S, T);
true
gap> T := JonesMonoid(4);
<regular bipartition *-monoid of degree 4 with 3 generators>
gap> IsIsomorphicSemigroup(S, T);
false

# isomorph: IsomorphismSemigroups, for infinite semigroup(s)
gap> S := FreeSemigroup(1);;
gap> T := TrivialSemigroup();;
gap> IsomorphismSemigroups(S, T);
fail
gap> IsomorphismSemigroups(S, FreeSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `IsomorphismSemigroups' on 2 arguments

# isomorph: IsomorphismSemigroups, for trivial semigroups
gap> S := TrivialSemigroup(IsTransformationSemigroup);
<trivial transformation group of degree 0 with 1 generator>
gap> T := TrivialSemigroup(IsBipartitionSemigroup);
<trivial block bijection group of degree 1 with 1 generator>
gap> map := IsomorphismSemigroups(S, T);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# isomorph: IsomorphismSemigroups, for monogenic semigroups
gap> S := MonogenicSemigroup(IsTransformationSemigroup, 3, 2);
<commutative non-regular transformation semigroup of size 4, degree 5 with 1 
 generator>
gap> T := MonogenicSemigroup(IsBipartitionSemigroup, 3, 2);
<commutative non-regular block bijection semigroup of size 4, degree 6 with 1 
 generator>
gap> map := IsomorphismSemigroups(S, T);
MappingByFunction( <commutative non-regular transformation semigroup 
 of size 4, degree 5 with 1 generator>, <commutative non-regular 
 block bijection semigroup of size 4, degree 6 with 1 generator>
 , function( x ) ... end, function( x ) ... end )
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# isomorph: IsomorphismSemigroups, for simple semigroups
gap> S := ReesMatrixSemigroup(SymmetricGroup(3), [[(), (1, 3, 2)],
>                                                 [(2, 3), (1, 2)],
>                                                 [(), (2, 3, 1)]]);
<Rees matrix semigroup 2x3 over Sym( [ 1 .. 3 ] )>
gap> T := ReesMatrixSemigroup(SymmetricGroup(3), [[(), ()],
>                                                 [(), ()],
>                                                 [(), ()]]);
<Rees matrix semigroup 2x3 over Sym( [ 1 .. 3 ] )>
gap> U := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of size 36, degree 37 with 4 generators>
gap> V := AsSemigroup(IsTransformationSemigroup, S);
<transformation semigroup of size 36, degree 37 with 4 generators>
gap> map := IsomorphismSemigroups(S, U);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroups(U, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroups(U, V);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> IsomorphismSemigroups(U, T);
fail

# isomorph: IsomorphismSemigroups, for 0-simple semigroups
gap> S := ReesZeroMatrixSemigroup(SymmetricGroup(3), [[(), (1, 3, 2)],
>                                                     [0, (1, 2)],
>                                                     [(), (2, 3, 1)]]);
<Rees 0-matrix semigroup 2x3 over Sym( [ 1 .. 3 ] )>
gap> T := ReesZeroMatrixSemigroup(SymmetricGroup(3), [[(), ()],
>                                                     [(), ()],
>                                                     [(), 0]]);
<Rees 0-matrix semigroup 2x3 over Sym( [ 1 .. 3 ] )>
gap> U := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of size 37, degree 38 with 5 generators>
gap> V := AsSemigroup(IsTransformationSemigroup, S);
<transformation semigroup of size 37, degree 38 with 5 generators>
gap> map := IsomorphismSemigroups(S, U);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroups(U, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroups(U, V);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> IsomorphismSemigroups(U, T);
fail
gap> F := FreeSemigroup(1);;
gap> F := F / [[F.1 ^ 4, F.1]];;
gap> S := ReesZeroMatrixSemigroup(F, [[F.1]]);;
gap> T := ReesZeroMatrixSemigroup(F, [[F.1 ^ 2]]);;
gap> map := IsomorphismSemigroups(S, T);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# isomorph: IsomorphismSemigroups, non-isomorphic partial order of D-classes
gap> S := ZeroSemigroup(3);
<commutative non-regular transformation semigroup of size 3, degree 4 with 2 
 generators>
gap> T := MonogenicSemigroup(3, 1);
<commutative non-regular transformation semigroup of size 3, degree 4 with 1 
 generator>
gap> IsomorphismSemigroups(S, T);
fail

# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(T);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/isomorph.tst");
