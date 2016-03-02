#############################################################################
##
#W  standard/grpperm.tst
#Y  Copyright (C) 2015                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/grpperm.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Load a function for isomorphism checking
gap> BruteForceIsoCheck := function(iso)
>   local x, y;
>   if not IsInjective(iso) or not IsSurjective(iso) then
>     return false;
>   fi;
>   #homomorphism
>   for x in Source(iso) do
>     for y in Source(iso) do
>       if x ^ iso * y ^ iso <> (x * y) ^ iso then
>         return false;
>       fi;
>     od;
>   od;
>   return true;
> end;
function( iso ) ... end

#T# IsomorphismPermGroup: for a transformation semigroup
gap> S := Semigroup([Transformation([3, 2, 4, 1]), Transformation([2, 1])]);
<transformation semigroup of degree 4 with 2 generators>
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> G := Range(iso);
Group([ (1,3,4), (1,2) ])
gap> S := Semigroup([Transformation([2, 1, 2]), Transformation([3, 3, 1])]);
<transformation semigroup of degree 3 with 2 generators>
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument <S> must satisfy IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Rees Matrix Semigroup
gap> R := ReesMatrixSemigroup(Group((1,3,5), (2,4)), [[()]]);
<Rees matrix semigroup 1x1 over Group([ (1,3,5), (2,4) ])>
gap> iso := IsomorphismPermGroup(R);;
gap> G := Range(iso);
<group of size 6, with 2 generators>
gap> BruteForceIsoCheck(iso);
true
gap> R := ReesMatrixSemigroup(Group((1,3,5), (2,4)), [[(1,5,3), ()]]);
<Rees matrix semigroup 2x1 over Group([ (1,3,5), (2,4) ])>
gap> iso := IsomorphismPermGroup(R);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a FP Semigroup
gap> F := FreeSemigroup(2);
<free semigroup on the generators [ s1, s2 ]>
gap> S := F /
> [[F.1^3,F.1],
> [F.1^2*F.2,F.1*F.2],
> [F.1*F.2*F.1,F.2*F.1],
> [F.2*F.1^2,F.1*F.2^2],
> [F.2*F.1*F.2,F.1*F.2],
> [F.2^2*F.1,F.2*F.1],
> [F.2^3,F.2]];
<fp semigroup on the generators [ s1, s2 ]>
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,
gap> S := F /
> [[F.1^3,F.2^2],
> [F.1*F.2^2,F.1],
> [F.2^2*F.1,F.1],
> [F.2^3,F.2],
> [(F.1*F.2)^2*F.1,F.2*F.1^2*F.2],
> [(F.2*F.1)^2*F.2,F.1^2*F.2*F.1^2],
> [(F.1^2*F.2)^2,(F.2*F.1)^2],
> [(F.2*F.1^2)^2,(F.1*F.2)^2],
> [F.2*(F.1*F.2*F.1)^2,(F.1*F.2*F.1)^2*F.2]];
<fp semigroup on the generators [ s1, s2 ]>
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> G := Range(iso);
<group of size 24, with 2 generators>

#T# IsomorphismPermGroup: for a PBR Semigroup
gap> S := Semigroup([PBR([[-1],[-4],[-2],[-3]],[[1],[3],[4],[2]]),
>                    PBR([[-2],[-1],[-3],[-4]],[[2],[1],[3],[4]])]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> G := Range(iso);
<group of size 24, with 2 generators>
gap> S := Semigroup([PBR([[2], [2]], [[], []]),
>                    PBR([[], [-2]], [[], [-2, 2]])]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Bipartition Semigroup
gap> S := Semigroup([Bipartition([[1, 2, -2, -3], [3], [-1]]),
>                    Bipartition([[1, 2, -2], [3, -3], [-1]])]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,
gap> S := Semigroup([PBR([[-2], [-3], [-1]], [[3], [1], [2]])]);
<commutative pbr semigroup of degree 3 with 1 generator>
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> G := Range(iso);
<group of size 3, with 1 generator>

#T# IsomorphismPermGroup: for a Transformation Semigroup
gap> S := Semigroup([Transformation([3, 4, 1, 2, 6, 5]),
>                    Transformation([4, 5, 2, 6, 3, 1])]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> G := Range(iso);
Group([ (1,3)(2,4)(5,6), (1,4,6)(2,5,3) ])
gap> S := Semigroup([Transformation([2, 1, 2]), Transformation([3, 3, 1])]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument <S> must satisfy IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Partial Perm Semigroup
gap> S := Semigroup([PartialPerm([1, 2, 3, 4], [2, 1, 3, 4]),
>                    PartialPerm([1, 2, 3, 4], [1, 2, 4, 3])]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> G := Range(iso);
Group([ (1,2), (3,4) ])
gap> S := Semigroup([PartialPerm([1, 2], [2, 1]),
>                    PartialPerm([1, 3], [3, 1])]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument <S> must be a partial perm semigroup satisfying IsGroupAsSemigrou\
p,

#T# IsomorphismPermGroup: for a Boolean Mat Semigroup
gap> S := Semigroup([Matrix(IsBooleanMat, [[0, 1, 0], [0, 0, 1], [1, 0, 0]])]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> G := Range(iso);
<group of size 3, with 1 generator>
gap> S := Semigroup([Matrix(IsBooleanMat, [[1, 0, 1], [0, 1, 1], [0, 0, 0]]),
>                    Matrix(IsBooleanMat, [[1, 1, 1], [1, 0, 0], [0, 0, 0]])]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Max Plus Matrix Semigroup
gap> S := Semigroup([Matrix(IsMaxPlusMatrix, [[-infinity, 0, -infinity],
>                                             [-infinity, -infinity, 0],
>                                             [0, -infinity, -infinity]])]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> G := Range(iso);
<group of size 3, with 1 generator>
gap> S := Monoid([Matrix(IsMaxPlusMatrix,[[0,-infinity,-infinity,-infinity],
>                                         [-infinity,-infinity,0,-infinity],
>                                         [-infinity,-infinity,-infinity,0],
>                                         [-infinity,-infinity,-infinity,0]])]);
<commutative monoid of 4x4 max-plus matrices with 1 generator>
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Min Plus Matrix Semigroup
gap> S := 
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> G := Range(iso);

gap> S :=
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Tropical Max Plus Matrix Semigroup
gap> S := 
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> G := Range(iso);

gap> S :=
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Tropical Min Plus Matrix Semigroup
gap> S := 
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> G := Range(iso);

gap> S :=
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Projective Max Plus Matrix Semigroup
gap> S := 
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> G := Range(iso);

gap> S :=
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a NTP Matrix Semigroup
gap> S := 
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> G := Range(iso);

gap> S :=
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Block Bijection Semigroup
gap> S := 
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> G := Range(iso);

gap> S :=
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Integer Matrix Semigroup
gap> S := 
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> G := Range(iso);

gap> S :=
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(BruteForceoCheck);
gap> Unbind(G);
gap> Unbind(S);
gap> Unbind(iso);

#E#
gap> STOP_TEST("Semigroups package: standard/grpperm.tst");
