#############################################################################
##
#W  semipperm.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: semipperm.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS_StartTest();

#T# SemiPPermTest1: NumberSubset
gap> sets := Combinations([1 .. 10]);;
gap> Sort(sets,
> function(x, y)
>    if Length(x) <> Length(y) then
>      return Length(x) < Length(y);
>    fi;
>    return x < y;
>  end);
gap> List(sets, x -> NumberSubset(x, 10)) = [1 .. 2 ^ 10];
true

#T# SemiPPermTest2: Enumerator for a symmetric inverse monoid
gap> S := SymmetricInverseMonoid(3);;
gap> enum := Enumerator(S);
<enumerator of symmetric inverse monoid on 3 pts>
gap> ForAll([1 .. Length(enum)], x -> Position(enum, enum[x]) = x);
true
gap> ForAll(enum, x -> enum[Position(enum, x)] = x);
true
gap> Length(enum) = Size(S);
true
gap> ForAll(enum, x -> x in S);
true
gap> ForAll(S, x -> x in enum);
true

#T# SemiPPermTest3: NumberSubsetOfEqualSize
gap> ForAll([1 .. 10], m -> List(Combinations([1 .. 10], m), x ->
> NumberSubsetOfEqualSize(x, 10)) = [1 .. Binomial(10, m)]);
true

#T# SmallerDegreeTest5: SmallerDegreePartialPermRepresentation Issue 2:
# Example where the degree being returned was greater than the original degree
gap> f1 := PartialPerm([2, 1, 0, 0, 4]);;
gap> f2 := PartialPerm([1, 2, 3, 5]);;
gap> f := InverseSemigroup(f1, f2);;
gap> F := SmallerDegreePartialPermRepresentation(f);;
gap> NrMovedPoints(f);
4
gap> NrMovedPoints(Image(F));
4
gap> Size(f);
15
gap> Size(Image(F));
15

#T# SmallerDegreeTest6: SmallerDegreePartialPermRepresentation:
# Example where using SupermumIdempotents helps to give a better result 
gap> f1 := PartialPermNC([2, 1, 4, 5, 3, 7, 6, 9, 10, 8]);;
gap> f2 := PartialPermNC([2, 1, 0, 0, 0, 7, 6]);;
gap> f := InverseSemigroup(f1, f2);;
gap> F := SmallerDegreePartialPermRepresentation(f);;
gap> NrMovedPoints(f);
10
gap> NrMovedPoints(Image(F));
5
gap> Size(f);
8
gap> Size(Image(F));
8

#T# SmallerDegreeTest7: SmallerDegreePartialPermRepresentation:
# Example where the degree is reduced but not the number of moved points
gap> f1 := PartialPermNC([1, 2, 3, 4, 5, 6, 10, 11, 15, 16, 17, 18],
> [7, 5, 11, 8, 4, 2, 20, 14, 12, 17, 9, 3]);;
gap> f2 := PartialPermNC([1, 2, 3, 6, 8, 10, 12, 15, 16, 17, 18, 19],
> [2, 4, 14, 3, 17, 7, 9, 16, 15, 10, 11, 1]);;
gap> f := InverseSemigroup(f1, f2);;
gap> F := SmallerDegreePartialPermRepresentation(f);;
gap> NrMovedPoints(f);
19
gap> NrMovedPoints(Image(F));
19
gap> ActionDegree(f);
20
gap> ActionDegree(Image(F));
19

#T# SmallerDegreeTest8: SmallerDegreePartialPermRepresentation:
# Example made complicated by right regular representation of Sym(5).
gap> S := SymmetricGroup(5);
Sym( [ 1 .. 5 ] )
gap> rho := ActionHomomorphism(S, S);
<action homomorphism>
gap> T := Image(rho);
<permutation group with 2 generators>
gap> H1 := [];
[  ]
gap> H2 := [];
[  ]
gap> for x in Elements(T) do
>   L := [];
>   for y in [1 .. 120] do
>     Add(L, y ^ x);
>   od;
>   g := PartialPerm(L);
>   Add(H2, g);
>   Add(L, 121);
>   Add(L, 122);
>   f := PartialPerm(L);
>   Add(H1, f);
> od;
gap> J := [1 .. 120];
[ 1 .. 120 ]
gap> Add(J, 122);
gap> Add(J, 121);
gap> h := PartialPerm(J);
<partial perm on 122 pts with degree 122, codegree 122>
gap> V := InverseSemigroup(H1, H2, h);
<inverse partial perm monoid of rank 122 with 240 generators>
gap> iso := SmallerDegreePartialPermRepresentation(V);;
gap> ActionDegree(Range(iso)) <= 12; # Genuine minimum degree of V is 7.
true

#T# SemiPPermTest4: RepresentativeOfMinimalIdeal
gap> empty_map := PartialPerm([], []);;

### Semigroups containing the empty partial perm

# S = {empty_map}
gap> s := Semigroup(empty_map);
<trivial partial perm group of rank 0 with 0 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# S = 0-simple semigroup of order 2
gap> s := Semigroup(empty_map, PartialPerm([1], [1]));
<commutative partial perm monoid of rank 1 with 1 generator>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# empty_map is a generator 
gap> s := Semigroup(PartialPerm([1, 2, 3], [1, 3, 4]), empty_map);
<partial perm semigroup on 3 pts with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Length(DomainOfPartialPermCollection) of size 1
gap> s := Semigroup(PartialPerm([2], [1]));
<commutative partial perm semigroup on 1 pts with 1 generator>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Length(DomainOfPartialPermCollection) of size 1
gap> s := Semigroup(PartialPerm([2], [2]), PartialPerm([2], [3]));
<partial perm semigroup on 1 pts with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Length(DomainOfPartialPermCollection) of size 1
gap> s := Semigroup(PartialPerm([2], [4]), PartialPerm([2], [3]));
<partial perm semigroup on 1 pts with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Length(ImageOfPartialPermCollection) of size 1
gap> s := Semigroup(PartialPerm([2], [2]), PartialPerm([3], [2]));
<partial perm semigroup on 2 pts with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Length(ImageOfPartialPermCollection) of size 1
gap> s := Semigroup(PartialPerm([4], [2]), PartialPerm([3], [2]));
<partial perm semigroup on 2 pts with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Construction of graph reveals that empty_map in S
gap> s := Semigroup(PartialPerm([2, 0, 0, 4, 0]),
> PartialPerm([3, 0, 0, 0, 5]));
<partial perm semigroup on 3 pts with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Rank 1 generator is not idempotent
gap> s := Semigroup(PartialPerm([3], [2]), PartialPerm([2], [1]));
<partial perm semigroup on 2 pts with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Rank 1 generator is not idempotent
gap> s := Semigroup(PartialPerm([2], [1]), PartialPerm([3], [2]));
<partial perm semigroup on 2 pts with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Analysis of graph reveals that empty_map in S (but not construction)
gap> s := Semigroup(PartialPerm([3, 2, 0]), PartialPerm([2, 3, 0]));
<partial perm semigroup on 2 pts with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

### Semigroups not containing the empty partial perm

# Semigroup with multiplicative zero = empty_map
gap> s := Semigroup(
> PartialPerm(
>   [1, 2, 3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 17],
>   [5, 7, 1, 3, 6, 9, 8, 15, 2, 18, 13, 20, 17, 4]),
> PartialPerm(
>   [1, 2, 3, 4, 5, 6, 7, 9, 12, 13, 17, 18, 19, 20],
>   [9, 2, 5, 12, 4, 11, 17, 8, 14, 13, 1, 18, 3, 16]),
> PartialPerm(
>   [1, 2, 3, 4, 5, 6, 8, 10, 11, 13, 14, 15, 20],
>   [14, 3, 12, 4, 18, 15, 5, 16, 8, 13, 10, 9, 20]));
<partial perm semigroup on 19 pts with 3 generators>
gap> RepresentativeOfMinimalIdeal(s);
<identity partial perm on [ 13 ]>
gap> last in s;
true
gap> empty_map in s;
false

# Trivial partial perm semigroup: GAP knows that it is simple at creation
gap> s := Semigroup(PartialPerm([2], [2]));
<trivial partial perm group of rank 1 with 0 generators>
gap> HasIsSimpleSemigroup(s);
true
gap> RepresentativeOfMinimalIdeal(s);
<identity partial perm on [ 2 ]>
gap> last in s;
true
gap> empty_map in s;
false

# Trivial partial perm semigroup: GAP does not know that it is simple
gap> s := Semigroup(PartialPerm([2], [2]), PartialPerm([2], [2]));
<commutative partial perm monoid of rank 1 with 1 generator>
gap> HasIsSimpleSemigroup(s);
false
gap> RepresentativeOfMinimalIdeal(s);
<identity partial perm on [ 2 ]>
gap> last in s;
true
gap> empty_map in s;
false

# Group as partial perm semigroup
gap> s := Semigroup(PartialPerm([2, 3], [3, 2]));
<commutative partial perm semigroup on 2 pts with 1 generator>
gap> HasIsGroupAsSemigroup(s);
false
gap> RepresentativeOfMinimalIdeal(s);
(2,3)
gap> HasIsGroupAsSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
true
gap> RepresentativeOfMinimalIdeal(s) in s;
true
gap> empty_map in s;
false

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(enum);
gap> Unbind(sets);
gap> Unbind(f1);
gap> Unbind(f2);
gap> Unbind(f3);
gap> Unbind(I5);
gap> Unbind(B);
gap> Unbind(F);
gap> Unbind(J);
gap> Unbind(L);
gap> Unbind(T);
gap> Unbind(rho);
gap> Unbind(V);
gap> Unbind(g);
gap> Unbind(f);
gap> Unbind(H2);
gap> Unbind(h);
gap> Unbind(H1);
gap> Unbind(iso);
gap> Unbind(y);
gap> Unbind(x);
gap> Unbind(s);
gap> Unbind(empty_map);

#E#
gap> STOP_TEST("Semigroups package: semipperm.tst");
