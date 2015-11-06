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
gap> SemigroupsStartTest();

#T# SemiPPermTest1: NumberSubset
gap> sets := Combinations([1..10]);;
gap> Sort(sets, 
> function(x, y)
>    if Length(x) <> Length(y) then 
>      return Length(x) < Length(y);
>    fi;
>    return x < y;
>  end);
gap> List(sets, x -> NumberSubset(x, 10)) = [ 1 .. 2 ^ 10 ];
true

#T# SemiPPermTest2: Enumerator for a symmetric inverse monoid
gap> S := SymmetricInverseMonoid(3);;
gap> enum := Enumerator(S);
<enumerator of symmetric inverse monoid on 3 pts>
gap> ForAll([1..Length(enum)], x -> Position(enum, enum[x]) = x);
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
gap> ForAll([1..10], m -> List(Combinations([1 .. 10], m), x ->
> NumberSubsetOfEqualSize(x, 10)) = [1 .. Binomial(10, m)]);
true

#T# SemiPPermTest4: RepresentativeOfMinimalIdeal
gap> empty_map := PartialPerm([], []);;

### Semigroups containing the empty partial perm

# S = {empty_map}
gap> s := Semigroup(empty_map);
<trivial partial perm group of rank 0 with 1 generator>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# S = 0-simple semigroup of order 2
gap> s := Semigroup(empty_map, PartialPerm([1],[1]));
<partial perm monoid of rank 1 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# empty_map is a generator 
gap> s := Semigroup(PartialPerm([1, 2, 3], [1, 3, 4]), empty_map); 
<partial perm semigroup of rank 3 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Length(DomainOfPartialPermCollection) of size 1
gap> s := Semigroup(PartialPerm([2], [1]));
<commutative partial perm semigroup of rank 1 with 1 generator>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Length(DomainOfPartialPermCollection) of size 1
gap> s := Semigroup(PartialPerm([2], [2]), PartialPerm([2], [3]));
<partial perm semigroup of rank 1 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Length(DomainOfPartialPermCollection) of size 1
gap> s := Semigroup(PartialPerm([2], [4]), PartialPerm([2], [3]));
<partial perm semigroup of rank 1 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Length(ImageOfPartialPermCollection) of size 1
gap> s := Semigroup(PartialPerm([2], [2]), PartialPerm([3], [2]));
<partial perm semigroup of rank 2 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Length(ImageOfPartialPermCollection) of size 1
gap> s := Semigroup(PartialPerm([4], [2]), PartialPerm([3], [2]));
<partial perm semigroup of rank 2 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Construction of graph reveals that empty_map in S
gap> s := Semigroup(PartialPerm([2, 0, 0, 4, 0]),
> PartialPerm([3, 0, 0, 0, 5]));
<partial perm semigroup of rank 3 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Rank 1 generator is not idempotent
gap> s := Semigroup(PartialPerm([3],[2]), PartialPerm([2],[1]));
<partial perm semigroup of rank 2 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Rank 1 generator is not idempotent
gap> s := Semigroup(PartialPerm([2],[1]), PartialPerm([3],[2]));
<partial perm semigroup of rank 2 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Analysis of graph reveals that empty_map in S (but not construction)
gap> s := Semigroup(PartialPerm([3, 2, 0]), PartialPerm([2, 3, 0]));
<partial perm semigroup of rank 2 with 2 generators>
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
>   [1, 2, 3, 4, 5, 6, 7, 9, 12, 13, 17, 18, 19, 20 ],
>   [9, 2, 5, 12, 4, 11, 17, 8, 14, 13, 1, 18, 3, 16]),
> PartialPerm(
>   [1, 2, 3, 4, 5, 6, 8, 10, 11, 13, 14, 15, 20],
>   [14, 3, 12, 4, 18, 15, 5, 16, 8, 13, 10, 9, 20])
> );
<partial perm semigroup of rank 19 with 3 generators>
gap> RepresentativeOfMinimalIdeal(s);
<identity partial perm on [ 13 ]>
gap> last in s;
true
gap> empty_map in s;
false

# Trivial partial perm semigroup: GAP knows that it is simple at creation
gap> s := Semigroup(PartialPerm([2], [2]));
<trivial partial perm group of rank 1 with 1 generator>
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
<commutative partial perm semigroup of rank 2 with 1 generator>
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
gap> Unbind(s);
gap> Unbind(empty_map);

#E#
gap> STOP_TEST( "Semigroups package: semipperm.tst");
