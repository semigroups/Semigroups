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

#T# SemiPPerm1: NumberSubset
gap> sets := Combinations([1..10]);;
gap> Sort(sets, 
> function(x, y)
>    if Length(x) <> Length(y) then 
>      return Length(x) < Length(y);
>    fi;
>    return x < y;
>  end);
gap> List(sets, x-> NumberSubset(x, 10)) = [ 1 .. 2 ^ 10 ];
true

#T# SemiPPerm2: Enumerator for a symmetric inverse monoid
gap> S := SymmetricInverseMonoid(3);;
gap> enum := Enumerator(S);
<enumerator of symmetric inverse monoid on 3 pts>
gap> ForAll([1..Length(enum)], x-> Position(enum, enum[x]) = x);
true
gap> ForAll(enum, x-> enum[Position(enum, x)] = x);              
true
gap> Length(enum) = Size(S);
true
gap> ForAll(enum, x-> x in S);
true
gap> ForAll(S, x-> x in enum);
true

#T# SemiPPerm3: NumberSubsetOfEqualSize
gap> ForAll([1..10], m-> List(Combinations([1..10], m), x->
> NumberSubsetOfEqualSize(x, 10)) = [1.. Binomial(10, m)]);
true

#E#
gap> STOP_TEST( "Semigroups package: semipperm.tst");

