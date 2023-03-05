###########################################################################
##
##  standard/fp/freeinverse.tst 
#Y  Copyright (C) 2011-2022                                 Julius Jonusas
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local S, gens, i, iter, list1, u, w, x, y, z
gap> START_TEST("Semigroups package: standard/fp/freeinverse.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# FreeInverseTest1: Creating free inverse semigroups and basic methods
# (with default generators)
gap> FreeInverseSemigroup(\<);
Error, FreeInverseSemigroup(<name1>,<name2>..) or FreeInverseSemigroup(<rank> \
[, name]),
gap> FreeInverseSemigroup([]);
Error, the number of generators of a free inverse semigroup must be non-zero
gap> FreeInverseSemigroup(1, 2);
Error, FreeInverseSemigroup(<name1>,<name2>..) or FreeInverseSemigroup(<rank> \
[, name]),
gap> FreeInverseSemigroup(1, 2, 3);
Error, FreeInverseSemigroup(<name1>,<name2>..) or FreeInverseSemigroup(<rank> \
[, name]),
gap> FreeInverseSemigroup(20, "r");
<free inverse semigroup with 20 generators>
gap> S := FreeInverseSemigroup(3);
<free inverse semigroup on the generators [ x1, x2, x3 ]>
gap> Size(S);
infinity
gap> x := S.1;
x1
gap> y := S.2;
x2
gap> z := S.3;
x3
gap> u := x ^ 5 * y ^ 3 * z;
x1*x1*x1*x1*x1*x2*x2*x2*x3
gap> u ^ -1;
x3^-1*x2^-1*x2^-1*x2^-1*x1^-1*x1^-1*x1^-1*x1^-1*x1^-1
gap> x ^ 2 * y = x ^ 2 * y;
true
gap> x * x ^ -1 = y * y ^ -1;
false

# FreeInverseTest2: Creating free inverse semigroups and basic methods
# (with named generators)
gap> S := FreeInverseSemigroup("a", "b", "c");
<free inverse semigroup on the generators [ a, b, c ]>
gap> Size(S);
infinity
gap> x := S.1;
a
gap> y := S.2;
b
gap> z := S.3;
c
gap> u := x ^ 5 * y ^ 3 * z;
a*a*a*a*a*b*b*b*c
gap> u ^ -1;
c^-1*b^-1*b^-1*b^-1*a^-1*a^-1*a^-1*a^-1*a^-1
gap>  x ^ 2 * y = x ^ 2 * y;
true
gap> x * x ^ -1 = y * y ^ -1;
false

# FreeInverseTest3: IsFreeInverseSemigroup
gap> gens := Generators(FreeInverseSemigroup(2));
[ x1, x2 ]
gap> IsFreeInverseSemigroup(InverseSemigroup(gens));
true
gap> IsFreeInverseSemigroup(InverseSemigroup(gens{[1, 2]}));
true
gap> IsFreeInverseSemigroup(SymmetricGroup(3));
Error, cannot determine the answer
gap> IsFreeInverseSemigroup(MonogenicSemigroup(3, 2));
false

# FreeInverseTest4: Iterator, for a free inverse semigroup
gap> iter := Iterator(FreeInverseSemigroup(["a", "b"]));
<iterator>
gap> for i in [1 .. 10] do
> NextIterator(iter);
> od;
gap> NextIterator(iter);
b*a^-1
gap> IsDoneIterator(iter);
false

# Test Iterator, for a free inverse semigroup
gap> iter := Iterator(FreeInverseSemigroup(["a", "b"]));;
gap> for i in [1 .. 1000] do 
> NextIterator(iter);
> od;
gap> iter := Iterator(FreeInverseSemigroup(["a"]));;
gap> list1 := [];;
gap> for i in [1 .. 100] do 
> Add(list1, NextIterator(iter));
> od;

# FreeInverseTest5: CanonicalForm
gap> S := FreeInverseSemigroup(3);
<free inverse semigroup on the generators [ x1, x2, x3 ]>
gap> CanonicalForm(S.1);
"x1"
gap> CanonicalForm(S.1 * (S.1 ^ -1));
"x1x1^-1"
gap> CanonicalForm(S.1 * (S.1 ^ -1) * S.1 * (S.1 ^ -1));
"x1x1^-1"
gap> CanonicalForm((S.1 ^ -1) * S.1);
"x1^-1x1"
gap> CanonicalForm((S.1 ^ -1) * S.1 * (S.1 ^ -1) * S.1);
"x1^-1x1"

# Test ViewObj with different user preferences
gap> S := FreeInverseSemigroup(3);;
gap> w := S.1 * (S.2 ^ -1) * S.2 * (S.1 ^ -1);;
gap> CanonicalForm(w);
"x1x2^-1x2x1^-1"
gap> MinimalWord(w);
"x1*x1^-1*x1*x2^-1*x2*x1^-1"
gap> w;
x1*x1^-1*x1*x2^-1*x2*x1^-1
gap> SetUserPreference("semigroups", 
>                      "FreeInverseSemigroupElementDisplay",
>                      "bananas");
gap> w;
x1x2^-1x2x1^-1

# Test MinimalWord
gap> iter := Iterator(FreeInverseSemigroup(3));;
gap> x := NextIterator(iter);;
gap> for i in [1 .. 10] do
> y := x;
> x := NextIterator(iter);
> MinimalWord(x * y);
> od; 

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/fp/freeinverse.tst");
