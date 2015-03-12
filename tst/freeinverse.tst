###########################################################################
##
##  freeinverse.tst 
#Y  Copyright (C) 2011-15                                   Julius Jonusas
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
gap> START_TEST("Semigroups package: freeinverse.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#T# FreeInverseTest1: Creating free inverse semigroups (with default 
# generators) and basic methods
gap> FreeInverseSemigroup(\<);
Error, Semigroups: FreeInverseSemigroup: usage,
FreeInverseSemigroup(<name1>,<name2>..) or FreeInverseSemigroup(<rank> [, name\
]),
gap> FreeInverseSemigroup([  ]);
Error, Semigroups: FreeInverseSemigroup: usage,
the number of generators of a free inverse semigroup must be non-zero,
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
gap> u := x^5 * y^3 * z;
x1*x1*x1*x1*x1*x2*x2*x2*x3
gap> u^-1;
x3^-1*x2^-1*x2^-1*x2^-1*x1^-1*x1^-1*x1^-1*x1^-1*x1^-1
gap> x^2 * y = x^2 * y;
true
gap> x * x^-1 = y * y^-1;
false

#T# FreeInverseTest2: Creating free inverse semigroups (with named
# generators) and basic methods
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
gap> u := x^5 * y^3 * z;
a*a*a*a*a*b*b*b*c
gap> u ^ -1;
c^-1*b^-1*b^-1*b^-1*a^-1*a^-1*a^-1*a^-1*a^-1
gap>  x^2 * y = x^2 * y;
true
gap> x * x^-1 = y * y^-1;
false

#T# FreeInverseTest3: IsFreeInverseSemigroup
gap> gens := Generators(FreeInverseSemigroup(2));
[ x1, x2 ]
gap> IsFreeInverseSemigroup(InverseSemigroup(gens));
true
gap> IsFreeInverseSemigroup(InverseSemigroup(gens{[1,2]}));
true
gap> IsFreeInverseSemigroup(SymmetricGroup(3));
Error, Semigroups: IsFreeInverseSemigroup:
can not determine the answer

#T# FreeInverseTest4: Iterator for free inverse semigroups
gap> iter := Iterator(FreeInverseSemigroup(["a", "b"]));
<iterator>
gap> for i in [1 .. 10] do
> NextIterator(iter);
> od;
gap> NextIterator(iter);
a*b
gap> IsDoneIterator(iter);
false

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(i);
gap> Unbind(gens);
gap> Unbind(iter);
gap> Unbind(S);
gap> Unbind(u);
gap> Unbind(y);
gap> Unbind(x);
gap> Unbind(z);

#E#
gap> STOP_TEST("Semigroups package: freeinverse.tst");
