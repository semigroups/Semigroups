#############################################################################
##
#W  standard/fp/tietze.tst
#Y  Copyright (C) 2021-2022                               Tom Conti-Leslie
#Y                                                              Ben Spiers
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

#@local a, argument, b, bad, c, d, e, elm, f, f1, f2, g, infoRel, inv, l, len
#@local map, prevFpSemigroupInfoLevel, r, rels, s, s1, s2, s3, stz, stz1, stz2
#@local stz3, t, word
gap> START_TEST("Semigroups package: standard/fp/tietze.tst");
gap> LoadPackage("semigroups", false);;

# Whenever Stz Print methods are tested, the previous InfoLevel for
# InfoFpSemigroup is saved, set to whatever level is needed for the test, then
# set back. This is done in each chunk so that every chunk is independent.
gap> SEMIGROUPS.StartTest();

# Test StzPresentation, basic attributes, viewing methods
gap> f := FreeSemigroup(["a", "b", "c"]);;
gap> r := [[(f.1 ^ 2 * f.2) ^ 2, f.1 ^ 2 * f.2], [f.3, f.1 ^ 2 * f.2]];;
gap> s := f / r;;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 3 generators and 2 relations with length 16>
gap> IsStzPresentation(stz);
true
gap> IsStzPresentation(s);
false
gap> GeneratorsOfStzPresentation(stz);
[ "a", "b", "c" ]
gap> RelationsOfStzPresentation(stz);
[ [ [ 1, 1, 2, 1, 1, 2 ], [ 1, 1, 2 ] ], [ [ 3 ], [ 1, 1, 2 ] ] ]
gap> s = UnreducedFpSemigroup(stz);
true
gap> Length(stz);
16
gap> TietzeForwardMap(stz);
[ [ 1 ], [ 2 ], [ 3 ] ]
gap> TietzeBackwardMap(stz);
[ [ 1 ], [ 2 ], [ 3 ] ]

# Test StzPresentation viewing methods
gap> prevFpSemigroupInfoLevel := InfoLevel(InfoFpSemigroup);;
gap> SetInfoLevel(InfoFpSemigroup, 1);;
gap> f := FreeSemigroup(["a", "b", "c"]);;
gap> r := [[(f.1 ^ 2 * f.2) ^ 2, f.1 ^ 2 * f.2], [f.3, f.1 ^ 2 * f.2]];;
gap> s := f / r;;
gap> stz := StzPresentation(s);;
gap> StzPrintGenerators(stz, [1, 3, 1, 4, 5, s]);
#I  1.  a  8 occurrences
#I  3.  c  1 occurrences
#I  1.  a  8 occurrences
gap> StzPrintGenerators(stz);
#I  1.  a  8 occurrences
#I  2.  b  4 occurrences
#I  3.  c  1 occurrences
gap> StzPrintRelation(stz, 2);
#I  2. c = a^2*b
gap> StzPrintRelations(stz, [2, 2, 1, 3, "t"]);
#I  2. c = a^2*b
#I  2. c = a^2*b
#I  1. (a^2*b)^2 = a^2*b
gap> StzPrintRelations(stz);
#I  1. (a^2*b)^2 = a^2*b
#I  2. c = a^2*b
gap> StzPrintPresentation(stz);
#I  Current generators:
#I  1.  a  8 occurrences
#I  2.  b  4 occurrences
#I  3.  c  1 occurrences
#I  
#I  Current relations:
#I  1. (a^2*b)^2 = a^2*b
#I  2. c = a^2*b
#I  
#I  There are 3 generators and 2 relations of total length 16.
#I  
#I  Generators of original fp semigroup expressed as
#I  combinations of generators in current presentation:
#I  1. a = a
#I  2. b = b
#I  3. c = c
#I  
#I  Generators of current presentation expressed as
#I  combinations of generators of original fp semigroup:
#I  1. a = a
#I  2. b = b
#I  3. c = c
gap> SEMIGROUPS.StzRelationDisplayString(stz, 4);
fail
gap> SetInfoLevel(InfoFpSemigroup, prevFpSemigroupInfoLevel);;

# Multiple small Stz presentations, comparisons, display words in singular
gap> prevFpSemigroupInfoLevel := InfoLevel(InfoFpSemigroup);;
gap> SetInfoLevel(InfoFpSemigroup, 1);;
gap> f1 := FreeSemigroup("fred");;
gap> f2 := FreeSemigroup("foo", "bar");;
gap> s1 := f1 / [[f1.1 * f1.1, f1.1]];;
gap> s2 := f2 / [[f2.1 * f2.2, f2.2 * f2.1]];;
gap> s3 := f2 / [];;
gap> stz1 := StzPresentation(s1);
<fp semigroup presentation with 1 generator and 1 relation with length 4>
gap> stz2 := StzPresentation(s2);
<fp semigroup presentation with 2 generators and 1 relation with length 6>
gap> stz3 := StzPresentation(s3);
<fp semigroup presentation with 2 generators and 0 relations with length 2>
gap> stz1 < stz2;
true
gap> stz1 > stz2;
false
gap> StzPrintRelation(stz2, 1);
#I  1. foo*bar = bar*foo
gap> StzPrintRelations(stz3);
#I  There are no relations in the presentation <stz>
gap> SetGeneratorsOfStzPresentation(stz3, []);
gap> GeneratorsOfStzPresentation(stz3);
[  ]
gap> StzPrintGenerators(stz3);
#I  There are no generators in the presentation <stz>
gap> StzPrintPresentation(stz1);
#I  Current generators:
#I  1.  fred  3 occurrences
#I  
#I  Current relations:
#I  1. fred^2 = fred
#I  
#I  There is 1 generator and 1 relation of total length 4.
#I  
#I  Generators of original fp semigroup expressed as
#I  combinations of generators in current presentation:
#I  1. fred = fred
#I  
#I  Generators of current presentation expressed as
#I  combinations of generators of original fp semigroup:
#I  1. fred = fred
gap> SetInfoLevel(InfoFpSemigroup, prevFpSemigroupInfoLevel);;

# Test SetRelationsOfStzPresentation
gap> prevFpSemigroupInfoLevel := InfoLevel(InfoFpSemigroup);;
gap> SetInfoLevel(InfoFpSemigroup, 1);;
gap> f := FreeSemigroup("s1", "s2", "s3");;
gap> s := f / [[f.1 * f.2, f.3], [f.2 * f.3, f.1], [f.3 * f.1, f.2]];;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 3 generators and 3 relations with length 12>
gap> SetRelationsOfStzPresentation(stz, "yeet");
Error, parameter <arg> must be a list of pairs of words in LetterRep format
gap> SetRelationsOfStzPresentation(stz, [[1, "yeet"]]);
Error, parameter <arg> must be a list of pairs of words in LetterRep format
gap> SetRelationsOfStzPresentation(stz, [[1, 2, 3]]);
Error, parameter <arg> must be a list of pairs of words in LetterRep format
gap> SetRelationsOfStzPresentation(stz, [[[1], [2], [3]]]);
Error, parameter <arg> must be a list of pairs of words in LetterRep format
gap> SetRelationsOfStzPresentation(stz, [[["yeet"], [2]]]);
Error, parameter <arg> must be a list of pairs of words in LetterRep format
gap> SetRelationsOfStzPresentation(stz,
>    [[[1, 2], [3, 3, 2]], [[1, 3], [3, 1]]]);
gap> StzPrintRelations(stz);
#I  1. s1*s2 = s3^2*s2
#I  2. s1*s3 = s3*s1
gap> SetInfoLevel(InfoFpSemigroup, prevFpSemigroupInfoLevel);;

# Test SetTietzeForwardMap
gap> f := FreeSemigroup("s1", "s2", "s3");;
gap> s := f / [[f.1 * f.2, f.3], [f.2 * f.3, f.1], [f.3 * f.1, f.2]];;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 3 generators and 3 relations with length 12>
gap> TietzeForwardMap(stz);
[ [ 1 ], [ 2 ], [ 3 ] ]
gap> SetTietzeForwardMap(stz, [1, 2, 3]);
Error, the 2nd argument <newMaps> must be a list of lists of positive integers
gap> SetTietzeForwardMap(stz, [["yeet", 1]]);
Error, the 2nd argument <newMaps> must be a list of lists of positive integers
gap> SetTietzeForwardMap(stz, [[1], [2], [1, 1]]);
gap> TietzeForwardMap(stz);
[ [ 1 ], [ 2 ], [ 1, 1 ] ]

# Test SetTietzeBackwardMap
gap> f := FreeSemigroup("s1", "s2", "s3");;
gap> s := f / [[f.1 * f.2, f.3], [f.2 * f.3, f.1], [f.3 * f.1, f.2]];;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 3 generators and 3 relations with length 12>
gap> TietzeBackwardMap(stz);
[ [ 1 ], [ 2 ], [ 3 ] ]
gap> SetTietzeBackwardMap(stz, [1, 2, 3]);
Error, the 2nd argument <newMaps> must be a list of lists of positive integers
gap> SetTietzeBackwardMap(stz, [["yeet", 1]]);
Error, the 2nd argument <newMaps> must be a list of lists of positive integers
gap> SetTietzeBackwardMap(stz, [[1], [2], [3], [1, 2, 3]]);
gap> TietzeBackwardMap(stz);
[ [ 1 ], [ 2 ], [ 3 ], [ 1, 2, 3 ] ]

# Test TietzeForwardMapReplaceSubword
gap> prevFpSemigroupInfoLevel := InfoLevel(InfoFpSemigroup);;
gap> SetInfoLevel(InfoFpSemigroup, 1);;
gap> f := FreeSemigroup("f", "g", "h", "i");;
gap> s := f / [[(f.1 * f.2) ^ 2 * f.1 * f.3 * f.1 * f.2 * f.1, f.4]];;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 4 generators and 1 relation with length 14>
gap> TietzeForwardMap(stz);
[ [ 1 ], [ 2 ], [ 3 ], [ 4 ] ]
gap> StzRemoveGenerator(stz, "i");
gap> TietzeForwardMap(stz);
[ [ 1 ], [ 2 ], [ 3 ], [ 1, 2, 1, 2, 1, 3, 1, 2, 1 ] ]
gap> TietzeForwardMapReplaceSubword(stz, [1, 2, 1], [3, 3]);
gap> TietzeForwardMap(stz);
[ [ 1 ], [ 2 ], [ 3 ], [ 3, 3, 2, 1, 3, 3, 3 ] ]
gap> StzPrintPresentation(stz);
#I  Current generators:
#I  1.  f  0 occurrences
#I  2.  g  0 occurrences
#I  3.  h  0 occurrences
#I  
#I  Current relations:
#I  There are no relations in the presentation <stz>
#I  
#I  There are 3 generators and 0 relations of total length 3.
#I  
#I  Generators of original fp semigroup expressed as
#I  combinations of generators in current presentation:
#I  1. f = f
#I  2. g = g
#I  3. h = h
#I  4. i = h^2*g*f*h^3
#I  
#I  Generators of current presentation expressed as
#I  combinations of generators of original fp semigroup:
#I  1. f = f
#I  2. g = g
#I  3. h = h
gap> SetInfoLevel(InfoFpSemigroup, prevFpSemigroupInfoLevel);;

# Test StzAddRelation and StzAddRelationNC
# (and SEMIGROUPS.TietzeTransformation1)
# Undecidable example (justifies NC although it can still verify some rels)
gap> prevFpSemigroupInfoLevel := InfoLevel(InfoFpSemigroup);;
gap> SetInfoLevel(InfoFpSemigroup, 1);;
gap> f := FreeSemigroup("a", "b", "c", "d", "e");;
gap> g := GeneratorsOfSemigroup(f);;
gap> bad := "ac=ca, bc=cb, ce=eca, ad=da, bd=db, de=edb, cca=ccae";;
gap> rels := ParseRelations(g, bad);;
gap> s := f / rels;;
gap> a := s.1;;b := s.2;;c := s.3;;d := s.4;;e := s.5;;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 5 generators and 7 relations with length 38>
gap> StzAddRelation(stz, [[3, 5], [5, 1, 3]]);
gap> StzAddRelation(stz, [d * a * b, a * b * d]);
gap> StzAddRelationNC(stz, [[1, 5, 2, 1, 4], [1, 4, 5, 1]]);
gap> StzAddRelationNC(stz, [a * c * c * d * b, c * c * d * a * e]);
gap> StzPrintRelations(stz);
#I  1. a*c = c*a
#I  2. b*c = c*b
#I  3. c*e = e*c*a
#I  4. a*d = d*a
#I  5. b*d = d*b
#I  6. d*e = e*d*b
#I  7. c^2*a = c^2*a*e
#I  8. c*e = e*a*c
#I  9. d*a*b = a*b*d
#I  10. a*e*b*a*d = a*d*e*a
#I  11. a*c^2*d*b = c^2*d*a*e
gap> StzAddRelation(stz, [[1], [2], [3]]);
Error, StzAddRelation: second argument <pair> should be a list
of length 2
gap> StzAddRelation(stz, [[], [2]]);
Error, StzAddRelation: words in second argument <pair> should
be non-empty
gap> StzAddRelation(stz, [[1, 2, 3], [6]]);
Error, StzAddRelation: words in second argument <pair>
should be lists of pos ints no greater than the
number of generators of first argument <stz>
gap> StzAddRelation(stz, [a * b, b * c, c * d]);
Error, StzAddRelation: second argument <pair> should be a list
of length 2
gap> StzAddRelationNC(stz, [[1], [2], [3]]);
Error, StzAddRelationNC: second argument <pair> should be a list
of length 2
gap> StzAddRelationNC(stz, [[], [2]]);
Error, StzAddRelationNC: words in second argument <pair>
should be non-empty
gap> StzAddRelationNC(stz, [[1, 2, 3], [6]]);
Error, StzAddRelationNC: words in second argument <pair>
should be lists of pos ints no greater than the
number of generators of first argument <stz>
gap> StzAddRelationNC(stz, [a * b, b * c, c * d]);
Error, StzAddRelationNC: second argument <pair> should be a list
of length 2
gap> StzAddRelation(stz, [42, 42]);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `StzAddRelation' on 2 arguments
gap> StzAddRelationNC(stz, [42, 42]);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `StzAddRelationNC' on 2 arguments
gap> SetInfoLevel(InfoFpSemigroup, prevFpSemigroupInfoLevel);;

# Test StzAddRelation (with decidable problem, so that it rejects a
# non-redundant rel)
gap> f := FreeSemigroup("a", "b");;
gap> s := f / [[f.1 * f.2, f.2 * f.1]];;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 2 generators and 1 relation with length 6>
gap> StzAddRelation(stz, [[1, 1], [1]]);
Error, StzAddRelation: second argument <pair> must list two
words that are equal in the presentation <stz>
gap> StzAddRelation(stz, [s.1 * s.1, s.1]);
Error, StzAddRelation: second argument <pair> must list two
words that are equal in the presentation <stz>

# Test StzRemoveRelation and StzRemoveRelationNC
gap> prevFpSemigroupInfoLevel := InfoLevel(InfoFpSemigroup);;
gap> SetInfoLevel(InfoFpSemigroup, 1);;
gap> f := FreeSemigroup("x", "y");;
gap> r := ParseRelations([f.1, f.2],
>         "xx=x, yy=y, xyxy=xy, yxyx=yx, xyxyx=xyx");;
gap> s := f / r;;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 2 generators and 5 relations with length 28>
gap> StzRemoveRelation(stz, 5);
gap> StzRemoveRelation(stz, 4);
Error, StzRemoveRelation: second argument <index> must point to
a relation that is redundant in the presentation <stz>
gap> StzRemoveRelationNC(stz, 4);
gap> StzRemoveRelation(stz, 4);
Error, StzRemoveRelation: second argument <index> must be less
than or equal to the number of relations of the first
argument <stz>
gap> StzRemoveRelationNC(stz, 4);
Error, StzRemoveRelationNC: second argument <index> must be less
than or equal to the number of relations of the first
argument <stz>
gap> StzPrintPresentation(stz);
#I  Current generators:
#I  1.  x  6 occurrences
#I  2.  y  6 occurrences
#I  
#I  Current relations:
#I  1. x^2 = x
#I  2. y^2 = y
#I  3. (x*y)^2 = x*y
#I  
#I  There are 2 generators and 3 relations of total length 14.
#I  
#I  Generators of original fp semigroup expressed as
#I  combinations of generators in current presentation:
#I  1. x = x
#I  2. y = y
#I  
#I  Generators of current presentation expressed as
#I  combinations of generators of original fp semigroup:
#I  1. x = x
#I  2. y = y
gap> SetInfoLevel(InfoFpSemigroup, prevFpSemigroupInfoLevel);;

# Test StzAddGenerator (on 2 arguments, auto generator name creation)
gap> f := FreeSemigroup("a", "b");;
gap> s := f / [[(f.1 * f.1 * f.2) ^ 2, f.1 * f.1 * f.2]];;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 2 generators and 1 relation with length 11>
gap> StzAddGenerator(stz, ["what?", "how?"]);
Error, StzAddGenerator: second argument <word> is not a
list of pos ints at most equal to the number of
generators of the first argument <stz>
gap> StzAddGenerator(stz, [1, 3, 1]);
Error, StzAddGenerator: second argument <word> is not a
list of pos ints at most equal to the number of
generators of the first argument <stz>
gap> StzAddGenerator(stz, []);
Error, StzAddGenerator: cannot add generator equal to the empty
word
gap> StzAddGenerator(stz, [1, 1, 2]);
gap> TietzeBackwardMap(stz);
[ [ 1 ], [ 2 ], [ 1, 1, 2 ] ]
gap> elm := (f / []).1;;  # fp smgp elm but not in s
gap> StzAddGenerator(stz, elm);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `StzAddGenerator' on 2 arguments
gap> StzAddGenerator(stz, s.1 * s.2);
gap> TietzeBackwardMap(stz);
[ [ 1 ], [ 2 ], [ 1, 1, 2 ], [ 1, 2 ] ]
gap> GeneratorsOfStzPresentation(stz);
[ "a", "b", "c", "d" ]

# Test StzAddGenerator (on 3 arguments: specified new generator name)
gap> f := FreeSemigroup("a", "b");;
gap> s := f / [[(f.1 * f.1 * f.2) ^ 2, f.1 * f.1 * f.2]];;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 2 generators and 1 relation with length 11>
gap> StzAddGenerator(stz, ["what?", "how?"], "hehe");
Error, StzAddGenerator: second argument <word> is not a
list of pos ints at most equal to the number of
generators of the first argument <stz>
gap> StzAddGenerator(stz, [1, 3, 1], "dd");
Error, StzAddGenerator: second argument <word> is not a
list of pos ints at most equal to the number of
generators of the first argument <stz>
gap> StzAddGenerator(stz, [], "x");
Error, StzAddGenerator: cannot add generator equal to the empty
word
gap> StzAddGenerator(stz, [1, 1, 2], "a");
Error, StzAddGenerator: third argument <name> should not be the
name of a pre-existing generator
gap> StzAddGenerator(stz, [1, 1, 2], "newgenname");
gap> TietzeBackwardMap(stz);
[ [ 1 ], [ 2 ], [ 1, 1, 2 ] ]
gap> elm := (f / []).1;;  # fp smgp elm but not in s
gap> StzAddGenerator(stz, elm, "somethingweird");
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `StzAddGenerator' on 3 arguments
gap> StzAddGenerator(stz, s.1 * s.2, "ff");
gap> TietzeBackwardMap(stz);
[ [ 1 ], [ 2 ], [ 1, 1, 2 ], [ 1, 2 ] ]
gap> GeneratorsOfStzPresentation(stz);
[ "a", "b", "newgenname", "ff" ]

# Test StzRemoveGenerator on 2 arguments (index of relation unspecified)
gap> f := FreeSemigroup("x", "y", "z");;
gap> r := ParseRelations([f.1, f.2, f.3], "yx=xz, x=zy, y=z^3");;
gap> s := f / r;;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 3 generators and 3 relations with length 14>
gap> StzRemoveGenerator(stz, 4);
Error, StzRemoveGenerator: second argument <gen> must be no
greater than the total number of generators
gap> StzRemoveGenerator(stz, "a");
Error, StzRemoveGenerator: second argument <gen> does not
correspond to a generator name in first argument <stz>
gap> StzRemoveGenerator(stz, 1);
gap> GeneratorsOfStzPresentation(stz);
[ "y", "z" ]
gap> RelationsOfStzPresentation(stz);
[ [ [ 1, 2, 1 ], [ 2, 1, 2 ] ], [ [ 1 ], [ 2, 2, 2 ] ] ]
gap> TietzeForwardMap(stz);
[ [ 2, 1 ], [ 1 ], [ 2 ] ]
gap> TietzeBackwardMap(stz);
[ [ 2 ], [ 3 ] ]
gap> StzRemoveGenerator(stz, 2);
Error, StzRemoveGenerator: there is no relation in first
argument <stz> expressing second argument <gen> as a
product of other generators
gap> StzRemoveGenerator(stz, "y");
gap> GeneratorsOfStzPresentation(stz);
[ "z" ]
gap> RelationsOfStzPresentation(stz);
[ [ [ 1, 1, 1, 1, 1, 1, 1 ], [ 1, 1, 1, 1, 1 ] ] ]
gap> TietzeForwardMap(stz);
[ [ 1, 1, 1, 1 ], [ 1, 1, 1 ], [ 1 ] ]
gap> TietzeBackwardMap(stz);
[ [ 3 ] ]
gap> StzRemoveGenerator(stz, 2);
Error, StzRemoveGenerator: cannot remove only remaining
generator "z"
gap> StzRemoveGenerator(stz, "y");
Error, StzRemoveGenerator: second argument <gen> does not
correspond to a generator name in first argument <stz>
gap> StzRemoveGenerator(stz, 1);
Error, StzRemoveGenerator: cannot remove only remaining
generator "z"

# Test StzRemoveGenerator on 3 arguments (specified relation)
gap> f := FreeSemigroup(3);;
gap> s := f / [[f.1, f.2 ^ 2], [f.1, f.3 ^ 2], [f.2, f.3 ^ 2]];;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 3 generators and 3 relations with length 12>
gap> StzRemoveGenerator(stz, 4, 1);
Error, StzRemoveGenerator: second argument <gen> must be no
greater than the total number of generators
gap> StzRemoveGenerator(stz, 1, 4);
Error, StzRemoveGenerator: third argument <index> must be no
greater than the total number of relations in first
argument <stz>
gap> StzRemoveGenerator(stz, 1, 3);
Error, StzRemoveGenerator: third argument <index> does not point
to a relation expressing second argument <gen> as a
combination of other generators in first argument <stz>
gap> SEMIGROUPS.TietzeTransformation4(stz, 1, 3);
Error, TietzeTransformation4, internal function: third argument
<index> does not point to a relation expressing second
argument <gen> as a combination of other generators
gap> StzRemoveGenerator(stz, "a", 1);
Error, StzRemoveGenerator: second argument <gen> does not
correspond to a generator name in first argument <stz>
gap> StzRemoveGenerator(stz, 1, 2);
gap> GeneratorsOfStzPresentation(stz);
[ "s2", "s3" ]
gap> RelationsOfStzPresentation(stz);
[ [ [ 2, 2 ], [ 1, 1 ] ], [ [ 1 ], [ 2, 2 ] ] ]
gap> TietzeForwardMap(stz);
[ [ 2, 2 ], [ 1 ], [ 2 ] ]
gap> TietzeBackwardMap(stz);
[ [ 2 ], [ 3 ] ]
gap> StzRemoveGenerator(stz, "s2", 2);
gap> GeneratorsOfStzPresentation(stz);
[ "s3" ]
gap> RelationsOfStzPresentation(stz);
[ [ [ 1, 1 ], [ 1, 1, 1, 1 ] ] ]
gap> TietzeForwardMap(stz);
[ [ 1, 1 ], [ 1, 1 ], [ 1 ] ]
gap> TietzeBackwardMap(stz);
[ [ 3 ] ]
gap> StzRemoveGenerator(stz, 1, 1);
Error, StzRemoveGenerator: cannot remove only remaining
generator "s3"

# Test all four Tietze transformations on trivial example
gap> f := FreeSemigroup("x");;
gap> s := f / [];;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 1 generator and 0 relations with length 1>
gap> StzAddRelation(stz, [[1, 1], [1]]);
Error, StzAddRelation: second argument <pair> must list two
words that are equal in the presentation <stz>
gap> StzAddRelation(stz, [[1, 1], [1, 1]]);
gap> RelationsOfStzPresentation(stz);
[ [ [ 1, 1 ], [ 1, 1 ] ] ]
gap> StzAddGenerator(stz, s.1 ^ 2, "y");
gap> RelationsOfStzPresentation(stz);
[ [ [ 1, 1 ], [ 1, 1 ] ], [ [ 1, 1 ], [ 2 ] ] ]
gap> GeneratorsOfStzPresentation(stz);
[ "x", "y" ]
gap> StzRemoveRelation(stz, 1);
gap> RelationsOfStzPresentation(stz);
[ [ [ 1, 1 ], [ 2 ] ] ]
gap> StzRemoveGenerator(stz, "y");
gap> RelationsOfStzPresentation(stz);
[  ]

# Test StzSubstituteRelation
gap> f := FreeSemigroup("x", "y", "z");;
gap> r := ParseRelations([f.1, f.2, f.3], "xyxyx=zxyxz, xyx=x^2");;
gap> s := f / r;;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 3 generators and 2 relations with length 18>
gap> RelationsOfStzPresentation(stz);
[ [ [ 1, 2, 1, 2, 1 ], [ 3, 1, 2, 1, 3 ] ], [ [ 1, 2, 1 ], [ 1, 1 ] ] ]
gap> StzSubstituteRelation(stz, 3, 1);
Error, StzSubstituteRelation: second argument <index> must be no
greater than the number of relations in first argument
<stz>
gap> StzSubstituteRelation(stz, 2, 3);
Error, StzSubstituteRelation: third argument <side> must be
either 1 or 2
gap> StzSubstituteRelation(stz, 2, 1);
gap> SortedList(RelationsOfStzPresentation(stz));
[ [ [ 1, 1, 2, 1 ], [ 3, 1, 1, 3 ] ], [ [ 1, 2, 1 ], [ 1, 1 ] ] ]

# Test (internal) function SEMIGROUPS.NewGeneratorName
gap> SEMIGROUPS.NewGeneratorName([]);
"a"
gap> SEMIGROUPS.NewGeneratorName(["a"]);
"b"
gap> SEMIGROUPS.NewGeneratorName(["g"]);
"a"
gap> SEMIGROUPS.NewGeneratorName(["A"]);
"B"
gap> SEMIGROUPS.NewGeneratorName(["R"]);
"A"
gap> SEMIGROUPS.NewGeneratorName(["yeet"]);
"a"
gap> SEMIGROUPS.NewGeneratorName(["?"]);
"a"
gap> SEMIGROUPS.NewGeneratorName(["a", "c", "d", "A", "B", "m4", "m1"]);
"b"
gap> SEMIGROUPS.NewGeneratorName(["A", "B", "C", "m1"]);
"D"
gap> SEMIGROUPS.NewGeneratorName(["a", "c", "d", "A", "B", "D", "X"]);
"C"
gap> SEMIGROUPS.NewGeneratorName(["s1", "s3", "m4", "m6", "m2"]);
"m7"
gap> SEMIGROUPS.NewGeneratorName(["gar", "bage"]);
"s1"

# Test StzSimplifyPresentation
# We do not require any specific steps to be carried, so suppress output
# (setting InfoLevel low enough) and only require that the final reduction
# is below a certain bar, and cannot be reduced further.
gap> prevFpSemigroupInfoLevel := InfoLevel(InfoFpSemigroup);;
gap> SetInfoLevel(InfoFpSemigroup, 1);;
gap> f := FreeSemigroup("a", "b", "c", "d");;
gap> r := ParseRelations([f.1, f.2, f.3, f.4],
>         "a^5=a,a^25=cb,(cb)^3=c,a=a^5,cb=cb,(cb)^12=b^12,d=a^10");;
gap> s := f / r;;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 4 generators and 7 relations with length 101>
gap> StzSimplifyPresentation(stz);
gap> l := Length(stz);;
gap> l < 30;  # first version of the algorithm was able to achieve this
true
gap> StzSimplifyOnce(stz);
false
gap> l = Length(stz);
true
gap> SetInfoLevel(InfoFpSemigroup, prevFpSemigroupInfoLevel);;

# Test StzSimplifyPresentation, 2
gap> prevFpSemigroupInfoLevel := InfoLevel(InfoFpSemigroup);;
gap> SetInfoLevel(InfoFpSemigroup, 1);;
gap> f := FreeSemigroup("f", "g");;
gap> s := f / [[f.1 * f.2, f.2 * f.1], [f.1, f.2]];;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 2 generators and 2 relations with length 8>
gap> StzSimplifyPresentation(stz);
gap> GeneratorsOfStzPresentation(stz);
[ "g" ]
gap> RelationsOfStzPresentation(stz);
[  ]
gap> SetInfoLevel(InfoFpSemigroup, prevFpSemigroupInfoLevel);;

# Test SEMIGROUPS.StzFrequentSubwordApply
gap> prevFpSemigroupInfoLevel := InfoLevel(InfoFpSemigroup);;
gap> SetInfoLevel(InfoFpSemigroup, 1);;
gap> f := FreeSemigroup("a", "b", "c");;
gap> r := ParseRelations([f.1, f.2, f.3], "abbabb=cabb, ababb=c");;
gap> s := f / r;;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 3 generators and 2 relations with length 19>
gap> SEMIGROUPS.StzFrequentSubwordApply(stz, rec(word := [1, 2, 2]));
gap> SortedList(RelationsOfStzPresentation(stz));
[ [ [ 1, 2, 2 ], [ 4 ] ], [ [ 1, 2, 4 ], [ 3 ] ], [ [ 4, 4 ], [ 3, 4 ] ] ]
gap> SetInfoLevel(InfoFpSemigroup, prevFpSemigroupInfoLevel);;

# Test SEMIGROUPS.StzTrivialRelationApply
#  and SEMIGROUPS.StzDuplicateRelsApply
#  and SEMIGROUPS.StzGensRedundantApply
gap> prevFpSemigroupInfoLevel := InfoLevel(InfoFpSemigroup);;
gap> SetInfoLevel(InfoFpSemigroup, 1);;
gap> f := FreeSemigroup("x", "y", "z");;
gap> r := ParseRelations([f.1, f.2, f.3], "xy=yx, yx=xy, x=x, y=xz");;
gap> s := f / r;;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 3 generators and 4 relations with length 16>
gap> SEMIGROUPS.StzTrivialRelationApply(stz, rec(argument := 3));
gap> SortedList(RelationsOfStzPresentation(stz));
[ [ [ 1, 2 ], [ 2, 1 ] ], [ [ 2 ], [ 1, 3 ] ], [ [ 2, 1 ], [ 1, 2 ] ] ]
gap> SEMIGROUPS.StzDuplicateRelsApply(stz, rec(argument := 2));
gap> SortedList(RelationsOfStzPresentation(stz));
[ [ [ 1, 2 ], [ 2, 1 ] ], [ [ 2 ], [ 1, 3 ] ] ]
gap> SEMIGROUPS.StzGensRedundantApply(stz, rec(argument := 2, infoRel := 2));
gap> SortedList(RelationsOfStzPresentation(stz));
[ [ [ 1, 1, 2 ], [ 1, 2, 1 ] ] ]
gap> SetInfoLevel(InfoFpSemigroup, prevFpSemigroupInfoLevel);;

# Test SEMIGROUPS.StzRelsSubApply
gap> prevFpSemigroupInfoLevel := InfoLevel(InfoFpSemigroup);;
gap> SetInfoLevel(InfoFpSemigroup, 1);;
gap> f := FreeSemigroup("a", "b", "c", "d", "e");;
gap> r := ParseRelations(GeneratorsOfSemigroup(f),
>         "abc=ca, acb=d, e=ca, ed=abc, ee=e");;
gap> s := f / r;;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 5 generators and 5 relations with length 25>
gap> SEMIGROUPS.StzRelsSubApply(stz, rec(argument := 4));
gap> SortedList(RelationsOfStzPresentation(stz)) =
>    [[[1, 3, 2], [4]],
>     [[5], [3, 1]],
>     [[5, 4], [1, 2, 3]],
>     [[5, 4], [3, 1]],
>     [[5, 5], [5]]];
true
gap> SetInfoLevel(InfoFpSemigroup, prevFpSemigroupInfoLevel);;

# Test StzSimplifyOnce on 1 argument
gap> prevFpSemigroupInfoLevel := InfoLevel(InfoFpSemigroup);;
gap> SetInfoLevel(InfoFpSemigroup, 0);;
gap> f := FreeSemigroup("a", "b", "c");
<free semigroup on the generators [ a, b, c ]>
gap> s := f / [[f.1 ^ 4, f.1], [f.1, f.1 ^ 44], [f.1 ^ 8, f.2 * f.3]];
<fp semigroup with 3 generators and 3 relations of length 63>
gap> stz := StzPresentation(s);
<fp semigroup presentation with 3 generators and 3 relations with length 63>
gap> len := Length(stz);
63
gap> StzSimplifyOnce(stz);
true
gap> Length(stz) < len;
true
gap> s := f / [[f.1, f.1 ^ 2]];
<fp semigroup with 3 generators and 1 relation of length 6>
gap> stz := StzPresentation(s);
<fp semigroup presentation with 3 generators and 1 relation with length 6>
gap> len := Length(stz);
6
gap> StzSimplifyOnce(stz);
false
gap> len = Length(stz);
true
gap> s := f / [[f.1, f.2 ^ 5]];
<fp semigroup with 3 generators and 1 relation of length 9>
gap> stz := StzPresentation(s);
<fp semigroup presentation with 3 generators and 1 relation with length 9>
gap> len := Length(stz);
9
gap> StzSimplifyOnce(stz);
true
gap> Length(stz) < len;
true
gap> len := Length(stz);
2
gap> StzSimplifyOnce(stz);
false
gap> len = Length(stz);
true
gap> SetInfoLevel(InfoFpSemigroup, prevFpSemigroupInfoLevel);;

# Test StzIsomorphism
gap> prevFpSemigroupInfoLevel := InfoLevel(InfoFpSemigroup);;
gap> SetInfoLevel(InfoFpSemigroup, 1);;
gap> f := FreeSemigroup("a", "b", "c");;
gap> r := ParseRelations([f.1, f.2, f.3], "ab=c, ababa=aba, c^2=bc");;
gap> s := f / r;;
gap> stz := StzPresentation(s);
<fp semigroup presentation with 3 generators and 3 relations with length 18>
gap> StzPrintGenerators(stz);
#I  1.  a  6 occurrences
#I  2.  b  5 occurrences
#I  3.  c  4 occurrences
gap> StzPrintRelations(stz);
#I  1. a*b = c
#I  2. (a*b)^2*a = a*b*a
#I  3. c^2 = b*c
gap> StzRemoveGenerator(stz, "c");
gap> StzPrintRelations(stz);
#I  1. (a*b)^2*a = a*b*a
#I  2. (a*b)^2 = b*a*b
gap> StzAddGenerator(stz, s.3 * s.1, "d");
gap> StzPrintRelations(stz);
#I  1. (a*b)^2*a = a*b*a
#I  2. (a*b)^2 = b*a*b
#I  3. a*b*a = d
gap> StzSubstituteRelation(stz, 3, 1);
gap> StzPrintRelations(stz);
#I  1. d*b*a = d
#I  2. d*b = b*a*b
#I  3. a*b*a = d
gap> map := StzIsomorphism(stz);;
gap> s = Source(map);
true
gap> t := Range(map);;
gap> s.1 ^ map = t.1;
true
gap> s.2 ^ map = t.2;
true
gap> s.3 ^ map = t.1 * t.2;
true
gap> RespectsMultiplication(map);
true
gap> (s.1 * s.2) ^ map = s.3 ^ map;
true
gap> (s.1 * s.2 * s.1 * s.2 * s.1) ^ map = (s.1 * s.2 * s.1) ^ map;
true
gap> (s.3 ^ 2) ^ map = (s.2 * s.3) ^ map;
true
gap> inv := InverseGeneralMapping(map);;
gap> t.1 ^ inv = s.1;
true
gap> t.2 ^ inv = s.2;
true
gap> t.3 ^ inv = s.1 * s.2 * s.1;
true
gap> RespectsMultiplication(inv);
true
gap> GeneratorsOfSemigroup(t);
[ a, b, d ]
gap> SetInfoLevel(InfoFpSemigroup, prevFpSemigroupInfoLevel);;

# Test SimplifiedFpSemigroup
gap> prevFpSemigroupInfoLevel := InfoLevel(InfoFpSemigroup);;
gap> SetInfoLevel(InfoFpSemigroup, 1);;
gap> s := AsSemigroup(IsFpSemigroup, FullTransformationSemigroup(3));;
gap> t := SimplifiedFpSemigroup(s);;
gap> s := AsSemigroup(IsTransformationSemigroup, s);;
gap> t := AsSemigroup(IsTransformationSemigroup, t);;
gap> IsomorphismSemigroups(s, t) <> fail;
true
gap> SetInfoLevel(InfoFpSemigroup, prevFpSemigroupInfoLevel);;

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/fp/tietze.tst");
