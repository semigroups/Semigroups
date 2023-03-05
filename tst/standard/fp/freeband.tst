#############################################################################
##
##  standard/fp/freeband.tst
#Y  Copyright (C) 2013-2022                                 Julius Jonusas
##                                                          Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local D, S, T, gens, ht, i, it, iter, j, list, new, next, result, s, words, x
#@local y, z
gap> START_TEST("Semigroups package: standard/fp/freeband.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# FreeBandTest1: Creating free bands and basic methods
# (with default generators)
gap> FreeBand(["a", "b", "c"]);
<free band on the generators [ a, b, c ]>
gap> FreeBand(3, "abc");
<free band on the generators [ abc1, abc2, abc3 ]>
gap> FreeBand("a", "b", "c");
<free band on the generators [ a, b, c ]>
gap> FreeBand(\<);
Error, FreeBand(<name1>,<name2>..) or FreeBand(<rank> [, name])
gap> S := FreeBand(3);
<free band on the generators [ x1, x2, x3 ]>
gap> Size(S);
159
gap> Size(Elements(S));
159
gap> S := FreeBand(4);
<free band on the generators [ x1, x2, x3, x4 ]>
gap> Size(S);
332380
gap> FreeBand(1, 2);
Error, FreeBand(<name1>,<name2>..) or FreeBand(<rank> [, name])
gap> FreeBand(1, 2, 3);
Error, FreeBand(<name1>,<name2>..) or FreeBand(<rank> [, name])

# FreeBandTest2: Free band D-class iterator
gap> S := FreeBand(5);
<free band on the generators [ x1, x2, x3, x4, x5 ]>
gap> x := S.3 * S.2 * S.1;
x3x2x1
gap> T := FreeBand(4, "t");;
gap> D := GreensDClassOfElement(S, T.3 * T.2);
Error, the 2nd argument (a free band element) does not belong to 1st argument \
(a free band category)
gap> D := GreensDClassOfElement(S, x);
<Green's D-class: x3x2x1>
gap> iter := Iterator(D);
<iterator>
gap> NextIterator(iter);
x3x2x3x1x3x2x3
gap> NextIterator(iter);
x2x3x1x3x2x3
gap> NextIterator(iter);
x3x2x1x3x2x3
gap> S := FreeBand(2);
<free band on the generators [ x1, x2 ]>
gap> D := GreensDClassOfElement(S, S.1 * S.2);;
gap> iter := Iterator(D);
<iterator>
gap> for x in iter do od;
gap> S := FreeBand(3);
<free band on the generators [ x1, x2, x3 ]>
gap> D := GreensDClassOfElement(S, S.1 * S.2 * S.3 * S.2 * S.1);
<Green's D-class: x1x2x3x2x1>
gap> it := Iterator(D);
<iterator>
gap> for x in it do od;
gap> it := ShallowCopy(it);
<iterator>
gap> for x in it do od;

# FreeBandTest3: Free band iterator
gap> S := FreeBand(10);
<free band on the generators [ x1, x2, x3, x4, x5, x6, x7, x8, x9, x10 ]>
gap> iter := Iterator(S);
<iterator>
gap> NextIterator(iter);
x1
gap> NextIterator(iter);
x2
gap> NextIterator(iter);
x3
gap> NextIterator(iter);
x4
gap> NextIterator(iter);
x5
gap> NextIterator(iter);
x6

# FreeBandTest4: Size
gap> Size(FreeBand(1));
1
gap> Size(FreeBand(2));
6
gap> Size(FreeBand(3));
159
gap> Size(FreeBand(4));
332380
gap> Size(FreeBand(7));
3641839910835401567626683593436003894250931310990279691

# FreeBandTest5: \< for a free band
gap> S := FreeBand(3);
<free band on the generators [ x1, x2, x3 ]>
gap> x := Generators(S)[1];
x1
gap> y := Generators(S)[2];
x2
gap> z := Generators(S)[3];
x3
gap> x < y;
true
gap> y < z;
true
gap> x * y < z;
true
gap> x * y < y;
false
gap> z < x * y * x * y * x;
false

# FreeBandTest6: Free band full iterator
gap> S := FreeBand(3);;
gap> list := [];;
gap> iter := Iterator(S);;
gap> next := NextIterator(iter);;
gap> while next <> fail do
> Add(list, next);
> next := NextIterator(iter);
> od;
gap> Size(list);
159
gap> IsDuplicateFree(list);
true

# FreeBandTest7: Equality of free bands
gap> S := FreeBand(3);
<free band on the generators [ x1, x2, x3 ]>
gap> x := Generators(S)[1];
x1
gap> y := Generators(S)[2];
x2
gap> z := Generators(S)[3];
x3
gap> x * y = z;
false
gap> x * y * y = z;
false
gap> x * x = x;
true
gap> x * y * x * y * z * x * y * z = x * y * z;
true

# FreeBandTest8: IsFreeBandCategory
gap> IsFreeBandCategory(FreeBand(4));
true
gap> IsFreeBandCategory(FreeBand(4, "b"));
true
gap> IsFreeBandCategory(SymmetricGroup(6));
false
gap> IsFreeBandCategory(FullTransformationMonoid(7));
false

# FreeBandTest9: \* for a free band
gap> S := FreeBand(7);;
gap> gens := Generators(S);;
gap> Product(List([1, 7], x -> gens[x]));
x1x7
gap> Product(List([1, 2, 3, 4, 3, 2, 1], x -> gens[x]));
x1x2x3x4x3x2x1
gap> Product(List([1, 5, 5, 6, 7, 2, 3, 2, 3], x -> gens[x]));
x1x5x6x7x2x3
gap> Product(List([1, 1, 1, 1, 2, 1, 1, 2], x -> gens[x]));
x1x2
gap> Product(List([5, 2, 4, 5, 1, 7, 7, 6, 2, 1], x -> gens[x]));
x5x2x4x2x4x5x1x2x4x5x1x7x2x4x5x1x7x6x4x5x1x7x6x2x5x1x7x6x2x7x6x2x1

# FreeBandTest10: Issue #112
gap> iter := Iterator(FreeBand(4, "b"));;
gap> x := NextIterator(iter);;
gap> for i in [1 .. 1000] do NextIterator(iter); od;
gap> y := NextIterator(iter);
b4b2b1b3b4b2b1b3b1b3b2b3
gap> T := Semigroup(x, y);;
gap> IsFreeBandSubsemigroup(T);
true
gap> Size(T);
5

# FreeBandTest11: Hash tables
gap> s := FreeBand(3);;
gap> x := Generators(s)[1];;
gap> y := Generators(s)[2];;
gap> iter := Iterator(s);;
gap> ht := HTCreate(x);
<tree hash table len=100003 used=0 colls=0 accs=0>
gap> for i in [1 .. 10] do
> HTAdd(ht, NextIterator(iter), true);
> od;
gap> new := NextIterator(iter);
x1x2x3
gap> HTValue(ht, new);
fail
gap> z := HTAdd(ht, new, true);;
gap> while not IsDoneIterator(iter) do
> HTAdd(ht, NextIterator(iter), true);
> od;
gap> ht!.len;
100003
gap> ht!.nr;
159
gap> ht!.collisions in [0, 1];  # for 32-bit and 64-bit mode
true
gap> ht!.accesses;
160

# FreeBandTest12: IsFreeBand
gap> gens := Generators(FreeBand(3));
[ x1, x2, x3 ]
gap> IsFreeBand(Semigroup(gens));
true
gap> IsFreeBand(Semigroup(gens{[1, 2]}));
true
gap> IsFreeBand(Semigroup(gens[1] * gens[2]));
true
gap> IsFreeBand(Semigroup([gens[1] * gens[2], gens[1] * gens[3]]));
true
gap> IsFreeBand(SymmetricGroup(3));
false
gap> IsFreeBand(Semigroup([gens[1] * gens[2], gens[1]]));
false

# Test ContentOfFreeBandElement
gap> S := FreeBand(3);;
gap> x := EvaluateWord(GeneratorsOfSemigroup(S), 
>                      [1, 1, 3, 2, 1, 2, 1, 1, 1, 1]);
x1x3x2x3x2x1
gap> ContentOfFreeBandElement(x);
[ 1, 2, 3 ]
gap> ContentOfFreeBandElement(S.2 * S.1);
[ 1, 2 ]
gap> ContentOfFreeBandElementCollection([S.2 * S.1, x]);
[ 1, 2, 3 ]

# Test ViewObj for large number of generators
gap> FreeBand(100);
<free band with 100 generators>

# Test EqualInFreeBand
gap> x := [1, 4, 2, 3, 10];
[ 1, 4, 2, 3, 10 ]
gap> y := [1, 4, 1, 4, 2, 3, 10];
[ 1, 4, 1, 4, 2, 3, 10 ]
gap> EqualInFreeBand(x, y);
true
gap> x := "aaaaaaaaaaaaaaaaaababaaaaaaabbbbb";
"aaaaaaaaaaaaaaaaaababaaaaaaabbbbb"
gap> y := "ab";
"ab"
gap> EqualInFreeBand(x, y);
true
gap> EqualInFreeBand("a", "b");
false
gap> EqualInFreeBand("", "");
true
gap> x := "abcbcccbabacab";
"abcbcccbabacab"
gap> y := "abcab";
"abcab"
gap> EqualInFreeBand(x, y);
true
gap> EqualInFreeBand("abac", "abad");
false
gap> it := Iterator(FreeSemigroup("a", "b"));;
gap> words := [];
[  ]
gap> for x in [1 .. 1000] do
> Add(words, SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(NextIterator(it))));
> od;
gap> result := [];;
> for i in [1 .. 1000] do
> j := PositionProperty([1 .. 1000], k -> EqualInFreeBand(words[i], words[k]));
> if j = i then
> Add(result, words[i]);
> fi;
> od;
> result;
[ [ 1 ], [ 2 ], [ 2, 1 ], [ 1, 2 ], [ 1, 2, 1 ], [ 2, 1, 2 ] ]
gap> EqualInFreeBand(["a"], [1]);
Error, expected int, found list (string)
gap> EqualInFreeBand([], [1]);
false
gap> EqualInFreeBand([], []);
true
gap> EqualInFreeBand([], ["a"]);
Error, expected int, found list (string)
gap> EqualInFreeBand([1], [2, 2, 2]);
false

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/fp/freeband.tst");
