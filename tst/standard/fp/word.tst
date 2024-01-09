#############################################################################
##
##  standard/fp/word.tst
#Y  Copyright (C) 2020-2022                                Murray T. Whyte
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local A, w
gap> START_TEST("Semigroups package: standard/fp/word.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Test WordToString
gap> WordToString("abc", [1, 1, 2, 1, 3]);
"aabac"
gap> WordToString("e3a", [3, 3, 3, 1]);
"aaae"
gap> WordToString("abc", []);
""
gap> WordToString("", []);
""
gap> WordToString("abc", [1]);
"a"
gap> WordToString("abc", [4]);
Error, the 1st argument (a string) is too short, expected at least 
4 but found 3
gap> WordToString("ab", [1, -1]);
Error, expected list of positive integers as 2nd argument

# Test RandomWord
gap> Length(RandomWord(4, 4)) = 4;
true
gap> Length(RandomWord(3, 10)) = 3;
true
gap> IsEmpty(RandomWord(0, 100));
true
gap> Length(DuplicateFreeList(RandomWord(100, 20))) <= 20;
true
gap> RandomWord(0, 0);
[  ]
gap> RandomWord(1, 0);
Error, the 1st argument (an integer) cannot be non-zero if the 2nd argument is\
 0
gap> RandomWord(-1, 2);
Error, expected non-negative integer as 1st argument
gap> RandomWord(2, -1);
Error, expected non-negative integer as 2nd argument

# Test StandardiseWord
gap> A := [3, 100, 2, 100, 3];
[ 3, 100, 2, 100, 3 ]
gap> StandardiseWord(A);
[ 1, 2, 3, 2, 1 ]
gap> A;
[ 1, 2, 3, 2, 1 ]
gap> A := [];
[  ]
gap> StandardiseWord(A);
[  ]
gap> StandardiseWord([10]);
[ 1 ]
gap> A := [1, 2, 3, 4, 5];
[ 1, 2, 3, 4, 5 ]
gap> StandardiseWord(A);
[ 1, 2, 3, 4, 5 ]
gap> A := [1, 1, 1, 1, 1, 1, 1, 1, 3];
[ 1, 1, 1, 1, 1, 1, 1, 1, 3 ]
gap> StandardizeWord(A);
[ 1, 1, 1, 1, 1, 1, 1, 1, 2 ]
gap> A := [2, 1, 2, 3, 2];
[ 2, 1, 2, 3, 2 ]
gap> StandardiseWord(A);
[ 1, 2, 1, 3, 1 ]
gap> StandardiseWord([0, 1, 2, 1]);
Error, expected a list of positive integers as 2nd argument
gap> StandardiseWord([[1, 2], [0]]);
Error, expected a list of positive integers as 2nd argument

# Test StringToWord
gap> w := "aabaacaad";
"aabaacaad"
gap> StringToWord(w);
[ 1, 1, 2, 1, 1, 3, 1, 1, 4 ]
gap> w;
"aabaacaad"
gap> w := "3a5bz!";
"3a5bz!"
gap> StringToWord(w);
[ 1, 2, 3, 4, 5, 6 ]
gap> w := "xyab77x";
"xyab77x"
gap> StringToWord(w);
[ 1, 2, 3, 4, 5, 5, 1 ]
gap> StringToWord("");
[  ]
gap> StringToWord("a");
[ 1 ]

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/fp/word.tst");
