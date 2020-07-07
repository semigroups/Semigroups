#############################################################################
##
##  standard/word.tst
#Y  Copyright (C) 2020                                   Murray T. Whyte
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/word.tst");
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

# Test RandomWord
gap> Length(RandomWord(4, 4)) = 4;
true
gap> Length(RandomWord(3, 10)) = 3;
true
gap> RandomWord(0, 100) = [];
true
gap> Length(DuplicateFreeList(RandomWord(100, 20))) <= 20;
true
gap> RandomWord(0, 0);
[  ]

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

# SEMIGROUPS_UnbindVariables
gap> Unbind(A);
gap> Unbind(w);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/word.tst");
