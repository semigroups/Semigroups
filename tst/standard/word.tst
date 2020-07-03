#############################################################################
##
##  standard/freeband.tst
#Y  Copyright (C) 2020                                   Murray T. Whyte
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/freeband.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

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
gap> STOP_TEST("Semigroups package: standard/freeband.tst");
