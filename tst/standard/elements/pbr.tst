#############################################################################
##
#W  partbinrel.tst
#Y  Copyright (C) 2015                                Attila Egri-Nagy
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: partbinrel.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS_StartTest();

#T# PartBinRel1: PartitionedBinaryRelation
gap> u := BinaryRelationOnPoints([[3], [2], [1], [1, 4]]);;
gap> pbr := PartitionedBinaryRelation(u);
a11: [ [  ], [ 2 ], [  ], [  ] ] a12: [ [ 3 ], [  ], [  ], [  ] ] a21: 
[ [  ], [  ], [ 1 ], [ 1 ] ] a22: [ [  ], [  ], [  ], [ 4 ] ] 

#E#
gap> STOP_TEST("Semigroups package: partbinrel.tst");
