#############################################################################
##
#W  standard/elements/blocks.tst
#Y  Copyright (C) 2014-2022                               Attila Egri-Nagy
##                                                       James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local S, a, b, blocks, x, y
gap> START_TEST("Semigroups package: standard/elements/blocks.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# blocks: BLOCKS_NC 1/1
gap> x := BLOCKS_NC([[1], [2], [-3]]);
<blocks: [ 1* ], [ 2* ], [ 3 ]>

# blocks: PrintString 1/1
gap> x := BLOCKS_NC([[1, 6], [2], [3, 4], [-5]]);;
gap> PrintString(x);
"BLOCKS_NC([ [ 1, 6 ], [ 2 ], [ 3, 4 ], [ -5 ] ])"

# blocks: SEMIGROUPS.HashFunctionForBlocks 1/1
gap> Size(Monoid([Bipartition([[1, 2, -1], [-2]]),
>  Bipartition([[1], [2, -1, -2]])]));
8

# blocks: \< 1/1
gap> a := BLOCKS_NC([[1, 2], [3, 5], [4]]);;
gap> b := BLOCKS_NC([[1], [2, 3], [4], [-5]]);;
gap> a < b;
true
gap> b < a;
false

# blocks: \= 1/1
gap> a := BLOCKS_NC([[1, 2], [3, 5], [4]]);;
gap> b := BLOCKS_NC([[1], [2, 3], [4], [-5]]);;
gap> b = a;
false
gap> a = b;
false
gap> a = a;
true

# blocks: ProjectionFromBlocks 1/1
gap> x :=
> Bipartition([[1, 2, 3], [4, 7, 9], [5, 6, 10, -1], [8, -3, -5, -8],
>  [-2, -4], [-6, -7], [-9, -10]]);;
gap> y := LeftBlocks(x);
<blocks: [ 1, 2, 3 ], [ 4, 7, 9 ], [ 5*, 6*, 10* ], [ 8* ]>
gap> ProjectionFromBlocks(y);
<bipartition: [ 1, 2, 3 ], [ 4, 7, 9 ], [ 5, 6, 10, -5, -6, -10 ], [ 8, -8 ], 
 [ -1, -2, -3 ], [ -4, -7, -9 ]>
gap> last = LeftProjection(x);
true

# blocks: BLOCKS_RIGHT_ACT 1/1 
gap> NrRClasses(PartitionMonoid(3));
22

# blocks: BLOCKS_LEFT_ACT 1/3 
gap> S := Monoid([Bipartition([[1, 2, -1], [-2]]),
>  Bipartition([[1, -2], [2, -1]])]);;
gap> NrLClasses(S);
3

# blocks: BLOCKS_LEFT_ACT 2/3
gap> NrLClasses(PartitionMonoid(2));
6

# blocks: BLOCKS_E_TESTER 1/3
gap> NrIdempotents(PartitionMonoid(3));
114

# blocks: BLOCKS_E_TESTER 2/3
gap> x := BLOCKS_NC([[1, 4], [2, 3, 5]]);;
gap> y := BLOCKS_NC([[1, 2, 3], [4], [-5, -6]]);;
gap> BLOCKS_E_TESTER(x, y);
false

# blocks: BLOCKS_E_TESTER 3/3
gap> x := BLOCKS_NC([[1, 4], [2, 3, 5]]);;
gap> y := BLOCKS_NC([[1], [-2, -3, -4], [-5]]);;
gap> BLOCKS_E_TESTER(x, y);
false

# blocks: BLOCKS_E_CREATOR 1/3
gap> Set(Idempotents(PartitionMonoid(2)));
[ <block bijection: [ 1, 2, -1, -2 ]>, <bipartition: [ 1, 2, -1 ], [ -2 ]>, 
  <bipartition: [ 1, 2, -2 ], [ -1 ]>, <bipartition: [ 1, 2 ], [ -1, -2 ]>, 
  <bipartition: [ 1, 2 ], [ -1 ], [ -2 ]>, <bipartition: [ 1, -1, -2 ], [ 2 ]>
    , <block bijection: [ 1, -1 ], [ 2, -2 ]>, 
  <bipartition: [ 1, -1 ], [ 2 ], [ -2 ]>, <bipartition: [ 1 ], [ 2, -1, -2 ]>
    , <bipartition: [ 1 ], [ 2, -2 ], [ -1 ]>, 
  <bipartition: [ 1 ], [ 2 ], [ -1, -2 ]>, 
  <bipartition: [ 1 ], [ 2 ], [ -1 ], [ -2 ]> ]

# blocks: BLOCKS_RIGHT_ACT 1/1
gap> x := Bipartition([[1, 10], [2, -7, -9], [3, 4, 6, 8],
> [5, -5], [7, 9, -2], [-1, -10], [-3, -4, -6, -8]]);;
gap> blocks := BLOCKS_NC([[-1, -10], [2], [-3, -4, -6, -8], [5],
> [7, 9]]);;
gap> BLOCKS_RIGHT_ACT(blocks, x) = blocks;
true

# blocks: BLOCKS_LEFT_ACT 1/1
gap> x := Bipartition([[1, 10], [2, -7, -9], [3, 4, 6, 8],
> [5, -5], [7, 9, -2], [-1, -10], [-3, -4, -6, -8]]);;
gap> blocks := BLOCKS_NC([[-1, -10], [2], [-3, -4, -6, -8], [5],
> [7, 9]]);;
gap> BLOCKS_LEFT_ACT(blocks, Star(x)) = blocks;
true

# blocks: BLOCKS_INV_RIGHT, 1/1
gap> x := Bipartition([[1, 4, 7, 8, -4], [2, 3, 5, -2, -7],
> [6, -1], [-3], [-5, -6, -8]]);;
gap> blocks := BLOCKS_NC([[-1, -4, -5, -8], [-2, -3, -7], [6]]);;
gap> RankOfBlocks(blocks);
1
gap> RankOfBlocks(BLOCKS_RIGHT_ACT(blocks, x));
1
gap> BLOCKS_INV_RIGHT(blocks, x);
<bipartition: [ 1, -6 ], [ 2, 3, 4, 5, 6, 7, 8 ], [ -1, -4, -5, -8 ], 
 [ -2, -3, -7 ]>

# blocks: BLOCKS_INV_LEFT, 1/1
gap> x := Bipartition([[1, 4, 7, 8, -4], [2, 3, 5, -2, -7],
> [6, -1], [-3], [-5, -6, -8]]);;
gap> blocks := BLOCKS_NC([[-1, -2, -6], [3, 4, 5], [-7, -8]]);;
gap> RankOfBlocks(BLOCKS_LEFT_ACT(blocks, x));
1
gap> BLOCKS_INV_LEFT(blocks, x);
<bipartition: [ 1, 2, 6 ], [ 3, 4, 5, -1, -2, -3, -4, -5, -6, -7, -8 ], 
 [ 7, 8 ]>

# blocks: AsDigraph and CanonicalBlocks
gap> x := Bipartition([[1, 4, 7, 8, 12, -1, -5, -14],
> [2, 3, 5, 10, 15, -2, -4, -17], [6, 9, 17, 19, -9, -20],
> [11, 20, -6, -8, -16], [13, 14, 16, 18, -12, -18], [-3, -7, -11, -19],
> [-10, -13, -15]]);;
gap> blocks := LeftBlocks(x);;
gap> CanonicalBlocks(blocks);
<blocks: [ 1*, 2* ], [ 3*, 4*, 5*, 6* ], [ 7*, 8*, 9*, 10* ], [ 11*, 12*, 13*,\
 14*, 15* ], [ 16*, 17*, 18*, 19*, 20* ]>
gap> blocks := RightBlocks(x);;
gap> CanonicalBlocks(blocks);
<blocks: [ 1*, 2* ], [ 3*, 4* ], [ 5*, 6*, 7* ], [ 8*, 9*, 10* ], [ 11*, 12*, \
13* ], [ 14, 15, 16 ], [ 17, 18, 19, 20 ]>
gap> Print(last); "this string allows us to test the PrintObj method";
BLOCKS_NC([ [ 1, 2 ], [ 3, 4 ], [ 5, 6, 7 ], [ 8, 9, 10 ], [ 11, 12, 13 ], [ -\
14, -15, -16 ], [ -17, -18, -19, -20 ] ])"this string allows us to test the Pr\
intObj method"

# Test empty blocks
gap> BLOCKS_NC([]);
<empty blocks>

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/elements/blocks.tst");
