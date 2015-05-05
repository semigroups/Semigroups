#############################################################################
##
#W  blocks.tst
#Y  Copyright (C) 2014-15                                 Attila Egri-Nagy
##                                                       James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: blocks.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

# blocks: BlocksNC 1/1
gap> x := BlocksNC([ [ 1 ], [ 2 ], [ -3 ] ]);
<blocks: [ 1 ], [ 2 ], [ -3 ]>

# blocks: PrintString 1/1
gap> x := BlocksNC([ [ 1, 6 ], [ 2 ], [ 3, 4 ], [ -5 ] ]);;
gap> PrintString(x);
"BlocksNC([ [ 1, 6 ], [ 2 ], [ 3, 4 ], [ -5 ] ])"

# blocks: SEMIGROUPS_HashFunctionForBlocks 1/1
gap> Size(Monoid( [ Bipartition( [ [ 1, 2, -1 ], [ -2 ] ] ),
>  Bipartition( [ [ 1 ], [ 2, -1, -2 ] ] ) ] ));
8

# blocks: EmptyBlocks 1/1
gap> EmptyBlocks;
<empty blocks>

# blocks: \< 1/1
gap> a := BlocksNC([ [ 1, 2 ], [ 3, 5 ], [ 4 ] ]);;
gap> b := BlocksNC([ [ 1 ], [ 2, 3 ], [ 4 ], [ -5 ] ]);;
gap> a < b;
true
gap> b < a;
false

# blocks: \= 1/1
gap> a := BlocksNC([ [ 1, 2 ], [ 3, 5 ], [ 4 ] ]);;
gap> b := BlocksNC([ [ 1 ], [ 2, 3 ], [ 4 ], [ -5 ] ]);;
gap> b = a;
false
gap> a = b;
false
gap> a = a;
true

# blocks: BlocksByIntRepNC 1/1
gap> x := BlocksNC([ [ 1, 2, 3, 8 ], [ -4, -9, -10 ], [ 5, 6 ], [ 7 ] ]);;
gap> BlocksByIntRepNC(x!.blocks);
<blocks: [ 1, 2, 3, 8 ], [ -4, -9, -10 ], [ 5, 6 ], [ 7 ]>

# blocks: ProjectionFromBlocks 1/1
gap> x := 
> Bipartition( [ [ 1, 2, 3 ], [ 4, 7, 9 ], [ 5, 6, 10, -1 ], [ 8, -3, -5, -8 ],
>  [ -2, -4 ], [ -6, -7 ], [ -9, -10 ] ] );;
gap> y := LeftBlocks(x);
<blocks: [ -1, -2, -3 ], [ -4, -7, -9 ], [ 5, 6, 10 ], [ 8 ]>
gap> ProjectionFromBlocks(y);
<bipartition: [ 1, 2, 3 ], [ 4, 7, 9 ], [ 5, 6, 10, -5, -6, -10 ], [ 8, -8 ], 
 [ -1, -2, -3 ], [ -4, -7, -9 ]>
gap> last = LeftProjection(x);
true

# blocks: OnRightBlocks 1/1 
gap> NrRClasses(PartitionMonoid(3));
22

# blocks: OnLeftBlocks 1/3 
gap> S := Monoid( [ Bipartition( [ [ 1, 2, -1 ], [ -2 ] ] ),
>  Bipartition( [ [ 1, -2 ], [ 2, -1 ] ] ) ] );;
gap> NrLClasses(S);
3

# blocks: OnLeftBlocks 2/3
gap> NrLClasses(PartitionMonoid(2));
6

# blocks: OnLeftBlocks 3/3
gap> OnLeftBlocks(EmptyBlocks, IdentityBipartition(2));
<blocks: [ 1 ], [ 2 ]>

# blocks: SEMIGROUPS_BlocksIdempotentTester 1/3
gap> NrIdempotents(PartitionMonoid(3));
114

# blocks: SEMIGROUPS_BlocksIdempotentTester 2/3
gap> x := BlocksNC([ [ 1, 4 ], [ 2, 3, 5 ] ]);;
gap> y := BlocksNC([ [ 1, 2, 3 ], [ 4 ], [ -5, -6 ] ]);;
gap> SEMIGROUPS_BlocksIdempotentTester(x, y);
Error, Semigroups: SEMIGROUPS_BlocksIdempotentTester: usage,
the degrees of the blocks <lambda> and <rho> must be equal,

# blocks: SEMIGROUPS_BlocksIdempotentTester 3/3
gap> x := BlocksNC([ [ 1, 4 ], [ 2, 3, 5 ] ]);;
gap> y := BlocksNC([ [ 1 ], [ -2, -3, -4 ], [ -5 ] ]);;
gap> SEMIGROUPS_BlocksIdempotentTester(x, y);
false

# blocks: SEMIGROUPS_BlocksIdempotentCreator 1/3
gap> Set(Idempotents(PartitionMonoid(2)));
[ <block bijection: [ 1, 2, -1, -2 ]>, <bipartition: [ 1, 2, -1 ], [ -2 ]>, 
  <bipartition: [ 1, 2, -2 ], [ -1 ]>, <bipartition: [ 1, 2 ], [ -1, -2 ]>, 
  <bipartition: [ 1, 2 ], [ -1 ], [ -2 ]>, <bipartition: [ 1, -1, -2 ], [ 2 ]>
    , <block bijection: [ 1, -1 ], [ 2, -2 ]>, 
  <bipartition: [ 1, -1 ], [ 2 ], [ -2 ]>, <bipartition: [ 1 ], [ 2, -1, -2 ]>
    , <bipartition: [ 1 ], [ 2, -2 ], [ -1 ]>, 
  <bipartition: [ 1 ], [ 2 ], [ -1, -2 ]>, 
  <bipartition: [ 1 ], [ 2 ], [ -1 ], [ -2 ]> ]

# blocks: PermRightBlocks 1/1
gap> x := Bipartition( [ [ 1, 10 ], [ 2, -7, -9 ], [ 3, 4, 6, 8 ], 
> [ 5, -5 ], [ 7, 9, -2 ], [ -1, -10 ], [ -3, -4, -6, -8 ] ] );;
gap> blocks := BlocksNC([[ -1, -10 ], [ 2 ], [ -3, -4, -6, -8 ], [ 5 ], 
> [ 7, 9 ]]);;
gap> OnRightBlocks(blocks, x) = blocks;
true
gap> PermRightBlocks(blocks, x);
(2,5)

# blocks: PermLeftBlocks 1/1
gap> x := Bipartition( [ [ 1, 10 ], [ 2, -7, -9 ], [ 3, 4, 6, 8 ], 
> [ 5, -5 ], [ 7, 9, -2 ], [ -1, -10 ], [ -3, -4, -6, -8 ] ] );;
gap> blocks := BlocksNC([[ -1, -10 ], [ 2 ], [ -3, -4, -6, -8 ], [ 5 ], 
> [ 7, 9 ]]);;
gap> OnLeftBlocks(blocks, Star(x)) = blocks;
true
gap> PermLeftBlocks(blocks, x);
(2,5)

# blocks: InverseRightBlocks 1/1
gap> x := Bipartition( [ [ 1, 4, 7, 8, -4 ], [ 2, 3, 5, -2, -7 ], 
> [ 6, -1 ], [ -3 ], [ -5, -6, -8 ] ] );;
gap> blocks := BlocksNC([[ -1, -4, -5, -8 ], [ -2, -3, -7 ], [ 6 ]]);;
gap> RankOfBlocks(blocks);
1
gap> RankOfBlocks(OnRightBlocks(blocks, x));
1
gap> InverseRightBlocks(blocks, x);
<bipartition: [ 1, -6 ], [ 2, 3, 4, 5, 6, 7, 8 ], [ -1, -4, -5, -8 ], 
 [ -2, -3, -7 ]>

# blocks: InverseLeftBlocks 1/1
gap> x := Bipartition( [ [ 1, 4, 7, 8, -4 ], [ 2, 3, 5, -2, -7 ], 
> [ 6, -1 ], [ -3 ], [ -5, -6, -8 ] ] );;
gap> blocks := BlocksNC([[ -1, -2, -6 ], [ 3, 4, 5 ], [ -7, -8 ]]);;
gap> RankOfBlocks(OnLeftBlocks(blocks, x));
1
gap> InverseLeftBlocks(blocks, x);
<bipartition: [ 1, 2, 6 ], [ 3, 4, 5, -1, -2, -3, -4, -5, -6, -7, -8 ], 
 [ 7, 8 ]>

#E# 
gap> STOP_TEST( "Semigroups package: blocks.tst");
