
gap> classes:=[[1,2,3, -2], [4, -5], [5, -7], [6, -3, -4], [7], [-1], [-6]];
[ [ 1, 2, 3, -2 ], [ 4, -5 ], [ 5, -7 ], [ 6, -3, -4 ], [ 7 ], [ -1 ], [ -6 ] 
 ]
gap> f:=BipartitionNC(classes);
<bipartition: [ 1, 2, 3, -2 ], [ 4, -5 ], [ 5, -7 ], [ 6, -3, -4 ], [ 7 ], 
[ -1 ], [ -6 ]>
gap> LeftProjection(f);
<bipartition: [ 1, 2, 3, -1, -2, -3 ], [ 4, -4 ], [ 5, -5 ], [ 6, -6 ], 
[ 7 ], [ -7 ]>
gap> f:=BipartitionNC([[1,2,-3,-5, -6], [3,-2,-4], [4,7], [5, -7, -8, -9], 
> [6], [8,9,-1]]);
<bipartition: [ 1, 2, -3, -5, -6 ], [ 3, -2, -4 ], [ 4, 7 ], [ 5, -7, -8, -9 
 ], [ 6 ], [ 8, 9, -1 ]>
gap> LeftProjection(f);
<bipartition: [ 1, 2, -1, -2 ], [ 3, -3 ], [ 4, 7 ], [ 5, -5 ], [ 6 ], 
[ 8, 9, -8, -9 ], [ -4 ], [ -6 ], [ -7 ]>
gap> f!.block;
Error, Record: '<rec>.block' must have an assigned value
not in any function at line 7 of *stdin*
you can 'return;' after assigning a value
brk> quit;    
gap> f!.blocks;
[ 1, 1, 2, 3, 4, 5, 3, 6, 6, 6, 2, 1, 2, 1, 1, 4, 4, 4 ]
gap> RereadPackage("semigroups/gap/bipartition.gi");
true
gap> f:=BipartitionNC([[1,2,-3,-5, -6], [3,-2,-4], [4,7], [5, -7, -8, -9],
> [6], [8,9,-1]]);
<bipartition: [ 1, 2, -3, -5, -6 ], [ 3, -2, -4 ], [ 4, 7 ], [ 5, -7, -8, -9 
 ], [ 6 ], [ 8, 9, -1 ]>
gap> LeftProjection(f);
<bipartition: [ 1, 2, -1, -2 ], [ 3, -3 ], [ 4, 7 ], [ 5, -5 ], [ 6 ], 
[ 8, 9, -8, -9 ], [ -4, -7 ], [ -6 ]>

