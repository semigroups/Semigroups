#############################################################################
##
#W  maximal.tst
##  Test file for Maximal Subsemigroups related algorithms
##  Written by Wilf
##
#############################################################################
##
gap> START_TEST("Semigroups package: maximal.tst");
gap> LoadPackage("semigroups", false);;

# 
gap> SemigroupsStartTest();

# A 3x2 RMS over group C2xC2
gap> G:=Group([ (1,2), (3,4) ]);
Group([ (1,2), (3,4) ])
gap> mat:=[[ (), (1,2), (1,2)(3,4)], [(), (1,2), ()]];;
gap> R:=ReesMatrixSemigroup(G, mat);
<Rees matrix semigroup 3x2 over Group([ (1,2), (3,4) ])>
gap> max:=MaximalSubsemigroups(R);
[ <subsemigroup of 3x2 Rees matrix semigroup with 4 generators>, 
  <Rees matrix semigroup 3x1 over Group([ (1,2), (3,4) ])>, 
  <Rees matrix semigroup 3x1 over Group([ (1,2), (3,4) ])>, 
  <Rees matrix semigroup 2x2 over Group([ (1,2), (3,4) ])>, 
  <Rees matrix semigroup 2x2 over Group([ (1,2), (3,4) ])>, 
  <Rees matrix semigroup 2x2 over Group([ (1,2), (3,4) ])> ]
gap> Size(max);
6

# A 2x2 RMS over group C3, with only maximal subgroup trivial
gap> G:=Group([ (1,2,3) ]);
Group([ (1,2,3) ])
gap> mat:=[ [(), (1,2,3)], [(), (1,2,3)] ];;
gap> R:=ReesMatrixSemigroup(G,mat);
<Rees matrix semigroup 2x2 over Group([ (1,2,3) ])>
gap> max:=MaximalSubsemigroups(R);
[ <subsemigroup of 2x2 Rees matrix semigroup with 2 generators>, 
  <Rees matrix semigroup 2x1 over Group([ (1,2,3) ])>, 
  <Rees matrix semigroup 2x1 over Group([ (1,2,3) ])>, 
  <Rees matrix semigroup 1x2 over Group([ (1,2,3) ])>, 
  <Rees matrix semigroup 1x2 over Group([ (1,2,3) ])> ]
gap> MaximalSubsemigroups(R, Group([ () ]));
[ <subsemigroup of 2x2 Rees matrix semigroup with 3 generators> ]

# A 2x3 RMS over group S4 with maximal subgroup isomorphic to D8
gap> G:=Group([ (1,2,3,4), (1,2) ]);
Group([ (1,2,3,4), (1,2) ])
gap> mat:=[ [(1,2), ()], [(), (1,3)(2,4)], [(), ()] ];;
gap> R:=ReesMatrixSemigroup(G, mat);
<Rees matrix semigroup 2x3 over Group([ (1,2,3,4), (1,2) ])>
gap> H:=Group([ (1,2), (1,4,2,3) ]);
Group([ (1,2), (1,4,2,3) ])
gap> max:=MaximalSubsemigroups(R);;
gap> Size(max);
6
gap> MaximalSubsemigroups(R,H);
[ <subsemigroup of 2x3 Rees matrix semigroup with 5 generators> ]
gap> MaximalSubsemigroups(R, Group([ (1,2,3), (1,2) ]));
[  ]
gap> MaximalSubsemigroups(R, Group(()));
fail

# A connected 2x2 RZMS over group S3
gap> G:=Group([ (1,2), (1,2,3) ]);
Group([ (1,2), (1,2,3) ])
gap> mat:=[ [(1,2), 0], [0, (2,3)] ];
[ [ (1,2), 0 ], [ 0, (2,3) ] ]
gap> R:=ReesZeroMatrixSemigroup(G,mat);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2), (1,2,3) ])>
gap> max:=MaximalSubsemigroups(R);
[ <subsemigroup of 2x2 Rees 0-matrix semigroup with 6 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 6 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 6 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 6 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 6 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 6 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 6 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 6 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 6 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 6 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 6 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 9 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 9 generators> ]
gap> Size(max);
13

# A connected 5x5 RZMS over group S3, the first one we focused on
gap> G:=Group([ (1,4,2), (1,4,5) ]);
Group([ (1,4,2), (1,4,5) ])
gap> mat:=[
> [ 0, 0, (1,4,2), (1,4,5), 0 ],
> [ 0, (), (1,2,5), 0, 0 ],
> [ (), (1,2,5), 0, 0, 0 ],
> [ (), 0, 0, 0, (1,5,2) ],
> [ 0, 0, 0, (1,4,2), (1,4,5) ] ];;
gap> R:=ReesZeroMatrixSemigroup(G,mat);
<Rees 0-matrix semigroup 5x5 over Group([ (1,4,2), (1,4,5) ])>
gap> max:=MaximalSubsemigroups(R);
[ <subsemigroup of 5x5 Rees 0-matrix semigroup with 12 generators>, 
  <Rees 0-matrix semigroup 5x4 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 5x4 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 5x4 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 5x4 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 5x4 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 4x5 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 4x5 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 4x5 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 4x5 over Group([ (1,4,2), (1,4,5) ])>, 
  <Rees 0-matrix semigroup 4x5 over Group([ (1,4,2), (1,4,5) ])>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 25 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 25 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 25 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 25 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 25 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 25 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 25 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 25 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 25 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 25 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 25 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 25 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 25 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 25 generators>, 
  <subsemigroup of 5x5 Rees 0-matrix semigroup with 25 generators> ]
gap> Size(max);
26

# The above connected 5x5 RZMS over S3, with maximal subgroup <(1,2,5)>
gap> H:=Group([ (1,2,5) ]);
Group([ (1,2,5) ])
gap> MaximalSubsemigroups(R,H);
[ <subsemigroup of 5x5 Rees 0-matrix semigroup with 12 generators> ]
gap> MaximalSubsemigroups(R, Group(()));
fail

# An unconnected (3 components) 5x6 RZMS over group S4
gap> G:=Group([ (1,2,3,4), (1,3,2,4) ]);
Group([ (1,2,3,4), (1,3,2,4) ])
gap> mat:=[ [ (), (4,3), 0, 0, 0 ],
> [ (1,3)(2,4), 0, 0, 0, 0 ],
> [ 0, 0, (4,3), 0, 0 ],
> [ 0, 0, 0, (), 0 ],
> [ 0, 0, 0, (), () ],
> [ 0, 0, 0, (), 0 ] ];;
gap> R:=ReesZeroMatrixSemigroup(G,mat);
<Rees 0-matrix semigroup 5x6 over Group([ (1,2,3,4), (1,3,2,4) ])>
gap> max:=MaximalSubsemigroups(R);;
gap> Size(max);
116

# A principal factor of an inverse sgp; 13x13 completely unconnected over () gp
gap> mat:=[ [ (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
>   [ 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
>   [ 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
>   [ 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
>   [ 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0 ], 
>   [ 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0 ], 
>   [ 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0 ], 
>   [ 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0 ], 
>   [ 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0 ], 
>   [ 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0 ], 
>   [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0 ], 
>   [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0 ], 
>   [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, () ] ];;
gap> G:=Group(());;
gap> R:=ReesZeroMatrixSemigroup(G, mat);;
gap> max:=MaximalSubsemigroups(R);;
gap> Size(max);
8190

# Maximal subsemigroups of T3
gap> T3:=FullTransformationMonoid(3);
<full transformation semigroup on 3 pts>
gap> max:=MaximalSubsemigroups(T3);;
gap> correct:=[
>   Semigroup([
>     Transformation( [ 2, 3, 1 ] ),
>     Transformation( [ 3, 1, 1 ] ) ]),
>   Semigroup([
>     Transformation( [ 1, 3, 2 ] ),
>     Transformation( [ 3, 1, 1 ] ),
>     Transformation( [ 3, 3, 2 ] ),
>     Transformation( [ 1, 3, 1 ] ) ]),
>   Semigroup([ Transformation( [ 2, 1 ] ),
>     Transformation( [ 3, 3, 1 ] ),
>     Transformation( [ 1, 3, 3 ] ),
>     Transformation( [ 1, 2, 2 ] ) ]),
>   Semigroup([ Transformation( [ 3, 2, 1 ] ),
>     Transformation( [ 1, 2, 1 ] ),
>     Transformation( [ 3, 3, 1 ] ),
>     Transformation( [ 1, 1, 2 ] ) ]),
>   Semigroup([ Transformation( [ 2, 1 ] ),
>     Transformation( [ 2, 3, 1 ] ),
>     Transformation( [ 2, 2, 2 ] ) ])
> ];;
gap> max = correct;
true
gap> Size(max);
5

# Transformation semigroup
gap> S:=Semigroup([
>   Transformation( [ 2, 1, 5, 2, 4 ] ),
>   Transformation( [ 2, 3, 4, 3, 1 ] ),
>   Transformation( [ 3, 4, 1, 4, 3 ] ),
>   Transformation( [ 3, 4, 2, 2, 2 ] ),
>   Transformation( [ 5, 1, 1, 2, 3 ] ) ]);
<transformation semigroup on 5 pts with 5 generators>
gap> max:=MaximalSubsemigroups(S);;
gap> Size(max);
8
gap> IsDuplicateFreeList(max);
true
gap> ForAll(max,x->IsMaximalSubsemigroup(S,x));
true

# Random inverse semigroup of partial permutations
gap> S:=InverseSemigroup([
>   PartialPerm( [ 1, 3, 4, 5 ], [ 6, 5, 2, 4 ] ),
>   PartialPerm( [ 1, 2, 3, 4, 6 ], [ 5, 4, 3, 1, 6 ] ),
>   PartialPerm( [ 1, 2, 5, 7 ], [ 3, 1, 4, 6 ] ) ]);;
gap> max:=MaximalSubsemigroups(S);;
gap> Size(max);
6
gap> List(max, Size);
[ 715, 715, 714, 714, 713, 713 ]

# Random inverse semigroup of block bijections
gap> C:=InverseSemigroup( [ 
>   Bipartition( [ [ 1, -4 ], [ 2, -5 ], [ 3, 4, 5, 6, 7, -1, -2, -3, -6, -7 ] ] ), 
>   Bipartition( [ [ 1, -6 ], [ 2, -3 ], [ 3, 5, 6, 7, -1, -4, -5, -7 ], [ 4, -2 ] ] ), 
>   Bipartition( [ [ 1, -6 ], [ 2, -2 ], [ 3, 6, 7, -1, -5, -7 ], [ 4, -4 ], [ 5, -3 ] ] ), 
>   Bipartition( [ [ 1, -6 ], [ 2, -3 ], [ 3, -2 ], [ 4, 5, 7, -1, -5, -7 ], [ 6, -4 ] ] ) ] );
<inverse bipartition semigroup on 7 pts with 4 generators>
gap> max:=MaximalSubsemigroups(C);
[ <bipartition semigroup on 7 pts with 11 generators>, 
  <bipartition semigroup on 7 pts with 10 generators>, 
  <bipartition semigroup on 7 pts with 11 generators>, 
  <bipartition semigroup on 7 pts with 13 generators>, 
  <bipartition semigroup on 7 pts with 12 generators>, 
  <bipartition semigroup on 7 pts with 8 generators>, 
  <bipartition semigroup on 7 pts with 8 generators>, 
  <bipartition semigroup on 7 pts with 8 generators> ]
gap> Size(max);
8
gap> IsDuplicateFreeList(max);
true
gap> ForAll(max, x->IsMaximalSubsemigroup(C,x));
true

# Partition monoid of degree 3
gap> B:=PartitionMonoid(3);;
gap> max:=MaximalSubsemigroups(B);;
gap> Size(max);
8
gap> List(max, Size);
[ 200, 199, 199, 199, 167, 167, 167, 167 ]
gap> S:=max[1];;
gap> max:=MaximalSubsemigroups(S);;

# Random Bipartition example
gap> B:=Semigroup( [ Bipartition( [ [ 1, 2, -4 ], [ 3, 4, 5, 7 ], [ 6, -5, -7 ], [ -1, -3, -6 ], [ -2 ] ] ), 
>   Bipartition( [ [ 1, 5, 6, -2, -4, -6, -7 ], [ 2, 3, 4, -3, -5 ], [ 7, -1 ] ] ), 
>   Bipartition( [ [ 1, 6 ], [ 2, 3, 4 ], [ 5 ], [ 7, -6, -7 ], [ -1, -3, -4 ], [ -2 ], [ -5 ] ] ), 
>   Bipartition( [ [ 1, -4 ], [ 2, 3, 7 ], [ 4, 5, -7 ], [ 6, -1, -2, -3, -5 ], [ -6 ] ] ), 
>   Bipartition( [ [ 1, 7, -3 ], [ 2, 4, 5, -4 ], [ 3, 6 ], [ -1 ], [ -2 ], [ -5 ], [ -6 ], [ -7 ] ] ), 
>   Bipartition( [ [ 1, 5, -2, -4, -6, -7 ], [ 2, -1, -3, -5 ], [ 3, 4, 6, 7 ] ] ) ] );
<bipartition semigroup on 7 pts with 6 generators>
gap> max:=MaximalSubsemigroups(B);;                                    
gap> IsDuplicateFreeList(max);
true
gap> Size(max);
7
gap> ForAll(max,x->IsMaximalSubsemigroup(B,x));
true

# Test of IsMaximalSubsemigroup
gap> S:=Semigroup([
>   Transformation( [ 1, 2, 4, 4, 1 ] ),
>   Transformation( [ 4, 4, 1, 4 ] ),
>   Transformation( [ 5, 1, 4, 2, 3 ] ) ]);
<transformation semigroup on 5 pts with 3 generators>
gap> T:=Semigroup([
>   Transformation( [ 5, 1, 4, 2, 3 ] ),
>   Transformation( [ 4, 4, 2, 4, 1 ] ),
>   Transformation( [ 3, 1, 2, 2, 2 ] ) ]);
<transformation semigroup on 5 pts with 3 generators>
gap> IsMaximalSubsemigroup(S, T);
true
gap> U:=Semigroup([
>   Transformation( [ 5, 5, 1, 1, 5 ] ),
>   Transformation( [ 2, 2, 3, 4, 3 ] ),
>   Transformation( [ 3, 4, 5, 4, 3 ] ) ]);
<transformation semigroup on 5 pts with 3 generators>
gap> IsSubsemigroup(S, U);
true
gap> IsMaximalSubsemigroup(S, U);
false

# Maximal subsemigroups of a random regular transformation ideal
gap> S:=SingularTransformationSemigroup(5);
<regular transformation semigroup ideal on 5 pts with 1 generator>
gap> max:=MaximalSubsemigroups(S);;
gap> Size(max);
40
gap> S=max[1];
false

#
gap> SemigroupsStopTest();

#
gap> STOP_TEST("Semigroups package: maximal.tst", 10000);