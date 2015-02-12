############################################################################
##
#W  maximal.tst
#Y  Copyright (C) 2012-15                                  Wilfred Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## Tests for the maximal subsemigroups code
gap> START_TEST("Semigroups package: maximal.tst");
gap> LoadPackage("semigroups", false);;

#  
gap> SemigroupsStartTest();;

#T# IsMaximalSubsemigroup
gap> S := Semigroup([
>   Transformation( [ 1, 2, 4, 4, 1 ] ),
>   Transformation( [ 4, 4, 1, 4 ] ),
>   Transformation( [ 5, 1, 4, 2, 3 ] ) ]);
<transformation semigroup on 5 pts with 3 generators>
gap> T := Semigroup([
>   Transformation( [ 5, 1, 4, 2, 3 ] ),
>   Transformation( [ 4, 4, 2, 4, 1 ] ),
>   Transformation( [ 3, 1, 2, 2, 2 ] ) ]);
<transformation semigroup on 5 pts with 3 generators>
gap> IsMaximalSubsemigroup(S, T);
true
gap> U := Semigroup([
>   Transformation( [ 5, 5, 1, 1, 5 ] ),
>   Transformation( [ 2, 2, 3, 4, 3 ] ),
>   Transformation( [ 3, 4, 5, 4, 3 ] ) ]);
<transformation semigroup on 5 pts with 3 generators>
gap> IsSubsemigroup(S, U);
true
gap> IsMaximalSubsemigroup(S, U);
false
gap> IsSubsemigroup(U, S);
false
gap> IsMaximalSubsemigroup(U, S);
false
gap> IsSubsemigroup(S, S);
true
gap> S <> S;
false
gap> IsMaximalSubsemigroup(S, S);
false

#T# MaximalSubsemigroups: for a Rees matrix semigroup
gap> G := Group([ (1,2), (3,4) ]);
Group([ (1,2), (3,4) ])
gap> mat := [ [ (), (1,2), (1,2)(3,4) ], [ (), (1,2), ( )] ];;
gap> R := ReesMatrixSemigroup(G, mat); # 3x2 RMS over C2 x C2
<Rees matrix semigroup 3x2 over Group([ (1,2), (3,4) ])>
gap> max := MaximalSubsemigroups(R);
[ <subsemigroup of 3x2 Rees matrix semigroup with 4 generators>, 
  <Rees matrix semigroup 3x1 over Group([ (1,2), (3,4) ])>, 
  <Rees matrix semigroup 3x1 over Group([ (1,2), (3,4) ])>, 
  <Rees matrix semigroup 2x2 over Group([ (1,2), (3,4) ])>, 
  <Rees matrix semigroup 2x2 over Group([ (1,2), (3,4) ])>, 
  <Rees matrix semigroup 2x2 over Group([ (1,2), (3,4) ])> ]
gap> Size(max);
6
gap> G := Group([ (1,2,3) ]);
Group([ (1,2,3) ])
gap> mat := [ [ (), (1,2,3) ], [ (), (1,2,3) ] ];;
gap> R := ReesMatrixSemigroup(G, mat); # 2x2 RMS over C3
<Rees matrix semigroup 2x2 over Group([ (1,2,3) ])>
gap> max := MaximalSubsemigroups(R);
[ <subsemigroup of 2x2 Rees matrix semigroup with 2 generators>, 
  <Rees matrix semigroup 2x1 over Group([ (1,2,3) ])>, 
  <Rees matrix semigroup 2x1 over Group([ (1,2,3) ])>, 
  <Rees matrix semigroup 1x2 over Group([ (1,2,3) ])>, 
  <Rees matrix semigroup 1x2 over Group([ (1,2,3) ])> ]
gap> G := Group([ (1,2,3), (1,2) ]);
Group([ (1,2,3), (1,2) ])
gap> mat := [ [ (), (1,3,2) ], [ (1,3), (2,3) ], [ (1,2,3), () ] ];;
gap> R := ReesMatrixSemigroup(G, mat); # 2x3 RMS over Sym(3)
<Rees matrix semigroup 2x3 over Group([ (1,2,3), (1,2) ])>
gap> max := MaximalSubsemigroups(R);
[ <subsemigroup of 2x3 Rees matrix semigroup with 4 generators>, 
  <Rees matrix semigroup 2x2 over Group([ (1,2,3), (1,2) ])>, 
  <Rees matrix semigroup 2x2 over Group([ (1,2,3), (1,2) ])>, 
  <Rees matrix semigroup 2x2 over Group([ (1,2,3), (1,2) ])>, 
  <Rees matrix semigroup 1x3 over Group([ (1,2,3), (1,2) ])>, 
  <Rees matrix semigroup 1x3 over Group([ (1,2,3), (1,2) ])> ]
gap> S := max[1]; # a non-RMS subsemigroup of an RMS
<subsemigroup of 2x3 Rees matrix semigroup with 4 generators>
gap> IsReesMatrixSubsemigroup(S);
true
gap> IsReesMatrixSemigroup(S);
false
gap> IsRegularSemigroup(S);
true
gap> max := MaximalSubsemigroups(S);;
gap> IsDuplicateFreeList(max);
true
gap> Length(max);
5
gap> T := FullTransformationMonoid(3);
<full transformation semigroup on 3 pts>
gap> mat := [ [ Transformation([ 3, 2, 3 ]) ] ];;
gap> R := ReesMatrixSemigroup(T, mat); # 1x1 RMS over a non-group semigroup
<Rees matrix semigroup 1x1 over <full transformation semigroup on 3 pts>>
gap> IsReesMatrixSubsemigroup(R);
true
gap> IsReesMatrixSemigroup(R);
true
gap> IsGroup(T);
false
gap> IsSimpleSemigroup(T);
false
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 1x1 Rees matrix semigroup with 5 generators>, 
  <subsemigroup of 1x1 Rees matrix semigroup with 5 generators>, 
  <subsemigroup of 1x1 Rees matrix semigroup with 5 generators>, 
  <subsemigroup of 1x1 Rees matrix semigroup with 5 generators>, 
  <subsemigroup of 1x1 Rees matrix semigroup with 5 generators>, 
  <subsemigroup of 1x1 Rees matrix semigroup with 5 generators> ]
gap> S := Semigroup( [
> Transformation( [ 1, 2, 1 ] ),
> Transformation( [ 1, 2, 2 ] ) ]);
<transformation semigroup on 3 pts with 2 generators>
gap> mat := [ [ Transformation( [ 1, 2, 1 ] ) ] ];;
gap> R := ReesMatrixSemigroup(S, mat); # simple 1x1 RMS over non-group semigroup
<Rees matrix semigroup 1x1 over <transformation semigroup 
  on 3 pts with 2 generators>>
gap> IsReesMatrixSubsemigroup(R);
true
gap> IsReesMatrixSemigroup(R);
true
gap> IsGroup(S);
false
gap> IsSimpleSemigroup(S);
true
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 1x1 Rees matrix semigroup with 1 generator>, 
  <subsemigroup of 1x1 Rees matrix semigroup with 1 generator> ]

#T# MaximalSubsemigroups: for a Rees matrix semigroup and a maximal subgroup
gap> G := Group([ (1,2,3) ]);
Group([ (1,2,3) ])
gap> mat := [ [ (), (1,2,3) ], [ (), (1,2,3) ] ];;
gap> R := ReesMatrixSemigroup(G, mat); # 2x2 RMS over C3
<Rees matrix semigroup 2x2 over Group([ (1,2,3) ])>
gap> MaximalSubsemigroups(R, Group([ () ]));
[ <subsemigroup of 2x2 Rees matrix semigroup with 3 generators> ]
gap> S := Semigroup( RMSElement( R, 2, (1,3,2), 2 ) ); # a non-RMS subsemigroup
<subsemigroup of 2x2 Rees matrix semigroup with 1 generator>
gap> MaximalSubsemigroups(S, Group([ () ]));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `MaximalSubsemigroups' on 2 arguments
gap> G := Group([ (1,2,3,4), (1,2) ]);
Group([ (1,2,3,4), (1,2) ])
gap> mat := [ [ (1,2), () ], [ (), (1,3)(2,4) ], [ (), () ] ];;
gap> R := ReesMatrixSemigroup(G, mat); # 2x3 RMS over Sym(4); and subgroup D8
<Rees matrix semigroup 2x3 over Group([ (1,2,3,4), (1,2) ])>
gap> H := Group([ (1,2), (1,4,2,3) ]);
Group([ (1,2), (1,4,2,3) ])
gap> max := MaximalSubsemigroups(R);;
gap> Size(max);
6
gap> MaximalSubsemigroups(R, H);
[ <subsemigroup of 2x3 Rees matrix semigroup with 5 generators> ]
gap> last[1] in max;
true
gap> MaximalSubsemigroups(R, Group([ (1,2,3), (1,2) ])); # no results exist
[  ]
gap> MaximalSubsemigroups(R, Group([ (1,3,5) ])); # not a subgroup
Error, Semigroups: MaximalSubsemigroups: usage,
the second argument <H> must be a subgroup of the underlying
group of the Rees matrix semigroup in the first first argument, <R>,
gap> MaximalSubsemigroups(R, Group(())); # not a maximal subgroup
Error, Semigroups: MaximalSubsemigroups: usage,
the second argument <H> must be a maximal subgroup of the underlying
group of the Rees matrix semigroup in the first first argument, <R>,
gap> T := FullTransformationMonoid(2);
<full transformation semigroup on 2 pts>
gap> mat := [ [ Transformation( [ ] ) ] ];
[ [ IdentityTransformation ] ]
gap> R := ReesMatrixSemigroup(T, mat);
<Rees matrix semigroup 1x1 over <full transformation semigroup on 2 pts>>
gap> MaximalSubsemigroups(R, Group(())); # not a RMS over a group
Error, Semigroups: MaximalSubsemigroups: usage,
the first argument <R> must be a Rees matrix semigroup whose underlying
semigroup is a group,
gap> G := Group([ (1,2) ]);;
gap> mat := [ [ (1,2), (1,2), (1,2) ], [ (1,2), (1,2), (1,2) ] ];;
gap> R := ReesMatrixSemigroup(G, mat); # a 3x2 RMS over C2 with trivial subgroup
<Rees matrix semigroup 3x2 over Group([ (1,2) ])>
gap> MaximalSubsemigroups(R, Group(()));
[ <subsemigroup of 3x2 Rees matrix semigroup with 4 generators> ]
gap> IsMaximalSubsemigroup(R, last[1]);
true

#T# MaximalSubsemigroups: for a Rees 0-matrix semigroup
gap> R := ReesZeroMatrixSemigroup(Group(()), [ [ 0 ] ]); # a non-regular RZMS
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator> ]
gap> R1 := ReesZeroMatrixSemigroup(Group(()), [ [ () ] ]); # 2-elt regular RZMS
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> MaximalSubsemigroups(R1);
[ <subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator>, 
  <subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator> ]
gap> R2 := last[2]; # RZMS subsemigroup, but not itself a RZMS
<subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator>
gap> IsReesZeroMatrixSubsemigroup(R2);
true
gap> IsReesZeroMatrixSemigroup(R2);
false
gap> MaximalSubsemigroups(R2);
[  ]
gap> s1 := Transformation( [ 1, 2, 1 ] );;
gap> s2 := Transformation( [ 1, 2, 2 ] );;
gap> S := Semigroup([ s1, s2 ]);
<transformation semigroup on 3 pts with 2 generators>
gap> R := ReesZeroMatrixSemigroup( S, [ [ s1 ] ] ); # 0-simple RZMS over non-gp
<Rees 0-matrix semigroup 1x1 over <transformation semigroup 
  on 3 pts with 2 generators>>
gap> IsReesZeroMatrixSemigroup(R);
true
gap> SetIsRegularSemigroup(R, true); # temp hack till Issue #108 is resolved
gap> IsRegularSemigroup(R);
true
gap> G := UnderlyingSemigroup(R);
<transformation semigroup on 3 pts with 2 generators>
gap> IsGroup(G);
false
gap> IsZeroSimpleSemigroup(R);
true
gap> MaximalSubsemigroups(R);
Error, Semigroups: MaximalSubsemigroups,
not yet implemented for a 0-simple Rees 0-matrix semigroup whose
underlying semigroup is not a group,
gap> t1 := Transformation( [ 4, 3, 1, 3 ] );;
gap> t2 := Transformation( [ 3, 3, 2, 2 ] );;
gap> T := Semigroup([ t1, t2 ]);
<transformation semigroup on 4 pts with 2 generators>
gap> IsRegularSemigroup(T);
true
gap> IsGroup(T);
false
gap> mat := [ [ t2, t1 ], [ t1, t2 ] ];;
gap> R3 := ReesZeroMatrixSemigroup(T, mat); # a RZMS over a non-group semigroup
<Rees 0-matrix semigroup 2x2 over <regular transformation semigroup 
  on 4 pts with 2 generators>>
gap> IsReesZeroMatrixSubsemigroup(R3);
true
gap> IsReesZeroMatrixSemigroup(R3);
true
gap> SetIsRegularSemigroup(R3, true); # temp hack till Issue #108 is resolved
gap> IsRegularSemigroup(R3);
true
gap> max := MaximalSubsemigroups(R3);;
gap> Length(max);
10
gap> IsDuplicateFreeList(max);
true
gap> G := Group([ (1,2), (1,2,3) ]);;
gap> mat := [ [ (1,2), 0 ], [ 0, (2,3) ] ];;
gap> R := ReesZeroMatrixSemigroup(G, mat); # un-connected 2x2 inverse RZMS / S3
<Rees 0-matrix semigroup 2x2 over Group([ (1,2), (1,2,3) ])>
gap> max := MaximalSubsemigroups(R);
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
gap> IsDuplicateFreeList(max);
true
gap> ForAll(max, x -> IsMaximalSubsemigroup(R, x));
true
gap> G := Group([ (1,2,3,4), (1,3,2,4) ]);
Group([ (1,2,3,4), (1,3,2,4) ])
gap> mat := [
> [ (),         (4,3),  0,      0,  0 ],
> [ (1,3)(2,4), 0,      0,      0,  0 ],
> [ 0,          0,      (4,3),  0,  0 ],
> [ 0,          0,      0,      (), 0 ],
> [ 0,          0,      0,      (), () ],
> [ 0,          0,      0,      (), 0 ] ];;
gap> R := ReesZeroMatrixSemigroup(G, mat); # (3-component) 5x6 RZMS over Sym(4)
<Rees 0-matrix semigroup 5x6 over Group([ (1,2,3,4), (1,3,2,4) ])>
gap> max := MaximalSubsemigroups(R);;
gap> Size(max);
116
gap> mat:=[
>   [ (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
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
gap> G := Group(());;
gap> R := ReesZeroMatrixSemigroup(G, mat);;
gap> max := MaximalSubsemigroups(R);; # 13x13 inverse RZMS over trivial group
gap> Size(max);
8190
gap> G := Group([ (1,4,2), (1,4,5) ]);
Group([ (1,4,2), (1,4,5) ])
gap> mat := [
> [ 0,  0,        (1,4,2),  (1,4,5),  0 ],
> [ 0,  (),       (1,2,5),  0,        0 ],
> [ (), (1,2,5),  0,        0,        0 ],
> [ (), 0,        0,        0,        (1,5,2) ],
> [ 0,  0,        0,        (1,4,2),  (1,4,5) ] ];;
gap> R1 := ReesZeroMatrixSemigroup(G, mat); # a connected 5x5 RZMS over Sym(3)
<Rees 0-matrix semigroup 5x5 over Group([ (1,4,2), (1,4,5) ])>
gap> max := MaximalSubsemigroups(R1);
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

#T# MaximalSubsemigroups: for a Rees 0-matrix semigroup and a maximal subgroup
gap> H := Group([ (1,2,5) ]); # maximal subgroup <(1,2,5)>, using R1 from before
Group([ (1,2,5) ])
gap> MaximalSubsemigroups(R1, H);
[ <subsemigroup of 5x5 Rees 0-matrix semigroup with 12 generators> ]
gap> last[1] in max;
true
gap> MaximalSubsemigroups(R1, Group([ (2,4,5) ])); # no results exist
[  ]
gap> MaximalSubsemigroups(R1, Group([ (6,7) ])); # not a subgroup
Error, Semigroups: MaximalSubsemigroups: usage,
the second argument <H> must be a subgroup of the underlying
group of the Rees 0-matrix semigroup in the first first argument, <R>,
gap> MaximalSubsemigroups(R1, Group(())); # not a maximal subgroup
Error, Semigroups: MaximalSubsemigroups: usage,
the second argument <H> must be a maximal subgroup of the underlying
group of the Rees 0-matrix semigroup in the first first argument, <R>,
gap> R2 := Semigroup( RMSElement( R1, 1, (1,4,2), 3 ) ); # not a RZMS
<subsemigroup of 5x5 Rees 0-matrix semigroup with 1 generator>
gap> IsReesZeroMatrixSubsemigroup(R2);
true
gap> IsReesZeroMatrixSemigroup(R2);
false
gap> MaximalSubsemigroups(R2, Group(()));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `MaximalSubsemigroups' on 2 arguments
gap> t1 := Transformation( [ 4, 3, 1, 3 ] );;
gap> t2 := Transformation( [ 3, 3, 2, 2 ] );;
gap> t3 := Transformation( [  ] );
IdentityTransformation
gap> T := Semigroup([ t1, t2 ]);
<transformation semigroup on 4 pts with 2 generators>
gap> IsRegularSemigroup(T);
true
gap> IsGroup(T);
false
gap> mat := [ [ t2, t1 ], [ t1, t2 ] ];;
gap> R3 := ReesZeroMatrixSemigroup(T, mat); # a RZMS over a non-group semigroup
<Rees 0-matrix semigroup 2x2 over <regular transformation semigroup 
  on 4 pts with 2 generators>>
gap> IsReesZeroMatrixSubsemigroup(R3);
true
gap> IsReesZeroMatrixSemigroup(R3);
true
gap> SetIsRegularSemigroup(R3, true); # temp hack till Issue #108 is resolved
gap> IsRegularSemigroup(R3);
true
gap> MaximalSubsemigroups(R3, Group(()));
Error, Semigroups: MaximalSubsemigroups: usage,
the first argument <R> must be a Rees 0-matrix semigroup whose
underlying semigroup is a group,
gap> R4 := ReesZeroMatrixSemigroup(H, [ [ 0 ] ]); # not a regular RZMS
<Rees 0-matrix semigroup 1x1 over Group([ (1,2,5) ])>
gap> IsReesZeroMatrixSubsemigroup(R4);
true
gap> IsReesZeroMatrixSemigroup(R4);
true
gap> IsRegularSemigroup(R4);
false
gap> MaximalSubsemigroups(R4, Group(()));
Error, Semigroups: MaximalSubsemigroups,
the first argument <R> must be a regular Rees 0-matrix semigroup,

#T# MaximalSubsemigroups: for a transformation semigroup
gap> S := Semigroup( Transformation( [  ] ) ); # trivial semigroup
<trivial transformation group>
gap> MaximalSubsemigroups(S);
[  ]
gap> S := Semigroup( Transformation( [ 2, 3, 1 ] ) ); # group C3 as semigroup
<commutative transformation semigroup on 3 pts with 1 generator>
gap> MaximalSubsemigroups(S);
[ <trivial transformation group> ]
gap> S := Semigroup( [
> Transformation( [ 1, 2, 1 ] ),
> Transformation( [ 1, 2, 2 ] ) ] ); # simple semigroup
<transformation semigroup on 3 pts with 2 generators>
gap> max := MaximalSubsemigroups(S);
[ <commutative transformation semigroup on 3 pts with 1 generator>, 
  <commutative transformation semigroup on 3 pts with 1 generator> ]
gap> List(max, Size);
[ 1, 1 ]
gap> max;
[ <trivial transformation group on 3 pts with 1 generator>, 
  <trivial transformation group on 3 pts with 1 generator> ]
gap> S := Monoid( [
> Transformation( [ 1, 1 ] ) ] ); # simple semigroup with adjoined zero
<commutative transformation monoid on 2 pts with 1 generator>
gap> max := MaximalSubsemigroups(S);
[ <commutative transformation semigroup on 2 pts with 1 generator>, 
  <trivial transformation group> ]
gap> S := Monoid( [ 
> Transformation( [ 1, 1, 2 ] ) ] ); # semigroup with gen in non-regular D-class
<commutative transformation monoid on 3 pts with 1 generator>
gap> max := MaximalSubsemigroups(S);
[ <commutative transformation semigroup on 3 pts with 1 generator>, 
  <commutative transformation monoid on 3 pts with 1 generator> ]
gap> List(max, Elements);
[ [ Transformation( [ 1, 1, 1 ] ), Transformation( [ 1, 1, 2 ] ) ], 
  [ Transformation( [ 1, 1, 1 ] ), IdentityTransformation ] ]
gap> S := Monoid( [
> Transformation( [ 1, 1, 1, 1, 1 ] ),     # semigroup with a result arising
> Transformation( [ 2, 1, 4, 3, 2 ] ),     # from non-maximal regular D-class
> Transformation( [ 2, 1, 4, 3, 4 ] ) ] ); # which intersects every H-class
<transformation monoid on 5 pts with 3 generators>
gap> max := MaximalSubsemigroups(S);;
gap> Length(max);
5
gap> ForAll(max, x -> IsMaximalSubsemigroup(S, x));
true
gap> IsDuplicateFreeList(max);
true
gap> List(max, Elements);
[ [ Transformation( [ 1, 1, 1, 1, 1 ] ), Transformation( [ 1, 2, 3, 4, 1 ] ), 
      Transformation( [ 1, 2, 3, 4, 3 ] ), Transformation( [ 2, 1, 4, 3, 2 ] )
        , Transformation( [ 2, 1, 4, 3, 4 ] ), 
      Transformation( [ 2, 2, 2, 2, 2 ] ) ], 
  [ Transformation( [ 1, 2, 3, 4, 1 ] ), Transformation( [ 1, 2, 3, 4, 3 ] ), 
      IdentityTransformation, Transformation( [ 2, 1, 4, 3, 2 ] ), 
      Transformation( [ 2, 1, 4, 3, 4 ] ) ], 
  [ Transformation( [ 1, 1, 1, 1, 1 ] ), Transformation( [ 1, 2, 3, 4, 1 ] ), 
      Transformation( [ 1, 2, 3, 4, 3 ] ), IdentityTransformation, 
      Transformation( [ 2, 2, 2, 2, 2 ] ) ], 
  [ Transformation( [ 1, 1, 1, 1, 1 ] ), Transformation( [ 1, 2, 3, 4, 3 ] ), 
      IdentityTransformation, Transformation( [ 2, 1, 4, 3, 4 ] ), 
      Transformation( [ 2, 2, 2, 2, 2 ] ) ], 
  [ Transformation( [ 1, 1, 1, 1, 1 ] ), Transformation( [ 1, 2, 3, 4, 1 ] ), 
      IdentityTransformation, Transformation( [ 2, 1, 4, 3, 2 ] ), 
      Transformation( [ 2, 2, 2, 2, 2 ] ) ] ]
gap> S := Monoid([
> Transformation( [ 2, 2, 4, 4, 6, 6, 8, 8, 10, 10, 12, 12, 14, 14, 16, 16,
>      18, 18, 20, 20, 22, 22, 24, 24, 14 ] ), 
>  Transformation( [ 12, 12, 10, 10, 8, 8, 6, 6, 4, 4, 2, 2, 24, 24, 22, 22,
>      20, 20, 18, 18, 16, 16, 14, 14, 12 ] ), 
>  Transformation( [ 5, 5, 7, 7, 1, 1, 3, 3, 11, 11, 9, 9, 17, 17, 19, 19, 13,
>      13, 15, 15, 23, 23, 21, 21, 5 ] ) ]); # highlights a special case
<transformation monoid on 25 pts with 3 generators>
gap> max := MaximalSubsemigroups(S);;
gap> Size(max);
9
gap> IsDuplicateFreeList(max);
true
gap> ForAll(max, x -> IsMaximalSubsemigroup(S, x));
true
gap> T3 := FullTransformationMonoid(3); # Trans(3)
<full transformation semigroup on 3 pts>
gap> max := MaximalSubsemigroups(T3);;
gap> correct := [
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
gap> S := Semigroup([
>   Transformation( [ 2, 1, 5, 2, 4 ] ),
>   Transformation( [ 2, 3, 4, 3, 1 ] ),
>   Transformation( [ 3, 4, 1, 4, 3 ] ),
>   Transformation( [ 3, 4, 2, 2, 2 ] ),
>   Transformation( [ 5, 1, 1, 2, 3 ] ) ]); # A random example
<transformation semigroup on 5 pts with 5 generators>
gap> max := MaximalSubsemigroups(S);;
gap> Size(max);
8
gap> IsDuplicateFreeList(max);
true
gap> ForAll(max, x -> IsMaximalSubsemigroup(S, x));
true

#T# MaximalSubsemigroups: for semigroup of partial permutations
gap> gens := [
>   PartialPerm( [ 1, 3, 4, 5 ], [ 6, 5, 2, 4 ] ),
>   PartialPerm( [ 1, 2, 3, 4, 6 ], [ 5, 4, 3, 1, 6 ] ),
>   PartialPerm( [ 1, 2, 5, 7 ], [ 3, 1, 4, 6 ] ) ];;
gap> S := InverseSemigroup(gens); # a random inverse semigroup of partial perms
<inverse partial perm semigroup on 7 pts with 3 generators>
gap> max := MaximalSubsemigroups(S);;
gap> Size(max);
6
gap> IsDuplicateFreeList(max);
true
gap> List(max, Size);
[ 715, 715, 714, 714, 713, 713 ]
gap> gens := [ PartialPerm( [ 1, 2, 3, 4 ], [ 3, 2, 5, 4 ] ), 
>  PartialPerm( [ 1, 2, 4 ], [ 3, 5, 4 ] ), 
>  PartialPerm( [ 1, 2, 3, 4 ], [ 5, 2, 3, 1 ] ), 
>  PartialPerm( [ 1, 3, 4, 5 ], [ 5, 3, 4, 1 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5 ], [ 5, 4, 3, 2, 1 ] ) ];;
gap> S := InverseSemigroup(gens); # a random inverse semigroup of partial perms
<inverse partial perm semigroup on 5 pts with 5 generators>
gap> max := MaximalSubsemigroups(S);;
gap> Size(max);
9
gap> IsDuplicateFreeList(max);
true
gap> List(max, Size);
[ 955, 892, 892, 892, 892, 892, 892, 924, 924 ]
gap> ForAll(max, x -> IsMaximalSubsemigroup(S, x));
true

#T# MaximalSubsemigroups: for a semigroup of partitions
gap> B := PartitionMonoid(3); # partition monoid of degree 3
<regular bipartition monoid on 3 pts with 4 generators>
gap> max := MaximalSubsemigroups(B);;
gap> Size(max);
8
gap> List(max, Size);
[ 200, 199, 199, 199, 167, 167, 167, 167 ]
gap> S := max[1];; # the first maximal subsemigroup of partition monoid 3
gap> max := MaximalSubsemigroups(S);;
gap> Size(max);
5
gap> B := Semigroup( [ Bipartition( [ [ 1, 2, -4 ], [ 3, 4, 5, 7 ], [ 6, -5, -7 ], [ -1, -3, -6 ], [ -2 ] ] ), 
>   Bipartition( [ [ 1, 5, 6, -2, -4, -6, -7 ], [ 2, 3, 4, -3, -5 ], [ 7, -1 ] ] ), 
>   Bipartition( [ [ 1, 6 ], [ 2, 3, 4 ], [ 5 ], [ 7, -6, -7 ], [ -1, -3, -4 ], [ -2 ], [ -5 ] ] ), 
>   Bipartition( [ [ 1, -4 ], [ 2, 3, 7 ], [ 4, 5, -7 ], [ 6, -1, -2, -3, -5 ], [ -6 ] ] ), 
>   Bipartition( [ [ 1, 7, -3 ], [ 2, 4, 5, -4 ], [ 3, 6 ], [ -1 ], [ -2 ], [ -5 ], [ -6 ], [ -7 ] ] ), 
>   Bipartition( [ [ 1, 5, -2, -4, -6, -7 ], [ 2, -1, -3, -5 ], [ 3, 4, 6, 7 ] ] ) ] ); # a random example
<bipartition semigroup on 7 pts with 6 generators>
gap> max := MaximalSubsemigroups(B);;
gap> IsDuplicateFreeList(max);
true
gap> Size(max);
7
gap> ForAll(max,x -> IsMaximalSubsemigroup(B,x) );
true

#T# MaximalSubsemigroups: for a semigroup of block bijections
gap> C := InverseSemigroup( [ 
>   Bipartition( [ [ 1, -4 ], [ 2, -5 ], [ 3, 4, 5, 6, 7, -1, -2, -3, -6, -7 ] ] ), 
>   Bipartition( [ [ 1, -6 ], [ 2, -3 ], [ 3, 5, 6, 7, -1, -4, -5, -7 ], [ 4, -2 ] ] ), 
>   Bipartition( [ [ 1, -6 ], [ 2, -2 ], [ 3, 6, 7, -1, -5, -7 ], [ 4, -4 ], [ 5, -3 ] ] ), 
>   Bipartition( [ [ 1, -6 ], [ 2, -3 ], [ 3, -2 ], [ 4, 5, 7, -1, -5, -7 ], [ 6, -4 ] ] ) ] ); # a random inverse semigroup of block bijections
<inverse bipartition semigroup on 7 pts with 4 generators>
gap> max := MaximalSubsemigroups(C);;
gap> Size(max);
8
gap> List(max, Size);
[ 446, 446, 446, 446, 446, 446, 433, 433 ]
gap> IsDuplicateFreeList(max);
true
gap> ForAll(max, x -> IsMaximalSubsemigroup(C, x));
true

#T# MaximalSubsemigroups: for a transformation semigroup ideal
gap> S := SingularTransformationSemigroup(5); # Trans(5) \ Sym(5)
<regular transformation semigroup ideal on 5 pts with 1 generator>
gap> max := MaximalSubsemigroups(S);;
gap> Size(max);
40
gap> S = max[1];
false

#T# Issue 107 (problems with Green's classes of ideals, and inverse semigroups)
gap> gens := [ PartialPerm( [ 1, 2, 3, 4 ], [ 3, 2, 5, 4 ] ), 
>  PartialPerm( [ 1, 2, 4 ], [ 3, 5, 4 ] ), 
>  PartialPerm( [ 1, 2, 3, 4 ], [ 5, 2, 3, 1 ] ), 
>  PartialPerm( [ 1, 3, 4, 5 ], [ 5, 3, 4, 1 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5 ], [ 5, 4, 3, 2, 1 ] ) ];;
gap> S := InverseSemigroup(gens);;
gap>  S := Semigroup(S);;
gap> Length(MaximalSubsemigroups(S));
9

#E#
gap> STOP_TEST("Semigroups package: maximal.tst");
