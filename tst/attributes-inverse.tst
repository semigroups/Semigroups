#%T##########################################################################
##
#W  attributes-inverse.tst
#Y  Copyright (C) 2012-15                                  Wilfred Wilson
##
##  Licensing information can be found in the README file of this package.
#
#############################################################################
##
gap> START_TEST("Semigroups package: attributes-inverse.tst");
gap> LoadPackage("semigroups", false);;

# 
gap> SemigroupsStartTest();

#T# AttributesInverseTest1: JoinIrreducibleDClasses
gap> S := InverseSemigroup([
>  PartialPerm( [ 1, 2, 3, 4 ], [ 4, 1, 2, 6 ] ),
>  PartialPerm( [ 1, 2, 3, 4 ], [ 5, 7, 1, 6 ] ),
>  PartialPerm( [ 1, 2, 3, 5 ], [ 5, 2, 7, 3 ] ),
>  PartialPerm( [ 1, 2, 3, 6, 7 ], [ 1, 3, 4, 7, 5 ] ),
>  PartialPerm( [ 1, 2, 3, 4, 5, 7 ], [ 3, 2, 4, 6, 1, 5 ] ) ]);
<inverse partial perm semigroup on 7 pts with 5 generators>
gap> iso := IsomorphismBipartitionSemigroup(S);;
gap> A := Range(iso);
<inverse bipartition semigroup on 7 pts with 5 generators>
gap> I := SemigroupIdeal(S, PartialPerm( [ 1, 3, 4, 5, 7 ],[ 1, 3, 4, 5, 7 ] ));
<inverse partial perm semigroup ideal on 7 pts with 1 generator>
gap> B := InverseSemigroup([
> Bipartition([ [ 1, -6 ],[ 2, -4 ],[ 3, -3 ],[ 4, 5, 6, 7, -1, -2, -5, -7 ] ]),
> Bipartition([[ 1, -4 ],[ 2, -5 ],[ 3, 6, 7, -2, -3, -7 ],[ 4, -1 ],[ 5, -6]]),
> Bipartition([[ 1, -6 ],[ 2, -5 ],[ 3, 5, 7, -3, -4, -7 ],[ 4, -2 ],[ 6, -1]]) 
> ]);
<inverse bipartition semigroup on 7 pts with 3 generators>
gap> J := SemigroupIdeal(B,
>  Bipartition([ [ 1, -1 ], [ 2, 3, 5, 7, -2, -3, -5, -7 ], [ 4, -4 ],
>    [ 6, -6 ] ]),
>  Bipartition([ [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ], [ 3, -3 ] ]));
<inverse bipartition semigroup ideal on 7 pts with 2 generators>
gap> JoinIrreducibleDClasses(S);
[ {PartialPerm( [ 2 ], [ 2 ] )} ]
gap> JoinIrreducibleDClasses(I);
[ {PartialPerm( [ 2 ], [ 2 ] )} ]
gap> JoinIrreducibleDClasses(A);
[ {Bipartition( [ [ 1 ], [ 2, -2 ], [ 3 ], [ 4 ], [ 5 ], [ 6 ], [ 7 ], 
     [ -1 ], [ -3 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ] ] )} ]
gap> JoinIrreducibleDClasses(B);
[ {Bipartition( [ [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ], [ 3, -3 ] ] )}
    , {Bipartition( [ [ 1, -1 ], [ 2, 3, 4, 5, 6, 7, -2, -3, -4, -5, -6, -7 ] 
      ] )} ]
gap> JoinIrreducibleDClasses(J);
[ {Bipartition( [ [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ], [ 3, -3 ] ] )}
    , {Bipartition( [ [ 1, 2, 3, 4, 5, 7, -1, -2, -3, -4, -5, -7 ], 
     [ 6, -6 ] ] )} ]

#T# AttributesInverseTest2: IsJoinIrreducible
gap> S := InverseSemigroup([
>  PartialPerm( [ 1, 2, 3, 4 ], [ 4, 1, 2, 6 ] ),
>  PartialPerm( [ 1, 2, 3, 4 ], [ 5, 7, 1, 6 ] ),
>  PartialPerm( [ 1, 2, 3, 5 ], [ 5, 2, 7, 3 ] ),
>  PartialPerm( [ 1, 2, 3, 6, 7 ], [ 1, 3, 4, 7, 5 ] ),
>  PartialPerm( [ 1, 2, 3, 4, 5, 7 ], [ 3, 2, 4, 6, 1, 5 ] ) ]);;
gap> iso := IsomorphismBipartitionSemigroup(S);;
gap> A := Range(iso);;
gap> I := SemigroupIdeal(S,
> PartialPerm( [ 1, 3, 4, 5, 7 ],[ 1, 3, 4, 5, 7 ] ));;
gap> B := InverseSemigroup([
> Bipartition([ [ 1, -6 ],[ 2, -4 ],[ 3, -3 ],[ 4, 5, 6, 7, -1, -2, -5, -7 ] ]),
> Bipartition([[ 1, -4 ],[ 2, -5 ],[ 3, 6, 7, -2, -3, -7 ],[ 4, -1 ],[ 5, -6]]),
> Bipartition([[ 1, -6 ],[ 2, -5 ],[ 3, 5, 7, -3, -4, -7 ],[ 4, -2 ],[ 6, -1]]) 
> ]);;
gap> J := SemigroupIdeal(B,
>  Bipartition([ [ 1, -1 ],[ 2, 3, 5, 7, -2, -3, -5, -7 ], [ 4, -4 ],
>    [ 6, -6 ] ]),
>  Bipartition([ [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ], [ 3, -3 ] ]));;
gap> x := PartialPerm( [ 1, 2, 4, 6 ], [ 2, 3, 1, 4 ] );;   xx := x ^ iso;;
gap> x in S;
true
gap> IsJoinIrreducible(S, x);
false
gap> x in I;
true
gap> IsJoinIrreducible(I, x);
false
gap> xx in A;
true
gap> IsJoinIrreducible(A, xx);
false
gap> y := PartialPerm( [ 5 ], [ 3 ] );;   yy := y ^ iso;;
gap> IsJoinIrreducible(S, y);
true
gap> IsJoinIrreducible(I, y);
true
gap> IsJoinIrreducible(A, yy);
true
gap> P := Bipartition( [ [ 1, 3, 5, 6, 7, -3, -4, -5, -6, -7 ], [ 2, -2 ],
> [ 4, -1 ] ] );;
gap> IsJoinIrreducible(B, P);
false
gap> IsJoinIrreducible(J, P);
false
gap> Q := Bipartition( [ [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ],
> [ 3, -3 ] ] );;
gap> IsJoinIrreducible(B, Q);
true
gap> IsJoinIrreducible(J, Q);
true
gap> R := Bipartition( [ [ 1, -4 ],
> [ 2, 3, 4, 5, 6, 7, -1, -2, -3, -5, -6, -7 ] ] );;
gap> IsJoinIrreducible(B, R);
true
gap> IsJoinIrreducible(J, R);
true

#T# AttributesInverseTest3: Minorants 1
gap> S := InverseSemigroup([
>  PartialPerm( [ 1, 2, 3, 4 ], [ 4, 1, 2, 6 ] ),
>  PartialPerm( [ 1, 2, 3, 4 ], [ 5, 7, 1, 6 ] ),
>  PartialPerm( [ 1, 2, 3, 5 ], [ 5, 2, 7, 3 ] ),
>  PartialPerm( [ 1, 2, 3, 6, 7 ], [ 1, 3, 4, 7, 5 ] ),
>  PartialPerm( [ 1, 2, 3, 4, 5, 7 ], [ 3, 2, 4, 6, 1, 5 ] ) ]);;
gap> iso := IsomorphismBipartitionSemigroup(S);;
gap> A := Range(iso);;
gap> I := SemigroupIdeal(S,
> PartialPerm( [ 1, 3, 4, 5, 7 ],[ 1, 3, 4, 5, 7 ] ));;
gap> B := InverseSemigroup([
> Bipartition([ [ 1, -6 ],[ 2, -4 ],[ 3, -3 ],[ 4, 5, 6, 7, -1, -2, -5, -7 ] ]),
> Bipartition([[ 1, -4 ],[ 2, -5 ],[ 3, 6, 7, -2, -3, -7 ],[ 4, -1 ],[ 5, -6]]),
> Bipartition([[ 1, -6 ],[ 2, -5 ],[ 3, 5, 7, -3, -4, -7 ],[ 4, -2 ],[ 6, -1]]) 
> ]);;
gap> J := SemigroupIdeal(B,
>  Bipartition([ [ 1, -1 ], [ 2, 3, 5, 7, -2, -3, -5, -7 ],
>    [ 4, -4 ], [ 6, -6 ] ]),
>  Bipartition([ [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ],[ 3, -3 ] ]));;
gap> x := PartialPerm( [ 1, 2, 4, 6 ], [ 2, 3, 1, 4 ] );;
gap> Minorants(S, x);
[ <empty partial perm>, [1,2], [2,3], [6,4], [4,1], [1,2,3], [2,3][6,4], 
  [4,1,2], [2,3][4,1], [1,2][6,4], [6,4,1], [4,1,2,3], [6,4,1,2], 
  [1,2,3][6,4], [2,3][6,4,1] ]
gap> Minorants(I, x);
[ <empty partial perm>, [1,2], [2,3], [6,4], [4,1], [1,2,3], [6,4,1], 
  [2,3][4,1], [1,2][6,4], [4,1,2], [2,3][6,4], [4,1,2,3], [2,3][6,4,1], 
  [6,4,1,2], [1,2,3][6,4] ]
gap> Minorants(A, x ^ iso);
[ <bipartition: [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 6 ], [ 7 ], [ -1 ], 
     [ -2 ], [ -3 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 6, -4 ], [ 7 ], [ -1 ], 
     [ -2 ], [ -3 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2 ], [ 3 ], [ 4, -1 ], [ 5 ], [ 6 ], [ 7 ], [ -2 ], 
     [ -3 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1, -2 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 6 ], [ 7 ], [ -1 ], 
     [ -3 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2, -3 ], [ 3 ], [ 4 ], [ 5 ], [ 6 ], [ 7 ], [ -1 ], 
     [ -2 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1, -2 ], [ 2, -3 ], [ 3 ], [ 4 ], [ 5 ], [ 6 ], [ 7 ], 
     [ -1 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2, -3 ], [ 3 ], [ 4, -1 ], [ 5 ], [ 6 ], [ 7 ], 
     [ -2 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1, -2 ], [ 2 ], [ 3 ], [ 4, -1 ], [ 5 ], [ 6 ], [ 7 ], 
     [ -3 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2, -3 ], [ 3 ], [ 4 ], [ 5 ], [ 6, -4 ], [ 7 ], 
     [ -1 ], [ -2 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1, -2 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 6, -4 ], [ 7 ], 
     [ -1 ], [ -3 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2 ], [ 3 ], [ 4, -1 ], [ 5 ], [ 6, -4 ], [ 7 ], 
     [ -2 ], [ -3 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2, -3 ], [ 3 ], [ 4, -1 ], [ 5 ], [ 6, -4 ], [ 7 ], 
     [ -2 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1, -2 ], [ 2, -3 ], [ 3 ], [ 4, -1 ], [ 5 ], [ 6 ], [ 7 ], 
     [ -4 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1, -2 ], [ 2 ], [ 3 ], [ 4, -1 ], [ 5 ], [ 6, -4 ], [ 7 ], 
     [ -3 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1, -2 ], [ 2, -3 ], [ 3 ], [ 4 ], [ 5 ], [ 6, -4 ], [ 7 ], 
     [ -1 ], [ -5 ], [ -6 ], [ -7 ]> ]
gap> z := PartialPerm( [  ], [  ] );
<empty partial perm>
gap> zz := z ^ iso;;
gap> z in S;
true
gap> z in I;
true
gap> zz in A;
true
gap> Minorants(S, z);
[  ]
gap> Minorants(I, z);
[  ]
gap> Minorants(A, zz);
[  ]
gap> P := Bipartition( [ [ 1, 3, 5, 6, 7, -3, -4, -5, -6, -7 ], [ 2, -2 ],
> [ 4, -1 ] ] );;
gap> Q := Bipartition( [ [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ],
> [ 3, -3 ] ] );;
gap> R := Bipartition( [ [ 1, -4 ],
> [ 2, 3, 4, 5, 6, 7, -1, -2, -3, -5, -6, -7 ] ] );;
gap> m := Minorants(B, Q);
[ <block bijection: [ 1, 2, 3, 4, 5, 6, 7, -1, -2, -3, -4, -5, -6, -7 ]> ]
gap> Minorants(B, R) = m;
true
gap> Minorants(J, Q) = m;
true
gap> Minorants(J, R) = m;
true
gap> m[1] = MultiplicativeZero(B);
true
gap> m := Minorants(B, Bipartition(
> [ [ 1, -6 ], [ 2, -4 ], [ 3, 4, 7, -3, -5, -7 ], [ 5, -2 ], [ 6, -1 ] ] ));;
gap> Size(m);
15
gap> m1 := Minorants(J, Bipartition(
> [ [ 1, -6 ], [ 2, -5 ], [ 3, 5, 6, 7, -1, -3, -4, -7 ], [ 4, -2 ] ] ));;
gap> Size(m1);
7

#T# AttributesInverseTest4: Minorants 2
gap> U := InverseSemigroup(
> PartialPerm([ 1, 3, 4, 5, 7 ], [ 1, 5, 3, 8, 4 ]),
> PartialPerm([ 1, 2, 3, 4, 5, 6 ] ,[ 6, 7, 1, 4, 3, 2 ]),
> PartialPerm([ 1, 2, 3, 4, 5, 8 ] ,[ 5, 6, 3, 8, 4, 7 ]),
> PartialPerm([ 1, 3, 4, 5, 6, 8 ] ,[ 8, 7, 5, 1, 3, 4 ]),
> PartialPerm([ 1, 3, 4, 5, 7, 8 ] ,[ 6, 5, 7, 1, 4, 2 ]) );;
gap> t := Elements(U)[51624];
[7,1][8,6](4,5)
gap> Minorants(U, t);
[ <empty partial perm>, [7,1], [4,5], [5,4], [8,6], (4,5), [4,5][8,6], 
  [5,4][8,6], [5,4][7,1], [7,1][8,6], [4,5][7,1], [7,1](4,5), [4,5][7,1][8,6],
  [8,6](4,5), [5,4][7,1][8,6] ]

#T# AttributesInverseTest5: MajorantClosure and IsMajorantlyClosed
gap> S := InverseSemigroup([
>  PartialPerm( [ 1, 2, 3, 4 ], [ 4, 1, 2, 6 ] ),
>  PartialPerm( [ 1, 2, 3, 4 ], [ 5, 7, 1, 6 ] ),
>  PartialPerm( [ 1, 2, 3, 5 ], [ 5, 2, 7, 3 ] ),
>  PartialPerm( [ 1, 2, 3, 6, 7 ], [ 1, 3, 4, 7, 5 ] ),
>  PartialPerm( [ 1, 2, 3, 4, 5, 7 ], [ 3, 2, 4, 6, 1, 5 ] ) ]);;
gap> iso := IsomorphismBipartitionSemigroup(S);;
gap> A := Range(iso);;
gap> I := SemigroupIdeal(S,
> PartialPerm( [ 1, 3, 4, 5, 7 ],[ 1, 3, 4, 5, 7 ] ));;
gap> B := InverseSemigroup([
> Bipartition([ [ 1, -6 ],[ 2, -4 ],[ 3, -3 ],[ 4, 5, 6, 7, -1, -2, -5, -7 ] ]),
> Bipartition([[ 1, -4 ],[ 2, -5 ],[ 3, 6, 7, -2, -3, -7 ],[ 4, -1 ],[ 5, -6]]),
> Bipartition([[ 1, -6 ],[ 2, -5 ],[ 3, 5, 7, -3, -4, -7 ],[ 4, -2 ],[ 6, -1]]) 
> ]);;
gap> J := SemigroupIdeal(B,
>  Bipartition([ [ 1, -1 ], [ 2, 3, 5, 7, -2, -3, -5, -7 ], [ 4, -4 ],
>    [ 6, -6 ] ]),
>  Bipartition([ [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ],[ 3, -3 ] ]));;
gap> x := PartialPerm( [ 1, 2, 4, 6 ], [ 2, 3, 1, 4 ] );;   xx := x ^ iso;;
gap> y := PartialPerm( [ 5 ], [ 3 ] );;   yy := y ^ iso;;
gap> P := Bipartition( [ [ 1, 3, 5, 6, 7, -3, -4, -5, -6, -7 ], [ 2, -2 ],
> [ 4, -1 ] ] );;
gap> Q := Bipartition( [ [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ],
> [ 3, -3 ] ] );;
gap> R := Bipartition( [ [ 1, -4 ],
> [ 2, 3, 4, 5, 6, 7, -1, -2, -3, -5, -6, -7 ] ] );;
gap> IsMajorantlyClosed(S, [ x ]);
true
gap> MajorantClosure(S, [ x ]) = [ x ];
true
gap> IsMajorantlyClosed(I, [ x ]);
true
gap> MajorantClosure(I, [ x ]) = [ x ];
true
gap> IsMajorantlyClosed(A, [ xx ]);
true
gap> MajorantClosure(A, [ xx ]) = [ xx ];
true
gap> IsMajorantlyClosed(S, [ y ]);      
false
gap> IsMajorantlyClosed(I, [ y ]);      
false
gap> IsMajorantlyClosed(A, [ yy ]);
false
gap> m := MajorantClosure(S, [ y ]);;
gap> Size(m);
486
gap> m1 := MajorantClosure(I, [ y ]);;
gap> Size(m1);
485
gap> m2 := MajorantClosure(A, [ yy ]);;
gap> Size(m2);
486
gap> ForAll(m1, x -> x in m);
true
gap> IsMajorantlyClosed(B, [ P ]);
true
gap> IsMajorantlyClosed(B, [ Q ]);
false
gap> IsMajorantlyClosed(B, [ R ]);
false
gap> IsMajorantlyClosed(J, [ P ]);
true
gap> IsMajorantlyClosed(J, [ Q ]);
true
gap> IsMajorantlyClosed(J, [ R ]);
false
gap> MajorantClosure(B, [ Q ]);
[ <block bijection: [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ], [ 3, -3 ]>, 
  <block bijection: [ 1, 2, 5, 7, -1, -2, -5, -7 ], [ 3, -3 ], [ 4, -4 ], 
     [ 6, -6 ]>, <block bijection: [ 1, 2, 5, 7, -4, -5, -6, -7 ], [ 3, -3 ], 
     [ 4, -2 ], [ 6, -1 ]>, 
  <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], 
     [ 4, 5, 6, 7, -4, -5, -6, -7 ]>, 
  <block bijection: [ 1, -6 ], [ 2, -4 ], [ 3, -3 ], 
     [ 4, 5, 6, 7, -1, -2, -5, -7 ]> ]
gap> m := MajorantClosure(B, [ R ]);;
gap> IsMajorantlyClosed(B, m);
true
gap> Size(MajorantClosure(B, [ Q, R ]));
33
gap> m := MajorantClosure(J, [ R ]);;
gap> IsMajorantlyClosed(J, m);
true
gap> Size(m);
25

#T# AttributesInverseTest6: RightCosetsOfInverseSemigroup
gap> S := InverseSemigroup([
>  PartialPerm( [ 1, 2, 3, 4 ], [ 4, 1, 2, 6 ] ),
>  PartialPerm( [ 1, 2, 3, 4 ], [ 5, 7, 1, 6 ] ),
>  PartialPerm( [ 1, 2, 3, 5 ], [ 5, 2, 7, 3 ] ),
>  PartialPerm( [ 1, 2, 3, 6, 7 ], [ 1, 3, 4, 7, 5 ] ),
>  PartialPerm( [ 1, 2, 3, 4, 5, 7 ], [ 3, 2, 4, 6, 1, 5 ] ) ]);;
gap> iso := IsomorphismBipartitionSemigroup(S);;
gap> A := Range(iso);;
gap> B := InverseSemigroup([
> Bipartition([ [ 1, -6 ],[ 2, -4 ],[ 3, -3 ],[ 4, 5, 6, 7, -1, -2, -5, -7 ] ]),
> Bipartition([[ 1, -4 ],[ 2, -5 ],[ 3, 6, 7, -2, -3, -7 ],[ 4, -1 ],[ 5, -6]]),
> Bipartition([[ 1, -6 ],[ 2, -5 ],[ 3, 5, 7, -3, -4, -7 ],[ 4, -2 ],[ 6, -1]]) 
> ]);;
gap> w := PartialPerm( [ 1, 2, 3, 4 ], [ 1, 2, 3, 4 ] );
<identity partial perm on [ 1, 2, 3, 4 ]>
gap> m := MajorantClosure(S, [ w ]);;
gap> W := InverseSemigroup(m);
<inverse partial perm semigroup on 7 pts with 5 generators>
gap> IsMajorantlyClosed(S, W);
true
gap> RightCosetsOfInverseSemigroup(S, W);
[ [ [3,2,1,4,6] ], [ [2,7][3,1,5][4,6] ], [ [1,3,5][4,7](2) ], 
  [ <identity partial perm on [ 1, 2, 3, 4 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 5 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 5, 6 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 5, 7 ]> ], 
  [ [4,3,1,5](2), [4,3,1,5,7](2), [6,4,3,1,5](2), [6,4,3,1,5,7](2) ], 
  [ [1,3,2,5](4) ], [ [3,1,4,5](2) ], [ [2,7][4,3,1,5] ], [ [2,3,4,1,7] ], 
  [ [4,3,2,7](1) ], [ [1,3,4,6](2), [5,1,3,4,6](2), [7,5,1,3,4,6](2) ], 
  [ [3,5][4,1,7](2), [4,1,7][6,3,5](2) ], [ [2,5](1)(3)(4) ], 
  [ [4,1,6](2)(3) ], [ [2,1,3,4,6] ], [ [3,1,7][4,2,6] ], [ [1,5][2,4,3,6] ] ]
gap> ww := w ^ iso;;
gap> m := MajorantClosure(A, [ ww ]);;
gap> WW := InverseSemigroup(m);
<inverse bipartition semigroup on 7 pts with 5 generators>
gap> IsMajorantlyClosed(A, WW);
true
gap> RightCosetsOfInverseSemigroup(A, WW);
[ [ <bipartition: [ 1, -4 ], [ 2, -1 ], [ 3, -2 ], [ 4, -6 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -3 ], [ -5 ], [ -7 ]> ], 
  [ <bipartition: [ 1, -5 ], [ 2, -7 ], [ 3, -1 ], [ 4, -6 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -2 ], [ -3 ], [ -4 ]> ], 
  [ <bipartition: [ 1, -3 ], [ 2, -2 ], [ 3, -5 ], [ 4, -7 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -1 ], [ -4 ], [ -6 ]> ], 
  [ <bipartition: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], [ 4, -4 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -5 ], [ -6 ], [ -7 ]>, 
      <bipartition: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], [ 4, -4 ], [ 5, -5 ], 
         [ 6, -6 ], [ 7 ], [ -7 ]>, 
      <bipartition: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], [ 4, -4 ], [ 5, -5 ], 
         [ 6 ], [ 7, -7 ], [ -6 ]>, 
      <bipartition: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], [ 4, -4 ], [ 5, -5 ], 
         [ 6 ], [ 7 ], [ -6 ], [ -7 ]>, 
      <bipartition: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], [ 4, -4 ], [ 5 ], 
         [ 6, -6 ], [ 7 ], [ -5 ], [ -7 ]> ], 
  [ <bipartition: [ 1, -5 ], [ 2, -2 ], [ 3, -1 ], [ 4, -3 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -4 ], [ -6 ], [ -7 ]>, 
      <bipartition: [ 1, -5 ], [ 2, -2 ], [ 3, -1 ], [ 4, -3 ], [ 5, -7 ], 
         [ 6, -4 ], [ 7 ], [ -6 ]>, 
      <bipartition: [ 1, -5 ], [ 2, -2 ], [ 3, -1 ], [ 4, -3 ], [ 5 ], 
         [ 6, -4 ], [ 7 ], [ -6 ], [ -7 ]>, 
      <bipartition: [ 1, -5 ], [ 2, -2 ], [ 3, -1 ], [ 4, -3 ], [ 5, -7 ], 
         [ 6 ], [ 7 ], [ -4 ], [ -6 ]> ], 
  [ <bipartition: [ 1, -3 ], [ 2, -5 ], [ 3, -2 ], [ 4, -4 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -1 ], [ -6 ], [ -7 ]> ], 
  [ <bipartition: [ 1, -4 ], [ 2, -2 ], [ 3, -1 ], [ 4, -5 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -3 ], [ -6 ], [ -7 ]> ], 
  [ <bipartition: [ 1, -5 ], [ 2, -7 ], [ 3, -1 ], [ 4, -3 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -2 ], [ -4 ], [ -6 ]> ], 
  [ <bipartition: [ 1, -7 ], [ 2, -3 ], [ 3, -4 ], [ 4, -1 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -2 ], [ -5 ], [ -6 ]> ], 
  [ <bipartition: [ 1, -1 ], [ 2, -7 ], [ 3, -2 ], [ 4, -3 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -4 ], [ -5 ], [ -6 ]> ], 
  [ <bipartition: [ 1, -3 ], [ 2, -2 ], [ 3, -4 ], [ 4, -6 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -1 ], [ -5 ], [ -7 ]>, 
      <bipartition: [ 1, -3 ], [ 2, -2 ], [ 3, -4 ], [ 4, -6 ], [ 5, -1 ], 
         [ 6 ], [ 7, -5 ], [ -7 ]>, 
      <bipartition: [ 1, -3 ], [ 2, -2 ], [ 3, -4 ], [ 4, -6 ], [ 5, -1 ], 
         [ 6 ], [ 7 ], [ -5 ], [ -7 ]> ], 
  [ <bipartition: [ 1, -7 ], [ 2, -2 ], [ 3, -5 ], [ 4, -1 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -3 ], [ -4 ], [ -6 ]>, 
      <bipartition: [ 1, -7 ], [ 2, -2 ], [ 3, -5 ], [ 4, -1 ], [ 5 ], 
         [ 6, -3 ], [ 7 ], [ -4 ], [ -6 ]> ], 
  [ <bipartition: [ 1, -1 ], [ 2, -5 ], [ 3, -3 ], [ 4, -4 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -2 ], [ -6 ], [ -7 ]> ], 
  [ <bipartition: [ 1, -6 ], [ 2, -2 ], [ 3, -3 ], [ 4, -1 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -4 ], [ -5 ], [ -7 ]> ], 
  [ <bipartition: [ 1, -3 ], [ 2, -1 ], [ 3, -4 ], [ 4, -6 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -2 ], [ -5 ], [ -7 ]> ], 
  [ <bipartition: [ 1, -7 ], [ 2, -6 ], [ 3, -1 ], [ 4, -2 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -3 ], [ -4 ], [ -5 ]> ], 
  [ <bipartition: [ 1, -5 ], [ 2, -4 ], [ 3, -6 ], [ 4, -3 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -1 ], [ -2 ], [ -7 ]> ] ]
gap> I2 := SemigroupIdeal(S, PartialPerm( [ 1, 2, 3, 4, 5, 6 ] ));
<inverse partial perm semigroup ideal on 7 pts with 1 generator>
gap> RightCosetsOfInverseSemigroup(I2, W);
[ [ [3,2,1,4,6] ], [ [2,7][3,1,5][4,6] ], [ [1,3,5][4,7](2) ], 
  [ [2,3,4,1,7] ], 
  [ <identity partial perm on [ 1, 2, 3, 4 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 5 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 5, 6 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 5, 7 ]> ], [ [4,3,2,7](1) ], 
  [ [1,3,2,5](4) ], [ [3,1,4,5](2) ], 
  [ [4,3,1,5](2), [4,3,1,5,7](2), [6,4,3,1,5](2), [6,4,3,1,5,7](2) ], 
  [ [1,5][2,4,3,6] ], [ [4,1,6](2)(3) ], 
  [ [1,3,4,6](2), [5,1,3,4,6](2), [7,5,1,3,4,6](2) ], [ [2,5](1)(3)(4) ], 
  [ [3,5][4,1,7](2), [4,1,7][6,3,5](2) ], [ [2,1,3,4,6] ], [ [2,7][4,3,1,5] ],
  [ [3,1,7][4,2,6] ] ]
gap> C := Bipartition(
> [ [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ], [ 3, -3 ] ] );;
gap> m := MajorantClosure(B, [ C ]);;
gap> V := InverseSemigroup(m);
<inverse bipartition semigroup on 7 pts with 5 generators>
gap> IsMajorantlyClosed(B, V);
true
gap> RightCosetsOfInverseSemigroup(B, V);
[ [ <block bijection: [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ], [ 3, -3 ]>
        , <block bijection: [ 1, 2, 5, 7, -1, -2, -5, -7 ], [ 3, -3 ], 
         [ 4, -4 ], [ 6, -6 ]>, 
      <block bijection: [ 1, 2, 5, 7, -4, -5, -6, -7 ], [ 3, -3 ], [ 4, -2 ], 
         [ 6, -1 ]>, <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], 
         [ 4, 5, 6, 7, -4, -5, -6, -7 ]>, 
      <block bijection: [ 1, -6 ], [ 2, -4 ], [ 3, -3 ], 
         [ 4, 5, 6, 7, -1, -2, -5, -7 ]> ] ]
gap> gens := [
> Bipartition(
>  [ [ 1, 2, 5, 7, -1, -2, -5, -7 ], [ 3, -3 ], [ 4, -4 ], [ 6, -6 ] ] ),
> Bipartition(
>  [ [ 1, -1 ], [ 2, 3, 7, -2, -3, -7 ], [ 4, -4 ], [ 5, -5 ], [ 6, -6 ] ] ), 
> Bipartition(
>  [ [ 1, -1 ], [ 2, -2 ], [ 3, 4, 7, -3, -4, -7 ], [ 5, -5 ], [ 6, -6 ] ] )];;
gap> J2 := SemigroupIdeal(B, gens);
<inverse bipartition semigroup ideal on 7 pts with 3 generators>
gap> RightCosetsOfInverseSemigroup(J2, V);
[ [ <block bijection: [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ], [ 3, -3 ]>
        , <block bijection: [ 1, 2, 5, 7, -1, -2, -5, -7 ], [ 3, -3 ], 
         [ 4, -4 ], [ 6, -6 ]>, 
      <block bijection: [ 1, 2, 5, 7, -4, -5, -6, -7 ], [ 3, -3 ], [ 4, -2 ], 
         [ 6, -1 ]>, <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], 
         [ 4, 5, 6, 7, -4, -5, -6, -7 ]>, 
      <block bijection: [ 1, -6 ], [ 2, -4 ], [ 3, -3 ], 
         [ 4, 5, 6, 7, -1, -2, -5, -7 ]> ] ]

#T# AttributesInverseTest7: SameMinorantsSubgroup
# (trivial examples)
gap> S := InverseSemigroup([
>  PartialPerm( [ 1, 2, 3, 4 ], [ 4, 1, 2, 6 ] ),
>  PartialPerm( [ 1, 2, 3, 4 ], [ 5, 7, 1, 6 ] ),
>  PartialPerm( [ 1, 2, 3, 5 ], [ 5, 2, 7, 3 ] ),
>  PartialPerm( [ 1, 2, 3, 6, 7 ], [ 1, 3, 4, 7, 5 ] ),
>  PartialPerm( [ 1, 2, 3, 4, 5, 7 ], [ 3, 2, 4, 6, 1, 5 ] ) ]);;
gap> iso := IsomorphismBipartitionSemigroup(S);;
gap> A := Range(iso);;
gap> B := InverseSemigroup([
> Bipartition([ [ 1, -6 ],[ 2, -4 ],[ 3, -3 ],[ 4, 5, 6, 7, -1, -2, -5, -7 ] ]),
> Bipartition([[ 1, -4 ],[ 2, -5 ],[ 3, 6, 7, -2, -3, -7 ],[ 4, -1 ],[ 5, -6]]),
> Bipartition([[ 1, -6 ],[ 2, -5 ],[ 3, 5, 7, -3, -4, -7 ],[ 4, -2 ],[ 6, -1]]) 
> ]);;
gap> J := SemigroupIdeal(B,
>  Bipartition([ [ 1, -1 ],[ 2, 3, 5, 7, -2, -3, -5, -7 ], [ 4, -4 ],
>    [ 6, -6 ] ]),
>  Bipartition([ [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ],
>    [ 3, -3 ] ]));;
gap> h := GroupHClass(DClasses(S)[4]);
{PartialPerm( [ 1, 4, 6 ], [ 1, 4, 6 ] )}
gap> SameMinorantsSubgroup(h);
[ <identity partial perm on [ 1, 4, 6 ]> ]
gap> h := GroupHClass(DClasses(A)[4]);
{Bipartition( [ [ 1, -1 ], [ 2 ], [ 3 ], [ 4, -4 ], [ 5 ], [ 6, -6 ], [ 7 ], 
 [ -2 ], [ -3 ], [ -5 ], [ -7 ] ] )}
gap> SameMinorantsSubgroup(h);
[ <bipartition: [ 1, -1 ], [ 2 ], [ 3 ], [ 4, -4 ], [ 5 ], [ 6, -6 ], [ 7 ], 
     [ -2 ], [ -3 ], [ -5 ], [ -7 ]> ]
gap> h := GroupHClass(DClasses(B)[6]);
{Bipartition( [ [ 1, -1 ], [ 2, -2 ], [ 3, 4, 5, 6, 7, -3, -4, -5, -6, -7 ] 
  ] )}
gap> SameMinorantsSubgroup(h);
[ <block bijection: [ 1, -1 ], [ 2, -2 ], 
     [ 3, 4, 5, 6, 7, -3, -4, -5, -6, -7 ]> ]
gap> h := GroupHClass(DClasses(J)[4]);;
gap> SameMinorantsSubgroup(h);
[ <block bijection: [ 1, -1 ], [ 2, 3, 5, 6, 7, -2, -3, -5, -6, -7 ], 
     [ 4, -4 ]> ]

#T# AttributesInverseTest8: SameMinorantsSubgroup 
# (non-trivial examples)
gap> f := PartialPermNC( [ 2, 1, 4, 5, 3, 7, 6, 9, 10, 8 ] );;
gap> g := PartialPermNC([ 2, 1, 0, 0, 0, 7, 6 ]);;
gap> S := InverseSemigroup(f, g);;
gap> T := SemigroupIdeal(S, PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] ));;
gap> h1 := GroupHClass(DClasses(S)[1]);
{PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], 
 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] )}
gap> m1 := SameMinorantsSubgroup(h1);
[ <identity partial perm on [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]>, 
  (1)(2)(3,4,5)(6)(7)(8,9,10), (1)(2)(3,5,4)(6)(7)(8,10,9) ]
gap> h2 := GroupHClass(DClasses(S)[2]);
{PartialPerm( [ 1, 2, 6, 7 ], [ 1, 2, 6, 7 ] )}
gap> m2 := SameMinorantsSubgroup(h2);
[ <identity partial perm on [ 1, 2, 6, 7 ]>, (1,2)(6,7) ]
gap> h1 := GroupHClass(DClasses(T)[1]);;
gap> SameMinorantsSubgroup(h1) = m1;
true
gap> h2 := GroupHClass(DClasses(T)[2]);;
gap> SameMinorantsSubgroup(h2) = m2;
true

#T# AttributesInverseTest9: NaturalLeqInverseSemigroup
# for partial perms
gap> a := PartialPerm( [ 1 ], [ 6 ] );;
gap> b := PartialPerm( [ 1, 2, 5 ], [ 6, 3, 1 ] );;
gap> NaturalLeqInverseSemigroup(a, b);
true
gap> NaturalLeqInverseSemigroup(b, a);
false
gap> NaturalLeqInverseSemigroup(a, a);
true

#T# AttributesInverseTest10: NaturalLeqInverseSemigroup
# for block bijections
gap> A := Bipartition( [ [ 1, 2, 3, 4, 6, 7, 8, -1, -2, -4, -5, -6, -7, -8 ],
> [ 5, -3 ] ] );;
gap> B := Bipartition( [ [ 1, 2, 3, 4, 8, -4, -5, -6, -7, -8 ], [ 5, -3 ],
> [ 6, -2 ], [ 7, -1 ] ] );;
gap> NaturalLeqInverseSemigroup(A, B);
true
gap> NaturalLeqInverseSemigroup(B, A);
false
gap> NaturalLeqInverseSemigroup(B, B);
true

#T# AttributesInverseTest11: NaturalLeqInverseSemigroup
# for partial perm bipartitions
gap> f := Bipartition( [ [ 1, -2 ], [ 2 ], [ -1 ] ] );;
gap> f2 := Bipartition( [ [ 1, -2 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 6 ], [ -1 ],
> [ -3 ], [-4 ], [ -5 ], [ -6 ] ] );;
gap> g := Bipartition( [ [ 1, -2 ], [ 2 ], [ 3, -5 ], [ 4 ], [ 5 ], [ -1 ],
> [ -3 ], [ -4 ] ] );;
gap> NaturalLeqInverseSemigroup(f, g);
true
gap> NaturalLeqInverseSemigroup(f2, g);
true
gap> AsPartialPerm(f) = AsPartialPerm(f2);
true
gap> f = f2;
false
gap> NaturalLeqInverseSemigroup(f, f2);
true
gap> NaturalLeqInverseSemigroup(f2, f);
true
gap> NaturalLeqInverseSemigroup(g, g);
true
gap> NaturalLeqInverseSemigroup(g, f);
false
gap> NaturalLeqInverseSemigroup(g, f2);
false

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(f2);
gap> Unbind(h2);
gap> Unbind(h1);
gap> Unbind(ww);
gap> Unbind(WW);
gap> Unbind(xx);
gap> Unbind(zz);
gap> Unbind(I2);
gap> Unbind(m1);
gap> Unbind(m2);
gap> Unbind(A);
gap> Unbind(C);
gap> Unbind(B);
gap> Unbind(I);
gap> Unbind(J);
gap> Unbind(Q);
gap> Unbind(P);
gap> Unbind(S);
gap> Unbind(R);
gap> Unbind(U);
gap> Unbind(T);
gap> Unbind(W);
gap> Unbind(V);
gap> Unbind(a);
gap> Unbind(yy);
gap> Unbind(b);
gap> Unbind(g);
gap> Unbind(f);
gap> Unbind(h);
gap> Unbind(m);
gap> Unbind(gens);
gap> Unbind(J2);
gap> Unbind(iso);
gap> Unbind(t);
gap> Unbind(w);
gap> Unbind(y);
gap> Unbind(x);
gap> Unbind(z);

#E#
gap> STOP_TEST("Semigroups package: attributes-inverse.tst");
