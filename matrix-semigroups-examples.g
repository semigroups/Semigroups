e := NewMatrix(IsPlistMatrixRep,GF(3),5,
[ [ Z(3)^0, 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ],
[ Z(3)^0, Z(3)^0, Z(3)^0, Z(3), 0*Z(3) ],
[ Z(3), 0*Z(3), 0*Z(3), Z(3)^0, 0*Z(3) ],
[ 0*Z(3), 0*Z(3), 0*Z(3), Z(3)^0, 0*Z(3) ],
[ Z(3)^0, 0*Z(3), Z(3)^0, Z(3), Z(3)^0 ] ]);

is an idempotent in the semigroup:

S :=
Semigroup(
[ NewSMatrix(IsPlistSMatrixRep,GF(3),5,[ [ Z(3), Z(3), Z(3)^0, Z(3), Z(3)^0 ],
[ 0*Z(3), 0*Z(3), Z(3), 0*Z(3), Z(3)^0 ],
[ Z(3), Z(3), Z(3)^0, 0*Z(3), Z(3) ],
[ Z(3)^0, Z(3)^0, Z(3), Z(3), 0*Z(3) ],
[ Z(3), Z(3)^0, Z(3), Z(3)^0, 0*Z(3) ] ]),
NewSMatrix(IsPlistSMatrixRep,GF(3),5,
[ [ 0*Z(3), Z(3)^0, 0*Z(3), Z(3), Z(3) ],
[ Z(3), 0*Z(3), Z(3)^0, 0*Z(3), Z(3)^0 ],
[ Z(3), Z(3), Z(3)^0, Z(3), Z(3) ], [ Z(3), Z(3), 0*Z(3), Z(3), Z(3)^0 ]
, [ Z(3), Z(3), Z(3)^0, Z(3)^0, Z(3) ] ]) ]);;

upper := function(mat)
  local zero, n, i, j;
  zero := Zero(BaseDomain(mat));
  n := Length(mat);
  for i in [2 .. n] do 
    for j in [1 .. i - 1] do 
      if mat[i][j] <> zero then 
        return false;
      fi;
    od;
  od;
  return true;
end;
S := GeneralLinearSemigroup(3,3);
T := SubsemigroupByProperty(S, upper);



S :=
Semigroup(
[ NewMatrix(IsPlistMatrixRep,GF(3),5,[ [ Z(3), Z(3), Z(3)^0, Z(3), Z(3)^0 ],
[ 0*Z(3), 0*Z(3), Z(3), 0*Z(3), Z(3)^0 ],
[ Z(3), Z(3), Z(3)^0, 0*Z(3), Z(3) ],
[ Z(3)^0, Z(3)^0, Z(3), Z(3), 0*Z(3) ],
[ Z(3), Z(3)^0, Z(3), Z(3)^0, 0*Z(3) ] ]),
NewMatrix(IsPlistMatrixRep,GF(3),5,
[ [ 0*Z(3), Z(3)^0, 0*Z(3), Z(3), Z(3) ],
[ Z(3), 0*Z(3), Z(3)^0, 0*Z(3), Z(3)^0 ],
[ Z(3), Z(3), Z(3)^0, Z(3), Z(3) ], [ Z(3), Z(3), 0*Z(3), Z(3), Z(3)^0 ]
, [ Z(3), Z(3), Z(3)^0, Z(3)^0, Z(3) ] ]) ]);;


gens := [ NewSMatrix(IsPlistSMatrixRep,GF(25),2,
    [ [ Z(5^2), Z(5^2)^13 ], [ 0*Z(5), Z(5^2)^14 ] ]),
  NewSMatrix(IsPlistSMatrixRep,GF(25),2,
    [ [ Z(5^2)^21, Z(5)^0 ], [ Z(5)^0, 0*Z(5) ] ]),
  NewSMatrix(IsPlistSMatrixRep,GF(25),2,
    [ [ Z(5^2)^23, Z(5^2)^5 ], [ Z(5^2)^20, Z(5^2)^20 ] ]) ];;

S2 := Semigroup(gens);
Size(S2);

m1 := NewSMatrix(IsPlistSMatrixRep, GF(3), 3, Z(3)*[[2,2,0],[2,2,0],[2,0,2]]);
m2 := NewSMatrix(IsPlistSMatrixRep, GF(3), 3, Z(3)*[[2,0,1],[0,2,0],[0,0,2]]);

S := Semigroup(m1,m2);
T := AsTransformationSemigroup(S);
