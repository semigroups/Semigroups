############################################################################
##
#W  partbinrel.gi
#Y  Copyright (C) 2015                                   Attila Egri-Nagy
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## Partitioned Binary Relations
## Binary relations on 2n points with a different multiplication
## as described in: MARTIN, Paul; MAZORCHUK, Volodymyr.
## Partitioned Binary Relations. MATHEMATICA SCANDINAVICA,v113, n1, p. 30-52, 
## http://arxiv.org/abs/1102.0862
##  

# input: a binary relation on 2n points
# the function decomposes the relation into 4 relations on n points
# Dom: 1..n
# Codom: n+1..2n
InstallMethod(PartitionedBinaryRelation, "for a binary relation on 2n points",
        [IsBinaryRelation],
function(binrel)
  local a11, a12, a21, a22, deg, half, successors,i;
  deg := DegreeOfBinaryRelation(binrel);
  if not IsEvenInt(deg) then return fail; fi;
  half := deg/2;
  a11 := List([1..half], x->[]); 
  a12 := List([1..half], x->[]); 
  a21 := List([1..half], x->[]); 
  a22 := List([1..half], x->[]); 
  #sorting relations
  successors := Successors(binrel);
  for i in [1..half] do
    a11[i] := Filtered(successors[i], x-> x <= half);
    a12[i] := List(Filtered(successors[i], x-> x > half), y->y-half);
  od;
  for i in [half+1..deg] do
    a21[i-half] := Filtered(successors[i], x-> x <= half);
    a22[i-half] := List(Filtered(successors[i], x-> x > half), y->y-half);  
  od;
  return PartitionedBinaryRelation(
                 BinaryRelationOnPoints(a11),
                 BinaryRelationOnPoints(a12),
                 BinaryRelationOnPoints(a21),
                 BinaryRelationOnPoints(a22));
end);

# in case you have the Dom-Dom, Dom-Codom, Codom-Dom, Codom-Codom relations
# nicely separated
InstallOtherMethod(PartitionedBinaryRelation, "for 4 binary relations on 2n points",
        [IsBinaryRelation,IsBinaryRelation,IsBinaryRelation,IsBinaryRelation],
function(a11, a12, a21, a22)
  return Objectify(PartitionedBinaryRelationType,
                 rec(a11:=a11,a12:=a12,a21:=a21,a22:=a22));    
end);

InstallGlobalFunction(CombinePartitionedBinaryRelations,
function(alpha, beta)
  local a11, a12, a21, a22,abs,bas, tmp;
  
  abs := AsSet(Monoid(alpha!.a22 * beta!.a11));
  bas := AsSet(Monoid(beta!.a11 * alpha!.a22));
  #a11
  a11 := alpha!.a11;
  tmp := List(abs, x->Product([alpha!.a12, beta!.a11, x, alpha!.a21]));
  Perform(tmp, function(x) a11 := UnionOfBinaryRelations(a11,x); end);
  
  #a12
  a12 := alpha!.a12 * beta!.a12;
  tmp := List(bas, x->Product([alpha!.a12, x, beta!.a12]));
  Perform(tmp, function(x) a12 := UnionOfBinaryRelations(a12,x); end);
  
  #a21
  a21 := beta!.a21 * alpha!.a21;
  tmp := List(abs, x->Product([beta!.a21, x, alpha!.a21]));
  Perform(tmp, function(x) a21 := UnionOfBinaryRelations(a21,x); end);
  
  #a22
  a22 := beta!.a22;
  tmp := List(bas, x->Product([beta!.a21, alpha!.a22, x, beta!.a12]));
  Perform(tmp, function(x) a22 := UnionOfBinaryRelations(a22,x); end);

  return PartitionedBinaryRelation(a11,a12,a21,a22);
end);

InstallGlobalFunction(DegreeOfPartitionedBinaryRelation,
        pbr->DegreeOfBinaryRelation(pbr!.a11));

InstallMethod(\*, "for partitioned binary relations",
        [IsPartitionedBinaryRelation, IsPartitionedBinaryRelation],
        CombinePartitionedBinaryRelations);

InstallMethod(\=, "for partitioned binary relations",
        [IsPartitionedBinaryRelation, IsPartitionedBinaryRelation],
function(p1, p2)
  return p1!.a11 = p2!.a11 and 
         p1!.a12 = p2!.a12 and 
         p1!.a21 = p2!.a21 and 
         p1!.a22 = p2!.a22;
  end);
  
  FlatPBR := function(pbr)
    return Concatenation([Successors(pbr!.a11),
                   Successors(pbr!.a12),
                   Successors(pbr!.a21),
                   Successors(pbr!.a22)]);
  end;
  
  InstallMethod(\<, "for partitioned binary relations",
        [IsPartitionedBinaryRelation, IsPartitionedBinaryRelation],
function(p1, p2)
    return FlatPBR(p1) < FlatPBR(p2);
  end);


InstallMethod(One, "for a partitioned binary relation",
[IsPartitionedBinaryRelation],
function(pbr)
  local half, tmp;
  half := DegreeOfBinaryRelation(pbr!.a11);
  tmp := [];
  Perform([1..half], function(x) tmp[x] := [half+x]; tmp[half+x]:=[x]; end);
  return PartitionedBinaryRelation(
                 BinaryRelationOnPoints(tmp));
end);


InstallMethod( ViewObj,"for partitioned binary relation",
        [IsPartitionedBinaryRelation and IsPartitionedBinaryRelationRep],
function(pbr)
  Print("a11: ", Successors(pbr!.a11), " ");
  Print("a12: ", Successors(pbr!.a12), " ");
  Print("a21: ", Successors(pbr!.a21), " ");
  Print("a22: ", Successors(pbr!.a22), " ");
end);

InstallMethod(Display,"for partitioned binary relation",
        [IsPartitionedBinaryRelation and IsPartitionedBinaryRelationRep],
function( pbr ) ViewObj(pbr); Print("\n"); end);

#fully calculated elements
PartitionedBinaryRelationMonoid := function(n)
  local binrels;
  binrels := List(Tuples(Combinations([1..n]),n),
                  x -> BinaryRelationOnPoints(x));
  return Semigroup(List(Tuples(binrels,4),
                 x-> PartitionedBinaryRelation(x[1],x[2],x[3],x[4])));
end;
