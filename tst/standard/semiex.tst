#############################################################################
##
#W  standard/semiex.tst
#Y  Copyright (C) 2016                                  James D. Mitchell
##                                                       
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semiex.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Test EndomorphismsPartition 1
gap> EndomorphismsPartition([-1]);
Error, Semigroups: EndomorphismsPartition: usage,
the argument <partition> must be a list of positive integers,
gap> EndomorphismsPartition([1,1,1]);
<full transformation monoid of degree 3>
gap> EndomorphismsPartition([5]);
<full transformation monoid of degree 5>
gap> part := [2, 1, 3];
[ 2, 1, 3 ]
gap> EndomorphismsPartition(part);
<transformation semigroup of degree 6 with 7 generators>
gap> part;
[ 2, 1, 3 ]

# Test EndomorphismsPartition 2
gap> List(Partitions(11), EndomorphismsPartition);
[ <full transformation monoid of degree 11>, 
  <transformation semigroup of degree 11 with 6 generators>, 
  <transformation semigroup of degree 11 with 8 generators>, 
  <transformation semigroup of degree 11 with 8 generators>, 
  <transformation semigroup of degree 11 with 8 generators>, 
  <transformation semigroup of degree 11 with 5 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 10 generators>, 
  <transformation semigroup of degree 11 with 12 generators>, 
  <transformation semigroup of degree 11 with 12 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 9 generators>, 
  <transformation semigroup of degree 11 with 12 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 9 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 13 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 15 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 9 generators>, 
  <transformation semigroup of degree 11 with 10 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 13 generators>, 
  <transformation semigroup of degree 11 with 8 generators>, 
  <transformation semigroup of degree 11 with 12 generators>, 
  <transformation semigroup of degree 11 with 13 generators>, 
  <transformation semigroup of degree 11 with 8 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 10 generators>, 
  <transformation semigroup of degree 11 with 6 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 12 generators>, 
  <transformation semigroup of degree 11 with 10 generators>, 
  <transformation semigroup of degree 11 with 10 generators>, 
  <transformation semigroup of degree 11 with 5 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 8 generators>, 
  <transformation semigroup of degree 11 with 10 generators>, 
  <transformation semigroup of degree 11 with 6 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 8 generators>, 
  <transformation semigroup of degree 11 with 6 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 6 generators>, 
  <transformation semigroup of degree 11 with 5 generators>, 
  <full transformation monoid of degree 11> ]

#T# ExamplesTest37: ham-examples
# planar uniform block bijection monoid
gap> S := PlanarUniformBlockBijectionMonoid(1);
<trivial bipartition group of degree 1 with 1 generator>
gap> Size(S);
1
gap> Size(Generators(S));
1
gap> NrHClasses(S);
1
gap> NrRClasses(S);
1
gap> NrDClasses(S);
1
gap> NrIdempotents(S);
1
gap> IsBlockBijectionMonoid(S);
true
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
true
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := PlanarUniformBlockBijectionMonoid(4);
<inverse bipartition monoid of degree 4 with 3 generators>
gap> Size(S);
8
gap> Size(Generators(S));
3
gap> NrHClasses(S);
8
gap> NrRClasses(S);
8
gap> NrDClasses(S);
8
gap> NrIdempotents(S);
8
gap> IsBlockBijectionMonoid(S);
true
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]

# singular planar uniform block bijection monoid
gap> S := SingularPlanarUniformBlockBijectionMonoid(4);
<inverse bipartition semigroup ideal of degree 4 with 1 generator>
gap> Size(S);
4
gap> Size(Generators(S));
1
gap> NrHClasses(S);
4
gap> NrRClasses(S);
4
gap> NrDClasses(S);
4
gap> NrIdempotents(S);
4
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> SingularPlanarUniformBlockBijectionMonoid(1);
Error, Semigroups: SingularPlanarUniformBlockBijectionMonoid: usage,
the argument must be greater than 1,

# uniform block bijection monoid
gap> S := UniformBlockBijectionMonoid(1);
<trivial bipartition group of degree 1 with 1 generator>
gap> Size(S);
1
gap> Size(Generators(S));
1
gap> NrHClasses(S);
1
gap> NrRClasses(S);
1
gap> NrDClasses(S);
1
gap> NrIdempotents(S);
1
gap> IsBlockBijectionMonoid(S);
true
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
true
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := UniformBlockBijectionMonoid(4);
<inverse bipartition monoid of degree 4 with 3 generators>
gap> Size(S);
131
gap> Size(Generators(S));
3
gap> NrHClasses(S);
63
gap> NrRClasses(S);
15
gap> NrDClasses(S);
5
gap> NrIdempotents(S);
15
gap> IsBlockBijectionMonoid(S);
true
gap> IsHTrivial(S);
false
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1", "C2", "S4" ]

# singular uniform block bijection monoid
gap> S := SingularUniformBlockBijectionMonoid(4);
<inverse bipartition semigroup ideal of degree 4 with 1 generator>
gap> Size(S);
107
gap> Size(Generators(S));
1
gap> NrHClasses(S);
62
gap> NrRClasses(S);
14
gap> NrDClasses(S);
4
gap> NrIdempotents(S);
14
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
false
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1", "C2" ]
gap> SingularUniformBlockBijectionMonoid(1);
Error, Semigroups: SingularUniformBlockBijectionMonoid: usage,
the argument must be greater than 1,

# apsis monoid
gap> S := ApsisMonoid(1, 1);
<commutative inverse bipartition monoid of degree 1 with 1 generator>
gap> Size(S);
2
gap> Size(Generators(S));
1
gap> NrHClasses(S);
2
gap> NrRClasses(S);
2
gap> NrDClasses(S);
2
gap> NrIdempotents(S);
2
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := ApsisMonoid(2, 1);
<trivial bipartition group of degree 1 with 1 generator>
gap> Size(S);
1
gap> Size(Generators(S));
1
gap> NrHClasses(S);
1
gap> NrRClasses(S);
1
gap> NrDClasses(S);
1
gap> NrIdempotents(S);
1
gap> IsBlockBijectionMonoid(S);
true
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
true
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := ApsisMonoid(3, 2);
<trivial bipartition group of degree 2 with 1 generator>
gap> Size(S);
1
gap> Size(Generators(S));
1
gap> NrHClasses(S);
1
gap> NrRClasses(S);
1
gap> NrDClasses(S);
1
gap> NrIdempotents(S);
1
gap> IsBlockBijectionMonoid(S);
true
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
true
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := ApsisMonoid(3, 4);
<regular bipartition monoid of degree 4 with 2 generators>
gap> Size(S);
5
gap> Size(Generators(S));
2
gap> NrHClasses(S);
5
gap> NrRClasses(S);
3
gap> NrDClasses(S);
2
gap> NrIdempotents(S);
5
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := ApsisMonoid(5, 4);
<trivial bipartition group of degree 4 with 1 generator>
gap> Size(S);
1
gap> Size(Generators(S));
1
gap> NrHClasses(S);
1
gap> NrRClasses(S);
1
gap> NrDClasses(S);
1
gap> NrIdempotents(S);
1
gap> IsBlockBijectionMonoid(S);
true
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
true
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]

# singular apsis monoid
gap> S := SingularApsisMonoid(1, 1);
<commutative inverse bipartition semigroup ideal of degree 1 with 1 generator>
gap> Size(S);
1
gap> Size(Generators(S));
1
gap> NrHClasses(S);
1
gap> NrRClasses(S);
1
gap> NrDClasses(S);
1
gap> NrIdempotents(S);
1
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
true
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := SingularApsisMonoid(2, 4);
<regular bipartition semigroup ideal of degree 4 with 1 generator>
gap> Size(S);
13
gap> Size(Generators(S));
1
gap> NrHClasses(S);
13
gap> NrRClasses(S);
5
gap> NrDClasses(S);
2
gap> NrIdempotents(S);
11
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := SingularApsisMonoid(3, 4);
<regular bipartition semigroup ideal of degree 4 with 1 generator>
gap> Size(S);
4
gap> Size(Generators(S));
1
gap> NrHClasses(S);
4
gap> NrRClasses(S);
2
gap> NrDClasses(S);
1
gap> NrIdempotents(S);
4
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
true
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> SingularApsisMonoid(2, 1);
Error, Semigroups: SingularApsisMonoid: usage,
the first argument must be less than or equal to the second argument,

# crossed apsis monoid
gap> S := CrossedApsisMonoid(1, 1);
<commutative inverse bipartition monoid of degree 1 with 1 generator>
gap> Size(S);
2
gap> Size(Generators(S));
1
gap> NrHClasses(S);
2
gap> NrRClasses(S);
2
gap> NrDClasses(S);
2
gap> NrIdempotents(S);
2
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := CrossedApsisMonoid(2, 1);
<trivial bipartition group of degree 1 with 1 generator>
gap> Size(S);
1
gap> Size(Generators(S));
1
gap> NrHClasses(S);
1
gap> NrRClasses(S);
1
gap> NrDClasses(S);
1
gap> NrIdempotents(S);
1
gap> IsBlockBijectionMonoid(S);
true
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
true
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := CrossedApsisMonoid(2, 4);
<regular bipartition monoid of degree 4 with 3 generators>
gap> Size(S);
105
gap> Size(Generators(S));
3
gap> NrHClasses(S);
46
gap> NrRClasses(S);
10
gap> NrDClasses(S);
3
gap> NrIdempotents(S);
40
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
false
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1", "C2", "S4" ]
gap> S := CrossedApsisMonoid(3, 4);
<regular bipartition monoid of degree 4 with 3 generators>
gap> Size(S);
40
gap> Size(Generators(S));
3
gap> NrHClasses(S);
17
gap> NrRClasses(S);
5
gap> NrDClasses(S);
2
gap> NrIdempotents(S);
17
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
false
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1", "S4" ]
gap> S := CrossedApsisMonoid(5, 4);
<regular bipartition monoid of degree 4 with 2 generators>
gap> Size(S);
24
gap> Size(Generators(S));
2
gap> NrHClasses(S);
1
gap> NrRClasses(S);
1
gap> NrDClasses(S);
1
gap> NrIdempotents(S);
1
gap> IsBlockBijectionMonoid(S);
true
gap> IsHTrivial(S);
false
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
true
gap> StructureDescriptionMaximalSubgroups(S);
[ "S4" ]

# singular crossed apsis monoid
gap> S := SingularCrossedApsisMonoid(1, 1);
<commutative inverse bipartition semigroup ideal of degree 1 with 1 generator>
gap> Size(S);
1
gap> Size(Generators(S));
1
gap> NrHClasses(S);
1
gap> NrRClasses(S);
1
gap> NrDClasses(S);
1
gap> NrIdempotents(S);
1
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
true
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := SingularCrossedApsisMonoid(2, 4);
<regular bipartition semigroup ideal of degree 4 with 1 generator>
gap> Size(S);
81
gap> Size(Generators(S));
1
gap> NrHClasses(S);
45
gap> NrRClasses(S);
9
gap> NrDClasses(S);
2
gap> NrIdempotents(S);
39
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
false
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1", "C2" ]
gap> S := SingularCrossedApsisMonoid(3, 4);
<regular bipartition semigroup ideal of degree 4 with 1 generator>
gap> Size(S);
16
gap> Size(Generators(S));
1
gap> NrHClasses(S);
16
gap> NrRClasses(S);
4
gap> NrDClasses(S);
1
gap> NrIdempotents(S);
16
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
true
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> SingularCrossedApsisMonoid(2, 1);
Error, Semigroups: SingularCrossedApsisMonoid: usage,
the first argument must be less than or equal to the second argument,

# planar modular partition monoid
gap> S := PlanarModularPartitionMonoid(1, 1);
<commutative inverse bipartition monoid of degree 1 with 1 generator>
gap> Size(S);
2
gap> Size(Generators(S));
1
gap> NrHClasses(S);
2
gap> NrRClasses(S);
2
gap> NrDClasses(S);
2
gap> NrIdempotents(S);
2
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := PlanarModularPartitionMonoid(2, 1);
<trivial bipartition group of degree 1 with 1 generator>
gap> Size(S);
1
gap> Size(Generators(S));
1
gap> NrHClasses(S);
1
gap> NrRClasses(S);
1
gap> NrDClasses(S);
1
gap> NrIdempotents(S);
1
gap> IsBlockBijectionMonoid(S);
true
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
true
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := PlanarModularPartitionMonoid(2, 4);
<regular bipartition monoid of degree 4 with 6 generators>
gap> Size(S);
55
gap> Size(Generators(S));
6
gap> NrHClasses(S);
55
gap> NrRClasses(S);
17
gap> NrDClasses(S);
8
gap> NrIdempotents(S);
45
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := PlanarModularPartitionMonoid(3, 4);
<regular bipartition monoid of degree 4 with 5 generators>
gap> Size(S);
16
gap> Size(Generators(S));
5
gap> NrHClasses(S);
16
gap> NrRClasses(S);
10
gap> NrDClasses(S);
8
gap> NrIdempotents(S);
16
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := PlanarModularPartitionMonoid(5, 4);
<inverse bipartition monoid of degree 4 with 3 generators>
gap> Size(S);
8
gap> Size(Generators(S));
3
gap> NrHClasses(S);
8
gap> NrRClasses(S);
8
gap> NrDClasses(S);
8
gap> NrIdempotents(S);
8
gap> IsBlockBijectionMonoid(S);
true
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]

# singular planar modular partition monoid
gap> S := SingularPlanarModularPartitionMonoid(1, 1);
<commutative inverse bipartition semigroup ideal of degree 1 with 1 generator>
gap> Size(S);
1
gap> Size(Generators(S));
1
gap> NrHClasses(S);
1
gap> NrRClasses(S);
1
gap> NrDClasses(S);
1
gap> NrIdempotents(S);
1
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
true
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := SingularPlanarModularPartitionMonoid(2, 4);
<regular bipartition semigroup ideal of degree 4 with 1 generator>
gap> Size(S);
52
gap> Size(Generators(S));
1
gap> NrHClasses(S);
52
gap> NrRClasses(S);
14
gap> NrDClasses(S);
5
gap> NrIdempotents(S);
42
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := SingularPlanarModularPartitionMonoid(3, 4);
<regular bipartition semigroup ideal of degree 4 with 1 generator>
gap> Size(S);
12
gap> Size(Generators(S));
1
gap> NrHClasses(S);
12
gap> NrRClasses(S);
6
gap> NrDClasses(S);
4
gap> NrIdempotents(S);
12
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := SingularPlanarModularPartitionMonoid(5, 4);
<inverse bipartition semigroup ideal of degree 4 with 1 generator>
gap> Size(S);
4
gap> Size(Generators(S));
1
gap> NrHClasses(S);
4
gap> NrRClasses(S);
4
gap> NrDClasses(S);
4
gap> NrIdempotents(S);
4
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> SingularPlanarModularPartitionMonoid(2, 1);
Error, Semigroups: SingularPlanarModularPartitionMonoid: usage,
the second argument must be greater than 1 when the first argument is also gre\
ater than 1,

# planar partition monoid
gap> S := PlanarPartitionMonoid(1);
<commutative inverse bipartition monoid of degree 1 with 1 generator>
gap> Size(S);
2
gap> Size(Generators(S));
1
gap> NrHClasses(S);
2
gap> NrRClasses(S);
2
gap> NrDClasses(S);
2
gap> NrIdempotents(S);
2
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := PlanarPartitionMonoid(4);
<regular bipartition monoid of degree 4 with 7 generators>
gap> Size(S);
1430
gap> Size(Generators(S));
7
gap> NrHClasses(S);
1430
gap> NrRClasses(S);
70
gap> NrDClasses(S);
5
gap> NrIdempotents(S);
886
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]

# singular planar partition monoid
gap> S := SingularPlanarPartitionMonoid(1);
<commutative inverse bipartition semigroup ideal of degree 1 with 1 generator>
gap> Size(S);
1
gap> Size(Generators(S));
1
gap> NrHClasses(S);
1
gap> NrRClasses(S);
1
gap> NrDClasses(S);
1
gap> NrIdempotents(S);
1
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
true
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := SingularPlanarPartitionMonoid(4);
<regular bipartition semigroup ideal of degree 4 with 1 generator>
gap> Size(S);
1429
gap> Size(Generators(S));
1
gap> NrHClasses(S);
1429
gap> NrRClasses(S);
69
gap> NrDClasses(S);
4
gap> NrIdempotents(S);
885
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]

# modular partition monoid
gap> S := ModularPartitionMonoid(1, 1);
<commutative inverse bipartition monoid of degree 1 with 1 generator>
gap> Size(S);
2
gap> Size(Generators(S));
1
gap> NrHClasses(S);
2
gap> NrRClasses(S);
2
gap> NrDClasses(S);
2
gap> NrIdempotents(S);
2
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := ModularPartitionMonoid(2, 1);
<commutative inverse bipartition monoid of degree 1 with 1 generator>
gap> Size(S);
2
gap> Size(Generators(S));
1
gap> NrHClasses(S);
2
gap> NrRClasses(S);
2
gap> NrDClasses(S);
2
gap> NrIdempotents(S);
2
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
true
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1" ]
gap> S := ModularPartitionMonoid(2, 4);
<regular bipartition monoid of degree 4 with 4 generators>
gap> Size(S);
379
gap> Size(Generators(S));
4
gap> NrHClasses(S);
211
gap> NrRClasses(S);
31
gap> NrDClasses(S);
6
gap> NrIdempotents(S);
127
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
false
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1", "C2", "S4" ]
gap> S := ModularPartitionMonoid(3, 4);
<regular bipartition monoid of degree 4 with 4 generators>
gap> Size(S);
155
gap> Size(Generators(S));
4
gap> NrHClasses(S);
87
gap> NrRClasses(S);
19
gap> NrDClasses(S);
5
gap> NrIdempotents(S);
39
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
false
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1", "C2", "S4" ]
gap> S := ModularPartitionMonoid(5, 4);
<regular bipartition monoid of degree 4 with 3 generators>
gap> Size(S);
131
gap> Size(Generators(S));
3
gap> NrHClasses(S);
63
gap> NrRClasses(S);
15
gap> NrDClasses(S);
5
gap> NrIdempotents(S);
15
gap> IsBlockBijectionMonoid(S);
true
gap> IsHTrivial(S);
false
gap> IsInverseMonoid(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1", "C2", "S4" ]

# singular modular partition monoid
gap> S := SingularModularPartitionMonoid(2, 4);
<regular bipartition semigroup ideal of degree 4 with 1 generator>
gap> Size(S);
355
gap> Size(Generators(S));
1
gap> NrHClasses(S);
210
gap> NrRClasses(S);
30
gap> NrDClasses(S);
5
gap> NrIdempotents(S);
126
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
false
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1", "C2" ]
gap> S := SingularModularPartitionMonoid(3, 4);
<regular bipartition semigroup ideal of degree 4 with 1 generator>
gap> Size(S);
131
gap> Size(Generators(S));
1
gap> NrHClasses(S);
86
gap> NrRClasses(S);
18
gap> NrDClasses(S);
4
gap> NrIdempotents(S);
38
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
false
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1", "C2" ]
gap> S := SingularModularPartitionMonoid(5, 4);
<regular bipartition semigroup ideal of degree 4 with 1 generator>
gap> Size(S);
107
gap> Size(Generators(S));
1
gap> NrHClasses(S);
62
gap> NrRClasses(S);
14
gap> NrDClasses(S);
4
gap> NrIdempotents(S);
14
gap> IsBlockBijectionMonoid(S);
false
gap> IsHTrivial(S);
false
gap> IsInverseMonoid(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroup(S);
false
gap> StructureDescriptionMaximalSubgroups(S);
[ "1", "C2" ]
gap> SingularModularPartitionMonoid(1, 1);
<commutative inverse bipartition semigroup ideal of degree 1 with 1 generator>
gap> SingularModularPartitionMonoid(2, 1);
Error, Semigroups: SingularModularPartitionMonoid: usage,
the second argument must be greater than 1 when the first argument is also gre\
ater than 1,

#E# 
gap> STOP_TEST("Semigroups package: standard/semiex.tst");
