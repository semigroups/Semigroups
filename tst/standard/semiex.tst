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
<trivial block bijection group of degree 1 with 1 generator>
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
<inverse block bijection monoid of degree 4 with 3 generators>
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
<trivial block bijection group of degree 1 with 1 generator>
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
<inverse block bijection monoid of degree 4 with 3 generators>
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
<trivial block bijection group of degree 1 with 1 generator>
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
<trivial block bijection group of degree 2 with 1 generator>
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
<regular bipartition *-monoid of degree 4 with 2 generators>
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
<trivial block bijection group of degree 4 with 1 generator>
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
<regular bipartition *-semigroup ideal of degree 4 with 1 generator>
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
<regular bipartition *-semigroup ideal of degree 4 with 1 generator>
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
<trivial block bijection group of degree 1 with 1 generator>
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
<regular bipartition *-monoid of degree 4 with 3 generators>
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
<regular bipartition *-monoid of degree 4 with 3 generators>
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
<block bijection group of degree 4 with 2 generators>
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
<regular bipartition *-semigroup ideal of degree 4 with 1 generator>
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
<regular bipartition *-semigroup ideal of degree 4 with 1 generator>
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
<trivial block bijection group of degree 1 with 1 generator>
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
<regular bipartition *-monoid of degree 4 with 6 generators>
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
<regular bipartition *-monoid of degree 4 with 5 generators>
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
<inverse block bijection monoid of degree 4 with 3 generators>
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
<regular bipartition *-semigroup ideal of degree 4 with 1 generator>
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
<regular bipartition *-semigroup ideal of degree 4 with 1 generator>
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
<regular bipartition *-monoid of degree 4 with 7 generators>
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
<regular bipartition *-semigroup ideal of degree 4 with 1 generator>
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
<regular bipartition *-monoid of degree 4 with 4 generators>
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
<regular bipartition *-monoid of degree 4 with 4 generators>
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
<regular block bijection *-monoid of degree 4 with 3 generators>
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
<regular bipartition *-semigroup ideal of degree 4 with 1 generator>
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
<regular bipartition *-semigroup ideal of degree 4 with 1 generator>
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
<regular bipartition *-semigroup ideal of degree 4 with 1 generator>
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

#T# Catalan monoid
gap> S := CatalanMonoid(1);
<trivial transformation group of degree 0 with 1 generator>
gap> S := CatalanMonoid(2);
<commutative transformation monoid of degree 2 with 1 generator>
gap> Size(S);
2
gap> S := CatalanMonoid(3);
<transformation monoid of degree 3 with 2 generators>
gap> Size(S);
5
gap> S := CatalanMonoid(4);
<transformation monoid of degree 4 with 3 generators>
gap> Size(S);
14

#T# Example from Semigroupe manual, Section 5.18: Knast's counterexample
gap> S := Semigroup(Transformation([4, 4, 8, 8, 8, 8, 4, 8]), 
>                   Transformation([8, 2, 8, 2, 5, 5, 8, 8]),
>                   Transformation([8, 8, 3, 7, 8, 3, 7, 8]),
>                   Transformation([8, 6, 6, 8, 6, 8, 8, 8]));;
gap> Size(S);
30
gap> NrDClasses(S);
6
gap> NrRClasses(S);
12
gap> NrLClasses(S);
12
gap> NrHClasses(S);
30
gap> NrIdempotents(S);
15
gap> S.1 ^ 2 = MultiplicativeZero(S);
true
gap> IsCommutative(S);
false
gap> IsBand(S);
false
gap> IsNilpotentSemigroup(S);
false
gap> IsAperiodicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false

#T# Example from Semigroupe manual, Section 5.19
gap> S := Semigroup(Transformation([3, 5, 3, 3, 5]), 
>                   Transformation([6, 2, 4, 2, 2, 6]));;
gap> Size(S);
8
gap> IsomorphismFpSemigroup(S);;
gap> Length(RelationsOfFpSemigroup(Range(last)));
4
gap> Length(MinimalFactorization(S, Enumerator(S)[Size(S)]));
4
gap> NrDClasses(S);
2
gap> NrRClasses(S);
4
gap> NrLClasses(S);
4
gap> NrHClasses(S);
8
gap> NrIdempotents(S);
7
gap> MultiplicativeZero(S);
fail
gap> IsCommutative(S);
false
gap> IsBand(S);
false
gap> IsNilpotentSemigroup(S);
false
gap> IsAperiodicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false

# Example from Semigroupe manual, Section 5.20
gap> S := Monoid(Matrix(IsBooleanMat, [[0, 1, 0], 
>                                      [1, 1, 0],
>                                      [0, 1, 0]]), 
>                Matrix(IsBooleanMat, [[1, 0, 0], 
>                                      [1, 0, 1],
>                                      [1, 0, 0]]));
<monoid of 3x3 boolean matrices with 2 generators>
gap> Size(S);
7
gap> Length(RelationsOfFpMonoid(Range(IsomorphismFpMonoid(S))));
8
gap> Length(MinimalFactorization(S, Enumerator(S)[Size(S)]));
2
gap> NrDClasses(S);
4
gap> NrRClasses(S);
4
gap> NrLClasses(S);
7
gap> NrHClasses(S);
7
gap> NrIdempotents(S);
5
gap> MultiplicativeZero(S);
fail
gap> IsCommutative(S);
false
gap> IsBand(S);
false
gap> IsNilpotentSemigroup(S);
false
gap> IsAperiodicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsLTrivial(S);
true
gap> IsSemigroupWithCommutingIdempotents(S);
false

# Example from Semigroupe manual, Section 5.21
gap> S := Monoid(Matrix(IsNTPMatrix, [[0, 1, 0], 
>                                     [1, 1, 0],
>                                     [0, 1, 0]], 
>                       1, 2), 
>                Matrix(IsNTPMatrix, [[1, 0, 0], 
>                                     [1, 0, 1],
>                                     [1, 0, 0]],
>                       1, 2));
<monoid of 3x3 ntp matrices with 2 generators>
gap> Size(S);
37
gap> Length(RelationsOfFpMonoid(Range(IsomorphismFpMonoid(S))));
12
gap> Length(MinimalFactorization(S, Enumerator(S)[Size(S)]));
7
gap> NrDClasses(S);
8
gap> NrRClasses(S);
14
gap> NrLClasses(S);
17
gap> NrHClasses(S);
35
gap> NrIdempotents(S);
20
gap> MultiplicativeZero(S);
fail
gap> IsCommutative(S);
false
gap> IsBand(S);
false
gap> IsNilpotentSemigroup(S);
false
gap> IsAperiodicSemigroup(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false

# Example from Semigroupe manual, Section 5.22
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[0, -4],
>                                            [-4, -1]]),
>                   Matrix(IsMaxPlusMatrix, [[0, -3], 
>                                            [-3, -1]]));
<semigroup of 2x2 max-plus matrices with 2 generators>
gap> Size(S);
26
gap> Length(RelationsOfFpSemigroup(Range(IsomorphismFpSemigroup(S))));
9
gap> Length(MinimalFactorization(S, Enumerator(S)[Size(S)]));
8
gap> NrDClasses(S);
23
gap> NrRClasses(S);
24
gap> NrLClasses(S);
24
gap> NrHClasses(S);
26
gap> NrIdempotents(S);
4
gap> MultiplicativeZero(S);
fail
gap> IsCommutative(S);
false
gap> IsBand(S);
false
gap> IsNilpotentSemigroup(S);
false
gap> IsAperiodicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false

# Example from Semigroupe manual, Section 6
gap> S := Monoid(Transformation([2, 3, 4, 4]), 
>                Transformation([4, 1, 2, 4]), rec(generic := true));;
gap> Size(S);
15
gap> Length(RelationsOfFpMonoid(Range(IsomorphismFpMonoid(S))));
9
gap> Length(MinimalFactorization(S, Enumerator(S)[Size(S)]));
4
gap> NrDClasses(S);
4
gap> NrRClasses(S);
7
gap> NrLClasses(S);
7
gap> NrHClasses(S);
15
gap> NrIdempotents(S);
7
gap> MultiplicativeZero(S) = S.2 ^ 3; 
true
gap> IsCommutative(S);
false
gap> IsBand(S);
false
gap> IsNilpotentSemigroup(S);
false
gap> IsAperiodicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
true

# Test PartialUniformBlockBijectionMonoid
gap> S := PartialUniformBlockBijectionMonoid(5);
<inverse block bijection monoid of degree 6 with 4 generators>
gap> S := PartialUniformBlockBijectionMonoid(2);
<inverse block bijection monoid of degree 3 with 3 generators>
gap> Size(S);
8
gap> List([1 .. 5], n -> NrIdempotents(PartialUniformBlockBijectionMonoid(n)));
[ 2, 5, 15, 52, 203 ]
gap> List([1 .. 5], n -> Size(PartialUniformBlockBijectionMonoid(n)));
[ 2, 8, 53, 512, 6697 ]
gap> S := PartialUniformBlockBijectionMonoid(1);
<commutative inverse block bijection monoid of degree 2 with 1 generator>
gap> List([1 .. 5], n -> NrDClasses(PartialUniformBlockBijectionMonoid(n)));
[ 2, 4, 7, 12, 19 ]

# Test PartialDualSymmetricInverseMonoid
gap> S := PartialDualSymmetricInverseMonoid(4);
<inverse block bijection monoid of degree 5 with 4 generators>
gap> S := PartialDualSymmetricInverseMonoid(1);
<commutative inverse block bijection monoid of degree 2 with 1 generator>
gap> S := PartialDualSymmetricInverseMonoid(0);
<trivial block bijection group of degree 0 with 1 generator>
gap> S := PartialDualSymmetricInverseMonoid(2);
<inverse block bijection monoid of degree 3 with 3 generators>
gap> List([0 .. 5], n -> NrIdempotents(PartialDualSymmetricInverseMonoid(n)));
[ 1, 2, 5, 15, 52, 203 ]
gap> List([0 .. 5], n -> Size(PartialDualSymmetricInverseMonoid(n)));
[ 0, 2, 8, 80, 1280, 29072 ]
gap> List([0 .. 5], n -> NrDClasses(PartialDualSymmetricInverseMonoid(n)));
[ 0, 2, 4, 6, 8, 10 ]
gap> PartialDualSymmetricInverseMonoid(-1);
Error, Semigroups: PartialDualSymmetricInverseMonoid: usage,
the argument <n> must be a non-negative integer,

# Test RookPartitionMonoid
gap> S := RookPartitionMonoid(4);
<regular bipartition *-monoid of degree 5 with 5 generators>
gap> Size(S);
21147
gap> NrLClasses(S);
227
gap> NrRClasses(S);
227
gap> NrDClasses(S);
5
gap> NrHClasses(S);
16423
gap> NrIdempotents(S);
6255
gap> IsStarSemigroup(S);
true
gap> List([1 .. 3], x -> Size(RookPartitionMonoid(x)));
[ 5, 52, 877 ]

#E# 
gap> STOP_TEST("Semigroups package: standard/semiex.tst");
