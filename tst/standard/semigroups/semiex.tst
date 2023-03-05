#############################################################################
##
#W  standard/semigroups/semiex.tst
#Y  Copyright (C) 2016-2022                              James D. Mitchell
##                                                       
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local M, S, T, acting, n, part
gap> START_TEST("Semigroups package: standard/semigroups/semiex.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Test EndomorphismsPartition 1
gap> EndomorphismsPartition([-1]);
Error, the argument (a cyclo. coll.) does not consist of positive integers
gap> EndomorphismsPartition([1, 1, 1]);
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

# ExamplesTest37: ham-examples
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
Error, the argument (an int) is not > 1

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
Error, the argument (an int) is not > 1

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
Error, the 1st argument (a pos. int.) is not <= to the 2nd argument (a pos. in\
t.)

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
Error, the 1st argument (a pos. int.) is not <= to the 2nd argument (a pos. in\
t.)

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
Error, the 2nd argument (a pos. int.) must be > 1 when the 1st argument (a pos\
. int.) is also > 1

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
gap> SingularModularPartitionMonoid(1, 1);
<commutative inverse bipartition semigroup ideal of degree 1 with 1 generator>
gap> SingularModularPartitionMonoid(2, 1);
Error, the 2nd argument (a pos. int.) must be > 1 when the 1st argument (a pos\
. int.) is also > 1

# Catalan monoid
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

# Example from Semigroupe manual, Section 5.18: Knast's counterexample
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

# Example from Semigroupe manual, Section 5.19
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
>                Transformation([4, 1, 2, 4]), rec(acting := false));;
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
[ 1, 2, 8, 80, 1280, 29072 ]
gap> List([0 .. 5], n -> NrDClasses(PartialDualSymmetricInverseMonoid(n)));
[ 1, 2, 4, 6, 8, 10 ]
gap> PartialDualSymmetricInverseMonoid(-1);
Error, the argument (an int) is not >= 0

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

# Test GLM
gap> S := GLM(3, 3);
<general linear monoid 3x3 over GF(3)>
gap> Size(S);
19683
gap> NrLClasses(S);
28
gap> NrRClasses(S);
28
gap> NrDClasses(S);
4
gap> NrHClasses(S);
340
gap> NrIdempotents(S);
236
gap> IsRegularSemigroup(S);
true
gap> IsFullMatrixMonoid(S);
true

# Test SLM
gap> S := SLM(3, 3);
<regular monoid of 3x3 matrices over GF(3) with 3 generators>
gap> Size(S);
14067
gap> NrLClasses(S);
28
gap> NrRClasses(S);
28
gap> NrDClasses(S);
4
gap> NrHClasses(S);
340
gap> NrIdempotents(S);
236
gap> IsRegularSemigroup(S);
true
gap> IsFullMatrixMonoid(S);
false

# Test MunnSemigroup
gap> S := InverseSemigroup(PartialPerm([1, 2, 3, 4, 5, 6, 7, 10], 
>                                      [4, 6, 7, 3, 8, 2, 9, 5]),
>                          PartialPerm([1, 2, 7, 9], 
>                                      [5, 6, 4, 3]));;
gap> T := IdempotentGeneratedSubsemigroup(S);;
gap> M := MunnSemigroup(T);;
gap> NrIdempotents(M);
60
gap> NrIdempotents(S);
60
gap> MunnSemigroup(S);
Error, the argument (a semigroup) is not a semilattice

# Test OrderEndomorphisms
gap> S := OrderEndomorphisms(4);
<regular transformation monoid of degree 4 with 4 generators>
gap> Size(S);
35
gap> NrLClasses(S);
15
gap> NrRClasses(S);
8
gap> NrDClasses(S);
4
gap> NrHClasses(S);
35
gap> NrIdempotents(S);
21
gap> IsRegularSemigroup(S);
true
gap> S := OrderEndomorphisms(1);
<trivial transformation group of degree 0 with 1 generator>
gap> Size(S);
1
gap> NrLClasses(S);
1
gap> NrRClasses(S);
1
gap> NrDClasses(S);
1
gap> NrHClasses(S);
1
gap> NrIdempotents(S);
1
gap> IsRegularSemigroup(S);
true

# Test PartialOrderEndomorphisms
gap> S := PartialOrderEndomorphisms(4);
<regular transformation monoid of degree 5 with 8 generators>
gap> Size(S);
192
gap> NrLClasses(S);
16
gap> NrRClasses(S);
41
gap> NrDClasses(S);
5
gap> NrHClasses(S);
192
gap> NrIdempotents(S);
76
gap> IsRegularSemigroup(S);
true
gap> S := PartialOrderEndomorphisms(1);
<commutative inverse transformation monoid of degree 2 with 1 generator>

# Test OrderAntiEndomorphisms
gap> S := OrderAntiEndomorphisms(4);
<regular transformation monoid of degree 4 with 5 generators>
gap> Size(S);
66
gap> NrLClasses(S);
15
gap> NrRClasses(S);
8
gap> NrDClasses(S);
4
gap> NrHClasses(S);
35
gap> NrIdempotents(S);
21
gap> IsRegularSemigroup(S);
true
gap> S := OrderAntiEndomorphisms(1);
<trivial transformation group of degree 0 with 1 generator>

# Test PartialOrderAntiEndomorphisms
gap> S := PartialOrderAntiEndomorphisms(4);
<regular transformation monoid of degree 5 with 9 generators>
gap> Size(S);
323
gap> NrLClasses(S);
16
gap> NrRClasses(S);
41
gap> NrDClasses(S);
5
gap> NrHClasses(S);
192
gap> NrIdempotents(S);
76
gap> IsRegularSemigroup(S);
true
gap> S := PartialOrderAntiEndomorphisms(1);
<regular transformation monoid of degree 2 with 2 generators>

# Test PartialTranformationMonoid
gap> S := PartialTransformationMonoid(1);
<commutative inverse transformation monoid of degree 2 with 1 generator>
gap> Size(S);
2
gap> NrLClasses(S);
2
gap> NrRClasses(S);
2
gap> NrDClasses(S);
2
gap> NrHClasses(S);
2
gap> NrIdempotents(S);
2
gap> IsRegularSemigroup(S);
true
gap> S := PartialTransformationMonoid(2);
<regular transformation monoid of degree 3 with 3 generators>
gap> Size(S);
9
gap> NrLClasses(S);
4
gap> NrRClasses(S);
5
gap> NrDClasses(S);
3
gap> NrHClasses(S);
8
gap> NrIdempotents(S);
6
gap> IsRegularSemigroup(S);
true
gap> S := PartialTransformationMonoid(4);
<regular transformation monoid of degree 5 with 4 generators>
gap> Size(S);
625
gap> NrLClasses(S);
16
gap> NrRClasses(S);
52
gap> NrDClasses(S);
5
gap> NrHClasses(S);
252
gap> NrIdempotents(S);
104
gap> IsRegularSemigroup(S);
true

# PartitionMonoid
gap> PartitionMonoid(-1);
Error, the argument (an int) is not >= 0
gap> PartitionMonoid(0);
<trivial block bijection group of degree 0 with 1 generator>
gap> PartitionMonoid(1);
<commutative bipartition monoid of degree 1 with 1 generator>
gap> PartitionMonoid(5);
<regular bipartition *-monoid of size 115975, degree 5 with 4 generators>

# DualSymmetricInverseMonoid
gap> DualSymmetricInverseMonoid(-1);
Error, the argument (an int) is not >= 0
gap> DualSymmetricInverseMonoid(0);
<trivial block bijection group of degree 0 with 1 generator>
gap> DualSymmetricInverseMonoid(1);
<trivial block bijection group of degree 1 with 1 generator>
gap> DualSymmetricInverseMonoid(2);
<inverse block bijection monoid of degree 2 with 2 generators>
gap> DualSymmetricInverseMonoid(5);
<inverse block bijection monoid of degree 5 with 3 generators>

# BrauerMonoid
gap> BrauerMonoid(-1);
Error, the argument (an int) is not >= 0
gap> BrauerMonoid(0);
<trivial block bijection group of degree 0 with 1 generator>
gap> BrauerMonoid(1);
<trivial block bijection group of degree 1 with 1 generator>
gap> BrauerMonoid(2);
<regular bipartition *-monoid of degree 2 with 2 generators>
gap> BrauerMonoid(5);
<regular bipartition *-monoid of degree 5 with 3 generators>

# PartialBrauerMonoid
gap> PartialBrauerMonoid(-1);
Error, the argument (an int) is not >= 0
gap> PartialBrauerMonoid(0);
<trivial block bijection group of degree 0 with 1 generator>
gap> PartialBrauerMonoid(1);
<regular bipartition *-monoid of degree 1 with 2 generators>
gap> PartialBrauerMonoid(2);
<regular bipartition *-monoid of degree 2 with 5 generators>
gap> PartialBrauerMonoid(5);
<regular bipartition *-monoid of degree 5 with 8 generators>

# JonesMonoid
gap> JonesMonoid(-1);
Error, the argument (an int) is not >= 0
gap> JonesMonoid(0);
<trivial block bijection group of degree 0 with 1 generator>
gap> JonesMonoid(1);
<trivial block bijection group of degree 1 with 1 generator>
gap> JonesMonoid(2);
<commutative inverse bipartition monoid of degree 2 with 1 generator>
gap> JonesMonoid(5);
<regular bipartition *-monoid of degree 5 with 4 generators>

# AnnularJonesMonoid
gap> AnnularJonesMonoid(-1);
Error, the argument (an int) is not >= 0
gap> AnnularJonesMonoid(0);
<trivial block bijection group of degree 0 with 1 generator>
gap> AnnularJonesMonoid(1);
<trivial block bijection group of degree 1 with 1 generator>
gap> AnnularJonesMonoid(2);
<regular bipartition *-monoid of degree 2 with 2 generators>
gap> AnnularJonesMonoid(5);
<regular bipartition *-monoid of degree 5 with 2 generators>

# PartialJonesMonoid
gap> PartialJonesMonoid(-1);
Error, the argument (an int) is not >= 0
gap> PartialJonesMonoid(0);
<trivial block bijection group of degree 0 with 1 generator>
gap> PartialJonesMonoid(1);
<commutative bipartition monoid of degree 1 with 1 generator>
gap> PartialJonesMonoid(2);
<regular bipartition *-monoid of degree 2 with 3 generators>
gap> PartialJonesMonoid(5);
<regular bipartition *-monoid of degree 5 with 9 generators>

# MotzkinMonoid
gap> MotzkinMonoid(-1);
Error, the argument (an int) is not >= 0
gap> MotzkinMonoid(0);
<trivial block bijection group of degree 0 with 1 generator>
gap> MotzkinMonoid(1);
<regular bipartition *-monoid of degree 1 with 2 generators>
gap> MotzkinMonoid(2);
<regular bipartition *-monoid of degree 2 with 4 generators>
gap> MotzkinMonoid(5);
<regular bipartition *-monoid of degree 5 with 10 generators>

# POI
gap> POI(1);
<symmetric inverse monoid of degree 1>
gap> POI(2);
<inverse partial perm monoid of rank 2 with 2 generators>
gap> POI(5);
<inverse partial perm monoid of rank 5 with 5 generators>

# POPI
gap> POPI(1);
<symmetric inverse monoid of degree 1>
gap> POPI(2);
<symmetric inverse monoid of degree 2>
gap> POPI(5);
<inverse partial perm monoid of rank 5 with 2 generators>

# PODI
gap> PODI(1);
<symmetric inverse monoid of degree 1>
gap> PODI(2);
<symmetric inverse monoid of degree 2>
gap> PODI(5);
<inverse partial perm monoid of rank 5 with 6 generators>

# PORI
gap> PORI(1);
<symmetric inverse monoid of degree 1>
gap> PORI(2);
<symmetric inverse monoid of degree 2>
gap> PORI(5);
<inverse partial perm monoid of rank 5 with 3 generators>

# SingularPartitionMonoid
gap> SingularPartitionMonoid(1);;
gap> SingularPartitionMonoid(2);
<regular bipartition *-semigroup ideal of degree 2 with 1 generator>
gap> SingularPartitionMonoid(5);
<regular bipartition *-semigroup ideal of degree 5 with 1 generator>

# SingularTransformationMonoid
gap> SingularTransformationMonoid(1);
Error, the argument (an int) is not > 1
gap> SingularTransformationMonoid(2);
<regular transformation semigroup ideal of degree 2 with 1 generator>
gap> SingularTransformationMonoid(5);
<regular transformation semigroup ideal of degree 5 with 1 generator>

# SingularOrderEndomorphisms
gap> SingularOrderEndomorphisms(1);
Error, the argument (an int) is not > 1
gap> SingularOrderEndomorphisms(2);
<regular transformation semigroup ideal of degree 2 with 1 generator>
gap> SingularOrderEndomorphisms(5);
<regular transformation semigroup ideal of degree 5 with 1 generator>

# SingularBrauerMonoid
gap> SingularBrauerMonoid(1);
Error, the argument (an int) is not > 1
gap> SingularBrauerMonoid(2);
<regular bipartition *-semigroup ideal of degree 2 with 1 generator>
gap> SingularBrauerMonoid(5);
<regular bipartition *-semigroup ideal of degree 5 with 1 generator>

# SingularJonesMonoid
gap> SingularJonesMonoid(1);
Error, the argument (an int) is not > 1
gap> SingularJonesMonoid(2);
<commutative inverse bipartition semigroup ideal of degree 2 with 1 generator>
gap> SingularJonesMonoid(5);
<regular bipartition *-semigroup ideal of degree 5 with 1 generator>

# SingularDualSymmetricInverseMonoid
gap> SingularDualSymmetricInverseMonoid(1);
Error, the argument (an int) is not > 1
gap> SingularDualSymmetricInverseMonoid(2);
<inverse bipartition semigroup ideal of degree 2 with 1 generator>
gap> SingularDualSymmetricInverseMonoid(5);
<inverse bipartition semigroup ideal of degree 5 with 1 generator>

# FullTropicalMinPlusMonoid
gap> FullTropicalMinPlusMonoid(3, 10);
<monoid of 3x3 tropical min-plus matrices with 521 generators>
gap> FullTropicalMinPlusMonoid(10, 10);
Error, the 1st argument (dimension) must be 2 or 3

# FullPBRMonoid
gap> FullPBRMonoid(1);
<pbr monoid of degree 1 with 4 generators>
gap> FullPBRMonoid(2);
<pbr monoid of degree 2 with 10 generators>
gap> FullPBRMonoid(3);
Error, the argument (a pos. int.) must be at most 2

# Test RegularBooleanMatMonoid
gap> S := RegularBooleanMatMonoid(1);
<commutative monoid of 1x1 boolean matrices with 1 generator>
gap> S = FullBooleanMatMonoid(1);
true
gap> Size(S);
2
gap> S := RegularBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> Size(S);
16
gap> S = FullBooleanMatMonoid(2);
true
gap> S := RegularBooleanMatMonoid(3);
<monoid of 3x3 boolean matrices with 4 generators>
gap> T := FullBooleanMatMonoid(3);
<monoid of 3x3 boolean matrices with 5 generators>
gap> Size(S);
506
gap> S = T;
false
gap> S = SubsemigroupByProperty(T, x -> IsRegularSemigroupElement(T, x));
true

# Test GossipMonoid
gap> S := GossipMonoid(1);
<trivial group of 1x1 boolean matrices with 1 generator>
gap> Size(S);
1
gap> T := SubsemigroupByProperty(FullBooleanMatMonoid(2),
>                                IsEquivalenceBooleanMat);
<commutative monoid of size 2, 2x2 boolean matrices with 1 generator>
gap> GossipMonoid(3) = SubsemigroupByProperty(FullBooleanMatMonoid(3),
>                                             IsEquivalenceBooleanMat);
true

# Test UnitriangularBooleanMatMonoid
gap> UnitriangularBooleanMatMonoid(1);
<trivial group of 1x1 boolean matrices with 1 generator>
gap> n := 2;;
gap> S := UnitriangularBooleanMatMonoid(n);
<commutative monoid of 2x2 boolean matrices with 1 generator>
gap> Size(S) = 2 ^ (n * (n - 1) / 2);
true
gap> IsDTrivial(S);
true
gap> n := 3;;
gap> S := UnitriangularBooleanMatMonoid(n);
<monoid of 3x3 boolean matrices with 3 generators>
gap> Size(S) = 2 ^ (n * (n - 1) / 2);
true
gap> IsDTrivial(S);
true

# Test TriangularBooleanMatMonoid
gap> TriangularBooleanMatMonoid(1);
<trivial group of 1x1 boolean matrices with 1 generator>
gap> n := 2;;
gap> S := TriangularBooleanMatMonoid(n);
<monoid of 2x2 boolean matrices with 3 generators>
gap> Size(S);
8
gap> n := 3;;
gap> S := TriangularBooleanMatMonoid(n);
<monoid of 3x3 boolean matrices with 6 generators>
gap> Size(S);
64

# Test ReflexiveBooleanMatMonoid
gap> ReflexiveBooleanMatMonoid(1);
<trivial group of 1x1 boolean matrices with 1 generator>
gap> S := ReflexiveBooleanMatMonoid(3);
<monoid of 3x3 boolean matrices with 8 generators>
gap> S := ReflexiveBooleanMatMonoid(5);
<monoid of 5x5 boolean matrices with 1414 generators>
gap> S := ReflexiveBooleanMatMonoid(7);
Error, generators for this monoid are only provided up to dimension 6

# Test HallMonoid
gap> HallMonoid(1);
<trivial group of 1x1 boolean matrices with 1 generator>
gap> S := HallMonoid(3);
<monoid of 3x3 boolean matrices with 4 generators>
gap> Size(S);
247
gap> S := HallMonoid(5);
<monoid of 5x5 boolean matrices with 12 generators>
gap> S := HallMonoid(6);
<monoid of 6x6 boolean matrices with 67 generators>
gap> S := HallMonoid(7);
<monoid of 7x7 boolean matrices with 2141 generators>
gap> S := HallMonoid(9);
Error, generators for this monoid are only known up to dimension 8

# Test FullBooleanMatMonoid
gap> FullBooleanMatMonoid(1);
<commutative monoid of 1x1 boolean matrices with 1 generator>
gap> S := FullBooleanMatMonoid(3);
<monoid of 3x3 boolean matrices with 5 generators>
gap> Size(S) = 2 ^ (3 ^ 2);
true
gap> S := FullBooleanMatMonoid(5);
<monoid of 5x5 boolean matrices with 13 generators>
gap> S := FullBooleanMatMonoid(6);
<monoid of 6x6 boolean matrices with 68 generators>
gap> S := FullBooleanMatMonoid(7);
<monoid of 7x7 boolean matrices with 2142 generators>
gap> S := FullBooleanMatMonoid(9);
Error, generators for this monoid are only known up to dimension 8

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/semigroups/semiex.tst");
