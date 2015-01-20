###########################################################################
##
#W  reesmat-cong.tst
#Y  Copyright (C) 2014-15                                   Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: reesmat-cong.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels and user preferences
gap> SemigroupsStartTest();

# All ReesZeroMatrixSemigroup functions tested with a small example
# Create a Rees 0-matrix semigroup
gap> g := Group( [ (1,4,5), (1,5,3,4) ] );;
gap> mat := [ [ 0, 0, (1,4,5), 0, 0, (1,4,3,5) ],
> [ 0, (), 0, 0, (3,5), 0 ],
> [ (), 0, 0, (3,5), 0, 0 ] ];;
gap> s := ReesZeroMatrixSemigroup(g, mat);;

#T# Find all its congruences
gap> congs := CongruencesOfSemigroup(s);;
gap> Size(congs);
33

#T# Construct a congruence manually
gap> n := Group([(1,4)(3,5),(1,5)(3,4)]);;
gap> colBlocks := [ [ 1 ], [ 4 ], [ 2, 5 ], [ 3, 6 ] ];;
gap> rowBlocks := [ [ 1 ], [ 2 ], [ 3 ] ];;
gap> cong := RZMSCongruenceByLinkedTriple(s, n, colBlocks, rowBlocks);
<semigroup congruence over <Rees 0-matrix semigroup 6x3 over Group([ (1,4,5),
 (1,5,3,4) ])> with linked triple (2^2,4,3)>
gap> cong = congs[12];
false
gap> cong = congs[13];
true

#T# Testing membership
gap> x := ReesZeroMatrixSemigroupElement(s, 3, (4,5), 1);;
gap> y := ReesZeroMatrixSemigroupElement(s, 3, (1,5,3,4), 1);;
gap> z := ReesZeroMatrixSemigroupElement(s, 1, (1,3,5), 2);;
gap> [x,y] in cong;
true
gap> [x,z] in cong;
false
gap> y := ReesZeroMatrixSemigroupElement(s, 6, (1,3,5), 1);;
gap> [x,y] in cong;
true

#T# Equivalence classes
gap> classes := EquivalenceClasses(cong);;
gap> Size(classes) = NrCongruenceClasses(cong);
true
gap> class1 := EquivalenceClassOfElement(cong, x);;
gap> class2 := EquivalenceClassOfElement(cong, y);;
gap> class3 := EquivalenceClassOfElement(cong, z);
{(1,(1,3,5),2)}
gap> class1 = class2;
true
gap> class1 = class3;
false
gap> y in class1;
true
gap> x in class3;
false
gap> class1 = classes[38];
true
gap> nCoset := RightCoset(congs[13]!.n, (1,5));;
gap> class := RZMSCongruenceClassByLinkedTriple(congs[13], nCoset, 3, 2);;
gap> class = classes[44];
true
gap> classes[45] * classes[4] = classes[73]; # 0 class
true
gap> classes[28] * classes[32] = classes[36]; # non-0 class
true
gap> classes[28] * classes[32] = classes[15];
false
gap> Size(classes[13]);
4
gap> Size(classes[72]);
4
gap> Size(classes[73]);
1

#T# Join and meet congruences
gap> JoinSemigroupCongruences(congs[12], congs[31]);
<semigroup congruence over <Rees 0-matrix semigroup 6x3 over Group([ (1,4,5),
 (1,5,3,4) ])> with linked triple (S4,3,3)>
gap> MeetSemigroupCongruences(congs[12], congs[31]);
<semigroup congruence over <Rees 0-matrix semigroup 6x3 over Group([ (1,4,5),
 (1,5,3,4) ])> with linked triple (2^2,6,3)>

#T# Quotients
gap> q := s / congs[13];;
gap> Size(q);
73

#T# Convert to and from semigroup congruence by generating pairs
gap> cong := AsSemigroupCongruenceByGeneratingPairs(congs[2]);;
gap> ccong := AsRZMSCongruenceByLinkedTriple(cong);;
gap> congs[2] = ccong;
true

#T# Universal semigroup congruences
gap> uni := UniversalSemigroupCongruence(s);
<universal semigroup congruence over <Rees 0-matrix semigroup 6x3 over 
  Group([ (1,4,5), (1,5,3,4) ])>>
gap> [x,z] in uni;
true
gap> Length(EquivalenceClasses(uni)) = 1 
> and (Representative(EquivalenceClasses(uni)[1]) = RMSElement(s, 1,(1,4,5),3)
> or Representative(EquivalenceClasses(uni)[1]) = RMSElement(s, 1,(),1)); # the first is after 4.7.7 the latter before
true
gap> eq := EquivalenceClassOfElement(uni, y);
{(6,(1,3,5),1)}
gap> eq := EquivalenceClassOfElement(uni, y);;
gap> z in eq;
true
gap> cong := AsSemigroupCongruenceByGeneratingPairs(uni);;
gap> AsRZMSCongruenceByLinkedTriple(cong) = uni;
true
gap> Size(s / uni);
1

#E#
gap> STOP_TEST( "Semigroups package: reesmat-cong.tst");
