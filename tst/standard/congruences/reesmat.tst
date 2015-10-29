###########################################################################
##
#W  standard/congruences/reesmat.tst
#Y  Copyright (C) 2014-15                                   Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/congruences/reesmat.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# All ReesMatrixSemigroup functions tested with a small example
#T# ReesMatCongTest1: Create a Rees matrix semigroup
gap> g := SymmetricGroup( 4 );;
gap> StructureDescription(g);
"S4"
gap> mat :=  [ [ (1,3), (1,2)(3,4) ],
>              [ (1,4,3,2), () ],
>              [ (1,3)(2,4), (1,3,4,2) ] ];;
gap> S := ReesMatrixSemigroup(g, mat);;

#T# ReesMatCongTest2: Find all its congruences
gap> congs := CongruencesOfSemigroup(S);;
gap> Size(congs);
23

#T# ReesMatCongTest3: Construct a congruence manually
gap> n := Group( [ (1,4)(2,3), (1,3)(2,4) ] );;
gap> colBlocks := [ [ 1 ], [ 2 ] ];;
gap> rowBlocks := [ [ 1, 2 ], [ 3 ] ];;
gap> cong := RMSCongruenceByLinkedTriple(S, n, colBlocks, rowBlocks);
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (2^2,2,2)>
gap> cong = congs[12];
false
gap> cong = congs[3];
true

#T# RMSCongruenceByLinkedTriple: Bad input
gap> RMSCongruenceByLinkedTriple(S, SymmetricGroup(3), colBlocks, rowBlocks);
Error, Semigroups: RMSCongruenceByLinkedTriple: usage,
the second arg <n> must be a normal subgroup,
gap> RMSCongruenceByLinkedTriple(S, n, [1, [2]], rowBlocks);
Error, Semigroups: RMSCongruenceByLinkedTriple: usage,
the third arg <colBlocks> must be a list of lists,
gap> RMSCongruenceByLinkedTriple(S, n, colBlocks, [[1,2], 3]);
Error, Semigroups: RMSCongruenceByLinkedTriple: usage,
the fourth arg <rowBlocks> must be a list of lists,

#T# ReesMatCongTest4: Testing membership
gap> x := ReesZeroMatrixSemigroupElement(S, 1, (2, 3), 2);;
gap> y := ReesZeroMatrixSemigroupElement(S, 1, (1, 4), 1);;
gap> z := ReesZeroMatrixSemigroupElement(S, 1, (2, 3, 4), 3);;
gap> [x, y] in cong;
true
gap> [x, z] in cong;
false
gap> [y, z] in cong;
false

#T# ReesMatCongTest5: Equivalence classes
gap> classes := CongruenceClasses(cong);;
gap> Size(classes) = NrCongruenceClasses(cong);
true
gap> class1 := CongruenceClassOfElement(cong, x);;
gap> class2 := CongruenceClassOfElement(cong, y);;
gap> class3 := CongruenceClassOfElement(cong, z);
<congruence class of (1,(2,3,4),3)>
gap> class1 = class2;
true
gap> class1 = class3;
false
gap> y in class1;
true
gap> x in class3;
false
gap> class1 = classes[3];
true
gap> nCoset := RightCoset(congs[3]!.n, (1, 3));;
gap> class := RMSCongruenceClassByLinkedTriple(congs[3], nCoset, 1, 2);;
gap> class = classes[7];
true
gap> classes[11] * classes[19] = classes[12];
true
gap> classes[12] * classes[10] = classes[8];
true
gap> classes[24] * classes[2] = classes[15]; # actually 16
false
gap> Size(classes[13]);
8
gap> Size(classes[24]);
4
gap> Size(classes[1]);
8

#T# ReesMatCongTest6: Join and meet congruences
gap> congs[3];
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (2^2,2,2)>
gap> congs[9];
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (A4,1,3)>
gap> JoinSemigroupCongruences(congs[3], congs[9]);
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (A4,1,2)>
gap> MeetSemigroupCongruences(congs[3], congs[9]);
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (2^2,2,3)>

#T# ReesMatCongTest7: Quotients
gap> q := S / congs[13];;
gap> Size(q);
4

#T# ReesMatCongTest8
# Convert to and from semigroup congruence by generating pairs
gap> cong := AsSemigroupCongruenceByGeneratingPairs(congs[2]);;
gap> ccong := AsRZMSCongruenceByLinkedTriple(cong);;
gap> congs[2] = ccong;
true

#T# ReesMatCongTest9: Universal semigroup congruences
gap> uni := UniversalSemigroupCongruence(S);
<universal semigroup congruence over <Rees matrix semigroup 2x3 over S4>>
gap> [x, z] in uni;
true
gap> Length(CongruenceClasses(uni)) = 1;
true
gap> eq := CongruenceClassOfElement(uni, y);;
gap> z in eq;
true
gap> cong := AsSemigroupCongruenceByGeneratingPairs(uni);;
gap> cong := AsRMSCongruenceByLinkedTriple(cong);;
gap> cong = uni;
true
gap> Size(S / uni);
1

# Similar tests, but with zero
#T# ReesZeroMatCongTest1: Create a Rees 0-matrix semigroup
gap> g := Group([(1, 4, 5), (1, 5, 3, 4)]);;
gap> mat := [[0, 0, (1, 4, 5), 0, 0, (1, 4, 3, 5)],
> [0, (), 0, 0, (3, 5), 0],
> [(), 0, 0, (3, 5), 0, 0]];;
gap> S := ReesZeroMatrixSemigroup(g, mat);;

#T# ReesZeroMatCongTest2: Find all its congruences
gap> congs := CongruencesOfSemigroup(S);;
gap> Size(congs);
33

#T# ReesZeroMatCongTest3: Construct a congruence manually
gap> n := Group([(1, 4)(3, 5), (1, 5)(3, 4)]);;
gap> colBlocks := [[1], [4], [2, 5], [3, 6]];;
gap> rowBlocks := [[1], [2], [3]];;
gap> cong := RZMSCongruenceByLinkedTriple(S, n, colBlocks, rowBlocks);
<semigroup congruence over <Rees 0-matrix semigroup 6x3 over Group([ (1,4,5),
 (1,5,3,4) ])> with linked triple (2^2,4,3)>
gap> cong = congs[12];
false
gap> cong = congs[13];
true

#T# ReesZeroMatCongTest4: Testing membership
gap> x := ReesZeroMatrixSemigroupElement(S, 3, (4, 5), 1);;
gap> y := ReesZeroMatrixSemigroupElement(S, 3, (1, 5, 3, 4), 1);;
gap> z := ReesZeroMatrixSemigroupElement(S, 1, (1, 3, 5), 2);;
gap> [x, y] in cong;
true
gap> [x, z] in cong;
false
gap> y := ReesZeroMatrixSemigroupElement(S, 6, (1, 3, 5), 1);;
gap> [x, y] in cong;
true

#T# ReesZeroMatCongTest5: Equivalence classes
gap> classes := CongruenceClasses(cong);;
gap> Size(classes) = NrCongruenceClasses(cong);
true
gap> class1 := CongruenceClassOfElement(cong, x);;
gap> class2 := CongruenceClassOfElement(cong, y);;
gap> class3 := CongruenceClassOfElement(cong, z);
<congruence class of (1,(1,3,5),2)>
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
gap> nCoset := RightCoset(congs[13]!.n, (1, 5));;
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

#T# ReesZeroMatCongTest6: Join and meet congruences
gap> JoinSemigroupCongruences(congs[12], congs[31]);
<semigroup congruence over <Rees 0-matrix semigroup 6x3 over Group([ (1,4,5),
 (1,5,3,4) ])> with linked triple (S4,3,3)>
gap> MeetSemigroupCongruences(congs[12], congs[31]);
<semigroup congruence over <Rees 0-matrix semigroup 6x3 over Group([ (1,4,5),
 (1,5,3,4) ])> with linked triple (2^2,6,3)>

#T# ReesZeroMatCongTest7: Quotients
gap> q := S / congs[13];;
gap> Size(q);
73

#T# ReesZeroMatCongTest8
# Convert to and from semigroup congruence by generating pairs
gap> cong := AsSemigroupCongruenceByGeneratingPairs(congs[2]);;
gap> ccong := AsRZMSCongruenceByLinkedTriple(cong);;
gap> congs[2] = ccong;
true

#T# ReesZeroMatCongTest9: Universal semigroup congruences
gap> uni := UniversalSemigroupCongruence(S);
<universal semigroup congruence over <Rees 0-matrix semigroup 6x3 over 
  Group([ (1,4,5), (1,5,3,4) ])>>
gap> [x, z] in uni;
true
gap> Length(CongruenceClasses(uni)) = 1 and
>   (Representative(CongruenceClasses(uni)[1]) = RMSElement(S, 1, (1, 4, 5), 3)
> or Representative(CongruenceClasses(uni)[1]) =
>   RMSElement(S, 1, (), 1)); # the first is after 4.7.7 the latter before
true
gap> eq := CongruenceClassOfElement(uni, y);
<congruence class of (6,(1,3,5),1)>
gap> eq := CongruenceClassOfElement(uni, y);;
gap> z in eq;
true
gap> cong := AsSemigroupCongruenceByGeneratingPairs(uni);;
gap> cong := AsRZMSCongruenceByLinkedTriple(cong);;
gap> cong = uni;
true
gap> Size(S / uni);
1

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(z);
gap> Unbind(colBlocks);
gap> Unbind(g);
gap> Unbind(eq);
gap> Unbind(nCoset);
gap> Unbind(cong);
gap> Unbind(ccong);
gap> Unbind(n);
gap> Unbind(q);
gap> Unbind(class);
gap> Unbind(S);
gap> Unbind(classes);
gap> Unbind(uni);
gap> Unbind(class1);
gap> Unbind(rowBlocks);
gap> Unbind(y);
gap> Unbind(x);
gap> Unbind(congs);
gap> Unbind(class2);
gap> Unbind(class3);
gap> Unbind(mat);

#E#
gap> STOP_TEST("Semigroups package: standard/congruences/reesmat.tst");
