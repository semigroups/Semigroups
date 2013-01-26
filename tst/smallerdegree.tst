#############################################################################
#
# Test file for St Andrews Pure Maths Summer School 2012
#
#############################################################################

gap> START_TEST("Summer school 2012 functions: summer2012.tst");

# Copied from Citrus
gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoCitrus:=InfoLevel(InfoCitrus);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoCitrus, 0);

# Example *was* returning G of size 820, not 821
gap> g:=RandomInverseSemigroup(1,1000);
<inverse semigroup with 1 generator>
gap> G:=SmallerDegreePartialPermRep(g);
<inverse semigroup with 1 generator>
gap> g=G;
false
gap> Size(g);
821
gap> Size(G);
821
gap> Degree(g);
1000
gap> NrMovedPoints(g);
840
gap> Degree(G);
16

# Another random example
gap> g:=RandomInverseSemigroup(3,20);
<inverse semigroup with 3 generators>
gap> G:=SmallerDegreePartialPermRep(g);;
gap> g=G;
true
gap> Size(g);
94062
gap> NrMovedPoints(g);
20

# Another random example
gap> g:=RandomInverseSemigroup(2,50);
<inverse semigroup with 2 generators>
gap> G:=SmallerDegreePartialPermRep(g);;
gap> g=G;
false
gap> Size(g);
1141653
gap> Size(G);
1141653
gap> Degree(g);
50
gap> NrMovedPoints(g);
47
gap> Degree(G);
47

# Standard WagnerPreston example
gap> I5:=SymmetricInverseSemigp(5);
<inverse semigroup with 3 generators>
gap> NrMovedPoints(I5);
5
gap> Size(I5);
1546
gap> I5:=WagnerPrestonRepresentation(I5);;
gap> NrMovedPoints(I5);
1545
gap> Size(I5);
1546
gap> I5:=SmallerDegreePartialPermRep(I5);;
gap> NrMovedPoints(I5);
5
gap> Size(I5);
1546

# Example of higher returned degree spotted by Rhiannon
gap> f1:=PartialPermNC([2,1,4,5,3]);;
gap> f2:=PartialPermNC([2,1]);;
gap> f:=InverseSemigroup(f1,f2);
<inverse semigroup with 2 generators>
gap> F:=SmallerDegreePartialPermRep(f);;
gap> f=F;
true
gap> NrMovedPoints(f);
5
gap> Size(f);
8

# Example of higher returned degree spotted by Robert
gap> f1:=PartialPermNC([2,1,0,0,4]);;
gap> f2:=PartialPermNC([1,2,3,5]);;
gap> f:=InverseSemigroup(f1,f2);
<inverse semigroup with 2 generators>
gap> F:=SmallerDegreePartialPermRep(f);;
gap> NrMovedPoints(f);
4
gap> NrMovedPoints(F);
4
gap> Size(f);
15
gap> Size(F);
15

# Example where Rhiannon's function returns a better result
gap> f1:=PartialPermNC([2,1,4,5,3,7,6,9,10,8]);;
gap> f2:=PartialPermNC([2,1,0,0,0,7,6]);;
gap> f:=InverseSemigroup(f1,f2);
<inverse semigroup with 2 generators>
gap> F:=SmallerDegreePartialPermRep(f);;
gap> NrMovedPoints(f);
10
gap> NrMovedPoints(F);
7
gap> Size(f);
8
gap> Size(F);
8

# Example of reducing degree but not moved points
gap> f1:=PartialPermNC([ 1, 2, 3, 4, 5, 6, 10, 11, 15, 16, 17, 18 ], [ 7, 5, 11, 8, 4, 2, 20, 14, 12, 17, 9, 3 ]);;
gap> f2:=PartialPermNC([ 1, 2, 3, 6, 8, 10, 12, 15, 16, 17, 18, 19 ], [ 2, 4, 14, 3, 17, 7, 9, 16, 15, 10, 11, 1 ]);;
gap> f:=InverseSemigroup(f1,f2);
<inverse semigroup with 2 generators>
gap> F:=SmallerDegreePartialPermRep(f);;
gap> NrMovedPoints(f);
19
gap> NrMovedPoints(F);
19
gap> Degree(f);
20
gap> Degree(F);
19

# Copied from Citrus
gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;
gap> STOP_TEST("st-andrews-summer-school-2012/summer2012.tst", 10000);