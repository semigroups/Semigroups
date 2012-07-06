#############################################################################
##
#W  greens.gd
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
##  This file contains algorithms for computing Green's relations
##  and related notions for transformation semigroups and monoids. 
##  The theory behind these algorithms is developed in 
##  
##  [LPRR1] S. A.   Linton, G.  Pfeiffer, E.  F.  Robertson, and N.   Ruskuc,
##  Groups  and actions in  transformation semigroups, to appear in Math Z.
##  (1998).
##
##  Early versions of the algorithms themselves are described in
##
##  [LPRR2] S. A.   Linton, G.  Pfeiffer, E.  F.  Robertson, and N.   Ruskuc,
##  Computing transformation semigroups, (1998), in preparation.
##  
##  Another reference is
##
##  [LM]  G.  Lallement and R. McFadden, On the   determination of Green's
##  relations in finite transformation semigroups, J. Symbolic Computation 10
##  (1990), 481--489.
##
#############################################################################
#############################################################################

#new

DeclareProperty("IsActingSemigroupGreensClass", IsGreensClass);
DeclareProperty("IsGreensClassOfPartPermSemigroup", IsGreensClass);
DeclareProperty("IsGreensClassOfInverseSemigroup", IsGreensClass);

# indicates if the class was created using info from SemigroupData
DeclareProperty("IsGreensClassSD", IsActingSemigroupGreensClass);
# indicates if the rep is known to be an element of the semi or not
DeclareProperty("IsGreensClassNC", IsActingSemigroupGreensClass);
# IsGreensClassNC=true implies IsGreensClassSD=false, but no other implications
# except the contrapositive IsGreensClassSD=true implies IsGreensClassNC=false
# Hence the admissible pairs for [IsGreensClassNC, IsGreensClassSD] are:
# [true, false], [false, true], [false, false].

DeclareAttribute("LambdaOrbSCC", IsActingSemigroupGreensClass);
DeclareAttribute("RhoOrbSCC", IsActingSemigroupGreensClass);
DeclareAttribute("SemigroupDataSCC", IsActingSemigroupGreensClass);
DeclareAttribute("SemigroupDataSCCIndex", IsActingSemigroupGreensClass);
DeclareAttribute("SemigroupDataIndex", IsActingSemigroupGreensClass);

#old

DeclareGlobalFunction("CreateDClass"); # corresponds to [false, true] above
DeclareGlobalFunction("CreateHClass");
DeclareGlobalFunction("CreateLClass");
DeclareGlobalFunction("CreateRClass");

DeclareAttribute("DClassType", IsSemigroup);
DeclareAttribute("HClassType", IsSemigroup);
DeclareAttribute("LClassType", IsSemigroup);
DeclareAttribute("RClassType", IsSemigroup);

DeclareAttribute("DClassReps", IsSemigroup);
DeclareAttribute("HClassReps", IsSemigroup);
DeclareAttribute("LClassReps", IsSemigroup); 
DeclareAttribute("RClassReps", IsSemigroup);

DeclareOperation("EnumeratorOfRClasses", [IsSemigroup]);

DeclareOperation("GreensDClassOfElementNC", [IsSemigroup]); 
DeclareOperation("GreensHClassOfElementNC", [IsSemigroup]); 
DeclareOperation("GreensLClassOfElementNC", [IsSemigroup]); 
DeclareOperation("GreensRClassOfElementNC", [IsSemigroup]);

DeclareProperty("IsGreensClassOfTransSemigp", IsGreensClass);

DeclareProperty("IsRegularLClass", IsGreensClass); 
DeclareProperty("IsRegularRClass", IsGreensClass);

DeclareOperation("IteratorOfDClasses", [IsSemigroup]);
DeclareOperation("IteratorOfHClasses", [IsSemigroup]);
DeclareOperation("IteratorOfLClasses", [IsSemigroup]); 
DeclareOperation("IteratorOfRClasses", [IsSemigroup]);

DeclareOperation("IteratorOfDClassReps", [IsSemigroup]); 
DeclareOperation("IteratorOfLClassReps", [IsSemigroup]); 
DeclareOperation("IteratorOfHClassReps", [IsSemigroup]); 
DeclareOperation("IteratorOfRClassReps", [IsSemigroup]);

DeclareOperation("IteratorOfDClassRepsData", [IsSemigroup]);
DeclareOperation("IteratorOfHClassRepsData", [IsSemigroup]);
DeclareOperation("IteratorOfLClassRepsData", [IsSemigroup]);
DeclareOperation("IteratorOfRClassRepsData", [IsSemigroup]);

DeclareOperation("IteratorOfRClassData", [IsSemigroup]);

DeclareAttribute("NrIdempotents", IsSemigroup);

DeclareAttribute("NrDClasses", IsSemigroup); 
DeclareAttribute("NrHClasses", IsSemigroup);
DeclareAttribute("NrLClasses", IsSemigroup);
DeclareAttribute("NrRClasses", IsSemigroup);
DeclareAttribute("NrRegularDClasses", IsSemigroup); 

DeclareAttribute("PartialOrderOfDClasses", IsSemigroup);

DeclareAttribute("SchutzenbergerGroup", IsGreensClass);

DeclareSynonymAttr("DClasses", GreensDClasses);
DeclareSynonymAttr("HClasses", GreensHClasses);
DeclareSynonymAttr("JClasses", GreensJClasses);
DeclareSynonymAttr("LClasses", GreensLClasses);
DeclareSynonymAttr("RClasses", GreensRClasses);

# the following functions in r.gi are currently undocumented

DeclareAttribute("HClassRepsData", IsTransformationSemigroup);

DeclareProperty("IsIteratorOfRClassRepsData", IsIterator);
DeclareGlobalFunction("IsRegularRClassData");

DeclareGlobalFunction("HClassRepFromData");
DeclareGlobalFunction("RClassRepFromData");
DeclareAttribute("RClassRepsData", IsTransformationSemigroup);
DeclareGlobalFunction("TraceRClassRepsTree");

# the documentation for the functions below can be found in 
# /monoid/doc/d.xml


# the following functions in d.gi are currently undocumented

DeclareAttribute("GroupHClass", IsGreensDClass and
 IsGreensClassOfTransSemigp);

# the following functions in l.gi are currently undocumented


# the following functions in h.gi are currently undocumented

DeclareProperty("IsIteratorOfRClassReps", IsIterator);
DeclareProperty("IsIteratorOfLClassReps", IsIterator);
DeclareProperty("IsIteratorOfDClassReps", IsIterator);
DeclareProperty("IsIteratorOfHClassReps", IsIterator);

DeclareProperty("IsIteratorOfRClasses", IsIterator);
DeclareProperty("IsIteratorOfLClasses", IsIterator);
DeclareProperty("IsIteratorOfDClasses", IsIterator);
DeclareProperty("IsIteratorOfHClasses", IsIterator);

DeclareProperty("IsIteratorOfSemigroup", IsIterator);
DeclareProperty("IsIteratorOfRClassElements", IsIterator);
DeclareProperty("IsIteratorOfLClassElements", IsIterator);
DeclareProperty("IsIteratorOfDClassElements", IsIterator);
DeclareProperty("IsIteratorOfHClassElements", IsIterator);

#EOF
