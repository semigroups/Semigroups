############################################################################
##
##  iterators.gi
##  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# constructors for iterators
DeclareGlobalFunction("IteratorByOrbFunc");
DeclareGlobalFunction("IteratorByNextIterator");
DeclareGlobalFunction("IteratorByIterOfIters");
DeclareGlobalFunction("IteratorByIterator");

# Keeper 
DeclareOperation("IteratorOfDClasses", [IsSemigroup]);

# Not get processed

DeclareOperation("IteratorOfRClasses", [IsSemigroup]);

DeclareOperation("IteratorOfDClassReps", [IsSemigroup]);
DeclareOperation("IteratorOfLClassReps", [IsSemigroup]);
DeclareOperation("IteratorOfHClassReps", [IsSemigroup]);

DeclareOperation("IteratorOfRClassData", [IsSemigroup]);
DeclareOperation("IteratorOfHClassData", [IsSemigroup]);
DeclareOperation("IteratorOfLClassData", [IsSemigroup]);
